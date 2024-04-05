{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
module Exe
( main
)
where

import Types
import qualified Json
import GHC hiding (moduleName)
import qualified GHC.Paths
import GHC.Core.Type (splitFunTys, expandTypeSynonyms)
import GHC.Driver.Ppr (showSDocForUser)
import GHC.Unit.State (lookupUnitId, lookupPackageName)
import GHC.Unit.Info (UnitInfo, unitExposedModules, unitId, PackageName(..))
import GHC.Unit.Types (UnitId)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Env (hsc_units)
import GHC.Utils.Outputable hiding (sep, (<>))
import GHC.Types.TyThing (tyThingParent_maybe)
import GHC.Types.Name (nameOccName, getSrcLoc)
import GHC.Types.Name.Occurrence (OccName)
import GHC.Types.Var (varName, varType)
import Data.Function (on)
import Data.List (sortBy)
import System.Environment (getArgs)
import Control.Monad (forM, forM_, unless)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified System.Exit as Exit
import Control.Monad.IO.Class (liftIO, MonadIO)
import GHC.IO.Unsafe (unsafeInterleaveIO)
import qualified System.IO as IO
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Control.Exception as Ex
import GHC.Core.Multiplicity (scaledThing)
import qualified Control.Monad.Catch
import qualified Control.Exception
import GHC.Core.TyCo.Rep (Type(..))
import GHC.Core.TyCon (isUnboxedTupleTyCon, isBoxedTupleTyCon, isTupleTyCon)
import GHC.Builtin.Names (listTyConKey, getUnique)
import qualified Data.Text as T
import Data.Bifunctor (bimap, first)
import GHC.Stack (HasCallStack)
import Data.Functor.Identity (Identity(Identity))

main :: IO ()
main = do
  pkg_names <- getArgs
  let runGhc' :: Ghc a -> IO (Either Control.Monad.Catch.SomeException a)
      runGhc' action = reallyCatch $ runGhc (Just GHC.Paths.libdir) action
  pprFun <- case pkg_names of
    [] -> Exit.die "Missing argument(s): one or more packages"
    first_package_name : _ -> runGhc' (getPprFun first_package_name) >>= either (fail . show) pure
  lst <- forM pkg_names $ \pkg_nm -> do
    unsafeInterleaveIO $ runGhc' (getDefinitions pprFun pkg_nm) >>= logErrors
  let declarationMapJsonList = map (declarationMapToJson pprFun) (catMaybes lst)
  forM_ declarationMapJsonList $ \declarationMapJson -> do
    let errors = Map.assocs $ Map.assocs <$> Json.moduleDeclarations_mapFail (Json.declarationMapJson_moduleDeclarations declarationMapJson)
    forM_ errors $ \(modName, pkgErrs) ->
      forM_ pkgErrs $ \(defnName, err) ->
        logError $ ("WARNING: " <>) $ unwords
          [ "Parse failure in"
          , T.unpack $ Json.declarationMapJson_package declarationMapJson <> "."
          , T.unpack $ modName <> "." <> defnName
          , unTyConParseError err
          ]
  Json.streamPrintJsonList declarationMapJsonList
  where
    reallyCatch :: IO a -> IO (Either Control.Exception.SomeException a)
    reallyCatch ioAction =
      Control.Exception.catch
        (Right <$> (ioAction >>= Control.Exception.evaluate))
        $ \e -> case Control.Exception.fromException e :: Maybe Ex.AsyncException of
            Nothing -> pure . Left $ e
            Just eAsync -> logError ("Caught async exception: " ++ show eAsync) >> Ex.throwIO eAsync

    logErrors
      :: Either Control.Monad.Catch.SomeException (Maybe DeclarationMap)
      -> IO (Maybe DeclarationMap)
    logErrors = \case
      Left ex -> logError (show ex) >> pure Nothing
      Right res -> pure res

logError :: MonadIO m => String -> m ()
logError = liftIO . IO.hPutStrLn IO.stderr

setDFlags :: GhcMonad m => String -> m DynFlags
setDFlags pkg_nm = do
  dflags <- do
    dflags <- getSessionDynFlags
    logger <- getLogger
    (dflags', _fileish_args, _dynamicFlagWarnings) <-
      GHC.parseDynamicFlags logger dflags args
    return dflags'
  _ <- setProgramDynFlags dflags
  pure dflags
  where
    args = map noLoc
      [ "-package=" ++ pkg_nm
      , "-dppr-cols=1000"
      , "-fprint-explicit-runtime-reps"
      , "-fprint-explicit-foralls"
      ]

getPprFun :: GhcMonad m => String -> m (SDoc -> T.Text)
getPprFun pkg_nm = do
  dflags <- setDFlags pkg_nm
  unit_state <- hsc_units <$> getSession
  name_ppr_ctx <- GHC.getNamePprCtx
  pure $ T.pack . showSDocForUser dflags unit_state name_ppr_ctx

getDefinitions :: (SDoc -> T.Text) -> String -> Ghc (Maybe DeclarationMap)
getDefinitions pprFun pkg_nm = do
  _ <- setDFlags pkg_nm
  unit_state <- hsc_units <$> getSession
  unit_id <- case lookupPackageName unit_state (PackageName $ fsLit pkg_nm) of
    Just unit_id -> return unit_id
    Nothing -> fail "failed to find package"
  unit_info <- case lookupUnitId unit_state unit_id of
    Just unit_info -> return unit_info
    Nothing -> fail "unknown package"
  mDefinitions <- reportUnitDecls pprFun unit_info
  liftIO $ IO.hPutStrLn IO.stderr $ "getDefinitions " ++ pkg_nm
  pure $ DeclarationMap unit_id <$> mDefinitions

reportUnitDecls :: (SDoc -> T.Text) -> UnitInfo -> Ghc (Maybe (Map ModuleName (Map Name (Json.FunctionType Type))))
reportUnitDecls pprFun unit_info = do
    let exposed :: [ModuleName]
        exposed = map fst (unitExposedModules unit_info)
    map' <- fmap (Map.fromList . catMaybes) $ forM exposed $ \moduleName' -> do
      map' <- reportModuleDecls pprFun (unitId unit_info) moduleName'
      pure $ if null map'
        then Nothing
        else Just (moduleName', map')
    pure $ if null map'
      then Nothing
      else Just map'

reportModuleDecls :: (SDoc -> T.Text) -> UnitId -> ModuleName -> Ghc (Map Name (Json.FunctionType Type))
reportModuleDecls pprFun unit_id modl_nm = do
    modl <- GHC.lookupQualifiedModule (OtherPkg unit_id) modl_nm
    mb_mod_info <- GHC.getModuleInfo modl
    mod_info <- case mb_mod_info of
      Nothing -> fail $ "Failed to find module: " ++ GHC.Utils.Outputable.showPprUnsafe modl
      Just mod_info -> return mod_info

    let names = GHC.modInfoExports mod_info
        sorted_names = sortBy (compare `on` nameOccName) names

        exported_occs :: [OccName]
        exported_occs = map nameOccName names

        is_exported :: OccName -> Bool
        is_exported occ = occ `elem` exported_occs

    things <- mapM GHC.lookupName sorted_names
    let contents =
            [ (varName _id, Json.FunctionType (scaledThing arg) res)
            | Just thing <- things
            , AnId _id <- [thing]
            , (arg, res) <- case splitFunTys $ varType _id of -- is it a function with exactly one argument?
                ([arg], res) -> [(arg, res)]
                (_, _) -> []
            , case tyThingParent_maybe thing of
                Just parent
                  | is_exported (getOccName parent) -> False
                _ -> True
            ]
    pure $ Map.fromList contents

data DeclarationMap = DeclarationMap
  { declarationMap_package :: UnitId
  , declarationMap_moduleDeclarations :: Map ModuleName (Map Name (Json.FunctionType Type))
    -- ^ -- A map from a module name to the declarations in that module
  }

declarationMapToJson
  :: (SDoc -> T.Text)
  -> DeclarationMap
  -> Json.DeclarationMapJson T.Text
declarationMapToJson pprFun dm =
  let
    eitherMap :: Map T.Text (Map T.Text (Either TyConParseError (Json.TypeInfo (FgType (FgTyCon T.Text)))))
    eitherMap = mapMap (declarationMap_moduleDeclarations dm) $ \(modName, nameMap) ->
      ( fullyQualify' modName
      , mapMapMaybe nameMap $ \(name, functionType) ->
          (noQualify' name, funtionTypeToTypeInfo (modName, name) functionType)
      )

  in Json.DeclarationMapJson
    { Json.declarationMapJson_package = fullyQualify' package
    , Json.declarationMapJson_moduleDeclarations =
        Json.ModuleDeclarations
          (nonEmptyMapMap $ mapEitherRight <$> eitherMap)
          (nonEmptyMapMap $ mapEitherLeft <$> eitherMap)
    }
  where
    package = declarationMap_package dm

    nonEmptyMapMap = Map.filter (not . Map.null)

    mapEitherLeft :: Map k (Either a b) -> Map k a
    mapEitherLeft map' = Map.mapMaybe id $ either Just (const Nothing) <$> map'

    mapEitherRight :: Map k (Either a b) -> Map k b
    mapEitherRight map' = Map.mapMaybe id $ either (const Nothing) Just <$> map'

    mapMap :: Ord k' => Map k a -> ((k, a) -> (k', a')) -> Map k' a'
    mapMap map' f = Map.fromList . map f . Map.toList $ map'

    mapMapMaybe :: Ord k' => Map k a -> ((k, a) -> (k', Maybe a')) -> Map k' a'
    mapMapMaybe map' f = Map.fromList . map (fmap fromJust) . filter (isJust . snd) . map f . Map.toList $ map'

    funtionTypeToTypeInfo
      :: (ModuleName, Name) -- for debugging purposes
      -> Json.FunctionType Type
      -> Maybe (Either TyConParseError (Json.TypeInfo (FgType (FgTyCon T.Text))))
    funtionTypeToTypeInfo dbg funType = do
      funTyExpanded <- traverse (toBuiltinType . expandTypeSynonyms) funType
      funTy <- traverse toBuiltinType funType
      let funTypeInfo = Json.TypeInfo
            { Json.typeInfo_fullyQualified = funTyExpanded
            , Json.typeInfo_tmpUnexpanded = funTy
            }
      pure $ traverse (traverse (tyConToFgTyCon dbg)) funTypeInfo

    fullyQualify', noQualify' :: Outputable a => a -> T.Text
    fullyQualify' = pprFun . fullyQualify
    noQualify' = pprFun . noQualify

    tyConToFgTyCon
      :: HasCallStack
      => (ModuleName, Name) -- for debugging purposes
      -> TyCon
      -> Either TyConParseError (FgTyCon T.Text)
    tyConToFgTyCon (modName, functionName) tyCon =
      first (TyConParseError . bugMsg) . parsePprTyCon . fullyQualify' $ tyCon
      where
        fullyQualify' = pprFun . fullyQualify

        bugMsg e = T.unpack $ T.unwords
          [ "tyConToFgTyCon: parsing failed.", T.pack e <> "."
          , "Function:", pprFun (ppr functionName) <> "."
          , "Package:", pprFun (ppr package) <> "."
          , "Module:", pprFun (ppr modName) <> "."
          , "Unique:" , pprFun (ppr $ getUnique tyCon) <> "."
          , "SrcLoc:", pprFun (ppr $ getSrcLoc tyCon) <> "."
          ]

fullyQualify :: Outputable a => a -> SDoc
fullyQualify =
  withUserStyle fullyQualify' AllTheWay . ppr
  where
    fullyQualify' :: NamePprCtx
    fullyQualify' =
      QueryQualify
        { queryQualifyName = \_ _ -> NameNotInScope2
        , queryQualifyModule = const True
        , queryQualifyPackage = const True
        , queryPromotionTick = const True
        }

noQualify :: Outputable a => a -> SDoc
noQualify =
  withUserStyle noQualify' AllTheWay . ppr
  where
    noQualify' =
      QueryQualify
        { queryQualifyName = \_ _ -> NameUnqual
        , queryQualifyModule = const False
        , queryQualifyPackage = const False
        , queryPromotionTick = const True
        }

-- | Parse a 'FgTyCon' from a 'TyCon' pretty-printed in fully-qualified form
--  (e.g. "base-4.18.0.0:Data.Either.Either").
--
-- NOTE: Does not handle the /built-in/ types: list, tuple (including unit).
--
-- A 'Left' signifies a bug in the parser.
--
-- Examples:
--
-- >>> parsePprTyCon "base-4.18.0.0:Data.Either.Either"
-- Right (FgTyCon {fgTyConName = "Either", fgTyConModule = "Data.Either", fgTyConPackageName = "base", fgTyConPackageVersion = "4.18.0.0"})
--
-- >>> parsePprTyCon "text-2.0.2:Data.Text.Internal.Text"
-- Right (FgTyCon {fgTyConName = "Text", fgTyConModule = "Data.Text.Internal", fgTyConPackageName = "text", fgTyConPackageVersion = "2.0.2"})
--
-- >>> parsePprTyCon "base-4.18.0.0:GHC.Maybe.Maybe"
-- Right (FgTyCon {fgTyConName = "Maybe", fgTyConModule = "GHC.Maybe", fgTyConPackageName = "base", fgTyConPackageVersion = "4.18.0.0"})
parsePprTyCon :: T.Text -> Either String (FgTyCon T.Text)
parsePprTyCon str = do
  (packageAndVersion, fqn) <- case T.splitOn ":" str of
    [packageAndVersion, fqn] -> pure (packageAndVersion, fqn)
    _ -> Left $ "missing colon in " <> show (T.unpack str)
  (packageName, packageVersion) <-
    splitByNonEmpty "invalid package identifier" '-' packageAndVersion
  (moduleName, name) <-
    splitByNonEmpty "invalid fully qualified identifier" '.' fqn
  pure $ FgTyCon
    { fgTyConName = name
    , fgTyConModule = moduleName
    , fgTyConPackageName = packageName
    , fgTyConPackageVersion = packageVersion
    }
  where
    -- split by "char" and return pair of non-empty text strings
    splitByNonEmpty err char str =
      case T.spanEndM (pure . (/= char)) str of
        Identity (a', b)
          | Just (a, char) <- T.unsnoc a' -- remove trailing "char"
          , not (T.null b) && not (T.null a) ->
            pure (a, b)
        _ -> Left $ err <> " in " <> show (T.unpack str)

-- | Convert a 'TyConApp' 'Type' to a 'FgType'
toBuiltinType :: Type -> Maybe (FgType TyCon)
toBuiltinType !ty = case ty of
  TyConApp tyCon [] | isTupleTyCon tyCon -> do -- unit
      pure FgType_Unit
  TyConApp tyCon (ty1:ty2:tyTail) | Just boxity <- tupleBoxity tyCon -> do -- tuple (of size >= 2)
      ty1' <- toBuiltinType ty1
      tyTail' <- mapM toBuiltinType (ty2 NE.:| tyTail)
      pure $ FgType_Tuple boxity ty1' tyTail'
  TyConApp tyCon [ty1] | getUnique tyCon == listTyConKey -> do -- list
    ty1' <- toBuiltinType ty1
    pure $ FgType_List ty1'
  TyConApp tyCon tyList -> do -- neither a tuple nor a list
    tyList' <- mapM toBuiltinType tyList
    pure $ FgType_TyConApp tyCon tyList'
  _ -> Nothing
  where
    tupleBoxity tyCon
      | isUnboxedTupleTyCon tyCon = Just Types.Unboxed
      | isBoxedTupleTyCon tyCon = Just Types.Boxed
      | otherwise = Nothing
