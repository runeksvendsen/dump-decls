{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}
{-# LANGUAGE BangPatterns #-}
module Main
( main
)
where

import Types
import qualified Json
import GHC
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
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (OccName)
import GHC.Types.Var (varName, varType)
import Data.Function (on)
import Data.List (sortBy)
import System.Environment (getArgs)
import Prelude hiding ((<>))
import Control.Monad (forM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified System.Exit as Exit
import Control.Monad.IO.Class (liftIO, MonadIO)
import GHC.IO.Unsafe (unsafeInterleaveIO)
import qualified System.IO as IO
import Data.Maybe (catMaybes, fromMaybe)
import qualified Control.Exception as Ex
import GHC.Core.Multiplicity (scaledThing)
import qualified Control.Monad.Catch
import qualified Control.Exception
import GHC.Core.TyCo.Rep (Type(TyConApp))
import GHC.Core.TyCon (isUnboxedTupleTyCon, isBoxedTupleTyCon)
import GHC.Builtin.Names (listTyConKey, getUnique)
import qualified Data.Text as T

main :: IO ()
main = do
  pkg_names <- getArgs
  let runGhc' :: Ghc a -> IO (Either Control.Monad.Catch.SomeException a)
      runGhc' action = reallyCatch $ runGhc (Just GHC.Paths.libdir) action
  pprFun <- case pkg_names of
    [] -> Exit.die "Missing argument(s): one or more packages"
    first_package_name : _ -> runGhc' (getPprFun first_package_name) >>= either (fail . show) pure
  lst <- forM pkg_names $ \pkg_nm -> do
    unsafeInterleaveIO $ runGhc' (getDefinitions pkg_nm) >>= logErrors
  Json.streamPrintJsonList $ map (declarationMapToJson pprFun) (catMaybes lst)
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

getDefinitions :: String -> Ghc (Maybe DeclarationMap)
getDefinitions pkg_nm = do
  _ <- setDFlags pkg_nm
  unit_state <- hsc_units <$> getSession
  unit_id <- case lookupPackageName unit_state (PackageName $ fsLit pkg_nm) of
    Just unit_id -> return unit_id
    Nothing -> fail "failed to find package"
  unit_info <- case lookupUnitId unit_state unit_id of
    Just unit_info -> return unit_info
    Nothing -> fail "unknown package"
  mDefinitions <- reportUnitDecls unit_info
  liftIO $ IO.hPutStrLn IO.stderr $ "getDefinitions " ++ pkg_nm
  pure $ DeclarationMap unit_id <$> mDefinitions

reportUnitDecls :: UnitInfo -> Ghc (Maybe (Map ModuleName (Map Name (Json.FunctionType Type))))
reportUnitDecls unit_info = do
    let exposed :: [ModuleName]
        exposed = map fst (unitExposedModules unit_info)
    map' <- fmap (Map.fromList . catMaybes) $ forM exposed $ \moduleName' -> do
      map' <- reportModuleDecls (unitId unit_info) moduleName'
      pure $ if null map'
        then Nothing
        else Just (moduleName', map')
    pure $ if null map'
      then Nothing
      else Just map'

reportModuleDecls :: UnitId -> ModuleName -> Ghc (Map Name (Json.FunctionType Type))
reportModuleDecls unit_id modl_nm = do
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

        -- TODO: only works for "outer" types; e.g. "[Char]" but not "Maybe [Char]".
        toBuiltinType :: Type -> BuiltinType Type
        toBuiltinType !ty = case ty of
          TyConApp tyCon (ty1:ty2:tyTail) -> -- tuple (of size >= 2)
            let mBoxity -- 'Nothing' if it's not a tuple
                  | isUnboxedTupleTyCon tyCon = Just Types.Unboxed
                  | isBoxedTupleTyCon tyCon = Just Types.Boxed
                  | otherwise = Nothing
            in fromMaybe (BuiltinType_Type ty) $ do
              boxity <- mBoxity
              pure $ BuiltinType_Tuple
                boxity
                (toBuiltinType ty1)
                (NE.map toBuiltinType $ ty2 NE.:| tyTail)
          TyConApp tyCon [ty1] | getUnique tyCon == listTyConKey -> -- list
            BuiltinType_List (toBuiltinType ty1)
          _ -> BuiltinType_Type ty -- neither a tuple nor list

    things <- mapM GHC.lookupName sorted_names
    let contents =
            [ (varName _id, Json.FunctionType (toBuiltinType $ scaledThing arg) (toBuiltinType res))
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
  Json.DeclarationMapJson
    { Json.declarationMapJson_package = fullyQualify' $ declarationMap_package dm
    , Json.declarationMapJson_moduleDeclarations = Json.ModuleDeclarations $
        mapMap (declarationMap_moduleDeclarations dm) $ \(modName, nameMap) ->
          ( fullyQualify' modName
          , mapMap nameMap $ \(name, funType) ->
              ( noQualify' name
              , fmap (fullyQualify' . expandTypeSynonyms) funType
              )
          )
    }
  where
    mapMap :: Ord k' => Map k a -> ((k, a) -> (k', a')) -> Map k' a'
    mapMap map' f = Map.fromList . map f . Map.toList $ map'

    fullyQualify', noQualify' :: Outputable a => a -> T.Text
    fullyQualify' = pprFun . withUserStyle fullyQualify AllTheWay . ppr
    noQualify' = pprFun . withUserStyle noQualify AllTheWay . ppr

    noQualify =
      QueryQualify
        { queryQualifyName = \_ _ -> NameUnqual
        , queryQualifyModule = const False
        , queryQualifyPackage = const False
        , queryPromotionTick = const True
        }

    fullyQualify :: NamePprCtx
    fullyQualify =
      QueryQualify
        { queryQualifyName = \_ _ -> NameNotInScope2
        , queryQualifyModule = const True
        , queryQualifyPackage = const True
        , queryPromotionTick = const True
        }
