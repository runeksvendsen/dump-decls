{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Exe
( main
, StdoutJsonFormat
)
where

import Types
import qualified Json
import Types.Doodle -- TODO
import GHC hiding (moduleName)
import qualified GHC.Paths
import GHC.Core.Type (splitFunTys, expandTypeSynonyms, isLiftedTypeKind, isConstraintKind, returnsConstraintKind, typeKind)
import GHC.Driver.Ppr (showSDocForUser)
import GHC.Unit.State (lookupUnitId, lookupPackageName, pprWithUnitState)
import GHC.Unit.Info (UnitInfo, unitExposedModules, unitId, PackageName(..))
import GHC.Unit.Types (UnitId)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Env (hsc_units)
import GHC.Utils.Outputable hiding (sep, (<>))
import GHC.Types.TyThing (tyThingParent_maybe)
import GHC.Types.Name (nameOccName, getSrcLoc)
import GHC.Types.Name.Occurrence (OccName)
import GHC.Types.Var (varName, varType, VarBndr (Bndr), tyVarKind)
import Data.Function (on)
import Data.List (sortBy, foldl')
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
import GHC.Core.TyCo.Rep (Type(..), KindOrType)
import GHC.Core.TyCon (isUnboxedTupleTyCon, isBoxedTupleTyCon, isTupleTyCon)
import GHC.Builtin.Names (listTyConKey, getUnique)
import qualified Data.Text as T
import Data.Bifunctor (bimap, first)
import GHC.Stack (HasCallStack)
import Data.Functor.Identity (Identity(Identity))
import Debug.Trace (trace)
import qualified Types.Forall as Forall
import Data.Either (fromLeft, fromRight)
import Data.Functor ((<&>), void)
import qualified Data.Text.IO as TIO
import qualified GHC.Driver.Session

-- | The output printed to stdout can be parsed as JSON into this data type.
--
--   Using e.g. @Data.Aeson.decode :: Data.ByteString.Lazy.ByteString -> Maybe StdoutJsonFormat@
type StdoutJsonFormat = [Json.DeclarationMapJson T.Text]

main :: IO ()
main = do
  args <- getArgs
  let runGhc' :: FilePath -> Ghc a -> IO (Either Control.Monad.Catch.SomeException a)
      runGhc' libdir action = reallyCatch $ runGhc (Just libdir) action
  (pprFun, ghcLibDir, pkg_names) <- case args of
    [] -> Exit.die "Missing argument(s): one or more packages"
    ghcLibDir : pkg_names@(first_package_name : _) ->
      runGhc' ghcLibDir (getPprFun first_package_name) >>= either (fail . show) (\pprFun -> pure $ (pprFun, ghcLibDir, pkg_names))
  lst <- forM pkg_names $ \pkg_nm -> do
    unsafeInterleaveIO $ runGhc' ghcLibDir (getDefinitions (pprFun . pprSuppressVarKinds) pkg_nm) >>= logErrors
  let declarationMapJsonList = map (declarationMapToJson (pprFun . pprSuppressVarKinds) (Just . Right)) (catMaybes lst)
  forM_ declarationMapJsonList $ \declarationMapJson -> do
    let errors = Map.assocs $ Map.assocs <$> Json.moduleDeclarations_mapFail (Json.declarationMapJson_moduleDeclarations declarationMapJson)
    forM_ errors $ \(modName, pkgErrs) ->
      forM_ pkgErrs $ \(defnName, err) ->
        logError $ T.unpack $ T.unwords
          [ "WARNING:"
          , T.pack $ show (tyConParseErrorInput err)
          , "failed to parse" <> "."
          , renderTyConParseError err
          ]
  Json.streamPrintJsonList (declarationMapJsonList :: StdoutJsonFormat)
  where
    reallyCatch :: IO a -> IO (Either Control.Exception.SomeException a)
    reallyCatch ioAction =
      Control.Exception.catch
        (Right <$> (ioAction >>= Control.Exception.evaluate))
        $ \e -> case Control.Exception.fromException e :: Maybe Ex.AsyncException of
            Nothing -> pure . Left $ e
            Just eAsync -> logError ("Caught async exception: " ++ show eAsync) >> Ex.throwIO eAsync

    logErrors
      :: Either Control.Monad.Catch.SomeException (Maybe (DeclarationMap ty))
      -> IO (Maybe (DeclarationMap ty))
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
  pure $ T.pack . showSDocForUser' dflags unit_state name_ppr_ctx
  where
    showSDocForUser' dflags unit_state name_ppr_ctx doc =
      let sty  = mkUserStyle name_ppr_ctx AllTheWay
          doc' = GHC.Unit.State.pprWithUnitState unit_state doc
          sDocContext = GHC.Driver.Session.initSDocContext dflags sty
          blahTodo sDocContext' = sDocContext'{sdocSuppressVarKinds = True, sdocPrintExplicitKinds = False, sdocStarIsType = True}
      in renderWithContext (blahTodo sDocContext) doc'

getDefinitions :: (SDoc -> T.Text) -> String -> Ghc (Maybe (DeclarationMap (FgType (FgTyCon T.Text))))
getDefinitions pprFun pkg_nm = do
  _ <- setDFlags pkg_nm
  unit_state <- hsc_units <$> getSession
  unit_id <- case lookupPackageName unit_state (PackageName $ fsLit pkg_nm) of
    Just unit_id -> return unit_id
    Nothing -> fail "failed to find package"
  unit_info <- case lookupUnitId unit_state unit_id of
    Just unit_info -> return unit_info
    Nothing -> fail "unknown package"
  liftIO $ IO.hPutStrLn IO.stderr $ "   getDefinitions " ++ pkg_nm
  mDefinitions <- reportUnitDecls pprFun unit_info
  forM_ mDefinitions $ \defs -> do
    let blah = concat $ map (ppFunctionMap pprFun) (Map.elems defs)
    void $ liftIO $ mapM (TIO.hPutStrLn IO.stderr) blah
  let f :: Map ModuleName FunctionMap -> Map ModuleName (Map Name (Json.FunctionType (FgType (FgTyCon T.Text))))
      f = fmap $ Map.mapMaybe $ either Just (const Nothing)
  pure $ DeclarationMap unit_id . f <$> mDefinitions

ppFunctionMap
  :: (SDoc -> T.Text)
  -> FunctionMap
  -> [T.Text]
ppFunctionMap pprFun fm = catMaybes $
   -- WIP: ignore non-forall functions for now
  Map.toList fm <&> \(name, fun) ->
    either (const Nothing) (ppTodo name) fun
  where
    ppTodo name !fun = Just $
      T.unwords
        [ pprFun (ppr name)
        , "::"
        , prettyPrintFTFGeneric renderFgTyConUnqualified fun
        ]

prettyPrintFunction
  :: Either
      (Json.FunctionType (FgType (FgTyCon T.Text)))
      (FunctionTypeForall T.Text T.Text)
  -> T.Text
prettyPrintFunction =
  either prettyPrintFT prettyPrintFTF

prettyPrintFT :: Json.FunctionType (FgType (FgTyCon T.Text)) -> T.Text
prettyPrintFT ft = T.unwords
  [ renderFgType renderFgTyConQualified (Json.functionType_arg ft)
  , "->"
  , renderFgType renderFgTyConQualified (Json.functionType_ret ft)
  ]

prettyPrintFTF :: FunctionTypeForall T.Text T.Text -> T.Text
prettyPrintFTF ftf = T.unwords
  [ Forall.renderForall id $ ftf_forall ftf
  , renderFgType' $ ftf_arg ftf
  , "->"
  , renderFgType' $ ftf_ret ftf
  ]
  where
    renderFgType' :: FgType (Either (FgTyCon T.Text) (Forall.TyVar T.Text)) -> T.Text
    renderFgType' =
      renderFgType (either renderFgTyConQualifiedNoPackage Forall.getTyVar)

prettyPrintFTFGeneric
  :: (FgTyCon T.Text -> T.Text)
  -> FunctionTypeForall T.Text T.Text
  -> T.Text
prettyPrintFTFGeneric renderFgTyCon ftf = T.unwords
  [ Forall.renderForall id $ ftf_forall ftf
  , renderFgType' $ ftf_arg ftf
  , "->"
  , renderFgType' $ ftf_ret ftf
  ]
  where
    renderFgType' :: FgType (Either (FgTyCon T.Text) (Forall.TyVar T.Text)) -> T.Text
    renderFgType' =
      renderFgType (either renderFgTyCon Forall.getTyVar)

type FunctionMap =
  Map
    Name
    (Either
      (Json.FunctionType (FgType (FgTyCon T.Text)))
      (FunctionTypeForall T.Text T.Text)
    )

reportUnitDecls :: (SDoc -> T.Text) -> UnitInfo -> Ghc (Maybe (Map ModuleName FunctionMap))
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

reportModuleDecls
  :: (SDoc -> T.Text)
  -> UnitId
  -> ModuleName
  -> Ghc FunctionMap
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
            [ (varName _id, blah)
            | Just thing <- things
            , AnId _id <- [thing]
            , Just blah <-
                let ty = expandTypeSynonyms $ varType _id -- NOTE: we need to expand type synonyms because two types are considered equal only if their 'FgType' representations are equal (==). And a type synonym is a distinct 'TyConApp', which means it'll become a distinct 'FgType'.
                in [parseType pprFun unit_id (modl_nm, varName _id) ty]
            , case tyThingParent_maybe thing of
                Just parent
                  | is_exported (getOccName parent) -> False
                _ -> True
            ]
    pure $ Map.fromList contents

-- TODO: postpone conversion of GHC 'Type' to 'FgType'? Or just use 'funtionTypeExpandAndConvertToFgType' in here?
parseType
  :: (SDoc -> T.Text)
  -> UnitId
  -> (ModuleName, Name)
  -> Type
  -> Maybe -- a 'Just' if this type is supported
      (Either
        (Json.FunctionType (FgType (FgTyCon T.Text))) -- only concrete types
        (FunctionTypeForall T.Text T.Text) -- both concrete types and type variables
      )
parseType pprFun package dbg tyInit =
  case splitFunTys tyInit of
    ([], res) ->
      case res of
        ForAllTy bndr ty' | isTypeKind bndr ->
          Right <$> goForall (parseForall Nothing bndr) ty'
        _ -> Nothing
    ([_], _) ->
      Left <$> goSimple tyInit
    _ -> Nothing
  where
    goForall forall_ ty =
      case splitFunTys ty of
        ([], res) ->
          case res of
            ForAllTy bndr ty' | isTypeKind bndr -> do
              goForall (parseForall (Just forall_) bndr) ty'
            _ -> Nothing
        ([arg], res) | not (returnsConstraintKind $ GHC.Core.Type.typeKind (scaledThing arg)) -> do
            arg' <- toFgType' pprFun $ scaledThing arg
            res' <- toFgType' pprFun res
            let eResult = do
                  arg'' <- traverse (tyConOrTyVarTODO pprFun package dbg forall_) arg'
                  res'' <- traverse (tyConOrTyVarTODO pprFun package dbg forall_) res'
                  let debugPrintDiff ftf =
                        let ftfTxt = prettyPrintFTFGeneric renderFgTyConQualified ftf
                            ftfTxtGhc = pprFun $ fullyQualify $ ppr tyInit
                            nameTxt = pprFun $ ppr (snd dbg)
                        -- TODO: add this to a test suite!!
                        in if ftfTxt /= ftfTxtGhc
                          then T.unpack ("DIFF: " <> nameTxt <> "\n      " <> ftfTxt <> "\n      " <> ftfTxtGhc <> "\n") `trace` ftf
                          else ftf
                  pure $ (if doDebugPrintDiff then debugPrintDiff else id) $ mkFunctionTypeForall forall_ arg'' res''
            pure $
              either
              throwError -- WIP: don't throw exception
              id
              eResult
        (_, _) -> Nothing

    doDebugPrintDiff = True

    goSimple ty =
      case splitFunTys ty of
        ([arg], res) -> do
          arg' <- toFgType pprFun $ scaledThing arg
          res' <- toFgType pprFun res
          let eResult = do
                arg'' <- traverse (tyConToFgTyCon pprFun package dbg) arg'
                res'' <- traverse (tyConToFgTyCon pprFun package dbg) res'
                pure $ Json.FunctionType arg'' res''
          pure $
            either
            throwError -- WIP: don't throw exception
            id
            eResult
        _ -> Nothing

    parseForall mForall (Bndr tyCoVar _) =
      let mkForall = maybe (Right . Forall.singleton) (\forall' -> (`Forall.appendTyVar` forall')) mForall
          tyVarName = pprFun (ppr tyCoVar) -- WIP: correct?
          eForall = mkForall tyVarName
      in either throwError id eForall -- WIP: don't throw exception

    throwError showable =
      error $ show showable ++ " -- " ++ T.unpack (pprFun $ ppr tyInit)

    -- is kind *
    isTypeKind (Bndr tyVar _) =
      isLiftedTypeKind (tyVarKind tyVar) && not (isConstraintKind $ tyVarKind tyVar)

data DeclarationMap ty = DeclarationMap
  { declarationMap_package :: UnitId
  , declarationMap_moduleDeclarations :: Map ModuleName (Map Name (Json.FunctionType ty))
    -- ^ -- A map from a module name to the declarations in that module
  }

declarationMapToJson
  :: forall ty.
     (SDoc -> T.Text)
  -> (ty -> Maybe (Either TyConParseError (FgType (FgTyCon T.Text))))
  -> DeclarationMap ty
  -> Json.DeclarationMapJson T.Text
declarationMapToJson pprFun tyToFgType dm =
  let
    eitherMap :: Map T.Text (Map T.Text (Either TyConParseError (Json.FunctionType (FgType (FgTyCon T.Text)))))
    eitherMap = mapMap (declarationMap_moduleDeclarations dm) $ \(modName, nameMap) ->
      ( fullyQualify' modName
      , mapMapMaybe nameMap $ \(name, functionType) ->
          (noQualify' name, funtionTypeConvertToFgType (modName, name) functionType)
      )

  in Json.DeclarationMapJson
    { Json.declarationMapJson_package = parsePackageFromUnitId pprFun package
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

    funtionTypeConvertToFgType
      :: (ModuleName, Name) -- for debugging purposes
      -> Json.FunctionType ty
      -> Maybe (Either TyConParseError (Json.FunctionType (FgType (FgTyCon T.Text))))
    funtionTypeConvertToFgType dbg funType = do
      let convert :: Type -> Maybe (Either TyConParseError (FgType (FgTyCon T.Text)))
          convert = fmap (traverse (tyConToFgTyCon pprFun package dbg)) . toFgType pprFun
      sequenceA <$> traverse tyToFgType funType

    fullyQualify', noQualify' :: Outputable a => a -> T.Text
    fullyQualify' = pprFun . fullyQualify
    noQualify' = pprFun . noQualify

data TodoError -- WIP
  = TodoError_Forall (Forall.ForallError T.Text)
  | TodoError_TyCon TyConParseError
      deriving (Eq, Show)

tyConOrTyVarTODO
  :: (SDoc -> T.Text)
  -> UnitId
  -> (ModuleName, Name) -- for debugging purposes
  -> Forall.Forall T.Text
  -> Either TyCon TyVar
  -> Either TodoError (Either (FgTyCon T.Text) (Forall.TyVar T.Text))
tyConOrTyVarTODO pprFun package dbg forall_ =
  either
    (fmap Left . first TodoError_TyCon . tyConToFgTyCon pprFun package dbg)
    (fmap Right . first TodoError_Forall . tyVarToTODO pprFun package dbg forall_)

tyVarToTODO
  :: (SDoc -> T.Text)
  -> UnitId
  -> (ModuleName, Name) -- for debugging purposes
  -> Forall.Forall T.Text
  -> TyVar
  -> Either (Forall.ForallError T.Text) (Forall.TyVar T.Text)
tyVarToTODO pprFun package dbg forall_ tyVar =
  let getTyVarName = pprFun . ppr -- WIP: correct?
  in Forall.lookupTyVar (getTyVarName tyVar) forall_

tyConToFgTyCon
  :: (SDoc -> T.Text)
  -> UnitId
  -> (ModuleName, Name) -- for debugging purposes
  -> TyCon
  -> Either TyConParseError (FgTyCon T.Text)
tyConToFgTyCon pprFun package (modName, functionName) tyCon =
  first mkTyConParseError . parsePprTyCon $ tyConPpr
  where
    tyConPpr = fullyQualify' tyCon

    fullyQualify' = pprFun . fullyQualify

    mkTyConParseError e = TyConParseError
      { tyConParseErrorMsg = e
      , tyConParseErrorInput = tyConPpr
      , tyConParseErrorFunctionName = pprFun (ppr functionName)
      , tyConParseErrorPackage = parsePackageFromUnitId pprFun package
      , tyConParseErrorSrcLoc = pprFun (ppr $ getSrcLoc tyCon)
      }

pprSuppressVarKinds :: SDoc -> SDoc
pprSuppressVarKinds =
  updSDocContext (\sDocContext -> sDocContext{sdocSuppressVarKinds = True, sdocPrintExplicitKinds = False})

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

-- | Convert a 'TyConApp' to a 'FgType TyCon'
tyConAppToFgTypeTyCon
  :: (SDoc -> T.Text)
  -> (KindOrType -> Maybe (FgType (Either TyCon a)))
     -- ^ Recursive case
     --
     -- TODO: why 'Maybe' and 'Either'?
  -> TyCon
     -- ^ First argument to 'TyConApp'
  -> [KindOrType]
     -- ^ Second argument to 'TyConApp'
  -> Maybe (FgType (Either TyCon a))
tyConAppToFgTypeTyCon pprFun recurse tyCon = \case
  [] | isTupleTyCon tyCon, Just boxity <- tupleBoxity -> do -- unit
      pure $ FgType_Unit boxity
  args@(_:_:_) | Just boxity <- tupleBoxity -> do -- tuple (of size >= 2)
      args' <- mapM recurse args
      pure $ FgType_Tuple boxity (fromIntegral $ tyConArity tyCon) args'
  mTy | getUnique tyCon == listTyConKey -> do -- list
    case mTy of
      [] -> pure $ FgType_List Nothing
      [ty1] -> do
        ty1' <- recurse ty1
        pure $ FgType_List (Just ty1')
      _ -> Nothing
  tyList -> do -- neither a tuple nor a list
    tyList' <- mapM recurse tyList
    pure $ FgType_TyConApp (Left tyCon) tyList'
  where
    tupleBoxity
      | isUnboxedTupleTyCon tyCon = Just Types.Unboxed
      | isBoxedTupleTyCon tyCon = Just Types.Boxed
      | otherwise = Nothing

-- | Convert a 'Type' to a 'FgType'. Only 'TyConApp' is supported currently.
toFgType :: (SDoc -> T.Text) -> Type -> Maybe (FgType TyCon)
toFgType pprFun =
  go
  where
    go = \case
      TyConApp tyCon tyConList ->
        fmap (fromLeft (error "toFgType: impossible")) <$>
          tyConAppToFgTypeTyCon pprFun (fmap (fmap Left) . go) tyCon tyConList
      _ -> Nothing

toFgType'
  :: (SDoc -> T.Text)
  -> Type
  -> Maybe (FgType (Either TyCon TyVar))
toFgType' pprFun ty =
  go ty
  where
    go = \case
      TyConApp tyCon tyConList ->
        tyConAppToFgTypeTyCon pprFun go tyCon tyConList
      TyVarTy tyVar ->
        pure $ FgType_TyConApp (Right tyVar) []
      appTy@AppTy{} -> do
        -- Flatten nested AppTy's. Ie. converting nested AppTy's into (1) the "function" type variable and (2) the "argument" type variable(s)/constructor(s).
        -- E.g. from "((f a) b) c" to "FgType_TyConApp (Right f) [a, b, c]"
        let goAppTy
              :: [FgType (Either TyCon TyVar)] -- "argument" accumulator. accumulates the second argument to "AppTy" (ie. the "argument" type).
              -> Type -- the first argument to 'AppTy' (ie. the "function" type variable)
              -> Maybe (FgType (Either TyCon TyVar))
            goAppTy acc = \case
              tyVarTy@TyVarTy{} -> go tyVarTy
              AppTy fun2 arg2 -> do
                arg2' <- go arg2
                goAppTy (arg2' : acc) fun2
              TyConApp{} ->
                -- 'TyConApp' is not allowed as first argument to 'AppTy'.
                -- See docs: https://hackage.haskell.org/package/ghc-9.6.1/docs/GHC-Core-TyCo-Rep.html#v:AppTy
                error $ unwords
                  [ "Unexpected TyConApp as first argument to AppTy:"
                  , (T.unpack . pprFun . ppr $ appTy) <> "."
                  , "Outer type:"
                  , T.unpack . pprFun . ppr $ ty
                  ]
              _ -> Nothing
        goAppTy [] appTy
      _ -> Nothing

parsePackageFromUnitId
  :: (SDoc -> T.Text)
  -> UnitId
  -> FgPackage T.Text
parsePackageFromUnitId pprFun unitId =
  either (error . ("BUG: parsePackageFromUnitId: " <>)) id (parsePackageWithVersion $ fullyQualify' unitId)
  where
    fullyQualify' = pprFun . fullyQualify
