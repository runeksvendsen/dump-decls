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
import GHC.Core.Type (splitFunTys, expandTypeSynonyms, isLiftedTypeKind, isConstraintKind, returnsConstraintKind, typeKind)
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
import Data.List (sortBy)
import System.Environment (getArgs)
import Control.Monad (forM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified System.Exit as Exit
import Control.Monad.IO.Class (liftIO, MonadIO)
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
import Data.Bifunctor (first)
import qualified Types.Forall as Forall
import Data.Either (fromLeft)
import qualified GHC.Driver.Session
import qualified Types.Doodle as Doodle
import Control.Monad.Trans.Except (ExceptT, throwE, except, withExceptT, Except, runExcept)
import Debug.Trace (trace)
import qualified Types.FunctionInfo
import Types.FunctionInfo (FunctionInfo)
import qualified GHC.Types.Unique
import qualified Streaming.Prelude as S

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
    ghcLibDir : pkg_names@(first_package_name : _) ->
      runGhc' ghcLibDir (getPprFun first_package_name) >>= either (fail . show) (\pprFun -> pure $ (pprFun, ghcLibDir, pkg_names))
    _ -> Exit.die "Missing arguments: GHC libdir and one or more packages"
  let throwError = either
        (\err -> logError (show err) >> Control.Exception.throwIO err)
        pure

  let
    declarationMapToJson' pkg_nm =
      declarationMapToJson pprFun <$> getDefinitions (pprFun . pprSuppressVarKinds) pkg_nm

    getDefinitionsHandleErrors pkg_nm =
        reallyCatch (declarationMapToJson' pkg_nm)
          >>= logErrors

  let stream :: S.Stream (S.Of (Json.DeclarationMapJson T.Text)) Ghc ()
      stream =
          S.catMaybes
        $ S.mapM getDefinitionsHandleErrors
        $ S.each pkg_names

  (throwError =<<) $ runGhc' ghcLibDir $ Json.streamPrintJson stream
  where
    reallyCatch
      :: (MonadIO m, Control.Monad.Catch.MonadCatch m)
      => m a
      -> m (Either Control.Exception.SomeException a)
    reallyCatch action =
      Control.Monad.Catch.catch
        (action >>= (liftIO . Control.Exception.evaluate) . Right)
        $ \e -> case Control.Exception.fromException e :: Maybe Ex.AsyncException of
            Nothing -> pure . Left $ e
            Just eAsync -> logError ("Caught async exception: " ++ show eAsync) >> Control.Monad.Catch.throwM eAsync

    logErrors
      :: MonadIO m
      => Either Control.Monad.Catch.SomeException a
      -> m (Maybe a)
    logErrors = \case
      Left ex -> logError (show ex) >> pure Nothing
      Right res -> pure (Just res)

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

getDefinitions
  :: (SDoc -> T.Text)
  -> String
  -> Ghc (DeclarationMap (FunctionInfo (Either FgError SomeFunction)))
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
  definitions <- reportUnitDecls pprFun unit_info
  let f :: Map ModuleName FunctionMap
        -> Map ModuleName (Map Name (FunctionInfo (Either FgError Doodle.SomeFunction)))
      f = fmap (fmap (fmap (fmap eitherToSomeFunction . runExcept)))
  pure $ DeclarationMap unit_id (f definitions)

type FunctionMap =
  Map
    Name
    (Types.FunctionInfo.FunctionInfo
      (Except
        FgError
        (Either
          (FunctionType (FgType (FgTyCon T.Text)))
          (FunctionTypeForall T.Text T.Text)
        )
      )
    )

reportUnitDecls :: (SDoc -> T.Text) -> UnitInfo -> Ghc (Map ModuleName FunctionMap)
reportUnitDecls pprFun unit_info = do
    let exposed :: [ModuleName]
        exposed = map fst (unitExposedModules unit_info)
    fmap (Map.fromList . catMaybes) $ forM exposed $ \moduleName' -> do
      map' <- reportModuleDecls pprFun (unitId unit_info) moduleName'
      pure $ if null map'
        then Nothing
        else Just (moduleName', map')

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

    let mkTraceString _id = T.unpack $ T.unwords
          [ "(" <> pprFun (ppr $ getUnique (varName _id)) <> ")"
          , "[" <> pprFun (ppr unit_id) <> "]"
          , pprFun (ppr $ varName _id)
          , "::"
          , pprFun (ppr $ varType _id)
          ]
    things <- mapM GHC.lookupName sorted_names
    let contents =
            [ mkTraceString _id `trace`
                ( varName _id
                , Types.FunctionInfo.mkFunctionInfo (GHC.Types.Unique.getKey $ getUnique $ varName _id) fn
                )
            | Just thing <- things
            , AnId _id <- [thing]
            , Just fn <-
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
  :: forall m.
     (Traversable m, Monad m)
  => (SDoc -> T.Text)
  -> UnitId
  -> (ModuleName, Name)
  -> Type
  -> Maybe
      (ExceptT
        FgError
        m
        (Either
            (FunctionType (FgType (FgTyCon T.Text))) -- only concrete types
            (FunctionTypeForall T.Text T.Text) -- both concrete types and type variables
        )
      )
    -- ^ 'Nothing': type not supported
    --   'Just': type supported
parseType pprFun package dbg tyInit = sequenceA $
  case splitFunTys tyInit of
    ([], res) ->
      case res of
        ForAllTy bndr ty' | isTypeKind bndr -> do
          forall_ <- parseForall Nothing bndr
          fmap Right <$> goForall forall_ ty'
        _ -> pure Nothing
    ([_], _) ->
      fmap Left <$> goSimple tyInit
    _ -> pure Nothing
  where
    goForall
      :: Forall.Forall T.Text
      -> Type
      -> ExceptT
          FgError
          m
          (Maybe (FunctionTypeForall T.Text T.Text))
    goForall forall_ ty =
      case splitFunTys ty of
        ([], res) ->
          case res of
            ForAllTy bndr ty' | isTypeKind bndr -> do
              forall_' <- parseForall (Just forall_) bndr
              goForall forall_' ty'
            _ -> pure Nothing
        ([arg], res) | not (returnsConstraintKind $ GHC.Core.Type.typeKind (scaledThing arg)) -> do
            let mArgRes = do
                  arg' <- toFgType' pprFun $ scaledThing arg
                  res' <- toFgType' pprFun res
                  Just (arg', res')
            case mArgRes of
              Just (arg', res') -> do
                arg'' <- except $ traverse (tyConOrTyVarTODO pprFun package dbg forall_) arg'
                res'' <- except $ traverse (tyConOrTyVarTODO pprFun package dbg forall_) res'
                pure $ Just $ mkFunctionTypeForall forall_ arg'' res''
              Nothing -> pure Nothing
        (_, _) -> pure Nothing

    goSimple ty =
      case splitFunTys ty of
        ([arg], res) -> do
          let mArgRes = do
                arg' <- toFgType pprFun $ scaledThing arg
                res' <- toFgType pprFun res
                Just (arg', res')
          case mArgRes of
            Just (arg', res') -> withExceptT FgError_TyCon $ do
              arg'' <- except $ traverse (tyConToFgTyCon pprFun package dbg) arg'
              res'' <- except $ traverse (tyConToFgTyCon pprFun package dbg) res'
              pure $ Just $ FunctionType arg'' res''
            Nothing -> pure Nothing
        _ -> pure Nothing

    parseForall mForall (Bndr tyCoVar _) =
      let mkForall = maybe (Right . Forall.singleton) (\forall' -> (`Forall.appendTyVar` forall')) mForall
          tyVarName = pprFun (ppr tyCoVar) -- WIP: correct?
          eForall = mkForall tyVarName
      in either (throwE . FgError_Forall) pure eForall -- WIP: don't throw exception

    -- is kind *
    isTypeKind (Bndr tyVar _) =
      isLiftedTypeKind (tyVarKind tyVar) && not (isConstraintKind $ tyVarKind tyVar)

data DeclarationMap ty = DeclarationMap
  { declarationMap_package :: UnitId
  , declarationMap_moduleDeclarations :: Map ModuleName (Map Name ty)
    -- ^ -- A map from a module name to the declarations in that module
  }

declarationMapToJson
  :: (SDoc -> T.Text)
  -> DeclarationMap (FunctionInfo (Either FgError SomeFunction))
  -> Json.DeclarationMapJson T.Text
declarationMapToJson pprFun dm =
  let
    eitherMap :: Map T.Text (Map T.Text (Either (FunctionInfo FgError) (FunctionInfo SomeFunction)))
    eitherMap = mapMap (declarationMap_moduleDeclarations dm) $ \(modName, nameMap) ->
      ( fullyQualify' modName
      , mapMapMaybe nameMap $ \(name, functionInfoEither) ->
          ( noQualify' name
          , Just $
              let mkFi fun = functionInfoEither{ Types.FunctionInfo.functionInfo_function = fun }
              in either (Left . mkFi) (Right . mkFi) (Types.FunctionInfo.functionInfo_function functionInfoEither)
          ) -- WIP: no Maybe
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

    fullyQualify', noQualify' :: Outputable a => a -> T.Text
    fullyQualify' = pprFun . fullyQualify
    noQualify' = pprFun . noQualify

tyConOrTyVarTODO
  :: (SDoc -> T.Text)
  -> UnitId
  -> (ModuleName, Name) -- for debugging purposes
  -> Forall.Forall T.Text
  -> Either TyCon TyVar
  -> Either FgError (Either (FgTyCon T.Text) (Forall.TyVar T.Text))
tyConOrTyVarTODO pprFun package dbg forall_ =
  either
    (fmap Left . first FgError_TyCon . tyConToFgTyCon pprFun package dbg)
    (fmap Right . first FgError_Forall . tyVarToTODO pprFun package dbg forall_)

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
