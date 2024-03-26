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
import GHC.Core.Type (splitFunTys, expandTypeSynonyms, tcSplitTyConApp_maybe)
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
import GHC.Core.TyCo.Rep (Type(..))
import GHC.Core.TyCon (isUnboxedTupleTyCon, isBoxedTupleTyCon)
import GHC.Builtin.Names (listTyConKey, getUnique)
import qualified Data.Text as T
import Data.Bifunctor (bimap)
import Debug.Trace (trace)
import qualified GHC.Builtin.Types as Tmp

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
            [ (varName _id, Json.FunctionType (newTraceType (varName _id) $ scaledThing arg) (newTraceType (varName _id) res))
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
  where
    newTraceType :: Name -> Type -> Type
    newTraceType = traceP

    traceP name ty = trace_ (ppr2 name ty) ty ty

    ppr2 :: Name -> Type -> Type -> String
    ppr2 name origTy subTy = ppr_ name ++ ",\"" ++ ppr_ subTy ++ "\",\"" ++ ppr_ origTy ++ "\""

    ppr_ :: Outputable a => a -> String
    ppr_ = T.unpack . fullyQualify'

    fullyQualify', noQualify' :: Outputable a => a -> T.Text
    fullyQualify' = pprFun . fullyQualify
    noQualify' = pprFun . noQualify


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
          , mapMap nameMap $ bimap noQualify' funtionTypeToTypeInfo
          )
    }
  where
    mapMap :: Ord k' => Map k a -> ((k, a) -> (k', a')) -> Map k' a'
    mapMap map' f = Map.fromList . map f . Map.toList $ map'


    ppr_ :: Outputable a => a -> String
    ppr_ = T.unpack . fullyQualify'

    funtionTypeToTypeInfo :: Json.FunctionType Type -> Json.TypeInfo T.Text T.Text
    funtionTypeToTypeInfo funType = Json.TypeInfo
      { Json.typeInfo_fullyQualified =
          bimap fullyQualify' fullyQualify' . toBuiltinType . expandTypeSynonyms <$> funType -- trace_ ppr_ (Json.functionType_ret funType) . trace_ ppr_ (Json.functionType_arg funType) <$> funType
      , Json.typeInfo_unqualified =
          bimap noQualify' noQualify' . toBuiltinType . expandTypeSynonyms <$> funType
      , Json.typeInfo_tmpUnexpanded =
          bimap fullyQualify' fullyQualify' . toBuiltinType <$> funType
      }

    fullyQualify', noQualify' :: Outputable a => a -> T.Text
    fullyQualify' = pprFun . fullyQualify
    noQualify' = pprFun . noQualify


trace_ ppr_ ty tyRet =
  let
      f ty' = case ty' of
        TyConApp tyCon _tyList ->
          ( "TMP_DEBUG Nothing " ++ if isTypeSynonymTyCon tyCon then "(synonym): " else ": " ++ typeConsActual (expandTypeSynonyms ty') ++ ", " ++ ppr_ ty') `trace` ty'
        _ -> ty'

      f' ty_ = case ty_ of
        AppTy _ _ -> ( "TMP_DEBUG: " ++ ppr_ ty_) `trace` ty_
        _ -> ty_
  in f ty `seq` tyRet

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




-- TODO:

t = Tmp.boolTyCon

toBuiltinType :: Type -> BuiltinType TyCon Type
toBuiltinType !ty' = BuiltinType_Type Tmp.boolTyCon []

-- toBuiltinType' :: Type -> BuiltinType TyCon Type
-- toBuiltinType' !ty = case tcSplitTyConApp_maybe ty of
--   TyConApp tyCon (ty1:ty2:tyTail) -> -- tuple (of size >= 2)
--     let mBoxity -- 'Nothing' if it's not a tuple
--           | isUnboxedTupleTyCon tyCon = Just Types.Unboxed
--           | isBoxedTupleTyCon tyCon = Just Types.Boxed
--           | otherwise = Nothing
--     in fromMaybe (BuiltinType_Type tycon tyList) $ do
--       boxity <- mBoxity
--       pure $ BuiltinType_Tuple
--         boxity
--         (toBuiltinType ty1)
--         (NE.map toBuiltinType $ ty2 NE.:| tyTail)
--   TyConApp tyCon [ty1] | getUnique tyCon == listTyConKey -> -- list
--     BuiltinType_List (toBuiltinType ty1)
--   _ -> BuiltinType_Type tycon tyList -- neither a tuple nor list

traceType :: (Type -> Type) -> Type -> Type
traceType ppr_ initTy =
  let go !ty' = case ty' of
        TyVarTy _var -> ty'
        AppTy ty1 ty2 ->
          -- let tyList = [ty1, ty2] in map ppr_ tyList `seq` map go tyList `seq` ty'
          ppr_ ty1 `seq` ppr_ ty2 `seq` go ty1 `seq` go ty2 `seq` ty'
        TyConApp _tyCon tyList ->
          map ppr_ tyList `seq` map go tyList `seq` ty'
        ForAllTy _forAllTyBinder ty ->
          ppr_ ty `seq` go ty `seq` ty'
        FunTy _af _mult ty1 ty2 ->
          ppr_ ty1 `seq` ppr_ ty2 `seq` go ty1 `seq` go ty2 `seq` ty'
        LitTy _tyLit -> ty'
        CastTy ty _kindCoercion ->
          ppr_ ty `seq` go ty `seq` ty'
        CoercionTy _coercion  -> ty'
  in ppr_ initTy `seq` go initTy

typeConsActual :: Type -> String
typeConsActual ty' =
  case ty' of
    TyVarTy _var -> "TyVarTy"
    AppTy ty1 ty2 -> "AppTy"
    TyConApp _tyCon tyList -> "TyConApp"
    ForAllTy _forAllTyBinder ty -> "ForAllTy"
    FunTy _af _mult ty1 ty2 -> "FunTy"
    LitTy _tyLit -> "LitTy"
    CastTy ty _kindCoercion -> "CastTy"
    CoercionTy _coercion  -> "CoercionTy"
