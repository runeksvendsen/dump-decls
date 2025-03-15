{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE TypeOperators #-}
{-# HLINT ignore "Use first" #-}
{-# LANGUAGE LambdaCase #-}
module Types.Doodle
( FunctionTypeForallSpecialized(..)
, FunctionTypeForall
, specializeType
, mkFunctionTypeForall
, extendFrom
  -- * SomeFunction
, SomeFunction(..)
, eitherToSomeFunction
, someFunctionMonomorphic
, someFunctionPolymorphic
)
where

import Types
import Types.Forall
import qualified Types.Forall as Forall
import Json
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe, fromJust)

type FunctionTypeNoTyVar =
  FunctionType (FgType (FgTyCon T.Text))

type FunctionTypeForall tyVar text = FunctionTypeForallSpecialized tyVar () text

mkFunctionTypeForall
  :: ForallSpecialized tyVar tyVarAssoc
  -> FgType (Either (FgTyCon text) (TyVar tyVar))
  -> FgType (Either (FgTyCon text) (TyVar tyVar))
  -> FunctionTypeForallSpecialized tyVar tyVarAssoc text
mkFunctionTypeForall = FunctionTypeForallSpecialized

data FunctionTypeForallSpecialized tyVar tyVarAssoc text = FunctionTypeForallSpecialized
  { ftf_forall :: ForallSpecialized tyVar tyVarAssoc
  , ftf_arg :: FgType (Either (FgTyCon text) (TyVar tyVar))
  , ftf_ret :: FgType (Either (FgTyCon text) (TyVar tyVar))
  }

-- WIP: something more type safe than this conversion
specializationEnvToForallSpecialized
  :: Ord tyVar
  => Map (TyVar tyVar) (FgType tyCon)
  -> ForallSpecialized tyVar ()
  -> Maybe (ForallSpecialized tyVar (FgType tyCon)) -- Nothing: not all type variables specialized ()
specializationEnvToForallSpecialized env forallSpecialized =
  const (fromJust <$> forallMaybeValue) <$> theMaybe
  where
    theMaybe = sequence_ $ Forall.elems forallMaybeValue
    forallMaybeValue = mapWithKey f forallSpecialized
    f tyVar () = Map.lookup tyVar env

functionTypeForallToSpecializedFgType
  :: (Ord tyVar, Show tyVar)
  => ForallSpecialized tyVar (FgType (FgTyCon T.Text))
  -> FgType (Either (FgTyCon T.Text) (TyVar tyVar))
  -> Either (ForallError (TyVar tyVar)) (FgType (FgTyCon T.Text)) -- WIP: accumulate _all_ ForallErrors
functionTypeForallToSpecializedFgType forallSpecialized fgTypePoly =
  joinFgType <$>
  traverse
    (\eitherFgTyConOrTyVar ->
        either
          (\tyCon -> Right $ FgType_TyConApp tyCon [])
          (\tyVar -> Right $ snd $ lookupTyVarAssoc tyVar forallSpecialized)
          eitherFgTyConOrTyVar
    )
    fgTypePoly

-- | Extend from the return type of the monomorphic function
--
-- NOTE: quadratic!!!
extendFrom
  :: (Ord tyVar, Show tyVar)
  => [FunctionTypeNoTyVar]
      -- ^ monomorphic functions.
      --
      --   e.g. @Int -> [Bool]@
  -> [FunctionTypeForall tyVar T.Text]
      -- ^ polymorphic functions
      --
      --   e.g. @forall a. [a] -> Maybe a@
  -> [FunctionTypeNoTyVar]
      -- ^ a list of: a specialization of one of the polymorphic functions
      --    that takes as argument a type retuned by one of the monomorphic functions
      --
      --  e.g. @[Bool] -> Maybe Bool@
extendFrom monoFuns polyFuns =
  concat $ foldl' foldFun [] monoFuns
  where
    foldFun
      :: [[FunctionTypeNoTyVar]]
      -> FunctionTypeNoTyVar
      -> [[FunctionTypeNoTyVar]]
    foldFun accum monoFun =
      let foldFun' accum' polyFun =
            case specializeType (ftf_arg polyFun) (functionType_ret monoFun) of
              Right (Just (fgTypePolyArg, env)) ->
                let mForallSpecialized =
                      specializationEnvToForallSpecialized env (ftf_forall polyFun)
                    forallSpecialized = fromMaybe (error $ "WIP: not all type variables specialized in " <> show (env, ftf_forall polyFun)) $
                      mForallSpecialized
                    eFgTypePolyRet =
                      functionTypeForallToSpecializedFgType forallSpecialized (ftf_ret polyFun)
                    fgTypePolyRet =
                      either (error . (<> show (forallSpecialized, ftf_ret polyFun)) . show) id eFgTypePolyRet -- WIP
                in FunctionType
                  { functionType_arg = fgTypePolyArg
                  , functionType_ret = fgTypePolyRet
                  } : accum'
              Left e ->
                error e : accum' -- WIP
              _ -> accum'
      in foldl' foldFun' [] polyFuns : accum

-- | TODO
--
--   Example 1: Map k v / Map Bool Int: [k ~ Bool, v ~ Int]
--
-- >>> let withTyVars = FgType_TyConApp (Left "Map") [FgType_TyConApp (Right "k") [], FgType_TyConApp (Right "v") []]
-- >>> let withoutTyVars = FgType_TyConApp "Map" [FgType_TyConApp "Bool" [], FgType_TyConApp "Int" []]
-- >>> specializeType withTyVars withoutTyVars
-- Right (Just (FgType_TyConApp "Map" [FgType_TyConApp "Bool" [],FgType_TyConApp "Int" []],fromList [("k",FgType_TyConApp "Bool" []),("v",FgType_TyConApp "Int" [])]))
--
--   Example 2: Map a a / Map Bool Bool: [a ~ Bool]
--
-- >>> let withTyVars = FgType_TyConApp (Left "Map") [FgType_TyConApp (Right "a") [], FgType_TyConApp (Right "a") []]
-- >>> let withoutTyVars = FgType_TyConApp "Map" [FgType_TyConApp "Bool" [], FgType_TyConApp "Bool" []]
-- >>> specializeType withTyVars withoutTyVars
-- Right (Just (FgType_TyConApp "Map" [FgType_TyConApp "Bool" [],FgType_TyConApp "Bool" []],fromList [("a",FgType_TyConApp "Bool" [])]))
--
--   Example 2: Map a a / Map Bool Int: (no match because Bool and Int are different types)
--
-- >>> let withTyVars = FgType_TyConApp (Left "Map") [FgType_TyConApp (Right "a") [], FgType_TyConApp (Right "a") []]
-- >>> let withoutTyVars = FgType_TyConApp "Map" [FgType_TyConApp "Bool" [], FgType_TyConApp "Int" []]
-- >>> specializeType withTyVars withoutTyVars
-- Right Nothing
--
--   Example 2: f a b / Map Bool Int:
--
-- >>> let withTyVars = FgType_TyConApp (Right "f") [FgType_TyConApp (Right "a") [], FgType_TyConApp (Right "b") []]
-- >>> let withoutTyVars = FgType_TyConApp "Map" [FgType_TyConApp "Bool" [], FgType_TyConApp "Int" []]
-- >>> specializeType withTyVars withoutTyVars
-- Right (Just (FgType_TyConApp "Map" [FgType_TyConApp "Bool" [],FgType_TyConApp "Int" []],fromList [("a",FgType_TyConApp "Bool" []),("b",FgType_TyConApp "Int" []),("f",FgType_TyConApp "Map" [])]))
--
-- >>> specializeType (FgType_Tuple Boxed 2 [FgType_TyConApp (Right "int") [], FgType_TyConApp (Right "bool") []]) (FgType_Tuple Boxed 2 [FgType_TyConApp (Left "Int") [], FgType_TyConApp (Left "Bool") []])
-- Right (Just (FgType_Tuple Boxed 2 [FgType_TyConApp (Left "Int") [],FgType_TyConApp (Left "Bool") []],fromList [("bool",FgType_TyConApp (Left "Bool") []),("int",FgType_TyConApp (Left "Int") [])]))
--
-- NOTE: Implementaion strategy:
--    1. Prefer many small, specific matches over few generic matches,
--       e.g. separately match on `Right tyVar` and `Left tyCon`.
--       If this leads to code duplication then factor out common code into a helper function.
--    2. No wildcard matches. Match constructors only until no warnings are left.
--       Helps to ensure that no relevant case has been overlooked.
specializeType
  :: forall tyCon tyVar.
     ( Eq tyCon
     , Eq tyVar
     , Ord tyVar
     , Show tyCon
     , Show tyVar
     )
  => FgType (Either tyCon tyVar)
     -- ^ (a) Type with type variables
  -> FgType tyCon
     -- ^ (b) Type without type variables
  -> Either String (Maybe (FgType tyCon, Map tyVar (FgType tyCon)))
     -- ^ If (b) is a specialization of (a) then this is a 'Just'
     --    with each type variable in (a) instantiated to a type in (b).
     --
     --   A 'Left' denotes an bug somewhere.
specializeType =
  go mempty
  where
    go :: Map tyVar (FgType tyCon)
       -> _
       -> _
       -> Either String (Maybe (FgType tyCon, Map tyVar (FgType tyCon)))
    go env poly mono =
      let handleTyConArgs' = handleTyConArgs (poly, mono)
      in case (poly, mono) of
        (FgType_TyConApp (Left pTyCon) pArgs, FgType_TyConApp mTyCon mArgs) | pTyCon == mTyCon -> do -- (Either a b, Either Int Bool)
          mResult <- handleTyConArgs' env pArgs mArgs
          pure $ first (FgType_TyConApp mTyCon) <$> mResult

        (FgType_TyConApp (Left _) _, FgType_TyConApp _ _) -> -- (Map a b, Either Int Bool)
          Right Nothing -- NOTE: pTyCon /= mTyCon

        (FgType_TyConApp (Right tyVar) pArgs, FgType_TyConApp mTyCon mArgs) ->
          let mTyCon' =
                let mTyConFgType = FgType_TyConApp mTyCon []
                in case Map.lookup tyVar env of
                  Just existingTypeEq ->
                    if existingTypeEq == mTyConFgType
                      then Just (mTyCon, env) -- previously instantiated to the same type. NOTE: unless `mArgs == []` this is a type variable _NOT_ of kind *
                      else Nothing -- this type variable was previously instantiated to something else
                  Nothing ->
                    Just (mTyCon, Map.insert tyVar mTyConFgType env)
          in case mTyCon' of
              Just (tyCon', env') -> do
                fmap (first (FgType_TyConApp tyCon')) <$> handleTyConArgs' env' pArgs mArgs
              Nothing -> Right Nothing

        (FgType_TyConApp (Right tyVar) [], ty@(FgType_List _)) -> -- f [] ; a [Int] ; a [b]
          Right $ Just (ty, Map.insert tyVar ty env)

        (FgType_TyConApp (Right tyVar) [tyVarArg], FgType_List (Just lstArg)) -> -- (f a, [Int]) ; (f a, [[Int]])
          let env' = Map.insert tyVar (FgType_List Nothing) env -- f ~ []
          in do
            mResult <- go env' tyVarArg lstArg
            pure $ mResult <&> \(resultArg, env'') -> (FgType_List (Just resultArg), env'')

        (FgType_TyConApp (Right _) (_:_:_), FgType_List _) -> -- (f a b [...], []) ; (f a b [...], [Int])
          Right Nothing

        (FgType_TyConApp (Right _) [_], FgType_List Nothing) -> -- (f a, [])
          Right Nothing

        (FgType_TyConApp (Right tyVar) [], fgTuple@(FgType_Tuple{})) -> -- (a, (Int, Bool)) ; (f, (,)) ; (f, (,) Bool)
          Right $ Just (fgTuple, Map.insert tyVar fgTuple env)

        (FgType_TyConApp (Right tyVar) pArgs, FgType_Tuple boxity size args) | length pArgs == length args -> do -- (f a, (,) Bool) ; f a a, (,) Bool Bool) ; (f a b c, (,) Int Bool String)
          let env' = Map.insert tyVar (FgType_Tuple boxity size []) env
          mRes <- handleTyConArgs' env' pArgs args
          pure $ mRes <&> \(result, env'') ->
            (FgType_Tuple boxity size result, env'')

        (FgType_TyConApp (Right _) _, FgType_Tuple{}) -> -- (f a, (,) Bool Int) ; (f a b, (,) Bool)
          Right Nothing -- NOTE: length pArgs /= length args

        (FgType_TyConApp (Right tyVar) [], fgUnit@FgType_Unit{}) -> -- a ()
          Right $ Just (fgUnit, Map.insert tyVar fgUnit env)

        (FgType_TyConApp (Right _) _, FgType_Unit{}) -> -- (f a, ())
          Right Nothing

        (FgType_TyConApp (Left _) _, FgType_List{}) ->
          Right Nothing
        (FgType_TyConApp (Left _) _, FgType_Tuple{}) ->
          Right Nothing
        (FgType_TyConApp (Left _) _, FgType_Unit{}) ->
          Right Nothing

        (FgType_List (Just fgType), FgType_List (Just fgType')) -> do -- [a] [Bool]
          mResult <- go env fgType fgType'
          pure $ mResult <&> \(result, env') ->
            (FgType_List (Just result), env')

        (FgType_List _, _) ->
          Right Nothing

        (FgType_Tuple boxity size args, FgType_Tuple boxity' size' args') | boxity == boxity' && size == size' -> do -- (a, b) (Int, Bool)
          mArgsEnv <- handleTyConArgs' env args args'
          pure $ mArgsEnv <&> first (FgType_Tuple boxity size)
        (FgType_Tuple{}, FgType_Tuple{}) -> -- boxity /= boxity' || size /= size'
          Right Nothing

        (FgType_Tuple _ _ _, FgType_TyConApp _ _) ->
          Right Nothing
        (FgType_Tuple _ _ _, FgType_List _) ->
          Right Nothing
        (FgType_Tuple _ _ _, FgType_Unit _) ->
          Right Nothing

        (FgType_Unit b, fgUnit@(FgType_Unit b')) | b == b' ->
          Right $ Just (fgUnit, env)

        (FgType_Unit{}, FgType_Unit{}) -> -- boxity mismatch
          Right Nothing
        (FgType_Unit _, FgType_TyConApp{}) ->
          Right Nothing
        (FgType_Unit _, FgType_List{}) ->
          Right Nothing
        (FgType_Unit _, FgType_Tuple{}) ->
          Right Nothing

    handleTyConArgs
      :: forall env.
         env ~ Map tyVar (FgType tyCon)
      => (FgType (Either tyCon tyVar), FgType tyCon) -- For debug printing
      -> env
      -> [FgType (Either tyCon tyVar)] -- polymorphic TyCon args
      -> [FgType tyCon] -- monomorphic TyCon args
      -> Either String (Maybe ([FgType tyCon], env))
    handleTyConArgs _ env_ pArgs mArgs =
      let matchTyConArg
            :: Maybe ([FgType tyCon], env)
            -> (FgType (Either tyCon tyVar), FgType tyCon)
            -> Either String (Maybe ([FgType tyCon], env))
          matchTyConArg state (poly', mono') = case state of
            Just (args, env) -> do
              mFgTypePair <- go env poly' mono'
              pure $ do
                (fgTypePair, env') <- mFgTypePair
                Just (fgTypePair : args, env')
            other -> pure other

          eTyConArgsResult env'
            | length mArgs /= length pArgs =
                Right Nothing
            | otherwise =
                foldM matchTyConArg (Just ([], env')) (zip pArgs mArgs)
      in do
        mResultArgs <- eTyConArgsResult env_
        Right $ do
          (resultArgs, env'') <- mResultArgs
          Just (reverse resultArgs, env'')

-- ####################################
-- ########### SomeFunction ###########
-- ####################################

data SomeFunction
  = SomeFunction_Monomorphic (FunctionType (FgType (FgTyCon T.Text)))
  | SomeFunction_Polymorphic (FunctionTypeForall T.Text T.Text)

eitherToSomeFunction
  :: Either
      (Json.FunctionType (FgType (FgTyCon T.Text)))
      (FunctionTypeForall T.Text T.Text)
  -> SomeFunction
eitherToSomeFunction =
  either SomeFunction_Monomorphic SomeFunction_Polymorphic

someFunctionMonomorphic
  :: SomeFunction -> Maybe (FunctionType (FgType (FgTyCon T.Text)))
someFunctionMonomorphic = \case
  SomeFunction_Monomorphic mono -> Just mono
  SomeFunction_Polymorphic _ -> Nothing

someFunctionPolymorphic
  :: SomeFunction -> Maybe (FunctionTypeForall T.Text T.Text)
someFunctionPolymorphic = \case
  SomeFunction_Polymorphic poly -> Just poly
  SomeFunction_Monomorphic _ -> Nothing
