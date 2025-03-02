{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE TypeOperators #-}
module Types.Doodle
( FunctionTypeForall(..)
)

where

import Types
import Types.Forall
import Json
import qualified Data.Text as T
import Data.Foldable (foldl')
import qualified Data.Map as Map
import Control.Monad (foldM)

type FunctionTypeNoTyVar =
  FunctionType (FgType (FgTyCon T.Text))

data FunctionTypeForall tyVar text = FunctionTypeForall
  { ftf_forall :: Forall tyVar
  , ftf_arg :: FgType (Either (FgTyCon text) (TyVar tyVar))
  , ftf_ret :: FgType (Either (FgTyCon text) (TyVar tyVar))
  }

-- |
extendFrom
  :: [FunctionTypeNoTyVar]
      -- ^ monomorphic functions.
      --
      --   e.g. @Int -> [Bool]@
  -> [FunctionTypeForall tyVar text]
      -- ^ polymorphic functions
      --
      --   e.g. @forall a. [a] -> Maybe a@
  -> [FunctionTypeNoTyVar]
      -- ^ a list of: a specialization of a polymorphic function
      --    that takes as argument a type retuned by one of the monomorphic functions
      --
      --  e.g. @[Bool] -> Maybe Bool@
extendFrom =
  undefined

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
      let handleTyConWithArgs' = handleTyConWithArgs (poly, mono) env
      in case (poly, mono) of
        (FgType_TyConApp pTyCon pArgs, FgType_TyConApp mTyCon mArgs) -> -- (Map k v) (Map Bool Int)
          handleTyConWithArgs' (pTyCon, pArgs) (mTyCon, mArgs)

        (FgType_TyConApp _ _, _) ->
          Right Nothing

        (FgType_List _, FgType_List _) -> -- [a] [Bool]
          undefined

        (FgType_List _, _) ->
          Right Nothing

        (FgType_Tuple _ _ _, FgType_Tuple _ _ _) -> -- (a, b) (a, Bool)
          undefined

        (FgType_Tuple _ _ _, _) ->
          Right Nothing

        (FgType_Unit _, _) ->
          Right Nothing

    handleTyConWithArgs
      :: forall env.
         env ~ Map tyVar (FgType tyCon)
      => (FgType (Either tyCon tyVar), FgType tyCon) -- For debug printing
      -> env
      -> (Either tyCon tyVar, [FgType (Either tyCon tyVar)]) -- polymorphic (TyCon, TyCon args)
      -> (tyCon, [FgType tyCon]) -- monomorphic (TyCon, TyCon args)
      -> Either String (Maybe (FgType tyCon, env))
    handleTyConWithArgs dbg env_ (pTyCon, pArgs) (mTyCon, mArgs) =
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
                Left $ "unsaturated type constructor. " <> show dbg
            | otherwise =
                foldM matchTyConArg (Just ([], env')) (zip pArgs mArgs)

          mTyCon'
            | pTyCon == Left mTyCon =
                Just (mTyCon, env_) -- both lhs and rhs are the same type constructor.
            | Right tyVar <- pTyCon = -- lhs is tyvar
                let blahType = FgType_TyConApp mTyCon [] -- WIP: name
                in case Map.lookup tyVar env_ of
                  Just existingTypeEq ->
                    if existingTypeEq == blahType
                      then Just (mTyCon, env_) -- previously instantiated to the same type. NOTE: unless `mArgs == []` this is a type variable _NOT_ of kind *
                      else Nothing -- this type variable was previously instantiated to something else
                  Nothing ->
                    Just (mTyCon, Map.insert tyVar blahType env_)
            | otherwise =
                Nothing -- lhs and rhs are different type constructors
      in case mTyCon' of
        Just (tyCon', env') -> do
          mResultArgs <- eTyConArgsResult env'
          Right $ do
            (resultArgs, env'') <- mResultArgs
            Just (FgType_TyConApp tyCon' (reverse resultArgs), env'')
        _ -> Right Nothing
