{-# LANGUAGE RankNTypes #-} -- TODO: remove
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Types.Forall
( -- * Types
  Forall
, ForallSpecialized
, TyVar
  -- * Operations
, singleton, appendTyVar, renderForall
, getTyVar, lookupTyVar, lookupTyVarAssoc
  -- * Errors
, ForallError(..), mapWithKey
)
where
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Map.Ordered.Strict (OMap, (<|))
import qualified Data.Map.Ordered.Strict as OMap
import Data.String (fromString)

-- TODO
type OrdMap k v = OMap k v

-- insertion order
toOrderedList :: OrdMap tyVar assoc -> [tyVar]
toOrderedList = map fst . OMap.assocs

-- | Represents the @forall@-part of a type signature, which has the form
--   @forall x1 x2 x3 [...] xn.@.
--
--   This part of the type signature /introduces/ type variables, which are
--   then referenced in the part of the type signature that follows.
type Forall tyVar = ForallSpecialized tyVar ()

newtype ForallSpecialized tyVar assoc = ForallSpecialized (OrdMap tyVar assoc)
  deriving (Eq, Show, Ord, Functor, Foldable)

mapWithKey
  :: Ord tyVar
  => (TyVar tyVar -> assoc -> assoc')
  -> ForallSpecialized tyVar assoc
  -> ForallSpecialized tyVar assoc'
mapWithKey f (ForallSpecialized ordMap) =
  ForallSpecialized $ OMap.fromList $ map (\(k, v) -> (k, f (TyVar k) v)) $ OMap.assocs ordMap

renderForall
  :: (tyVar -> T.Text)
  -> Forall tyVar
  -> T.Text
renderForall renderTyVar (ForallSpecialized ordMap) =
    T.unwords (fromString "forall" : map renderTyVar (toOrderedList ordMap)) <> fromString "."

-- | Represents a type variable in a type signature, e.g. the
--   last two occurrences of @a@ in @forall a. a -> a@.
--
--   This part of the type signature /references/ type variables introduced
--   by the @forall@ part of the type signature.
newtype TyVar tyVar = TyVar { unTyVar :: tyVar }
  deriving (Show, Eq, Ord)

getTyVar :: TyVar tyVar -> tyVar
getTyVar = unTyVar

singleton
  :: tyVar
  -> Forall tyVar
singleton tyVar =
  ForallSpecialized $ OMap.singleton (tyVar, ())

-- | Append a type variable to the end of the list of type variables in a @forall@
appendTyVar
  :: (Ord tyVar)
  => tyVar
  -> Forall tyVar
  -> Either (ForallError tyVar) (Forall tyVar)
appendTyVar tyVar (ForallSpecialized ordMap) =
  maybe
    (Left $ DuplicateTypeVar (NE.fromList $ toOrderedList ordMap) tyVar) -- WIP
    (const $ Right $ ForallSpecialized $ (tyVar, ()) <| ordMap)
    (OMap.lookup tyVar ordMap)

lookupTyVar
  :: (Ord tyVar)
  => tyVar
  -> Forall tyVar
  -> Either (ForallError tyVar) (TyVar tyVar)
lookupTyVar tyVar  =
  fmap fst . lookupTyVarAssoc tyVar

lookupTyVarAssoc
  :: (Ord tyVar)
  => tyVar
  -> ForallSpecialized tyVar assoc
  -> Either (ForallError tyVar) (TyVar tyVar, assoc)
lookupTyVarAssoc tyVar (ForallSpecialized ordMap) =
  maybe
    (Left $ NoSuchTypeVar (NE.fromList $ toOrderedList ordMap) tyVar) -- WIP
    (\assoc -> Right (TyVar tyVar, assoc))
    (OMap.lookup tyVar ordMap)

data ForallError tyVar
  = DuplicateTypeVar -- ^ 'appendTyVar' was called attempting to introduce a type variable that already exists
      (NE.NonEmpty tyVar) -- ^ Existing type variables
      tyVar -- ^ New type variable (contained within the existing type variables)
  | NoSuchTypeVar -- ^ TODO:
      (NE.NonEmpty tyVar)
      tyVar
  deriving (Eq, Show)