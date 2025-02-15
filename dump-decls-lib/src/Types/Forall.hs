{-# LANGUAGE RankNTypes #-} -- TODO: remove
module Types.Forall
( -- * Types
  Forall
, TyVar
  -- * Operations
, singleton, appendTyVar
, getTyVar, lookupTyVar
  -- * Errors
, ForallError(..)
)
where
import qualified Data.List.NonEmpty as NE

-- TODO
type OrdSet a = ()

-- | Represents the @forall@-part of a type signature, which has the form
--   @forall x1 x2 x3 [...] xn.@.
--
--   This part of the type signature /introduces/ type variables, which are
--   then referenced in the part of the type signature that follows.
newtype Forall tyVar = Forall (OrdSet tyVar)

-- | Represents a type variable in a type signature, e.g. the
--   last two occurrences of @a@ in @forall a. a -> a@.
--
--   This part of the type signature /references/ type variables introduced
--   by the @forall@ part of the type signature.
newtype TyVar tyVar = TyVar { unTyVar :: tyVar }

getTyVar :: TyVar tyVar -> tyVar
getTyVar = unTyVar

singleton
  :: tyVar
  -> Forall tyVar
singleton = error "TODO"

-- | Append a type variable to the end of the list of type variables in a @forall@
appendTyVar
  :: tyVar
  -> Forall tyVar
  -> Either (ForallError tyVar) (Forall tyVar)
appendTyVar = error "TODO"

lookupTyVar
  :: tyVar
  -> Forall tyVar
  -> Either (ForallError tyVar) (TyVar tyVar)
lookupTyVar = error "TODO"

data ForallError tyVar
  = DuplicateTypeVar -- ^ 'appendTyVar' was called attempting to introduce a type variable that already exists
      (NE.NonEmpty tyVar) -- ^ Existing type variables
      tyVar -- ^ New type variable (contained within the existing type variables)
  | NoSuchTypeVar -- ^ TODO:
      (NE.NonEmpty tyVar)
      tyVar
  deriving (Eq, Show)