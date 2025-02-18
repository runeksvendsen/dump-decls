{-# LANGUAGE RankNTypes #-} -- TODO: remove
module Types.Forall
( -- * Types
  Forall
, TyVar
  -- * Operations
, singleton, appendTyVar, renderForall
, getTyVar, lookupTyVar
  -- * Errors
, ForallError(..)
)
where
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Foldable (toList)
import Data.String (fromString)

-- TODO
type OrdSet a = [a]

-- | Represents the @forall@-part of a type signature, which has the form
--   @forall x1 x2 x3 [...] xn.@.
--
--   This part of the type signature /introduces/ type variables, which are
--   then referenced in the part of the type signature that follows.
newtype Forall tyVar = Forall (OrdSet tyVar)

renderForall
  :: (tyVar -> T.Text)
  -> Forall tyVar
  -> T.Text
renderForall renderTyVar (Forall ordSet) =
    T.unwords (fromString "forall" : map renderTyVar (toList ordSet)) <> fromString "."

-- | Represents a type variable in a type signature, e.g. the
--   last two occurrences of @a@ in @forall a. a -> a@.
--
--   This part of the type signature /references/ type variables introduced
--   by the @forall@ part of the type signature.
newtype TyVar tyVar = TyVar { unTyVar :: tyVar }

getTyVar :: TyVar tyVar -> tyVar
getTyVar = unTyVar

-- WIP
singleton
  :: tyVar
  -> Forall tyVar
singleton tyVar =
  Forall [tyVar]

-- | Append a type variable to the end of the list of type variables in a @forall@
appendTyVar
  :: Eq tyVar
  => tyVar
  -> Forall tyVar
  -> Either (ForallError tyVar) (Forall tyVar)
appendTyVar tyVar (Forall ordSet) =
  -- WIP
  if tyVar `elem` ordSet
    then Left $ DuplicateTypeVar (NE.fromList ordSet) tyVar
    else Right $ Forall (tyVar : ordSet)

lookupTyVar
  :: Eq tyVar
  => tyVar
  -> Forall tyVar
  -> Either (ForallError tyVar) (TyVar tyVar)
lookupTyVar tyVar (Forall ordSet) =
  -- WIP
  if tyVar `elem` ordSet
    then Right (TyVar tyVar)
    else Left $ NoSuchTypeVar (NE.fromList ordSet) tyVar

data ForallError tyVar
  = DuplicateTypeVar -- ^ 'appendTyVar' was called attempting to introduce a type variable that already exists
      (NE.NonEmpty tyVar) -- ^ Existing type variables
      tyVar -- ^ New type variable (contained within the existing type variables)
  | NoSuchTypeVar -- ^ TODO:
      (NE.NonEmpty tyVar)
      tyVar
  deriving (Eq, Show)