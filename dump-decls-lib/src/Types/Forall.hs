{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fmap" #-}
module Types.Forall
( -- * Types
  Forall
, ForallSpecialized
, TyVar
  -- * Operations
, singleton, appendTyVar, renderForall
, getTyVar, lookupTyVar, lookupTyVarAssocM, lookupTyVarAssoc
, mapWithKey, elems
  -- * Errors
, ForallError(..)
)
where
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Map.Ordered.Strict (OMap, (<|))
import qualified Data.Map.Ordered.Strict as OMap
import Data.String (fromString)
import GHC.Stack (HasCallStack)
import Data.Functor (void)
import Control.DeepSeq (NFData (rnf))
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Vector as V

-- TODO newtype
type OrdMap k v = OMap k v

instance (NFData k, NFData v) => NFData (OMap k v) where
  rnf = rnf . OMap.toAscList
instance (A.ToJSON k, A.ToJSONKey k, A.ToJSON v) => A.ToJSON (OMap k v) where
  toJSON = A.toJSON . OMap.assocs
instance (A.FromJSON k, A.FromJSONKey k, Ord k, A.FromJSON v) => A.FromJSON (OMap k v) where
  parseJSON =
    let parseKV = A.withArray "KeyValue" $ \vec ->
          case V.toList vec of
            [k, v] -> (,) <$> A.parseJSON k <*> A.parseJSON v
            other -> fail $ "Expected two-element key-value list. Got: " <> show other
    in A.withArray "OrdMap" $ \vec ->
      OMap.fromList . V.toList <$> traverse parseKV vec

-- insertion order
toOrderedList :: OrdMap tyVar assoc -> [tyVar]
toOrderedList = reverse . map fst . OMap.assocs

-- | Represents the @forall@-part of a type signature, which has the form
--   @forall x1 x2 x3 [...] xn.@.
--
--   This part of the type signature /introduces/ type variables, which are
--   then referenced in the part of the type signature that follows.
type Forall tyVar = ForallSpecialized tyVar ()

newtype ForallSpecialized tyVar assoc = ForallSpecialized (OrdMap tyVar assoc)
  deriving (Eq, Show, Ord, Functor, Foldable, Generic)

instance (NFData tyVar, NFData assoc) => NFData (ForallSpecialized tyVar assoc)

instance (A.ToJSON tyVar, A.ToJSONKey tyVar, A.ToJSON assoc) => A.ToJSON (ForallSpecialized tyVar assoc)
instance (A.FromJSON tyVar, A.FromJSONKey tyVar, Ord tyVar, A.FromJSON assoc) => A.FromJSON (ForallSpecialized tyVar assoc)

elems
  :: ForallSpecialized k v
  -> [v]
elems (ForallSpecialized ordMap) =
  snd <$> OMap.assocs ordMap

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
  deriving (Show, Eq, Ord, Generic)

instance (NFData tyVar) => NFData (TyVar tyVar)
instance (A.ToJSON tyVar, A.ToJSONKey tyVar) => A.ToJSON (TyVar tyVar)
instance (A.FromJSON tyVar, A.FromJSONKey tyVar, Ord tyVar) => A.FromJSON (TyVar tyVar)

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
    (Right $ ForallSpecialized $ (tyVar, ()) <| ordMap)
    (const $ Left $ DuplicateTypeVar (NE.fromList $ toOrderedList ordMap) tyVar) -- WIP
    (OMap.lookup tyVar ordMap)

lookupTyVar
  :: (Ord tyVar)
  => tyVar
  -> Forall tyVar
  -> Either (ForallError tyVar) (TyVar tyVar)
lookupTyVar tyVar  =
  fmap fst . lookupTyVarAssocM tyVar

lookupTyVarAssocM
  :: (Ord tyVar)
  => tyVar
  -> ForallSpecialized tyVar assoc
  -> Either (ForallError tyVar) (TyVar tyVar, assoc)
lookupTyVarAssocM tyVar (ForallSpecialized ordMap) =
  maybe
    (Left $ NoSuchTypeVar (NE.fromList $ toOrderedList ordMap) tyVar) -- WIP
    (\assoc -> Right (TyVar tyVar, assoc))
    (OMap.lookup tyVar ordMap)

-- | TODO: The 'TyVar' is a witness that the given type variable exists
lookupTyVarAssoc
  :: (HasCallStack, Show tyVar)
  => (Ord tyVar)
  => TyVar tyVar
  -> ForallSpecialized tyVar assoc
  -> (TyVar tyVar, assoc)
lookupTyVarAssoc (TyVar tyVar) fs =
  either
    (error . T.unpack $ errTxt)
    id
  $ lookupTyVarAssocM tyVar fs
  where
    errTxt = T.unwords
      [ "No such tyvar"
      , T.pack (show tyVar)
      , "in"
      , renderForall (T.pack . show) (void fs) <> "."
      , "This is a bug unless you got the TyVar from applying 'lookupTyVar' to a different 'ForallSpecialized'."
      ]

data ForallError tyVar
  = DuplicateTypeVar -- ^ 'appendTyVar' was called attempting to introduce a type variable that already exists
      (NE.NonEmpty tyVar) -- ^ Existing type variables
      tyVar -- ^ New type variable (contained within the existing type variables)
  | NoSuchTypeVar -- ^ TODO:
      (NE.NonEmpty tyVar)
      tyVar
  deriving (Eq, Show, Ord, Generic)

instance (A.ToJSON tyVar, A.ToJSONKey tyVar) => A.ToJSON (ForallError tyVar)
instance (A.FromJSON tyVar, A.FromJSONKey tyVar, Ord tyVar) => A.FromJSON (ForallError tyVar)
instance (NFData tyVar) => NFData (ForallError tyVar)
