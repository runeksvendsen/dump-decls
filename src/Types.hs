{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
-- |
-- TODO: Test 'Data.Aeson.encode
module Types
( BuiltinType(..)
, Boxity(..)
, isBoxed
)
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Text as T
import Control.Applicative (empty, (<|>))
import qualified Compat.Aeson
import qualified Data.Aeson.Types as A
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor(..))

-- | Either just a type, a list or a tuple
data BuiltinType tycon ty
  = BuiltinType_Type -- ^ A type consisting of (1) a type constructor, and (2) the types to which the constructor is applied.
      tycon
      -- ^ A /type constructor/.
      -- Essentially the name of a type without any of its type variables filled in.
      -- E.g. 'Maybe', 'Either', 'IO', 'Map'.
      -- Note that this is never the list nor tuple type constructor as they're handled by 'BuiltinType_List' and 'BuiltinType_Tuple', respectively.
      [BuiltinType tycon ty]
      -- ^ Arguments to the type constructor.
      -- The empty list if the type constructor does not take any arguments (e.g. 'Int', 'Char', 'Text') and otherwise one type for all type variables of the type constructor (since we're only looking at the types of functions exported from a module where a partially applied type constructor is invalid).
  | BuiltinType_List (BuiltinType tycon ty)
  -- ^ A list
  | BuiltinType_Tuple Boxity (BuiltinType tycon ty) (NE.NonEmpty (BuiltinType tycon ty))
  -- ^ A tuple of size @1 + length nonEmptyList@
    deriving (Eq, Show, Ord, Generic)

instance Functor (BuiltinType tycon) where
  fmap = bimap id

instance Bifunctor BuiltinType where
  bimap f g = \case
    BuiltinType_Type tycon tyList ->
      BuiltinType_Type (f tycon) $ fmap (bimap f g) tyList
    BuiltinType_List bty ->
      BuiltinType_List $ bimap f g bty
    BuiltinType_Tuple boxity bty neBty ->
      BuiltinType_Tuple boxity (bimap f g bty) (NE.map (bimap f g) neBty)

data Boxity
  = Boxed
  | Unboxed
    deriving (Eq, Show, Ord, Generic, Enum, Bounded)

isBoxed :: Boxity -> Bool
isBoxed Boxed   = True
isBoxed Unboxed = False

instance A.ToJSON Boxity where
  toJSON = \case
    Boxed -> "boxed"
    Unboxed -> "unboxed"

instance A.FromJSON Boxity where
  parseJSON = A.withText "Boxity" $ \case
    "boxed" -> pure Boxed
    "unboxed" -> pure Unboxed
    other -> fail . T.unpack $ T.unwords
      [ "Invalid boxity '" <> other <> "'."
      , "Valid boxities:"
      , T.intercalate ", " $ map (T.toLower . T.pack . show) (enumFromTo minBound maxBound :: [Boxity])
      ]

instance (A.ToJSON ty, A.ToJSON tycon) => A.ToJSON (BuiltinType tycon ty) where
  toJSON = \case
    BuiltinType_Type tycon tyList -> A.object
      [("type", A.toJSON [("tycon" :: Compat.Aeson.Key, A.toJSON tycon), ("tycon_args", A.toJSON tyList)])]
    BuiltinType_List bty -> A.object
      [("list", A.toJSON bty)]
    BuiltinType_Tuple boxity bty neBty ->
      let key = case boxity of {Unboxed -> "tuple#"; Boxed -> "tuple"}
      in A.object
        [(key, A.toJSON $ bty : NE.toList neBty)]

instance (A.FromJSON ty, A.FromJSON tycon) => A.FromJSON (BuiltinType tycon ty) where
  parseJSON = A.withObject "BuiltinType" $ \o -> do
        parseKind o "type" (\o' -> BuiltinType_Type <$> o' A..: "tycon" <*> o' A..: "tycon_args")
    <|> parseKind o "list" (pure . BuiltinType_List)
    <|> parseKind o "tuple" (tupleFromList Boxed)
    <|> parseKind o "tuple#" (tupleFromList Unboxed)
      where
        -- apply function to value if (key,value) exists in the object
        parseKind
          :: A.FromJSON a
          => A.Object
          -> Compat.Aeson.Key
          -> (a -> A.Parser (BuiltinType tycon ty))
          -> A.Parser (BuiltinType tycon ty)
        parseKind o keyTxt mkType =
          maybe empty (A.parseJSON >=> mkType) (Compat.Aeson.lookup keyTxt o)

        tupleFromList boxity = \case
          ty1:ty2:tyTail ->
            pure $ BuiltinType_Tuple boxity ty1 (ty2 NE.:| tyTail)
          other ->
            fail $ "Tuple size must be >= 2 but size is: " <> show (length other)

instance NFData Boxity
instance (NFData ty, NFData tycon) => NFData (BuiltinType tycon ty)
