{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
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
import Data.Aeson.KeyMap ((!?))
import qualified Data.Aeson.Types as A
import Control.Monad ((>=>))

-- | Either just a type, a list or a tuple
data BuiltinType ty
  = BuiltinType_Type ty
  -- ^ Just the type 'ty'. Guaranteed to /not/ be either a list or tuple.
  | BuiltinType_List (BuiltinType ty)
  -- ^ A list
  | BuiltinType_Tuple Boxity (BuiltinType ty) (NE.NonEmpty (BuiltinType ty))
  -- ^ A tuple of size @1 + length nonEmptyList@
    deriving (Eq, Show, Ord, Generic)

instance Functor BuiltinType where
  fmap f = \case
    BuiltinType_Type ty -> BuiltinType_Type $ f ty
    BuiltinType_List bty -> BuiltinType_List $ fmap f bty
    BuiltinType_Tuple boxity bty neBty -> BuiltinType_Tuple boxity (fmap f bty) (NE.map (fmap f) neBty)

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

instance A.ToJSON ty => A.ToJSON (BuiltinType ty) where
  toJSON = \case
    BuiltinType_Type ty -> A.object
      [("type", A.toJSON ty)]
    BuiltinType_List bty -> A.object
      [("list", A.toJSON bty)]
    BuiltinType_Tuple boxity bty neBty ->
      let key = case boxity of {Unboxed -> "tuple#"; Boxed -> "tuple"}
      in A.object
        [(key, A.toJSON $ bty : NE.toList neBty)]

instance A.FromJSON ty => A.FromJSON (BuiltinType ty) where
  parseJSON = A.withObject "BuiltinType" $ \o -> do
        parseKind o "type" (pure . BuiltinType_Type)
    <|> parseKind o "list" (pure . BuiltinType_List)
    <|> parseKind o "tuple" (tupleFromList Boxed)
    <|> parseKind o "tuple#" (tupleFromList Unboxed)
      where
        -- apply function to value if (key,value) exists in the object
        parseKind
          :: A.FromJSON a
          => A.Object
          -> A.Key
          -> (a -> A.Parser (BuiltinType ty))
          -> A.Parser (BuiltinType ty)
        parseKind o keyTxt mkType =
          maybe empty (A.parseJSON >=> mkType) (o !? keyTxt)

        tupleFromList boxity = \case
          ty1:ty2:tyTail ->
            pure $ BuiltinType_Tuple boxity ty1 (ty2 NE.:| tyTail)
          other ->
            fail $ "Tuple size must be >= 2 but size is: " <> show (length other)

instance NFData Boxity
instance NFData ty => NFData (BuiltinType ty)
