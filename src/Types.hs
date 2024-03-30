{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveFunctor #-}
-- |
-- TODO: Test 'Data.Aeson.encode
module Types
( FgType(..)
, Boxity(..)
, FgTyCon(..)
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

-- | A fully qualified type constructor used by /Haskell Function Graph/.
--
--   Examples:
--      @text-2.0.2:Data.Text.Internal.Text@,
--      @base-4.18.0.0:GHC.Maybe.Maybe@,
--      @base-4.18.0.0:Data.Either.Either@
data FgTyCon text = FgTyCon
  { fgTyConName :: text
    -- ^ Name, e.g. the @Text@ in @text-2.0.2:Data.Text.Internal.Text@
  , fgTyConModule :: text
    -- ^ Module, e.g. the @Data.Text.Internal@ in @text-2.0.2:Data.Text.Internal.Text@
  , fgTyConPackageName :: text
    -- ^ Package name, e.g. the @text@ in @text-2.0.2:Data.Text.Internal.Text@
  , fgTyConPackageVersion :: text
    -- ^ Package version, e.g. the @2.0.2@ in @text-2.0.2:Data.Text.Internal.Text@
  } deriving (Eq, Show, Ord, Generic, Functor) -- A.ToJSON, A.FromJSON, NFData)

instance (A.ToJSON a) => A.ToJSON (FgTyCon a)
instance (A.FromJSON a) => A.FromJSON (FgTyCon a)
instance (NFData a) => NFData (FgTyCon a)

-- | Types supported by /Haskell Function Graph/.
--
--   Currently only type constructors applications are supported,
--   which does not include functions (the arrow type constructor).
--
--   More will be added later, probably.
data FgType tycon
  = BuiltinType_TyConApp
    -- ^ A type consisting of (1) a type constructor,
    -- and (2) the types to which the constructor is applied.
      tycon
      -- ^ A /type constructor/.
      -- Essentially the name of a type without any of its type variables filled in.
      -- E.g. 'Maybe', 'Either', 'IO', 'Map'.
      [FgType tycon]
      -- ^ All arguments to the type constructor (fully saturated application).
      -- The empty list if the type constructor does not take any arguments
      --  (e.g. 'Int', 'Char', 'Text') and otherwise one type for all type variables
      --  of the type constructor (since we're only looking at the types of functions
      --  exported from a module where a partially applied type constructor is invalid).
  -- ^ A type that's neither a list nor a tuple
  | BuiltinType_List (FgType tycon)
  -- ^ A list
  | BuiltinType_Tuple Boxity (FgType tycon) (NE.NonEmpty (FgType tycon))
  -- ^ A tuple of size @1 + length nonEmptyList@
    deriving (Eq, Show, Ord, Generic)

instance Functor FgType where
  fmap f = \case
    BuiltinType_TyConApp tycon tyList ->
      BuiltinType_TyConApp (f tycon) (map (fmap f) tyList)
    BuiltinType_List bty ->
      BuiltinType_List $ fmap f bty
    BuiltinType_Tuple boxity bty neBty ->
      BuiltinType_Tuple boxity (fmap f bty) (NE.map (fmap f) neBty)

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

instance (A.ToJSON tycon) => A.ToJSON (FgType tycon) where
  toJSON = \case
    BuiltinType_TyConApp tycon tyList -> A.object
      [("type", A.toJSON [("tycon" :: Compat.Aeson.Key, A.toJSON tycon), ("tycon_args", A.toJSON tyList)])]
    BuiltinType_List bty -> A.object
      [("list", A.toJSON bty)]
    BuiltinType_Tuple boxity bty neBty ->
      let key = case boxity of {Unboxed -> "tuple#"; Boxed -> "tuple"}
      in A.object
        [(key, A.toJSON $ bty : NE.toList neBty)]

instance (A.FromJSON tycon) => A.FromJSON (FgType tycon) where
  parseJSON = A.withObject "FgType" $ \o -> do
        parseKind o "type" (\o' ->  BuiltinType_TyConApp <$> o' A..: "tycon" <*> o' A..: "tycon_args")
    <|> parseKind o "list" (pure . BuiltinType_List)
    <|> parseKind o "tuple" (tupleFromList Boxed)
    <|> parseKind o "tuple#" (tupleFromList Unboxed)
      where
        -- apply function to value if (key,value) exists in the object
        parseKind
          :: A.FromJSON a
          => A.Object
          -> Compat.Aeson.Key
          -> (a -> A.Parser (FgType tycon))
          -> A.Parser (FgType tycon)
        parseKind o keyTxt mkType =
          maybe empty (A.parseJSON >=> mkType) (Compat.Aeson.lookup keyTxt o)

        tupleFromList boxity = \case
          ty1:ty2:tyTail ->
            pure $ BuiltinType_Tuple boxity ty1 (ty2 NE.:| tyTail)
          other ->
            fail $ "Tuple size must be >= 2 but size is: " <> show (length other)

instance NFData Boxity
instance (NFData tycon) => NFData (FgType tycon)
