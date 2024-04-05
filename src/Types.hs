{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- TODO: Test 'Data.Aeson.encode
module Types
( FgType(..)
, Boxity(..)
, FgTyCon(..), parsePprTyCon, TyConParseError(..)
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
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Identity (Identity(Identity))

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
  } deriving (Eq, Show, Ord, Generic, Functor)

instance (A.ToJSON a) => A.ToJSON (FgTyCon a)
instance (A.FromJSON a) => A.FromJSON (FgTyCon a)
instance (NFData a) => NFData (FgTyCon a)

-- | An error converting a GHC TyCon into an 'FgTyCon'
data TyConParseError = TyConParseError { unTyConParseError:: String } -- TODO: not just a String
  deriving (Eq, Show, Ord, Generic)

instance A.ToJSON TyConParseError
instance A.FromJSON TyConParseError
instance NFData TyConParseError


-- | Types supported by /Haskell Function Graph/.
--
--   Currently only type constructors applications are supported,
--   which does not include functions (the arrow type constructor).
--
--   More will be added later, probably.
--
--   Type constructors that have special syntax are handled separately: lists, tuples, unit.
data FgType tycon
  = FgType_TyConApp
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
  | FgType_List (FgType tycon)
  -- ^ A list
  | FgType_Tuple Boxity (FgType tycon) (NE.NonEmpty (FgType tycon))
  -- ^ A tuple of size @1 + length nonEmptyList@
  | FgType_Unit
  -- ^ Unit ('()')
    deriving (Eq, Show, Ord, Foldable, Generic)

instance Functor FgType where
  fmap f = \case
    FgType_TyConApp tycon tyList ->
      FgType_TyConApp (f tycon) (map (fmap f) tyList)
    FgType_List bty ->
      FgType_List $ fmap f bty
    FgType_Tuple boxity bty neBty ->
      FgType_Tuple boxity (fmap f bty) (NE.map (fmap f) neBty)
    FgType_Unit ->
      FgType_Unit

instance Traversable FgType where
  traverse f = \case
    FgType_TyConApp tycon tyList ->
      FgType_TyConApp <$> f tycon <*> traverse (traverse f) tyList
    FgType_List bty ->
      FgType_List <$> traverse f bty
    FgType_Tuple boxity bty neBty ->
      FgType_Tuple boxity <$> traverse f bty <*> traverse (traverse f) neBty
    FgType_Unit ->
      pure FgType_Unit

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
    FgType_TyConApp tycon tyList -> A.object
      [("type", A.object [("tycon" :: Compat.Aeson.Key, A.toJSON tycon), ("tycon_args", A.toJSON tyList)])]
    FgType_List bty -> A.object
      [("list", A.toJSON bty)]
    FgType_Tuple boxity bty neBty ->
      let key = case boxity of {Unboxed -> "tuple#"; Boxed -> "tuple"}
      in A.object
        [(key, A.toJSON $ bty : NE.toList neBty)]
    FgType_Unit -> A.String "unit"

instance (A.FromJSON tycon) => A.FromJSON (FgType tycon) where
  parseJSON = \case
    A.String "unit" -> pure FgType_Unit
    A.Object o -> parseObject o
    val -> failParse val
    where
      parseObject o = do
            parseKind o "type" (\o' ->  FgType_TyConApp <$> o' A..: "tycon" <*> o' A..: "tycon_args")
        <|> parseKind o "list" (pure . FgType_List)
        <|> parseKind o "tuple" (tupleFromList Boxed)
        <|> parseKind o "tuple#" (tupleFromList Unboxed)
        <|> failParse (A.Object o)

      failParse val = fail $ unwords
        [ "expected one of:"
        , "the string 'unit',"
        , "an object with one of the keys"
        , "'type',"
        , "'list',"
        , "'tuple',"
        , "'tuple#'."
        , "found:" , UTF8.decode . BSL.unpack $ A.encode val
        ]

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
          pure $ FgType_Tuple boxity ty1 (ty2 NE.:| tyTail)
        other ->
          fail $ "Tuple size must be >= 2 but size is: " <> show (length other)

instance NFData Boxity
instance (NFData tycon) => NFData (FgType tycon)

-- | Parse a 'FgTyCon' from a GHC @TyCon@ pretty-printed in fully-qualified form
--  (e.g. "base-4.18.0.0:Data.Either.Either").
--
-- NOTE: Does not handle the /built-in/ types: list, tuple (including unit).
--
-- A 'Left' signifies a bug in the parser.
--
-- Examples:
--
-- >>> parsePprTyCon "base-4.18.0.0:Data.Either.Either"
-- Right (FgTyCon {fgTyConName = "Either", fgTyConModule = "Data.Either", fgTyConPackageName = "base", fgTyConPackageVersion = "4.18.0.0"})
--
-- >>> parsePprTyCon "text-2.0.2:Data.Text.Internal.Text"
-- Right (FgTyCon {fgTyConName = "Text", fgTyConModule = "Data.Text.Internal", fgTyConPackageName = "text", fgTyConPackageVersion = "2.0.2"})
--
-- >>> parsePprTyCon "base-4.18.0.0:GHC.Maybe.Maybe"
-- Right (FgTyCon {fgTyConName = "Maybe", fgTyConModule = "GHC.Maybe", fgTyConPackageName = "base", fgTyConPackageVersion = "4.18.0.0"})
parsePprTyCon :: T.Text -> Either String (FgTyCon T.Text)
parsePprTyCon str = do
  (packageAndVersion, fqn) <- case T.splitOn ":" str of
    [packageAndVersion, fqn] -> pure (packageAndVersion, fqn)
    _ -> Left $ "missing colon in " <> show (T.unpack str)
  (packageName, packageVersion) <-
    splitByNonEmpty "invalid package identifier" '-' packageAndVersion
  (moduleName, name) <-
    splitByNonEmpty "invalid fully qualified identifier" '.' fqn
  pure $ FgTyCon
    { fgTyConName = name
    , fgTyConModule = moduleName
    , fgTyConPackageName = packageName
    , fgTyConPackageVersion = packageVersion
    }
  where
    -- split by "char" and return pair of non-empty text strings
    splitByNonEmpty err char str' =
      case T.spanEndM (pure . (/= char)) str' of
        Identity (a', b)
          | Just (a, _) <- T.unsnoc a' -- remove trailing "char"
          , not (T.null b) && not (T.null a) ->
            pure (a, b)
        _ -> Left $ err <> " in " <> show (T.unpack str')
