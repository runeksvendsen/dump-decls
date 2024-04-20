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
-- TODO: Merge "Types" and "JSON"?
module Types
( -- * 'FgType'
  FgType(..), renderFgType
, Boxity(..)
, isBoxed
  -- * 'FgTyCon'
, FgTyCon(..), parsePprTyCon, renderFgTyConQualified, TyConParseError(..), renderTyConParseError
  -- * Rendering 'FgType (FgTyCon T.Text)'
, renderFgTypeFgTyConUnqualified, renderFgTypeFgTyConQualified, renderFgTypeFgTyConQualifiedNoPackage
  -- * 'FgPackage'
, FgPackage(..), parsePackageWithVersion, renderFgPackage
  -- * (For testing)
, splitByEndNonEmpty,
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
  , fgTyConPackage :: FgPackage text
    -- ^ Package
  } deriving (Eq, Show, Ord, Generic, Functor)

instance (A.ToJSON a) => A.ToJSON (FgTyCon a)
instance (A.FromJSON a) => A.FromJSON (FgTyCon a)
instance (NFData a) => NFData (FgTyCon a)

-- | Render in the format /package_name-package_version:module_name.name/.
--   The inverse of 'parsePprTyCon'.
--
-- Example:
--
-- >>> :set -XOverloadedStrings
-- >>> renderFgTyConQualified (FgTyCon "Text" "Data.Text.Internal" (FgPackage "text" "2.0.2"))
-- "text-2.0.2:Data.Text.Internal.Text"
renderFgTyConQualified
  :: FgTyCon T.Text
  -> T.Text
renderFgTyConQualified tc =
  T.concat
    [ renderFgPackage $ fgTyConPackage tc
    , ":"
    , fgTyConModule tc
    , "."
    , fgTyConName tc
    ]

-- | Render in the format /module_name.name/.
--
-- Example:
--
-- >>> :set -XOverloadedStrings
-- >>> renderFgTyConQualified (FgTyCon "Text" "Data.Text.Internal" (FgPackage "text" "2.0.2"))
-- "Data.Text.Internal.Text"
renderFgTyConQualifiedNoPackage
  :: FgTyCon T.Text
  -> T.Text
renderFgTyConQualifiedNoPackage tc =
  T.concat
    [ fgTyConModule tc
    , "."
    , fgTyConName tc
    ]

-- | A package used by /Haskell Function Graph/.
data FgPackage text = FgPackage
  { fgPackageName :: text
    -- ^ Package name, e.g. @text@
  , fgPackageVersion :: text
    -- ^ Package version, e.g. @2.0.2@
  } deriving (Eq, Show, Ord, Generic, Functor)

instance (A.ToJSON a) => A.ToJSON (FgPackage a)
instance (A.FromJSON a) => A.FromJSON (FgPackage a)
instance (NFData a) => NFData (FgPackage a)

-- | Inverse of 'parsePackageWithVersion'
renderFgPackage
  :: FgPackage T.Text
  -> T.Text
renderFgPackage p =
  fgPackageName p <> "-" <> fgPackageVersion p

-- | An error converting a 'GHC.Core.TyCon.TyCon' into an 'FgTyCon'
data TyConParseError = TyConParseError
  { tyConParseErrorMsg :: String
    -- ^ A human-readable message about what failed
  , tyConParseErrorInput :: T.Text
    -- ^ The pretty-printed 'GHC.Core.TyCon.TyCon' for which parsing failed
  , tyConParseErrorFunctionName :: T.Text
    -- ^ Fully qualified name (includes module but not package) of the function for which the failure occurred.
    --
    --   The failure occurred for either this function's argument type or return type (TODO: specify which).
  , tyConParseErrorPackage :: FgPackage T.Text
    -- ^ The package that contains 'tyConParseErrorFunctionName'
  , tyConParseErrorSrcLoc :: T.Text
    -- ^ The source location of the 'GHC.Core.TyCon.TyCon'
  } deriving (Eq, Show, Ord, Generic)

instance A.ToJSON TyConParseError
instance A.FromJSON TyConParseError
instance NFData TyConParseError

-- | Render a 'TyConParseError' as a human-readable text string
renderTyConParseError
  :: TyConParseError
  -> T.Text
renderTyConParseError e = T.unwords
  [ T.pack (tyConParseErrorMsg e) <> "."
  , "Function:", tyConParseErrorFunctionName e <> "."
  , "Package:", renderFgPackage (tyConParseErrorPackage e) <> "."
  , "SrcLoc:", tyConParseErrorSrcLoc e <> "."
  ]

-- | Types supported by /Haskell Function Graph/.
--
--   Currently only type constructor applications are supported,
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
      -- E.g. 'Maybe', 'Either', 'IO', 'Data.Map.Map'.
      [FgType tycon]
      -- ^ All arguments to the type constructor (fully saturated application).
      -- The empty list if the type constructor does not take any arguments
      --  (e.g. 'Int', 'Char', 'Data.Text.Text') and otherwise one type for all type variables
      --  of the type constructor (since we're only looking at the types of functions
      --  exported from a module where a partially applied type constructor is invalid).
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

-- | A /boxed/ value is one that's represented by a pointer to the actual data representing the value.
--   An /unboxed/ value is represented by the actual data (no pointer).
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

-- | Render a 'FgType' to Haskell syntax.
--
-- Examples:
--
-- >>> renderFgType id $ FgType_TyConApp "Either" [FgType_TyConApp "String" [], FgType_TyConApp "Value" []]
-- "Either String Value"
--
-- >>> renderFgType id $ FgType_List $ FgType_Tuple Boxed (FgType_TyConApp "Key" []) (NE.singleton $ FgType_TyConApp "Value" [])
-- "[(Key, Value)]"
--
-- >>> renderFgType id $ FgType_TyConApp "Either" [FgType_TyConApp "String" [], FgType_TyConApp "IO" [FgType_Unit]]
-- "Either String (IO ())"
renderFgType
  :: forall tycon.
     (tycon -> T.Text)
  -> FgType tycon
  -> T.Text
renderFgType renderTycon fgType' =
  let
    parens :: T.Text -> T.Text
    parens txt = "(" <> txt <> ")"

    tupleParens Boxed = parens
    tupleParens Unboxed = \txt -> "(#" <> txt <> "#)"

    go :: Bool -> FgType tycon -> T.Text
    go parenthesize = \case
      FgType_TyConApp tycon fgTypeList ->
        (if not (null fgTypeList) && parenthesize then parens else id) $
          T.unwords $ renderTycon tycon : map (go True) fgTypeList
      FgType_List fgType ->
        "[" <> go False fgType <> "]"
      FgType_Tuple boxity fgType fgTypeList ->
        tupleParens boxity $ T.intercalate ", " $ map (go False) (fgType : NE.toList fgTypeList)
      FgType_Unit ->
        "()"
  in go False fgType'

-- | Render only the 'fgTyConName' of the 'FgTyCon'.
--
-- Examples:
--
-- >>> let Right ioTycon = parsePprTyCon "ghc-prim-0.10.0:GHC.Types.IO"
-- >>> renderFgTypeFgTyConUnqualified $ FgType_TyConApp ioTycon [FgType_Unit]
-- "IO ()"
renderFgTypeFgTyConUnqualified
  :: FgType (FgTyCon T.Text)
  -> T.Text
renderFgTypeFgTyConUnqualified =
  renderFgType fgTyConName

-- | Render using 'renderFgTyConQualified' for the 'FgTyCon'.
--
-- Examples:
--
-- >>> let Right ioTycon = parsePprTyCon "ghc-prim-0.10.0:GHC.Types.IO"
-- >>> renderFgTypeFgTyConQualified $ FgType_TyConApp ioTycon [FgType_Unit]
-- "ghc-prim-0.10.0:GHC.Types.IO ()"
renderFgTypeFgTyConQualified
  :: FgType (FgTyCon T.Text)
  -> T.Text
renderFgTypeFgTyConQualified =
  renderFgType renderFgTyConQualified

-- | Render using 'renderFgTyConQualifiedNoPackage' for the 'FgTyCon'.
--
-- Examples:
--
-- >>> let Right ioTycon = parsePprTyCon "ghc-prim-0.10.0:GHC.Types.IO"
-- >>> renderFgTypeFgTyConQualifiedNoPackage $ FgType_TyConApp ioTycon [FgType_Unit]
-- "GHC.Types.IO ()"
renderFgTypeFgTyConQualifiedNoPackage
  :: FgType (FgTyCon T.Text)
  -> T.Text
renderFgTypeFgTyConQualifiedNoPackage =
  renderFgType renderFgTyConQualifiedNoPackage

-- | Parse a 'FgPackage' from a string of the form /package_name-package_version/.
--
-- Examples:
--
-- >>> :set -XOverloadedStrings
-- >>> parsePackageWithVersion "base-4.18.0.0"
-- Right (FgPackage {fgPackageName = "base", fgPackageVersion = "4.18.0.0"})
--
-- >>> :set -XOverloadedStrings
-- >>> parsePackageWithVersion "text-2.0.2"
-- Right (FgPackage {fgPackageName = "text", fgPackageVersion = "2.0.2"})
parsePackageWithVersion
  :: T.Text
  -> Either String (FgPackage T.Text)
parsePackageWithVersion packageAndVersion = do
  (packageName, packageVersion) <-
    splitByEndNonEmpty "invalid package identifier" '-' packageAndVersion
  pure $ FgPackage packageName packageVersion

-- | Parse a 'FgTyCon' from a 'GHC.Core.TyCon.TyCon' pretty-printed in fully-qualified form
--  (e.g. "base-4.18.0.0:Data.Either.Either").
--  The inverse of 'renderFgTyConQualified'.
--
-- NOTE: Does not handle the /built-in/ types: list, tuple (including unit).
--
-- A 'Left' signifies a bug in the parser.
--
-- Examples:
--
-- >>> :set -XOverloadedStrings
-- >>> parsePprTyCon "base-4.18.0.0:Data.Either.Either"
-- Right (FgTyCon {fgTyConName = "Either", fgTyConModule = "Data.Either", fgTyConPackage = FgPackage {fgPackageName = "base", fgPackageVersion = "4.18.0.0"}})
--
-- >>> :set -XOverloadedStrings
-- >>> parsePprTyCon "text-2.0.2:Data.Text.Internal.Text"
-- Right (FgTyCon {fgTyConName = "Text", fgTyConModule = "Data.Text.Internal", fgTyConPackage = FgPackage {fgPackageName = "text", fgPackageVersion = "2.0.2"}})
--
-- >>> :set -XOverloadedStrings
-- >>> parsePprTyCon "base-4.18.0.0:GHC.Maybe.Maybe"
-- Right (FgTyCon {fgTyConName = "Maybe", fgTyConModule = "GHC.Maybe", fgTyConPackage = FgPackage {fgPackageName = "base", fgPackageVersion = "4.18.0.0"}})
parsePprTyCon :: T.Text -> Either String (FgTyCon T.Text)
parsePprTyCon str = do
  (packageAndVersion, fqn) <- case T.splitOn ":" str of
    [packageAndVersion, fqn] -> pure (packageAndVersion, fqn)
    _ -> Left $ "missing colon in " <> show (T.unpack str)
  package <- parsePackageWithVersion packageAndVersion
  (moduleName, name) <-
    splitByEndNonEmpty "invalid fully qualified identifier" '.' fqn
  pure $ FgTyCon
    { fgTyConName = name
    , fgTyConModule = moduleName
    , fgTyConPackage = package
    }

-- | Split string by last occurence of character.
--   Return pair of non-empty text strings before and after character (neither string includes the character).
--
-- Example:
--
-- >>> :set -XOverloadedStrings
-- >>> splitByEndNonEmpty "oops" '.' "Data.ByteString.Lazy.Internal.ByteString"
-- Right ("Data.ByteString.Lazy.Internal","ByteString")
splitByEndNonEmpty
  :: String -- Error prefix
  -> Char -- Split by this character
  -> T.Text -- String to split
  -> Either String (T.Text, T.Text)
splitByEndNonEmpty err char str' =
  case Compat.Aeson.spanEnd (/= char) str' of
    (a', b)
      | Just (a, _) <- T.unsnoc a' -- remove trailing "char"
      , not (T.null b) && not (T.null a) ->
        pure (a, b)
    _ -> Left $ err <> " in " <> show (T.unpack str')
