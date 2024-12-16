{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Json
( FunctionType(..)
, TypeInfo(..)
, ModuleDeclarations(..), fmapModuleDeclarations, explodeModuleDeclarations
, DeclarationMapJson(..), fmapDeclarationMapJson
  -- * Util
, streamPrintJsonList
  -- * Re-exports
, Map, A.ToJSON
)
where

import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import Data.List (intersperse)
import Control.DeepSeq (NFData)
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Control.Exception as Ex
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import Types (FgType, FgTyCon, TyConParseError, FgPackage)

streamPrintJsonList
  :: A.ToJSON a
  => [a]
  -> IO ()
streamPrintJsonList jsonList =
  Ex.bracket_
    (putStr "[ ")
    (putStrLn "]") $
    (sequence_ $
      intersperse
        (BSL.putStr ", ")
        (map (BSL.putStrLn . A.encode) jsonList)
    )

data FunctionType value = FunctionType
  { functionType_arg :: value
  , functionType_ret :: value
  } deriving (Eq, Show, Ord, Functor, Foldable, Generic)

instance A.ToJSON value => A.ToJSON (FunctionType value) where
  toJSON (FunctionType a b) = A.toJSON (a, b)
  toEncoding (FunctionType a b) = A.toEncoding (a, b)
instance A.FromJSON value => A.FromJSON (FunctionType value) where
  parseJSON = A.withArray "FunctionType" $ \vec -> do
    !mVal0 <- V.indexM vec 0
    !mVal1 <- V.indexM vec 1
    !val0 <- A.parseJSON mVal0
    !val1 <- A.parseJSON mVal1
    pure $! FunctionType val0 val1

instance NFData value => NFData (FunctionType value)

instance Traversable FunctionType where
  traverse f ft =
    FunctionType <$> f (functionType_arg ft) <*> f (functionType_ret ft)

data TypeInfo tycon = TypeInfo
  { typeInfo_expanded :: Maybe (FunctionType tycon)
    -- ^ Does not contain type synonyms. 'Nothing' if there are no type synonyms in 'typeInfo_unexpanded'.
  , typeInfo_unexpanded :: FunctionType tycon
    -- ^ Potentially contains type synonyms
  } deriving (Eq, Show, Ord, Functor, Foldable, Generic)

instance A.ToJSON value => A.ToJSON (TypeInfo value) where
  toJSON (TypeInfo a b) = A.toJSON (a, b)
  toEncoding (TypeInfo a b) = A.toEncoding (a, b)
instance A.FromJSON value => A.FromJSON (TypeInfo value) where
  parseJSON = A.withArray "TypeInfo" $ \vec -> do
    !mVal0 <- V.indexM vec 0
    !mVal1 <- V.indexM vec 1
    !val0 <- A.parseJSON mVal0
    !val1 <- A.parseJSON mVal1
    pure $! TypeInfo val0 val1
instance (NFData tycon) => NFData (TypeInfo tycon)

instance Traversable TypeInfo where
  traverse f ti =
    TypeInfo
      <$> traverse (traverse f) (typeInfo_expanded ti)
      <*> traverse f (typeInfo_unexpanded ti)

data ModuleDeclarations value = ModuleDeclarations
  { moduleDeclarations_map :: Map value (Map value (TypeInfo (FgType (FgTyCon value))))
    -- ^ Map from module name to a map of unqualified function names to 'TypeInfo'
  , moduleDeclarations_mapFail :: Map value (Map value TyConParseError)
    -- ^ Declarations for which an error occurred converting a 'GHC.Core.TyCon.TyCon' into a 'FgTyCon'.
    --   This is probably a bug in 'Types.parsePprTyCon'.
  } deriving (Eq, Show, Ord, Generic)

instance (A.ToJSON value, A.ToJSONKey value) => A.ToJSON (ModuleDeclarations value) where
  toJSON (ModuleDeclarations a b) = A.toJSON (a, b)
  toEncoding (ModuleDeclarations a b) = A.toEncoding (a, b)
instance (A.FromJSON value, A.FromJSONKey value, Ord value) => A.FromJSON (ModuleDeclarations value) where
  parseJSON = A.withArray "ModuleDeclarations" $ \vec -> do
    !mVal0 <- V.indexM vec 0
    !mVal1 <- V.indexM vec 1
    !val0 <- A.parseJSON mVal0
    !val1 <- A.parseJSON mVal1
    pure $! ModuleDeclarations val0 val1

instance (NFData a) => NFData (ModuleDeclarations a)

fmapModuleDeclarations
  :: Ord b
  => (a -> b)
  -> ModuleDeclarations a
  -> ModuleDeclarations b
fmapModuleDeclarations f (ModuleDeclarations map' mapFail) = ModuleDeclarations
  (Map.mapKeys f (fmap (Map.mapKeys f . fmap (fmap (fmap (fmap f)))) map'))
  (Map.mapKeys f (fmap (Map.mapKeys f) mapFail))

explodeModuleDeclarations
  :: ModuleDeclarations value
  -> [(value, (value, TypeInfo (FgType (FgTyCon value))))]
explodeModuleDeclarations =
  concatMap (\(value, lst) -> map (value,) lst)
    . Map.toList
    . fmap Map.toList
    . moduleDeclarations_map

data DeclarationMapJson value = DeclarationMapJson
  { declarationMapJson_package :: FgPackage value
  , declarationMapJson_moduleDeclarations :: ModuleDeclarations value
  } deriving (Eq, Generic, Show)

instance NFData a => NFData (DeclarationMapJson a)

fmapDeclarationMapJson
  :: Ord b
  => (a -> b)
  -> DeclarationMapJson a
  -> DeclarationMapJson b
fmapDeclarationMapJson f dmj =
  DeclarationMapJson
    { declarationMapJson_package = f <$> declarationMapJson_package dmj
    , declarationMapJson_moduleDeclarations = fmapModuleDeclarations f $ declarationMapJson_moduleDeclarations dmj
    }

instance (A.ToJSONKey value, A.ToJSON value) => A.ToJSON (DeclarationMapJson value) where
  toJSON (DeclarationMapJson a b) = A.toJSON (a, b)
  toEncoding (DeclarationMapJson a b) = A.toEncoding (a, b)
instance (Ord value, A.FromJSONKey value, A.FromJSON value) => A.FromJSON (DeclarationMapJson value) where
  parseJSON = A.withArray "DeclarationMapJson" $ \vec -> do
    !mVal0 <- V.indexM vec 0
    !mVal1 <- V.indexM vec 1
    !val0 <- A.parseJSON mVal0
    !val1 <- A.parseJSON mVal1
    pure $! DeclarationMapJson val0 val1
