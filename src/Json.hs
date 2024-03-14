{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
module Json
( FunctionType(..)
, TypeInfo(..)
, ModuleDeclarations(..), fmapModuleDeclarations
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
import qualified Control.Exception as Ex
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import Types (BuiltinType)
import Data.Bifunctor (Bifunctor(..))

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
  } deriving (Eq, Show, Ord, Functor, Generic)

instance A.ToJSON value => A.ToJSON (FunctionType value)
instance A.FromJSON value => A.FromJSON (FunctionType value)
instance NFData value => NFData (FunctionType value)

data TypeInfo tycon ty = TypeInfo
  { typeInfo_fullyQualified :: FunctionType (BuiltinType tycon ty)
  , typeInfo_unqualified :: FunctionType (BuiltinType tycon ty)
  , typeInfo_tmpUnexpanded :: FunctionType (BuiltinType tycon ty) -- ^ TODO: contains type synonyms
  } deriving (Eq, Show, Ord, Functor, Generic)

instance Bifunctor TypeInfo where
  bimap f g (TypeInfo a b c) =
    TypeInfo (fmap (bimap f g) a) (fmap (bimap f g) b) (fmap (bimap f g) c)

instance (A.ToJSON tycon, A.ToJSON ty) => A.ToJSON (TypeInfo tycon ty)
instance (A.FromJSON tycon, A.FromJSON ty) => A.FromJSON (TypeInfo tycon ty)
instance (NFData tycon, NFData ty) => NFData (TypeInfo tycon ty)

newtype ModuleDeclarations value = ModuleDeclarations
  { moduleDeclarations_map :: Map value (Map value (TypeInfo value value))
    -- ^ Map from module name to a map of unqualified function names to 'TypeInfo'
  } deriving (Eq, Show, Ord, A.ToJSON, A.FromJSON, NFData)

fmapModuleDeclarations
  :: Ord b
  => (a -> b)
  -> ModuleDeclarations a
  -> ModuleDeclarations b
fmapModuleDeclarations f (ModuleDeclarations map') = ModuleDeclarations $
  Map.mapKeys f (fmap (Map.mapKeys f . fmap (bimap f f)) map')

data DeclarationMapJson value = DeclarationMapJson
  { declarationMapJson_package :: value
  , declarationMapJson_moduleDeclarations :: ModuleDeclarations value
  } deriving (Generic, Show)

instance NFData a => NFData (DeclarationMapJson a)

fmapDeclarationMapJson
  :: Ord b
  => (a -> b)
  -> DeclarationMapJson a
  -> DeclarationMapJson b
fmapDeclarationMapJson f dmj =
  DeclarationMapJson
    { declarationMapJson_package = f $ declarationMapJson_package dmj
    , declarationMapJson_moduleDeclarations = fmapModuleDeclarations f $ declarationMapJson_moduleDeclarations dmj

    }

instance (A.ToJSONKey value, A.ToJSON value) => A.ToJSON (DeclarationMapJson value)
instance (Ord value, A.FromJSONKey value, A.FromJSON value) => A.FromJSON (DeclarationMapJson value)
