{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
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
import Types (FgType, FgTyCon)

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

instance Foldable FunctionType where
  foldr f b ft =
    foldr f b [functionType_arg ft, functionType_ret ft]

instance Traversable FunctionType where
  traverse f ft =
    (\case {[arg, ret] -> FunctionType arg ret; _ -> error "unpossible"})
      <$> traverse f [functionType_arg ft, functionType_ret ft]

data TypeInfo tycon = TypeInfo
  { typeInfo_fullyQualified :: FunctionType (FgType tycon)
  , typeInfo_tmpUnexpanded :: FunctionType (FgType tycon) -- ^ TODO: contains type synonyms
  } deriving (Eq, Show, Ord, Functor, Generic)

instance (A.ToJSON tycon) => A.ToJSON (TypeInfo tycon)
instance (A.FromJSON tycon) => A.FromJSON (TypeInfo tycon)
instance (NFData tycon) => NFData (TypeInfo tycon)

newtype ModuleDeclarations value = ModuleDeclarations
  { moduleDeclarations_map :: Map value (Map value (TypeInfo (FgTyCon value)))
    -- ^ Map from module name to a map of unqualified function names to 'TypeInfo'
  } deriving (Eq, Show, Ord, A.ToJSON, A.FromJSON, NFData)

fmapModuleDeclarations
  :: Ord b
  => (a -> b)
  -> ModuleDeclarations a
  -> ModuleDeclarations b
fmapModuleDeclarations f (ModuleDeclarations map') = ModuleDeclarations $
  Map.mapKeys f (fmap (Map.mapKeys f . fmap (fmap (fmap f))) map')

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
