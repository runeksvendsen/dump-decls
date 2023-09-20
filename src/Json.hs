{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Json
( FunctionType(..)
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
import qualified Data.Aeson as A
import qualified Control.Exception as Ex
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map

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
  { functionType_arg :: !value
  , functionType_ret :: !value
  , functionType_srcSpan :: !String -- TODO: doesn't belong here
  } deriving (Eq, Show, Ord, Functor, Generic)

instance A.ToJSON value => A.ToJSON (FunctionType value)
instance A.FromJSON value => A.FromJSON (FunctionType value)

newtype ModuleDeclarations value = ModuleDeclarations
  { moduleDeclarations_map :: Map value (Map value (FunctionType value))
    -- ^ Map from module name to a map of function names to 'FunctionType'
  } deriving (Eq, Show, Ord, A.ToJSON, A.FromJSON)

fmapModuleDeclarations
  :: Ord b
  => (a -> b)
  -> ModuleDeclarations a
  -> ModuleDeclarations b
fmapModuleDeclarations f (ModuleDeclarations map') = ModuleDeclarations $
  Map.mapKeys f (fmap (Map.mapKeys f . fmap (fmap f)) map')

data DeclarationMapJson value = DeclarationMapJson
  { declarationMapJson_package :: value
  , declarationMapJson_moduleDeclarations :: ModuleDeclarations value
  } deriving (Generic, Show)

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
