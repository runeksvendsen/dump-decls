{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}
module Json
( ModuleDeclarations(..), explodeModuleDeclarations
, DeclarationMapJson(..)
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
import Types (FgPackage)
import qualified Types.Doodle as Doodle
import Types.FunctionInfo (FunctionInfo)

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

data ModuleDeclarations value = ModuleDeclarations
  { moduleDeclarations_map :: Map value (Map value (FunctionInfo Doodle.SomeFunction)) -- WIP: move somewhere else
    -- ^ Map from module name to a map of unqualified function names to 'TypeInfo'
  , moduleDeclarations_mapFail :: Map value (Map value (FunctionInfo Doodle.FgError))
    -- ^ TODO
  } deriving (Eq, Show, Ord, Generic)

instance (A.ToJSON a, A.ToJSONKey a) => A.ToJSON (ModuleDeclarations a)
instance (A.FromJSON a, A.FromJSONKey a, Ord a) => A.FromJSON (ModuleDeclarations a)
instance (NFData a) => NFData (ModuleDeclarations a)

explodeModuleDeclarations
  :: ModuleDeclarations value
  -> [(value, (value, FunctionInfo Doodle.SomeFunction))]
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

instance (A.ToJSONKey value, A.ToJSON value) => A.ToJSON (DeclarationMapJson value)
instance (Ord value, A.FromJSONKey value, A.FromJSON value) => A.FromJSON (DeclarationMapJson value)
