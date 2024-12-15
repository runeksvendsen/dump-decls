{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module JsonNew
()
where

import Json
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
import qualified Data.Aeson.Types as A
import qualified Data.Text as T

fileRewriteDeclarationMap
  :: FilePath -- old
  -> FilePath -- new
  -> IO ()
fileRewriteDeclarationMap fileNameOld fileNameNew = do
  Right blah :: Either String [Json.DeclarationMapJson T.Text] <- A.eitherDecode <$> BSL.readFile fileNameOld
  let hey = map toJSONDeclarationMapJson blah
  BSL.writeFile fileNameNew (A.encode hey)

fileReadDeclarationMapNew
  :: FilePath
  -> IO (Either String [Json.DeclarationMapJson T.Text])
fileReadDeclarationMapNew fileName = do
  blah :: Either String [DeclarationMapJsonNew T.Text] <- A.eitherDecode <$> BSL.readFile fileName
  pure $ map unDeclarationMapJsonNew <$> blah

newtype DeclarationMapJsonNew value =
  DeclarationMapJsonNew { unDeclarationMapJsonNew :: Json.DeclarationMapJson T.Text}

instance A.ToJSON (DeclarationMapJsonNew value) where
  toJSON = toJSONDeclarationMapJson . unDeclarationMapJsonNew
  toEncoding = toEncodingDeclarationMapJson . unDeclarationMapJsonNew
instance A.FromJSON (DeclarationMapJsonNew value) where
  parseJSON = fmap DeclarationMapJsonNew . parseJSONDeclarationMapJson

-- FunctionType
toJSONFunctionType :: ToJSON b => FunctionType b -> A.Value
toJSONFunctionType (FunctionType a b) = A.toJSON (a, b)
toEncodingFunctionType :: ToJSON b => FunctionType b -> A.Encoding
toEncodingFunctionType (FunctionType a b) = A.toEncoding (a, b)
parseJSONFunctionType :: A.FromJSON a => A.Value -> A.Parser (FunctionType a)
parseJSONFunctionType = A.withArray "FunctionType" $ \vec -> do
  !mVal0 <- V.indexM vec 0
  !mVal1 <- V.indexM vec 1
  !val0 <- A.parseJSON mVal0
  !val1 <- A.parseJSON mVal1
  pure $! FunctionType val0 val1

-- TypeInfo
toJSONTypeInfo :: ToJSON tycon => TypeInfo tycon -> A.Value
toJSONTypeInfo (TypeInfo a b) = A.toJSON (a, b)
toEncodingTypeInfo :: ToJSON tycon => TypeInfo tycon -> A.Encoding
toEncodingTypeInfo (TypeInfo a b) = A.toEncoding (a, b)
parseJSONTypeInfo :: A.FromJSON a => A.Value -> A.Parser (TypeInfo a)
parseJSONTypeInfo = A.withArray "TypeInfo" $ \vec -> do
  !mVal0 <- V.indexM vec 0
  !mVal1 <- V.indexM vec 1
  !val0 <- A.parseJSON mVal0
  !val1 <- A.parseJSON mVal1
  pure $! TypeInfo val0 val1

-- ModuleDeclarations
toJSONModuleDeclarations :: (ToJSON value, A.ToJSONKey value) => ModuleDeclarations value -> A.Value
toJSONModuleDeclarations (ModuleDeclarations a b) = A.toJSON (a, b)
toEncodingModuleDeclarations :: (ToJSON value, A.ToJSONKey value) => ModuleDeclarations value -> A.Encoding
toEncodingModuleDeclarations (ModuleDeclarations a b) = A.toEncoding (a, b)
parseJSONModuleDeclarations :: (A.FromJSONKey a, Ord a, A.FromJSON a) => A.Value -> A.Parser (ModuleDeclarations a)
parseJSONModuleDeclarations = A.withArray "ModuleDeclarations" $ \vec -> do
  !mVal0 <- V.indexM vec 0
  !mVal1 <- V.indexM vec 1
  !val0 <- A.parseJSON mVal0
  !val1 <- A.parseJSON mVal1
  pure $! ModuleDeclarations val0 val1

-- DeclarationMapJson
toJSONDeclarationMapJson :: (ToJSON value, A.ToJSONKey value) => DeclarationMapJson value -> A.Value
toJSONDeclarationMapJson (DeclarationMapJson a b) = A.toJSON (a, b)
toEncodingDeclarationMapJson :: (ToJSON value, A.ToJSONKey value) => DeclarationMapJson value -> A.Encoding
toEncodingDeclarationMapJson (DeclarationMapJson a b) = A.toEncoding (a, b)
parseJSONDeclarationMapJson :: (A.FromJSONKey a, Ord a, A.FromJSON a) => A.Value -> A.Parser (DeclarationMapJson a)
parseJSONDeclarationMapJson = A.withArray "DeclarationMapJson" $ \vec -> do
  !mVal0 <- V.indexM vec 0
  !mVal1 <- V.indexM vec 1
  !val0 <- A.parseJSON mVal0
  !val1 <- A.parseJSON mVal1
  pure $! DeclarationMapJson val0 val1
