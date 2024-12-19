{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Json.Version4
(
)
where

import Types
import Json
import Versioned
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.Text as T
import qualified Data.Aeson.Types as A
import qualified Data.Vector

instance (A.ToJSON a) => A.ToJSON (FgTyCon a) where
  toJSON = $(A.mkToJSON A.defaultOptions ''FgTyCon)
  toEncoding = $(A.mkToEncoding A.defaultOptions ''FgTyCon)
instance (A.FromJSON a) => A.FromJSON (FgTyCon a) where
  parseJSON = $(A.mkParseJSON A.defaultOptions ''FgTyCon)

instance (A.ToJSON a) => A.ToJSON (FgPackage a) where
  toJSON = $(A.mkToJSON A.defaultOptions ''FgPackage)
  toEncoding = $(A.mkToEncoding A.defaultOptions ''FgPackage)
instance (A.FromJSON a) => A.FromJSON (FgPackage a) where
  parseJSON = $(A.mkParseJSON A.defaultOptions ''FgPackage)

instance A.ToJSON TyConParseError where
  toJSON = $(A.mkToJSON A.defaultOptions ''TyConParseError)
  toEncoding = $(A.mkToEncoding A.defaultOptions ''TyConParseError)
instance A.FromJSON TyConParseError where
  parseJSON = $(A.mkParseJSON A.defaultOptions ''TyConParseError)

instance A.ToJSON value => A.ToJSON (FunctionType value) where
  toJSON = $(A.mkToJSON A.defaultOptions ''FunctionType)
  toEncoding = $(A.mkToEncoding A.defaultOptions ''FunctionType)
instance A.FromJSON value => A.FromJSON (FunctionType value) where
  parseJSON = $(A.mkParseJSON A.defaultOptions ''FunctionType)
instance (A.ToJSON tycon) => A.ToJSON (TypeInfo tycon) where
  toJSON = $(A.mkToJSON A.defaultOptions ''TypeInfo)
  toEncoding = $(A.mkToEncoding A.defaultOptions ''TypeInfo)
instance (A.FromJSON tycon) => A.FromJSON (TypeInfo tycon) where
  parseJSON = $(A.mkParseJSON A.defaultOptions ''TypeInfo)
instance (A.ToJSON a, A.ToJSONKey a) => A.ToJSON (ModuleDeclarations a) where
  toJSON = $(A.mkToJSON A.defaultOptions ''ModuleDeclarations)
  toEncoding = $(A.mkToEncoding A.defaultOptions ''ModuleDeclarations)
instance (A.FromJSON a, A.FromJSONKey a, Ord a) => A.FromJSON (ModuleDeclarations a) where
  parseJSON = $(A.mkParseJSON A.defaultOptions ''ModuleDeclarations)

instance A.ToJSON (Versioned.Versioned 4 [DeclarationMapJson T.Text]) where
  toJSON =
    let toJSON' :: DeclarationMapJson T.Text -> A.Value
        toJSON' = $(A.mkToJSON A.defaultOptions ''DeclarationMapJson)
    in versionedToJSON (A.toJSON . map toJSON')

instance A.FromJSON (Versioned.Versioned 4 [DeclarationMapJson T.Text]) where
  parseJSON =
    let parseJSON' :: A.Value -> A.Parser (DeclarationMapJson T.Text)
        parseJSON' = $(A.mkParseJSON A.defaultOptions ''DeclarationMapJson)
    in versionedParseJSON $ A.withArray "[DeclarationMapJson Text]" $ \vectorOfValue ->
      traverse parseJSON' (Data.Vector.toList vectorOfValue)
