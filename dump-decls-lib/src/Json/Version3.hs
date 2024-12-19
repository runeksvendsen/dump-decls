{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Json.Version3
( Versioned.Versioned
, DeclarationMapJson
)
where

import Types
import Json
import Versioned
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Aeson.Types as A
import qualified Data.Vector

instance (A.ToJSON a) => A.ToJSON (FgTyCon a)
instance (A.FromJSON a) => A.FromJSON (FgTyCon a)

instance (A.ToJSON a) => A.ToJSON (FgPackage a)
instance (A.FromJSON a) => A.FromJSON (FgPackage a)

instance A.ToJSON TyConParseError
instance A.FromJSON TyConParseError

instance A.ToJSON value => A.ToJSON (FunctionType value)
instance A.FromJSON value => A.FromJSON (FunctionType value)

instance (A.ToJSON tycon) => A.ToJSON (TypeInfo tycon)
instance (A.FromJSON tycon) => A.FromJSON (TypeInfo tycon)

instance (A.ToJSON a, A.ToJSONKey a) => A.ToJSON (ModuleDeclarations a)
instance (A.FromJSON a, A.FromJSONKey a, Ord a) => A.FromJSON (ModuleDeclarations a)

instance A.ToJSON (DeclarationMapJson T.Text)
instance A.FromJSON (DeclarationMapJson T.Text)

instance A.ToJSON (Versioned.Versioned 3 (DeclarationMapJson T.Text)) where
  toJSON = versionedToJSON (A.genericToJSON A.defaultOptions)
instance A.FromJSON (Versioned.Versioned 3 (DeclarationMapJson T.Text)) where
  parseJSON = versionedParseJSON (A.genericParseJSON A.defaultOptions)

instance A.ToJSON (Versioned.Versioned 3 [DeclarationMapJson T.Text]) where
  toJSON =
    let toJSON' :: DeclarationMapJson T.Text -> A.Value
        toJSON' = A.genericToJSON A.defaultOptions
    in versionedToJSON (A.toJSON . map toJSON')

instance A.FromJSON (Versioned.Versioned 3 [DeclarationMapJson T.Text]) where
  parseJSON =
    let parseJSON' :: A.Value -> A.Parser (DeclarationMapJson T.Text)
        parseJSON' = A.genericParseJSON A.defaultOptions
    in versionedParseJSON $ A.withArray "[DeclarationMapJson Text]" $ \vectorOfValue ->
      traverse parseJSON' (Data.Vector.toList vectorOfValue)
