{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Json.Version5
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
import qualified Data.Map as Map

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

instance A.ToJSON value => A.ToJSON (ModuleDeclarations value) where
  toJSON md = A.object
    [ ("map", A.toJSON $ Map.toAscList $ Map.toAscList <$> moduleDeclarations_map md)
    , ("map_fail", A.toJSON $ map (fmap Map.toAscList) $ Map.toAscList (moduleDeclarations_mapFail md))
    ]

instance (A.FromJSON value, Eq value) => A.FromJSON (ModuleDeclarations value) where
  parseJSON = A.withObject "ModuleDeclarations" $ \o -> do
    !map' <- o A..: "map"
    !mapFail<- o A..: "map_fail"
    pure $! ModuleDeclarations
      (Map.fromAscList $ map (fmap Map.fromAscList) map')
      (Map.fromAscList $ map (fmap Map.fromAscList) mapFail)

instance A.ToJSON (Versioned.Versioned 5 [DeclarationMapJson T.Text]) where
  toJSON =
    let toJSON' :: DeclarationMapJson T.Text -> A.Value
        toJSON' = $(A.mkToJSON A.defaultOptions ''DeclarationMapJson)
    in versionedToJSON (A.toJSON . map toJSON')

instance A.FromJSON (Versioned.Versioned 5 [DeclarationMapJson T.Text]) where
  parseJSON =
    let parseJSON' :: A.Value -> A.Parser (DeclarationMapJson T.Text)
        parseJSON' = $(A.mkParseJSON A.defaultOptions ''DeclarationMapJson)
    in versionedParseJSON $ A.withArray "[DeclarationMapJson Text]" $ \vectorOfValue ->
      traverse parseJSON' (Data.Vector.toList vectorOfValue)
