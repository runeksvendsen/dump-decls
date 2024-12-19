{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Main
(main)
where

import Json.Version3
import Json.Version4
import Criterion.Main
import Data.Functor (void, (<&>))
import Control.Monad ((<=<))
import qualified Data.List.NonEmpty as NE
import qualified Control.Exception as Ex

import qualified Json
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import qualified Data.FileEmbed

-- TODO: include in this package

dataFileNameV3 :: FilePath
dataFileNameV3 =
  "/Users/rune/code/haskell-function-graph/data/all-v3.json"

dataFileNameV4 :: FilePath
dataFileNameV4 =
  "/Users/rune/code/haskell-function-graph/data/all-v4.json"

fileReadDeclarationMapReadFile
  :: A.FromJSON (Versioned version [DeclarationMapJson T.Text]) => FilePath
  -> IO (Versioned version [Json.DeclarationMapJson T.Text])
fileReadDeclarationMapReadFile fileName =
  BSL.readFile fileName >>=
    either
      fail
      pure
    . A.eitherDecode

fileReadDeclarationMapDecodeFileStrict
  :: A.FromJSON (Versioned version [DeclarationMapJson T.Text])
  => FilePath
  -> IO (Versioned version [Json.DeclarationMapJson T.Text])
fileReadDeclarationMapDecodeFileStrict fileName =
  A.eitherDecodeFileStrict fileName >>=
    either
      fail
      pure

main :: IO ()
main = do
  defaultMain
    [ bgroup "Decode DeclarationMapJson"
      [ bgroup "eitherDecode . readFile"
          [ bench "v3" $ nfIO (fileReadDeclarationMapReadFile dataFileNameV3 :: IO (Versioned 3 [Json.DeclarationMapJson T.Text]))
          , bench "v4" $ nfIO (fileReadDeclarationMapReadFile dataFileNameV4 :: IO (Versioned 4 [Json.DeclarationMapJson T.Text]))
          ]
      , bgroup "decodeFileStrict"
          [ bench "v3" $ nfIO (fileReadDeclarationMapDecodeFileStrict dataFileNameV3 :: IO (Versioned 3 [Json.DeclarationMapJson T.Text]))
          , bench "v4" $ nfIO (fileReadDeclarationMapDecodeFileStrict dataFileNameV4 :: IO (Versioned 4 [Json.DeclarationMapJson T.Text]))
          ]
      ]
    ]
