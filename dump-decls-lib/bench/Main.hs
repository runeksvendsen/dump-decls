{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main
(main)
where

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

dataFileName :: FilePath
dataFileName =
  "/Users/rune/code/haskell-function-graph/data/all3.json"

blah :: FilePath
blah = $(Data.FileEmbed.makeRelativeToProject "css/chota.css")

fileReadDeclarationMapReadFile
  :: FilePath
  -> IO [Json.DeclarationMapJson T.Text]
fileReadDeclarationMapReadFile fileName =
  BSL.readFile fileName >>=
    either
      fail
      pure
    . A.eitherDecode

fileReadDeclarationMapDecodeFileStrict
  :: FilePath
  -> IO [Json.DeclarationMapJson T.Text]
fileReadDeclarationMapDecodeFileStrict fileName =
  A.eitherDecodeFileStrict fileName >>=
    either
      fail
      pure

main :: IO ()
main = do
  defaultMain
    [ bgroup "Decode DeclarationMapJson"
      [ bench "eitherDecode <$> BSL.readFile" $ nfIO (fileReadDeclarationMapReadFile dataFileName)
      , bench "decodeFileStrict" $ nfIO (fileReadDeclarationMapDecodeFileStrict dataFileName)
      ]
    ]
  where

