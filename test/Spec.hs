{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Exe
import qualified Json
import qualified Types
import System.Environment (withArgs)
import qualified System.IO.Silently
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Text as T
import qualified Test.Hspec
import Test.Hspec.Expectations.Pretty (shouldNotBe, shouldBe)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

main :: IO ()
main = do
  (stdout, ()) <- System.IO.Silently.capture $
    withArgs ["base", "text"] Exe.main
  let stdoutBs = Data.ByteString.Lazy.Char8.pack stdout
  declarationMapJson :: [Json.DeclarationMapJson T.Text] <-
      either
        (\e -> fail $ "JSON parse failure: " <> e)
        pure
        (A.eitherDecode stdoutBs)
  Test.Hspec.hspec $ spec declarationMapJson

spec :: [Json.DeclarationMapJson T.Text] -> Test.Hspec.Spec
spec declarationMapJson =
  Test.Hspec.describe "Expected TypeInfo" $
    Test.Hspec.it "System.IO.putStrLn" $ do
      let mBaseDeclarationMapJson = lookupOn ((== "base-4.18.0.0") . Json.declarationMapJson_package) declarationMapJson
          baseDeclarationMapJson = fromJust mBaseDeclarationMapJson
      mBaseDeclarationMapJson `shouldNotBe` Nothing
      let modDecls = Json.declarationMapJson_moduleDeclarations baseDeclarationMapJson
          map' = Json.moduleDeclarations_map modDecls
          mDefnMap = Map.lookup "System.IO" map'
          mTypeInfo = mDefnMap >>= Map.lookup "putStrLn"
          typeInfo = fromJust mTypeInfo
      mTypeInfo `shouldNotBe` Nothing
      typeInfo `shouldBe` tiPutStrLn
  where
    tyConIO = Types.FgTyCon
      { Types.fgTyConName = "IO"
      , Types.fgTyConModule = "GHC.Types"
      , Types.fgTyConPackageName = "ghc-prim"
      , Types.fgTyConPackageVersion = "0.10.0"
      }
    tyConString = Types.FgTyCon
      { Types.fgTyConName = "String"
      , Types.fgTyConModule = "GHC.Base"
      , Types.fgTyConPackageName = "base"
      , Types.fgTyConPackageVersion = "4.18.0.0"
      }
    tyConChar = Types.FgTyCon
      { Types.fgTyConName = "Char"
      , Types.fgTyConModule = "GHC.Types"
      , Types.fgTyConPackageName = "ghc-prim"
      , Types.fgTyConPackageVersion = "0.10.0"
      }
    tyConAppIOUnit = Types.FgType_TyConApp tyConIO [Types.FgType_Unit] -- IO ()
    tiPutStrLn =
      Json.TypeInfo
        { Json.typeInfo_fullyQualified = Json.FunctionType
            { Json.functionType_arg = Types.FgType_List $ Types.FgType_TyConApp tyConChar [] -- [Char]
            , Json.functionType_ret = tyConAppIOUnit
            }
        , Json.typeInfo_tmpUnexpanded = Json.FunctionType
            { Json.functionType_arg = Types.FgType_TyConApp tyConString [] -- String
            , Json.functionType_ret = tyConAppIOUnit
            }
        }

lookupOn :: (a -> Bool) -> [a] -> Maybe a
lookupOn _ [] =  Nothing
lookupOn f  (x:xs)
    | f x = Just x
    | otherwise = lookupOn f xs
