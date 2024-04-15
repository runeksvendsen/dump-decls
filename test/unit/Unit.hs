{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Types
import Test.Hspec

main :: IO ()
main = hspec $
  describe "Types" $ do
    describe "parsePprTyCon" $ do
      it "Either" $
        parsePprTyCon "base-4.18.0.0:Data.Either.Either"
          `shouldBe` Right (FgTyCon {fgTyConName = "Either", fgTyConModule = "Data.Either", fgTyConPackage = FgPackage {fgPackageName = "base", fgPackageVersion = "4.18.0.0"}})

      it "Text" $
        parsePprTyCon "text-2.0.2:Data.Text.Internal.Text"
          `shouldBe` Right (FgTyCon {fgTyConName = "Text", fgTyConModule = "Data.Text.Internal", fgTyConPackage = FgPackage {fgPackageName = "text", fgPackageVersion = "2.0.2"}})

      it "Maybe" $
        parsePprTyCon "base-4.18.0.0:GHC.Maybe.Maybe"
          `shouldBe` Right (FgTyCon {fgTyConName = "Maybe", fgTyConModule = "GHC.Maybe", fgTyConPackage = FgPackage {fgPackageName = "base", fgPackageVersion = "4.18.0.0"}})

    describe "splitByEndNonEmpty" $
      it "ByteString" $
        splitByEndNonEmpty "oops" '.' "Data.ByteString.Lazy.Internal.ByteString"
          `shouldBe` Right ("Data.ByteString.Lazy.Internal","ByteString")

    describe "parsePackageWithVersion" $ do
      it "base" $
        parsePackageWithVersion "base-4.18.0.0"
          `shouldBe` Right (FgPackage {fgPackageName = "base", fgPackageVersion = "4.18.0.0"})

      it "text" $
        parsePackageWithVersion "text-2.0.2"
          `shouldBe` Right (FgPackage {fgPackageName = "text", fgPackageVersion = "2.0.2"})
