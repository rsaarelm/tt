module Main where

import Tt
import Test.Hspec
import qualified Data.Time as Time

main :: IO ()
main = hspec $ do
  describe "Tt.parseToken" $ do
    it "parses regular words" $ do
      parseToken "foo" `shouldBe` Word "foo"
    it "parses date" $ do
      parseToken "2012-12-21" `shouldBe` (Date $ Time.fromGregorian 2012 12 21)
    it "parses time of day without seconds" $ do
      parseToken "18:32" `shouldBe` (Time $ Time.TimeOfDay 18 32 00)
    it "parses time of day with seconds" $ do
      parseToken "18:32:43" `shouldBe` (Time $ Time.TimeOfDay 18 32 43)
    it "parses project identifiers" $ do
      parseToken "+stuff" `shouldBe` (Project "stuff")
    it "parses attributes with string values" $ do
      parseToken "prio:C" `shouldBe` (Attr "prio" $ Word "C")
    it "parses attributes with date values" $ do
      parseToken "due:2018-03-20" `shouldBe` (Attr "due" $ Date $ Time.fromGregorian 2018 3 20)
