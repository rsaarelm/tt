module Main where

import Tt
import Test.Hspec
import qualified Data.Time as Time

main :: IO ()
main = hspec $
  describe "Tt.parseToken" $ do
    it "parses identifiers" $
      parseToken "foo" `shouldBe` Identifier "foo"
    it "parses random test" $
      parseToken "123stuff!" `shouldBe` Text "123stuff!"
    it "parses date" $
      parseToken "2012-12-21" `shouldBe` (Date $ Time.fromGregorian 2012 12 21)
    it "parses time of day without seconds" $
      parseToken "18:32" `shouldBe` (Time $ Time.TimeOfDay 18 32 00)
    it "parses time of day with seconds" $
      parseToken "18:32:43" `shouldBe` (Time $ Time.TimeOfDay 18 32 43)
    it "parses project identifiers" $
      parseToken "+stuff" `shouldBe` Project "stuff"
    it "parses attributes with string values" $
      parseToken "prio:C" `shouldBe` Colon (Identifier "prio") (Identifier "C")
    it "parses attributes with date values" $
      parseToken "due:2018-03-20" `shouldBe` Colon (Identifier "due") (Date $ Time.fromGregorian 2018 3 20)
