module Main where

import Tt
import Test.Hspec
import qualified Data.Time as Time

main :: IO ()
main = hspec
  $ describe "Tt.parseToken" $ do
-- FIXME: Currently invalid because Token with ZonedTime cannot derive Eq,
-- Maybe fix it by turning it into (localTime, tzOffsetMinutes) or something?
    return ()
--    it "parses identifiers" $
--      parseToken "foo" `shouldBe` Sym "foo"
--    it "parses random test" $
--      parseToken "123stuff!" `shouldBe` Text "123stuff!"
--    it "parses date" $
--      parseToken "2012-12-21" `shouldBe` (Date $ Time.fromGregorian 2012 12 21)
--    it "parses time of day" $
--      parseToken "18:32:43+0200" `shouldBe` (Time $ Time.ZonedTime (Time.LocalTime (Time.ModifiedJulianDay 0) (Time.TimeOfDay 18 32 43)) (Time.TimeZone 120 False "0200"))
--    it "parses project identifiers" $
--      parseToken "+stuff" `shouldBe` Project "stuff"
--    it "parses attributes with string values" $
--      parseToken "prio:C" `shouldBe` Colon (Sym "prio") (Sym "C")
--    it "parses attributes with date values" $
--      parseToken "due:2018-03-20" `shouldBe` Colon (Sym "due") (Date $ Time.fromGregorian 2018 3 20)
