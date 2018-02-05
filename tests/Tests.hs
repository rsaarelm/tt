module Main where

import           Data.Ratio
import           Data.Time
import           Test.Hspec
import           Text.Parsec
import           Tt.Token

main :: IO ()
main = hspec $ describe "Token parser" $ do
  return ()
  it "parses identifiers" $ parseToken "foo" `shouldBe` Sym "foo"
  it "parses date"
    $          parseToken "2012-12-21"
    `shouldBe` (Date $ fromGregorian 2012 12 21)
  it "parses time of day" $ parseToken "18:32:43+0200" `shouldBe` Time
    (TimeOfDay 18 32 43)
    (minutesToTimeZone 120)
  it "parses time of day without seconds" $ parseToken "18:32+0200" `shouldBe` Time
    (TimeOfDay 18 32 00)
    (minutesToTimeZone 120)
  it "parses time of day with negative tz"
    $          parseToken "18:32:43-0330"
    `shouldBe` Time (TimeOfDay 18 32 43) (minutesToTimeZone (-210))
  it "parses project identifiers" $ parseToken "+stuff" `shouldBe` Sym "+stuff"
  it "parses attributes with string values"
    $          parseToken "prio:C"
    `shouldBe` Colon (Sym "prio") (Sym "C")
  it "parses priority tags" $ parseToken "(A)" `shouldBe` Priority 'A'
  it "parses attributes with date values"
    $          parseToken "due:2018-03-20"
    `shouldBe` Colon (Sym "due") (Date $ fromGregorian 2018 3 20)
  it "parses integers" $ parseToken "123" `shouldBe` Number 123
  it "parses negative integers" $ parseToken "-321" `shouldBe` Number (-321)
  it "parses decimal parts" $ parseToken "1.01" `shouldBe` Number (101 % 100)
  it "parses negatives and decimal parts" $ parseToken "-8.76" `shouldBe` Number
    (-876 % 100)
  it "allows a plus sign in from of numbers"
    $          parseToken "+123"
    `shouldBe` Number 123
  it "parses random text" $ parseToken "123stuff!" `shouldBe` Text "123stuff!"
  it "parses random text from longer string"
    $          parseToken "123stuff! And things!"
    `shouldBe` Text "123stuff!"
  it "does not parse punctuation junk" $ parseToken "(()" `shouldBe` Text "(()"
  it "does not parse degenerate colon definitions"
    $          parseToken ":"
    `shouldBe` Text ":"
  it "does not parse too long colon chains" $ parseToken "a:b:c" `shouldBe` Text
    "a:b:c"
  it "does not parse invalid colon keys" $ parseToken "10:foo" `shouldBe` Text
    "10:foo"
  it "does not parse invalid colon values" $ parseToken "a:12b" `shouldBe` Text
    "a:12b"

parseToken :: String -> Token
parseToken t = unwrap $ parse tokenParser "" t
 where
  unwrap (Right x) = x
  unwrap (Left  _) = error "Parsing failed"
