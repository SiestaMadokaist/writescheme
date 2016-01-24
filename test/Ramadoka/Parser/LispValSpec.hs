module Ramadoka.Parser.SchemerSpec where
  import Ramadoka.Parser.LispVal
  import Test.Hspec

  main :: IO()
  main = hspec spec

  -- method to easify testing
  -- how do I handle error here anyway?
  exprTest :: String -> LispVal
  exprTest input = case getExpr input of
                     (Right val) -> val

  spec :: Spec
  spec = do
    describe "normalizeRational" $ do
      it "is correct" $ do
        normalizeRational 12 10 `shouldBe` LRational 6 5
      it "is correct" $ do
        normalizeRational 1200 10 `shouldBe` LRational 120 1

    describe "getExpr" $ do

      describe "parseNumber" $ do
        it "works on integer" $ do
          exprTest "3" `shouldBe` LInteger 3
        it "works on powered integer" $ do
          exprTest "3e2" `shouldBe` LInteger 300
        it "works on float" $ do
          exprTest "3.2" `shouldBe` LFloat 3.2
        it "works on powered float" $ do
          exprTest "3.2e1" `shouldBe` LFloat 32.0

      describe "parseInexactNumber" $ do
        it "works on integer-like" $ do
          exprTest "#i3" `shouldBe` (LFloat 3.0)
        it "works on powered-integer-like" $ do
          exprTest "#i3e1" `shouldBe` (LFloat 30.0)
        it "works on floating point" $ do
          exprTest "#i3.2" `shouldBe` (LFloat 3.2)
        it "works on powered-floating-point" $ do
          exprTest "#i3.2e1" `shouldBe` (LFloat 32.0)

      describe "parseExactNumber" $ do
        it "works on integer" $ do
          exprTest "#e3" `shouldBe` (LInteger 3)
        it "works on float" $ do
          exprTest "#e3.2" `shouldBe` (LRational 16 5)
        it "works on powered integer" $ do
          exprTest "#e3e5" `shouldBe` (LInteger 300000)
        it "works on powered float" $ do
          exprTest "#e3.12e1" `shouldBe` (LRational 156 5)
