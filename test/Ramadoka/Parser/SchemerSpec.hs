module Ramadoka.Parser.SchemerSpec where
  import qualified Ramadoka.Parser.Schemer as RS
  import Test.Hspec

  main :: IO()
  main = hspec spec

  -- method to easify testing
  -- how do I handle error here anyway?
  exprTest :: String -> RS.LispVal
  exprTest input = case RS.getExpr input of
                     (Right val) -> val

  spec :: Spec
  spec = do
    describe "getExpr" $ do
      it "read Atom" $ do
        exprTest "test" `shouldBe` (RS.Atom "test")

      it "read String" $ do
        exprTest "\"hello world\"" `shouldBe` (RS.String "hello world")

      it "read List" $ do
        exprTest "(Hello Atom F)" `shouldBe` (RS.List [RS.Atom "Hello", RS.Atom "Atom", RS.Atom "F"])

      it "read Quoted" $ do
        exprTest "`(Hello + World)" `shouldBe` (RS.Quoted $ RS.List [RS.Atom "Hello", RS.Atom "+", RS.Atom "World"])
