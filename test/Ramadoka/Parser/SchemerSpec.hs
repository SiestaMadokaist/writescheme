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
    describe "showVal" $ do
      it "print DottedList correctly" $ do
        let dottedList = RS.showVal . exprTest $ "(Hello . 5 . X)"
        dottedList `shouldBe` "(Atom Hello, Integer 5 . Atom X)"

    describe "getExpr" $ do
      it "read Atom" $ do
        exprTest "test" `shouldBe` (RS.Atom "test")

      it "read String" $ do
        exprTest "\"hello world\"" `shouldBe` (RS.String "hello world")

      it "read List" $ do
        exprTest "(Hello Atom F)" `shouldBe` (RS.List [RS.Atom "Hello", RS.Atom "Atom", RS.Atom "F"])

      it "read Quoted List" $ do
        exprTest "`(Hello + World)" `shouldBe` (RS.List [RS.Atom "quote", RS.List [RS.Atom "Hello", RS.Atom "+", RS.Atom "World"]])

      it "read Quoted expression" $ do
        exprTest "`Hello" `shouldBe` (RS.List [RS.Atom "quote", RS.Atom "Hello"])

      it "read Quoted DottedList" $ do
        exprTest "`(Hello . Atom)" `shouldBe` (RS.List [RS.Atom "quote", (RS.DottedList [RS.Atom "Hello"] (RS.Atom "Atom"))])
      it "read DottedList" $ do
        exprTest "(\"hello\" . 2)" `shouldBe` (RS.DottedList [RS.String "hello"] (RS.LispNumber $ RS.Integer 2))

      it "read Symbol" $ do
        exprTest "$" `shouldBe` RS.Atom "$"

      it "read Float" $ do
        exprTest "2.3" `shouldBe` (RS.LispNumber $ RS.Float 2.3)

      it "readExponential" $ do
        -- need to be simplified ?
        exprTest "#e2.30e1" `shouldBe` (RS.LispNumber $ RS.Rational 2300 100)

      it "read InexactNumber" $ do
        exprTest "#i2.33" `shouldBe` (RS.LispNumber $ RS.Float 2.33)

      it "read Rational" $ do
        exprTest "#e0.3" `shouldBe` (RS.LispNumber $ RS.Rational 3 10)

      it "read Integer" $ do
        exprTest "1" `shouldBe` (RS.LispNumber $ RS.Integer 1)
