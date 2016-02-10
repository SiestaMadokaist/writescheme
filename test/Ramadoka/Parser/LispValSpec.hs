module Ramadoka.Parser.SchemerSpec where
  import Ramadoka.Parser.LispVal
  import Test.Hspec

  main :: IO()
  main = hspec spec

  -- method to easify testing
  -- how do I handle error here anyway?
  runParser :: String -> LispVal
  runParser input = case getExpr input of
                     (Right val) -> val
                     (Left err) -> Failure (show err)

  runEval :: String -> LispVal
  runEval input = eval $ runParser input

  spec :: Spec
  spec = do
    describe "normalizeRational" $ do
      it "does correctly on trivial integer-like rational" $ do
        normalizeRational 3 1 `shouldBe` Integer 3
      it "does it correctly" $ do
        normalizeRational 3 5 `shouldBe` Rational 3 5

    describe "getExpr" $ do
      describe "parseString" $ do
        it "parse normal string" $ do
          runParser "\"hello\"" `shouldBe` String "hello"
        it "parse escaped string" $ do
          runParser "\"hello\\\" world\"" `shouldBe` String "hello\" world"

      describe "parseNumber" $ do
        it "parse integer" $ do
          runParser "3" `shouldBe` Number (Integer 3)
        it "parse powered integer" $ do
          runParser "3e2" `shouldBe` Number (Integer 300)
        it "parse float" $ do
          runParser "3.2" `shouldBe` Number (Float 3.2)
        it "parse powered float" $ do
          runParser "3.2e1" `shouldBe` Number (Float 32.0)

      describe "parseInexactNumber" $ do
        it "parse integer-like" $ do
          runParser "#i3" `shouldBe` (Number $ Float 3.0)
        it "parse powered-integer-like" $ do
          runParser "#i3e1" `shouldBe` (Number $ Float 30.0)
        it "parse floating point" $ do
          runParser "#i3.2" `shouldBe` (Number $ Float 3.2)
        it "parse powered-floating-point" $ do
          runParser "#i3.2e1" `shouldBe` (Number $ Float 32.0)

      describe "parseExactNumber" $ do
        it "parse integer" $ do
          runParser "#e3" `shouldBe` (Number $ Integer 3)
        it "parse float" $ do
           runParser "#e3.2" `shouldBe` (Number $ Rational 16 5)
        it "parse powered integer" $ do
          runParser "#e3e5" `shouldBe` (Number $ Integer 300000)
        -- it "parse powered float" $ do
          -- runParser "#e3.12e1" `shouldBe` (Number $ Rational 156 5)

      describe "parseList" $ do
        it "parse variable of the same type" $ do
          runParser "(hello world)" `shouldBe` List [Atom "hello", Atom "world"]
        it "parse variable of different type" $ do
          runParser "(hello \"world\" 5)" `shouldBe` List [Atom "hello",  String "world", Number (Integer 5)]
        it "parse nested list" $ do
          runParser "(hello (world))" `shouldBe` List [Atom "hello", List [Atom "world"] ]
        it "parse empty nested list" $ do
          runParser "(hello ())" `shouldBe` List [Atom "hello", List []]
        it "parse quotedList" $ do
          runParser "(hello `(hello))" `shouldBe` List [Atom "hello", List [Atom "quote", List [Atom "hello"]]]

      describe "parseQuoted" $ do
        it "parse single valued" $ do
          runParser "`5" `shouldBe` List [Atom "quote", Number (Integer 5)]
        it "parse values inside list" $ do
          runParser "`(hello world)" `shouldBe` List [Atom "quote", List [Atom "hello", Atom "world"]]
          runParser "``hello" `shouldBe` List [Atom "quote", List [Atom "quote", Atom "hello"]]

      describe "parseDotted" $ do
        it "parse a simple list" $ do
          runParser "(5 . 3)" `shouldBe` DottedList [Number (Integer 5), Number (Integer 3)]
