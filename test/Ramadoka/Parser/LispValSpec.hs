module Ramadoka.Parser.SchemerSpec where
  import Ramadoka.Parser.LispVal
  import Test.Hspec

  main :: IO()
  main = hspec spec

  -- method to easify testing
  -- how do I handle error here anyway?
  runParser :: String -> LispVal
  runParser input = unpackLispVal $ getExpr input

  unpackLispVal :: ThrowsError LispVal -> LispVal
  unpackLispVal (Right val) = val
  unpackLispVal (Left err) = Failure err

  runEval :: String -> LispVal
  runEval input = unpackLispVal $ eval $ runParser input

  int :: Integer -> LispVal
  int x = Number $ Rational x 1

  flt :: Float -> LispVal
  flt x = Number $ Float x

  ratio :: Integer -> Integer -> LispVal
  ratio a b = Number $ Rational a b

  spec :: Spec
  spec = do

    describe "evaluation" $ do
      describe "unOp" $ do
        describe "symbol?" $ do
          it "return false on quoted empty list" $ do
            runEval "(symbol? `())" `shouldBe` Bool False
          it "return true on quoted value" $ do
            runEval "(symbol? `hello)" `shouldBe` Bool True
          it "return true on quoted expression" $ do
            runEval "(symbol? `(+ 5 3))" `shouldBe` Bool False
          it "return false on expressionns" $ do
            runEval "(symbol? (+ 5 3))" `shouldBe` Bool False
          it "return true on quoted number" $ do
            runEval "(symbol? `5)" `shouldBe` Bool False
          it "return false on number" $ do
            runEval "(symbol? 5)" `shouldBe` Bool False

        describe "string?" $ do
          it "return false on number" $ do
            runEval "(string? 3)" `shouldBe` Bool False
          it "return true on string" $ do
            runEval "(string? \"hello\")" `shouldBe` Bool True
        describe "number?" $ do
          it "return false on atom" $ do
            runEval "(number? `hello)" `shouldBe` Bool False
          it "return true on number" $ do
            runEval "(number? 2/5)" `shouldBe` Bool True
          it "return true on expression that evaluate to number" $ do
            runEval "(number? (+ 2/5 3/5))" `shouldBe` Bool True

      describe "binOp" $ do
        describe "boolean operation" $ do
          describe "string>?" $ do
            it "correctly compare" $ do
              runEval "(string>? \"a\" \"b\")" `shouldBe` Bool False
              runEval "(string<? \"a\" \"b\")" `shouldBe` Bool True
          describe ">" $ do
            it "correctly compare rational and rational" $ do
              runEval "(> 5 3)" `shouldBe` Bool True
            it "correctly compare rational and float" $ do
              runEval "(> #e5.3 #i5.2)" `shouldBe` Bool True
            it "correctly compare float and float" $ do
              runEval "(> #i5.3 #i5.2)" `shouldBe` Bool True
            it "correctly compare float and rational" $ do
              runEval "(> #i5.3 #e5.4)" `shouldBe` Bool False
              runEval "(> #e5.2 #e5.2)" `shouldBe` Bool False
          describe "<" $ do
            it "correctly compare rational and rational" $ do
              runEval "(< #e5.2 #e5.001)" `shouldBe` Bool False
            it "correctly compare rational and float" $ do
              runEval "(< #e5.1 #i5.5)" `shouldBe` Bool True
            it "correctly compare float and rational" $ do
              runEval "(< #i5.0 #e5.1)"`shouldBe` Bool True
            it "correctly compare float and float" (pendingWith "trivial")

          describe "&&" $ do
            it "is correct(1)" $ do
              runEval "(&& #t #t)" `shouldBe` Bool True
            it "is correct(2)" $ do
              runEval "(&& #t #f)" `shouldBe` Bool False
            it "is correct(3)" $ do
              runEval "(&& #f #t)" `shouldBe` Bool False
            it "is correct(4)" $ do
              runEval "(&& #f #f)" `shouldBe` Bool False
            it" is correct on multiple argument" $ do
              runEval "(&& #f #f #t)" `shouldBe` Bool False

          describe "||" $ do
            it "is correct(1)" $ do
              runEval "(|| #t #t)" `shouldBe` Bool True
            it "is correct(2)" $ do
              runEval "(|| #t #f)" `shouldBe` Bool True
            it "is correct(3)" $ do
              runEval "(|| #f #t)" `shouldBe` Bool True
            it "is correct(4)" $ do
              runEval "(|| #f #f)" `shouldBe` Bool False
            it" is correct on multiple argument" $ do
              runEval "(|| #f #f #t)" `shouldBe` Bool True

        describe "addition" $ do
          it "works between 2 integer" $ do
            runEval "(+ 2 2)" `shouldBe` int 4
          it "works between 2 rational" $ do
            runEval "(+ 2/5 1/5)" `shouldBe` ratio 3 5
          it "works between integer and rational" $ do
            runEval "(+ 3/5 1 2/5)" `shouldBe` int 2
        describe "multiplication" $ do
          it "works between 2 integer" $ do
            runEval "(* 3 2)" `shouldBe` int 6
          it "works between 2 rational" $ do
            runEval "(* 1/5 3/5)" `shouldBe` ratio 3 25
        describe "multiple evaluation" $ do
          it "works on nested expression" $ do
            runEval "(* 2 (+ 3 5))" `shouldBe` int 16
          it "works for compettitive programming" $ do
            runEval "(* 1/2 2/3 4/5 6/7)" `shouldBe` ratio 8 35


      describe "normalizeRational" $ do
        it "does correctly on trivial integer-like rational" $ do
          normalizeRational 3 1 `shouldBe` Rational 3 1
        it "does trivial rational correctly" $ do
          normalizeRational 3 5 `shouldBe` Rational 3 5
        it "use gcd correctly" $ do
          normalizeRational 24 32 `shouldBe` Rational 3 4

    describe "getExpr" $ do
      describe "parseString" $ do
        it "parse normal string" $ do
          runParser "\"hello\"" `shouldBe` String "hello"
        it "parse escaped string" $ do
          runParser "\"hello\\\" world\"" `shouldBe` String "hello\" world"

      describe "parseNumber" $ do
        it "parse integer" $ do
          runParser "3" `shouldBe` (int 3)
        it "parse powered integer" $ do
          runParser "3e2" `shouldBe` (int 300)
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
          runParser "#e3" `shouldBe` int 3
        it "parse float" $ do
           runParser "#e3.2" `shouldBe` (Number $ Rational 16 5)
        it "parse powered integer" $ do
          runParser "#e3e5" `shouldBe` (int 300000)
        -- it "parse powered float" $ do
          -- runParser "#e3.12e1" `shouldBe` (Number $ Rational 156 5)

      describe "parseList" $ do
        it "parse variable of the same type" $ do
          runParser "(hello world)" `shouldBe` List [Atom "hello", Atom "world"]
        it "parse variable of different type" $ do
          runParser "(hello \"world\" 5)" `shouldBe` List [Atom "hello",  String "world", (int 5)]
        it "parse nested list" $ do
          runParser "(hello (world))" `shouldBe` List [Atom "hello", List [Atom "world"] ]
        it "parse empty nested list" $ do
          runParser "(hello ())" `shouldBe` List [Atom "hello", List []]
        it "parse quotedList" $ do
          runParser "(hello `(hello))" `shouldBe` List [Atom "hello", List [Atom "quote", List [Atom "hello"]]]

      describe "parseQuoted" $ do
        it "parse single valued" $ do
          runParser "`5" `shouldBe` List [Atom "quote", (int 5)]
        it "parse values inside list" $ do
          runParser "`(hello world)" `shouldBe` List [Atom "quote", List [Atom "hello", Atom "world"]]
          runParser "``hello" `shouldBe` List [Atom "quote", List [Atom "quote", Atom "hello"]]

      describe "parseDotted" $ do
        it "parse a simple list" $ do
          runParser "(5 . 3)" `shouldBe` DottedList [(int 5), (int 3)]
