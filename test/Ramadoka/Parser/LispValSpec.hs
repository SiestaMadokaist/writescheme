module Ramadoka.Parser.LispValSpec where
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

      describe "car" $

        it "return the first element" $
          runEval "(car `(5 3 2))" `shouldBe` ratio 5 1

      describe "if" $ do

        it "run correctly on #t" $
          runEval "(if (> 5 3) `a `b)" `shouldBe` Atom "a"

        it "run correctly on #f" $
          runEval "(if (> 5 9) 3 2)" `shouldBe` ratio 2 1

      describe "unOp" $ do

        describe "symbol?" $ do

          it "return false on quoted empty list" $
            runEval "(symbol? `())" `shouldBe` Bool False

          it "return true on quoted value" $
            runEval "(symbol? `hello)" `shouldBe` Bool True

          it "return true on quoted expression" $
            runEval "(symbol? `(+ 5 3))" `shouldBe` Bool False

          it "return false on expressionns" $
            runEval "(symbol? (+ 5 3))" `shouldBe` Bool False

          it "return true on quoted number" $
            runEval "(symbol? `5)" `shouldBe` Bool False

          it "return false on number" $
            runEval "(symbol? 5)" `shouldBe` Bool False

        describe "string?" $ do
          it "return false on number" $
            runEval "(string? 3)" `shouldBe` Bool False

          it "return true on string" $
            runEval "(string? \"hello\")" `shouldBe` Bool True

        describe "number?" $ do
          it "return false on atom" $
            runEval "(number? `hello)" `shouldBe` Bool False

          it "return true on number" $
            runEval "(number? 2/5)" `shouldBe` Bool True

          it "return true on expression that evaluate to number" $
            runEval "(number? (+ 2/5 3/5))" `shouldBe` Bool True

      describe "binOp" $ do

        describe "boolean operation" $ do

          describe "string>?" $

            it "correctly compare" $ do
              runEval "(string>? \"a\" \"b\")" `shouldBe` Bool False
              runEval "(string<? \"a\" \"b\")" `shouldBe` Bool True

          describe ">" $ do

            it "correctly compare rational and rational" $
              runEval "(> 5 3)" `shouldBe` Bool True

            it "correctly compare rational and float" $
              runEval "(> #e5.3 #i5.2)" `shouldBe` Bool True

            it "correctly compare float and float" $
              runEval "(> #i5.3 #i5.2)" `shouldBe` Bool True

            it "correctly compare float and rational" $ do
              runEval "(> #i5.3 #e5.4)" `shouldBe` Bool False
              runEval "(> #e5.2 #e5.2)" `shouldBe` Bool False

          describe "<" $ do
            it "correctly compare rational and rational" $
              runEval "(< #e5.2 #e5.001)" `shouldBe` Bool False
            it "correctly compare rational and float" $
              runEval "(< #e5.1 #i5.5)" `shouldBe` Bool True
            it "correctly compare float and rational" $
              runEval "(< #i5.0 #e5.1)"`shouldBe` Bool True
            it "correctly compare float and float" (pendingWith "trivial")

          describe "&&" $ do
            it "is correct(1)" $
              runEval "(&& #t #t)" `shouldBe` Bool True
            it "is correct(2)" $
              runEval "(&& #t #f)" `shouldBe` Bool False
            it "is correct(3)" $
              runEval "(&& #f #t)" `shouldBe` Bool False
            it "is correct(4)" $
              runEval "(&& #f #f)" `shouldBe` Bool False
            it" is correct on multiple argument" $
              runEval "(&& #f #f #t)" `shouldBe` Bool False

          describe "||" $ do
            it "is correct(1)" $
              runEval "(|| #t #t)" `shouldBe` Bool True
            it "is correct(2)" $
              runEval "(|| #t #f)" `shouldBe` Bool True
            it "is correct(3)" $
              runEval "(|| #f #t)" `shouldBe` Bool True
            it "is correct(4)" $
              runEval "(|| #f #f)" `shouldBe` Bool False
            it" is correct on multiple argument" $
              runEval "(|| #f #f #t)" `shouldBe` Bool True

        describe "addition" $ do
          it "works between 2 integer" $
            runEval "(+ 2 2)" `shouldBe` int 4

          it "works between 2 rational" $
            runEval "(+ 2/5 1/5)" `shouldBe` ratio 3 5

          it "works between integer and rational" $
            runEval "(+ 3/5 1 2/5)" `shouldBe` int 2

        describe "multiplication" $ do

          it "works between 2 integer" $
            runEval "(* 3 2)" `shouldBe` int 6

          it "works between 2 rational" $
            runEval "(* 1/5 3/5)" `shouldBe` ratio 3 25

        describe "multiple evaluation" $ do
          it "works on nested expression" $
            runEval "(* 2 (+ 3 5))" `shouldBe` int 16

          it "works for compettitive programming" $
            runEval "(* 1/2 2/3 4/5 6/7)" `shouldBe` ratio 8 35

      describe "normalizeRational" $ do

        it "does correctly on trivial integer-like rational" $
          normalizeRational 3 1 `shouldBe` Rational 3 1

        it "does trivial rational correctly" $
          normalizeRational 3 5 `shouldBe` Rational 3 5

        it "use gcd correctly" $
          normalizeRational 24 32 `shouldBe` Rational 3 4

    describe "getExpr" $ do

      describe "parseString" $ do

        it "parse normal string" $
          runParser "\"hello\"" `shouldBe` String "hello"

        it "parse escaped string" $
          runParser "\"hello\\\" world\"" `shouldBe` String "hello\" world"

      describe "parseNumber" $ do

        it "parse integer" $
          runParser "3" `shouldBe` int 3

        it "parse powered integer" $
          runParser "3e2" `shouldBe` int 300

        it "parse float" $
          runParser "3.2" `shouldBe` flt 3.2

        it "parse powered float" $
          runParser "3.2e1" `shouldBe` flt 32.0

      describe "parseInexactNumber" $ do

        it "parse integer-like" $
          runParser "#i3" `shouldBe` flt 3.0

        it "parse powered-integer-like" $
          runParser "#i3e1" `shouldBe` flt 30.0

        it "parse floating point" $
          runParser "#i3.2" `shouldBe` flt 3.2

        it "parse powered-floating-point" $
          runParser "#i3.2e1" `shouldBe` flt 32.0

      describe "parseExactNumber" $ do

        it "parse integer" $
          runParser "#e3" `shouldBe` int 3

        it "parse float" $
           runParser "#e3.2" `shouldBe` ratio 16 5

        it "parse powered integer" $
          runParser "#e3e5" `shouldBe` int 300000

        it "parse powered float" $
          runParser "#e3.12e1" `shouldBe` ratio 156 5

      describe "parseList" $ do

        it "parse variable of the same type" $
          runParser "(hello world)" `shouldBe` List [Atom "hello", Atom "world"]

        it "parse variable of different type" $
          runParser "(hello \"world\" 5)" `shouldBe` List [Atom "hello",  String "world", int 5]

        it "parse nested list" $
          runParser "(hello (world))" `shouldBe` List [Atom "hello", List [Atom "world"] ]
        it "parse empty nested list" $
          runParser "(hello ())" `shouldBe` List [Atom "hello", List []]

        it "parse quotedList" $
          runParser "(hello `(hello))" `shouldBe` List [Atom "hello", List [Atom "quote", List [Atom "hello"]]]

      describe "parseQuoted" $ do

        it "parse single valued" $
          runParser "`5" `shouldBe` List [Atom "quote", int 5]

        it "parse values inside list" $ do
          runParser "`(hello world)" `shouldBe` List [Atom "quote", List [Atom "hello", Atom "world"]]
          runParser "``hello" `shouldBe` List [Atom "quote", List [Atom "quote", Atom "hello"]]

      describe "parseDotted" $

        it "parse a simple list" $
          runParser "(dotted ~ list)" `shouldBe` DottedList [Atom "dotted"] (Atom "list")
