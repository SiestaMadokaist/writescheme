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
                     (Left err) -> LFailure (show err)

  runEval :: String -> LispVal
  runEval input = eval $ runParser input

  spec :: Spec
  spec = do
    describe "eval" $ do
      let fGT (LFloat f1) (LFloat f2) = f1 > f2
      let fLT (LFloat f1) (LFloat f2) = f1 < f2
      describe "addition" $ do
        it "parse multiple values" $ do
          runEval "(+ 5 3 2)" `shouldBe` LInteger 10
        it "parse addition between integer and integer" $ do
          runEval "(+ 5 3)" `shouldBe` LInteger 8
        it "parse addition between integer and rational" $ do
          runEval "(+ 5 #e3.2)" `shouldBe` LRational 41 5
        it "parse addition between integer and float" $ do
          let queryString = "(+ 5 3.2)"
          runEval queryString `shouldSatisfy` (`fGT` LFloat 8.1)
          runEval queryString `shouldNotSatisfy` (`fGT` LFloat 8.3)
        it "parse addition between rational and rational" $ do
          runEval "(+ #e3.2 #e3.2)" `shouldBe` runEval "#e6.4"
        it "automatically convert to integer if its exact value is integer" $ do
          runEval "(+ #e1.5 #e1.5)" `shouldBe` LInteger 3
        it "parse addition between rational and float" $ do
          let result = runEval "(+ #e2.6 #i1.3)"
          -- specifically chosen floating point of which its addition doesn't equal
          -- to its exact value
          result `shouldNotBe` runEval "#i3.9"
          result `shouldSatisfy` (`fGT` LFloat 3.89)
          result `shouldSatisfy` (`fLT` LFloat 3.91)
        it "parse addition between float and float" $ do
          let result = runEval "(+ #i2.6 #i1.3)"
          result `shouldNotBe` runEval "#i3.9"
          result `shouldSatisfy` (`fGT` LFloat 3.89)
          result `shouldSatisfy` (`fLT` LFloat 3.91)

      describe "substraction" $ do
        it "parse a single values" $ do
          runEval "(- 5)" `shouldBe` LInteger 5
        it "parse multiple values" $ do
          runEval "(- 5 3 1)" `shouldBe` LInteger 1
        it "parse substraction between integer and integer" $ do
          runEval "(- 5 3)" `shouldBe` LInteger 2
        it "parse substraction between integer and rational" $ do
          runEval "(- 5 #e3.2)" `shouldBe` runEval "#e1.8"
          runEval "(- 5 #e1.2 #e1.2)" `shouldBe` runEval "#e2.6"
        it "parse substraction between rational and integer" $ do
          runEval "(- #e6.4 5)" `shouldBe` runEval "#e1.4"

      describe "multiplication" $ do
        it "parse simple example" $ do
          runEval "(* 5 3)" `shouldBe` LInteger 15
        it "parse multiple values" $ do
          runEval "(* 5 3 2)" `shouldBe` LInteger 30
        it "works between integer and rational" $ do
          runEval "(* 2 #e1.5)" `shouldBe` LInteger 3
        it "works between integer and float" $ do
          let result = runEval "(* 2 #i1.5)"
          result `shouldSatisfy` (`fGT` LFloat 2.99)
          result `shouldSatisfy` (`fLT` LFloat 3.01)
        it "works between rational and rational" $ do
          runEval "(* #e0.5 #e6.4)" `shouldBe` runEval "#e3.2"
        it "works between rational and float" $ do
          let result = runEval "(* #e0.5 #i2)"
          result `shouldSatisfy` (`fGT` LFloat 0.99)
          result `shouldSatisfy` (`fLT` LFloat 1.01)
        it "works between float and float" $ do
          let result = runEval "(* #i0.5 #i2)"
          result `shouldSatisfy` (`fGT` LFloat 0.99)
          result `shouldSatisfy` (`fLT` LFloat 1.01)

      describe "division" $ do
        it "works between integer and integer" $ do
          runEval "(/ 5 2)" `shouldBe` LRational 5 2
        it "works between integer and rational that result in integer" $ do
          runEval "(/ 2 #e0.5)" `shouldBe` LInteger 4
        it "works between integer and rational that result in rational" $ do
          runEval "(/ 2 #e0.7)" `shouldBe` LRational 20 7
        it "works between integer and float" $ do
          let result = runEval "(/ 2 #i0.7)"
          result `shouldSatisfy` (`fGT` LFloat 2.85)
          result `shouldSatisfy` (`fLT` LFloat 2.86)
        it "works between rational and rational" $ do
          runEval "(/ #e0.5 #e0.25)" `shouldBe` LInteger 2
        it "works between rational and float" $ do
          let result = runEval "(/ #e1.3 #i0.3)"
          result `shouldSatisfy` (`fGT` LFloat 4.33)
          result `shouldSatisfy` (`fLT` LFloat 4.34)
        it "works between float and rational" $ do
          let result = runEval "(/ #i1.3 #e0.3)"
          result `shouldSatisfy` (`fGT` LFloat 4.33)
          result `shouldSatisfy` (`fLT` LFloat 4.34)

      describe "nested expression" $ do
        it "works on nested expression (2 level)" $ do
          runEval "(+ 5 (/ 5 2))" `shouldBe` LRational 15 2
          runEval "(+ 5 (- 5 3 1) 2)" `shouldBe` LInteger 8
        it "works on nested expression (3 level)" $ do
          runEval "(+ 5 (- (/ 5 2) 2) 2)" `shouldBe` LRational 15 2

    describe "normalizeRational" $ do
      it "is correct" $ do
        normalizeRational 12 10 `shouldBe` LRational 6 5
      it "is correct" $ do
        normalizeRational 1200 10 `shouldBe` LInteger 120

    describe "getExpr" $ do
      describe "parseString" $ do
        it "parse normal string" $ do
          runParser "\"hello\"" `shouldBe` LString "hello"
        it "parse escaped string" $ do
          runParser "\"hello\\\" world\"" `shouldBe` LString "hello\" world"

      describe "parseNumber" $ do
        it "parse integer" $ do
          runParser "3" `shouldBe` LInteger 3
        it "parse powered integer" $ do
          runParser "3e2" `shouldBe` LInteger 300
        it "parse float" $ do
          runParser "3.2" `shouldBe` LFloat 3.2
        it "parse powered float" $ do
          runParser "3.2e1" `shouldBe` LFloat 32.0

      describe "parseInexactNumber" $ do
        it "parse integer-like" $ do
          runParser "#i3" `shouldBe` (LFloat 3.0)
        it "parse powered-integer-like" $ do
          runParser "#i3e1" `shouldBe` (LFloat 30.0)
        it "parse floating point" $ do
          runParser "#i3.2" `shouldBe` (LFloat 3.2)
        it "parse powered-floating-point" $ do
          runParser "#i3.2e1" `shouldBe` (LFloat 32.0)

      describe "parseExactNumber" $ do
        it "parse integer" $ do
          runParser "#e3" `shouldBe` (LInteger 3)
        it "parse float" $ do
          runParser "#e3.2" `shouldBe` (LRational 16 5)
        it "parse powered integer" $ do
          runParser "#e3e5" `shouldBe` (LInteger 300000)
        it "parse powered float" $ do
          runParser "#e3.12e1" `shouldBe` (LRational 156 5)

      describe "parseList" $ do
        it "parse variable of the same type" $ do
          runParser "(hello world)" `shouldBe` LList [LAtom "hello", LAtom "world"]
        it "parse variable of different type" $ do
          runParser "(hello \"world\" 5)" `shouldBe` LList [LAtom "hello",  LString "world", LInteger 5]
        it "parse nested list" $ do
          runParser "(hello (world))" `shouldBe` LList [LAtom "hello", LList [LAtom "world"] ]
        it "parse empty nested list" $ do
          runParser "(hello ())" `shouldBe` LList [LAtom "hello", LList []]
        it "parse quotedList" $ do
          runParser "(hello `(hello))" `shouldBe` LList [LAtom "hello", LList [LAtom "quote", LList [LAtom "hello"]]]

      describe "parseQuoted" $ do
        it "parse single valued" $ do
          runParser "`5" `shouldBe` LList [LAtom "quote", LInteger 5]
        it "parse values inside list" $ do
          runParser "`(hello world)" `shouldBe` LList [LAtom "quote", LList [LAtom "hello", LAtom "world"]]
          runParser "``hello" `shouldBe` LList [LAtom "quote", LList [LAtom "quote", LAtom "hello"]]

      describe "parseDotted" $ do
        it "parse a simple list" $ do
          runParser "(5 . 3)" `shouldBe` LDottedList [LInteger 5, LInteger 3]
