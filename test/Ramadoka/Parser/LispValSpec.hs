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
        it "works on multiple values" $ do
          runEval "(+ 5 3 2)" `shouldBe` LInteger 10
        it "works on addition between integer and integer" $ do
          runEval "(+ 5 3)" `shouldBe` LInteger 8
        it "works on addition between integer and rational" $ do
          runEval "(+ 5 #e3.2)" `shouldBe` LRational 41 5
        it "works on addition between integer and float" $ do
          let queryString = "(+ 5 3.2)"
          runEval queryString `shouldSatisfy` (`fGT` LFloat 8.1)
          runEval queryString `shouldNotSatisfy` (`fGT` LFloat 8.3)
        it "works on addition between rational and rational" $ do
          runEval "(+ #e3.2 #e3.2)" `shouldBe` runEval "#e6.4"
        it "automatically convert to integer if its exact value is integer" $ do
          runEval "(+ #e1.5 #e1.5)" `shouldBe` LInteger 3
        it "works on addition between rational and float" $ do
          let result = runEval "(+ #e2.6 #i1.3)"
          -- specifically chosen floating point of which its addition doesn't equal
          -- to its exact value
          result `shouldNotBe` runEval "#i3.9"
          result `shouldSatisfy` (`fGT` LFloat 3.89)
          result `shouldSatisfy` (`fLT` LFloat 3.91)
        it "works on addition between float and float" $ do
          let result = runEval "(+ #i2.6 #i1.3)"
          result `shouldNotBe` runEval "#i3.9"
          result `shouldSatisfy` (`fGT` LFloat 3.89)
          result `shouldSatisfy` (`fLT` LFloat 3.91)

      describe "substraction" $ do
        it "works on a single values" $ do
          runEval "(- 5)" `shouldBe` LInteger 5
        it "works on multiple values" $ do
          runEval "(- 5 3 1)" `shouldBe` LInteger 1
        it "works on substraction between integer and integer" $ do
          runEval "(- 5 3)" `shouldBe` LInteger 2
        it "works on substraction between integer and rational" $ do
          runEval "(- 5 #e3.2)" `shouldBe` runEval "#e1.8"
          runEval "(- 5 #e1.2 #e1.2)" `shouldBe` runEval "#e2.6"

      describe "multiplication" $ do
        it "works on simple example" $ do
          runEval "(* 5 3)" `shouldBe` LInteger 15
        it "works on multiple values" $ do
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
        it "works on integer" $ do
          runParser "3" `shouldBe` LInteger 3
        it "works on powered integer" $ do
          runParser "3e2" `shouldBe` LInteger 300
        it "works on float" $ do
          runParser "3.2" `shouldBe` LFloat 3.2
        it "works on powered float" $ do
          runParser "3.2e1" `shouldBe` LFloat 32.0

      describe "parseInexactNumber" $ do
        it "works on integer-like" $ do
          runParser "#i3" `shouldBe` (LFloat 3.0)
        it "works on powered-integer-like" $ do
          runParser "#i3e1" `shouldBe` (LFloat 30.0)
        it "works on floating point" $ do
          runParser "#i3.2" `shouldBe` (LFloat 3.2)
        it "works on powered-floating-point" $ do
          runParser "#i3.2e1" `shouldBe` (LFloat 32.0)

      describe "parseExactNumber" $ do
        it "works on integer" $ do
          runParser "#e3" `shouldBe` (LInteger 3)
        it "works on float" $ do
          runParser "#e3.2" `shouldBe` (LRational 16 5)
        it "works on powered integer" $ do
          runParser "#e3e5" `shouldBe` (LInteger 300000)
        it "works on powered float" $ do
          runParser "#e3.12e1" `shouldBe` (LRational 156 5)

      describe "parseList" $ do
        it "works on variable of the same type" $ do
          runParser "(hello world)" `shouldBe` LList [LAtom "hello", LAtom "world"]
        it "works on variable of different type" $ do
          runParser "(hello \"world\" 5)" `shouldBe` LList [LAtom "hello",  LString "world", LInteger 5]
        it "works on nested list" $ do
          runParser "(hello (world))" `shouldBe` LList [LAtom "hello", LList [LAtom "world"] ]
        it "works on empty nested list" $ do
          runParser "(hello ())" `shouldBe` LList [LAtom "hello", LList []]
        it "works on quotedList" $ do
          runParser "(hello `(hello))" `shouldBe` LList [LAtom "hello", LList [LAtom "quote", LList [LAtom "hello"]]]

      describe "parseQuoted" $ do
        it "works on single valued" $ do
          runParser "`5" `shouldBe` LList [LAtom "quote", LInteger 5]
        it "works on values inside list" $ do
          runParser "`(hello world)" `shouldBe` LList [LAtom "quote", LList [LAtom "hello", LAtom "world"]]
          runParser "``hello" `shouldBe` LList [LAtom "quote", LList [LAtom "quote", LAtom "hello"]]

      describe "parseDotted" $ do
        it "works on a simple list" $ do
          runParser "(5 . 3)" `shouldBe` LDottedList [LInteger 5, LInteger 3]
