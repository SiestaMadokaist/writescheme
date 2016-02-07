module Ramadoka.Parser.LispVal2
(

) where
  import Text.ParserCombinators.Parsec hiding (spaces)
  import Data.List
  import System.Environment
  import Control.Monad as M
  import Data.String.Interpolate

  data Number = Float Float
    | Integer Integer
    | Rational {numerator :: Integer, denominator :: Integer}
    deriving (Show)

  data LispVal = Bool Bool
    | Number Number
    | Atom String
    deriving (Show)

  normalizeRational :: Integer -> Integer -> Number
  normalizeRational n d
    | normalDivisor == 1 = Integer normalDividend
    | otherwise = Rational normalDividend normalDivisor
    where normalizer = gcd n d
          normalDividend = n `div` normalizer
          normalDivisor = d `div` normalizer

  (|+|) :: Number -> Number -> Number
  Integer a |+| Integer b = Integer (a + b)
  Integer a |+| Rational n d = Rational a 1 |+| Rational n d
  Integer a |+| Float b = Float $ (fromIntegral a) + b
  r@(Rational _ _) |+| i@(Integer _) = i |+| r
  Rational n1 d1 |+| Rational n2 d2 = normalizeRational normalN normalD
    where leftSide = Rational (n1 * d2) normalD
          rightSide = Rational (n2 * d1) normalD
          normalN = (numerator leftSide) + (numerator rightSide)
          normalD = d1 * d2
  Rational n d |+| Float f = Float $ ((fromIntegral n) / (fromIntegral d)) + f
  f@(Float _) |+| i@(Integer _) = i |+| f
  f@(Float _) |+| r@(Rational _ _) = r |+| f
  Float a |+| Float b = Float (a + b)

  (|-|) :: Number -> Number -> Number
  a |-| b = a |+| (b |*| Integer (-1))

  (|*|) :: Number -> Number -> Number
  Integer a |*| Integer b = Integer (a * b)
  Integer a |*| Rational n d = normalizeRational (a * n) d
  Integer a |*| Float f = Float $ (fromIntegral a) * f
  r@(Rational _ _) |*| i@(Integer _) = i |*| r
  Rational n1 d1 |*| Rational n2 d2 = normalizeRational (n1 * n2) (d1 * d2)
  Rational n d |*| Float f = Float $ ((fromIntegral n) / (fromIntegral d)) * f
  f@(Float _) |*| i@(Integer _) = i |*| f
  f@(Float _) |*| r@(Rational _ _) = r |*| f
  Float a |*| Float b = Float (a * b)

  -- instance Eq LNumber where
    -- (==) (!f f1) (LNumber (Just f2) _ _) = f1 == f2

  --(|+|) :: LNumber -> LNumber -> LNumber
  --(|+|) (LNumber (Just f1) _ _) (LNumber (Just f2) _ _) = LNumber (Just (f1 + f2)) Nothing Nothing
  --(|+|) (FVal x) (FVal y) = FVal (x + y)
  --(|+|) (FVal x) (LNumber (Just y) _ _) = (FVal x) |+| (FVal y)
--
  --main :: IO()
  --main = putStrLn (show a)
    --where a = (LNumber (Just 5) Nothing Nothing) |+| (FVal 5)
