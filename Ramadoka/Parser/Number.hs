module Ramadoka.Parser.Number
(
  Number(..),
  normalizeRational,
  (|+|),
  (|/|),
  (|-|),
  (|*|)
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

  instance Eq Number where
    Float f1 == Float f2              = f1 == f2
    Integer i1 == Integer i2          = i1 == i2
    Rational n1 d1 == Rational n2 d2  = nn1 == nn2 && dd1 == dd2
      where normal1 = normalizeRational n1 d1
            normal2 = normalizeRational n2 d2
            nn1 = numerator normal1
            nn2 = numerator normal2
            dd1 = denominator normal1
            dd2 = denominator normal2
    r@(Rational _ _) == Integer i = r == (Rational i 1)

  normalizeRational :: Integer -> Integer -> Number
  normalizeRational n d
    | normalDivisor == 1 = Integer normalDividend
    | otherwise = Rational normalDividend normalDivisor
    where normalizer = gcd n d
          normalDividend = n `div` normalizer
          normalDivisor = d `div` normalizer

  (|+|) :: Number -> Number -> Number
  Integer a |+| Integer b             = Integer (a + b)
  Integer a |+| Rational n d          = Rational a 1 |+| Rational n d
  Integer a |+| Float b               = Float $ (fromIntegral a) + b
  r@(Rational _ _) |+| i@(Integer _)  = i |+| r
  Rational n1 d1 |+| Rational n2 d2   = normalizeRational normalN normalD
    where leftSide = Rational (n1 * d2) normalD
          rightSide = Rational (n2 * d1) normalD
          normalN = (numerator leftSide) + (numerator rightSide)
          normalD = d1 * d2
  Rational n d |+| Float f            = Float $ ((fromIntegral n) / (fromIntegral d)) + f
  f@(Float _) |+| i@(Integer _)       = i |+| f
  f@(Float _) |+| r@(Rational _ _)    = r |+| f
  Float a |+| Float b                 = Float (a + b)

  (|-|) :: Number -> Number -> Number
  a |-| b = a |+| (b |*| Integer (-1))

  (|*|) :: Number -> Number -> Number
  Integer a |*| Integer b             = Integer (a * b)
  Integer a |*| Rational n d          = normalizeRational (a * n) d
  Integer a |*| Float f               = Float $ (fromIntegral a) * f
  r@(Rational _ _) |*| i@(Integer _)  = i |*| r
  Rational n1 d1 |*| Rational n2 d2   = normalizeRational (n1 * n2) (d1 * d2)
  Rational n d |*| Float f            = Float $ ((fromIntegral n) / (fromIntegral d)) * f
  f@(Float _) |*| i@(Integer _)       = i |*| f
  f@(Float _) |*| r@(Rational _ _)    = r |*| f
  Float a |*| Float b                 = Float (a * b)

  (|/|) :: Number -> Number -> Number
  Integer a |/| Integer b             = Rational a b
  Integer i |/| Float f               = Float $ (fromIntegral i) / f
  Rational n d |/| Float f            = Float $ numerator / denominator
    where fN = fromIntegral n
          fD = fromIntegral d
          numerator = fN / fD
          denominator = f
  Float f |/| Integer i = Float $ f / (fromIntegral i)
  Float f1 |/| Float f2 = Float $ f1 / f2
  anyNumber |/| Rational n d = anyNumber |*| Rational d n
