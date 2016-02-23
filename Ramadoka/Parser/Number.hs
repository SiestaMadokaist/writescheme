module Ramadoka.Parser.Number
(
  Number(..),
  normalizeRational,
  (|+|),
  (|/|),
  (|-|),
  (|*|),
) where
  import Text.ParserCombinators.Parsec hiding (spaces)
  import Data.List
  import System.Environment
  import Control.Monad as M
  import Data.String.Interpolate

  -- TODO:
  -- refactor all Integer to Rational
  data Number = Float Float
    | Rational {numerator :: Integer, denominator :: Integer}

  instance Show Number where
    show (Float f) = show f
    show (Rational x 1) = show x
    show (Rational x y) = show x ++ "/" ++ show y

  instance Eq Number where
    Float f1 == Float f2              = f1 == f2
    Rational n1 d1 == Rational n2 d2  = nn1 == nn2 && dd1 == dd2
      where normal1 = normalizeRational n1 d1
            normal2 = normalizeRational n2 d2
            nn1 = numerator normal1
            nn2 = numerator normal2
            dd1 = denominator normal1
            dd2 = denominator normal2

  normalizeRational :: Integer -> Integer -> Number
  normalizeRational n d = Rational normalDividend normalDivisor
    where normalizer = gcd n d
          normalDividend = n `div` normalizer
          normalDivisor = d `div` normalizer

  (|+|) :: Number -> Number -> Number
  Rational n1 d1 |+| Rational n2 d2   = normalizeRational normalN normalD
    where leftSide = Rational (n1 * d2) normalD
          rightSide = Rational (n2 * d1) normalD
          normalN = (numerator leftSide) + (numerator rightSide)
          normalD = d1 * d2
  Rational n d |+| Float f            = Float $ ((fromIntegral n) / (fromIntegral d)) + f
  f@(Float _) |+| r@(Rational _ _)    = r |+| f
  Float a |+| Float b                 = Float (a + b)

  (|-|) :: Number -> Number -> Number
  a |-| b = a |+| (b |*| Rational (-1) 1)

  (|*|) :: Number -> Number -> Number
  Rational n1 d1 |*| Rational n2 d2   = normalizeRational (n1 * n2) (d1 * d2)
  Rational n d |*| Float f            = Float $ ((fromIntegral n) / (fromIntegral d)) * f
  f@(Float _) |*| r@(Rational _ _)    = r |*| f
  Float a |*| Float b                 = Float (a * b)

  (|/|) :: Number -> Number -> Number
  Rational n d |/| Float f            = Float $ numerator / denominator
    where fN = fromIntegral n
          fD = fromIntegral d
          numerator = fN / fD
          denominator = f
  Float f1 |/| Float f2 = Float $ f1 / f2
  r@(Rational _ _) |/| Rational n d = r |*| Rational d n
