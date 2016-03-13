module Maya where

import Kins
import Data.Time

yearZero = 2012

zero :: Day
zero = fromGregorian yearZero 12 21

kinZero = 207 :: Int

daysFromZero :: Integer -> Int -> Int -> Integer
daysFromZero y m d =
  (flip diffDays) zero $ fromGregorian y m d


isPastNullDay :: Int -> Int -> Bool
isPastNullDay m d
  | m == 2 = d > 28
  | otherwise = m > 2


isBissextile :: Integer -> Bool
isBissextile y = y `mod` 4 == 0


isBeforeZero :: Integer -> Int -> Int -> Bool
isBeforeZero y m d = (daysFromZero y m d) < 0


isBeforeFirstNull :: Integer -> Int -> Int -> Bool
isBeforeFirstNull y m d =
  (< 0) $ (flip diffDays) (fromGregorian 2012 2 29) (fromGregorian y m d)


bissextiles :: Integral c => Integer -> c
bissextiles y =
  floor . (/4) . fromIntegral $ y - yearZero


adjustBissextiles :: Integral a => Integer -> Int -> Int -> a
adjustBissextiles y m d =
  let
    adjust = if isBissextile y && not (isPastNullDay m d) then - 1 else 0
  in
    bissextiles y + adjust


daysFromZeroNoBissextile :: Integer -> Int -> Int -> Integer
daysFromZeroNoBissextile y m d =
  daysFromZero y m d + negate (adjustBissextiles y m d)


kinByDate :: Integer -> Int -> Int -> Kin
kinByDate y m d =
  let
    isNullDay = m == 2 && d > 28
    m' = if isNullDay then 3 else m
    d' = if isNullDay then 1 else d
  in
    findKin . (+kinZero) . fromIntegral $ (daysFromZeroNoBissextile y m' d') `mod` 260