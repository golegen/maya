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


isBeforeNullDay :: Int -> Int -> Bool
isBeforeNullDay m d
  | m == 2 = d <= 28
  | otherwise = m < 2


isBissextile :: Integer -> Bool
isBissextile y = y `mod` 4 == 0


pastNullDays :: Integer -> Integer
pastNullDays y =
  floor . (/4) . fromIntegral $ y - yearZero


discountNullDays :: Integer -> Int -> Int -> Integer
discountNullDays y m d =
  let
    adjust = if isBissextile y && isBeforeNullDay m d then - 1 else 0
  in
    pastNullDays y + adjust


daysFromZeroNoNullDays :: Integer -> Int -> Int -> Integer
daysFromZeroNoNullDays y m d =
  daysFromZero y m d - discountNullDays y m d


kinByDate :: Integer -> Int -> Int -> Kin
kinByDate y m d =
  let
    isNullDay = m == 2 && d > 28
    (m', d') = if isNullDay then (3, 1) else (m, d)
  in
    findKin . (+kinZero) . fromIntegral $ daysFromZeroNoNullDays y m' d'


harmonicByDates :: [(Integer, Int, Int)] -> Kin
harmonicByDates list =
  findKin . sum . map kinIndex $ map (\(y, m, d) -> kinByDate y m d) list