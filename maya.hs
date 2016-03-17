module Maya where

import Kins
import Data.Time (Day, diffDays, fromGregorian)

type Date = (Integer, Int, Int)

kinZero = 207 :: Int
yearZero = 2012 :: Integer
zero = fromGregorian yearZero 12 21 :: Day


daysFromZero :: Date -> Integer
daysFromZero (y, m, d) =
  diffDays (fromGregorian y m d) zero


shouldApplyBissestile :: Date -> Bool
shouldApplyBissestile (y, m, d) =
  isBissestile && isBeforeNullDay
  where
    isBissestile = y `rem` 4 == 0
    isBeforeNullDay
      | m == 2 = d <= 28
      | otherwise = m < 2


pastNullDays :: Date -> Integer
pastNullDays (y, _, _) =
  div (y - yearZero) 4


discountNullDays :: Date -> Integer
discountNullDays date =
  pastNullDays date + adjust
  where
    adjust
      | shouldApplyBissestile date = -1
      | otherwise = 0


daysFromZeroNoNullDays :: Date -> Integer
daysFromZeroNoNullDays date =
  daysFromZero date - discountNullDays date


kinIndexByDate :: Date -> Int
kinIndexByDate (y, m, d) =
  (+) kinZero . fromIntegral $ daysFromZeroNoNullDays (y, m', d')
  where
    (m', d')
      | m == 2 && d > 28 = (3, 1)
      | otherwise = (m, d)


kinByDate :: Date -> Kin
kinByDate date =
  findKin $ kinIndexByDate date


harmonicByDates :: [Date] -> Kin
harmonicByDates list =
  findKin . sum $ map kinIndexByDate list
