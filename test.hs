succ' :: (Enum t) => t -> t
succ' x = [x..] !! 1

takeOdds :: (Enum t, Num t) => Int -> [t]
takeOdds 0 = []
takeOdds n =
  let x = n - 1
  in takeOdds x ++ [([1,3..] !! x)]


takeEvens :: (Enum a, Num a) => Int -> [a]
takeEvens n = [succ' x | x <- takeOdds n]

capitals :: String -> String
capitals string = [x | x <- string, x `elem` ['A'..'Z']]


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let
    smallerEqual = quicksort [a | a <- xs, a <= x]
    bigger = quicksort [b | b <- xs, b > x]
  in smallerEqual ++ [x] ++ bigger


removeWhite :: String -> String
removeWhite string = [a | a <- string, not (a `elem` [' '])]



sortString :: String -> String
sortString string = removeWhite (quicksort string)

