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


lowers :: String -> String
lowers = filter (`elem` ['a'..'z'])


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let
    smallerEqual = quicksort [a | a <- xs, a <= x]
    bigger = quicksort $ filter (>x) xs
  in smallerEqual ++ [x] ++ bigger


removeWhite :: String -> String
removeWhite string = [a | a <- string, a `notElem` [' ']]

sortString :: String -> String
sortString string = removeWhite $ quicksort string


largestDivisible :: (Integral a) => a -> a
largestDivisible n = head (filter p [100000,99999..])
    where p x = x `mod` n == 0


maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 max


appendIfValid :: (a -> Bool) -> a -> [a] -> [a]
appendIfValid f x xs = if f x then x : xs else xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (appendIfValid p) []


sqrtSumsUntil :: Float -> Int
sqrtSumsUntil n = length (takeWhile (<n) $ scanl1 (+) $ map sqrt [1..]) + 1


applyFunctionsToNumber :: Float -> [Float]
applyFunctionsToNumber n = map ($ n) [(2*), (10*), (^2), sqrt]