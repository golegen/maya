module Helpers
  ( (!!<)
  , (!!?)
  ) where


import Data.Maybe (fromMaybe)
import Data.List (elemIndex)


(!!<) :: [a] -> Int -> a
list !!< index =
  let
    size = length list
    i = index `mod` size
    posInd = if i < 0 then size - i else i
  in
    list !! posInd


(!!?) :: (Eq a) => [a] -> a -> Int
list !!? el =
  fromMaybe 0 $ el `elemIndex` list
