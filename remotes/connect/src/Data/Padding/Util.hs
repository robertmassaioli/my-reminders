module Data.Padding.Util
   ( requiredPadding
   ) where

requiredPadding :: Int -> Int -> Int
requiredPadding length' boundary =
   case length' `mod` boundary of
      0 -> 0
      x -> boundary - x

