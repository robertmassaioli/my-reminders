module AesonHelpers 
   ( baseOptions
   , stripFieldNamePrefix
   , dropAndSnakeCase
   ) where

import           Data.Aeson.Types
import qualified Data.Char as C
import qualified Data.List as L
import           Data.Maybe (fromMaybe)

baseOptions :: Options
baseOptions = defaultOptions
   { omitNothingFields = True
   }

stripFieldNamePrefix :: String -> String -> String
stripFieldNamePrefix pre = lowerFirst . try (L.stripPrefix pre)

dropAndSnakeCase :: String -> String -> String
dropAndSnakeCase toDrop = camelToSnakeCase . dropCondition toDrop . lowerFirst

lowerFirst :: String -> String
lowerFirst (x : xs) = C.toLower x : xs
lowerFirst [] = []

camelToSnakeCase :: String -> String
camelToSnakeCase input = case L.break C.isUpper input of
   (first, []) -> first
   (first, x : xs) -> first ++ ('_' : C.toLower x : camelToSnakeCase xs)

dropCondition :: String -> String -> String
dropCondition toDrop = L.reverse . try (L.stripPrefix . L.reverse $ toDrop) . L.reverse

try :: (a -> Maybe a) -> a -> a
try f v = fromMaybe v (f v)

