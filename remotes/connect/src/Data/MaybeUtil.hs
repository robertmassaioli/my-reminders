module Data.MaybeUtil where

import Data.Either.Combinators (maybeToRight)

m2e :: e -> Maybe a -> Either e a
m2e = maybeToRight
