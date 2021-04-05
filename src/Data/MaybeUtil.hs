module Data.MaybeUtil where

m2e :: e -> Maybe a -> Either e a
m2e e = maybe (Left e) Right