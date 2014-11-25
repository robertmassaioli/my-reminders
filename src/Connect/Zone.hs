module Connect.Zone
   ( Zone(..)
   ) where

data Zone = Dev | Dog | Prod
   deriving(Eq, Show, Ord)