module Connect.Instances
    ( ConnectURI(..)
    , getURI
    ) where

import           Control.Monad
import           Data.Aeson.Types
import qualified Data.Text        as T
import           Network.URI

newtype ConnectURI = CURI URI
    deriving(Eq, Show)

getURI :: ConnectURI -> URI
getURI (CURI u) = u

instance FromJSON ConnectURI where
   parseJSON (String uriString) = maybe mzero return (fmap CURI . parseURI . T.unpack $ uriString)
   parseJSON _ = mzero