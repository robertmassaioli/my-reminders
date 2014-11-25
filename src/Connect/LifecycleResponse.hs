{-# LANGUAGE DeriveGeneric #-}
module Connect.LifecycleResponse
   ( LifecycleResponse(..)
   , ClientKey
   ) where

import           AesonHelpers
import           Connect.Instances
import           Data.Aeson.Types
import qualified Data.Text         as T
import           GHC.Generics

type ClientKey = T.Text

data LifecycleResponse = LifecycleResponseInstalled {
    lrKey            :: T.Text
  , lrClientKey      :: ClientKey
  , lrPublicKey      :: T.Text
  , lrSharedSecret   :: Maybe T.Text
  , lrServerVersion  :: Maybe T.Text
  , lrPluginsVersion :: Maybe T.Text
  , lrBaseUrl        :: ConnectURI
  , lrProductType    :: Maybe T.Text
  , lrDescription    :: Maybe T.Text
  , lrEventType      :: Maybe T.Text
} deriving (Eq, Show, Generic)

instance FromJSON LifecycleResponse where
    parseJSON = genericParseJSON defaultOptions
      { omitNothingFields = True
      , fieldLabelModifier = stripFieldNamePrefix "lr"
      }