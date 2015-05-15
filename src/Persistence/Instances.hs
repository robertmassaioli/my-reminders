{-# LANGUAGE TypeSynonymInstances #-}
module Persistence.Instances where

import           Control.Applicative                  (pure, (<$>), (<*>))
import qualified Data.ByteString.Char8                as BSC
import           Data.Maybe                           (fromMaybe)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Network.URI
import qualified Snap.AtlassianConnect                as AC

instance FromRow AC.ClientKey where
   fromRow = field

instance FromRow AC.Tenant where
    fromRow = AC.Tenant <$> field <*> field <*> field <*> field <*> (AC.CURI <$> field) <*> field

instance FromField URI where
    fromField _ (Just bstr) = pure $ fromMaybe nullURI $ parseURI (BSC.unpack bstr)
    fromField f _           = returnError ConversionFailed f "data is not a valid URI value"

instance ToField URI where
    toField = Escape . BSC.pack . show
