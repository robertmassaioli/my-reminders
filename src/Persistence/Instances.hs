{-# LANGUAGE TypeSynonymInstances #-}
module Persistence.Instances where

import qualified Connect.Instances                    as CI
import qualified Connect.LifecycleResponse            as CL
import qualified Connect.Tenant                       as CT
import           Control.Applicative
import qualified Data.ByteString.Char8                as BSC
import           Data.Maybe                           (fromMaybe)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Network.URI

instance FromRow CL.ClientKey where
   fromRow = field

instance FromRow CT.Tenant where
    fromRow = CT.Tenant <$> field <*> field <*> field <*> field <*> (CI.CURI <$> field) <*> field

instance FromField URI where
    fromField _ (Just bstr) = pure $ fromMaybe nullURI $ parseURI (BSC.unpack bstr)
    fromField f _           = returnError ConversionFailed f "data is not a valid URI value"

instance ToField URI where
    toField = Escape . BSC.pack . show
