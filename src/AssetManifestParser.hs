module AssetManifestParser 
    ( AssetManifest
    , parseAssetManifest
    ) where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Aeson (decode')
import qualified Data.ByteString.Lazy as B

type AssetManifest = M.Map T.Text T.Text

parseAssetManifest :: FilePath -> IO AssetManifest
parseAssetManifest fp = do
    contents <- B.readFile fp
    return . maybe M.empty id $ decode' contents