{-# LANGUAGE FlexibleContexts #-}
module Connect.Routes
  ( connectRoutes
  , homeHandler
  , getLifecycleResponse -- TODO move to the LifecycleResponse package if that does not introduce more dependencies
  ) where

import qualified AtlassianConnect             as AC
import qualified Connect.Data                 as CD
import           Connect.Descriptor           (Name (..))
import qualified Connect.LifecycleResponse    as CL
import           Control.Applicative
import qualified Control.Arrow                as ARO
import           Control.Monad                (mzero)
import           Control.Monad.State.Class    (get)
import qualified Data.Aeson                   as A
import qualified Data.ByteString.Char8        as BC
import qualified Data.CaseInsensitive         as CI
import           Data.List
import           Data.List.Split              (splitOn)
import qualified Data.Map.Lazy                as ML
import           Data.Maybe                   (catMaybes, fromMaybe)
import qualified Network.HTTP.Media.MediaType as NM
import qualified Snap.Core                    as SC
import qualified Snap.Snaplet                 as SS
import qualified SnapHelpers                  as SH

data MediaType = ApplicationJson | TextHtml deriving (Eq)

newtype OrdMediaType = OMT NM.MediaType deriving (Eq, Show)

instance Ord OrdMediaType where
   compare a b = compare (show a) (show b)

instance Show (MediaType) where
  show ApplicationJson = "application/json"
  show TextHtml = "text/html"

homeHandler :: SS.Handler b CD.Connect () -> SS.Handler b CD.Connect ()
homeHandler sendHomePage = SC.method SC.GET handleGet <|> SH.respondWithError SH.badRequest "You can only GET the homepage."
  where
    handleGet = handleByMediaType mediaTypeMap <|> unknownHeader

    mediaTypeMap =
      [ (jsonMT, atlassianConnectHandler)
      , (textHtmlMT, sendHomePage)
      ]

    unknownHeader = SH.respondWithError SH.notFound "No response to a request with the provided Accept header."

    (Just jsonMT) = parseMediaType (show ApplicationJson)
    (Just textHtmlMT) = parseMediaType (show TextHtml)

parseMediaType :: String -> Maybe NM.MediaType
parseMediaType = NM.parse . BC.pack

handleByMediaType :: [ (NM.MediaType, SS.Handler b v ()) ] -> SS.Handler b v ()
handleByMediaType handlers = do
    orderedMediaTypes <- getAcceptMediaTypes
    case find (`elem` acceptableMediaTypes) orderedMediaTypes of
        Nothing -> mzero
        Just mediaType -> fromMaybe mzero (ML.lookup (OMT mediaType) responseMap)
    where
        acceptableMediaTypes = fst <$> handlers
        responseMap = ML.fromList . fmap (ARO.first OMT) $ handlers

-- An example accept header:
-- Just "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
getAcceptMediaTypes :: SS.Handler b v [NM.MediaType]
getAcceptMediaTypes = do
   request <- SC.getRequest
   case SC.getHeader bsAccept request of
      Nothing -> return []
      Just rawHeader -> return . catMaybes . fmap parseMediaType . splitOn "," . BC.unpack $ rawHeader

bsAccept :: CI.CI BC.ByteString
bsAccept = CI.mk . BC.pack $ "Accept"

connectRoutes :: [(BC.ByteString, SS.Handler b CD.Connect ())]
connectRoutes = fmap (ARO.first BC.pack) simpleConnectRoutes

-- Handler b v a
-- b: lens from the base state to the current snaplets state (is the base state)
-- v: is the state of the current "view" snaplet (or simply, current state)
-- a: Monad return type
-- The MonadSnaplet type class distills the essence of the operations used with this pattern.
-- Its functions define fundamental methods for navigating snaplet trees.

simpleConnectRoutes :: [(String, SS.Handler b CD.Connect ())]
simpleConnectRoutes =
  [ ("/atlassian-connect.json" , atlassianConnectHandler)
  ]

atlassianConnectHandler :: SS.Handler b CD.Connect ()
atlassianConnectHandler = do
  connectData <- get
  let dc = AC.DynamicDescriptorConfig
          { AC.dcPluginName = case CD.connectPluginName connectData of Name t -> Name t
          , AC.dcPluginKey = CD.connectPluginKey connectData
          , AC.dcBaseUrl = CD.connectBaseUrl connectData
          }
  SH.writeJson . AC.addonDescriptor $ dc

getLifecycleResponse :: SS.Handler b a (Maybe CL.LifecycleResponse)
getLifecycleResponse = do
    request <- SC.readRequestBody (1024 * 10)
    return . A.decode $ request
