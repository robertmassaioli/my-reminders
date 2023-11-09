{-# LANGUAGE OverloadedStrings #-}
module StaticSnaplet
    ( initStaticSnaplet
    , StaticConf
    ) where

import qualified AssetManifestParser       as AMP
import           Control.Applicative
import           Control.Arrow             (second)
import           Control.Lens              ((&), (.~))
import qualified Control.Monad.IO.Class    as MI
import           Control.Monad.State.Class (get)
import           Data.ByteString           (ByteString, append)
import qualified Data.ByteString.Char8     as BC
import qualified Data.CaseInsensitive      as CI
import           Data.Maybe                (isNothing)
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.UUID.V4              as UUID
import           Data.Version              (Version (..), showVersion)
import qualified Heist                     as H
import qualified Heist.Internal.Types      as HIT
import qualified Heist.Interpreted         as HI
import           MicrosZone                (Zone (..), fromEnv)
import qualified Snap.Core                 as SC
import qualified Snap.Snaplet              as SS
import qualified Snap.Snaplet.Heist        as SSH
import qualified SnapHelpers               as SH
import qualified Text.XmlHtml              as X
import qualified Data.Map.Syntax           as MS

-- | This initialiser is responsible for creating a Static Snaplet; all of the resources served by this snaplet
-- will be treated as immutable for the Version provided. It is here for the purpose of providing a way to serve
-- assets (such as CSS, JS and Images) to the browser and make sure that the browser caches these assets for as long
-- as possible. If you make any changes to your assets at all you will need to bump the Version that you provide to this
-- snaplet on your next deployment so that the browser knows to redownload your assets.
initStaticSnaplet
    :: Version -- ^ The current version of these assets
    -> [(ByteString, SS.Handler b StaticConf ())] -- ^ Your static content routes.
    -> SS.Snaplet (SSH.Heist b) -- ^ The application heist snaplet.
    -> SS.SnapletInit b StaticConf
initStaticSnaplet version staticRoutes appHeist = SS.makeSnaplet "Static Content" "Static content per version" Nothing $ do
    conf <- MI.liftIO $ generateStaticConf version
    SS.addRoutes $ versionRoutes staticRoutes
    SSH.addConfig appHeist (spliceConfig conf)
    return conf

data StaticConf = StaticConf
    { scResourcesVersion :: String
    , scCacheForever     :: Bool
    , scAssetManifest    :: AMP.AssetManifest
    } deriving (Show)

spliceConfig :: StaticConf -> H.SpliceConfig (SS.Handler a a)
spliceConfig sc = mempty
   & HIT.scInterpretedSplices .~ customSplices sc

customSplices :: StaticConf -> HIT.Splices (HI.Splice (SS.Handler a a))
customSplices sc = "resourcesVersion" MS.## resourcesVersion sc

resourcesVersion :: StaticConf -> SSH.SnapletISplice a
resourcesVersion = return . text . scResourcesVersion

text :: String -> [X.Node]
text x = [X.TextNode (T.pack x)]

generateStaticConf :: Version -> IO StaticConf
generateStaticConf version = do
   zone <- fromEnv
   versionString <- generateResourcesVersion version zone
   assetManifest <- AMP.parseAssetManifest "frontend/build/asset-manifest.json"
   return StaticConf
      { scResourcesVersion = versionString
      , scCacheForever = not . isLocal $ zone
      , scAssetManifest = assetManifest
      }
   where
      isLocal = isNothing

generateResourcesVersion :: Version -> Maybe Zone -> IO String
generateResourcesVersion v (Just _) = return . showVersion $ v
generateResourcesVersion v Nothing  = do
    hash <- UUID.nextRandom
    return $ showVersion v ++ "-" ++ show hash

-- We should first try and directly match and extract the resources version, and it it does match then
-- pass the route onwards without the resources version present. Otherwise, if no matching route could
-- be found then we should redirect the to the same URL but with the resources version injected.
versionRoutes :: [(ByteString, SS.Handler a StaticConf ())] -> [(ByteString, SS.Handler a StaticConf ())]
versionRoutes staticRoutes =
    [ (":rv/"   , handlePotentialResourcesVersion (SC.route $ unmatchedRoute : wrappedRoutes))
    , (""       , redirectStatic)
    ]
    where
        wrappedRoutes = map (second wrapHandlerInCacheOrNotFound) staticRoutes

handlePotentialResourcesVersion :: SS.Handler a StaticConf () -> SS.Handler a StaticConf ()
handlePotentialResourcesVersion handle = do
    potentialResourcesVersion <- SC.getParam "rv"
    case potentialResourcesVersion of
        Nothing -> SC.pass -- Don't handle this request
        Just passedResourcesVersion -> do
            actualResourcesVersion <- BC.pack . scResourcesVersion <$> get
            if passedResourcesVersion == actualResourcesVersion then handle else SC.pass

-- Add the resources version between the context path and the path information and redirect to that
redirectStatic :: SS.Handler a StaticConf ()
redirectStatic = do
    sc <- get
    r <- SC.getRequest
    let cp = SC.rqContextPath r
    let pathInfo = SC.rqPathInfo r
    let rawLookupPathInfo = T.decodeUtf8 (cp `append` pathInfo)
    let lookupPathInfo = maybe rawLookupPathInfo id . T.stripPrefix "/static/frontend/" $ rawLookupPathInfo
    let assetLookup = T.stripPrefix "/static/" =<< M.lookup lookupPathInfo (scAssetManifest sc)
    let redirectPath = maybe pathInfo T.encodeUtf8 assetLookup
    SC.redirect $ cp `append` addResourcesVersion sc redirectPath

addResourcesVersion :: StaticConf -> BC.ByteString -> BC.ByteString
addResourcesVersion (StaticConf rv _ _) original = BC.pack rv `append` BC.pack "/" `append` original

unmatchedRoute :: (ByteString, SS.Handler a b ())
unmatchedRoute = ("", SH.respondNotFound)

wrapHandlerInCacheOrNotFound :: SS.Handler a StaticConf () -> SS.Handler a StaticConf ()
wrapHandlerInCacheOrNotFound handle = wrapHandlerInCache handle <|> SH.respondNotFound

wrapHandlerInCache :: SS.Handler a StaticConf () -> SS.Handler a StaticConf ()
wrapHandlerInCache handle = do
    handle
    shouldCacheForever <- scCacheForever <$> get
    if shouldCacheForever then cacheForever else noCache

cacheForever :: SS.Handler a b ()
cacheForever = cache "max-age=31536000"

noCache :: SS.Handler a b ()
noCache = cache "no-cache"

cache :: BC.ByteString -> SS.Handler a b ()
cache v = SC.modifyResponse (SC.setHeader (CI.mk "Cache-Control") v)