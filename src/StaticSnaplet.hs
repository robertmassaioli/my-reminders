{-# LANGUAGE OverloadedStrings #-}
module StaticSnaplet
    ( initStaticSnaplet
    , StaticConf
    ) where

import           Control.Applicative
import           Control.Lens              ((&), (.~))
import qualified Control.Monad.IO.Class    as MI
import           Control.Monad.State.Class (get)
import           Data.ByteString           (ByteString, append)
import qualified Data.ByteString.Char8     as BC
import qualified Data.CaseInsensitive      as CI
import           Data.Maybe                (isNothing)
import           Data.Monoid               (mempty)
import qualified Data.Text                 as T
import qualified Data.UUID.V4              as UUID
import           Data.Version              (showVersion)
import qualified Heist                     as H
import qualified Heist.Internal.Types      as HIT
import qualified Heist.Interpreted         as HI
import           MicrosZone                (fromEnv)
import qualified Snap.Core                 as SC
import qualified Snap.Snaplet              as SS
import qualified Snap.Snaplet.Heist        as SSH
import           Snap.Util.FileServe
import qualified SnapHelpers               as SH
import qualified Text.XmlHtml              as X

import qualified Paths_my_reminders        as PMR

initStaticSnaplet :: SS.Snaplet (SSH.Heist b) -> SS.SnapletInit b StaticConf
initStaticSnaplet appHeist = SS.makeSnaplet "Static Content" "Static content per version" Nothing $ do
    conf <- StaticConf <$> MI.liftIO generateResourcesVersion
    SS.addRoutes versionRoutes
    SSH.addConfig appHeist (spliceConfig conf)
    return conf

spliceConfig :: StaticConf -> H.SpliceConfig (SS.Handler a a)
spliceConfig sc = mempty
   & HIT.scInterpretedSplices .~ customSplices sc

customSplices :: StaticConf -> HIT.Splices (HI.Splice (SS.Handler a a))
customSplices sc = "resourcesVersion" H.## resourcesVersion sc

resourcesVersion :: StaticConf -> SSH.SnapletISplice a
resourcesVersion = return . text . scResourcesVersion

text :: String -> [X.Node]
text x = [X.TextNode (T.pack x)]

generateResourcesVersion :: IO String
generateResourcesVersion = do
   localZone <- isLocal <$> fromEnv
   if localZone
      then do
         randomKey <- UUID.nextRandom
         return $ versionString ++ "-" ++ show randomKey
      else return versionString
   where
      isLocal = isNothing
      versionString = showVersion PMR.version

data StaticConf = StaticConf
    { scResourcesVersion :: String
    } deriving (Show)

versionRoutes :: [(ByteString, SS.Handler a StaticConf ())]
versionRoutes =
    [ (":rv/"   , handlePotentialResourcesVersion (SC.route staticRoutes))
    , (""       , redirectStatic)
    ]

handlePotentialResourcesVersion :: SS.Handler a StaticConf () -> SS.Handler a StaticConf ()
handlePotentialResourcesVersion handle = do
    potentialResourcesVersion <- SC.getParam "rv"
    case potentialResourcesVersion of
        Nothing -> SC.pass -- Don't handle this request
        Just passedResourcesVersion -> do
            actualResourcesVersion <- BC.pack . scResourcesVersion <$> get
            if passedResourcesVersion == actualResourcesVersion then handle else SC.pass

staticRoutes :: [(ByteString, SS.Handler a StaticConf ())]
staticRoutes =
  [ ("css"    , staticServeDirectory "static/css")
  , ("images" , staticServeDirectory "static/images")
  , ("js"     , staticServeDirectory "static-js")
  , (""       , SH.respondNotFound) -- If nothing else was matched then stop immediately.
  ]

staticServeDirectory :: FilePath -> SS.Handler a b ()
staticServeDirectory path = (serveDirectory path >> cacheForever) <|> SH.respondNotFound

cacheForever :: SS.Handler a b ()
cacheForever = SC.modifyResponse (SC.setHeader (CI.mk "Cache-Control") "max-age=31536000")

redirectStatic :: SS.Handler a StaticConf ()
redirectStatic = do
    sc <- get
    r <- SC.getRequest
    let cp = SC.rqContextPath r
    let pathInfo = SC.rqPathInfo r
    SC.redirect $ cp `append` addResourcesVersion sc pathInfo

addResourcesVersion :: StaticConf -> BC.ByteString -> BC.ByteString
addResourcesVersion (StaticConf rv) original = BC.pack rv `append` BC.pack "/" `append` original