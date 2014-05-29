{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  , fooHandler
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Aeson
import           GHC.Generics
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application

data Baz = Baz {
    id :: Int
  , name :: T.Text
  , stuff :: [Int]
} deriving (Show, Generic)

instance ToJSON Baz
statusCodeHandler :: MonadSnap m => m ()
statusCodeHandler = do
    modifyResponse $ setResponseCode 400 -- Bad Request
    writeBS "I'm a strict bytestring"

setHeaderHandler :: MonadSnap m => m ()
setHeaderHandler = do
    modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"
    writeLBS "I'm a lazy bytestring"

setContentTypeHandler :: MonadSnap m => m ()
setContentTypeHandler = do
    modifyResponse $ setContentType "text/plain"
    writeText "I'm strict text"

getHeaderHandler :: MonadSnap m => m ()
getHeaderHandler = do
    req <- getRequest
    let origin = getHeader "Origin" req
    maybe showError o origin
  where o origin = writeBS ("Origin: " `B.append` origin)
        showError = writeText "Uh, oh"

-- | The fooHandler only responds to GET requests
--
-- E.g.
--  curl -X DELETE http://localhost:9000/foo 
-- returns an error
fooHandler :: MonadSnap m => m ()
fooHandler = method GET someJson <|> showError
  where someJson = do
            let content = Baz 1 "Test" [3,4,5]
            writeJson content
        showError = do
            logError "Foo can only GET"
            modifyResponse $ setResponseCode 405 -- Method not allowed
            writeText "error"

barHandler :: MonadSnap m => m ()
barHandler = do
        mUId <- getParam "id"
        maybe (writeText "Bar") writeBS mUId

writeJson :: (MonadSnap m, ToJSON a) => a -> m ()
writeJson a = do
    modifyResponse $ setContentType "application/json"
    writeLBS $ encode a

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/foo"          , fooHandler)
         , ("/bar/:id"      , barHandler)
         , ("/status"       , statusCodeHandler)
         , ("/header"       , setHeaderHandler)
         , ("/content-type" , setContentTypeHandler)
         , (""              , serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "ping-me connect" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    db <- nestSnaplet "db" db pgsInit
    addRoutes routes
    return $ App h s db

