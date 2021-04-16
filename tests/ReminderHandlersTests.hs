module ReminderHandlersTests where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic (assert, monadicIO, PropertyM)
import Snap.Core hiding (setHeader)
import Snap.Test hiding (evalHandler, runHandler)
import Snap.Snaplet.Test

import ReminderHandlers (handleReminder, handleReminders)
import Site (app)
import SnapHelpers
import TestHelpers
import WithToken

tests :: [Test]
-- Generation for these tests is currently irrelevant and Snaplet testing is slow,
-- so we are keeping the generated test count low.
tests = map (plusTestOptions $ emptyTestOptions {topt_maximum_generated_tests = Just 5})
  [ testProperty "POST always fails with 404 Not found" prop_postFails
  , testProperty "GET fails without valid authorisation with 401 Unauthorized" prop_unauthorisedGetFails
  , testProperty "PUT fails without valid authorisation with 401 Unauthorized" prop_unauthorisedPutFails
  , testProperty "DELETE fails without valid authorisation with 401 Unauthorized" prop_unauthorisedDeleteFails
  ]

prop_postFails :: C.ByteString -> C.ByteString -> C.ByteString -> Property
prop_postFails path contentType body = monadicIO $ do
  errorOrResponse <- runHandler Nothing (postRaw path contentType body) handleReminder app
  return $ responseCodeIs notFound errorOrResponse

prop_unauthorisedGetFails :: Params -> Property
prop_unauthorisedGetFails params = requestIsBad $ do
  get emptyPath params
  uncurry setHeader emptyPageTokenHeader

prop_unauthorisedPutFails :: C.ByteString -> C.ByteString -> Property
prop_unauthorisedPutFails ct body = requestIsBad $ do
  put emptyPath ct body
  uncurry setHeader emptyPageTokenHeader

prop_unauthorisedDeleteFails :: Params -> Property
prop_unauthorisedDeleteFails params = requestIsBad $ do
  delete emptyPath params
  uncurry setHeader emptyPageTokenHeader

requestIs :: Int -> RequestBuilder (PropertyM IO) () -> Property
requestIs responseCode req = monadicIO $ do
  errorOrResponse <- runHandler Nothing req handleReminder app
  return $ responseCodeIs responseCode (testTrace "Response" errorOrResponse)

requestIsUnauthorised :: RequestBuilder (PropertyM IO) () -> Property
requestIsUnauthorised = requestIs unauthorised

requestIsBad :: RequestBuilder (PropertyM IO) () -> Property
requestIsBad = requestIs badRequest

