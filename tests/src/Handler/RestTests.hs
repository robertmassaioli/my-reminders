{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
module Handler.RestTests
  (
    main
  , defaultTestGroup
) where

import           Test.Tasty
import           Test.Tasty.TH
import           Test.Tasty.HUnit
import qualified Data.Map.Lazy as Map

import           Snap.Core hiding (setHeader)
import           Snap.Test

import           Site

defaultTestGroup :: TestTree
defaultTestGroup = $(testGroupGenerator)

main :: IO ()
main = defaultMain defaultTestGroup


case_fooHandlerGet = do
    let req = get "/foo" $ Map.fromList [("bar",["4"])]

    resp <- runHandler req fooHandler
    200 @=? rspStatus resp

case_fooHandlerPost = do
    let req = postUrlEncoded "/foo" $ Map.fromList [("bar",["4"])]

    resp <- runHandler req fooHandler
    405 @=? rspStatus resp





