{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
module Handler.RestTests
  (
    main
  , defaultTestGroup
  ) where

import qualified Test.HUnit
import qualified Data.Text              as T
import qualified Data.ByteString.Char8  as B
import           Test.Tasty
import           Test.Tasty.TH
import           Test.Tasty.QuickCheck  as QC
import           Test.Tasty.HUnit
import           Test.QuickCheck hiding (Success)
import           Control.Applicative
import           Control.Monad          (mzero)
import           Data.Bits
import           Data.Word
import qualified Data.Map.Lazy as Map
import           Data.Aeson.Encode
import           Data.Aeson.Parser      (value)
import           Data.Aeson.Types

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





