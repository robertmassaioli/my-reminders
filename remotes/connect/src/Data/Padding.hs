module Data.Padding 
   ( zeroPad
   , zeroUnpad
   ) where

import Data.Padding.Util (requiredPadding)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr)

zeroPad :: Int -> B.ByteString -> B.ByteString
zeroPad boundary input = input `B.append` padding
   where
      padding = BC.replicate padLength (chr 0)
      padLength = requiredPadding inputLength boundary
      inputLength = B.length input

zeroUnpad :: B.ByteString -> B.ByteString
zeroUnpad = fst . BC.spanEnd (== chr 0)
