module Data.Padding.PKCS where

import Data.Padding.Util (requiredPadding)

import Data.Word (Word8)
import Data.Char (chr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

pkcs7Pad :: Word8 -> B.ByteString -> B.ByteString
pkcs7Pad blockBoundary input = input `B.append` padding
   where
      padding = BC.replicate paddingLength padCharacter
      padCharacter = chr . fromIntegral $ paddingLength
      paddingLength = requiredPadding inputLength (fromIntegral blockBoundary)
      inputLength = B.length input

pkcs7Unpad :: Word8 -> B.ByteString -> Maybe B.ByteString
pkcs7Unpad blockBoundary input = if validLength
   then if lastChar >= blockBoundary
      then Just input
      else Just removedPadding
   else Nothing
   where
      removedPadding = if B.length padding == fromIntegral lastChar
         then potentialGoodData
         else input
      (potentialGoodData, padding) = B.spanEnd (== lastChar) input
      inputLength = B.length input
      lastChar = B.last input
      validLength = inputLength `mod` fromIntegral blockBoundary == 0
