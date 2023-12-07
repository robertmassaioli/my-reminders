module Data.ConfiguratorTimeUnits where

import qualified Data.Configurator.Types as DCT
import           Data.Time.Units
import           Data.Ratio              (numerator, denominator)

instance DCT.Configured Attosecond where
  convert (DCT.Number x) = Just . fromIntegral . rationalToInteger $ x
  convert _          = Nothing

instance DCT.Configured Femtosecond where
  convert (DCT.Number x) = Just . fromIntegral . rationalToInteger $ x
  convert _          = Nothing

instance DCT.Configured Picosecond where
  convert (DCT.Number x) = Just . fromIntegral . rationalToInteger $ x
  convert _          = Nothing

instance DCT.Configured Nanosecond where
  convert (DCT.Number x) = Just . fromIntegral . rationalToInteger $ x
  convert _          = Nothing

instance DCT.Configured Microsecond where
  convert (DCT.Number x) = Just . fromIntegral . rationalToInteger $ x
  convert _          = Nothing

instance DCT.Configured Millisecond where
  convert (DCT.Number x) = Just . fromIntegral . rationalToInteger $ x
  convert _          = Nothing

instance DCT.Configured Second where
  convert (DCT.Number x) = Just . fromIntegral . rationalToInteger $ x
  convert _          = Nothing

instance DCT.Configured Minute where
  convert (DCT.Number x) = Just . fromIntegral . rationalToInteger $ x
  convert _          = Nothing

instance DCT.Configured Hour where
  convert (DCT.Number x) = Just . fromIntegral . rationalToInteger $ x
  convert _          = Nothing

instance DCT.Configured Day where
  convert (DCT.Number x) = Just . fromIntegral . rationalToInteger $ x
  convert _          = Nothing

instance DCT.Configured Week where
  convert (DCT.Number x) = Just . fromIntegral . rationalToInteger $ x
  convert _          = Nothing

instance DCT.Configured Fortnight where
  convert (DCT.Number x) = Just . fromIntegral . rationalToInteger $ x
  convert _          = Nothing

rationalToInteger :: Rational -> Integer
rationalToInteger x = numerator x `div` denominator x


