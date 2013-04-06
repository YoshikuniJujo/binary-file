{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances.LittleEndian where

import File.Binary.Classes
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Control.Arrow
import Data.Bits

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n = first (wordsToInt . BSL.unpack) . getBytes n
	toBinary n = makeBinary . BSL.pack . intToWords n

instance Field Integer where
	type FieldArgument Integer = Int
	fromBinary n = first (wordsToInt . BSL.unpack) . getBytes n
	toBinary n = makeBinary . BSL.pack . intToWords n

{-
instance Field Bool where
	type FieldArgument Bool = ()
	fromBinary n = fromMaybe "fromBinary for Bool error" $ \(h, t) ->
		(toEnum $ fromIntegral $ h .&. 0x01, cons (shiftR h) t)
	toBinary n = 
-}

wordsToInt :: Integral i => [Word8] -> i
wordsToInt = foldr (\w i -> fromIntegral w + 256 * i) 0

intToWords :: Integral i => Int -> i -> [Word8]
intToWords 0 _ = []
intToWords n i = fromIntegral (i `mod` 256) : intToWords (n - 1) (i `div` 256)
