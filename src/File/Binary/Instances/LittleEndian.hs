{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances.LittleEndian where

import File.Binary.Classes
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Control.Arrow

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n = first (wordsToInt . BSL.unpack) . getBytes n
	toBinary n = makeBinary . BSL.pack . intToWords n

instance Field Integer where
	type FieldArgument Integer = Int
	fromBinary n = first (wordsToInt . BSL.unpack) . getBytes n
	toBinary n = makeBinary . BSL.pack . intToWords n

wordsToInt :: Integral i => [Word8] -> i
wordsToInt = foldr (\w i -> fromIntegral w + 256 * i) 0

intToWords :: Integral i => Int -> i -> [Word8]
intToWords 0 _ = []
intToWords n i = fromIntegral (i `mod` 256) : intToWords (n - 1) (i `div` 256)