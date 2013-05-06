{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances.LittleEndian () where

import File.Binary.Classes (Field(..), Binary(..))
import Data.ByteString.Lazy (pack, unpack)
import Data.Word (Word8, Word32)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Control.Arrow (first)

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n = return . first (wordsToInt . unpack) . getBytes n
	toBinary n = return . makeBinary . pack . intToWords n

instance Field Word32 where
	type FieldArgument Word32 = Int
	fromBinary n = return . first (wordsToInt . unpack) . getBytes n
	toBinary n = return . makeBinary . pack . intToWords n

instance Field Integer where
	type FieldArgument Integer = Int
	fromBinary n = return . first (wordsToInt . unpack) . getBytes n
	toBinary n = return . makeBinary . pack . intToWords n

wordsToInt :: (Num i, Bits i) => [Word8] -> i
wordsToInt = foldr (\w i -> fromIntegral w .|. i `shiftL` 8) 0

intToWords :: (Bits i, Integral i) => Int -> i -> [Word8]
intToWords 0 _ = []
intToWords n i = fromIntegral (i .&. 0xff) : intToWords (n - 1) (i `shiftR` 8)
