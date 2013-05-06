{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances.BigEndian () where

import File.Binary.Classes (Field(..), Binary(..))
import Data.ByteString.Lazy (pack, unpack)
import Data.Word (Word8, Word32)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Control.Arrow (first)

--------------------------------------------------------------------------------

instance Field Integer where
	type FieldArgument Integer = Int
	fromBinary n = return . first (wordsToInt . unpack) . getBytes n
	toBinary n = return . makeBinary . pack . intToWords n

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n = return . first (wordsToInt . unpack) . getBytes n
	toBinary n = return . makeBinary . pack . intToWords n

instance Field Word32 where
	type FieldArgument Word32 = Int
	fromBinary n = return . first (wordsToInt . unpack) . getBytes n
	toBinary n = return . makeBinary . pack . intToWords n

wordsToInt :: (Num i, Bits i) => [Word8] -> i
wordsToInt = foldl (\i w -> i `shiftL` 8 .|. fromIntegral w) 0

intToWords :: (Bits i, Integral i) => Int -> i -> [Word8]
intToWords = itw []
	where
	itw r 0 _ = r
	itw r n i = itw (fromIntegral (i .&. 0xff) : r) (n - 1) (i `shiftR` 8)
