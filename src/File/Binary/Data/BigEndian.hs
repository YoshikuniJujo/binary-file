{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Data.BigEndian (
	intToWords
) where

import Classes
import qualified Data.ByteString.Lazy as BSL
import Data.Word

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n s = (
		wordsToInt $ BSL.unpack $ fst $ getBytes n s,
		snd $ getBytes n s
	 )
--	toBinary n = makeBinary . BSLC.pack . reverse . lintToBin n . fromIntegral
	toBinary n = makeBinary . BSL.pack . intToWords n

wordsToInt :: [Word8] -> Int
wordsToInt = foldl (\i w -> i * 256 + fromIntegral w) 0

intToWords :: Integral i => Int -> i -> [Word8]
intToWords = itw []
	where
	itw result 0 _ = result
	itw result n i =
		itw (fromIntegral (i `mod` 256) : result) (n - 1) (i `div` 256)
