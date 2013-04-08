{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances.LittleEndian where

import File.Binary.Classes
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Control.Arrow
import Data.Bits
import Data.Monoid

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n = first (wordsToInt . BSL.unpack) . getBytes n
	toBinary n = makeBinary . BSL.pack . intToWords n

instance Field Integer where
	type FieldArgument Integer = Int
	fromBinary n = first (wordsToInt . BSL.unpack) . getBytes n
	toBinary n = makeBinary . BSL.pack . intToWords n

instance Field Bool where
	type FieldArgument Bool = ()
	fromBitsBinary () ([], bin) = fromBitsBinary () $ binToBools bin
	fromBitsBinary () (b : bs, bin) = (b, (bs, bin))
	consToBitsBinary () b (bs, bin)
		| length bs == 7 = ([], (b : bs) `appendBools` bin)
		| otherwise = (b : bs, bin)

data BitsInt = BitsInt { bitsInt :: Int } deriving Show

instance Field BitsInt where
	type FieldArgument BitsInt = Int
	fromBitsBinary 0 bb = (BitsInt 0, bb)
	fromBitsBinary n ([], bin) = fromBitsBinary n $ binToBools bin
	fromBitsBinary n (b : bs, bin) = let
		(BitsInt ret, rest) = fromBitsBinary (n - 1) (bs, bin) in
		(BitsInt $ fromEnum b .|. ret `shiftL` 1, rest)
	consToBitsBinary 0 _ bb = bb
	consToBitsBinary n (BitsInt f) bb = let
		(bs', bin') = consToBitsBinary (n - 1) (BitsInt $ f `shiftR` 1) bb in
		if length bs' == 7
			then ([], (toEnum (f .&. 1) : bs') `appendBools` bin')
			else (toEnum (f .&. 1) : bs', bin')

wordsToInt :: Integral i => [Word8] -> i
wordsToInt = foldr (\w i -> fromIntegral w + 256 * i) 0

intToWords :: Integral i => Int -> i -> [Word8]
intToWords 0 _ = []
intToWords n i = fromIntegral (i `mod` 256) : intToWords (n - 1) (i `div` 256)

binToBools :: Binary b => b -> ([Bool], b)
binToBools = first (wtbs (8 :: Int) . head . BSL.unpack) . getBytes 1
	where
	wtbs 0 _ = []
	wtbs n w = toEnum (fromIntegral $ 1 .&. w) : wtbs (n - 1) (w `shiftR` 1)

appendBools :: Binary b => [Bool] -> b -> b
appendBools bs = mappend $ makeBinary (BSL.singleton $ bstw bs)
	where
	bstw = foldr (\b w -> w `shiftL` 1 .|. fromIntegral (fromEnum b)) 0
