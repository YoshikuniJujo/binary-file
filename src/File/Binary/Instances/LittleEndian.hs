{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances.LittleEndian (BitsInt) where

import File.Binary.Classes (Field(..), Binary(..), pop, push)
import Data.ByteString.Lazy (pack, unpack)
import Data.Word (Word8, Word32)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Control.Arrow (first)
import Control.Applicative

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n = return . first (wordsToInt . unpack) . getBytes n
	toBinary n = makeBinary . pack . intToWords n

instance Field Word32 where
	type FieldArgument Word32 = Int
	fromBinary n = return . first (wordsToInt . unpack) . getBytes n
	toBinary n = makeBinary . pack . intToWords n

instance Field Integer where
	type FieldArgument Integer = Int
	fromBinary n = return . first (wordsToInt . unpack) . getBytes n
	toBinary n = makeBinary . pack . intToWords n

wordsToInt :: Bits i => [Word8] -> i
wordsToInt = foldr (\w i -> fromIntegral w .|. i `shiftL` 8) 0

intToWords :: (Bits i, Integral i) => Int -> i -> [Word8]
intToWords 0 _ = []
intToWords n i = fromIntegral (i .&. 0xff) : intToWords (n - 1) (i `shiftR` 8)

instance Field Bool where
	type FieldArgument Bool = ()
	fromBits () ([], bin) = fromBits () $ pop bin
	fromBits () (b : bs, bin) = return (b, (bs, bin))
	consToBits () b (bs, bin)
		| length bs == 7 = ([], push (b : bs, bin))
		| otherwise = (b : bs, bin)

data BitsInt = BitsInt { bitsInt :: Int } deriving Show

instance Field BitsInt where
	type FieldArgument BitsInt = Int
	fromBits 0 bb = return (BitsInt 0, bb)
	fromBits n ([], bin) = fromBits n $ pop bin
	fromBits n (b : bs, bin) = first
		(BitsInt . (fromEnum b .|.) . (`shiftL` 1) . bitsInt) <$>
		fromBits (n - 1) (bs, bin)
	consToBits 0 _ bb = bb
	consToBits n (BitsInt f) bb = let
		(bs, bin) = consToBits (n - 1) (BitsInt $ f `shiftR` 1) bb in
		if length bs == 7
			then ([], push (toEnum (f .&. 1) : bs, bin))
			else (toEnum (f .&. 1) : bs, bin)
