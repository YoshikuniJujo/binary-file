{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances.LittleEndian (BitsInt) where

import File.Binary.Classes (Field(..), Binary(..), pop, push)
import Data.ByteString.Lazy (pack, unpack)
import Data.Word (Word8, Word32)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Control.Arrow (first)
import Control.Monad

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

instance Field Bool where
	type FieldArgument Bool = ()
	fromBits () ([], bin) = fromBits () $ pop bin
	fromBits () (b : bs, bin) = return (b, (bs, bin))
	consToBits () b (bs, bin)
		| length bs == 7 = return ([], push (b : bs, bin))
		| otherwise = return (b : bs, bin)

newtype BitsInt = BitsInt { bitsInt :: Int } deriving Show

instance Eq BitsInt where
	BitsInt i1 == BitsInt i2 = i1 == i2

instance Num BitsInt where
	BitsInt i1 + BitsInt i2 = BitsInt $ i1 + i2
	BitsInt i1 * BitsInt i2 = BitsInt $ i1 * i2
	abs (BitsInt i) = BitsInt $ abs i
	signum (BitsInt i) = BitsInt $ signum i
	fromInteger i = BitsInt $ fromInteger i

instance Field BitsInt where
	type FieldArgument BitsInt = Int
	fromBits 0 bb = return (BitsInt 0, bb)
	fromBits n ([], bin) = fromBits n $ pop bin
	fromBits n (b : bs, bin) = first
		(BitsInt . (fromEnum b .|.) . (`shiftL` 1) . bitsInt) `liftM`
		fromBits (n - 1) (bs, bin)
	consToBits 0 _ bb = return bb
	consToBits n (BitsInt f) bb = do
		(bs, bin) <- consToBits (n - 1) (BitsInt $ f `shiftR` 1) bb
		return $ if length bs == 7
			then ([], push (toEnum (f .&. 1) : bs, bin))
			else (toEnum (f .&. 1) : bs, bin)
