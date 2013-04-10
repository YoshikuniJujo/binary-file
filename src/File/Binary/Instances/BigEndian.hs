{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances.BigEndian (BitsInt) where

import File.Binary.Classes (Field(..), Binary(..), pop, push)
import Data.ByteString.Lazy (pack, unpack)
import Data.Word (Word8)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Control.Arrow (first)

--------------------------------------------------------------------------------

instance Field Integer where
	type FieldArgument Integer = Int
	fromBinary n = first (wordsToInt . unpack) . getBytes n
	toBinary n = makeBinary . pack . intToWords n

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n = first (wordsToInt . unpack) . getBytes n
	toBinary n = makeBinary . pack . intToWords n

wordsToInt :: Bits i => [Word8] -> i
wordsToInt = foldl (\i w -> i `shiftL` 8 .|. fromIntegral w) 0

intToWords :: (Bits i, Integral i) => Int -> i -> [Word8]
intToWords = itw []
	where
	itw r 0 _ = r
	itw r n i = itw (fromIntegral (i .&. 0xff) : r) (n - 1) (i `shiftR` 8)

instance Field Bool where
	type FieldArgument Bool = ()
	fromBits () ([], bin) = fromBits () $ pop bin
	fromBits () (bs, bin) = (last bs, (init bs, bin))
	consToBits () b (bs, bin)
		| length bs == 7 = ([], push (bs ++ [b], bin))
		| otherwise = (bs ++ [b], bin)

data BitsInt = BitsInt { bitsInt :: Int } deriving Show

instance Field BitsInt where
	type FieldArgument BitsInt = Int
	fromBits n = first BitsInt . fb n 0
	consToBits n = ctb n . bitsInt

fromEnum' :: (Enum e, Num i) => e -> i
fromEnum' = fromIntegral . fromEnum

toEnum' :: (Enum e, Integral i) => i -> e
toEnum' = toEnum . fromIntegral

fb :: (Bits f, Binary b) => Int -> f -> ([Bool], b) -> (f, ([Bool], b))
fb 0 r bb = (r, bb)
fb n r ([], b) = fb n r $ pop b
fb n r (bs, b) = fb (n - 1) (r `shiftL` 1 .|. (fromEnum' $ last bs)) (init bs, b)

ctb :: (Bits f, Integral f, Binary b) => Int -> f -> ([Bool], b) -> ([Bool], b)
ctb 0 _ r = r
ctb n f (bs, b)
	| length bs == 7 = ctb (n - 1) (f `shiftR` 1) ([], push (bs ++ [bit], b))
	| otherwise = ctb (n - 1) (f `shiftR` 1) (bs ++ [bit], b)
	where bit = toEnum' $ f .&. 1
