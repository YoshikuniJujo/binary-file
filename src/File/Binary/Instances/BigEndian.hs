{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances.BigEndian (BitsInt) where

import File.Binary.Classes (Field(..), Binary(..), pop, push)
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

instance Field Bool where
	type FieldArgument Bool = ()
	fromBits () ([], bin) = fromBits () $ pop bin
	fromBits () (bs, bin) = return (last bs, (init bs, bin))
	consToBits () b (bs, bin)
		| length bs == 7 = return ([], push (bs ++ [b], bin))
		| otherwise = return (bs ++ [b], bin)

data BitsInt = BitsInt { bitsInt :: Int } deriving Show

instance Field BitsInt where
	type FieldArgument BitsInt = Int
	fromBits n = return . first BitsInt . fb n 0
	consToBits n = ctb n . bitsInt

fromEnum' :: (Enum e, Num i) => e -> i
fromEnum' = fromIntegral . fromEnum

toEnum' :: (Enum e, Integral i) => i -> e
toEnum' = toEnum . fromIntegral

fb :: (Bits f, Num f, Binary b) => Int -> f -> ([Bool], b) -> (f, ([Bool], b))
fb 0 r bb = (r, bb)
fb n r ([], b) = fb n r $ pop b
fb n r (bs, b) = fb (n - 1) (r `shiftL` 1 .|. fromEnum' (last bs)) (init bs, b)

ctb :: (Bits f, Integral f, Binary b, Functor m, Monad m) =>
	Int -> f -> ([Bool], b) -> m ([Bool], b)
ctb 0 _ r = return r
ctb n f (bs, b)
	| length bs == 7 = ctb (n - 1) (f `shiftR` 1) ([], push (bs ++ [bit], b))
	| otherwise = ctb (n - 1) (f `shiftR` 1) (bs ++ [bit], b)
	where bit = toEnum' $ f .&. 1
