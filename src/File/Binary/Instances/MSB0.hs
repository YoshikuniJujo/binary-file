{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances.MSB0 (BitsInt) where

import File.Binary.Classes
import Data.Bits (Bits, shiftL, shiftR, (.|.), (.&.))
import Control.Arrow

instance Field Bool where
	type FieldArgument Bool = ()
	fromBits () ([], bin) = fromBits () $ pop bin
	fromBits () (bs, bin) = return (last bs, (init bs, bin))
	consToBits () b (bs, bin)
		| length bs == 7 = return ([], push (bs ++ [b], bin))
		| otherwise = return (bs ++ [b], bin)

data BitsInt = BitsInt { bitsInt :: Int } deriving Show

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
