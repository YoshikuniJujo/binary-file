{-# LANGUAGE TypeFamilies #-}
{-# OPtIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances.LSB0 (BitsInt(..)) where

import File.Binary.Classes
import Data.Bits
import Control.Arrow
import Control.Monad

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
