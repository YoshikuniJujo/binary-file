{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances.BigEndian (
	intToWords,
	BitsInt
) where

import File.Binary.Classes
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Control.Applicative
import Data.Bits
import Data.Monoid
import Control.Arrow

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n s =
		(,) <$> wordsToInt . BSL.unpack . fst <*> snd $ getBytes n s
--		(wordsToInt $ BSL.unpack $ fst $ getBytes n s, snd $ getBytes n s)
	toBinary n = makeBinary . BSL.pack . intToWords n

wordsToInt :: Integral i => [Word8] -> i
wordsToInt = foldl (\i w -> i * 256 + fromIntegral w) 0

intToWords :: Integral i => Int -> i -> [Word8]
intToWords = itw []
	where
	itw result 0 _ = result
	itw result n i =
		itw (fromIntegral (i `mod` 256) : result) (n - 1) (i `div` 256)

instance Field Integer where
	type FieldArgument Integer = Int
	fromBinary n s = (
		wordsToInt $ BSL.unpack $ fst $ getBytes n s,
		snd $ getBytes n s
	 )
	toBinary n = makeBinary . BSL.pack . intToWords n

instance Field Bool where
	type FieldArgument Bool = ()
	fromBits () ([], bin) = fromBits () $ binToBools bin
	fromBits () (bs, bin) = (last bs, (init bs, bin))
	consToBits () b (bs, bin)
		| length bs == 7 = ([], (bs ++ [b]) `appendBools` bin)
		| otherwise = (bs ++ [b], bin)

data BitsInt = BitsInt Int deriving Show

instance Field BitsInt where
	type FieldArgument BitsInt = Int
	fromBits n = first BitsInt . fbb 0 n
	consToBits n (BitsInt f) = ctbb n f

fbb :: Binary b => Int -> Int -> ([Bool], b) -> (Int, ([Bool], b))
fbb r 0 bb = (r, bb)
fbb r n ([], bin) = fbb r n $ binToBools bin
fbb r n (bs, bin) = fbb (r `shiftL` 1 .|. fromIntegral (fromEnum $ last bs))
	(n - 1) (init bs, bin)

ctbb :: Binary b => Int -> Int -> ([Bool], b) -> ([Bool], b)
ctbb 0 _ r = r
ctbb n f (bs, bin)
	| length bs == 7 = ctbb (n - 1) (f `shiftR` 1)
		([], (bs ++ [toEnum (f .&. 1)]) `appendBools` bin)
	| otherwise = ctbb (n - 1) (f `shiftR` 1)
		(bs ++ [toEnum (f .&. 1)], bin)

binToBools :: Binary b => b -> ([Bool], b)
binToBools = first (wtbs (8 :: Int) . head . BSL.unpack) . getBytes 1
	where
	wtbs 0 _ = []
	wtbs n w = toEnum (fromIntegral $ 1 .&. w) : wtbs (n - 1) (w `shiftR` 1)

appendBools :: Binary b => [Bool] -> b -> b
appendBools bs = mappend $ makeBinary (BSL.singleton $ bstw bs)
	where
	bstw = foldr (\b w -> w `shiftL` 1 .|. fromIntegral (fromEnum b)) 0

{-
binToBools :: Binary b => b -> ([Bool], b)
binToBools = first (wtbs [] (8 :: Int) . head . BSL.unpack) . getBytes 1
	where
	wtbs bs 0 _ = bs
	wtbs bs n w = wtbs (toEnum (fromIntegral $ 1 .&. w) : bs) (n - 1)
		(w `shiftR` 1)

appendBools :: Binary b => [Bool] -> b -> b
appendBools bs = mappend $ makeBinary (BSL.singleton $ bstw bs)
	where
	bstw = foldl (\w b -> w `shiftL` 1 .|. fromIntegral (fromEnum b)) 0
-}
