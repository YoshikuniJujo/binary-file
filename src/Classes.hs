{-# LANGUAGE
	TemplateHaskell,
	TypeSynonymInstances,
	FlexibleInstances,
	TypeFamilies,
	OverloadedStrings #-}

module Classes (
	Field(..),
	Binary(..),
	fii, tii,
	readInt,
	dp, fs, ti, cc,
	lintToBin
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Control.Arrow

data Endian = BigEndian | LittleEndian deriving Show

readInt :: Endian -> String -> Integer
readInt LittleEndian "" = 0
readInt LittleEndian (c : cs) = fromIntegral (ord c) + 2 ^ (8 :: Integer) * readInt LittleEndian cs
readInt BigEndian str = readInt LittleEndian $ reverse str

class Field r where
	type FieldArgument r
	fromBinary :: Binary s => FieldArgument r -> s -> (r, s)
	toBinary :: Binary s => FieldArgument r -> r -> s

instance Field r => Field [r] where
	type FieldArgument [r] = (FieldArgument r, Maybe Int)
	fromBinary (a, Just b) s = (b `times` fromBinary a) s
	fromBinary (a, Nothing) s = whole (fromBinary a) s
	toBinary (a, _) rs = cc $ map (toBinary a) rs

instance Field Char where
	type FieldArgument Char = ()
	fromBinary _ str = (head $ BSC.unpack t, d)
		where
		(t, d) = getBytes 1 str
	toBinary _ = fs . (: [])

instance Field BS.ByteString where
	type FieldArgument BS.ByteString = Int
	fromBinary n str = getBytes n str
	toBinary _ = makeBinary

class Binary a where
	getBytes :: Int -> a -> (BS.ByteString, a)
	makeBinary :: BS.ByteString -> a
	concatBinary :: [a] -> a
	emptyBinary :: a -> Bool

empty :: Binary a => a -> Bool
empty = emptyBinary

cc :: Binary a => [a] -> a
cc = concatBinary

ti :: Binary a => a -> Integer
ti = readInt LittleEndian . BSC.unpack . fst . getBytes 100

fs :: Binary a => String -> a
fs = makeBinary . BSC.pack

dp :: Binary a => Int -> a -> a
dp n = snd . getBytes n

instance Binary String where
	getBytes n = BSC.pack . take n &&& drop n
	makeBinary = BSC.unpack

	concatBinary = concat
	emptyBinary = null

fii :: Binary a => Int -> Int -> a
fii n = makeBinary . BSC.pack . intToBin LittleEndian n . fromIntegral
tii :: Binary a => Int -> a -> (Int, a)
tii _ str = let
	(t, d) = getBytes 4 str in
	(fromIntegral $ ti t, d)

instance Binary BS.ByteString where
	getBytes n = BS.take n &&& BS.drop n
	makeBinary = id

	concatBinary = BS.concat
	emptyBinary = (== 0) . BS.length

lintToBin = intToBin LittleEndian

intToBin :: Endian -> Int -> Integer -> String
intToBin LittleEndian 0 _ = ""
intToBin LittleEndian n x = chr (fromIntegral $ x `mod` 256) :
	intToBin LittleEndian (fromIntegral n - 1) (x `div` 256)
intToBin BigEndian n x = reverse $ intToBin LittleEndian n x

times :: Int -> (s -> (ret, s)) -> s -> ([ret], s)
times 0 _ s = ([], s)
times n f s = let
	(ret, rest) = f s
	(rets, rest') = times (n - 1) f rest in
	(ret : rets, rest')

whole :: Binary s => (s -> (ret, s)) -> s -> ([ret], s)
whole f s
	| empty s = ([], s)
	| otherwise = let
		(ret, rest) = f s
		(rets, rest') = whole f rest in
		(ret : rets, rest')
