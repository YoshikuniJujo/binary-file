{-# LANGUAGE
	TemplateHaskell,
	TypeSynonymInstances,
	FlexibleInstances,
	TypeFamilies,
	OverloadedStrings #-}

module Classes (
	Field(..),
	Binary(..),
	fii, fiiBE,
	tii, tiiBE,
	readInt
) where

import qualified Data.ByteString as BS
import Data.Char

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
	fromBinary _ str = (head $ ts str, dp 1 str)
	toBinary _ = fs . (: [])

instance Field BS.ByteString where
	type FieldArgument BS.ByteString = Int
	fromBinary n str = (tbs $ tk n str, dp n str)
	toBinary _ = fbs

class Binary a where
	tk :: Int -> a -> a
	dp :: Int -> a -> a
	ts :: a -> String
	fs :: String -> a
	fbs :: BS.ByteString -> a
	tbs :: a -> BS.ByteString
	ti :: a -> Integer
	fi :: Int -> Integer -> a
	tiBE :: a -> Integer
	fiBE :: Int -> Integer -> a
	cc :: [a] -> a
	zero :: a
	len :: a -> Int
	empty :: a -> Bool
	rev :: a -> a

instance Binary String where
	tk = take
	dp = drop
	ts = id
	fs = id
	fbs = ts
	tbs = fs
	ti = readInt LittleEndian
	fi = intToBin LittleEndian
	tiBE = readInt BigEndian
	fiBE = intToBin BigEndian
	cc = concat
	zero = "\0"
	len = length
	empty = null
	rev = reverse

fii, fiiBE :: Binary a => Int -> Int -> a
fii n = fi n . fromIntegral
fiiBE n = fiBE n . fromIntegral
tii, tiiBE :: Binary a => Int -> a -> (Int, a)
tii _ str = (fromIntegral $ ti $ tk 4 str, dp 4 str)
tiiBE _ str = (fromIntegral $ tiBE $ tk 4 str, dp 4 str)

instance Binary BS.ByteString where
	tk = BS.take
	dp = BS.drop
	ts = map (chr . fromIntegral) . BS.unpack
	fs = BS.pack . map (fromIntegral . ord)
	fbs = id
	tbs = id
	ti = readInt LittleEndian . map (chr . fromIntegral) . BS.unpack
	fi n = BS.pack . map (fromIntegral . ord) . intToBin LittleEndian n
	tiBE = readInt BigEndian . map (chr . fromIntegral) . BS.unpack
	fiBE n = BS.pack . map (fromIntegral . ord) . intToBin BigEndian n
	cc = BS.concat
	zero = BS.singleton 0
	len = BS.length
	empty = (== 0) . BS.length
	rev = BS.reverse

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
