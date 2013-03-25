{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}

module Classes (
	RetType(..),
	Str(..),
	retTypeInt,
	fii, fiiBE,
	tii, tiiBE
) where

import qualified Data.ByteString as BS
import ParseBinaryStructure
import Data.Char
import Language.Haskell.TH

class RetType a where
	fromType :: Str b => Int -> a -> b
	toType :: Str b => b -> a
	defaultSize :: a -> Int

retTypeInt :: Endian -> DecQ
retTypeInt endian =
	instanceD (cxt []) (appT (conT ''RetType) (conT ''Int)) [dfii, dtii]
	where
	dfii = valD (varP 'fromType) (normalB $ fiiend) []
	dtii = valD (varP 'toType) (normalB $ tiiend) []
	sffx = case endian of
		LittleEndian -> ""
		BigEndian -> "BE"
	fiiend = varE $ mkName $ "fii" ++ sffx
	tiiend = varE $ mkName $ "tii" ++ sffx

{-
instance RetType Int where
	fromType = fii
	toType = tii
	defaultSize _ = 1
-}

instance RetType String where
	fromType _ = fs
	toType = ts
	defaultSize s = length s

instance RetType BS.ByteString where
	fromType _ = fbs
	toType = tbs
	defaultSize bs = BS.length bs

class Str a where
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

instance Str String where
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

fii, fiiBE :: Str a => Int -> Int -> a
fii n = fi n . fromIntegral
fiiBE n = fiBE n . fromIntegral
tii, tiiBE :: Str a => a -> Int
tii = fromIntegral . ti
tiiBE = fromIntegral . tiBE

instance Str BS.ByteString where
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

intToBin :: Endian -> Int -> Integer -> String
intToBin LittleEndian 0 _ = ""
intToBin LittleEndian n x = chr (fromIntegral $ x `mod` 256) :
	intToBin LittleEndian (fromIntegral n - 1) (x `div` 256)
intToBin BigEndian n x = reverse $ intToBin LittleEndian n x
