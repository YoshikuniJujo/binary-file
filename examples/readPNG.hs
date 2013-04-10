{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Prelude hiding (concat)
import File.Binary (binary, Field(..), Binary(..))
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import CRC (crc)
import Codec.Compression.Zlib (
	decompress, compressWith, defaultCompressParams, CompressParams(..),
	bestCompression, WindowBits(..))
import qualified Data.ByteString as BS (ByteString, length, readFile, writeFile)
import qualified Data.ByteString.Char8 as BSC (unpack)
import Data.ByteString.Lazy
	(ByteString, pack, unpack, concat, toChunks, fromChunks)
import Data.Word (Word8, Word32)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Data.Monoid (mconcat)
import Control.Applicative ((<$>))
import Control.Arrow(first)
import System.Environment (getArgs)

--------------------------------------------------------------------------------

main :: IO ()
main = do
	[fin, fout] <- getArgs
	png <- readPNG fin

	putStrLn $ take 800 (show png) ++ "..."

	let	dat = concatIDATs $ body png
		decomp = decompress dat
		recomp = compressWith defaultCompressParams {
			compressLevel = bestCompression,
			compressWindowBits = WindowBits 10
		 } decomp
		newIDAT = map makeIDAT $ toChunks recomp
		new = png { chunks = header png ++ newIDAT ++ footer png }
	writePNG fout new

	putStrLn ""
	putStrLn $ take 800 (show new) ++ "..."

readPNG :: FilePath -> IO PNG
readPNG fp = do
	(png, "") <- fromBinary () <$> BS.readFile fp
	return png

writePNG :: FilePath -> PNG -> IO ()
writePNG fout = BS.writeFile fout . toBinary ()

header :: PNG -> [Chunk]
header PNG{ chunks = cs } =
	filter ((`notElem` ["IEND", "IDAT"]) . chunkName) cs

body :: PNG -> [IDAT]
body PNG { chunks = cs } =
	map (cidat . chunkData) $ filter ((== "IDAT") . chunkName) cs

footer :: PNG -> [Chunk]
footer PNG{ chunks = cs } = filter ((== "IEND") . chunkName) cs

concatIDATs :: [IDAT] -> ByteString
concatIDATs = concat . map idat

makeIDAT :: BS.ByteString -> Chunk
makeIDAT bs = Chunk {
	chunkSize = fromIntegral $ BS.length bs,
	chunkName = "IDAT",
	chunkData = ChunkIDAT $ IDAT $ fromChunks [bs],
	chunkCRC = crc $ "IDAT" ++ BSC.unpack bs }

[binary|

PNG deriving Show

1: 0x89
3: "PNG"
2: "\r\n"
1: "\SUB"
1: "\n"
((), Nothing){[Chunk]}: chunks

|]

[binary|

Chunk deriving Show

4: chunkSize
((), Just 4){String}: chunkName
(chunkSize, chunkName){ChunkBody}: chunkData
4{Word32}:chunkCRC

|]

instance Field Word32 where
	type FieldArgument Word32 = Int
	toBinary n = makeBinary . pack . intToWords n
	fromBinary n = first (wordsToInt . unpack) . getBytes n

intToWords :: (Bits i, Integral i) => Int -> i -> [Word8]
intToWords = itw []
	where
	itw r 0 _ = r
	itw r n i = itw (fromIntegral (i .&. 0xff) : r) (n - 1) (i `shiftR` 8)

wordsToInt :: Bits i => [Word8] -> i
wordsToInt = foldl (\r w -> r `shiftL` 8 .|. fromIntegral w) 0

data ChunkBody
	= ChunkIHDR IHDR
	| ChunkGAMA GAMA
	| ChunkSRGB SRGB
	| ChunkCHRM CHRM
	| ChunkPLTE PLTE
	| ChunkBKGD BKGD
	| ChunkIDAT { cidat :: IDAT }
	| ChunkTEXT TEXT
	| ChunkIEND IEND
	| Others String
	deriving Show

instance Field ChunkBody where
	type FieldArgument ChunkBody = (Int, String)
	toBinary _ (ChunkIHDR c) = toBinary () c
	toBinary _ (ChunkGAMA c) = toBinary () c
	toBinary _ (ChunkSRGB c) = toBinary () c
	toBinary (n, _) (ChunkCHRM chrm) = toBinary n chrm
	toBinary (n, _) (ChunkPLTE plte) = toBinary n plte
	toBinary _ (ChunkBKGD c) = toBinary () c
	toBinary (n, _) (ChunkIDAT c) = toBinary n c
	toBinary (n, _) (ChunkTEXT c) = toBinary n c
	toBinary _ (ChunkIEND c) = toBinary () c
	toBinary (n, _) (Others str) = toBinary ((), Just n) str
	fromBinary (_, "IHDR") = first ChunkIHDR . fromBinary ()
	fromBinary (_, "gAMA") = first ChunkGAMA . fromBinary ()
	fromBinary (_, "sRGB") = first ChunkSRGB . fromBinary ()
	fromBinary (n, "cHRM") = first ChunkCHRM . fromBinary n
	fromBinary (n, "PLTE") = first ChunkPLTE . fromBinary n
	fromBinary (_, "bKGD") = first ChunkBKGD . fromBinary ()
	fromBinary (n, "IDAT") = first ChunkIDAT . fromBinary n
	fromBinary (n, "tEXt") = first ChunkTEXT . fromBinary n
	fromBinary (_, "IEND") = first ChunkIEND . fromBinary ()
	fromBinary (n, _) = first Others . fromBinary ((), Just n)

[binary|

IHDR deriving Show

4: width
4: height
1: depth
1: colorType
1: compressionType
1: filterType
1: interlaceType

|]

[binary|

GAMA deriving Show

4: gamma

|]

[binary|

SRGB deriving Show

1: srgb

|]

[binary|

CHRM

deriving Show

arg :: Int

(4, Just (arg `div` 4)){[Int]}: chrms

|]

[binary|

PLTE deriving Show

arg :: Int

((), Just (arg `div` 3)){[(Int, Int, Int)]}: colors

|]

instance Field (Int, Int, Int) where
	type FieldArgument (Int, Int, Int) = ()
	toBinary _ (b, g, r) = mconcat [toBinary 1 b, toBinary 1 g, toBinary 1 r]
	fromBinary _ s = let
		(r, rest) = fromBinary 1 s
		(g, rest') = fromBinary 1 rest
		(b, rest'') = fromBinary 1 rest' in
		((r, g, b), rest'')

[binary|

BKGD deriving Show

1: bkgd

|]

[binary|

IDAT deriving Show

arg :: Int

arg{ByteString}: idat

|]

[binary|

TEXT deriving Show

arg :: Int

((), Just arg){String}: text

|]

[binary|IEND deriving Show|]
