{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Prelude hiding (concat)
import File.Binary (binary, Field(..))
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.Instances.MSB0 ()
import CRC (crc)
import Codec.Compression.Zlib (
	decompress, compressWith, defaultCompressParams, CompressParams(..),
	bestCompression, WindowBits(..))
import qualified Data.ByteString as BS (ByteString, length, readFile, writeFile)
import qualified Data.ByteString.Char8 as BSC (unpack)
import Data.ByteString.Lazy
	(ByteString, concat, toChunks, fromChunks)
import Data.Word (Word32)
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
	Right (png, "") <- fromBinary () <$> BS.readFile fp
	return png

writePNG :: FilePath -> PNG -> IO ()
writePNG fout png = do
	let Right cnt = toBinary () png
	BS.writeFile fout cnt

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
-- ((), Nothing){[Chunk]}: chunks
repeat (){[Chunk]}: chunks

|]

[binary|

Chunk deriving Show

4: chunkSize
-- ((), Just 4){String}: chunkName
replicate 4 (){String}: chunkName
(chunkSize, chunkName){ChunkBody}: chunkData
4{Word32}:chunkCRC

|]

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
	toBinary (n, _) (Others str) = toBinary (replicate n ()) str -- toBinary ((), Just n) str
	fromBinary (_, "IHDR") = fmap (first ChunkIHDR) . fromBinary ()
	fromBinary (_, "gAMA") = fmap (first ChunkGAMA) . fromBinary ()
	fromBinary (_, "sRGB") = fmap (first ChunkSRGB) . fromBinary ()
	fromBinary (n, "cHRM") = fmap (first ChunkCHRM) . fromBinary n
	fromBinary (n, "PLTE") = fmap (first ChunkPLTE) . fromBinary n
	fromBinary (_, "bKGD") = fmap (first ChunkBKGD) . fromBinary ()
	fromBinary (n, "IDAT") = fmap (first ChunkIDAT) . fromBinary n
	fromBinary (n, "tEXt") = fmap (first ChunkTEXT) . fromBinary n
	fromBinary (_, "IEND") = fmap (first ChunkIEND) . fromBinary ()
	fromBinary (n, _) = fmap (first Others) . fromBinary (replicate n ()) -- ((), Just n)

[binary|

IHDR deriving Show

4: width
4: height
1: depth
: False
: False
: False
: False
: False
{Bool}: alpha
{Bool}: color
{Bool}: palet
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

-- (4, Just (arg `div` 4)){[Int]}: chrms
replicate (arg `div` 4) 4{[Int]}: chrms

|]

[binary|

PLTE deriving Show

arg :: Int

-- ((), Just (arg `div` 3)){[(Int, Int, Int)]}: colors
replicate (arg `div` 3) (){[(Int, Int, Int)]}: colors

|]

instance Field (Int, Int, Int) where
	type FieldArgument (Int, Int, Int) = ()
	toBinary _ (b, g, r) = do
		b' <- toBinary 1 b
		g' <- toBinary 1 g
		r' <- toBinary 1 r
		return $ mconcat [b', g', r']
	fromBinary _ s = do
		(r, rest) <- fromBinary 1 s
		(g, rest') <- fromBinary 1 rest
		(b, rest'') <- fromBinary 1 rest'
		return ((r, g, b), rest'')

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

-- ((), Just arg){String}: text
replicate arg (){String}: text

|]

[binary|IEND deriving Show|]
