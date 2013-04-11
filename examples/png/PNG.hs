{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PNG(
	PNG,
	chunkName,

	readPNG,
	ihdr,
	bplte,
	plte,
	bidat,
	body,
	aplace,
	others,

	writePNG,
	IHDR(..),
	PLTE(..),
	png
) where

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
import Data.List (find)
import Control.Applicative ((<$>))
import Control.Arrow(first)

--------------------------------------------------------------------------------

readPNG :: FilePath -> IO PNG
readPNG fp = do
	(p, "") <- fromBinary () <$> BS.readFile fp
	return p

writePNG :: FilePath -> PNG -> IO ()
writePNG fout = BS.writeFile fout . toBinary ()

body :: PNG -> ByteString
body = decompress . concatIDATs . body'

ihdr :: PNG -> IHDR
ihdr p = let
	ChunkIHDR i = chunkData . head . filter ((== "IHDR") . chunkName) $ chunks p
	in i

plte :: PNG -> Maybe PLTE
plte p = do
	ChunkPLTE pl <- chunkData <$> find ((== "PLTE") . chunkName) (chunks p)
	return pl

mkBody :: ByteString -> [Chunk]
mkBody = map makeIDAT . toChunks . compressWith defaultCompressParams {
		compressLevel = bestCompression,
		compressWindowBits = WindowBits 10
	 }

png :: IHDR -> [Chunk] -> Maybe PLTE -> [Chunk] -> ByteString -> [Chunk] -> [Chunk] -> PNG
png i bp (Just p) bi b ap o =
	PNG { chunks = makeIHDR i : bp ++ makePLTE p : bi ++ mkBody b ++ o ++
		ap ++ [iend] }
png i bp Nothing bi b ap o =
	PNG { chunks = makeIHDR i : bp ++ bi ++ mkBody b ++ o ++ ap ++ [iend] }

beforePLTEs :: [String]
beforePLTEs = ["cHRM", "gAMA", "sBIT", "sRGB", "iCCP"]

bplte :: PNG -> [Chunk]
bplte = filter ((`elem` beforePLTEs) . chunkName) . chunks

beforeIDATs :: [String]
beforeIDATs = ["bKGD", "hIST", "tRNS", "pHYs", "sPLT", "oFFs", "pCAL", "sCAL"]

bidat :: PNG -> [Chunk]
bidat = filter ((`elem` beforeIDATs) . chunkName) . chunks

anyplaces :: [String]
anyplaces = ["tIME", "tEXt", "zTXt", "iTXt", "gIFg", "gIFt", "gIFx", "fRAc"]

aplace :: PNG -> [Chunk]
aplace = filter ((`elem` anyplaces) . chunkName) . chunks

needs :: [String]
needs = ["IHDR", "PLTE", "IDAT", "IEND"]

others :: PNG -> [Chunk]
others PNG { chunks = cs } =
	filter ((`notElem` needs ++ beforePLTEs ++ beforeIDATs ++ anyplaces) . chunkName) cs

body' :: PNG -> [IDAT]
body' PNG { chunks = cs } =
	map (cidat . chunkData) $ filter ((== "IDAT") . chunkName) cs

concatIDATs :: [IDAT] -> ByteString
concatIDATs = concat . map idat

makeIDAT :: BS.ByteString -> Chunk
makeIDAT bs = Chunk {
	chunkSize = fromIntegral $ BS.length bs,
	chunkName = "IDAT",
	chunkData = ChunkIDAT $ IDAT $ fromChunks [bs],
	chunkCRC = crc $ "IDAT" ++ BSC.unpack bs }

makeIHDR :: IHDR -> Chunk
makeIHDR = makeChunk . ChunkIHDR

makePLTE :: PLTE -> Chunk
makePLTE = makeChunk . ChunkPLTE

size :: ChunkBody -> Int
size (ChunkIHDR _) = 13
size (ChunkPLTE (PLTE d)) = 3 * length d
size _ = error "yet"

name :: ChunkBody -> String
name (ChunkIHDR _) = "IHDR"
name (ChunkPLTE _) = "PLTE"
name _ = error "yet"

makeChunk :: ChunkBody -> Chunk
makeChunk cb = Chunk {
	chunkSize = length (toBinary (size cb, name cb) cb :: String),
	chunkName = name cb,
	chunkData = cb,
	chunkCRC = crc $ name cb ++ toBinary (size cb, name cb) cb }

iend :: Chunk
iend = Chunk {
	chunkSize = 0,
	chunkName = "IEND",
	chunkData = ChunkIEND IEND,
	chunkCRC = crc "IEND" }

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
	toBinary (n, _) (ChunkPLTE p) = toBinary n p
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
