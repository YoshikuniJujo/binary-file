{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances #-}

import File.Binary
import File.Binary.Data.BigEndian
import System.Environment
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Codec.Compression.Zlib
import CRC (crc)

main = do
	[fin, fout] <- getArgs
--	cnt <- readBinaryFile fin
	cnt <- BS.readFile fin
	let (png, rest) = readPNG () cnt
	print $ png
--	writeBinaryFile fout $ writePNG () png
--	BS.writeFile fout $ writePNG () png

	let	ChankIDAT idt = chankData $ chanks png !! 6
		dat = idat idt
		dec = decompress $ BSL.pack dat
		recomp = compressWith defaultCompressParams {
			compressLevel = BestCompression,
			compressWindowBits = WindowBits 10
		 } dec
	print $ length dat
	print $ length $ BSL.unpack recomp

	let	chank6 = chanks png !! 6
		newDat = chank6 {
			chankSize = length $ BSL.unpack recomp,
			chankData = ChankIDAT $ IDAT $ BSL.unpack recomp,
			chankCRC = crc $ "IDAT" ++ BSL.unpack recomp
		 }
		new = png {
			chanks = take 6 (chanks png) ++ [newDat] ++
				drop 7 (chanks png)
		 }
	print new

	print $ dat == BSL.unpack recomp

	BS.writeFile fout $ writePNG () new

test = readPNG () `fmap` readBinaryFile "tmp/out.png"

[binary|

PNG

1: 0x89
3: "PNG"
2: "\r\n"
1: "\SUB"
1: "\n"
((), Nothing)<[Chank]>: chanks

|]

[binary|

Chank

4: chankSize
((), Just 4)<String>: chankName
(chankSize, chankName)<ChankBody>: chankData
4<Word32>:chankCRC

|]

instance RetType Word32 where
	type Argument Word32 = Int
	fromType n = rev . fi n . fromIntegral
	toType n s = (fromIntegral $ ti $ rev $ tk n s, dp n s)

data ChankBody
	= ChankIHDR IHDR
	| ChankGAMA GAMA
	| ChankSRGB SRGB
	| ChankCHRM CHRM
	| ChankPLTE PLTE
	| ChankBKGD BKGD
	| ChankIDAT IDAT
	| ChankTEXT TEXT
	| ChankIEND IEND
	| Others String
	deriving Show

instance RetType ChankBody where
	type Argument ChankBody = (Int, String)
	fromType _ (ChankIHDR ihdr) = fromType () ihdr
	fromType _ (ChankGAMA gama) = fromType () gama
	fromType _ (ChankSRGB srgb) = fromType () srgb
	fromType (n, _) (ChankCHRM chrm) = fromType n chrm
	fromType (n, _) (ChankPLTE plte) = fromType n plte
	fromType _ (ChankBKGD bkgd) = fromType () bkgd
	fromType (n, _) (ChankIDAT idat) = fromType n idat
	fromType (n, _) (ChankTEXT text) = fromType n text
	fromType _ (ChankIEND iend) = fromType () iend
	fromType (n, _) (Others str) = fromType ((), Just n) str
	toType (_, "IHDR") str = let (ihdr, rest) = toType () str in
		(ChankIHDR ihdr, rest)
	toType (_, "gAMA") str = let (gama, rest) = toType () str in
		(ChankGAMA gama, rest)
	toType (_, "sRGB") str = let (srgb, rest) = toType () str in
		(ChankSRGB srgb, rest)
	toType (n, "cHRM") str = let (chrm, rest) = toType n str in
		(ChankCHRM chrm, rest)
	toType (n, "PLTE") str = let (plte, rest) = toType n str in
		(ChankPLTE plte, rest)
	toType (_, "bKGD") str = let (bkgd, rest) = toType () str in
		(ChankBKGD bkgd, rest)
	toType (n, "IDAT") str = let (idat, rest) = toType n str in
		(ChankIDAT idat, rest)
	toType (n, "tEXt") str = let (text, rest) = toType n str in
		(ChankTEXT text, rest)
	toType (_, "IEND") str = let (iend, rest) = toType () str in
		(ChankIEND iend, rest)
	toType (n, _) str = let (others, rest) = toType ((), Just n) str in
		(Others others, rest)

[binary|

IHDR

4: width
4: height
1: depth
1: colorType
1: compressionType
1: filterType
1: interlaceType

|]

[binary|

GAMA

4: gamma

|]

[binary|

SRGB

1: srgb

|]

[binary|

CHRM

<Int>

(4, Just (arg `div` 4))<[Int]>: chrms

|]

[binary|

PLTE

<Int>

((), Just (arg `div` 3))<[(Int, Int, Int)]>: colors

|]

instance RetType (Int, Int, Int) where
	type Argument (Int, Int, Int) = ()
	fromType _ (b, g, r) = cc [fromType 1 b, fromType 1 g, fromType 1 r]
	toType _ s = let
		(b, rest) = toType 1 s
		(g, rest') = toType 1 rest
		(r, rest'') = toType 1 rest' in
		((b, g, r), rest'')

[binary|

BKGD

1: bkgd

|]

[binary|

IDAT

<Int>

((), Just arg)<String>: idat
--arg<BS.ByteString>: idat

|]

[binary|

TEXT

<Int>

((), Just arg)<String>: text

|]

[binary|IEND|]
