{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary
import File.Binary.Data.BigEndian
import System.Environment
import Data.Word

main = do
	[f] <- getArgs
	cnt <- readBinaryFile f
	let (png, rest) = readPNG cnt
	print $ png
	print $ map chankName $ chanks png

test = readPNG `fmap` readBinaryFile "tmp/out.png"

instance RetType Word32 where
	type Argument Word32 = Int
	fromType n = rev . fi n . fromIntegral
	toType n s = (fromIntegral $ ti $ rev $ tk n s, dp n s)

instance RetType Chank where
	type Argument Chank = ()
	fromType _ = writeChank
	toType _ = readChank

[binary|

PNG

1: 0x89
3: "PNG"
2: "\r\n"
1: "\SUB"
1: "\n"
((), Nothing)<[Chank]>: chanks

|]

data ChankBody
	= ChankIHDR IHDR
	| ChankGAMA GAMA
	| ChankSRGB SRGB
	| ChankCHRM CHRM
	| Others String
	deriving Show

instance RetType ChankBody where
	type Argument ChankBody = (Int, String)
	fromType _ (ChankIHDR ihdr) = writeIHDR ihdr
	fromType _ (ChankGAMA gama) = writeGAMA gama
	fromType _ (ChankSRGB srgb) = writeSRGB srgb
	fromType _ (ChankCHRM chrm) = writeCHRM chrm
	fromType (n, _) (Others str) = fromType ((), Just n) str
	toType (_, "IHDR") str = let (ihdr, rest) = readIHDR str in
		(ChankIHDR ihdr, rest)
	toType (_, "gAMA") str = let (gama, rest) = readGAMA str in
		(ChankGAMA gama, rest)
	toType (_, "sRGB") str = let (srgb, rest) = readSRGB str in
		(ChankSRGB srgb, rest)
	toType (_, "cHRM") str = let (chrm, rest) = readCHRM str in
		(ChankCHRM chrm, rest)
	toType (n, _) str = let (others, rest) = toType ((), Just n) str in
		(Others others, rest)

[binary|

Chank

4: chankSize
((), Just 4)<String>: chankName
(chankSize, chankName)<ChankBody>: chankData
-- ((), Just chankSize)<String>: chankData
4<Word32>:chankCRC

|]

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

4: chrm1
4: chrm2
4: chrm3
4: chrm4
4: chrm5
4: chrm6
4: chrm7
4: chrm8

|]

[binary|

PLTE

-- <Int>

-- ((), arg)<[(Int, Int, Int)]>: colors

|]
