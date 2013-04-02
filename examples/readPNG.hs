{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import File.Binary
import File.Binary.Instances()
import File.Binary.Instances.BigEndian
import System.Environment
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Codec.Compression.Zlib
import CRC (crc)
import Control.Applicative

main :: IO ()
main = do
	[fin, fout] <- getArgs
	cnt <- BS.readFile fin
	let (png, _) = fromBinary () cnt

	putStrLn $ take 1000 (show png) ++ "..."

	let	dat = makeData png
		decomp = decompress dat
		recomp = compressWith defaultCompressParams {
			compressLevel = bestCompression,
			compressWindowBits = WindowBits 10
		 } decomp
		newData = makeDataChank recomp
		newnew = png {
			chanks = headerChanks png ++ newData ++
				footerChanks png
		 }
	BS.writeFile fout $ toBinary () newnew
	print $ dat == recomp

headerChanks :: PNG -> [Chank]
headerChanks PNG{ chanks = cs } =
	filter ((`notElem` ["IEND", "IDAT"]) . chankName) cs

footerChanks :: PNG -> [Chank]
footerChanks PNG{ chanks = cs } = filter ((== "IEND") . chankName) cs

makeData :: PNG -> BSL.ByteString
makeData PNG{ chanks = cs } =
	BSL.concat $ map (idat . cidat . chankData) $
		filter ((== "IDAT") . chankName) cs

makeDataChank :: BSL.ByteString -> [Chank]
makeDataChank = map makeOneDataChank . BSL.toChunks

makeOneDataChank :: BS.ByteString -> Chank
makeOneDataChank bs = Chank {
	chankSize = fromIntegral $ BS.length bs,
	chankName = "IDAT",
	chankData = ChankIDAT $ IDAT $ BSL.fromChunks [bs],
	chankCRC = crc $ "IDAT" ++ BSC.unpack bs
 }

test :: IO PNG
test = fst . fromBinary () <$> readBinaryFile "tmp/sample.png"

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

instance Field Word32 where
	type FieldArgument Word32 = Int
	toBinary n = makeBinary . BSL.pack . intToWords n
	fromBinary n s = (fromIntegral $ toIntgr $ BSL.reverse t, d)
		where
		(t, d) = getBytes n s

instance Field BSL.ByteString where
	type FieldArgument BSL.ByteString = Int
	toBinary _ = makeBinary -- . BS.concat . BSL.toChunks
	fromBinary n s = (t, d) -- (BSL.fromChunks [t], d)
		where
		(t, d) = getBytes n s

toIntgr :: BSL.ByteString -> Integer
toIntgr = mkNum . map fromIntegral . BSL.unpack

mkNum :: [Integer] -> Integer
mkNum [] = 0
mkNum (x : xs) = x + 2 ^ (8 :: Integer) * mkNum xs

data ChankBody
	= ChankIHDR IHDR
	| ChankGAMA GAMA
	| ChankSRGB SRGB
	| ChankCHRM CHRM
	| ChankPLTE PLTE
	| ChankBKGD BKGD
	| ChankIDAT { cidat :: IDAT }
	| ChankTEXT TEXT
	| ChankIEND IEND
	| Others String
	deriving Show

instance Field ChankBody where
	type FieldArgument ChankBody = (Int, String)
	toBinary _ (ChankIHDR c) = toBinary () c
	toBinary _ (ChankGAMA c) = toBinary () c
	toBinary _ (ChankSRGB c) = toBinary () c
	toBinary (n, _) (ChankCHRM chrm) = toBinary n chrm
	toBinary (n, _) (ChankPLTE plte) = toBinary n plte
	toBinary _ (ChankBKGD c) = toBinary () c
	toBinary (n, _) (ChankIDAT c) = toBinary n c
	toBinary (n, _) (ChankTEXT c) = toBinary n c
	toBinary _ (ChankIEND c) = toBinary () c
	toBinary (n, _) (Others str) = toBinary ((), Just n) str
	fromBinary (_, "IHDR") str = let (ihdr, rest) = fromBinary () str in
		(ChankIHDR ihdr, rest)
	fromBinary (_, "gAMA") str = let (gama, rest) = fromBinary () str in
		(ChankGAMA gama, rest)
	fromBinary (_, "sRGB") str = let (c, rest) = fromBinary () str in
		(ChankSRGB c, rest)
	fromBinary (n, "cHRM") str = let (chrm, rest) = fromBinary n str in
		(ChankCHRM chrm, rest)
	fromBinary (n, "PLTE") str = let (plte, rest) = fromBinary n str in
		(ChankPLTE plte, rest)
	fromBinary (_, "bKGD") str = let (c, rest) = fromBinary () str in
		(ChankBKGD c, rest)
	fromBinary (n, "IDAT") str = let (c, rest) = fromBinary n str in
		(ChankIDAT c, rest)
	fromBinary (n, "tEXt") str = let (c, rest) = fromBinary n str in
		(ChankTEXT c, rest)
	fromBinary (_, "IEND") str = let (iend, rest) = fromBinary () str in
		(ChankIEND iend, rest)
	fromBinary (n, _) str = let (others, rest) = fromBinary ((), Just n) str in
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

arg :: Int

(4, Just (arg `div` 4))<[Int]>: chrms

|]

[binary|

PLTE

arg :: Int

((), Just (arg `div` 3))<[(Int, Int, Int)]>: colors

|]

instance Field (Int, Int, Int) where
	type FieldArgument (Int, Int, Int) = ()
	toBinary _ (b, g, r) = concatBinary [toBinary 1 b, toBinary 1 g, toBinary 1 r]
	fromBinary _ s = let
		(b, rest) = fromBinary 1 s
		(g, rest') = fromBinary 1 rest
		(r, rest'') = fromBinary 1 rest' in
		((b, g, r), rest'')

[binary|

BKGD

1: bkgd

|]

[binary|

IDAT

arg :: Int

arg<BSL.ByteString>: idat
--((), Just arg)<String>: idat

|]

[binary|

TEXT

arg :: Int

((), Just arg)<String>: text

|]

[binary|IEND|]
