{-# LANGUAGE QuasiQuotes #-}

import File.Binary
import System.Environment
import Data.Word

main = do
	[f] <- getArgs
	cnt <- readBinaryFile f
	print $ readPNG cnt

instance RetType Word32 where
	fromType n = fiBE n . fromIntegral
	toType = fromIntegral . tiBE

instance RetType Integer where
	fromType n = fiBE n . fromIntegral
	toType = fromIntegral . tiBE

getTest :: IO PNG
getTest = readPNG `fmap` readBinaryFile "out.png"

[binary|

PNG

set big_endian

1: 0x89
3: "PNG"
2: "\r\n"
1: "\SUB"
1: "\n"

4: 13
-- 4: "IHDR"
17<String>: ihdr
4<Word32>: ihdrCRC

4: gamaSize
4: "gAMA"
gamaSize<String>: gama
4<Word32>:gamaCRC

repeat {

Chank

4: chankSize
4<String>: chankName
-- chankSize<String>: chankData
4<Word32>:chankCRC

}

|]
