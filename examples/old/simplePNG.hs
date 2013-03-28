{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary
import File.Binary.Data.BigEndian
import System.Environment
import Data.Word

main = do
	[f] <- getArgs
	cnt <- readBinaryFile f
	print $ readPNG cnt

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

[binary|

Chank

4: chankSize
((), Just 4)<String>: chankName
((), Just chankSize)<String>: chankData
4<Word32>:chankCRC

|]
