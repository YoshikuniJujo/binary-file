{-# LANGUAGE QuasiQuotes #-}

import File.Binary
import File.Binary.Data.BigEndian
import System.Environment

main = do
	[f] <- getArgs
	cnt <- readBinaryFile f
	print $ readPNG cnt

[binary|

PNG

1: 0x89
3: "PNG"
2: "\r\n"
1: "\SUB"
1: "\n"

|]
