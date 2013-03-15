{-# LANGUAGE QuasiQuotes #-}

import QuoteBinaryStructure
import System.Environment

[binary|

BitmapFileHeader

2: 19778
4: fileSize
2: 0
2: 0
4: offset
4: 40
4: bitmapWidth
4: bitmapHeight

|]

main = do
	[fn] <- getArgs
	cnt <- readFile fn
	print $ readBitmapFileHeader cnt
