{-# LANGUAGE QuasiQuotes #-}

import QuoteBinaryStructure

[binary|

BitmapFileHeader

2: 19778
4: fileSize
2: 0
2: 0
4: offset

|]

main = do
	print BitmapFileHeader
