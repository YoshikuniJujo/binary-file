{-# LANGUAGE QuasiQuotes #-}

import Here
import File.Binary.Parse

main :: IO ()
main = do
	putStrLn "ParseBinaryStructure"
	print $ parse [here|

BitmapFileHeader

2: 19778
4: fileSize
2: 0
2: 0
4: offset

4: 40
4: bitmapWidth
4: bitmapHeight
2: 1
2: bitPerPic
4: compress
4: imageDataSize
4: horizontalDensity
4: verticalDensity
4: colorIndexNumber
4: neededIndexNumber

(4, colorIndexNumber)<[Int]>: colors
4<(Int,Int,Int)>[colorIndexNumber]: colors
-- 1[3]: image
imageSize<ByteString>: image
10<String>: author
10<ByteString>: hoge
10<Some>: some
10: "abc\n\r\SUB"

|]
