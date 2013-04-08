{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TypeFamilies #-}

import File.Binary
import File.Binary.Instances
import qualified File.Binary.Instances.LittleEndian as L
import qualified File.Binary.Instances.BigEndian as B

main = do
	let (bits :: Bits, rest) = fromBinary () "\x1b\xf0"
	print bits
	print rest
	let bin = toBinary () bits :: String
	print bin

[binary|

Bits deriving Show

2{L.BitsInt}: little1
2{L.BitsInt}: little2
2{B.BitsInt}: big1
3{B.BitsInt}: big2
3{B.BitsInt}: big3
{Bool}: bb0
{Bool}: bb1
{Bool}: bb2
{Bool}: bb3

|]
