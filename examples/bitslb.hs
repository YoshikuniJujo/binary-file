{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TypeFamilies #-}

import File.Binary (binary, Field(..))
import File.Binary.Instances ()
import qualified File.Binary.Instances.LittleEndian as L (BitsInt)
import qualified File.Binary.Instances.BigEndian as B (BitsInt)

--------------------------------------------------------------------------------

main :: IO ()
main = do
	let (bits :: Bits, "") = fromBinary () "he" -- "\x1b\xf0"
	print bits
	print (toBinary () bits :: String)

[binary|

Bits deriving Show

2{L.BitsInt}: little1
2{L.BitsInt}: little2
2{B.BitsInt}: big1
3{B.BitsInt}: big2
3{B.BitsInt}: big3
4{L.BitsInt} : little3
-- {Bool}: bb0
-- {Bool}: bb1
-- {Bool}: bb2
-- {Bool}: bb3

|]
