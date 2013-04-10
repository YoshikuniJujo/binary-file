module CRC (crc, testCRC) where

import Data.Array
import Data.Bits
import Data.Char
import Data.Word
import Numeric

--------------------------------------------------------------------------------

crc :: String -> Word32
crc = xor 0xffffffff . foldl crc' 0xffffffff
	where
	crc' :: Word32 -> Char -> Word32
	crc' c x = table ! i `xor` shiftR c 8
		where
		i = (c `xor` fromIntegral (ord x)) .&. 0xff
	table :: Array Word32 Word32
	table = listArray (0, 255) $ map (\n -> foldl table' n [0 .. 7]) [0 .. 255]
	table' :: Word32 -> Int -> Word32
	table' c _
		| c .&. 1 == 0 = shiftR c 1
		| otherwise = xor 0xedb88320 $ shiftR c 1

testCRC :: IO ()
testCRC = p . crc =<< getContents
	where
	p :: Word32 -> IO ()
	p c = putStrLn $ "0x" ++ showHex c ""
