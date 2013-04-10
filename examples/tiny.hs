{-# LANGUAGE QuasiQuotes, TypeFamilies, ScopedTypeVariables #-}

import File.Binary (binary, Field(..), readBinaryFile, writeBinaryFile)
import File.Binary.Instances ()
-- import File.Binary.Instances.LittleEndian ()
import File.Binary.Instances.BigEndian ()
import Control.Applicative ((<$>))
import Numeric (showHex)
import System.Environment (getArgs)

--------------------------------------------------------------------------------

size :: Int
size = 2

main :: IO ()
main = do
	n <- read . head <$> getArgs
	writeBinaryFile "tmp/out/tiny.dat" $ toBinary n $ Tiny 0x0123456789abcdef
	(t ::Tiny, "") <- fromBinary n <$> readBinaryFile "tmp/out/tiny.dat"
	print t
	putStrLn $ flip showHex "" $ value t

[binary|

Tiny deriving Show

arg :: Int

4: "tiny"
: True
: False
: False
: False
: True
: False
: False
: True
Main.size * arg{Integer}: value

|]
