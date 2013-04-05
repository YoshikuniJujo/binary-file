{-# LANGUAGE QuasiQuotes, TypeFamilies, ScopedTypeVariables #-}

import File.Binary
import File.Binary.Instances
import File.Binary.Instances.LittleEndian
import System.Directory
import System.IO
import Control.Applicative
import Numeric
import System.Environment

size = 4

main = do
	n <- read . head <$> getArgs
	writeBinaryFile "tmp/out/tiny.dat" $ toBinary n $ Tiny 0x0123456789abcdef
	(t ::Tiny, rest) <- fromBinary n <$> readBinaryFile "tmp/out/tiny.dat"
	print t
	putStrLn $ flip showHex "" $ value t
	print rest

[binary|

Tiny deriving Show

arg :: Int

4: "tiny"
Main.size * arg{Integer}: value

|]
