module File.Binary (
	readBinaryFile,
	writeBinaryFile,
	binary,
	Field(..),
	Binary(..),
	tii, -- tiiBE,
	fii, -- fiiBE
--	times
 ) where

import QuoteBinaryStructure
import System.IO

readBinaryFile :: FilePath -> IO String
readBinaryFile path = openBinaryFile path ReadMode >>= hGetContents

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile path str = do
	h <- openBinaryFile path WriteMode
	hPutStr h str
	hClose h
