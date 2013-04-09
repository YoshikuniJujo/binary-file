module File.Binary (
	Field(..), Binary(..), binary,
	readBinaryFile, writeBinaryFile
 ) where

import File.Binary.Quote (Field(..), Binary(..), binary)
import System.IO (IOMode(..), withBinaryFile, openBinaryFile, hGetContents, hPutStr)

readBinaryFile :: FilePath -> IO String
readBinaryFile path = openBinaryFile path ReadMode >>= hGetContents

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile f = withBinaryFile f WriteMode . flip hPutStr
