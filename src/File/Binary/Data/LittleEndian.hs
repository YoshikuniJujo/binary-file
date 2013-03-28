{-# LANGUAGE TypeFamilies #-}

module File.Binary.Data.LittleEndian where

import Classes
import qualified Data.ByteString.Lazy.Char8 as BSLC

-- retTypeInt LittleEndian

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n s = (fromIntegral $ ti t, d)
		where
		(t, d) = getBytes n s
	toBinary n = makeBinary . BSLC.pack . reverse . lintToBin n . fromIntegral
