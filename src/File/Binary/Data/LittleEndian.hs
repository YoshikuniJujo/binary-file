{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module File.Binary.Data.LittleEndian where

import Classes
import qualified Data.ByteString.Char8 as BSC

-- retTypeInt LittleEndian

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n s = (fromIntegral $ ti t, d)
		where
		(t, d) = getBytes n s
	toBinary n = makeBinary . BSC.pack . rev . lintToBin n . fromIntegral
