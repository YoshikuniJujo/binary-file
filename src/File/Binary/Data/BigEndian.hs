{-# LANGUAGE TypeFamilies #-}

module File.Binary.Data.BigEndian (
	lintToBin
) where

import Classes
import qualified Data.ByteString.Lazy.Char8 as BSLC

-- retTypeInt BigEndian

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n s = (fromIntegral $ ti $ BSLC.reverse $ fst $ getBytes n s, dp n s)
	toBinary n = makeBinary . BSLC.pack . reverse . lintToBin n . fromIntegral
