{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module File.Binary.Data.BigEndian (
	lintToBin
) where

import Classes
import qualified Data.ByteString.Char8 as BSC

-- retTypeInt BigEndian

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n s = (fromIntegral $ ti $ BSC.reverse $ fst $ getBytes n s, dp n s)
	toBinary n = makeBinary . BSC.pack . reverse . lintToBin n . fromIntegral
