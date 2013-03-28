{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module File.Binary.Data.BigEndian where

import Classes

-- retTypeInt BigEndian

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n s = (fromIntegral $ ti $ rev $ fst $ getBytes n s, dp n s)
	toBinary n = rev . fi n . fromIntegral
