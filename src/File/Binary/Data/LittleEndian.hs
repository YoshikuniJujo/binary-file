{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module File.Binary.Data.LittleEndian where

import Classes

-- retTypeInt LittleEndian

instance Field Int where
	type FieldArgument Int = Int
	fromBinary n s = (fromIntegral $ ti $ tk n s, dp n s)
	toBinary n = fi n . fromIntegral
