{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module File.Binary.Data.LittleEndian where

import Classes

-- retTypeInt LittleEndian

instance RetType Int where
	type Argument Int = Int
	fromType n = fi n . fromIntegral
	toType n s = (fromIntegral $ ti $ tk n s, dp n s)
