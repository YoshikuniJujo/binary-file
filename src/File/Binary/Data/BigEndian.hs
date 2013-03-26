{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module File.Binary.Data.BigEndian where

import Classes

-- retTypeInt BigEndian

instance RetType Int where
	type Argument Int = Int
	fromType n = rev . fi n . fromIntegral
	toType n s = (fromIntegral $ ti $ rev $ tk n s, dp n s)
