{-# LANGUAGE TypeFamilies #-}

module File.Binary.Classes (Field(..), Binary(..)) where

import Data.Monoid (Monoid)
import Data.ByteString.Lazy (ByteString)

class Field r where
	type FieldArgument r
	fromBinary :: Binary s => FieldArgument r -> s -> (r, s)
	toBinary :: Binary s => FieldArgument r -> r -> s

class (Eq b, Monoid b) => Binary b where
	getBytes :: Int -> b -> (ByteString, b)
	makeBinary :: ByteString -> b
