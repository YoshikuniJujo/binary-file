{-# LANGUAGE TypeFamilies #-}

module Classes (Field(..), Binary(..)) where

import Data.ByteString.Lazy (ByteString)

class Field r where
	type FieldArgument r
	fromBinary :: Binary s => FieldArgument r -> s -> (r, s)
	toBinary :: Binary s => FieldArgument r -> r -> s

class Binary a where
	getBytes :: Int -> a -> (ByteString, a)
	makeBinary :: ByteString -> a
	concatBinary :: [a] -> a
	emptyBinary :: a -> Bool
