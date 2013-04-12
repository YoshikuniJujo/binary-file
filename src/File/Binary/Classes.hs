{-# LANGUAGE TypeFamilies, TupleSections #-}

module File.Binary.Classes (Field(..), Binary(..), pop, push) where

import Data.ByteString.Lazy (ByteString, unpack, singleton)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Monoid (Monoid, mappend, mempty)
import Control.Arrow (first, second)
import Control.Applicative

type AddBits b = ([Bool], b)

class Field f where
	type FieldArgument f
	fromBinary :: Binary b => FieldArgument f -> b -> Either String (f, b)
	toBinary :: Binary b => FieldArgument f -> f -> b
	fromBits :: Binary b => FieldArgument f -> AddBits b -> Either String (f, AddBits b)
	consToBits :: Binary b => FieldArgument f -> f -> AddBits b -> AddBits b

	fromBits a ([], b) = second ([] ,) <$> fromBinary a b
	fromBits _ _ = error "fromBits: not bytes (1 byte = 8 bits)"
	consToBits a f ([], b) = ([], toBinary a f `mappend` b)
	consToBits _ _ _ = error "consToBits: not bytes (1 byte = 8 bits)"

	fromBinary a b = do
		ret <- fromBits a ([], b)
		case ret of
			(f, ([], rest)) -> return (f, rest)
			_ -> Left "fromBinary: not bytes (1 byte = 8 bits)"
	toBinary a f = case consToBits a f ([], mempty) of
		([], bin) -> bin
		_ -> error "toBinary: not bytes (1 byte = 8 bits)"

pop :: Binary b => b -> AddBits b
pop = first (wtbs (8 :: Int) . head . unpack) . getBytes 1
	where
	wtbs 0 _ = []
	wtbs n w = toEnum (fromIntegral $ 1 .&. w) : wtbs (n - 1) (w `shiftR` 1)

push :: Binary b => AddBits b -> b
push = uncurry $ mappend . makeBinary . singleton . bstw
	where
	bstw = foldr (\b w -> w `shiftL` 1 .|. fromIntegral (fromEnum b)) 0

class (Eq b, Monoid b) => Binary b where
	getBytes :: Int -> b -> (ByteString, b)
	makeBinary :: ByteString -> b
