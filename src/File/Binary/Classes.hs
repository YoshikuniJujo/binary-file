{-# LANGUAGE TypeFamilies, TupleSections, OverloadedStrings #-}

module File.Binary.Classes (Field(..), Binary(..), pop, push) where

import Data.ByteString.Lazy (ByteString, unpack, singleton, cons')
import qualified Data.ByteString.Lazy.Char8 as BSLC ()
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Monoid (Monoid, mappend, mempty)
import Data.Word
import Control.Arrow (first, second)
import Control.Applicative
import Control.Monad

type AddBits b = ([Bool], b)

class Field f where
	type FieldArgument f
	fromBinary :: (Binary b, Functor m, Monad m) => FieldArgument f -> b -> m (f, b)
	toBinary :: (Binary b, Functor m, Monad m) => FieldArgument f -> f -> m b
	fromBits :: (Binary b, Functor m, Monad m) =>
		FieldArgument f -> AddBits b -> m (f, AddBits b)
	consToBits :: (Binary b, Functor m, Monad m) =>
		FieldArgument f -> f -> AddBits b -> m (AddBits b)

	fromBits a ([], b) = second ([] ,) `liftM` fromBinary a b
	fromBits _ _ = error "fromBits: not bytes (1 byte = 8 bits)"
	consToBits a f ([], b) = do
		ret <- (`mappend` b) <$> toBinary a f
		return ([], ret)
	consToBits _ _ _ = fail "consToBits: not bytes (1 byte = 8 bits)"

	fromBinary a b = do
		ret <- fromBits a ([], b)
		case ret of
			(f, ([], rest)) -> return (f, rest)
			_ -> fail "fromBinary: not bytes (1 byte = 8 bits)"
	toBinary a f = do
		ret <- consToBits a f ([], mempty)
		case ret of
			([], bin) -> return bin
			_ -> fail "toBinary: not bytes (1 byte = 8 bits)"

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
	spanBytes :: (Word8 -> Bool) -> b -> (ByteString, b)
	unconsByte :: b -> (Word8, b)
	makeBinary :: ByteString -> b

	getBytes 0 b = ("", b)
	getBytes n b = let
		(h, t) = unconsByte b
		(r, b') = getBytes (n - 1) t in
		(h `cons'` r, b')

	spanBytes p b
		| b == makeBinary "" = ("", b)
		| p h = let (ret, rest) = spanBytes p t in (h `cons'` ret, rest)
		| otherwise = ("", b)
		where
		(h, t) = unconsByte b
