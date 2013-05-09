{-# LANGUAGE TypeFamilies #-}

module Data.Image (
	TwoDImage(..), Color(..)
) where

import Data.Word
import Control.Applicative

class TwoDImage i where
	type TwoDImageColor i
	new :: Monad m => TwoDImageColor i -> Int -> Int -> m i
	fromColorList :: Monad m => [[TwoDImageColor i]] -> m i
	toColorList :: (Monad m, Applicative m) => i -> m [[TwoDImageColor i]]
	getSize :: Monad m => i -> m (Int, Int)
	up, down, left, right, next, prev :: Monad m => i -> m i
	getXY :: Monad m => i -> m (Int, Int)
	setXY :: Monad m => i -> (Int, Int) -> m i
	getPixel :: Monad m => i -> m (TwoDImageColor i)
	setPixel :: Monad m => i -> TwoDImageColor i -> m i

	up i = do
		(x, y) <- getXY i
		if y > 0 then setXY i (x, pred y) else fail "can't go up"
	down i = do
		(_, h) <- getSize i
		(x, y) <- getXY i
		if y < h - 1 then setXY i (x, succ y) else fail "can't go down"
	left i = do
		(x, y) <- getXY i
		if x > 0 then setXY i (pred x, y) else fail "can't go left"
	right i = do
		(w, _) <- getSize i
		(x, y) <- getXY i
		if x < w - 1 then setXY i (succ x, y) else fail "can't go right"
	next i = do
		(w, h) <- getSize i
		(x, y) <- getXY i
		case (x < w - 1, y < h - 1) of
			(True, _) -> setXY i (x + 1, y)
			(_, True) -> setXY i (0, y + 1)
			(_, _) -> fail "can't go next"
	prev i = do
		(x, y) <- getXY i
		case (x > 0, y > 0) of
			(True, _) -> setXY i (x - 1, y)
			(_, True) -> setXY i (0, y - 1)
			(_, _) -> fail "can't go previous"

class Color c where
	fromRGB8 :: Word8 -> Word8 -> Word8 -> c
	toRGB8 :: c -> (Word8, Word8, Word8)
