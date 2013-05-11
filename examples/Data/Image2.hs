{-# LANGUAGE TypeFamilies #-}

module Data.Image2 (TwoDImage(..), Color(..)) where

import Data.Word

class TwoDImage i where
	type TwoDImageMonad i :: * -> *
	type TwoDImageColor i
	type TwoDImageInput i
	type TwoDImageOutput i

	toTwoDImage :: TwoDImageInput i -> TwoDImageMonad i i
	fromTwoDImage :: i -> TwoDImageMonad i (TwoDImageOutput i)

	makeTwoDImage :: Int -> Int ->
		(Int -> Int -> TwoDImageColor i) -> TwoDImageMonad i i
	getTwoDImage :: i ->
		TwoDImageMonad i ((Int, Int), [((Int, Int), TwoDImageColor i)])

	up, down, left, right :: i -> TwoDImageMonad i i
	getXY :: i -> TwoDImageMonad i (Int, Int)
	setXY :: i -> Int -> Int -> TwoDImageMonad i i

class Color c where
	fromRGB8 :: Word8 -> Word8 -> Word8 -> c
	toRGB8 :: c -> (Word8, Word8, Word8)
