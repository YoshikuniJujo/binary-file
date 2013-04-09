{-# LANGUAGE QuasiQuotes, TypeFamilies, ScopedTypeVariables #-}

import File.Binary
import File.Binary.Instances
import File.Binary.Instances.LittleEndian

main = do
	let (bits :: Bits, rest) = fromBinary () "hello"
	print bits
	print rest
	print (consToBits () bits ([], "") :: ([Bool], String))
	print (toBinary () bits :: String)

[binary|

Bits deriving Show

{Bool}: b0
{Bool}: b1
4{BitsInt}: b2345
{Bool}: b6
{Bool}: b7
{Bool}: c0
{Bool}: c1
{Bool}: c2
{Bool}: c3
{Bool}: c4
{Bool}: c5
{Bool}: c6
{Bool}: c7
((), Nothing){[Bool]}: rest

|]
