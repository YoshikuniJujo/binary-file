{-# LANGUAGE QuasiQuotes, TypeFamilies, PackageImports #-}

import File.Binary
import File.Binary.Instances
import File.Binary.Instances.BigEndian

import Control.Monad.Instances
import "monads-tf" Control.Monad.Error

main :: IO ()
main = do
--	let hoge = fromBinary () "hello" :: Maybe (TestBitsConst, String)
	(hoge, rest) <- fromBinary () "hello" :: IO (TestBitsConst, String)
	print hoge
	hage <- toBinary () hoge :: IO String
	print hage

[binary|

TestBitsConst deriving Show

1: some
1{BitsInt}: 0
{Bool}: True
6{BitsInt}: seven
1: other

|]
