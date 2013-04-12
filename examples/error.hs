{-# LANGUAGE QuasiQuotes, TypeFamilies, ScopedTypeVariables #-}

import File.Binary
import File.Binary.Instances
import File.Binary.Instances.LittleEndian

main = do
	let	good :: Either String (Some, String) = fromBinary () "good-bye"
		bad :: Either String (Some, String) = fromBinary () "bad-bye"
	print good
	print bad

[binary|

Some deriving Show

4: "good"
4: some

|]
