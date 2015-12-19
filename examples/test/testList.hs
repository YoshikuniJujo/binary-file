{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary
import File.Binary.Instances
import File.Binary.Instances.LittleEndian

data Some = Some [Int] deriving Show

instance Field Some where
	type FieldArgument Some = [Int]
	fromBinary ns bs = return (Some ns, bs)

[binary|

TestList deriving Show

[1, 2, 4]{Some}: some
[1, 2, 4]{[Int]}: nums

|]
