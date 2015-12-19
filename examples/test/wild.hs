{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary
import File.Binary.Instances
import File.Binary.Instances.BigEndian

[binary|

TestWild deriving Show

1: some1
1: some2
1: _
1: some3

|]
