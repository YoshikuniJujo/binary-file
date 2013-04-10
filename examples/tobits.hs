{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary
import File.Binary.Instances
import File.Binary.Instances.BigEndian
import System.Environment

main :: IO ()
main = putStrLn . showBits . fst . fromBinary () . head =<< getArgs

showBits :: Bits -> String
showBits (Bits bs) = unwords $ map (concatMap (show . (fromEnum :: Bool -> Int))) bs

[binary|

Bits deriving Show

(((), Just 8), Nothing){[[Bool]]}: bs

|]
