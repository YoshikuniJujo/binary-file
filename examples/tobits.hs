{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary (binary, Field(..))
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import System.Environment (getArgs)

--------------------------------------------------------------------------------

main :: IO ()
main = mapM_ (putStrLn . showBits . fst . fromBinary ()) =<< getArgs

showBits :: Bits -> String
showBits = unwords . map (concatMap $ show . (fromEnum :: Bool -> Int)) . bools

[binary|

Bits deriving Show

(((), Just 8), Nothing){[[Bool]]}: bools

|]
