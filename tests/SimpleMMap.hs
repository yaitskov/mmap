

module Main where

import System.IO.MMap
import Data.ByteString.Char8 as BSC

content = BSC.pack "Ala ma kota, kot ma Ale"
main = do
    BSC.writeFile "test.bin" content
    bs <- mmapFileByteString "test.bin" Nothing
    print (bs == content)
    BSC.putStrLn bs
