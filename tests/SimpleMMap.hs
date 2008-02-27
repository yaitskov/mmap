

module Main where

import System.IO.MMap
import Data.ByteString.Char8 as BSC

content = BSC.pack "Memory mapping of files for POSIX and Windows"
main = do
{-
    BSC.writeFile "test.bin" content
    bs <- mmapFileByteString "test.bin" Nothing
    BSC.putStrLn bs
    print (bs == content)
    bs2 <- mmapFileByteString "test.bin" (Just (5,5))
    print (bs2 == BSC.take 5 (BSC.drop 5 content))
-}    
    -- create gigabyte file
    mmapFileForeignPtr "test.bin" ReadWrite (Just (1000000000,5))

