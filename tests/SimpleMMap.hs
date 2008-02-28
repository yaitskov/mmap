

module Main where

import System.IO.MMap
import Data.ByteString.Char8 as BSC
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr
import System.Mem
import Control.Concurrent
import Control.Exception as E


content = BSC.pack "Memory mapping of files for POSIX and Windows"

main = do
    BSC.writeFile "test.bin" content
    bs <- mmapFileByteString "test.bin" Nothing
    BSC.putStrLn bs
    print (bs == content)
    bs2 <- mmapFileByteString "test.bin" (Just (5,5))
    print (bs2 == BSC.take 5 (BSC.drop 5 content))

    -- create 5 gigabyte file
    let l = 1024*1024*1024*5
    (f,s) <- mmapFileForeignPtr "test.bin" ReadWrite (Just (l,5))
    withForeignPtr f $ \f -> poke (castPtr f) (64::Word8)

    E.catch (do
              bs3 <- mmapFileByteString "test.bin" Nothing
              print (fromIntegral l==BSC.length bs3 + 5 ))
          (\e -> print True -- exception here is also ok
          )
    System.Mem.performGC
    threadDelay 10000

