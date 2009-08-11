

module Main where

import System.IO.MMap
import Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr
import System.Mem
import Control.Concurrent
import Control.Exception as E
import Test.HUnit
import System.Directory

{-

Things to test:

1. Opening an existing file.
2. Opening non exisitng file.
3. Opening a file we don't have rights to.
4. Opening read only file for writting.
5. Opening zero lenght file read only
6. Opening zero lenght file read write
7. Extending file size.
8. MMaping only part of file.
9. MMaping negative offset.
10. Mmaping beyond end of file without extending
11. Mmaping 3GB file.
12. Mmaping 5GB file under 32bit (fail)
13. Mmaping 5GB file under 32bit (success)

-}

content = BSC.pack "Memory mapping of files for POSIX and Windows"

test_normal_readonly = do
    System.Mem.performGC
    BSC.writeFile "test.bin" content
    bs <- mmapFileByteString "test.bin" Nothing
    return (bs @?= content)

test_normal_readonly_zero_length = do
    System.Mem.performGC
    let content = BSC.pack ""
    BSC.writeFile "test1.bin" content
    bs <- mmapFileByteString "test1.bin" Nothing
    return (bs @?= content)

test_non_existing_readonly = do
    System.Mem.performGC
    removeFile "testx.bin" `E.catch` (\e -> return undefined)
    (do mmapFileByteString "testx.bin" Nothing
        return $ assertFailure "Should throw exception")
        `E.catch` (\e -> return (return ()))

alltests = [ test_normal_readonly
           , test_normal_readonly_zero_length
           , test_non_existing_readonly
           ]

main = do
    runTestTT (test alltests)

{-
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
    bs4 <- mmapFileByteStringLazy "test.bin" Nothing
    print (BSL.fromChunks [content] == BSL.take (fromIntegral $ BSC.length content) bs4)
    bs5 <- mmapFileByteStringLazy "test.bin" (Just (5,5))
    print (BSC.take 5 (BSC.drop 5 content) == BSC.concat (BSL.toChunks bs5))

    System.Mem.performGC
    threadDelay 10000

-}