

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
import Foreign.C.Types (CInt,CLLong)
import Control.Monad

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

ignoreExceptions doit = (doit >> return ()) `E.catch` (\(e::SomeException) -> return ())

foreign import ccall unsafe "HsMmap.h system_io_mmap_counters"
    c_system_io_counters :: IO CInt


content = BSC.pack "Memory mapping of files for POSIX and Windows"

test_normal_readonly = do
    BSC.writeFile "test_normal.bin" content
    bs <- mmapFileByteString "test_normal.bin" Nothing
    bs @?= content

test_normal_readonly_zero_length = do
    BSC.writeFile "test_zerolength.bin" BSC.empty
    bs <- mmapFileByteString "test_zerolength.bin" Nothing
    bs @?= BSC.empty

test_non_existing_readonly = do
    ignoreExceptions $ removeFile "test_notexists.bin"
    ignoreExceptions $ do
        mmapFileByteString "test_notexists.bin" Nothing
        assertFailure "Should throw exception"

test_no_permission_readonly = do
    let filename = "test_nopermission.bin"
    ignoreExceptions $ setPermissions filename (Permissions {readable = True, writable = True, executable = True, searchable = True})
    BSC.writeFile filename content
    setPermissions filename (Permissions {readable = False, writable = False, executable = False, searchable = False})
    Permissions {readable = readable} <- getPermissions filename
          -- no way to clear read flag under Windows, skip the test
    if not readable
        then ignoreExceptions $ do
                 mmapFileByteString filename Nothing
                 assertFailure "Should throw exception"
        else return ()

test_normal_negative_offset_readonly = do
    ignoreExceptions $ removeFile "test_normal1.bin"
    BSC.writeFile "test_normal1.bin" content
    ignoreExceptions $ do
        mmapFileByteString "test_normal1.bin" (Just (-20,5))
        assertFailure "Should throw exception"

test_normal_negative_size_readonly = do
    ignoreExceptions $ removeFile "test_normal2.bin"
    BSC.writeFile "test_normal2.bin" content
    ignoreExceptions $ do
        mmapFileByteString "test_normal2.bin" (Just (0,-5))
        assertFailure "Should throw exception"

test_normal_offset_size_readonly = do
    let filename = "test_normal5.bin"
    BSC.writeFile filename content
    bs <- mmapFileByteString filename (Just (5,5))
    let exp = BSC.take 5 (BSC.drop 5 content)
    bs @?= exp

test_normal_offset_size_zero_readonly = do
    let filename = "test_normal6.bin"
    BSC.writeFile filename content
    bs <- mmapFileByteString filename (Just (5,0))
    let exp = BSC.empty
    bs @?= exp

test_normal_offset_beyond_eof_readonly = do
    let filename = "test_normal6.bin"
    BSC.writeFile filename content
    ignoreExceptions $ do
        mmapFileByteString filename (Just (1000,5))
        assertFailure "Should throw exception"

test_normal_offset_plus_size_beyond_eof_readonly = do
    let filename = "test_normal7.bin"
    BSC.writeFile filename content
    ignoreExceptions $ do
        mmapFileByteString filename (Just (4,5000))
        assertFailure "Should throw exception"

test_counters_zero = do
    System.Mem.performGC
    threadDelay 1000
    counters <- c_system_io_counters
    return (counters @?= 0)

alltests = [ "Normal read only mmap" ~: test_normal_readonly
           , "Zero length file mmap" ~: test_normal_readonly_zero_length
           , "File does not exist" ~: test_non_existing_readonly
           , "No permission to read file" ~: test_no_permission_readonly
           , "Signal error when negative offset given" ~: test_normal_negative_offset_readonly
           , "Signal error when negative size given" ~: test_normal_negative_size_readonly
           , "Test if we can cut part of file" ~: test_normal_offset_size_readonly
           , "Test if we can cut zero length part of file" ~: test_normal_offset_size_zero_readonly
           , "Should throw error if mmaping readonly beyond end of file" ~: test_normal_offset_beyond_eof_readonly
           , "Should throw error if mmaping readonly with size beyond end of file" ~: test_normal_offset_plus_size_beyond_eof_readonly

           -- insert tests above this line
           , "Counters should be zero" ~: test_counters_zero
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