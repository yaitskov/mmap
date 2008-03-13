

module Main where

import System.IO.MMap
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr
import System.Mem
import Control.Concurrent
import Control.Exception as E
import Text.Printf
import Control.Exception
import System.CPUTime
import System.Environment
import System.IO
import Data.ByteString.Internal


time :: String -> IO t -> IO t
time txt a = do
    printf "%s: " txt
    start <- getCPUTime
    v <- a
    v `seq` return ()
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "%0.3f sec\n" (diff :: Double)
    return v


main = do
    [filename] <- getArgs

    time "Prelude copy" $ do
        hin <- openBinaryFile filename ReadMode
        hout <- openBinaryFile (filename ++ ".prelude") WriteMode
        c <- hGetContents hin
        hPutStr hout c
        hClose hin
        hClose hout

    time "ByteString copy" $ do
        hin <- openBinaryFile filename ReadMode
        hout <- openBinaryFile (filename ++ ".bytestring") WriteMode
        c <- BSC.hGetContents hin
        BSC.hPutStr hout c
        hClose hin
        hClose hout

    time "ByteString.Lazy copy" $ do
        hin <- openBinaryFile filename ReadMode
        hout <- openBinaryFile (filename ++ ".bytestringlazy") WriteMode
        c <- BSL.hGetContents hin
        BSL.hPutStr hout c
        hClose hin
        hClose hout

    time "MMap copy" $ do
        (mmapin,fin1,size) <- mmapFilePtr filename ReadOnly Nothing
        let newname = filename ++ ".mmap"
        writeFile newname ""
        (mmapout,fin2,size) <- mmapFilePtr newname ReadWrite (Just (0,size))
        memcpy mmapout mmapin (fromIntegral size)
        fin1
        fin2

    time "MMap copy lazy" $ do
        chunksin <- mmapFileByteStringLazy filename Nothing
        let newname = filename ++ ".mmaplazy"
        hout <- openBinaryFile newname WriteMode
        BSL.hPutStr hout chunksin

        {-
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
