
module System.IO.MMap where

import System.IO
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Concurrent
import System.Unsafe

data Mode = ReadOnly | ReadWrite | WriteCopy

newtype Handle = Handle (ForeignPtr ())

mmapFile :: FilePath -> Mode -> Maybe (Int64,Int) -> (ForeignPtr (),Int)
mmapFile filepath mode offset size = do
    bracket (mmapFileOpen filepath mode)
            (finalizeForeignPtr) mmap
    where
        mmap handle = do
            ptr <- c_mmap handle mode offset size
            newForeignPtrFinalizer ptr (c_unmap ptr size)

mmapFileByteString :: FilePath -> Maybe (Offset,Size) -> ByteString
mmapFileByteString filepath range = do
    (foreignptr,size) <- mmapFile filepath ReadOnly range
    unsafePackCStringFinalizer

{-
mmapFileLazy :: FilePath -> Mode -> Int64 -> Int64 -> [(ForeignPtr (),Int)]
mmapFileLazy filepath mode offset size = do
    handle <- mmapFileOpen filepath mode
    map (\(offset,size) -> unsafePerformIO (mmap handle offset size)) chunks
    where
        mmap handle offset size = do
            ptr <- c_mmap handle offset size
            newForeignPtrFinalizer ptr (c_unmap ptr size)
-}

--mmapFileRangeByteStringLazy :: FilePath -> Offset -> Size -> Lazy.ByteString
--mmapFileByteString :: FilePath -> ByteString
--mmapFileByteStringLazy :: FilePath -> Lazy.ByteString

mmapFileOpen :: FilePath -> Mode -> ForeignPtr ()

