{-# OPTIONS_GHC -fglasgow-exts #-}

module System.IO.MMap where

import System.IO
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Concurrent
import System.IO.Unsafe
import Data.ByteString.Unsafe
import Data.Int
import Control.Monad
import Control.Exception
import Data.ByteString

data Mode = ReadOnly | ReadWrite | WriteCopy
    deriving (Eq,Ord,Enum)

mmapFilePtr :: FilePath -> Mode -> Maybe (Int64,Int) -> IO (Ptr (),IO (),Int)
mmapFilePtr filepath mode offsetsize = do
    bracket (mmapFileOpen filepath mode)
            (finalizeForeignPtr) mmap
    where
        mmap handle = do
            (offset,size) <- case offsetsize of
                Just (offset,size) -> return (offset,size)
                Nothing -> do
                    longsize <- withForeignPtr handle c_system_io_file_size
                    return (0,fromIntegral longsize)
            withForeignPtr handle $ \handle -> do
                let align = offset `mod` fromIntegral c_system_io_granularity
                    offsetraw = offset - align
                    sizeraw = size + fromIntegral align
                ptr <- c_system_io_mmap_mmap handle (fromIntegral $ fromEnum mode) (fromIntegral offsetraw) (fromIntegral sizeraw)
                when (ptr == nullPtr) $
                    error "c_system_io_mmap_mmap returned NULL"
                let finalizer = c_system_io_mmap_munmap ptr (fromIntegral size)
                return (ptr `plusPtr` fromIntegral align,finalizer,fromIntegral size)

mmapFileForeignPtr :: FilePath -> Mode -> Maybe (Int64,Int) -> IO (ForeignPtr (),Int)
mmapFileForeignPtr filepath mode offsetsize = do
    (ptr,finalizer,size) <- mmapFilePtr filepath mode offsetsize
    foreignptr <- Foreign.Concurrent.newForeignPtr ptr finalizer
    return (foreignptr,size)


mmapFileByteString :: FilePath -> Maybe (Int64,Int) -> IO ByteString
mmapFileByteString filepath offsetsize = do
    (ptr,finalizer,size) <- mmapFilePtr filepath ReadOnly offsetsize
    bytestring <- unsafePackCStringFinalizer (castPtr ptr) size finalizer
    return bytestring

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

mmapFileOpen :: FilePath -> Mode -> IO (ForeignPtr ())
mmapFileOpen filepath mode = do
    ptr <- withCString filepath $ \filepath -> c_system_io_mmap_file_open filepath (fromIntegral $ fromEnum mode)
    when (ptr == nullPtr) $
        error "c_system_io_mmap_file_open returned NULL"
    handle <- Foreign.ForeignPtr.newForeignPtr c_system_io_mmap_file_close ptr
    return handle

foreign import ccall unsafe "system_io_mmap_file_open" c_system_io_mmap_file_open :: CString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "&system_io_mmap_file_close" c_system_io_mmap_file_close :: FunPtr(Ptr () -> IO ())
foreign import ccall unsafe "system_io_mmap_mmap" c_system_io_mmap_mmap :: Ptr () -> CInt -> CLLong -> CInt -> IO (Ptr ())
foreign import ccall unsafe "system_io_mmap_munmap" c_system_io_mmap_munmap :: Ptr () -> CInt -> IO ()
foreign import ccall unsafe "system_io_mmap_file_size" c_system_io_file_size :: Ptr () -> IO (CLLong)
foreign import ccall unsafe "system_io_mmap_granularity" c_system_io_granularity :: CInt


