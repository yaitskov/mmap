{-# OPTIONS_GHC -fglasgow-exts #-}

module System.IO.MMap 
(
     -- $mmap_intro
     -- * Memory mapped files
     mmapFilePtr,
     mmapFileForeignPtr,
     mmapFileByteString,
     
     Mode(..)
)
where

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

-- $mmap_intro
--
-- This module is an interface to mmap(2) system call under POSIX (Unix, Linux,
-- Mac OS X) and CreateFileMapping MapViewOfFile under Windows.
--
-- We can consider mmap as lazy IO pushed into the virtual memory
-- subsystem.
--
-- It is only safe to mmap a file if you know you are the sole user.
--
-- For more details about mmap, and its consequences, see:
--
-- * <http://opengroup.org/onlinepubs/009695399/functions/mmap.html>
--
-- * <http://www.gnu.org/software/libc/manual/html_node/Memory_002dmapped-I_002fO.html>
--
-- * <http://msdn2.microsoft.com/en-us/library/aa366781(VS.85).aspx>
--

-- | Mode of mapping. Three cases are supported.
data Mode = ReadOnly      -- ^ file is mapped read-only
          | ReadWrite     -- ^ file is mapped read-write
          | WriteCopy     -- ^ file is mapped read-write, but changes aren't propagated to disk
    deriving (Eq,Ord,Enum)

-- | The 'mmapFilePtr' function maps a file or device into memory,
-- returning a tripple containing pointer that accesses the mapped file,
-- the finalizer to run to unmap region and size of mmaped memory.
--
-- If the mmap fails for some reason, an error is thrown.
--
-- Memory mapped files will behave as if they were read lazily --
-- pages from the file will be loaded into memory on demand.
--
-- The storage manager is used to free the mapped memory. When
-- the garbage collector notices there are no further references to the
-- mapped memory, a call to munmap is made. It is not necessary to do
-- this yourself. In tight memory situations, it may be profitable to
-- use 'System.Mem.performGC' or 'finalizeForeignPtr' to force an unmap.
--
-- File must be created with correct attributes prior to mapping it
-- into memory.
--
-- If mode is 'ReadWrite' or 'WriteCopy', the returned memory region may
-- be written to with 'Foreign.Storable.poke' and friends.
--
-- Range specified may be 'Nothing', then whole file is maped. Otherwise
-- range should be 'Just (offset,size)' where offsets is the beginning byte
-- of file region to map and size tells its length. There are no alignment
-- requirements.
--
-- If range to map extends beyon end of file, it will be resized accordingly.
--
mmapFilePtr :: FilePath                -- ^ name of file to mmap
            -> Mode                    -- ^ access mode
            -> Maybe (Int64,Int)       -- ^ range to map, maps whole file if Nothing
            -> IO (Ptr a,IO (),Int)    -- ^ pointer, finalizer and size
mmapFilePtr filepath mode offsetsize = do
    bracket (mmapFileOpen filepath mode)
            (finalizeForeignPtr) mmap
    where
        mmap handle = do
            (offset,size) <- case offsetsize of
                Just (offset,size) -> return (offset,size)
                Nothing -> do
                    longsize <- withForeignPtr handle c_system_io_file_size
                    when (longsize > fromIntegral (maxBound :: Int)) $
                         error ("file is longer (" ++ show longsize ++ ") then maxBound :: Int")
                    return (0,fromIntegral longsize)
            withForeignPtr handle $ \handle -> do
                let align = offset `mod` fromIntegral c_system_io_granularity
                    offsetraw = offset - align
                    sizeraw = size + fromIntegral align
                ptr <- c_system_io_mmap_mmap handle (fromIntegral $ fromEnum mode) (fromIntegral offsetraw) (fromIntegral sizeraw)
                when (ptr == nullPtr) $
                    error "c_system_io_mmap_mmap returned NULL"
                let finalizer = c_system_io_mmap_munmap ptr (fromIntegral sizeraw)
                return (ptr `plusPtr` fromIntegral align,finalizer,fromIntegral size)

-- | Maps region of file and returns it as 'ForeignPtr'. See 'mmapFilePtr' for details.
mmapFileForeignPtr :: FilePath                     -- ^ name of file to map
                   -> Mode                         -- ^ access mode
                   -> Maybe (Int64,Int)            -- ^ range to map, maps whole file if Nothing
                   -> IO (ForeignPtr a,Int)        -- ^ foreign pointer to beginning of region and size
mmapFileForeignPtr filepath mode offsetsize = do
    (ptr,finalizer,size) <- mmapFilePtr filepath mode offsetsize
    foreignptr <- Foreign.Concurrent.newForeignPtr ptr finalizer
    return (foreignptr,size)


-- | Maps region of file and returns it as 'Data.ByteString.ByteString'.
-- File is mapped in in 'ReadOnly' mode. See 'mmapFilePtr' for details
--
-- Note: this operation may break referential transparency! If
-- any other process on the system changes the file when it is mapped
-- into Haskell, the contents of your 'ByteString' will change.
--
mmapFileByteString :: FilePath                     -- ^ name of file to map
                   -> Maybe (Int64,Int)            -- ^ range to map, maps whole file if Nothing
                   -> IO ByteString                -- ^ bytestring with file content
mmapFileByteString filepath offsetsize = do
    (ptr,finalizer,size) <- mmapFilePtr filepath ReadOnly offsetsize
    bytestring <- unsafePackCStringFinalizer ptr size finalizer
    return bytestring

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


