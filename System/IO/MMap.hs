
module System.IO.MMap
(
     -- $mmap_intro

     -- * Memory mapped files strict interface
     mmapFilePtr,
     mmapFileForeignPtr,
     mmapFileByteString,

     munmapFilePtr,
     munmapFilePtrFinalizer,

     -- * Memory mapped files lazy interface
     mmapFilePtrLazy,
     mmapFileForeignPtrLazy,
     mmapFileByteStringLazy,

     -- * Mapping mode
     Mode(..)
)
where

import System.IO ()
import Foreign.Ptr (Ptr,FunPtr,nullPtr,plusPtr,minusPtr,castPtr)
import Foreign.C.Types (CInt,CLLong)
import Foreign.C.String (CString,withCString)
import Foreign.ForeignPtr (ForeignPtr,withForeignPtr,finalizeForeignPtr,newForeignPtr,newForeignPtrEnv,newForeignPtr_)
import Foreign.Storable( poke )
import Foreign.Marshal.Alloc( malloc, mallocBytes, free )
import Foreign.C.Error ( throwErrno )
import qualified Foreign.Concurrent( newForeignPtr )
import System.IO.Unsafe  (unsafePerformIO)
import qualified Data.ByteString.Internal as BS (fromForeignPtr)
import Data.Int (Int64)
import Control.Monad  (when)
import Control.Exception   (bracket)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BSL  (ByteString,fromChunks)

-- $mmap_intro
--
-- This module is an interface to mmap(2) system call under POSIX (Unix, Linux,
-- Mac OS X) and CreateFileMapping,MapViewOfFile under Windows.
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
-- Range specified may be 'Nothing', then whole file is mapped. Otherwise
-- range should be 'Just (offset,size)' where offsets is the beginning byte
-- of file region to map and size tells its length. There are no alignment
-- requirements.
--
-- Range to map must not extend beyond end of file in ReadOnly or WriteCopy mode.
-- In ReadWrite mode file will be extended as needed to cover offset plus size.
-- File will never be truncated.
--
mmapFilePtr :: FilePath                     -- ^ name of file to mmap
            -> Mode                         -- ^ access mode
            -> Maybe (Int64,Int)            -- ^ range to map, maps whole file if Nothing
            -> IO (Ptr a,Int,Int,Int)       -- ^ pointer to data, size, pointer to beginning, raw size of mmap
mmapFilePtr filepath mode offsetsize = do
    bracket (mmapFileOpen filepath mode)
            (finalizeForeignPtr) mmap
    where
        mmap handle = withForeignPtr handle $ \handle -> do
            longsize <- c_system_io_file_size handle >>= \x -> return (fromIntegral x)
            (offset,size) <- case offsetsize of
                Just (offset,size) -> do
                    when (size<0) $
                         throwErrno $ "mmap of '" ++ filepath ++ "' failed, negative size reguested"
                    when (offset<0) $
                         throwErrno $ "mmap of '" ++ filepath ++ "' failed, negative offset reguested"
                    when (longsize<offset || longsize<(offset + fromIntegral size)) $
                         throwErrno $ "mmap of '" ++ filepath ++ "' failed, offset and size beyond end of file"
                    return (offset,size)
                Nothing -> do
                    when (longsize > fromIntegral (maxBound :: Int)) $
                         fail ("file is longer (" ++ show longsize ++ ") than maxBound::Int")
                    return (0,fromIntegral longsize)
            let align = offset `mod` fromIntegral c_system_io_granularity
            let offsetraw = offset - align
            let sizeraw = size + fromIntegral align
            ptr <- c_system_io_mmap_mmap handle (fromIntegral $ fromEnum mode) (fromIntegral offsetraw) (fromIntegral sizeraw)
            when (ptr == nullPtr) $
                  throwErrno $ "mmap of '" ++ filepath ++ "' failed"
            return (castPtr ptr,sizeraw,fromIntegral align,size)

-- | Maps region of file and returns it as 'ForeignPtr'. See 'mmapFilePtr' for details.
mmapFileForeignPtr :: FilePath                     -- ^ name of file to map
                   -> Mode                         -- ^ access mode
                   -> Maybe (Int64,Int)            -- ^ range to map, maps whole file if Nothing
                   -> IO (ForeignPtr a,Int,Int)    -- ^ foreign pointer to beginning of raw region, offset to your data and size of your data
mmapFileForeignPtr filepath mode range = do
  (rawptr,rawsize,offset,size) <- mmapFilePtr filepath mode range
  let rawsizeptr = castIntToPtr rawsize
  foreignptr <- newForeignPtrEnv c_system_io_mmap_munmap_funptr rawsizeptr rawptr
  return (foreignptr,offset,size)

-- | Maps region of file and returns it as 'Data.ByteString.ByteString'.
-- File is mapped in in 'ReadOnly' mode. See 'mmapFilePtr' for details
--
-- Note: this operation may break referential transparency! If
-- any other process on the system changes the file when it is mapped
-- into Haskell, the contents of your 'Data.ByteString.ByteString' may change.
--
mmapFileByteString :: FilePath                     -- ^ name of file to map
                   -> Maybe (Int64,Int)            -- ^ range to map, maps whole file if Nothing
                   -> IO BS.ByteString             -- ^ bytestring with file contents
mmapFileByteString filepath range = do
    (foreignptr,offset,size) <- mmapFileForeignPtr filepath ReadOnly range
    let bytestring = BS.fromForeignPtr foreignptr offset size
    return bytestring

-- | The 'mmapFilePtrLazy' function maps a file or device into memory,
-- returning a list of tripples containing pointer that accesses the mapped file,
-- the finalizer to run to unmap that region and size of mapped memory.
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
-- Range specified may be 'Nothing', then whole file is mapped. Otherwise
-- range should be 'Just (offset,size)' where offsets is the beginning byte
-- of file region to map and size tells its length. There are no alignment
-- requirements.
--
-- If range to map extends beyond end of file, it will be resized accordingly.
--
mmapFilePtrLazy :: FilePath                -- ^ name of file to mmap
            -> Mode                        -- ^ access mode
            -> Maybe (Int64,Int64)         -- ^ range to map, maps whole file if Nothing
            -> IO [(Ptr a,Int,Int,Int)]    -- ^ list of pointer, finalizer and size
mmapFilePtrLazy filepath mode offsetsize = do
    bracket (mmapFileOpen filepath mode)
            (finalizeForeignPtr) mmap
    where
        mmap handle = do
            (offset,size) <- case offsetsize of
                Just (offset,size) -> return (offset,size)
                Nothing -> do
                    longsize <- withForeignPtr handle c_system_io_file_size
                    return (0,fromIntegral longsize)
            return $ map (mapChunk handle) (chunks offset size)
        mapChunk handle (offset,size) = unsafePerformIO $
            withForeignPtr handle $ \handle -> do
                let align = offset `mod` fromIntegral c_system_io_granularity
                    offsetraw = offset - align
                    sizeraw = size + fromIntegral align
                ptr <- c_system_io_mmap_mmap handle (fromIntegral $ fromEnum mode) (fromIntegral offsetraw) (fromIntegral sizeraw)
                when (ptr == nullPtr) $
                     throwErrno $ "mmap of '" ++ filepath ++ "' failed"
                let sizeptr = castIntToPtr size
                let finalizer = c_system_io_mmap_munmap sizeptr ptr
                return (ptr,sizeraw,fromIntegral align,size)

chunks :: Int64 -> Int64 -> [(Int64,Int)]
chunks offset 0 = []
chunks offset size | size <= fromIntegral chunkSize = [(offset,fromIntegral size)]
                   | otherwise = let offset2 = offset + fromIntegral chunkSize `div` fromIntegral chunkSize * fromIntegral chunkSize
                                     size2 = fromIntegral (offset2 - offset)
                                 in (offset,size2) : chunks (offset2) (size-fromIntegral size2)

-- | Maps region of file and returns it as list of 'ForeignPtr's. See 'mmapFilePtr' for details.
-- Each chunk is mapped in on demand only.
mmapFileForeignPtrLazy :: FilePath                   -- ^ name of file to map
                   -> Mode                           -- ^ access mode
                   -> Maybe (Int64,Int64)            -- ^ range to map, maps whole file if Nothing
                   -> IO [(ForeignPtr a,Int,Int)]    -- ^ foreign pointer to beginning of region and size
mmapFileForeignPtrLazy filepath mode offsetsize = do
    list <- mmapFilePtrLazy filepath mode offsetsize
    return (map turn list)
    where
        turn (ptr,rawsize,offset,size) = unsafePerformIO $ do
            let rawsizeptr = castIntToPtr rawsize
            foreignptr <- newForeignPtrEnv c_system_io_mmap_munmap_funptr rawsizeptr ptr
            return (foreignptr,offset,size)

-- | Maps region of file and returns it as 'Data.ByteString.Lazy.ByteString'.
-- File is mapped in in 'ReadOnly' mode. See 'mmapFilePtrLazy' for details.
-- Chunks are mapped in on demand.
--
-- Note: this operation may break referential transparency! If
-- any other process on the system changes the file when it is mapped
-- into Haskell, the contents of your 'Data.ByteString.Lazy.ByteString' may change.
--
mmapFileByteStringLazy :: FilePath                     -- ^ name of file to map
                       -> Maybe (Int64,Int64)          -- ^ range to map, maps whole file if Nothing
                       -> IO BSL.ByteString            -- ^ bytestring with file content
mmapFileByteStringLazy filepath offsetsize = do
    list <- mmapFileForeignPtrLazy filepath ReadOnly offsetsize
    return (BSL.fromChunks (map turn list))
    where
        turn (foreignptr,offset,size) = BS.fromForeignPtr foreignptr offset size

munmapFilePtr :: Ptr () -> Ptr a -> IO ()
munmapFilePtr = c_system_io_mmap_munmap

munmapFilePtrFinalizer :: FunPtr(Ptr () -> Ptr a -> IO ())
munmapFilePtrFinalizer = c_system_io_mmap_munmap_funptr

chunkSize :: Int
chunkSize = fromIntegral $ (128*1024 `div` c_system_io_granularity) * c_system_io_granularity

mmapFileOpen :: FilePath -> Mode -> IO (ForeignPtr ())
mmapFileOpen filepath mode = do
    ptr <- withCString filepath $ \filepath ->
        c_system_io_mmap_file_open filepath (fromIntegral $ fromEnum mode)
    when (ptr == nullPtr) $
        throwErrno $ "opening of '" ++ filepath ++ "' failed"
    handle <- newForeignPtr c_system_io_mmap_file_close ptr
    return handle

-- we need to return non null ptr to nowhere, sometimes
nonZeroPtr :: Ptr a
nonZeroPtr = nullPtr `plusPtr` 128

castPtrToInt :: Ptr a -> Int
castPtrToInt ptr = ptr `minusPtr` nullPtr

castIntToPtr :: Int -> Ptr a
castIntToPtr int = nullPtr `plusPtr` int

-- | Should open file given as CString in mode given as CInt
foreign import ccall unsafe "HsMmap.h system_io_mmap_file_open"
    c_system_io_mmap_file_open :: CString       -- ^ file path, system encoding
                               -> CInt          -- ^ mode as 0, 1, 2, fromEnum
                               -> IO (Ptr ())   -- ^ file handle returned, nullPtr on error (and errno set)
-- | Used in finalizers, to close handle
foreign import ccall unsafe "HsMmap.h &system_io_mmap_file_close"
    c_system_io_mmap_file_close :: FunPtr(Ptr () -> IO ())

-- | Mmemory maps file from handle, using mode, starting offset and size
foreign import ccall unsafe "HsMmap.h system_io_mmap_mmap"
    c_system_io_mmap_mmap :: Ptr ()  -- ^ handle from c_system_io_mmap_file_open
                          -> CInt    -- ^ mode
                          -> CLLong  -- ^ starting offset, must be nonegative
                          -> CInt    -- ^ length, must be greater than zero, in ReadOnly or WriteCopy offset+length must be less than file size
                          -> IO (Ptr a) -- ^ starting pointer to byte data, nullPtr on error (plus errno set)
-- | Used in finalizers
foreign import ccall unsafe "HsMmap.h &system_io_mmap_munmap"
    c_system_io_mmap_munmap_funptr :: FunPtr(Ptr () -> Ptr a -> IO ())
-- | Unmap region of memory. Size must be the same as returned by mmap. If size is zero, does nothing (treat pointer as invalid)
foreign import ccall unsafe "HsMmap.h system_io_mmap_munmap"
    c_system_io_mmap_munmap :: Ptr () -> Ptr a -> IO ()
-- | Get file size in system specific manner
foreign import ccall unsafe "HsMmap.h system_io_mmap_file_size"
    c_system_io_file_size :: Ptr () -> IO CLLong
-- | Memory mapping granularity.
foreign import ccall unsafe "HsMmap.h system_io_mmap_granularity"
    c_system_io_granularity :: CInt


