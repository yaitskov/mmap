
module System.IO.MMap
(
     -- $mmap_intro

     -- * Memory mapped files strict interface
     mmapFilePtr,
     mmapWithFilePtr,
     mmapFileForeignPtr,
     mmapFileByteString,

     munmapFilePtr,

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
import Foreign.C.Types (CInt,CLLong,CSize)
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
import Control.Exception   (bracket,finally)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BSL  (ByteString,fromChunks)

-- $mmap_intro
--
-- This module is an interface to @mmap(2)@ system call under POSIX
-- (Unix, Linux, Mac OS X) and @CreateFileMapping@, @MapViewOfFile@ under
-- Windows.
--
-- We can consider mmap as lazy IO pushed into the virtual memory
-- subsystem.
--
-- It is only safe to mmap a file if you know you are the sole
-- user. Otherwise referential transparency may be compromised.
--
-- In case of IO errors all function use 'throwErrno'.
--
-- In case of 'ForeignPtr' or 'ByteString' functions the storage
-- manager is used to free the mapped memory. When the garbage
-- collector notices there are no further references to the mapped
-- memory, a call to @munmap@ is made. It is not necessary to do this
-- yourself. In tight memory situations, it may be profitable to use
-- 'System.Mem.performGC' or 'finalizeForeignPtr' to force an unmap.
--
-- In functions returning Ptr use 'munmapFilePtr'.
--
-- File must exist before mapping it into memory. It also needs
-- correct permissions for reading and/or writing (depending on mode).
--
-- If mode is 'ReadWrite' or 'WriteCopy', the returned memory region
-- may be written to with 'Foreign.Storable.poke' and friends. In
-- 'WriteCopy' mode changes will not be written to disk.  It is an
-- error to modify mapped memory in 'ReadOnly' mode. If is undefined if
-- and how changes from external sources affect your mmapped regions.
--
-- Range specified may be 'Nothing', in this case whole file will be
-- mapped. Otherwise range should be 'Just (offset,size)' where
-- offsets is the beginning byte of file region to map and size tells
-- its length. There are no alignment requirements. Returned Ptr or
-- ForeignPtr will be aligned to page size boundary and you'll be
-- given offset to your data. Neither @offset@ nor @size@ cannot be
-- negative.  Sum @offset + size@ should not be greater than file
-- length. We do allow @size@ to be 0 and we do mmap files of 0
-- length.
--
-- Range to map must not extend beyond end of file. Use @ftruncate@ or
-- @SetFileSize@ if you want bigger files.
--
-- For more details about mmap, and its consequences, see:
--
-- * <http://opengroup.org/onlinepubs/009695399/functions/mmap.html>
--
-- * <http://www.gnu.org/software/libc/manual/html_node/Memory_002dmapped-I_002fO.html>
--
-- * <http://msdn2.microsoft.com/en-us/library/aa366781(VS.85).aspx>
--
-- Questions and Answers
--
-- * Q: What happens if somebody writes to mmapped file? A:
-- Undefined. System is free to not synchronize write system call and
-- mmap so nothing is sure. So this might be reflected in your memory
-- or not.  This applies even in 'WriteCopy' mode.
--
-- * Q: What happens if I map 'ReadWrite' and change memory? A: After
-- some time in will be written to disk. It is unspecified when this
-- happens.
--
-- * Q: What if somebody removes my file? A: Undefined. File with
-- mmapped region is treated as open file. Same rules apply.
--
-- * Q: Why can't I open my file for writting after mmaping it? A:
-- File needs to be unmapped first. Either make sure you don't
-- reference memory mapped regions and force garbage collection (this
-- is hard to do). Or use mmaping with explicit memory management.
--
-- * Q: Can I map region after end of file? A: We forbid such
-- situation in our library. Theoretically systems allow to map
-- regions outside of file range, but accessing such memory is not
-- allowed. Unless somebody resizes this file meanwhile, but then
-- semantics diverge. Better to disable such possibility.
--


-- | Mode of mapping. Three cases are supported.
data Mode = ReadOnly      -- ^ file is mapped read-only
          | ReadWrite     -- ^ file is mapped read-write
          | WriteCopy     -- ^ file is mapped read-write, but changes aren't propagated to disk
    deriving (Eq,Ord,Enum)

-- | The 'mmapFilePtr' function maps a file or device into memory,
-- returning a tuple @(ptr,rawsize,offset,size)@ where:
--
-- * @ptr@ is pointer to mmapped region
--
-- * @rawsize@ is length (in bytes) of mapped data, rawsize might be
-- greater than size because of alignment
--
-- * @offset@ tell where your data lives: @plusPtr ptr offset@
--
-- * @size@ your data length (in bytes)
--
-- If 'mmapFilePtr' fails for some reason, a 'throwErrno' is used.
--
-- Use @munmapFilePtr ptr rawsize@ to unmap memory.
--
-- Memory mapped files will behave as if they were read lazily --
-- pages from the file will be loaded into memory on demand.
--
mmapFilePtr :: FilePath                     -- ^ name of file to mmap
            -> Mode                         -- ^ access mode
            -> Maybe (Int64,Int)            -- ^ range to map, maps whole file if Nothing
            -> IO (Ptr a,Int,Int,Int)       -- ^ (ptr,rawsize,offset,size)
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

-- | Memory map region of file using autounmap semantics. See
-- 'mmapFilePtr' for description of parameters.  The @action@ will be
-- executed with tuple @(ptr,size)@ as single argument. This is the
-- pointer to mapped data already adjusted and size of requested
-- region. Return value is that of action.
mmapWithFilePtr :: FilePath                        -- ^ name of file to mmap
                -> Mode                            -- ^ access mode
                -> Maybe (Int64,Int)               -- ^ range to map, maps whole file if Nothing
                -> ((Ptr (),Int) -> IO a)          -- ^ action to run
                -> IO a                            -- ^ (ptr,rawsize,offset,size)
mmapWithFilePtr filepath mode offsetsize action = do
    (ptr,rawsize,offset,size) <- mmapFilePtr filepath mode offsetsize
    result <- action (ptr `plusPtr` offset,size) `finally` munmapFilePtr ptr rawsize
    return result

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

-- | Maps region of file and returns it as
-- 'Data.ByteString.ByteString'.  File is mapped in in 'ReadOnly'
-- mode. See 'mmapFilePtr' for details.
mmapFileByteString :: FilePath                     -- ^ name of file to map
                   -> Maybe (Int64,Int)            -- ^ range to map, maps whole file if Nothing
                   -> IO BS.ByteString             -- ^ bytestring with file contents
mmapFileByteString filepath range = do
    (foreignptr,offset,size) <- mmapFileForeignPtr filepath ReadOnly range
    let bytestring = BS.fromForeignPtr foreignptr offset size
    return bytestring

-- | The 'mmapFilePtrLazy' function maps a file or device into memory,
-- returning a list of tuples with the same meaning as in function
-- 'mmapFilePtr'.
mmapFilePtrLazy :: FilePath                -- ^ name of file to mmap
            -> Mode                        -- ^ access mode
            -> Maybe (Int64,Int64)         -- ^ range to map, maps whole file if Nothing
            -> IO [(Ptr a,Int,Int,Int)]    -- ^ (ptr,rawsize,offset,size)
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
                   | otherwise = let offset2 = ((offset + chunkSize + chunkSize - 1) `div` chunkSize) * chunkSize
                                     size2 = offset2 - offset
                                 in (offset,fromIntegral size2) : chunks offset2 (size-size2)

-- | Maps region of file and returns it as list of 'ForeignPtr's. See
-- 'mmapFilePtrLazy' and 'mmapFileForeignPtr' for details.
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

-- | Maps region of file and returns it as
-- 'Data.ByteString.Lazy.ByteString'. File is mapped in in 'ReadOnly'
-- mode. See 'mmapFileForeignPtrLazy' for details.
mmapFileByteStringLazy :: FilePath                     -- ^ name of file to map
                       -> Maybe (Int64,Int64)          -- ^ range to map, maps whole file if Nothing
                       -> IO BSL.ByteString            -- ^ bytestring with file content
mmapFileByteStringLazy filepath offsetsize = do
    list <- mmapFileForeignPtrLazy filepath ReadOnly offsetsize
    return (BSL.fromChunks (map turn list))
    where
        turn (foreignptr,offset,size) = BS.fromForeignPtr foreignptr offset size

-- | Unmaps memory region. As parameters use values marked as ptr and rawsize in description of 'mmapFilePtr'.
munmapFilePtr :: Ptr a  -- ^ pointer 
              -> Int    -- ^ rawsize 
              -> IO ()
munmapFilePtr ptr rawsize = c_system_io_mmap_munmap (castIntToPtr rawsize) ptr

--munmapFilePtrFinalizer :: FunPtr(Ptr () -> Ptr a -> IO ())
--munmapFilePtrFinalizer = c_system_io_mmap_munmap_funptr

chunkSize :: Num a => a
chunkSize = fromIntegral $ (128*1024 `div` c_system_io_granularity) * c_system_io_granularity

mmapFileOpen :: FilePath -> Mode -> IO (ForeignPtr ())
mmapFileOpen filepath mode = do
    ptr <- withCString filepath $ \filepath ->
        c_system_io_mmap_file_open filepath (fromIntegral $ fromEnum mode)
    when (ptr == nullPtr) $
        throwErrno $ "opening of '" ++ filepath ++ "' failed"
    handle <- newForeignPtr c_system_io_mmap_file_close ptr
    return handle

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
    c_system_io_mmap_mmap :: Ptr ()     -- ^ handle from c_system_io_mmap_file_open
                          -> CInt       -- ^ mode
                          -> CLLong     -- ^ starting offset, must be nonegative
                          -> CSize      -- ^ length, must be greater than zero, in ReadOnly or WriteCopy offset+length must be less than file size
                          -> IO (Ptr a) -- ^ starting pointer to byte data, nullPtr on error (plus errno set)
-- | Used in finalizers
foreign import ccall unsafe "HsMmap.h &system_io_mmap_munmap"
    c_system_io_mmap_munmap_funptr :: FunPtr(Ptr () -> Ptr a -> IO ())
-- | Unmap region of memory. Size must be the same as returned by
-- mmap. If size is zero, does nothing (treat pointer as invalid)
foreign import ccall unsafe "HsMmap.h system_io_mmap_munmap"
    c_system_io_mmap_munmap :: Ptr () -> Ptr a -> IO ()
-- | Get file size in system specific manner
foreign import ccall unsafe "HsMmap.h system_io_mmap_file_size"
    c_system_io_file_size :: Ptr () -> IO CLLong
-- | Memory mapping granularity.
foreign import ccall unsafe "HsMmap.h system_io_mmap_granularity"
    c_system_io_granularity :: CInt
