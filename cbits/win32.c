


#include <windows.h>
#include <stdio.h>

//foreign import ccall unsafe "system_io_mmap_file_open" c_system_io_mmap_file_open :: CString -> CInt -> IO (Ptr ())
void *system_io_mmap_file_open(const char *filepath, int mode)
{
    /*
    HANDLE WINAPI CreateFileA(
      __in      LPCTSTR lpFileName,
      __in      DWORD dwDesiredAccess,
      __in      DWORD dwShareMode,
      __in_opt  LPSECURITY_ATTRIBUTES lpSecurityAttributes,
      __in      DWORD dwCreationDisposition,
      __in      DWORD dwFlagsAndAttributes,
      __in_opt  HANDLE hTemplateFile
    );
    */
    void *handle = NULL;
    if( !filepath )
        return NULL;
    handle = CreateFileA(filepath,
                         GENERIC_READ|GENERIC_WRITE,
                         FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,
                         NULL,
                         OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL,
                         NULL);
    if( handle==INVALID_HANDLE_VALUE )
        return NULL;
    return handle;
}

//foreign import ccall unsafe "system_io_mmap_file_close" c_system_io_mmap_file_close :: FunPtr(Ptr () -> IO ())
void system_io_mmap_file_close(void *handle)
{
    CloseHandle(handle);
}

//foreign import ccall unsafe "system_io_mmap_mmap" c_system_io_mmap_mmap :: Ptr () -> CLLong -> CInt -> IO (Ptr ())
void *system_io_mmap_mmap(void *handle, long long offset, int size)
{
    /*
    HANDLE WINAPI CreateFileMapping(
      __in      HANDLE hFile,
      __in_opt  LPSECURITY_ATTRIBUTES lpAttributes,
      __in      DWORD flProtect,
      __in      DWORD dwMaximumSizeHigh,
      __in      DWORD dwMaximumSizeLow,
      __in_opt  LPCTSTR lpName
    );
    LPVOID WINAPI MapViewOfFile(
      __in  HANDLE hFileMappingObject,
      __in  DWORD dwDesiredAccess,
      __in  DWORD dwFileOffsetHigh,
      __in  DWORD dwFileOffsetLow,
      __in  SIZE_T dwNumberOfBytesToMap
    );
    */
    HANDLE mapping;
    void *ptr = NULL;
    mapping = CreateFileMapping(handle, NULL, PAGE_READWRITE, (DWORD) ((offset + size)>>32), (DWORD)(offset + size), NULL);
    if( !mapping ) {
      DWORD dw = GetLastError();
      fprintf(stderr,"CreateFileMapping %d\n",dw);
    }
    ptr = MapViewOfFile(mapping,FILE_MAP_ALL_ACCESS, (DWORD)(offset>>32), (DWORD)(offset), size );
    if( !ptr ) {
      DWORD dw = GetLastError();
      fprintf(stderr,"MapViewOfFile %d\n",dw);
    }
    CloseHandle(mapping);
    return ptr;
}

//foreign import ccall unsafe "system_io_mmap_munmap" c_system_io_mmap_munmap :: Ptr () -> CInt -> IO ()
void system_io_mmap_munmap(void *ptr,int size)
{
    UnmapViewOfFile(ptr);
}

//foreign import ccall unsafe "system_io_mmap_file_size" c_system_io_file_size :: Ptr () -> IO (CLLong)
long long system_io_mmap_file_size(void *handle)
{
    DWORD lobits, hibits;
    lobits = GetFileSize(handle,&hibits);
    return (long long)lobits + ((long long)hibits << 32);
}
