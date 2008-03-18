


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
    DWORD dwDesiredAccess;
    DWORD dwCreationDisposition;
    if( !filepath )
        return NULL;
    switch(mode) {
        case 0:
            dwDesiredAccess = GENERIC_READ;
            dwCreationDisposition = OPEN_ALWAYS;
            break;
        case 1:
            dwDesiredAccess = GENERIC_WRITE|GENERIC_READ;
            dwCreationDisposition = CREATE_ALWAYS;
            break;
        case 2:
            dwDesiredAccess = GENERIC_READ;
            dwCreationDisposition = OPEN_ALWAYS;
            break;
        default:
            return NULL;
    }
    handle = CreateFileA(filepath,
                         dwDesiredAccess,
                         FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,
                         NULL,
                         dwCreationDisposition,
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

//foreign import ccall unsafe "system_io_mmap_mmap" c_system_io_mmap_mmap :: Ptr () -> CInt -> CLLong -> CInt -> IO (Ptr ())
void *system_io_mmap_mmap(void *handle, int mode, long long offset, int size)
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
    DWORD flProtect;
    DWORD dwDesiredAccess;
    switch(mode) {
        case 0:
            flProtect = PAGE_READONLY;
            dwDesiredAccess = FILE_MAP_READ;
            break;
        case 1:
            flProtect = PAGE_READWRITE;
            dwDesiredAccess = FILE_MAP_WRITE;
            break;
        case 2:
            flProtect = PAGE_WRITECOPY;
            dwDesiredAccess = FILE_MAP_COPY;
            break;
        default:
            return NULL;
    }
    mapping = CreateFileMapping(handle, NULL, flProtect, (DWORD) ((offset + size)>>32), (DWORD)(offset + size), NULL);
    if( !mapping ) {
      DWORD dw = GetLastError();
      fprintf(stderr,"CreateFileMapping %d\n",(int)dw);
    }
    ptr = MapViewOfFile(mapping,dwDesiredAccess, (DWORD)(offset>>32), (DWORD)(offset), size );
    if( !ptr ) {
      DWORD dw = GetLastError();
      fprintf(stderr,"MapViewOfFile %d\n",(int)dw);
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

//foreign import ccall unsafe "system_io_mmap_granularity" c_system_io_granularity :: CInt
int system_io_mmap_granularity()
{
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return sysinfo.dwAllocationGranularity;
}


