#include "HsMmap.h"

#define _LARGEFILE64_SOURCE1
#define _FILE_OFFSET_BITS 64

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/errno.h>

#ifdef _DEBUG
int counters = 0;

int system_io_mmap_counters()
{
    return counters;
}
#endif

//foreign import ccall unsafe "system_io_mmap_file_open" c_system_io_mmap_file_open :: CString -> CInt -> IO (Ptr ())
void *system_io_mmap_file_open(const char *filepath, int mode)
{
    void *handle = NULL;
    int access, fd;
    if( !filepath )
        return NULL;
    switch(mode) {
    case 0:
	access = O_RDONLY;
	break;
    case 1:
	access = O_RDWR;
	break;
    case 2:
	access = O_RDONLY;
	break;
    default:
	return NULL;
    }
#ifdef O_NOCTTY
    // access |= O_NOCTTY;
#endif
#ifdef O_LARGEFILE
    // access |= O_LARGEFILE;
#endif
    fd = open(filepath,access,0666);
    if( fd == -1 ) {
        return NULL;
    }
    handle = (void *)fd + 1;
    return handle;
}

//foreign import ccall unsafe "system_io_mmap_file_close" c_system_io_mmap_file_close :: FunPtr(Ptr () -> IO ())
void system_io_mmap_file_close(void *handle)
{
    int fd = (int)handle - 1;
    close(fd);
}

//foreign import ccall unsafe "system_io_mmap_mmap" c_system_io_mmap_mmap :: Ptr () -> CInt -> CLLong -> CInt -> IO (Ptr ())
void *system_io_mmap_mmap(void *handle, int mode, long long offset, int size)
{
    void *ptr = NULL;
    int prot;
    int flags;
    int fd = (int)handle - 1;
    switch(mode) {
    case 0:
	prot = PROT_READ;
	flags = MAP_PRIVATE;
	break;
    case 1:
	prot = PROT_READ|PROT_WRITE;
	flags = MAP_SHARED;
	break;
    case 2:
	prot = PROT_READ|PROT_WRITE;
	flags = MAP_PRIVATE;
	break;
    default:
	return NULL;
    }

    struct stat st;
    fstat(fd,&st);
    if( st.st_size<offset+size) {
	ftruncate(fd,offset+size);
    }

    ptr = mmap(NULL,size,prot,flags,fd,offset);

    if( ptr == MAP_FAILED ) {
	return NULL;
    }
    return ptr;
}

void system_io_mmap_munmap(int *size, void *ptr) // Ptr CInt -> Ptr a -> IO ()
{
    munmap(ptr,*size);
    free(size);
}

//foreign import ccall unsafe "system_io_mmap_file_size" c_system_io_file_size :: Ptr () -> IO (CLLong)
long long system_io_mmap_file_size(void *handle)
{
    int fd = (int)handle - 1;
    struct stat st;
    fstat(fd,&st);
    return st.st_size;
}

//foreign import ccall unsafe "system_io_mmap_granularity" c_system_io_granularity :: CInt
int system_io_mmap_granularity()
{
    return getpagesize();
}
