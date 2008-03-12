

#define _LARGEFILE64_SOURCE1
#define _FILE_OFFSET_BITS 64

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/errno.h>

//foreign import ccall unsafe "system_io_mmap_file_open" c_system_io_mmap_file_open :: CString -> CInt -> IO (Ptr ())
void *system_io_mmap_file_open(const char *filepath, int mode)
{
    void *handle = NULL;
    int access;
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
    handle = (void *)open(filepath,access|O_SHLOCK,0666);
    if( handle==(void*)(-1) ) {
	//fprintf(stderr,"open errno %d\n",errno);
        return NULL;
    }
    return handle;
}

//foreign import ccall unsafe "system_io_mmap_file_close" c_system_io_mmap_file_close :: FunPtr(Ptr () -> IO ())
void system_io_mmap_file_close(void *handle)
{
    close((int)handle);
}

//foreign import ccall unsafe "system_io_mmap_mmap" c_system_io_mmap_mmap :: Ptr () -> CInt -> CLLong -> CInt -> IO (Ptr ())
void *system_io_mmap_mmap(void *handle, int mode, long long offset, int size)
{
    void *ptr = NULL;
    int prot;
    int flags;
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
    fstat((int)handle,&st);
    if( st.st_size<offset+size) {
	ftruncate((int)handle,offset+size);
    }

    ptr = mmap(NULL,size,prot,flags,(int)handle,offset);

    if( ptr == (void*)(-1)) {
	//fprintf(stderr,"mmap errno %d\n",errno);
	return NULL;
    }
    return ptr;
}

//foreign import ccall unsafe "system_io_mmap_munmap" c_system_io_mmap_munmap :: Ptr () -> CInt -> IO ()
void system_io_mmap_munmap(void *ptr,int size)
{
    munmap(ptr,size);
}

//foreign import ccall unsafe "system_io_mmap_file_size" c_system_io_file_size :: Ptr () -> IO (CLLong)
long long system_io_mmap_file_size(void *handle)
{
    struct stat st;
    fstat((int)handle,&st);
    return st.st_size;
}

//foreign import ccall unsafe "system_io_mmap_granularity" c_system_io_granularity :: CInt
int system_io_mmap_granularity()
{
    return getpagesize();
}
