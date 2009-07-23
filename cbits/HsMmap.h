#ifndef __HSMMAP_H__
#define __HSMMAP_H__

void *system_io_mmap_file_open(const char *filepath, int mode);

void system_io_mmap_file_close(void *handle);

void *system_io_mmap_mmap(void *handle, int mode, long long offset, int size);

void system_io_mmap_munmap(int *size, void *ptr);

long long system_io_mmap_file_size(void *handle);

int system_io_mmap_granularity();

#endif /* __HSMMAP_H__ */
