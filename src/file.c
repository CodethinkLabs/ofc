#include "file.h"
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>


struct file_s
{
	const char* path;
	unsigned    size;
	char*       strz;
};


static char* file__read(const char* path, unsigned* size)
{
	int fd = open(path, O_RDONLY);
	if (fd < 0 ) return NULL;

	struct stat fs;
	if (fstat(fd, &fs) != 0)
	{
		close(fd);
		return NULL;
	}

	char* buff = (char*)malloc(fs.st_size + 1);
	if (!buff)
	{
		close(fd);
		return NULL;
	}

	ssize_t rsize = read(fd, buff, fs.st_size);
	close(fd);

	if (rsize != fs.st_size)
	{
		free(buff);
		return NULL;
	}

	buff[fs.st_size] = '\0';

	if (size) *size = fs.st_size;
	return buff;
}

file_t* file_create(const char* path)
{
	file_t* file = (file_t*)malloc(sizeof(file_t));
	if (!file) return NULL;

	file->path = path;
	file->strz = file__read(path, &file->size);

	if (!file->strz)
	{
		free(file);
		return NULL;
	}

	return file;
}

void file_delete(file_t* file)
{
	if (!file)
		return;

	free(file->strz);
	free(file);
}



const char* file_get_path(const file_t* file)
{
	return (file ? file->path : NULL);
}

const char* file_get_strz(const file_t* file)
{
	return (file ? file->strz : NULL);
}



bool file_get_position(
	const file_t* file, const char* ptr,
	unsigned* row, unsigned* col)
{
	if (!file || !file->strz || !ptr)
		return false;

	uintptr_t pos = ((uintptr_t)ptr - (uintptr_t)file->strz);
	if (pos >= file->size)
		return false;

	/* TODO - Use binary tree of line positions to find faster. */
	unsigned i, r, c;
	for (i = 0, r = 0, c = 0; i < pos; i++)
	{
		switch (file->strz[i])
		{
			case '\r':
			case '\n':
				r += 1;
				c = 0;
				break;
			default:
				c++;
				break;
		}
	}

	if (row) *row = r;
	if (col) *col = c;
	return true;
}
