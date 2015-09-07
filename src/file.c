#include "file.h"
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>


struct file_s
{
	const char* path;
	char*       strz;
};


static char* file__read(const char* path)
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
	return buff;
}

file_t* file_create(const char* path)
{
	file_t* file = (file_t*)malloc(sizeof(file_t));
	if (!file) return NULL;

	file->path = path;
	file->strz = file__read(path);

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
