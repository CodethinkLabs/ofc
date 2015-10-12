#include "file.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>


struct ofc_file_s
{
	char*           path;
	char*           include;
	char*           strz;
	ofc_lang_opts_t opts;
	unsigned        size;
	unsigned        ref;
};


static char* ofc_file__read(const char* path, unsigned* size)
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

ofc_file_t* ofc_file_create(const char* path, ofc_lang_opts_t opts)
{
	ofc_file_t* file = (ofc_file_t*)malloc(sizeof(ofc_file_t));
	if (!file) return NULL;

	file->path = strdup(path);
	file->strz = ofc_file__read(path, &file->size);
	file->opts = opts;

	file->include = NULL;

	file->ref = 0;

	if (!file->path || !file->strz)
	{
		ofc_file_delete(file);
		return NULL;
	}

	return file;
}

ofc_file_t* ofc_file_create_include(
	const char* path, ofc_lang_opts_t opts, const char* include)
{
	ofc_file_t* file = ofc_file_create(path, opts);
	if (!include) return file;
	if (!file) return NULL;

	file->include = strdup(include);
	if (!file->include)
	{
		ofc_file_delete(file);
		return NULL;
	}

	return file;
}

bool ofc_file_reference(ofc_file_t* file)
{
	if (!file)
		return false;

	unsigned nref = file->ref + 1;
	if (nref == 0) return false;

	file->ref += 1;
	return true;
}

void ofc_file_delete(ofc_file_t* file)
{
	if (!file)
		return;

	if (file->ref > 0)
	{
		file->ref -= 1;
		return;
	}

	free(file->strz);
	free(file->path);
	free(file->include);
	free(file);
}



const char* ofc_file_get_path(const ofc_file_t* file)
{
	return (file ? file->path : NULL);
}

const char* ofc_file_get_include(const ofc_file_t* file)
{
	if (!file)
		return NULL;
	return (file->include ? file->include : file->path);
}

const char* ofc_file_get_strz(const ofc_file_t* file)
{
	return (file ? file->strz : NULL);
}

ofc_lang_opts_t ofc_file_get_lang_opts(const ofc_file_t* file)
{
	return (file ? file->opts : OFC_LANG_OPTS_F77);
}


static char* ofc_file__include_path(
	const char* file, const char* path)
{
	if (!file)
		return strdup(path);

	unsigned prefix_len = 0;

	unsigned i;
	for (i = 0; file[i] != '\0'; i++)
	{
		if (file[i] == '/')
			prefix_len = (i + 1);
	}

	if (prefix_len == 0)
		return strdup(path);

	unsigned rpath_len = prefix_len + strlen(path);

	char* rpath = malloc(rpath_len + 1);
	if (!rpath) return NULL;

	sprintf(rpath, "%.*s%s",
		prefix_len, file, path);

	return rpath;
}

char* ofc_file_include_path(
	const ofc_file_t* file, const char* path)
{
	if (!file)
		return strdup(path);

	if (file->include)
		return ofc_file__include_path(
			file->include, path);

	return ofc_file__include_path(
		file->path, path);
}



bool ofc_file_get_position(
	const ofc_file_t* file, const char* ptr,
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



static void ofc_file__debug_va(
	const ofc_file_t* file, const char* ptr,
	const char* type, const char* format, va_list args)
{
	fprintf(stderr, "%s:", type);

	unsigned row, col;
	if (ofc_file_get_position(
		file, ptr, &row, &col))
		fprintf(stderr, "%s:%u,%u:",
			file->path, (row + 1), col);

	fprintf(stderr, " ");
	vfprintf(stderr, format, args);
	fprintf(stderr, "\n");
}

void ofc_file_error_va(
	const ofc_file_t* file, const char* ptr,
	const char* format, va_list args)
{
	ofc_file__debug_va(
		file, ptr, "Error", format, args);
}

void ofc_file_warning_va(
	const ofc_file_t* file, const char* ptr,
	const char* format, va_list args)
{
	ofc_file__debug_va(
		file, ptr, "Warning", format, args);
}



void ofc_file_error(
	const ofc_file_t* file, const char* ptr,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);
	ofc_file_warning_va(file, ptr, format, args);
	va_end(args);
}

void ofc_file_warning(
	const ofc_file_t* file, const char* ptr,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);
	ofc_file_error_va(file, ptr, format, args);
	va_end(args);
}
