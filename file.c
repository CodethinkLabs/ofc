#include "file.h"
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>


struct file_s
{
	const char* path;
	lang_opts_t opts;

	char*       strz;

	unsigned    line_count;
	line_t**    line;
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

file_t* file_create(const char* path, lang_opts_t opts)
{
	file_t* file = (file_t*)malloc(sizeof(file_t));
	if (!file) return NULL;

	file->path = path;
	file->opts = opts;

	file->strz = file__read(path);

	if (!file->strz)
	{
		free(file);
		return NULL;
	}

	file->line_count = 0;
	file->line       = NULL;

	bool success = false;
	switch (file->opts.form)
	{
		case LANG_FORM_FIXED:
		case LANG_FORM_TAB:
			success = preprocess_fixed_form(file);
			break;
		default:
			/* TODO - Parse free-form. */
			success = false;
			break;
	}

	if (!success)
	{
		file_delete(file);
		return NULL;
	}

	return file;
}

void file_delete(file_t* file)
{
	if (!file)
		return;

	unsigned i;
	for (i = 0; i < file->line_count; i++)
		line_delete(file->line[i]);
	free(file->line);

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

lang_opts_t file_get_lang_opts(const file_t* file)
{
	if (!file)
		return LANG_OPTS_F77;
	return file->opts;	
}


bool file_append_line(file_t* file, line_t* line)
{
	if (!file) return false;
	if (!line) return true;

	line_t** nline = (line_t**)realloc(file->line,
		(sizeof(line_t*) * (file->line_count + 1)));
	if (!nline) return false;

	file->line = nline;
	file->line[file->line_count++] = line;
	return true;
}

bool file_append_rope(file_t* file, rope_t* rope)
{
	if (!file || !file->line || (file->line_count == 0))
		return false;
	return line_append_rope(
		file->line[file->line_count - 1], rope);
}


bool file_foreach_line(
	const file_t* file, void* param,
	bool (*func)(const line_t* line, void* param))
{
	if (!file || !func)
		return false;

	unsigned i;
	for (i = 0; i < file->line_count; i++)
	{
		if (!func(file->line[i], param))
			return false;
	}

	return true;
}
