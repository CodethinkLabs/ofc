/* Copyright 2015 Codethink Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "ofc/fctype.h"
#include "ofc/file.h"
#include "ofc/global_opts.h"


struct ofc_file_s
{
	const ofc_file_t*      parent;

	ofc_sparse_ref_t include_stmt;

	char*                    path;
	ofc_file_include_list_t* include;
	char*                    strz;
	ofc_lang_opts_t          opts;
	unsigned                 size;
	unsigned                 ref;
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

	file->parent = NULL;
	file->include = NULL;

	file->ref = 0;

	if (!file->path || !file->strz)
	{
		ofc_file_delete(file);
		return NULL;
	}

	return file;
}

static char* ofc_file__include_path_search(
	const char* path, const char* file)
{
	if (!path)
		return strdup(file);

	unsigned path_len = strlen(path);

	if (path_len == 0)
		return strdup(file);

	bool needs_slash = (path[path_len - 1] != '/');

	unsigned rpath_len = path_len + strlen(file)
		+ (needs_slash ? 1 : 0);

	char* rpath = malloc(rpath_len + 1);
	if (!rpath) return NULL;

	strcpy(rpath, path);
	if (needs_slash)
		strcat(rpath, "/");
	strcat(rpath, file);

	return rpath;
}

static char* ofc_file__include_path_relative(
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

static char* ofc_file__base_parent_path(
	const ofc_file_t* file)
{
	if (!file) return NULL;

	if (file->parent)
		return ofc_file__base_parent_path(file->parent);

	return file->path;
}

ofc_file_t* ofc_file_create_include(
	const char* path, ofc_lang_opts_t opts,
	const ofc_file_t* parent_file, ofc_sparse_ref_t include_stmt)
{
	ofc_file_t* file = NULL;

	if (parent_file && parent_file->include)
	{
		ofc_file_include_list_t* include = parent_file->include;
		unsigned i;
		for (i = 0; i < include->count; i++)
		{
			char* rpath = ofc_file__include_path_search(
				include->path[i], path);
			file = ofc_file_create(rpath, opts);

			if (file)
			{
				file->parent = parent_file;
				file->include_stmt = include_stmt;
				file->include = parent_file->include;

				free(rpath);
				return file;
			}
			free(rpath);
		}
	}

	char* bpath = ofc_file__base_parent_path(parent_file);
	char* rpath = ofc_file__include_path_relative(bpath, path);
	file = ofc_file_create(rpath, opts);
	free(rpath);
	if (file && parent_file)
	{
		file->parent = parent_file;
		file->include_stmt = include_stmt;
		file->include = parent_file->include;
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

	/* The root file is responsible for cleaning up */
	if (!file->parent)
		ofc_file_include_list_delete(file->include);

	free(file);
}



const char* ofc_file_get_path(const ofc_file_t* file)
{
	return (file ? file->path : NULL);
}

const char* ofc_file_get_strz(const ofc_file_t* file)
{
	return (file ? file->strz : NULL);
}

const ofc_lang_opts_t* ofc_file_get_lang_opts(const ofc_file_t* file)
{
	if (!file) return NULL;
	return &file->opts;
}

ofc_lang_opts_t* ofc_file_modify_lang_opts(ofc_file_t* file)
{
	if (!file) return NULL;
	return &file->opts;
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
				/* Support Windows line endings when running on cygwin. */
				if (file->strz[i + 1] == '\n') i++;
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


ofc_file_list_t* ofc_file_list_create(void)
{
    ofc_file_list_t* list
		= (ofc_file_list_t*)malloc(
			sizeof(ofc_file_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->file  = NULL;

	return list;
}

bool ofc_file_list_add(
	ofc_file_list_t* list, ofc_file_t* file)
{
	if (!list || !file) return false;

	ofc_file_t** nlist = (ofc_file_t**)realloc(list->file,
		(sizeof(ofc_file_t*) * (list->count + 1)));
	if (!nlist) return false;

	list->file = nlist;
	list->file[list->count++] = file;

	return true;
}

void ofc_file_list_delete(
	ofc_file_list_t* list)
{
	if (!list) return;

	unsigned i;
	for (i = 0; i < list->count; i++)
		ofc_file_delete(list->file[i]);

	free(list->file);
	free(list);
}

bool ofc_file_include_list_add_create(
	ofc_file_t* file,
	char* path)
{
	if (!file || !path)
		return false;

	if (!file->include)
	{
		file->include = (ofc_file_include_list_t*)malloc(
			sizeof(ofc_file_include_list_t));

		if (!file->include)
			return false;

		file->include->count = 0;
		file->include->path = NULL;
	}

	char** nlist
		= (char**)realloc(file->include->path,
			(sizeof(char*) * (file->include->count + 1)));
	if (!nlist) return NULL;

	file->include->path = nlist;
	file->include->path[file->include->count++] = strdup(path);

	return true;
}

void ofc_file_include_list_delete(
	ofc_file_include_list_t* list)
{
	if (!list)
		return;

	unsigned i;
	for (i = 0; i < list->count; i++)
		free(list->path[i]);

	free(list->path);
	free(list);
}


static bool line_empty(const char* ptr, unsigned len)
{
	if (!ptr || (len == 0))
		return true;

	unsigned i;
	for (i = 0; i < len; i++)
	{
		if (isspace(ptr[i]))
			continue;

		if (isprint(ptr[i]))
			return false;
	}

	return true;
}

static void ofc_file__print_include_loc(
	const ofc_file_t* include_file,
	const ofc_file_t* parent_file)
{
	if (!include_file || !parent_file)
		return;

	if (parent_file->parent)
		ofc_file__print_include_loc(parent_file, parent_file->parent);

	unsigned incl_row, incl_col;
	bool incl_pos = ofc_file_get_position(
	parent_file, ofc_sparse_file_pointer(
			include_file->include_stmt.sparse,
			include_file->include_stmt.string.base),
		&incl_row, &incl_col);

	fprintf(stderr, "%s:", parent_file->path);
	if (incl_pos)
		fprintf(stderr, "%u,%u:", (incl_row + 1), incl_col);
	fprintf(stderr, "\n  ");
}

static void ofc_file__debug_va(
	const ofc_file_t* file,
	const char* sol, const char* ptr,
	const char* type, const char* format, va_list args)
{
	unsigned row, col;
	bool positional = ofc_file_get_position(
		file, ptr, &row, &col);

	fprintf(stderr, "%s:", type);

	if (file)
	{
		const ofc_file_t* include_file = file;
		const ofc_file_t* parent_file = file->parent;

		ofc_file__print_include_loc(include_file, parent_file);

		if (file->path)
			fprintf(stderr, "%s:", file->path);
		if (positional)
			fprintf(stderr, "%u,%u:", (row + 1), col);

		fprintf(stderr, "\n");
	}

	va_list nargs;
	va_copy(nargs, args);
	int fmt_len = vsnprintf(NULL, 0, format, nargs);
	char fmt_str[fmt_len + 1];
	vsprintf(fmt_str, format, args);
	va_end(nargs);

	int indent = 0;
	if (file) indent += 2;
	if (!file || !file->parent) indent += 1;

	const char* base = fmt_str;
	unsigned i, len;
	for (i = 0, len = 0; i <= strlen(fmt_str); i++)
	{
		if ((fmt_str[i] == '\n')
			|| (fmt_str[i] == '\0'))
		{
			fprintf(stderr, "%*s", indent, "");
			fprintf(stderr, "%.*s\n", len, base);
			base = &fmt_str[i + 1];
			len = 0;
		}
		else
		{
			len++;
		}
	}

	if (positional)
	{
		if (!sol)
			sol = ptr;

		const char* s = file->strz;
		const char* p;
		for (p = file->strz; p < sol; p++)
		{
			if (ofc_is_vspace(*p))
				s = &p[1];
		}

		unsigned len = ((uintptr_t)ptr - (uintptr_t)s);
		for (; !ofc_is_vspace(s[len]) && (s[len] != '\0'); len++);

		/* Print line(s) above if line is empty. */
		while (line_empty(s, len)
			&& (s != file->strz))
		{
			const char* ns = file->strz;
			for (p = file->strz; p < s; p++)
			{
				if (ofc_is_vspace(*p))
					ns = &p[1];
			}
			len += ((uintptr_t)s - (uintptr_t)ns);
			s = ns;
		}

		fprintf(stderr, "%.*s\n", len, s);

		unsigned i;
		for (i = 0; i < col; i++)
			fprintf(stderr, " ");
		fprintf(stderr, "^\n");
	}
}

static unsigned ofc_file__error_count = 0;

bool ofc_file_no_errors(void)
{
	return (ofc_file__error_count == 0);
}

void ofc_file_error_va(
	const ofc_file_t* file,
	const char* sol, const char* ptr,
	const char* format, va_list args)
{
	ofc_file__debug_va(
		file, sol, ptr, "Error", format, args);
	ofc_file__error_count++;
}

void ofc_file_warning_va(
	const ofc_file_t* file,
	const char* sol, const char* ptr,
	const char* format, va_list args)
{
	if (!global_opts.no_warn)
	{
		ofc_file__debug_va(
			file, sol, ptr, "Warning", format, args);
	}
}



void ofc_file_error(
	const ofc_file_t* file, const char* ptr,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);
	ofc_file_error_va(file, NULL, ptr, format, args);
	va_end(args);
}

void ofc_file_warning(
	const ofc_file_t* file, const char* ptr,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);
	ofc_file_warning_va(file, NULL, ptr, format, args);
	va_end(args);
}
