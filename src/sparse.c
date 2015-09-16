#include "sparse.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>


typedef struct
{
	const char* ptr;
	unsigned    len;
	unsigned    off;
} sparse_entry_t;

struct sparse_s
{
	const file_t*   file;
	const sparse_t* parent;

	unsigned len, count, max_count;
	sparse_entry_t* entry;

	char* strz;
};



sparse_t* sparse__create(
	const file_t* file, sparse_t* parent)
{
	sparse_t* sparse
		= (sparse_t*)malloc(
			sizeof(sparse_t));
	if (!sparse) return NULL;

	sparse->file   = file;
	sparse->parent = parent;

	sparse->len       = 0;
	sparse->count     = 0;
	sparse->max_count = 0;
	sparse->entry     = NULL;

	sparse->strz = NULL;

	return sparse;
}

sparse_t* sparse_create_file(const file_t* file)
{
	if (!file) return NULL;
	return sparse__create(file, NULL);
}

sparse_t* sparse_create_child(sparse_t* parent)
{
	if (!parent) return NULL;
	sparse_t* sparse = sparse__create(NULL, parent);
	if (!sparse) return NULL;

	/* Lock parent so it can't be modified. */
	sparse_lock(parent);

	return sparse;
}

void sparse_delete(sparse_t* sparse)
{
	if (!sparse)
		return;

	free(sparse->strz);
	free(sparse->entry);
	free(sparse);
}



unsigned sparse_len(const sparse_t* sparse)
{
	return (sparse ? sparse->len : 0);
}



bool sparse_append_strn(
	sparse_t* sparse,
	const char* src, unsigned len)
{
	if (!sparse)
		return false;
	if (len == 0)
		return true;
	if (!src)
		return false;

	/* If strz has been called disallow further modifications. */
	if (sparse->strz)
		return false;

	if (sparse->count >= sparse->max_count)
	{
		unsigned ncount = (sparse->max_count << 1);
		if (ncount == 0) ncount = 16;

		sparse_entry_t* nentry = (sparse_entry_t*)realloc(sparse->entry,
				(sizeof(sparse_entry_t) * ncount));
		if (!nentry) return false;
		sparse->entry = nentry;
		sparse->max_count = ncount;
	}

	sparse->entry[sparse->count].ptr = src;
	sparse->entry[sparse->count].len = len;
	sparse->entry[sparse->count].off = sparse->len;
	sparse->count++;
	sparse->len += len;

	return true;
}

void sparse_lock(sparse_t* sparse)
{
	if (!sparse || sparse->strz)
		return;

	sparse->strz = (char*)malloc(sparse->len + 1);
	if (!sparse->strz) return;

	unsigned i, j;
	for (i = 0, j = 0; i < sparse->count; j += sparse->entry[i++].len)
		memcpy(&sparse->strz[j], sparse->entry[i].ptr, sparse->entry[i].len);
	sparse->strz[j] = '\0';
}

const char* sparse_strz(const sparse_t* sparse)
{
	return (sparse ? sparse->strz : NULL);
}


bool sparse__ptr(
	const sparse_t* sparse, const char* ptr,
	sparse_entry_t* entry, unsigned* offset)
{
	if (!sparse || !sparse->strz || !ptr)
		return false;

	uintptr_t off = ((uintptr_t)ptr - (uintptr_t)sparse->strz);
	if (off >= sparse->len)
		return false;

	unsigned hi  = (sparse->count - 1);
	unsigned lo  = 0;
	unsigned mid;

	for (mid = lo + ((hi - lo) / 2); hi != lo; mid = lo + ((hi - lo) / 2))
	{
		unsigned start = sparse->entry[mid].off;
		unsigned end   = start + sparse->entry[mid].len;

		if (off < start)
			hi = (mid - 1);
		else if (off >= end)
			lo = (mid + 1);
		else
			break;
	}

	if (entry ) *entry  = sparse->entry[mid];
	if (offset) *offset = (off - sparse->entry[mid].off);
	return true;
}


bool sparse_sequential(
	const sparse_t* sparse, const char* ptr, unsigned size)
{
	if (!sparse || !ptr)
		return false;

	sparse_entry_t entry;
	unsigned offset;

	if (!sparse__ptr(
		sparse, ptr,
		&entry, &offset))
		return NULL;

	return ((offset + size) <= entry.len);
}

const char* sparse_parent_pointer(
	const sparse_t* sparse, const char* ptr)
{
	if (!sparse || !ptr)
		return NULL;

	sparse_entry_t entry;
	unsigned offset;

	if (!sparse__ptr(
		sparse, ptr,
		&entry, &offset))
		return NULL;

	return &entry.ptr[offset];
}

const char* sparse_file_pointer(
	const sparse_t* sparse, const char* ptr)
{
	sparse_entry_t entry;
	unsigned offset;

	if (!sparse__ptr(
		sparse, ptr,
		&entry, &offset))
		return NULL;

	const char* pptr = &entry.ptr[offset];

	if (sparse->parent)
		return sparse_file_pointer(sparse->parent, pptr);

	/* A sparse with no file or parent, can't have a file pointer. */
	return (sparse->file ? pptr : NULL);
}

bool sparse_file_position(
	const sparse_t* sparse, const char* ptr,
	const char** path, unsigned *row, unsigned* col)
{
	sparse_entry_t entry;
	unsigned offset;

	if (!sparse__ptr(
		sparse, ptr,
		&entry, &offset))
		return false;

	const char* pptr = &entry.ptr[offset];

	if (sparse->parent)
	{
		if (!sparse_file_position(
			sparse->parent, pptr, path, row, col))
			return false;
	}
	else if (sparse->file)
	{
		if (!file_get_position(sparse->file, pptr, row, col))
			return false;
		if (path) *path = file_get_path(sparse->file);
	}
	else
	{
		/* A sparse with no file or parent, can't have a file position. */
		return false;
	}

	return true;
}


lang_opts_t sparse_lang_opts(const sparse_t* sparse)
{
	if (!sparse)
		return LANG_OPTS_F77;
	if (sparse->file)
		return file_get_lang_opts(sparse->file);
	return sparse_lang_opts(sparse->parent);
}



#include <stdio.h>

void sparse_error(
	const sparse_t* sparse, const char* ptr,
	const char* format, ...)
{
	fprintf(stderr, "Error:");

	const char* path;
	unsigned row, col;
	if (sparse_file_position(
		sparse, ptr,
		&path, &row, &col))
		fprintf(stderr, "%s:%u,%u:",
			path, (row + 1), col);

	fprintf(stderr, " ");

	va_list args;
	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	fprintf(stderr, "\n");
}

void sparse_warning(
	const sparse_t* sparse, const char* ptr,
	const char* format, ...)
{
	fprintf(stderr, "Warning:");

	const char* path;
	unsigned row, col;
	if (sparse_file_position(
		sparse, ptr,
		&path, &row, &col))
		fprintf(stderr, "%s:%u,%u:",
			path, (row + 1), col);

	fprintf(stderr, " ");

	va_list args;
	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	fprintf(stderr, "\n");
}
