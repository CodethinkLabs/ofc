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
	file_t*   file;
	sparse_t* parent;

	unsigned len, count, max_count;
	sparse_entry_t* entry;

	char* strz;

	label_table_t* labels;

	unsigned ref;
};



sparse_t* sparse__create(
	file_t* file, sparse_t* parent)
{
	sparse_t* sparse
		= (sparse_t*)malloc(
			sizeof(sparse_t));
	if (!sparse) return NULL;

	sparse->labels
		= label_table_create();
	if (!sparse->labels)
	{
		free(sparse);
		return NULL;
	}

	sparse->file   = file;
	sparse->parent = parent;

	sparse->len       = 0;
	sparse->count     = 0;
	sparse->max_count = 0;
	sparse->entry     = NULL;

	sparse->strz = NULL;

	sparse->ref = 0;

	return sparse;
}

sparse_t* sparse_create_file(file_t* file)
{
	if (!file_reference(file))
		return NULL;
	sparse_t* sparse
		= sparse__create(file, NULL);
	if (!sparse) file_delete(file);
	return sparse;
}

sparse_t* sparse_create_child(sparse_t* parent)
{
	if (!sparse_reference(parent))
		return NULL;

	sparse_t* sparse
		= sparse__create(NULL, parent);
	if (!sparse)
	{
		sparse_delete(parent);
		return NULL;
	}

	/* Lock parent so it can't be modified. */
	sparse_lock(parent);

	return sparse;
}

bool sparse_reference(sparse_t* sparse)
{
	if (!sparse)
		return false;

	unsigned nref = sparse->ref + 1;
	if (nref == 0) return false;

	sparse->ref += 1;
	return true;
}

void sparse_delete(sparse_t* sparse)
{
	if (!sparse)
		return;

	if (sparse->ref > 0)
	{
		sparse->ref -= 1;
		return;
	}

	sparse_delete(sparse->parent);
	file_delete(sparse->file);

	label_table_delete(sparse->labels);

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


static bool sparse__ptr(
	const sparse_t* sparse, const char* ptr,
	sparse_entry_t* entry, unsigned* offset,
	sparse_entry_t** prev)
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

	off -= sparse->entry[mid].off;

	if (prev  ) *prev   = ((off == 0) && (mid > 0) ? &sparse->entry[mid - 1] : NULL);
	if (entry ) *entry  = sparse->entry[mid];
	if (offset) *offset = off;
	return true;
}

static const file_t* sparse__file(
	const sparse_t* sparse)
{
	if (!sparse)
		return NULL;

	if (sparse->file)
		return sparse->file;

	return sparse__file(
		sparse->parent);
}


bool sparse_label_add(
	sparse_t* sparse, unsigned number)
{
	if (!sparse || sparse->strz)
		return false;
	return label_table_add(
		sparse->labels, sparse->len, number);
}

bool sparse_label_find(
	const sparse_t* sparse, const char* ptr, unsigned* number)
{
	if (!sparse || !sparse->strz)
		return false;

	unsigned offset = ((uintptr_t)ptr - (uintptr_t)sparse->strz);

	if (label_table_find(
		sparse->labels, offset, number))
		return true;

	if (!sparse->parent)
		return false;

	sparse_entry_t entry;
	sparse_entry_t* prev = NULL;

	if (!sparse__ptr(sparse, ptr,
		&entry, &offset, &prev))
		return false;

	/* If we're at an the start of an entry, ensure there's no label attached
	   to the end of the previous entry. */
	if (prev && sparse_label_find(
		sparse->parent, &prev->ptr[prev->len], number))
		return true;

	return sparse_label_find(
		sparse->parent, &entry.ptr[offset], number);
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
		&entry, &offset, NULL))
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
		&entry, &offset, NULL))
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
		&entry, &offset, NULL))
		return NULL;

	const char* pptr = &entry.ptr[offset];

	if (sparse->parent)
		return sparse_file_pointer(sparse->parent, pptr);

	/* A sparse with no file or parent, can't have a file pointer. */
	return (sparse->file ? pptr : NULL);
}


lang_opts_t sparse_lang_opts(const sparse_t* sparse)
{
	if (!sparse)
		return LANG_OPTS_F77;
	if (sparse->file)
		return file_get_lang_opts(sparse->file);
	return sparse_lang_opts(sparse->parent);
}


char* sparse_relative_path(
	const sparse_t* sparse, const char* path)
{
	if (!sparse)
		return strdup(path);

	return file_relative_path(
		sparse__file(sparse), path);
}



void sparse_error(
	const sparse_t* sparse, const char* ptr,
	const char* format, ...)
{
	const file_t* file = sparse__file(sparse);
	const char*   fptr = sparse_file_pointer(sparse, ptr);

	va_list args;
	va_start(args, format);
	file_error_va(file, fptr, format, args);
	va_end(args);
}

void sparse_warning(
	const sparse_t* sparse, const char* ptr,
	const char* format, ...)
{
	const file_t* file = sparse__file(sparse);
	const char*   fptr = sparse_file_pointer(sparse, ptr);

	va_list args;
	va_start(args, format);
	file_warning_va(file, fptr, format, args);
	va_end(args);
}
