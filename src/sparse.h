#ifndef __sparse_h__
#define __sparse_h__

#include <stdbool.h>
#include "file.h"
#include "label_table.h"

typedef struct sparse_s sparse_t;

sparse_t* sparse_create_file(file_t* file);
sparse_t* sparse_create_child(sparse_t* parent);
bool      sparse_reference(sparse_t* sparse);
void      sparse_delete(sparse_t* sparse);

unsigned sparse_len(const sparse_t* sparse);

bool sparse_append_strn(sparse_t* sparse, const char* src, unsigned len);

/* No modifications are allowed after this call. */
void sparse_lock(sparse_t* sparse);

/* This will return NULL unless the sparse is locked. */
const char* sparse_strz(const sparse_t* sparse);

bool sparse_label_add(
	sparse_t* sparse, unsigned number);
bool sparse_label_find(
	const sparse_t* sparse, const char* ptr, unsigned* number);

bool sparse_sequential(
	const sparse_t* sparse, const char* ptr, unsigned size);

const char* sparse_parent_pointer(
	const sparse_t* sparse, const char* ptr);

const char* sparse_file_pointer(
	const sparse_t* sparse, const char* ptr);

lang_opts_t sparse_lang_opts(const sparse_t* sparse);

const char* sparse_get_include(
	const sparse_t* sparse);
char* sparse_include_path(
	const sparse_t* sparse, const char* path);


#include <stdarg.h>

void sparse_error(
	const sparse_t* sparse, const char* ptr,
	const char* format, ...);
void sparse_warning(
	const sparse_t* sparse, const char* ptr,
	const char* format, ...);

#endif
