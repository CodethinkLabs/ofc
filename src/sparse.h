#ifndef __sparse_h__
#define __sparse_h__

#include <stdbool.h>
#include "file.h"

typedef struct sparse_s sparse_t;

sparse_t* sparse_create_file(const file_t* file);
sparse_t* sparse_create_child(sparse_t* parent);
void      sparse_delete(sparse_t* sparse);

unsigned sparse_len(const sparse_t* sparse);

bool sparse_append_strn(sparse_t* sparse, const char* src, unsigned len);

/* No modifications are allowed after this call. */
void sparse_lock(sparse_t* sparse);

/* This will return NULL unless the sparse is locked. */
const char* sparse_strz(const sparse_t* sparse);

const char* sparse_file_pointer(
	const sparse_t* sparse, const char* ptr);

bool sparse_file_position(
	const sparse_t* sparse, const char* ptr,
	const char** path, unsigned *row, unsigned* col);

#endif
