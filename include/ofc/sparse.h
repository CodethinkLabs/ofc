#ifndef __ofc_sparse_h__
#define __ofc_sparse_h__

#include <stdbool.h>
#include "file.h"
#include "label_table.h"

typedef struct ofc_sparse_s ofc_sparse_t;

ofc_sparse_t* ofc_sparse_create_file(ofc_file_t* file);
ofc_sparse_t* ofc_sparse_create_child(ofc_sparse_t* parent);
bool      ofc_sparse_reference(ofc_sparse_t* sparse);
void      ofc_sparse_delete(ofc_sparse_t* sparse);

unsigned ofc_sparse_len(const ofc_sparse_t* sparse);

bool ofc_sparse_append_strn(ofc_sparse_t* sparse, const char* src, unsigned len);

/* No modifications are allowed after this call. */
void ofc_sparse_lock(ofc_sparse_t* sparse);

/* This will return NULL unless the sparse is locked. */
const char* ofc_sparse_strz(const ofc_sparse_t* sparse);

bool ofc_sparse_label_add(
	ofc_sparse_t* sparse, unsigned number);
bool ofc_sparse_label_find(
	const ofc_sparse_t* sparse, const char* ptr, unsigned* number);

bool ofc_sparse_sequential(
	const ofc_sparse_t* sparse, const char* ptr, unsigned size);

const char* ofc_sparse_parent_pointer(
	const ofc_sparse_t* sparse, const char* ptr);

ofc_lang_opts_t ofc_sparse_lang_opts(const ofc_sparse_t* sparse);

const char* ofc_sparse_get_include(
	const ofc_sparse_t* sparse);
char* ofc_sparse_include_path(
	const ofc_sparse_t* sparse, const char* path);


#include <stdarg.h>

void ofc_sparse_error(
	const ofc_sparse_t* sparse, const char* ptr,
	const char* format, ...);
void ofc_sparse_warning(
	const ofc_sparse_t* sparse, const char* ptr,
	const char* format, ...);

#endif
