#ifndef __ofc_file_h__
#define __ofc_file_h__

#include <stdbool.h>
#include "lang_opts.h"

typedef struct ofc_file_s ofc_file_t;

/* Path must be valid for as long as the ofc_file_t* is */
ofc_file_t* ofc_file_create(const char* path, ofc_lang_opts_t opts);
ofc_file_t* ofc_file_create_include(const char* path, ofc_lang_opts_t opts, const char* include);
bool        ofc_file_reference(ofc_file_t* file);
void        ofc_file_delete(ofc_file_t* file);

const char*     ofc_file_get_path(const ofc_file_t* file);
const char*     ofc_file_get_include(const ofc_file_t* file);
const char*     ofc_file_get_strz(const ofc_file_t* file);
ofc_lang_opts_t ofc_file_get_lang_opts(const ofc_file_t* file);

char* ofc_file_include_path(
	const ofc_file_t* file, const char* path);

bool ofc_file_get_position(
	const ofc_file_t* file, const char* ptr,
	unsigned* row, unsigned* col);


#include <stdarg.h>

void ofc_file_error(
	const ofc_file_t* file, const char* ptr,
	const char* format, ...)
	__attribute__ ((format (printf, 3, 4)));
void ofc_file_warning(
	const ofc_file_t* file, const char* ptr,
	const char* format, ...)
	__attribute__ ((format (printf, 3, 4)));

void ofc_file_error_va(
	const ofc_file_t* file,
	const char* sol, const char* ptr,
	const char* format, va_list args);
void ofc_file_warning_va(
	const ofc_file_t* file,
	const char* sol, const char* ptr,
	const char* format, va_list args);

#endif
