#ifndef __ofc_colstr_h__
#define __ofc_colstr_h__

#include <stdbool.h>

typedef struct ofc_colstr_s ofc_colstr_t;

ofc_colstr_t* ofc_colstr_create(
	unsigned cols, unsigned ext);
void ofc_colstr_delete(ofc_colstr_t* cstr);

bool ofc_colstr_newline(
	ofc_colstr_t* cstr, unsigned* label);

#include <stdarg.h>

bool ofc_colstr_write_escaped(
	ofc_colstr_t* cstr, const char* base, unsigned size);

bool ofc_colstr_write(
	ofc_colstr_t* cstr, const char* base, unsigned size);

bool ofc_colstr_writef(
	ofc_colstr_t* cstr,
	const char* format, ...);

bool ofc_colstr_atomic_write(
	ofc_colstr_t* cstr, const char* base, unsigned size);

bool ofc_colstr_atomic_writef(
	ofc_colstr_t* cstr,
	const char* format, ...);

bool ofc_colstr_fdprint(ofc_colstr_t* cstr, int fd);

#endif
