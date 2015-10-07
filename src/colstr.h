#ifndef __colstr_h__
#define __colstr_h__

#include <stdbool.h>

typedef struct colstr_s colstr_t;

colstr_t* colstr_create(unsigned cols);
void colstr_delete(colstr_t* cstr);

bool colstr_newline(
	colstr_t* cstr, unsigned* label);

#include <stdarg.h>

bool colstr_write_escaped(
	colstr_t* cstr, const char* base, unsigned size);

bool colstr_write(
	colstr_t* cstr, const char* base, unsigned size);

bool colstr_writef(
	colstr_t* cstr,
	const char* format, ...);

bool colstr_atomic_write(
	colstr_t* cstr, const char* base, unsigned size);

bool colstr_atomic_writef(
	colstr_t* cstr,
	const char* format, ...);

bool colstr_fdprint(colstr_t* cstr, int fd);

#endif
