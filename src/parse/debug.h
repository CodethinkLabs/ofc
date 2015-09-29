#ifndef __parse_debug_h__
#define __parse_debug_h__

#include "../sparse.h"

typedef struct parse_debug_s parse_debug_t;

parse_debug_t* parse_debug_create(void);
void parse_debug_delete(parse_debug_t* stack);

unsigned parse_debug_position(const parse_debug_t* stack);
void parse_debug_rewind(parse_debug_t* stack, unsigned position);

void parse_debug_print(const parse_debug_t* stack);

#include <stdarg.h>

void parse_debug_error(
	parse_debug_t* stack,
	const sparse_t* src, const char* ptr,
	const char* format, ...);

void parse_debug_warning(
	parse_debug_t* stack,
	const sparse_t* src, const char* ptr,
	const char* format, ...);

#endif
