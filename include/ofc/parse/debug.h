#ifndef __ofc_parse_debug_h__
#define __ofc_parse_debug_h__

#include <ofc/sparse.h>

typedef struct ofc_parse_debug_s ofc_parse_debug_t;

ofc_parse_debug_t* ofc_parse_debug_create(void);
void ofc_parse_debug_delete(ofc_parse_debug_t* stack);

unsigned ofc_parse_debug_position(const ofc_parse_debug_t* stack);
void ofc_parse_debug_rewind(ofc_parse_debug_t* stack, unsigned position);

void ofc_parse_debug_print(const ofc_parse_debug_t* stack);

#include <stdarg.h>

void ofc_parse_debug_error(
	ofc_parse_debug_t* stack,
	const ofc_sparse_t* src, const char* ptr,
	const char* format, ...);

void ofc_parse_debug_warning(
	ofc_parse_debug_t* stack,
	const ofc_sparse_t* src, const char* ptr,
	const char* format, ...);

#endif
