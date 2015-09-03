#ifndef __line_h__
#define __line_h__

#include <stdbool.h>
#include "rope.h"

typedef struct line_s line_t;

/* rope will be owned by line after this. */
line_t* line_create(unsigned* label, rope_t* rope);
void line_delete(line_t* line);

/* This operation is non-atomic, on failure rope may be partially appended. */
bool line_append_rope(line_t* line, const rope_t* rope);

const char* line_strz(const line_t* line, bool label);

bool line_position(
	const line_t* line, unsigned offset,
	const char** file, unsigned *row, unsigned* col);

#endif
