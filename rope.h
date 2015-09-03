#ifndef __rope_h__
#define __rope_h__

#include <stdbool.h>

typedef struct rope_s rope_t;

rope_t* rope_create(void);
void rope_delete(rope_t* rope);

unsigned rope_len(const rope_t* rope);

bool rope_append_strn(
	rope_t* rope,
	const char* file, unsigned row, unsigned col,
	const char* src, unsigned len);

/* This operation is non-atomic, on failure rope may be partially appended. */
bool rope_append_rope(rope_t* a, const rope_t* b);

const char* rope_strz(const rope_t* rope);

bool rope_position(
	const rope_t* rope, unsigned offset,
	const char** file, unsigned *row, unsigned* col);

#endif
