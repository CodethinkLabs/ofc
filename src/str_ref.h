#ifndef __str_ref_h__
#define __str_ref_h__

#include <stdlib.h>
#include <stdbool.h>

typedef struct
{
	const char* base;
	unsigned    size;
} str_ref_t;

#define STR_REF_EMPTY (str_ref_t){ .base = NULL, .size = 0 }

static inline str_ref_t str_ref(const char* base, unsigned size)
	{ return (str_ref_t){ base, size }; }

bool str_ref_empty(const str_ref_t ref);
bool str_ref_equal(const str_ref_t a, const str_ref_t b);
bool str_ref_equal_ci(const str_ref_t a, const str_ref_t b);
bool str_ref_print(int fd, const str_ref_t str_ref);

#endif
