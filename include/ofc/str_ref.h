#ifndef __ofc_str_ref_h__
#define __ofc_str_ref_h__

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <ofc/colstr.h>

typedef struct
{
	const char* base;
	unsigned    size;
} ofc_str_ref_t;

#define OFC_STR_REF_EMPTY (ofc_str_ref_t){ .base = NULL, .size = 0 }

static inline ofc_str_ref_t ofc_str_ref(const char* base, unsigned size)
	{ return (ofc_str_ref_t){ base, size }; }

bool    ofc_str_ref_empty(const ofc_str_ref_t ref);
uint8_t ofc_str_ref_hash(const ofc_str_ref_t ref);
bool    ofc_str_ref_equal(const ofc_str_ref_t a, const ofc_str_ref_t b);
bool    ofc_str_ref_equal_ci(const ofc_str_ref_t a, const ofc_str_ref_t b);
bool    ofc_str_ref_print(ofc_colstr_t* cs, const ofc_str_ref_t str_ref);

#endif
