#ifndef __ofc_string_h__
#define __ofc_string_h__

#include <stdbool.h>

typedef struct
{
	char*    base;
	unsigned size;
} ofc_string_t;

ofc_string_t* ofc_string_create(const char* base, unsigned size);
ofc_string_t* ofc_string_copy(const ofc_string_t* src);
void          ofc_string_delete(ofc_string_t* string);

bool ofc_string_empty(const ofc_string_t* string);

/* May return truncated string if string contains nul characters. */
const char* ofc_string_strz(const ofc_string_t* string);

unsigned ofc_string_length(const ofc_string_t* string);
bool     ofc_string_equal(const ofc_string_t a, const ofc_string_t b);

#endif
