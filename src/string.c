#include <ofc/string.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>

ofc_string_t* ofc_string_create(const char* base, unsigned size)
{
	ofc_string_t* string
		= (ofc_string_t*)malloc(
			sizeof(ofc_string_t));
	if (!string)
		return NULL;

	string->base = (size == 0 ? NULL : (char*)malloc(size + 1));
	string->size = (string->base ? size : 0);
	if (string->base)
	{
		if (base)
		{
			memcpy(string->base, base, size);
			string->base[size] = '\0';
		}
		else
		{
			memset(string->base, '\0', (size + 1));
		}
	}

	return string;
}

ofc_string_t* ofc_string_copy(const ofc_string_t* src)
{
	return ofc_string_create(
		src->base, src->size);
}

void ofc_string_delete(ofc_string_t* string)
{
	if (!string)
		return;

	free(string->base);
	free(string);
}


bool ofc_string_empty(const ofc_string_t* string)
{
	return (!string->base || (string->size == 0));
}


const char* ofc_string_strz(const ofc_string_t* string)
{
	if (!string)
		return NULL;
	return string->base;
}

unsigned ofc_string_length(const ofc_string_t* string)
{
	return (string->base ? string->size : 0);
}

bool ofc_string_equal(const ofc_string_t a, const ofc_string_t b)
{
	if (a.size != b.size)
		return false;

	if (a.base == b.base)
		return true;

	return (memcmp(a.base, b.base, a.size) == 0);
}
