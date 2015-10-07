#include "string.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>

string_t* string_create(const char* base, unsigned size)
{
	string_t* string = (string_t*)malloc(sizeof(string_t));
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

string_t* string_copy(const string_t* src)
{
	return string_create(
		src->base, src->size);
}

void string_delete(string_t* string)
{
	if (!string)
		return;

	free(string->base);
	free(string);
}


bool string_empty(const string_t* string)
{
	return (!string->base || (string->size == 0));
}


const char* string_strz(const string_t* string)
{
	if (!string)
		return NULL;
	return string->base;
}

unsigned string_length(const string_t* string)
{
	return (string->base ? string->size : 0);
}

bool string_equal(const string_t a, const string_t b)
{
	if (a.size != b.size)
		return false;

	if (a.base == b.base)
		return true;

	return (memcmp(a.base, b.base, a.size) == 0);
}
