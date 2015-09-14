#include "string.h"
#include <stdlib.h>
#include <string.h>


string_t string_create(char* base, unsigned size)
{
	string_t string;
	string.base = (char*)malloc(size + 1);
	string.size = (string.base ? size : 0);
	if (string.base)
	{
		if (base)
		{
			memcpy(string.base, base, size);
			string.base[size] = '\0';
		}
		else
		{
			memset(string.base, '\0', (size + 1));
		}
	}
	return string;
}

void string_delete(string_t string)
{
	free(string.base);
}


bool string_empty(const string_t string)
{
	return (!string.base || (string.size == 0));
}


const char* string_strz(const string_t string)
{
	return string.base;
}

unsigned string_length(const string_t string)
{
	return (string.base ? string.size : 0);
}

bool string_equal(const string_t a, const string_t b)
{
	if (a.size != b.size)
		return false;

	if (a.base == b.base)
		return true;

	return (memcmp(a.base, b.base, a.size) == 0);
}
