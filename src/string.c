#include "string.h"
#include "util.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


string_t string_create(const char* base, unsigned size)
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

string_t string_copy(const string_t src)
{
	return string_create(
		src.base, src.size);
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

bool string_print(int fd, const string_t string)
{
	return dprintf_bool(fd, "%.*s",
		string.size, string.base);
}

bool string_print_escaped(int fd, const string_t string)
{
	unsigned i;
	for (i = 0; i < string.size; i++)
	{
		const char* str = NULL;
		switch (string.base[i])
		{
			case '\r':
				str = "\\r";
				break;
			case '\n':
				str = "\\n";
				break;
			case '\v':
				str = "\\v";
				break;
			case '\t':
				str = "\\n";
				break;
			case '\"':
				str = "\\\"";
				break;
			case '\'':
				str = "\\'";
				break;
			case '\\':
				str = "\\\\";
				break;

			/* TODO - Implement all possible escape sequences. */

			default:
				if (!dprintf_bool(
					fd, "%c", string.base[i]))
					return false;
				break;
		}

		if (str && !dprintf_bool(fd, "%s", str))
			return false;
	}

	return true;
}
