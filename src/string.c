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
	string->max = (string->size != 0 ? (string->size + 1) : 0);

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

bool string_printf(string_t* string, const char* format, ...)
{
	if (!string)
		return false;

	va_list args;
	va_start(args, format);

	va_list largs;
	va_copy(largs, args);
	int fsize = vsnprintf(
		NULL, 0, format, largs);
	va_end(largs);

	if (fsize <= 0)
	{
		va_end(args);
		return (fsize == 0);
	}

	unsigned nsize = string->size + fsize;
	if ((nsize + 1) > string->max)
	{
		unsigned nmax = string->max << 1;
		if (nmax < (nsize + 1))
			nmax = (nsize + 1);
		char* nbase = realloc(
			string->base, nmax);
		if (!nbase)
		{
			va_end(args);
			return false;
		}

		string->base = nbase;
		string->max  = nmax;
	}

	unsigned psize = vsnprintf(
		&string->base[string->size],
		string->max, format, args);
	if (psize != (unsigned)fsize)
	{
		string->base[string->size] = '\0';
		va_end(args);
		return false;
	}
	va_end(args);

	string->size += psize;
	return true;
}

bool string_append(string_t* base_str, const string_t* append_str)
{
	return string_printf(base_str, "%.*s",
		append_str->size, append_str->base);
}

bool string_append_escaped(string_t* base_str, const string_t* append_str)
{
	char* escaped_str = (char*)malloc(append_str->size * 2);

	unsigned i;
	for (i = 0; i < append_str->size; i++)
	{
		switch (append_str->base[i])
		{
			case '\r':
				escaped_str[i++] = '\\';
				escaped_str[i] = 'r';
				break;
			case '\n':
				escaped_str[i++] = '\\';
				escaped_str[i] = 'n';
				break;
			case '\v':
				escaped_str[i++] = '\\';
				escaped_str[i] = 'n';
				break;
			case '\t':
				escaped_str[i++] = '\\';
				escaped_str[i] = 't';
				break;
			case '\"':
				escaped_str[i++] = '\\';
				escaped_str[i] = '\"';
				break;
			case '\'':
				escaped_str[i++] = '\\';
				escaped_str[i] = '\'';
				break;
			case '\\':
				escaped_str[i++] = '\\';
				escaped_str[i] = '\\';
				break;

			/* TODO - Implement all possible escape sequences. */

			default:
				escaped_str[i] = append_str->base[i];
				break;
		}
	}

	return string_printf(
		base_str, "%.*s",
		i, escaped_str);
}
