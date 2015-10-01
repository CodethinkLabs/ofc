#include "string.h"
#include "util.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

string_t* string_create(const char* base, unsigned size)
{
	string_t* string = (string_t*)malloc(sizeof(string_t));
	if (!string)
		return NULL;

	string->base = (char*)malloc(size + 1);
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
	va_list args;

	while (true)
	{
		unsigned avalible_space = string->size - strlen(string->base);

		va_start(args, format);
		int chars_written = vsprintf(string->base, format, args);
		va_end(args);

		if (chars_written < 0) return false;

		if ((unsigned)chars_written < avalible_space) return true;

		unsigned new_size = string->size * 2;
		while (new_size < (string->size + chars_written + 1))
			new_size = new_size * 2;

		char* new_base = (char *)realloc (string->base, new_size);

		if (new_base)
		{
			string->size = new_size;
			string->base = new_base;
		}
		else
		{
			return false;
		}
	}
}

bool string_print(int fd, const string_t* string)
{
	return dprintf_bool(fd, "%.*s",
		string->size, string->base);
}

bool string_print_escaped(int fd, const string_t* string)
{
	unsigned i;
	for (i = 0; i < string->size; i++)
	{
		const char* str = NULL;
		switch (string->base[i])
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
					fd, "%c", string->base[i]))
					return false;
				break;
		}

		if (str && !dprintf_bool(fd, "%s", str))
			return false;
	}

	return true;
}
