#include "rope.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


typedef struct rope__string_s rope__string_t;

struct rope_s
{
	char*           strz;
	rope__string_t* base;
};

struct rope__string_s
{
	const char* file;
	unsigned    row, col;
	const char* src;
	unsigned    len;

	rope__string_t* next;
};


rope_t* rope_create(void)
{
	rope_t* rope = (rope_t*)malloc(sizeof(rope_t));
	if (!rope) return NULL;

	rope->strz = NULL;
	rope->base = NULL;
	return rope;
}

void rope_delete(rope_t* rope)
{
	if (!rope)
		return;

	free(rope->strz);

	rope__string_t* str = rope->base;
	while (str)
	{
		rope__string_t* next = str->next;
		free(str);
		str = next;
	}

	free(rope);
}


unsigned rope_len(const rope_t* rope)
{
	if (!rope || !rope->base)
		return 0;

	unsigned len = 0;
	rope__string_t* str;
	for (str = rope->base; str; str = str->next)
		len += str->len;
	return len;
}


bool rope_append_strn(
	rope_t* rope,
	const char* file, unsigned row, unsigned col,
	const char* src, unsigned len)
{
	if (!rope)
		return false;

	rope__string_t* str
		= (rope__string_t*)malloc(
			sizeof(rope__string_t));
	if (!str) return NULL;

	str->file = file;
	str->row  = row;
	str->col  = col;
	str->src  = src;
	str->len  = len;

	str->next = NULL;

	if (!rope->base)
	{
		rope->base = str;
	}
	else
	{
		rope__string_t* last;
		for (last = rope->base; last->next; last = last->next);
		last->next = str;
	}

	free(rope->strz);
	rope->strz = NULL;

	return true;
}

bool rope_append_rope(rope_t* a, const rope_t* b)
{
	if (!a) return false;
	if (!b) return true;

	rope__string_t* str;
	for (str = b->base; str; str = str->next)
	{
		if (!rope_append_strn(a,
			str->file, str->row, str->col,
			str->src, str->len))
			return false;
	}

	return true;
}


const char* rope_strz(const rope_t* rope)
{
	if (!rope)
		return NULL;

	if (!rope->strz)
	{
		unsigned len = rope_len(rope);
		char* strz = (char*)malloc(len + 1);
		if (!strz) return NULL;

		unsigned i;
		rope__string_t* str;
		for (str = rope->base, i = 0; str; i += str->len, str = str->next)
			memcpy(&strz[i], str->src, str->len);
		strz[i] = '\0';

		/* Cast to non-const because we're caching and
           the behaviour is side-effect free. */
		((rope_t*)rope)->strz = strz;
	}

	return (const char*)rope->strz;
}

bool rope_position(
	const rope_t* rope, unsigned offset,
	const char** file, unsigned *row, unsigned* col)
{
	if (!rope)
		return false;

	unsigned i;
	rope__string_t* str;
	for (str = rope->base, i = 0; str; i += str->len, str = str->next)
	{
		if ((offset >= i) && ((offset - i) < str->len))
		{
			if (file) *file = str->file;
			if (row ) *row  = str->row;
			if (col ) *col  = str->col + (offset - i);
			return true;
		}
	}

	return false;
}
