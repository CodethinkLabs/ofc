#include "parse.h"


static unsigned parse_list__seperator_optional(
	const sparse_t* sparse, const char* ptr,
	parse_debug_t* debug,
	char seperator, bool optional,
	unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const sparse_t*, const char*, parse_debug_t*, unsigned*),
	void (*elem_delete)(void*))
{
	unsigned orig_count = *elem_count;
	unsigned max_count = *elem_count;

	unsigned i = 0;

	bool initial = true;
	while (true)
	{
		unsigned j = i;

		if (!initial && (seperator != '\0'))
		{
			if (ptr[j] == seperator)
				j += 1;
			else if (!optional)
				break;
		}
		initial = false;

		unsigned l;
		void* e = elem_parse(
			sparse, &ptr[j], debug, &l);
		if (!e) break;
		j += l;

		if (*elem_count >= max_count)
		{
			max_count <<= 1;
			if (max_count == 0)
				max_count = 4;
			void** nelem = realloc(*elem,
				(max_count * sizeof(void*)));
			if (!nelem)
			{
				elem_delete(e);

				unsigned n;
				for (n = orig_count; n < *elem_count; n++)
					elem_delete((*elem)[n]);
				*elem_count = orig_count;

				if (orig_count == 0)
				{
					free(*elem);
					*elem = NULL;
				}

				return 0;
			}
			*elem = nelem;
		}

		(*elem)[(*elem_count)++] = e;
		i = j;
	}

	return i;
}

unsigned parse_list(
	const sparse_t* sparse, const char* ptr,
	parse_debug_t* debug,
	char seperator, unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const sparse_t*, const char*, parse_debug_t*, unsigned*),
	void (*elem_delete)(void*))
{
	return parse_list__seperator_optional(
		sparse, ptr, debug, seperator, false,
		elem_count, elem, elem_parse, elem_delete);
}

unsigned parse_list_seperator_optional(
	const sparse_t* sparse, const char* ptr,
	parse_debug_t* debug,
	char seperator, unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const sparse_t*, const char*, parse_debug_t*, unsigned*),
	void (*elem_delete)(void*))
{
	return parse_list__seperator_optional(
		sparse, ptr, debug, seperator, true,
		elem_count, elem, elem_parse, elem_delete);
}


bool parse_list_copy(
	unsigned* dst_count, void*** dst,
	unsigned  src_count, const void** src,
	void* (*elem_copy)(const void*),
	void (*elem_delete)(void*))
{
	if (!elem_copy || !src || !dst || !dst_count)
		return false;

	void** copy = (void**)malloc(
		src_count * sizeof(void*));
	if (!copy) return false;

	unsigned i;
	for (i = 0; i < src_count; i++)
	{
		copy[i] = elem_copy(src[i]);
		if (!copy[i])
		{
			if (elem_delete)
			{
				unsigned j;
				for (j = 0; j < i; j++)
					elem_delete(copy[j]);
			}
			free(copy);
			return false;
		}
	}

	*dst = copy;
	*dst_count = src_count;
	return true;
}

void parse_list_delete(
	unsigned elem_count, void** elem,
	void (*elem_delete)(void*))
{
	if (!elem)
		return;

	unsigned i;
	for (i = 0; i < elem_count; i++)
		elem_delete(elem[i]);
	free(elem);
}


bool parse_list_print(
	int fd,
	unsigned elem_count, const void** elem,
	bool (*elem_print)(int, const void*))
{
	if (!elem || !elem_print)
		return false;

	unsigned i;
	for (i = 0; i < elem_count; i++)
	{
		if ((i > 0) && !dprintf_bool(
			fd, ", "))
			return false;

		if (!elem_print(fd, elem[i]))
			return false;
	}

	return true;
}
