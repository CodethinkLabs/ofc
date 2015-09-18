#include "parse.h"


static unsigned parse_implicit__mask(
	const sparse_t* src, const char* ptr,
	uint32_t* mask)
{
	if (!isalpha(ptr[0]))
		return false;

	unsigned len = 1;
	char start = toupper(ptr[0]);
	char end   = start;

	if (ptr[len] == '-')
	{
		len += 1;
		end = 'Z';

		if (isalpha(ptr[len]))
			end = toupper(ptr[len++]);

		if (end < start)
		{
			sparse_warning(src, ptr,
				"Implicit character rule backwards");

			unsigned swap = start;
			start = end; end = swap;
		}

		if (end == start)
		{
			sparse_warning(src, ptr,
				"Implicit rule has redundant range");
		}
	}

	char j;
	uint32_t m;
	for (j = start, m = (1U << (start - 'A')); j <= end; j++, m <<= 1)
		*mask |= m;
	return len;
}

static unsigned parse_implicit__mask_list(
	const sparse_t* src, const char* ptr,
	uint32_t* mask)
{
	unsigned i = 0;
	if (ptr[i++] != '(')
		return 0;

	uint32_t m = 0;
	bool initial;
	for (initial = true; initial || (ptr[i] == ','); initial = false)
	{
		unsigned j = i + (initial ? 0 : 1);
		unsigned len = parse_implicit__mask(
			src, &ptr[j], &m);
		if (len == 0) break;
		i = j + len;
	}

	if (ptr[i++] != ')')
		return 0;

	if (m == 0)
	{
		sparse_warning(src, ptr,
			"Implicit rule is empty");
	}

	if (mask) *mask = m;
	return i;
}

parse_implicit_t* parse_implicit(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_implicit_t implicit;

	unsigned i;
	implicit.type = parse_type(src, ptr, &i);
	if (!implicit.type) return NULL;

	unsigned l = parse_implicit__mask_list(
		src, &ptr[i], &implicit.mask);
	if (l == 0)
	{
		parse_type_delete(implicit.type);
		return NULL;
	}
	i += l;

	parse_implicit_t* aimplicit
		= (parse_implicit_t*)malloc(
			sizeof(parse_implicit_t));
	if (!aimplicit)
	{
		parse_type_delete(implicit.type);
		return NULL;
	}
	*aimplicit = implicit;

	if (len) *len = i;
	return aimplicit;
}

void parse_implicit_delete(
	parse_implicit_t* implicit)
{
	if (!implicit)
		return;

	parse_type_delete(implicit->type);
	free(implicit);
}


parse_implicit_list_t* parse_implicit_list(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_implicit_list_t* list
		= (parse_implicit_list_t*)malloc(
			sizeof(parse_implicit_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->rule = NULL;
	unsigned i = parse_list(src, ptr,
		&list->count, (void***)&list->rule,
		(void*)parse_implicit,
		(void*)parse_implicit_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void parse_implicit_list_delete(
	parse_implicit_list_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->rule,
		(void*)parse_implicit_delete);
	free(list);
}
