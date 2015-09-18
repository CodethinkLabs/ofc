#include "parse.h"


unsigned parse_list(
	const sparse_t* sparse, const char* ptr,
	unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const sparse_t*, const char*, unsigned*),
	void (*elem_delete)(void*))
{
	unsigned orig_count = *elem_count;
	unsigned max_count = *elem_count;

	unsigned i = 0;

	bool initial;
	for (initial = true; initial || (ptr[i] == ','); initial = false)
	{
		unsigned j = i + (initial ? 0 : 1);

		unsigned l;
		void* e = elem_parse(sparse, &ptr[j], &l);
		if (!e) return 0;
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
