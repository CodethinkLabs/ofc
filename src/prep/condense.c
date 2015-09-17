#include "../prep.h"
#include "../fctype.h"
#include <stdlib.h>


sparse_t* prep_condense(sparse_t* unformat)
{
	const char* src = sparse_strz(unformat);
	if (!src) return NULL;

	sparse_t* condense
		= sparse_create_child(unformat);
	if (!condense) return NULL;

	unsigned i = 0;
	while (src[i] != '\0')
	{
		/* Skip whitespace. */
		for (; (src[i] != '\0') && is_hspace(src[i]); i++);

		if (src[i] == '\0')
			break;

		/* Parse non-whitespace. */
		const char* base = &src[i];
		unsigned size;
		for(size = 0; (src[i] != '\0') && !is_hspace(src[i]); size++, i++);

		/* Append non-whitespace to condense sparse. */
		if (!sparse_append_strn(condense, base, size))
		{
			sparse_delete(condense);
			return NULL;
		}
	}

	sparse_lock(condense);
	return condense;
}
