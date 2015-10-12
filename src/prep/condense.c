#include "../prep.h"
#include "../fctype.h"
#include <stdlib.h>


ofc_sparse_t* ofc_prep_condense(ofc_sparse_t* unformat)
{
	const char* src = ofc_sparse_strz(unformat);
	if (!src) return NULL;

	ofc_sparse_t* condense
		= ofc_sparse_create_child(unformat);
	if (!condense) return NULL;

	unsigned i = 0;
	while (src[i] != '\0')
	{
		/* Skip whitespace. */
		for (; (src[i] != '\0') && ofc_is_hspace(src[i]); i++);

		if (src[i] == '\0')
			break;

		/* Parse non-whitespace. */
		const char* base = &src[i];
		unsigned size;
		for(size = 0; (src[i] != '\0') && !ofc_is_hspace(src[i]); size++, i++);

		/* Append non-whitespace to condense sparse. */
		if (!ofc_sparse_append_strn(condense, base, size))
		{
			ofc_sparse_delete(condense);
			return NULL;
		}
	}

	ofc_sparse_lock(condense);
	return condense;
}
