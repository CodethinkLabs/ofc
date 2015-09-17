#include "../prep.h"
#include <stdlib.h>


sparse_t* prep(file_t* file)
{
	sparse_t* unformat
		= prep_unformat(file);
	if (!unformat) return NULL;

	sparse_t* condense
		= prep_condense(unformat);
	sparse_delete(unformat);
	return condense;
}
