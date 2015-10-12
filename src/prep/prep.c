#include <ofc/prep.h>
#include <stdlib.h>


ofc_sparse_t* ofc_prep(ofc_file_t* file)
{
	ofc_sparse_t* unformat
		= ofc_prep_unformat(file);
	if (!unformat) return NULL;

	ofc_sparse_t* condense
		= ofc_prep_condense(unformat);
	ofc_sparse_delete(unformat);
	return condense;
}
