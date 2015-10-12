#ifndef __ofc_prep_h__
#define __ofc_prep_h__

#include "file.h"
#include "sparse.h"

ofc_sparse_t* ofc_prep_unformat(ofc_file_t* file);
ofc_sparse_t* ofc_prep_condense(ofc_sparse_t* unformat);
ofc_sparse_t* ofc_prep(ofc_file_t* file);

#endif
