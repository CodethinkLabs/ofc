#ifndef __prep_h__
#define __prep_h__

#include "file.h"
#include "sparse.h"

sparse_t* prep_unformat(file_t* file);
sparse_t* prep_condense(sparse_t* unformat);
sparse_t* prep(file_t* file);

#endif
