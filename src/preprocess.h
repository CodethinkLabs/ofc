#ifndef __preprocess_h__
#define __preprocess_h__

#include "file.h"
#include "sparse.h"
#include "label_table.h"

typedef struct preprocess_s preprocess_t;

/* If a non-null preprocess_t is returned it will own the file.*/
preprocess_t* preprocess(file_t* file);
void preprocess_delete(preprocess_t* context);

const file_t*        preprocess_original_file(const preprocess_t* context);
const sparse_t*      preprocess_unformat_sparse(const preprocess_t* context);
const sparse_t*      preprocess_condense_sparse(const preprocess_t* context);
const label_table_t* preprocess_labels(const preprocess_t* context);

#endif
