#ifndef __preprocess_h__
#define __preprocess_h__

#include "file.h"
#include "rope.h"
#include "lang_opts.h"

typedef struct preprocess_s preprocess_t;

/* If a non-null preprocess_t is returned it will own the file.*/
preprocess_t* preprocess(file_t* file, lang_opts_t opts);
void preprocess_delete(preprocess_t* context);

const file_t* preprocess_original_file(const preprocess_t* context);
const rope_t* preprocess_unformat_rope(const preprocess_t* context);
const rope_t* preprocess_condense_rope(const preprocess_t* context);

bool preprocess_unformat_has_label(
	const preprocess_t* context, unsigned position,
	unsigned* number);
bool preprocess_condense_has_label(
	const preprocess_t* context, unsigned position,
	unsigned* number);

#endif
