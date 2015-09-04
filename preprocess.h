#ifndef __preprocess_h__
#define __preprocess_h__

#include "file.h"
#include "lang_opts.h"

typedef struct preprocess_s preprocess_t;

/* If a non-null preprocess_t is returned it will own the file.*/
preprocess_t* preprocess(file_t* file, lang_opts_t opts);
void preprocess_delete(preprocess_t* context);

const char* preprocess_strz(const preprocess_t* context);

bool preprocess_debug_position(
	const preprocess_t* context, unsigned position,
	const char** file, unsigned *row, unsigned* col);

bool preprocess_has_label(
	const preprocess_t* context, unsigned position,
	unsigned* number);

#endif
