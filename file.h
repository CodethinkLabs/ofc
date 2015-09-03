#ifndef __file_h__
#define __file_h__

#include <stdbool.h>
#include "lang_opts.h"
#include "line.h"
#include "rope.h"

typedef struct file_s file_t;

/* Path must be valid for as long as the file_t* is */
file_t* file_create(const char* path, lang_opts_t opts);
void    file_delete(file_t* file);

const char* file_get_path(const file_t* file);
const char* file_get_strz(const file_t* file);
lang_opts_t file_get_lang_opts(const file_t* file);

/* Line will be owned by file after this. */
bool file_append_line(file_t* file, line_t* line);

/* Rope will not be owned by file after this. */
bool file_append_rope(file_t* file, rope_t* rope);

bool file_foreach_line(
	const file_t* file, void* param,
	bool (*func)(const line_t* line, void* param));

#endif
