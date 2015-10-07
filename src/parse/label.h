#ifndef __parse_label_h__
#define __parse_label_h__

#include <stdint.h>
#include "../str_ref.h"


typedef enum
{
	PARSE_LABEL_NUMBER,
	PARSE_LABEL_VARIABLE,
} parse_label_e;

typedef struct
{
	parse_label_e type;

	union
	{
		unsigned  number;
		str_ref_t variable;
	};
} parse_label_t;


unsigned parse_label(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_label_t* label);
bool parse_label_print(
	colstr_t* cs, const parse_label_t label);

#endif
