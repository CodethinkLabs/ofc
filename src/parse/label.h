#ifndef __ofc_parse_label_h__
#define __ofc_parse_label_h__

#include <stdint.h>
#include "../str_ref.h"


typedef enum
{
	OFC_PARSE_LABEL_NUMBER,
	OFC_PARSE_LABEL_VARIABLE,
} ofc_parse_label_e;

typedef struct
{
	ofc_parse_label_e type;

	union
	{
		unsigned      number;
		ofc_str_ref_t variable;
	};
} ofc_parse_label_t;


unsigned ofc_parse_label(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_label_t* label);
bool ofc_parse_label_print(
	ofc_colstr_t* cs, const ofc_parse_label_t label);

#endif
