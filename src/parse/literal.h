#ifndef __ofc_parse_literal_h__
#define __ofc_parse_literal_h__

#include <stdint.h>
#include "../string.h"


typedef enum
{
	OFC_PARSE_LITERAL_BINARY,
	OFC_PARSE_LITERAL_OCTAL,
	OFC_PARSE_LITERAL_HEX,
	OFC_PARSE_LITERAL_HOLLERITH,
	OFC_PARSE_LITERAL_CHARACTER,
	OFC_PARSE_LITERAL_NUMBER,
	OFC_PARSE_LITERAL_COMPLEX,
	OFC_PARSE_LITERAL_LOGICAL,
} ofc_parse_literal_e;

typedef struct
{
	ofc_parse_literal_e type;
	unsigned        kind;

	union
	{
		ofc_string_t* string;

		ofc_str_ref_t number;

		struct
		{
			ofc_str_ref_t real, imaginary;
		} complex;

		bool logical;
	};
} ofc_parse_literal_t;


ofc_string_t* ofc_parse_hollerith(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_string_t* ofc_parse_character(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);

unsigned ofc_parse_literal_number(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal);
unsigned ofc_parse_literal_integer(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal);
unsigned ofc_parse_literal(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_literal_t* literal);

void ofc_parse_literal_cleanup(
	ofc_parse_literal_t literal);

bool ofc_parse_literal_clone(
	ofc_parse_literal_t* dst, const ofc_parse_literal_t* src);

bool ofc_parse_literal_print(
	ofc_colstr_t* cs, const ofc_parse_literal_t literal);

unsigned ofc_parse_unsigned(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* value);

#endif
