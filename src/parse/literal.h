#ifndef __parse_literal_h__
#define __parse_literal_h__

#include <stdint.h>
#include "../string.h"


typedef enum
{
	PARSE_LITERAL_BINARY,
	PARSE_LITERAL_OCTAL,
	PARSE_LITERAL_HEX,
	PARSE_LITERAL_HOLLERITH,
	PARSE_LITERAL_CHARACTER,
	PARSE_LITERAL_NUMBER,
	PARSE_LITERAL_COMPLEX,
	PARSE_LITERAL_LOGICAL,
} parse_literal_e;

typedef struct
{
	str_ref_t left_number;
	str_ref_t right_number;
} complex_t;

typedef struct
{
	parse_literal_e type;
	unsigned        kind;

	union
	{
		string_t string;

		str_ref_t number;

		complex_t complex;

		bool logical;
	};
} parse_literal_t;


unsigned parse_hollerith(
	const sparse_t* src, const char* ptr,
	string_t* string);
unsigned parse_character(
	const sparse_t* src, const char* ptr,
	string_t* string);

unsigned parse_literal(
	const sparse_t* src, const char* ptr,
	parse_literal_t* literal);

void parse_literal_cleanup(
	parse_literal_t literal);

bool parse_literal_clone(
	parse_literal_t* dst, const parse_literal_t* src);

unsigned parse_unsigned(
	const sparse_t* src, const char* ptr,
	unsigned* value);

#endif
