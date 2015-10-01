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
	parse_literal_e type;
	unsigned        kind;

	union
	{
		string_t* string;

		str_ref_t number;

		struct
		{
			str_ref_t real, imaginary;
		} complex;

		bool logical;
	};
} parse_literal_t;


string_t* parse_hollerith(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
string_t* parse_character(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);

unsigned parse_literal_number(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_literal_t* literal);
unsigned parse_literal(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_literal_t* literal);

void parse_literal_cleanup(
	parse_literal_t literal);

bool parse_literal_clone(
	parse_literal_t* dst, const parse_literal_t* src);

bool parse_literal_print(
	string_t* tree_output, const parse_literal_t literal);

unsigned parse_unsigned(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* value);

#endif
