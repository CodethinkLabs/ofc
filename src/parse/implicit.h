#ifndef __parse_implicit_h__
#define __parse_implicit_h__

typedef struct
{
	parse_type_t c[26]; /* A-Z */
} parse_implicit_t;

const parse_implicit_t PARSE_IMPLICIT_DEFAULT;


unsigned parse_implicit(
	const sparse_t* src, const char* ptr,
	parse_implicit_t* implicit);

#endif
