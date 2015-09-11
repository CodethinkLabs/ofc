#ifndef __parse_type_h__
#define __parse_type_h__

typedef enum
{
	PARSE_TYPE_NONE,
	PARSE_TYPE_LOGICAL,
	PARSE_TYPE_CHARACTER,
	PARSE_TYPE_INTEGER,
	PARSE_TYPE_REAL,
	PARSE_TYPE_COMPLEX,
	PARSE_TYPE_BYTE,
} parse_type_e;

typedef struct
{
	parse_type_e type;
	unsigned     kind;
	unsigned     count;
} parse_type_t;


unsigned parse_type(
	const sparse_t* src, const char* ptr,
	parse_type_t* type);

#endif
