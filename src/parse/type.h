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


#define PARSE_TYPE_INTEGER_DEFAULT (parse_type_t)\
{\
	.type  = PARSE_TYPE_INTEGER,\
	.kind  = 0,\
	.count = 0,\
}

#define PARSE_TYPE_REAL_DEFAULT (parse_type_t)\
{\
	.type  = PARSE_TYPE_REAL,\
	.kind  = 0,\
	.count = 0,\
}


unsigned parse_type(
	const sparse_t* src, const char* ptr,
	parse_type_t* type);

#endif
