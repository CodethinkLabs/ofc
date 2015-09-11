#ifndef __parse_keyword_h__
#define __parse_keyword_h__

typedef enum
{
	PARSE_KEYWORD_PROGRAM = 0,
	PARSE_KEYWORD_END,
	PARSE_KEYWORD_END_PROGRAM,
	PARSE_KEYWORD_IF,
	PARSE_KEYWORD_THEN,
	PARSE_KEYWORD_ELSE_IF,
	PARSE_KEYWORD_ELSE,
	PARSE_KEYWORD_END_IF,
	PARSE_KEYWORD_GO_TO,
	PARSE_KEYWORD_CONTINUE,
	PARSE_KEYWORD_STOP,

	PARSE_KEYWORD_LOGICAL,
	PARSE_KEYWORD_CHARACTER,
	PARSE_KEYWORD_INTEGER,
	PARSE_KEYWORD_REAL,
	PARSE_KEYWORD_COMPLEX,
	PARSE_KEYWORD_BYTE,
	PARSE_KEYWORD_DOUBLE_PRECISION,
	PARSE_KEYWORD_DOUBLE_COMPLEX,

	PARSE_KEYWORD_IMPLICIT,
	PARSE_KEYWORD_IMPLICIT_NONE,

	PARSE_KEYWORD_WRITE,
	PARSE_KEYWORD_FORMAT,
} parse_keyword_e;

unsigned parse_name(
	const sparse_t* src, const char* ptr);

unsigned parse_keyword(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword);

unsigned parse_keyword_name(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword,
	str_ref_t* name);

#endif
