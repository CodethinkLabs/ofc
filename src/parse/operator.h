#ifndef __parse_operator_h__
#define __parse_operator_h__

typedef enum
{
		PARSE_OPERATOR_POWER,
		PARSE_OPERATOR_MULTIPLY,
		PARSE_OPERATOR_CONCAT,
		PARSE_OPERATOR_DIVIDE,
		PARSE_OPERATOR_ADD,
		PARSE_OPERATOR_SUBRACT,
		PARSE_OPERATOR_EQ,
		PARSE_OPERATOR_NE,
		PARSE_OPERATOR_LT,
		PARSE_OPERATOR_LE,
		PARSE_OPERATOR_GT,
		PARSE_OPERATOR_GE,
		PARSE_OPERATOR_NOT,
		PARSE_OPERATOR_AND,
		PARSE_OPERATOR_OR,
		PARSE_OPERATOR_EQV,
		PARSE_OPERATOR_NEQV,
} parse_operator_e;


unsigned parse_operator(
	const sparse_t* sparse, const char* ptr,
	parse_operator_e* operator);

#endif
