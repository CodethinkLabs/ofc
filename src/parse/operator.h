#ifndef __parse_operator_h__
#define __parse_operator_h__

typedef enum
{
	PARSE_OPERATOR_POWER,
	PARSE_OPERATOR_MULTIPLY,
	PARSE_OPERATOR_CONCAT,
	PARSE_OPERATOR_DIVIDE,
	PARSE_OPERATOR_ADD,
	PARSE_OPERATOR_SUBTRACT,
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

	PARSE_OPERATOR_COUNT
} parse_operator_e;


static const unsigned OPERATOR_PRECEDENCE_MAX = 11;

unsigned parse_operator(
	const sparse_t* sparse, const char* ptr,
	parse_debug_t* debug,
	parse_operator_e* operator);

bool parse_operator_unary(
	parse_operator_e operator);
bool parse_operator_binary(
	parse_operator_e operator);

unsigned parse_operator_precedence(
	parse_operator_e operator);

bool parse_operator_print(
	int fd, const parse_operator_e operator);

#endif
