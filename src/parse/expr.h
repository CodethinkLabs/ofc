#ifndef __parse_expr_h__
#define __parse_expr_h__

typedef enum
{
	PARSE_EXPR_CONSTANT,
	PARSE_EXPR_VARIABLE,
	PARSE_EXPR_UNARY,
	PARSE_EXPR_BINARY,
} parse_expr_e;

typedef struct parse_expr_s parse_expr_t;

struct parse_expr_s
{
	parse_expr_e type;

	union
	{
		parse_literal_t literal;
		str_ref_t       variable;

		struct
		{
			parse_expr_t*    a;
			parse_operator_e operator;
		} unary;

		struct
		{
			parse_expr_t*    a;
			parse_expr_t*    b;
			parse_operator_e operator;
		} binary;
	};
};


unsigned parse_expr(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr);

#endif
