#ifndef __parse_expr_h__
#define __parse_expr_h__

typedef enum
{
	PARSE_EXPR_NONE,
	PARSE_EXPR_CONSTANT,
	PARSE_EXPR_VARIABLE,
	PARSE_EXPR_BRACKETS,
	PARSE_EXPR_UNARY,
	PARSE_EXPR_BINARY,
} parse_expr_e;


struct parse_expr_s
{
	parse_expr_e type;

	union
	{
		parse_literal_t literal;
		parse_lhs_t     variable;

		struct
		{
			parse_expr_t* expr;
		} brackets;

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

typedef struct
{
	unsigned       count;
	parse_expr_t** expr;
} parse_expr_list_t;


#define PARSE_EXPR_EMPTY (parse_expr_t){ .type = PARSE_EXPR_NONE };


/* Returns a literal wrapped in an expression type. */
parse_expr_t* parse_expr_literal(
	const sparse_t* src, const char* ptr,
	unsigned* len);

parse_expr_t* parse_expr(
	const sparse_t* src, const char* ptr,
	unsigned* len);

void parse_expr_delete(
	parse_expr_t* expr);
parse_expr_t* parse_expr_copy(
	const parse_expr_t* expr);


parse_expr_list_t* parse_expr_list(
	const sparse_t* src, const char* ptr,
	unsigned* len);
void parse_expr_list_delete(
	parse_expr_list_t* list);

#endif
