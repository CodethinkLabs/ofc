#include "parse.h"


typedef unsigned (*parse_expr_func_t)(
	const sparse_t*, const char*, parse_expr_t*);


static unsigned parse_expr__primary(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr)
{
	unsigned len;

	len = parse_literal(src, ptr, &expr->literal);
	if (len > 0)
	{
		expr->type = PARSE_EXPR_CONSTANT;
		return len;
	}

	/* TODO - Check for intrinsics first. */
	len = parse_name(src, ptr);
	if (len > 0)
	{
		expr->variable.base = ptr;
		expr->variable.size = len;
		expr->type = PARSE_EXPR_VARIABLE;
		return len;
	}

	/* TODO - Parse constant-subobject. */

	/* TODO - Parse array constructor. */

	/* TODO - Parse structure constructor. */

	/* TODO - Parse function-reference. */

	if (ptr[0] == '(')
	{
		unsigned len = parse_expr(
			src, &ptr[1], expr);
		if ((len > 0) && (ptr[1 + len] != ')'))
			return (len + 2);
	}

	return 0;
}

static unsigned parse_expr__level_1(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr)
{
	/* TODO - Defined unary operators. */

	return parse_expr__primary(src, ptr, expr);
}

static unsigned parse_expr__mult_operand(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr)
{
	parse_expr_t a;
	unsigned a_len = parse_expr__level_1(
		src, ptr, &a);
	if (a_len == 0) return 0;

	parse_operator_e op;
	unsigned op_len = parse_operator(
		src, &ptr[a_len], &op);
	if ((op_len == 0) || (op != PARSE_OPERATOR_POWER))
	{
		*expr = a;
		return a_len;
	}

	parse_expr_t b;
	unsigned b_len = parse_expr__mult_operand(
		src, &ptr[a_len + op_len], &b);
	if (b_len == 0)
	{
		*expr = a;
		return a_len;
	}

	expr->type = PARSE_EXPR_BINARY;
	expr->binary.operator = op;

	expr->binary.a = (parse_expr_t*)malloc(sizeof(parse_expr_t));
	expr->binary.b = (parse_expr_t*)malloc(sizeof(parse_expr_t));
	if (!expr->binary.a
		|| !expr->binary.b)
	{
		free(expr->binary.a);
		free(expr->binary.b);
		return 0;
	}

	*(expr->binary.a) = a;
	*(expr->binary.b) = b;

	return (a_len + op_len + b_len);
}

static unsigned parse_expr__partial(
	const sparse_t* src, const char* ptr,
	parse_expr_func_t term_func, parse_operator_e allow_op,
	parse_expr_t* expr)
{
	parse_operator_e op;
	unsigned op_len = parse_operator(
		src, ptr, &op);
	if (op_len == 0)
		return 0;

	if (op != allow_op)
		return 0;

	parse_expr_t b;
	unsigned b_len = term_func(
		src, &ptr[op_len], &b);
	if (b_len == 0) return 0;

	parse_expr_t a = *expr;

	expr->type = PARSE_EXPR_BINARY;
	expr->binary.operator = op;

	expr->binary.a = (parse_expr_t*)malloc(sizeof(parse_expr_t));
	expr->binary.b = (parse_expr_t*)malloc(sizeof(parse_expr_t));
	if (!expr->binary.a
		|| !expr->binary.b)
	{
		free(expr->binary.a);
		free(expr->binary.b);
		return 0;
	}

	*(expr->binary.a) = a;
	*(expr->binary.b) = b;

	return (op_len + b_len);
}

static unsigned parse_expr__add_operand(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr)
{
	parse_expr_t a;
	unsigned a_len = parse_expr__mult_operand(
		src, ptr, &a);
	if (a_len == 0) return 0;

	while (true)
	{
		unsigned len = parse_expr__partial(src, &ptr[a_len],
			parse_expr__mult_operand,
			PARSE_OPERATOR_MULTIPLY, &a);
		if (len == 0)
		{
			len = parse_expr__partial(src, &ptr[a_len],
				parse_expr__mult_operand,
				PARSE_OPERATOR_DIVIDE, &a);
		}
		if (len == 0) break;
		a_len += len;
	}

	*expr = a;
	return a_len;
}

static unsigned parse_expr__level_2(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr)
{
	parse_expr_t a;
	unsigned a_len = parse_expr__add_operand(
		src, ptr, &a);
	if (a_len == 0) return 0;

	while (true)
	{
		unsigned len = parse_expr__partial(src, &ptr[a_len],
			parse_expr__add_operand,
			PARSE_OPERATOR_ADD, &a);
		if (len == 0)
		{
			len = parse_expr__partial(src, &ptr[a_len],
				parse_expr__add_operand,
				PARSE_OPERATOR_SUBTRACT, &a);
		}
		if (len == 0) break;
		a_len += len;
	}

	*expr = a;
	return a_len;
}

static unsigned parse_expr__level_3(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr)
{
	parse_expr_t a;
	unsigned a_len = parse_expr__level_2(
		src, ptr, &a);
	if (a_len == 0) return 0;

	while (true)
	{
		unsigned len = parse_expr__partial(src, &ptr[a_len],
			parse_expr__level_2,
			PARSE_OPERATOR_CONCAT, &a);
		if (len == 0) break;
		a_len += len;
	}

	*expr = a;
	return a_len;
}

static unsigned parse_expr__level_4(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr)
{
	parse_expr_t a;
	unsigned a_len = parse_expr__level_3(
		src, ptr, &a);
	if (a_len == 0) return 0;

	while (true)
	{
		unsigned len = parse_expr__partial(src, &ptr[a_len],
			parse_expr__level_3,
			PARSE_OPERATOR_EQ, &a);
		if (len == 0)
		{
			len = parse_expr__partial(src, &ptr[a_len],
				parse_expr__level_3,
				PARSE_OPERATOR_NE, &a);
		}
		if (len == 0)
		{
			len = parse_expr__partial(src, &ptr[a_len],
				parse_expr__level_3,
				PARSE_OPERATOR_GE, &a);
		}
		if (len == 0)
		{
			len = parse_expr__partial(src, &ptr[a_len],
				parse_expr__level_3,
				PARSE_OPERATOR_GT, &a);
		}
		if (len == 0)
		{
			len = parse_expr__partial(src, &ptr[a_len],
				parse_expr__level_3,
				PARSE_OPERATOR_LE, &a);
		}
		if (len == 0)
		{
			len = parse_expr__partial(src, &ptr[a_len],
				parse_expr__level_3,
				PARSE_OPERATOR_LT, &a);
		}
		if (len == 0) break;
		a_len += len;
	}

	*expr = a;
	return a_len;
}

static unsigned parse_expr__and_operand(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr)
{
	parse_operator_e op;
	unsigned op_len = parse_operator(
		src, ptr, &op);
	if ((op_len > 0) && (op != PARSE_OPERATOR_NOT))
		op_len = 0;

	parse_expr_t a;
	unsigned a_len = parse_expr__level_4(
		src, &ptr[op_len], &a);
	if (a_len == 0) return 0;

	if (op_len > 0)
	{
		expr->type = PARSE_EXPR_UNARY;
		expr->unary.operator = op;

		expr->unary.a = (parse_expr_t*)malloc(sizeof(parse_expr_t));
		if (!expr->unary.a)
		{
			free(expr->unary.a);
			return 0;
		}

		*(expr->unary.a) = a;
	}
	else
	{
		*expr = a;
	}

	return (op_len + a_len);
}

static unsigned parse_expr__or_operand(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr)
{
	parse_expr_t a;
	unsigned a_len = parse_expr__and_operand(
		src, ptr, &a);
	if (a_len == 0) return 0;

	while (true)
	{
		unsigned len = parse_expr__partial(src, &ptr[a_len],
			parse_expr__and_operand,
			PARSE_OPERATOR_AND, &a);
		if (len == 0) break;
		a_len += len;
	}

	*expr = a;
	return a_len;
}

static unsigned parse_expr__equiv_operand(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr)
{
	parse_expr_t a;
	unsigned a_len = parse_expr__or_operand(
		src, ptr, &a);
	if (a_len == 0) return 0;

	while (true)
	{
		unsigned len = parse_expr__partial(src, &ptr[a_len],
			parse_expr__or_operand,
			PARSE_OPERATOR_OR, &a);
		if (len == 0) break;
		a_len += len;
	}

	*expr = a;
	return a_len;
}

static unsigned parse_expr__level_5(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr)
{
	parse_expr_t a;
	unsigned a_len = parse_expr__equiv_operand(
		src, ptr, &a);
	if (a_len == 0) return 0;

	while (true)
	{
		unsigned len = parse_expr__partial(src, &ptr[a_len],
			parse_expr__equiv_operand,
			PARSE_OPERATOR_EQV, &a);
		if (len == 0)
		{
			len = parse_expr__partial(src, &ptr[a_len],
				parse_expr__equiv_operand,
				PARSE_OPERATOR_NEQV, &a);
		}
		if (len == 0) break;
		a_len += len;
	}

	*expr = a;
	return a_len;
}

unsigned parse_expr(
	const sparse_t* src, const char* ptr,
	parse_expr_t* expr)
{
	/* TODO - Defined binary operators. */

	return parse_expr__level_5(
		src, ptr, expr);
}

void parse_expr_cleanup(
	parse_expr_t expr)
{
	switch (expr.type)
	{
		case PARSE_EXPR_CONSTANT:
			parse_literal_cleanup(expr.literal);
			break;
		case PARSE_EXPR_UNARY:
			parse_expr_cleanup(*expr.unary.a);
			free(expr.unary.a);
			break;
		case PARSE_EXPR_BINARY:
			parse_expr_cleanup(*expr.binary.a);
			parse_expr_cleanup(*expr.binary.b);
			free(expr.binary.a);
			free(expr.binary.b);
			break;
		default:
			break;
	}
}
