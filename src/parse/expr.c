#include <ofc/parse.h>



static void ofc_parse_expr__cleanup(
	ofc_parse_expr_t expr)
{
	switch (expr.type)
	{
		case OFC_PARSE_EXPR_CONSTANT:
			ofc_parse_literal_cleanup(expr.literal);
			break;

		case OFC_PARSE_EXPR_VARIABLE:
			ofc_parse_lhs_delete(expr.variable);
			break;

		case OFC_PARSE_EXPR_BRACKETS:
			ofc_parse_expr_delete(expr.brackets.expr);
			break;

		case OFC_PARSE_EXPR_UNARY:
			ofc_parse_expr_delete(expr.unary.a);
			break;

		case OFC_PARSE_EXPR_BINARY:
			ofc_parse_expr_delete(expr.binary.a);
			ofc_parse_expr_delete(expr.binary.b);
			break;

		default:
			break;
	}
}

static bool ofc_parse_expr__clone(
	ofc_parse_expr_t* dst, const ofc_parse_expr_t* src)
{
	if (!src || !dst)
		return false;

	ofc_parse_expr_t clone;
	clone.type = src->type;

	switch (clone.type)
	{
		case OFC_PARSE_EXPR_CONSTANT:
			if (!ofc_parse_literal_clone(
				&clone.literal, &src->literal))
				return false;
			break;

		case OFC_PARSE_EXPR_VARIABLE:
			clone.variable = ofc_parse_lhs_copy(src->variable);
			if (!clone.variable)
				return false;
			break;

		case OFC_PARSE_EXPR_BRACKETS:
			clone.brackets.expr
				= ofc_parse_expr_copy(src->brackets.expr);
			if (!clone.brackets.expr)
				return false;
			break;

		case OFC_PARSE_EXPR_UNARY:
			clone.unary.operator = src->unary.operator;

			clone.unary.a
				= ofc_parse_expr_copy(src->unary.a);
			if (!clone.unary.a)
				return false;
			break;

		case OFC_PARSE_EXPR_BINARY:
			clone.binary.operator = src->binary.operator;

			clone.binary.a
				= ofc_parse_expr_copy(src->binary.a);
			if (!clone.binary.a)
				return false;

			clone.binary.b
				= ofc_parse_expr_copy(src->binary.b);
			if (!clone.binary.b)
			{
				ofc_parse_expr_delete(clone.binary.a);
				return false;
			}
			break;

		default:
			return false;
	}

	*dst = clone;
	return true;
}

static ofc_parse_expr_t* ofc_parse_expr__alloc(
	ofc_parse_expr_t expr)
{
	ofc_parse_expr_t* aexpr
		= (ofc_parse_expr_t*)malloc(
			sizeof(ofc_parse_expr_t));
	if (!aexpr) return NULL;
	*aexpr = expr;
	return aexpr;
}



static unsigned ofc_parse_expr__level(
	ofc_parse_expr_t expr)
{
	switch (expr.type)
	{
		case OFC_PARSE_EXPR_UNARY:
			return ofc_parse_operator_precedence(expr.unary.operator);
		case OFC_PARSE_EXPR_BINARY:
			return ofc_parse_operator_precedence(expr.binary.operator);
		default:
			break;
	}

	return 0;
}

static bool ofc_parse_expr__term(const char* ptr)
{
	if (ofc_is_end_statement(ptr, NULL))
		return true;

	switch (ptr[0])
	{
		case ',':
		case ')':
			return true;
		default:
			break;
	}
	return false;
}



static unsigned ofc_parse_expr__at_or_below(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* expr, unsigned level);

static unsigned ofc_parse__expr(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* expr)
{
	return ofc_parse_expr__at_or_below(
		src, ptr, debug, expr, OPERATOR_PRECEDENCE_MAX);
}

static unsigned ofc_parse_expr__literal(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* expr)
{
	unsigned len = ofc_parse_literal(
		src, ptr, debug, &expr->literal);
	if (len == 0) return 0;

	expr->type = OFC_PARSE_EXPR_CONSTANT;
	expr->src  = ofc_str_ref(ptr, len);
	return len;
}

static unsigned ofc_parse_expr__integer(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* expr)
{
	unsigned len = ofc_parse_literal_integer(
		src, ptr, debug, &expr->literal);
	if (len == 0) return 0;

	expr->type = OFC_PARSE_EXPR_CONSTANT;
	expr->src  = ofc_str_ref(ptr, len);
	return len;
}

static unsigned ofc_parse_expr__integer_variable(
       const ofc_sparse_t* src, const char* ptr,
       ofc_parse_debug_t* debug,
       ofc_parse_expr_t* expr)
{
	unsigned len = ofc_parse_expr__integer(
		src, ptr, debug, expr);
	if (len > 0) return len;

	expr->variable = ofc_parse_lhs_variable(
		   src, ptr, debug, &len);
	if (!expr->variable) return 0;

	expr->type = OFC_PARSE_EXPR_VARIABLE;
	expr->src  = ofc_str_ref(ptr, len);
	return len;
}

static unsigned ofc_parse_expr__primary(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* expr)
{
	unsigned len = ofc_parse_expr__literal(
		src, ptr, debug, expr);
	if (len > 0) return len;

	expr->variable
		= ofc_parse_lhs(src, ptr, debug, &len);
	if (expr->variable)
	{
		expr->type = OFC_PARSE_EXPR_VARIABLE;
		expr->src = ofc_str_ref(ptr, len);
		return len;
	}

	if (ptr[0] == '(')
	{
		unsigned dpos = ofc_parse_debug_position(debug);
		ofc_parse_expr_t expr_brackets;
		unsigned len = ofc_parse__expr(
			src, &ptr[1], debug, &expr_brackets);
		if (len > 0)
		{
			if  (ptr[1 + len] != ')')
			{
				ofc_parse_expr__cleanup(expr_brackets);
				ofc_parse_debug_rewind(debug, dpos);
			}
			else
			{
				len += 2;

				expr->brackets.expr
					= ofc_parse_expr__alloc(expr_brackets);
				if (!expr->brackets.expr)
				{
					ofc_parse_expr__cleanup(expr_brackets);
					ofc_parse_debug_rewind(debug, dpos);
					return 0;
				}
				expr->type = OFC_PARSE_EXPR_BRACKETS;
				expr->src = ofc_str_ref(ptr, len);
				return len;
			}
		}
	}

	return 0;
}

static unsigned ofc_parse_expr__unary(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* expr, unsigned level)
{
	/* TODO - Defined unary operators. */

	unsigned dpos = ofc_parse_debug_position(debug);

	ofc_parse_operator_e op;
	unsigned op_len = ofc_parse_operator(
		src, ptr, debug, &op);
	if ((op_len == 0)
		|| !ofc_parse_operator_unary(op))
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	unsigned op_level = ofc_parse_operator_precedence(op);
	if (op_level > level)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	ofc_parse_expr_t a;
	unsigned a_len = ofc_parse_expr__at_or_below(
		src, &ptr[op_len], debug, &a, op_level);
	if (a_len == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	expr->type = OFC_PARSE_EXPR_UNARY;
	expr->src = ofc_str_ref(ptr, (op_len + a_len));

	expr->unary.operator = op;

	expr->unary.a = ofc_parse_expr__alloc(a);
	if (!expr->unary.a)
	{
		ofc_parse_expr__cleanup(a);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	return (op_len + a_len);
}

static bool ofc_parse_expr__has_right_ambig_point(
	ofc_parse_expr_t* expr)
{
	if (!expr)
		return false;

	switch (expr->type)
	{
		case OFC_PARSE_EXPR_CONSTANT:
			return ((expr->literal.type == OFC_PARSE_LITERAL_NUMBER)
				&& (expr->literal.number.base[expr->literal.number.size - 1] == '.'));
		case OFC_PARSE_EXPR_UNARY:
			return ofc_parse_expr__has_right_ambig_point(expr->unary.a);
		case OFC_PARSE_EXPR_BINARY:
			return ofc_parse_expr__has_right_ambig_point(expr->binary.b);
		default:
			break;
	}
	return false;
}

static void ofc_parse_expr__cull_right_ambig_point(
	ofc_parse_expr_t* expr)
{
	if (!expr)
		return;

	switch (expr->type)
	{
		case OFC_PARSE_EXPR_CONSTANT:
			if (expr->literal.type == OFC_PARSE_LITERAL_NUMBER)
			{
				expr->literal.number.size -= 1;
				expr->src.size            -= 1;
			}
			break;
		case OFC_PARSE_EXPR_UNARY:
			ofc_parse_expr__cull_right_ambig_point(expr->unary.a);
			break;
		case OFC_PARSE_EXPR_BINARY:
			ofc_parse_expr__cull_right_ambig_point(expr->binary.b);
			break;
		default:
			break;
	}
}


static unsigned ofc_parse_expr__binary_at_or_below_b(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t a,
	ofc_parse_expr_t* expr, unsigned level)
{
	/* TODO - Defined binary operators. */

	ofc_parse_operator_e op;
	unsigned op_len = ofc_parse_operator(
		src, ptr, debug, &op);
	if ((op_len == 0) || !ofc_parse_operator_binary(op))
		return 0;

	unsigned op_level = ofc_parse_operator_precedence(op);
	if ((op_level > level)
		|| (ofc_parse_expr__level(a) > op_level))
		return 0;

	ofc_parse_expr_t b;
	unsigned b_len = ofc_parse_expr__at_or_below(
		src, &ptr[op_len], debug, &b, (op_level - 1));
	if (b_len == 0) return 0;

	ofc_parse_expr_t c;

	c.type = OFC_PARSE_EXPR_BINARY;
	c.src  = ofc_str_ref_bridge(a.src, b.src);
	c.binary.operator = op;

	c.binary.a = ofc_parse_expr__alloc(a);
	if (!c.binary.a)
	{
		ofc_parse_expr__cleanup(b);
		return 0;
	}

	c.binary.b = ofc_parse_expr__alloc(b);
	if (!c.binary.b)
	{
		/* Don't cleanup a here, we didn't create it. */
		free(c.binary.a);
		ofc_parse_expr__cleanup(b);
		return 0;
	}

	unsigned c_len = ofc_parse_expr__binary_at_or_below_b(
		src, &ptr[op_len + b_len], debug, c, expr, op_level);
	if (c_len > 0) return (op_len + b_len + c_len);

	*expr = c;
	return (op_len + b_len);
}

static unsigned ofc_parse_expr__binary_at_or_below(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* expr, unsigned level)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	ofc_parse_expr_t a;
	unsigned a_len = ofc_parse_expr__at_or_below(
		src, ptr, debug, &a, (level - 1));
	if (a_len == 0) return 0;

	/* Optimize by returning a if we see end of statement or close bracket. */
	if (ofc_parse_expr__term(&ptr[a_len]))
	{
		*expr = a;
		return a_len;
	}

	/* Handle case where we have something like:
	   ( 3 ** 3 .EQ. 76 ) */
	if (ofc_parse_expr__has_right_ambig_point(&a))
	{
		ofc_parse_operator_e op;
		unsigned op_len = ofc_parse_operator(
			src, &ptr[a_len - 1], debug, &op);
		if ((op_len > 0)
			&& ofc_parse_operator_binary(op)
			&& (ofc_parse_operator_precedence(op) <= level))
		{
			ofc_parse_expr__cull_right_ambig_point(&a);
			a_len -= 1;
		}
	}

	unsigned b_len = ofc_parse_expr__binary_at_or_below_b(
		src, &ptr[a_len], debug, a, expr, level);
	if (b_len == 0)
	{
		ofc_parse_expr__cleanup(a);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	return (a_len + b_len);
}

static unsigned ofc_parse_expr__binary(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* expr, unsigned level)
{
	unsigned i;
	for (i = level; i > 0; i--)
	{
		unsigned len = ofc_parse_expr__binary_at_or_below(
			src, ptr, debug, expr, i);
		if (len > 0) return len;
	}
	return 0;
}

static unsigned ofc_parse_expr__at_or_below(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t* expr, unsigned level)
{
	unsigned len = ofc_parse_expr__binary(
		src, ptr, debug, expr, level);
	if (len > 0) return len;

	len = ofc_parse_expr__unary(
		src, ptr, debug, expr, level);
	if (len > 0) return len;

	return ofc_parse_expr__primary(
		src, ptr, debug, expr);
}



ofc_parse_expr_t* ofc_parse_expr_integer(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	ofc_parse_expr_t e;
	unsigned i = ofc_parse_expr__integer(
		src, ptr, debug, &e);
	if (i == 0) return NULL;

	ofc_parse_expr_t* expr
		= ofc_parse_expr__alloc(e);
	if (!expr)
	{
		ofc_parse_debug_rewind(debug, dpos);
		ofc_parse_expr__cleanup(e);
		return NULL;
	}

	if (len) *len = i;
	return expr;
}

ofc_parse_expr_t* ofc_parse_expr_integer_variable(
       const ofc_sparse_t* src, const char* ptr,
       ofc_parse_debug_t* debug, unsigned* len)
{
       unsigned dpos = ofc_parse_debug_position(debug);

       ofc_parse_expr_t e;
       unsigned i = ofc_parse_expr__integer_variable(
               src, ptr, debug, &e);
       if (i == 0) return NULL;

       ofc_parse_expr_t* expr
               = ofc_parse_expr__alloc(e);
       if (!expr)
       {
               ofc_parse_debug_rewind(debug, dpos);
               ofc_parse_expr__cleanup(e);
               return NULL;
       }

       if (len) *len = i;
       return expr;
}


ofc_parse_expr_t* ofc_parse_expr(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	ofc_parse_expr_t e;
	unsigned i = ofc_parse__expr(
		src, ptr, debug, &e);
	if (i == 0) return NULL;

	ofc_parse_expr_t* expr
		= ofc_parse_expr__alloc(e);
	if (!expr)
	{
		ofc_parse_debug_rewind(debug, dpos);
		ofc_parse_expr__cleanup(e);
		return NULL;
	}

	if (len) *len = i;
	return expr;
}

void ofc_parse_expr_delete(
	ofc_parse_expr_t* expr)
{
	if (!expr)
		return;

	ofc_parse_expr__cleanup(*expr);
	free(expr);
}

bool ofc_parse_expr_print(
	ofc_colstr_t* cs, const ofc_parse_expr_t* expr)
{
	if (!expr)
		return false;

	switch(expr->type)
	{
		case OFC_PARSE_EXPR_CONSTANT:
			if (!ofc_parse_literal_print(
				cs, expr->literal))
				return false;
			break;
		case OFC_PARSE_EXPR_VARIABLE:
			if (!ofc_parse_lhs_print(
				cs, expr->variable, false))
				return false;
			break;
		case OFC_PARSE_EXPR_BRACKETS:
			if (!ofc_colstr_atomic_writef(cs, "(")
				|| !ofc_parse_expr_print(
					cs, expr->brackets.expr)
				|| !ofc_colstr_atomic_writef(cs, ")"))
				return false;
			break;
		case OFC_PARSE_EXPR_UNARY:
			if (!ofc_parse_operator_print(cs, expr->unary.operator)
				|| !ofc_parse_expr_print(cs, expr->unary.a))
				return false;
			break;
		case OFC_PARSE_EXPR_BINARY:
			if (!ofc_parse_expr_print(cs, expr->binary.a)
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_parse_operator_print(cs, expr->binary.operator)
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_parse_expr_print(cs, expr->binary.b))
				return false;
			break;

		default:
			return false;
	}

	return true;
}

ofc_parse_expr_t* ofc_parse_expr_copy(
	const ofc_parse_expr_t* expr)
{
	ofc_parse_expr_t copy;
	if (!ofc_parse_expr__clone(&copy, expr))
		return NULL;

	ofc_parse_expr_t* acopy
		= ofc_parse_expr__alloc(copy);
	if (!acopy)
		ofc_parse_expr__cleanup(copy);
	return acopy;
}



ofc_parse_expr_list_t* ofc_parse_expr_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_expr_list_t* list
		= (ofc_parse_expr_list_t*)malloc(
			sizeof(ofc_parse_expr_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->expr = NULL;

	unsigned l = ofc_parse_list(
		src, ptr, debug,
		',', &list->count, (void***)&list->expr,
		(void*)ofc_parse_expr,
		(void*)ofc_parse_expr_delete);
	if (l == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = l;
	return list;
}

void ofc_parse_expr_list_delete(
	ofc_parse_expr_list_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->expr,
		(void*)ofc_parse_expr_delete);
	free(list);
}

bool ofc_parse_expr_list_print(
	ofc_colstr_t* cs, const ofc_parse_expr_list_t* list)
{
	if (!list)
		return false;

	return ofc_parse_list_print(
		cs, list->count, (const void**)list->expr,
		(void*)ofc_parse_expr_print);
}
