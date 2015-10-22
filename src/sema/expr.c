#include <ofc/sema.h>


static const ofc_sema_type_t* OFC_SEMA_EXPR__LOGICAL_RETURN(
	const ofc_sema_type_t* a,
	const ofc_sema_type_t* b)
{
	(void)a;
	(void)b;
	return ofc_sema_type_create_primitive(
		OFC_SEMA_TYPE_LOGICAL, 0, false, false, false);
}

typedef struct
{
	/* We need to use a function pointer since we can't
	   statically define a type, it's also more flexible. */
	const ofc_sema_type_t* (*rtype)(
		const ofc_sema_type_t*,
		const ofc_sema_type_t*);

	bool allow_logical;
	bool allow_integer;
	bool allow_real;
	bool allow_complex;
	bool allow_character;
} ofc_sema_expr__rule_t;

static ofc_sema_expr__rule_t ofc_sema_expr__rule[] =
{
	{ NULL, 0, 0, 0, 0, 0 }, /* LITERAL */
	{ NULL, 0, 0, 0, 0, 0 }, /* PARAMETER */
	{ NULL, 0, 0, 0, 0, 0 }, /* DECL */
	{ NULL, 0, 0, 0, 0, 0 }, /* CAST */

	{ NULL, 0, 1, 1, 1, 0 }, /* POWER */
	{ NULL, 0, 1, 1, 1, 0 }, /* MULTIPLY */
	{ NULL, 0, 0, 0, 0, 1 }, /* CONCAT */
	{ NULL, 0, 1, 1, 1, 0 }, /* DIVIDE */
	{ NULL, 0, 1, 1, 1, 0 }, /* ADD */
	{ NULL, 0, 1, 1, 1, 0 }, /* SUBTRACT */
	{ NULL, 0, 1, 1, 1, 0 }, /* NEGATE */

	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 1, 0 }, /* EQ */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 1, 0 }, /* NE */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 1, 0 }, /* LT */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 1, 0 }, /* LE */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 1, 0 }, /* GT */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 1, 0 }, /* GE */

	{ NULL, 1, 1, 0, 0, 0 }, /* NOT */
	{ NULL, 1, 1, 0, 0, 0 }, /* AND */
	{ NULL, 1, 1, 0, 0, 0 }, /* OR */

	{ NULL, 1, 0, 0, 0, 0 }, /* EQV */
	{ NULL, 1, 0, 0, 0, 0 }, /* NEQV */
};

static bool ofc_sema_expr_type_allowed(
	ofc_sema_expr_e etype,
	const ofc_sema_type_t* type)
{
	if (!type)
		return false;

	if (etype >= OFC_SEMA_EXPR_COUNT)
		return false;

	ofc_sema_expr__rule_t rule
		= ofc_sema_expr__rule[etype];

	if (rule.allow_logical
		&& ofc_sema_type_is_logical(type))
		return true;

	if (rule.allow_integer
		&& ofc_sema_type_is_integer(type))
		return true;

	if (rule.allow_real
		&& (type->type == OFC_SEMA_TYPE_REAL))
		return true;

	if (rule.allow_complex
		&& (type->type == OFC_SEMA_TYPE_COMPLEX))
		return true;

	if (rule.allow_character
		&& (type->type == OFC_SEMA_TYPE_CHARACTER))
		return true;

	return false;
}


static ofc_sema_expr_t* ofc_sema_expr__create(
	ofc_sema_expr_e type)
{
	if (type >= OFC_SEMA_EXPR_COUNT)
		return NULL;

	ofc_sema_expr_t* expr
		= (ofc_sema_expr_t*)malloc(
			sizeof(ofc_sema_expr_t));
	if (!expr) return NULL;

	expr->type = type;

	switch (type)
	{
		case OFC_SEMA_EXPR_LITERAL:
			expr->literal = NULL;
			break;
		case OFC_SEMA_EXPR_PARAMETER:
			expr->parameter = NULL;
			break;
		case OFC_SEMA_EXPR_DECL:
			expr->decl = NULL;
			break;
		case OFC_SEMA_EXPR_CAST:
			expr->cast.type = NULL;
			expr->cast.expr = NULL;
			break;
		default:
			expr->a = NULL;
			expr->b = NULL;
			break;
	}

	return expr;
}


/* Map parse operators to sema expr type. */
static ofc_sema_expr_e ofc_sema_expr__binary_map[] =
{
	OFC_SEMA_EXPR_POWER,
	OFC_SEMA_EXPR_MULTIPLY,
	OFC_SEMA_EXPR_CONCAT,
	OFC_SEMA_EXPR_DIVIDE,
	OFC_SEMA_EXPR_ADD,
	OFC_SEMA_EXPR_SUBTRACT,
	OFC_SEMA_EXPR_EQ,
	OFC_SEMA_EXPR_NE,
	OFC_SEMA_EXPR_LT,
	OFC_SEMA_EXPR_LE,
	OFC_SEMA_EXPR_GT,
	OFC_SEMA_EXPR_GE,
	OFC_SEMA_EXPR_NOT,
	OFC_SEMA_EXPR_AND,
	OFC_SEMA_EXPR_OR,
	OFC_SEMA_EXPR_EQV,
	OFC_SEMA_EXPR_NEQV,
};

static ofc_sema_expr_t* ofc_sema_expr__cast(
	const ofc_sema_type_t* type,
	ofc_sema_expr_t* expr)
{
	if (!type || !expr)
		return NULL;

	ofc_sema_expr_t* cast
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_CAST);
	if (!cast) return NULL;

	cast->cast.type = type;
	cast->cast.expr = expr;
	return cast;
}

static ofc_sema_expr_t* ofc_sema_expr__binary(
	const ofc_sema_scope_t* scope,
	ofc_parse_operator_e op,
	const ofc_parse_expr_t* a,
	const ofc_parse_expr_t* b)
{
	if (op >= OFC_PARSE_OPERATOR_COUNT)
		return 0;

	ofc_sema_expr_e type
		= ofc_sema_expr__binary_map[op];

	ofc_sema_expr_t* as = ofc_sema_expr(scope, a);
	if (!as) return NULL;

	if (!ofc_sema_expr_type_allowed(
		type, ofc_sema_expr_type(as)))
	{
		/* TODO - Error: Can't use type TYPE in operator OPERATOR. */
		ofc_sema_expr_delete(as);
		return NULL;
	}

	ofc_sema_expr_t* bs = ofc_sema_expr(scope, b);
	if (!bs)
	{
		ofc_sema_expr_delete(as);
		return NULL;
	}

	if (!ofc_sema_expr_type_allowed(
		type, ofc_sema_expr_type(bs)))
	{
		/* TODO - Error: Can't use type TYPE in operator OPERATOR. */
		ofc_sema_expr_delete(bs);
		ofc_sema_expr_delete(as);
		return NULL;
	}

	const ofc_sema_type_t* at = ofc_sema_expr_type(as);
	const ofc_sema_type_t* bt = ofc_sema_expr_type(bs);
	if (!ofc_sema_type_compare(at, bt))
	{
		const ofc_sema_type_t* ptype
			= ofc_sema_type_promote(at, bt);
		if (!ptype)
		{
			/* TODO - Error: Incompatible types (A, B) in operator OPERATOR. */
			ofc_sema_expr_delete(bs);
			ofc_sema_expr_delete(as);
			return NULL;
		}

		/* TODO - Warn about implicit cast? */

		if (!ofc_sema_type_compare(at, ptype))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr__cast(ptype, as);
			if (!cast)
			{
				ofc_sema_expr_delete(bs);
				ofc_sema_expr_delete(as);
				return NULL;
			}
			as = cast;
		}

		if (!ofc_sema_type_compare(bt, ptype))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr__cast(ptype, bs);
			if (!cast)
			{
				ofc_sema_expr_delete(bs);
				ofc_sema_expr_delete(as);
				return NULL;
			}
			bs = cast;
		}
	}

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(type);
	if (!expr)
	{
		ofc_sema_expr_delete(bs);
		ofc_sema_expr_delete(as);
		return NULL;
	}

	expr->a = as;
	expr->b = bs;
	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__unary(
	const ofc_sema_scope_t* scope,
	ofc_parse_operator_e op,
	const ofc_parse_expr_t* a)
{
	ofc_sema_expr_e type;
	switch (op)
	{
		case OFC_PARSE_OPERATOR_ADD:
			return ofc_sema_expr(scope, a);
		case OFC_PARSE_OPERATOR_SUBTRACT:
			type = OFC_SEMA_EXPR_NEGATE;
			break;
		case OFC_PARSE_OPERATOR_NOT:
			type = OFC_SEMA_EXPR_NOT;
			break;
		default:
			return NULL;
	}

	ofc_sema_expr_t* as = ofc_sema_expr(scope, a);
	if (!as) return NULL;

	if (!ofc_sema_expr_type_allowed(
		type, ofc_sema_expr_type(as)))
	{
		/* TODO - Error: Can't use type TYPE in operator OPERATOR. */
		ofc_sema_expr_delete(as);
		return NULL;
	}

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(type);
	if (!expr)
	{
		ofc_sema_expr_delete(as);
		return NULL;
	}

	expr->a = as;
	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__literal(
	const ofc_parse_literal_t* literal)
{
	ofc_sema_typeval_t* tv
		= ofc_sema_typeval_literal(
			literal, NULL);
	if (!tv) return NULL;

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_LITERAL);
	if (!expr)
	{
		ofc_sema_typeval_delete(tv);
		return NULL;
	}

	expr->literal = tv;
	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__parameter(
	const ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* name)
{
	if (!name || (name->type != OFC_PARSE_LHS_VARIABLE))
		return false;

	const ofc_sema_parameter_t* param
		= ofc_hashmap_find(scope->parameter, &name->variable);
	if (!param) return NULL;

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_PARAMETER);
	if (!expr) return NULL;

	expr->parameter = param;
	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__decl(
	const ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* name)
{
	if (!name)
		return false;

	if (name != OFC_PARSE_LHS_VARIABLE)
	{
		/* TODO - Handle array indices, struct members, etc. */
		return false;
	}

	const ofc_sema_decl_t* decl = NULL;
	if (scope->decl)
		decl = ofc_hashmap_find(scope->decl->map, &name->variable);
	if (!decl) return NULL;

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_DECL);
	if (!expr) return NULL;

	expr->decl = decl;
	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__variable(
	const ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* name)
{
	ofc_sema_expr_t* expr
		= ofc_sema_expr__parameter(scope, name);
	return (expr ? expr : ofc_sema_expr__decl(scope, name));
}


ofc_sema_expr_t* ofc_sema_expr(
	const ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr)
{
	if (!expr)
		return NULL;

	switch (expr->type)
	{
		case OFC_PARSE_EXPR_CONSTANT:
			return ofc_sema_expr__literal(
				&expr->literal);
		case OFC_PARSE_EXPR_VARIABLE:
			return ofc_sema_expr__variable(
				scope, expr->variable);
		case OFC_PARSE_EXPR_BRACKETS:
			return ofc_sema_expr(
				scope, expr->brackets.expr);
		case OFC_PARSE_EXPR_UNARY:
			return ofc_sema_expr__unary(
				scope, expr->unary.operator, expr->unary.a);
		case OFC_PARSE_EXPR_BINARY:
			return ofc_sema_expr__binary(
				scope, expr->binary.operator,
				expr->binary.a, expr->binary.b);
		default:
			break;
	}

	return NULL;
}

void ofc_sema_expr_delete(
	ofc_sema_expr_t* expr)
{
	if (!expr)
		return;

	switch (expr->type)
	{
		case OFC_SEMA_EXPR_LITERAL:
			ofc_sema_typeval_delete(
				expr->literal);
			break;
		case OFC_SEMA_EXPR_PARAMETER:
		case OFC_SEMA_EXPR_DECL:
			/* Don't delete decl or parameter,
			   since we're referencing them. */
			break;
		case OFC_SEMA_EXPR_CAST:
			ofc_sema_expr_delete(expr->cast.expr);
			break;
		default:
			ofc_sema_expr_delete(expr->b);
			ofc_sema_expr_delete(expr->a);
			break;
	}

	free(expr);
}

const ofc_sema_type_t* ofc_sema_expr_type(
	const ofc_sema_expr_t* expr)
{
	if (!expr)
		return NULL;

	switch (expr->type)
	{
		case OFC_SEMA_EXPR_LITERAL:
			return expr->literal->type;
		case OFC_SEMA_EXPR_PARAMETER:
			return ofc_sema_parameter_type(
				expr->parameter);
		case OFC_SEMA_EXPR_DECL:
			return ofc_sema_decl_type(
				expr->decl);
		case OFC_SEMA_EXPR_CAST:
			return expr->cast.type;
		default:
			break;
	}

	if (expr->type >= OFC_SEMA_EXPR_COUNT)
		return NULL;

	ofc_sema_expr__rule_t rule
		= ofc_sema_expr__rule[expr->type];

	if (rule.rtype)
		return rule.rtype(
			ofc_sema_expr_type(expr->a),
			ofc_sema_expr_type(expr->b));

	return ofc_sema_expr_type(expr->a);
}

ofc_sema_typeval_t* ofc_sema_expr_resolve(
	const ofc_sema_expr_t* expr)
{
	if (!expr)
		return NULL;

	switch (expr->type)
	{
		case OFC_SEMA_EXPR_LITERAL:
			return ofc_sema_typeval_copy(
				expr->literal);

		case OFC_SEMA_EXPR_PARAMETER:
			return ofc_sema_typeval_copy(
				ofc_sema_parameter_get(
					expr->parameter));

		case OFC_SEMA_EXPR_CAST:
			/* TODO - Implement typeval cast. */
			return NULL;

		case OFC_SEMA_EXPR_DECL:
			return NULL;

		default:
			break;
	}

	/* TODO - Handle operators. */
	return NULL;
}
