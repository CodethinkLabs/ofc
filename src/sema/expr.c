#include <ofc/sema.h>


const ofc_sema_typeval_t* ofc_sema_expr_constant(
	const ofc_sema_expr_t* expr)
{
	return (expr && (expr->type == OFC_SEMA_EXPR_CONSTANT)
		? expr->constant : NULL);
}

bool ofc_sema_expr_is_constant(
	const ofc_sema_expr_t* expr)
{
	return (ofc_sema_expr_constant(expr) != NULL);
}


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
	{ NULL, 0, 0, 0, 0, 0 }, /* CONSTANT */
	{ NULL, 0, 0, 0, 0, 0 }, /* LHS */
	{ NULL, 0, 0, 0, 0, 0 }, /* CAST */
	{ NULL, 0, 0, 0, 0, 0 }, /* INTRINSIC */

	{ NULL, 0, 1, 1, 1, 0 }, /* POWER */
	{ NULL, 0, 1, 1, 1, 0 }, /* MULTIPLY */
	{ NULL, 0, 0, 0, 0, 1 }, /* CONCAT */
	{ NULL, 0, 1, 1, 1, 0 }, /* DIVIDE */
	{ NULL, 0, 1, 1, 1, 0 }, /* ADD */
	{ NULL, 0, 1, 1, 1, 0 }, /* SUBTRACT */
	{ NULL, 0, 1, 1, 1, 0 }, /* NEGATE */

	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 1, 0 }, /* EQ */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 1, 0 }, /* NE */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 0, 0 }, /* LT */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 0, 0 }, /* LE */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 0, 0 }, /* GT */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 0, 0 }, /* GE */

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


static ofc_sema_typeval_t* ofc_sema_typeval_negate__faux_binary(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	(void)b;
	return ofc_sema_typeval_negate(scope, a);
}

static ofc_sema_typeval_t* ofc_sema_typeval_not__faux_binary(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	(void)scope;
	(void)b;
	return ofc_sema_typeval_not(scope, a);
}

static ofc_sema_typeval_t* (*ofc_sema_expr__resolve[])(
	const ofc_sema_scope_t*,
	const ofc_sema_typeval_t*,
	const ofc_sema_typeval_t*) =
{
	NULL, /* CONSTANT */
	NULL, /* LHS */
	NULL, /* CAST */
	NULL, /* INTRINSIC */

	ofc_sema_typeval_power,
	ofc_sema_typeval_multiply,
	ofc_sema_typeval_concat,
	ofc_sema_typeval_divide,
	ofc_sema_typeval_add,
	ofc_sema_typeval_subtract,
	ofc_sema_typeval_negate__faux_binary,

	ofc_sema_typeval_eq,
	ofc_sema_typeval_ne,
	ofc_sema_typeval_lt,
	ofc_sema_typeval_le,
	ofc_sema_typeval_gt,
	ofc_sema_typeval_ge,

	ofc_sema_typeval_not__faux_binary,
	ofc_sema_typeval_and,
	ofc_sema_typeval_or,

	ofc_sema_typeval_eqv,
	ofc_sema_typeval_neqv,
};


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
		case OFC_SEMA_EXPR_CONSTANT:
			expr->constant = NULL;
			break;
		case OFC_SEMA_EXPR_LHS:
			expr->lhs = NULL;
			break;
		case OFC_SEMA_EXPR_CAST:
			expr->cast.type = NULL;
			expr->cast.expr = NULL;
			break;
		case OFC_SEMA_EXPR_INTRINSIC:
			/* TODO - Implement. */
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

ofc_sema_expr_t* ofc_sema_expr_cast(
	const ofc_sema_scope_t* scope,
	ofc_sema_expr_t* expr,
	const ofc_sema_type_t* type)
{
	if (!type || !expr)
		return NULL;

	if (ofc_sema_expr_is_constant(expr))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr__create(
				OFC_SEMA_EXPR_CONSTANT);
		if (!cast) return NULL;

		cast->constant = ofc_sema_typeval_cast(
			scope, expr->constant, type);
		if (!cast->constant)
		{
			ofc_sema_expr_delete(cast);
			return NULL;
		}

		cast->src = expr->src;
		ofc_sema_expr_delete(expr);
		return cast;
	}

	ofc_sema_expr_t* cast
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_CAST);
	if (!cast) return NULL;

	cast->src = expr->src;
	cast->cast.type = type;
	cast->cast.expr = expr;
	return cast;
}

static ofc_sema_expr_t* ofc_sema_expr__binary(
	ofc_sema_scope_t* scope,
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
		ofc_sema_scope_error(scope, a->src,
			"Can't use type TYPE in operator %s",
			ofc_parse_operator_str_rep(op));
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
		ofc_sema_scope_error(scope, b->src,
			"Can't use type TYPE in operator %s",
			ofc_parse_operator_str_rep(op));
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
			ofc_sema_scope_error(scope, a->src,
				"Incompatible types (%s, %s) in operator %s",
				ofc_parse_type_str_rep(at->type),
				ofc_parse_type_str_rep(bt->type),
				ofc_parse_operator_str_rep(op));
			ofc_sema_expr_delete(bs);
			ofc_sema_expr_delete(as);
			return NULL;
		}

		/* TODO - Warn about implicit cast? */

		if (!ofc_sema_type_compare(at, ptype))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(
					scope, as, ptype);
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
				= ofc_sema_expr_cast(
					scope, bs, ptype);
			if (!cast)
			{
				ofc_sema_expr_delete(bs);
				ofc_sema_expr_delete(as);
				return NULL;
			}
			bs = cast;
		}
	}

	if (ofc_sema_expr_is_constant(as)
		&& ofc_sema_expr_is_constant(bs)
		&& ofc_sema_expr__resolve[type])
	{
		ofc_sema_typeval_t* tv
			= ofc_sema_expr__resolve[type](scope,
				as->constant, bs->constant);
		ofc_sema_expr_delete(bs);
		ofc_sema_expr_delete(as);
		if (!tv) return NULL;

		ofc_sema_expr_t* expr
			= ofc_sema_expr__create(
				OFC_SEMA_EXPR_CONSTANT);
		if (!expr)
		{
			ofc_sema_typeval_delete(tv);
			return NULL;
		}

		expr->constant = tv;
		return expr;
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

	expr->src = ofc_str_ref_bridge(as->src, bs->src);

	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__unary(
	ofc_sema_scope_t* scope,
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
		ofc_sema_scope_error(scope, a->src,
			"Can't use type %s in operator %s",
			ofc_parse_type_str_rep(a->type),
			ofc_parse_operator_str_rep(op));
		ofc_sema_expr_delete(as);
		return NULL;
	}

	if (ofc_sema_expr_is_constant(as)
		&& ofc_sema_expr__resolve[type])
	{
		ofc_sema_typeval_t* tv
			= ofc_sema_expr__resolve[type](scope,
				as->constant, NULL);
		ofc_sema_expr_delete(as);
		if (!tv) return NULL;

		ofc_sema_expr_t* expr
			= ofc_sema_expr__create(
				OFC_SEMA_EXPR_CONSTANT);
		if (!expr)
		{
			ofc_sema_typeval_delete(tv);
			return NULL;
		}

		expr->constant = tv;
		return expr;
	}

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(type);
	if (!expr)
	{
		ofc_sema_expr_delete(as);
		return NULL;
	}

	expr->a = as;
	expr->src = a->src;
	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__literal(
	const ofc_sema_scope_t* scope,
	const ofc_parse_literal_t* literal)
{
	ofc_sema_typeval_t* tv
		= ofc_sema_typeval_literal(
			scope, literal, NULL);
	if (!tv) return NULL;

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_CONSTANT);
	if (!expr)
	{
		ofc_sema_typeval_delete(tv);
		return NULL;
	}

	expr->constant = tv;
	expr->src = literal->src;
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

	const ofc_sema_typeval_t* ctv
		= ofc_sema_parameter_get(param);
	if (!ctv) return NULL;

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_CONSTANT);
	if (!expr) return NULL;

	expr->constant
		= ofc_sema_typeval_copy(ctv);
	if (!expr->constant)
	{
		ofc_sema_expr_delete(expr);
		return NULL;
	}

	expr->src = name->src;
	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__lhs(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* name)
{
	if (!name)
		return false;

	ofc_sema_lhs_t* lhs = ofc_sema_lhs_expr(
		(ofc_sema_scope_t*)scope, name);
	if (!lhs) return NULL;

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_LHS);
	if (!expr)
	{
		ofc_sema_lhs_delete(lhs);
		return NULL;
	}

	expr->lhs = lhs;
	expr->src = name->src;
	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__variable(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* name)
{
	ofc_sema_expr_t* expr
		= ofc_sema_expr__parameter(scope, name);
	if (!expr) expr = ofc_sema_expr__lhs(scope, name);

	/* TODO - Intrinsics */

	/* TODO - Functions */

	return expr;
}


ofc_sema_expr_t* ofc_sema_expr(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr)
{
	if (!expr)
		return NULL;

	switch (expr->type)
	{
		case OFC_PARSE_EXPR_CONSTANT:
			return ofc_sema_expr__literal(
				scope, &expr->literal);
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
		case OFC_SEMA_EXPR_CONSTANT:
			ofc_sema_typeval_delete(
				expr->constant);
			break;
		case OFC_SEMA_EXPR_LHS:
			ofc_sema_lhs_delete(expr->lhs);
			break;
		case OFC_SEMA_EXPR_CAST:
			ofc_sema_expr_delete(expr->cast.expr);
			break;
		case OFC_SEMA_EXPR_INTRINSIC:
			/* TODO - Implement. */
			break;
		default:
			ofc_sema_expr_delete(expr->b);
			ofc_sema_expr_delete(expr->a);
			break;
	}

	free(expr);
}


bool ofc_sema_expr_compare(
	const ofc_sema_expr_t* a,
	const ofc_sema_expr_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if (a->type != b->type)
		return false;

	switch (a->type)
	{
		case OFC_SEMA_EXPR_CONSTANT:
			return ofc_sema_typeval_compare(
				a->constant, b->constant);

		case OFC_SEMA_EXPR_LHS:
			return ofc_sema_lhs_compare(a->lhs, b->lhs);

		case OFC_SEMA_EXPR_CAST:
			if (!ofc_sema_type_compare(
				a->cast.type, b->cast.type))
				return false;

			return ofc_sema_expr_compare(
				a->cast.expr, b->cast.expr);

		case OFC_SEMA_EXPR_INTRINSIC:
			return false;

		default:
			break;
	}

	if ((a->a != b->a)
		&& !ofc_sema_expr_compare(a->a, b->a))
		return false;

	return ((a->b == b->b)
		|| ofc_sema_expr_compare(a->b, b->b));
}



ofc_sema_expr_list_t* ofc_sema_expr_list(
	ofc_sema_scope_t*            scope,
	const ofc_parse_expr_list_t* list)
{
	if (!list)
		return NULL;

	ofc_sema_expr_list_t* slist
		= ofc_sema_expr_list_create();
	if (!slist) return NULL;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		ofc_sema_expr_t* expr = ofc_sema_expr(
			scope, list->expr[i]);
		if (!expr)
		{
			ofc_sema_expr_list_delete(slist);
			return NULL;
		}

		if (!ofc_sema_expr_list_add(slist, expr))
		{
			ofc_sema_expr_delete(expr);
			ofc_sema_expr_list_delete(slist);
			return NULL;
		}
	}

	return slist;
}

ofc_sema_expr_list_t* ofc_sema_expr_list_create(void)
{
	ofc_sema_expr_list_t* list
		= (ofc_sema_expr_list_t*)malloc(
			sizeof(ofc_sema_expr_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->expr  = NULL;
	return list;
}

void ofc_sema_expr_list_delete(
	ofc_sema_expr_list_t* list)
{
	if (!list)
		return;

	unsigned i;
	for (i = 0; i < list->count; i++)
		ofc_sema_expr_delete(list->expr[i]);
	free(list->expr);

	free(list);
}

bool ofc_sema_expr_list_add(
	ofc_sema_expr_list_t* list,
	ofc_sema_expr_t* expr)
{
	if (!list || !expr)
		return false;

	ofc_sema_expr_t** nexpr
		= (ofc_sema_expr_t**)realloc(list->expr,
			(sizeof(ofc_sema_expr_t*) * (list->count + 1)));
	if (!nexpr) return NULL;

	list->expr = nexpr;
	list->expr[list->count++] = expr;
	return true;
}

unsigned ofc_sema_expr_list_count(
	const ofc_sema_expr_list_t* list)
{
	return (list ? list->count : 0);
}

const ofc_sema_type_t* ofc_sema_expr_type(
	const ofc_sema_expr_t* expr)
{
	if (!expr)
		return NULL;

	switch (expr->type)
	{
		case OFC_SEMA_EXPR_CONSTANT:
			return expr->constant->type;
		case OFC_SEMA_EXPR_LHS:
			return ofc_sema_lhs_type(
				expr->lhs);
		case OFC_SEMA_EXPR_CAST:
			return expr->cast.type;
		case OFC_SEMA_EXPR_INTRINSIC:
			/* TODO - Implement. */
			return NULL;
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



bool ofc_sema_expr_validate_uint(
	const ofc_sema_expr_t* expr)
{
	if (!expr)
		return false;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(expr);
	if (!ofc_sema_type_is_integer(type))
		return false;

	const ofc_sema_typeval_t* tv
		= ofc_sema_expr_constant(expr);
    if (tv)
	{
		int64_t v;
		bool has_val = ofc_sema_typeval_get_integer(tv, &v);
		if (has_val && (v < 0)) return false;
	}

	return true;
}

bool ofc_sema_expr_resolve_uint(
	const ofc_sema_expr_t* expr,
	unsigned* value)
{
	const ofc_sema_typeval_t* tv
		= ofc_sema_expr_constant(expr);
	if (!tv) return false;

	int64_t i;
	if (!ofc_sema_typeval_get_integer(tv, &i))
		return false;
	if (i < 0) return false;

	unsigned u = i;
	if (i != u)
		return false;

	if (value) *value = u;
	return true;
}
