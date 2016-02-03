/* Copyright 2015 Codethink Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "ofc/sema.h"
#include <math.h>


const ofc_sema_typeval_t* ofc_sema_expr_constant(
	const ofc_sema_expr_t* expr)
{
	return (expr ? expr->constant : NULL);
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
	return ofc_sema_type_logical_default();
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
	{ NULL, 0, 0, 0, 0, 0 }, /* FUNCTION */
	{ NULL, 0, 0, 0, 0, 0 }, /* ALT_RETURN */
	{ NULL, 0, 0, 0, 0, 0 }, /* IMPLICIT_DO */

	{ NULL, 0, 1, 1, 1, 0 }, /* POWER */
	{ NULL, 0, 1, 1, 1, 0 }, /* MULTIPLY */
	{ NULL, 0, 0, 0, 0, 1 }, /* CONCAT */
	{ NULL, 0, 1, 1, 1, 0 }, /* DIVIDE */
	{ NULL, 0, 1, 1, 1, 0 }, /* ADD */
	{ NULL, 0, 1, 1, 1, 0 }, /* SUBTRACT */
	{ NULL, 0, 1, 1, 1, 0 }, /* NEGATE */

	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 1, 1 }, /* EQ */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 1, 1 }, /* NE */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 0, 1 }, /* LT */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 0, 1 }, /* LE */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 0, 1 }, /* GT */
	{ OFC_SEMA_EXPR__LOGICAL_RETURN, 0, 1, 1, 0, 1 }, /* GE */

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
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	(void)b;
	return ofc_sema_typeval_negate(a);
}

static ofc_sema_typeval_t* ofc_sema_typeval_not__faux_binary(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b)
{
	(void)b;
	return ofc_sema_typeval_not(a);
}

static ofc_sema_typeval_t* (*ofc_sema_expr__resolve[])(
	const ofc_sema_typeval_t*,
	const ofc_sema_typeval_t*) =
{
	NULL, /* CONSTANT */
	NULL, /* LHS */
	NULL, /* CAST */
	NULL, /* INTRINSIC */
	NULL, /* FUNCTION */
	NULL, /* ALT_RETURN */
	NULL, /* IMPLICIT_DO */

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

	expr->src = OFC_SPARSE_REF_EMPTY;

	expr->constant = NULL;

	expr->brackets = false;

	expr->repeat = 1;

	switch (type)
	{
		case OFC_SEMA_EXPR_CONSTANT:
			break;
		case OFC_SEMA_EXPR_LHS:
			expr->lhs = NULL;
			break;
		case OFC_SEMA_EXPR_CAST:
			expr->cast.type = NULL;
			expr->cast.expr = NULL;
			break;
		case OFC_SEMA_EXPR_INTRINSIC:
			expr->intrinsic = NULL;
			expr->args      = NULL;
			break;
		case OFC_SEMA_EXPR_FUNCTION:
			expr->function = NULL;
			expr->args     = NULL;
			break;
		case OFC_SEMA_EXPR_ALT_RETURN:
			expr->alt_return.expr = NULL;
			break;
		case OFC_SEMA_EXPR_IMPLICIT_DO:
			expr->implicit_do.expr = NULL;
			expr->implicit_do.iter = NULL;
			expr->implicit_do.init = NULL;
			expr->implicit_do.last = NULL;
			expr->implicit_do.step = NULL;
			break;
		default:
			expr->a = NULL;
			expr->b = NULL;
			break;
	}

	return expr;
}

ofc_sema_expr_t* ofc_sema_expr_copy_replace(
	const ofc_sema_expr_t* expr,
	const ofc_sema_decl_t* replace,
	const ofc_sema_expr_t* with)
{
	if (!expr) return NULL;

	ofc_sema_expr_t* copy
		= ofc_sema_expr__create(expr->type);
	if (!copy) return NULL;

	copy->src = expr->src;

	copy->brackets = expr->brackets;

	copy->repeat = expr->repeat;

	if (expr->constant)
	{
		copy->constant = ofc_sema_typeval_copy(expr->constant);
		if (!copy->constant)
		{
			ofc_sema_expr_delete(copy);
			return NULL;
		}
	}

	bool success = true;
	switch (copy->type)
	{
		case OFC_SEMA_EXPR_CONSTANT:
			break;

		case OFC_SEMA_EXPR_LHS:
			if (with && expr->lhs
				&& (expr->lhs->type == OFC_SEMA_LHS_DECL)
				&& (expr->lhs->decl == replace))
			{
				ofc_sema_expr_delete(copy);
				return ofc_sema_expr_copy(with);
			}
			else if (with)
			{
				copy->lhs = ofc_sema_lhs_copy_replace(
					expr->lhs, replace, with);
				if (!copy->lhs)
				{
					ofc_sema_expr_delete(copy);
					return NULL;
				}
			}
			else
			{
				if (!ofc_sema_lhs_reference(expr->lhs))
				{
					ofc_sema_expr_delete(copy);
					return NULL;
				}
				copy->lhs = expr->lhs;
			}
			break;

		case OFC_SEMA_EXPR_CAST:
			copy->cast.type = expr->cast.type;
			copy->cast.expr = ofc_sema_expr_copy_replace(
				expr->cast.expr, replace, with);
			success = (copy->cast.expr != NULL);
			break;

		case OFC_SEMA_EXPR_INTRINSIC:
			copy->intrinsic = expr->intrinsic;
			copy->args      = ofc_sema_expr_list_copy_replace(
				expr->args, replace, with);
			success = (copy->args != NULL);
			break;

		case OFC_SEMA_EXPR_FUNCTION:
			copy->function = expr->function;
			copy->args     = ofc_sema_expr_list_copy_replace(
				expr->args, replace, with);
			success = (copy->args != NULL);
			break;

		case OFC_SEMA_EXPR_ALT_RETURN:
			copy->alt_return.expr = ofc_sema_expr_copy_replace(
				expr->alt_return.expr, replace, with);
			success = (copy->alt_return.expr != NULL);
			break;

		case OFC_SEMA_EXPR_IMPLICIT_DO:
			copy->implicit_do.expr
				= ofc_sema_expr_copy_replace(
					expr->implicit_do.expr, replace, with);
			copy->implicit_do.iter
				= (ofc_sema_decl_reference(expr->implicit_do.iter)
					? expr->implicit_do.iter : NULL);
			copy->implicit_do.init
				= ofc_sema_expr_copy_replace(
					expr->implicit_do.init, replace, with);
			copy->implicit_do.last
				= ofc_sema_expr_copy_replace(
					expr->implicit_do.last, replace, with);
			copy->implicit_do.step
				= ofc_sema_expr_copy_replace(
					expr->implicit_do.step, replace, with);
			if (!copy->implicit_do.expr
				|| !copy->implicit_do.iter
				|| !copy->implicit_do.init
				|| !copy->implicit_do.last)
				success = false;
			if (expr->implicit_do.step
				&& !copy->implicit_do.step)
				success = false;
			break;

		default:
			copy->a = ofc_sema_expr_copy_replace(
				expr->a, replace, with);
			copy->b = ofc_sema_expr_copy_replace(
				expr->b, replace, with);

			/* We need to re-resolve the constant incase a subexpression
			   has been replaced. */
			if (ofc_sema_expr_is_constant(copy->a)
				&& ofc_sema_expr__resolve[expr->type])
			{
				if (!copy->b)
				{
					ofc_sema_typeval_delete(copy->constant);
					copy->constant = ofc_sema_expr__resolve[expr->type](
						copy->a->constant, NULL);
				}
				else if (ofc_sema_expr_is_constant(copy->b))
				{
					ofc_sema_typeval_delete(copy->constant);
					copy->constant = ofc_sema_expr__resolve[expr->type](
						copy->a->constant, copy->b->constant);
				}
			}

			success = (copy->a && (!expr->b || (copy->b != NULL)));
			break;
	}

	if (!success)
	{
		ofc_sema_expr_delete(copy);
		return NULL;
	}

	return copy;
}

ofc_sema_expr_t* ofc_sema_expr_copy(
	const ofc_sema_expr_t* expr)
{
	return ofc_sema_expr_copy_replace(
		expr, NULL, NULL);
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
	ofc_sema_expr_t* expr,
	const ofc_sema_type_t* type)
{
	if (!type || !expr)
		return NULL;

	if (!ofc_sema_type_cast_valid(
		ofc_sema_expr_type(expr), type))
		return NULL;

	ofc_sema_expr_t* cast
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_CAST);
	if (!cast) return NULL;

	if (ofc_sema_expr_is_constant(expr))
	{
		cast->constant = ofc_sema_typeval_cast(
			expr->constant, type);
		if (!cast->constant)
		{
			ofc_sema_expr_delete(cast);
			return NULL;
		}
	}
	else if (!ofc_sema_type_cast_is_lossless(
		ofc_sema_expr_type(expr), type))
	{
		ofc_sparse_ref_warning(expr->src,
			"Implicit cast may be lossy.");
	}

	cast->src = expr->src;
	cast->cast.type = type;
	cast->cast.expr = expr;
	return cast;
}

ofc_sema_expr_t* ofc_sema_expr_alt_return(
	ofc_sema_expr_t* expr)
{
	if (!expr)
		return NULL;

	ofc_sema_expr_t* alt_return
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_ALT_RETURN);
	if (!alt_return) return NULL;

	if (ofc_sema_expr_is_constant(expr))
	{
		alt_return->constant
			= ofc_sema_typeval_copy(expr->constant);
		if (!alt_return->constant)
		{
			ofc_sema_expr_delete(alt_return);
			return NULL;
		}
	}

	alt_return->src = expr->src;
	alt_return->alt_return.expr = expr;
	return alt_return;
}

ofc_sema_expr_t* ofc_sema_expr_typeval(
	ofc_sema_typeval_t* typeval)
{
	if (!typeval)
		return NULL;

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(OFC_SEMA_EXPR_CONSTANT);
	if (!expr) return NULL;

	expr->constant = typeval;
	expr->src = typeval->src;
	return expr;
}

ofc_sema_expr_t* ofc_sema_expr_integer(int value)
{
	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(OFC_SEMA_EXPR_CONSTANT);
	if (!expr) return NULL;

	expr->constant = ofc_sema_typeval_create_integer(
		value, OFC_SPARSE_REF_EMPTY);
	if (!expr->constant)
	{
		ofc_sema_expr_delete(expr);
		return NULL;
	}

	return expr;
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

	ofc_sema_expr_t* as = ofc_sema_expr(
		scope, a);
	if (!as) return NULL;

	const ofc_sema_type_t* at
		= ofc_sema_expr_type(as);
	if (!at)
	{
		ofc_sema_expr_delete(as);
		return NULL;
	}

	if (!ofc_sema_expr_type_allowed(type, at))
	{
		ofc_sparse_ref_error(a->src,
			"Can't use type %s in operator '%s'",
			ofc_sema_type_str_rep(at),
			ofc_parse_operator_str_rep(op));
		ofc_sema_expr_delete(as);
		return NULL;
	}

	ofc_sema_expr_t* bs = ofc_sema_expr(
		scope, b);
	if (!bs)
	{
		ofc_sema_expr_delete(as);
		return NULL;
	}

	const ofc_sema_type_t* bt
		= ofc_sema_expr_type(bs);
	if (!bt)
	{
		ofc_sema_expr_delete(bs);
		ofc_sema_expr_delete(as);
		return NULL;
	}

	if (!ofc_sema_expr_type_allowed(type, bt))
	{
		ofc_sparse_ref_error(a->src,
			"Can't use type %s in operator '%s'",
			ofc_sema_type_str_rep(bt),
			ofc_parse_operator_str_rep(op));
		ofc_sema_expr_delete(bs);
		ofc_sema_expr_delete(as);
		return NULL;
	}

	if ((at->type == OFC_SEMA_TYPE_CHARACTER)
		&& (bt->type == OFC_SEMA_TYPE_CHARACTER))
	{
		unsigned asize, bsize;
		if (!ofc_sema_type_base_size(at, &asize)
			|| !ofc_sema_type_base_size(bt, &bsize))
		{
			ofc_sema_expr_delete(bs);
			ofc_sema_expr_delete(as);
			return NULL;
		}

		if (asize < bsize)
		{
			at = ofc_sema_type_create_character(
				bt->kind, at->len, at->len_var);
			if (!at)
			{
				ofc_sema_expr_delete(bs);
				ofc_sema_expr_delete(as);
				return NULL;
			}

			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(as, at);
			if (!cast)
			{
				ofc_sema_expr_delete(bs);
				ofc_sema_expr_delete(as);
				return NULL;
			}
			as = cast;
		}
		else
		{
			bt = ofc_sema_type_create_character(
				at->kind, bt->len, bt->len_var);
			if (!bt)
			{
				ofc_sema_expr_delete(bs);
				ofc_sema_expr_delete(as);
				return NULL;
			}

			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(bs, bt);
			if (!cast)
			{
				ofc_sema_expr_delete(bs);
				ofc_sema_expr_delete(as);
				return NULL;
			}
			bs = cast;
		}
	}
	else if (!ofc_sema_type_compatible(at, bt))
	{
		const ofc_sema_type_t* ptype
			= ofc_sema_type_promote(at, bt);
		if (!ptype)
		{
			ofc_sparse_ref_error(a->src,
				"Incompatible types (%s, %s) in operator %s",
				ofc_sema_type_str_rep(at),
				ofc_sema_type_str_rep(bt),
				ofc_parse_operator_str_rep(op));
			ofc_sema_expr_delete(bs);
			ofc_sema_expr_delete(as);
			return NULL;
		}

		/* TODO - Warn about implicit cast? */

		if (!ofc_sema_type_compatible(at, ptype))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(as, ptype);
			if (!cast)
			{
				ofc_sema_expr_delete(bs);
				ofc_sema_expr_delete(as);
				return NULL;
			}
			as = cast;
		}

		if (!ofc_sema_type_compatible(bt, ptype))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(bs, ptype);
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

	if (ofc_sema_expr_is_constant(as)
		&& ofc_sema_expr_is_constant(bs)
		&& ofc_sema_expr__resolve[type])
	{
		expr->constant = ofc_sema_expr__resolve[type](
			as->constant, bs->constant);
	}

	expr->a = as;
	expr->b = bs;

	expr->src = OFC_SPARSE_REF_EMPTY;
	ofc_sparse_ref_bridge(
		as->src, bs->src, &expr->src);

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

	const ofc_sema_type_t* at
		= ofc_sema_expr_type(as);
	if (!ofc_sema_expr_type_allowed(type, at))
	{
		ofc_sparse_ref_error(a->src,
			"Can't use type %s in operator %s",
			ofc_sema_type_str_rep(at),
			ofc_parse_operator_str_rep(op));
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

	if (ofc_sema_expr_is_constant(as)
		&& ofc_sema_expr__resolve[type])
	{
		expr->constant = ofc_sema_expr__resolve[type](
			as->constant, NULL);
	}

	expr->a = as;
	expr->src = a->src;
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

static ofc_sema_expr_t* ofc_sema_expr__intrinsic(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* name,
	const ofc_sema_intrinsic_t* intrinsic)
{
	if (!name)
		return NULL;

	if (!intrinsic
		|| (name->type != OFC_PARSE_LHS_ARRAY)
		|| !name->array.index
		|| !name->parent
		|| (name->parent->type != OFC_PARSE_LHS_VARIABLE))
	{
		ofc_sparse_ref_error(name->src,
			"Invalid invocation of INTRINSIC function.");
		return NULL;
	}

	ofc_sema_expr_list_t* args = NULL;
	if (name->array.index->count > 0)
	{
		if (!name->array.index->range)
			return NULL;

		args = ofc_sema_expr_list_create();
		if (!args) return NULL;

		unsigned i;
		for (i = 0; i < name->array.index->count; i++)
		{
			const ofc_parse_array_range_t* range
				= name->array.index->range[i];

			if (!range || range->is_slice
				|| range->last || range->stride)
			{
				ofc_sema_expr_list_delete(args);
				return NULL;
			}

			ofc_sema_expr_t* expr
				= ofc_sema_expr(scope, range->first);
			if (!expr)
			{
				ofc_sema_expr_list_delete(args);
				return NULL;
			}

			if (!ofc_sema_expr_list_add(args, expr))
			{
				ofc_sema_expr_delete(expr);
				ofc_sema_expr_list_delete(args);
				return NULL;
			}
		}

		args = ofc_sema_intrinsic_cast(
			name->src, intrinsic, args);
		if (!args) return NULL;
	}

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_INTRINSIC);
	if (!expr)
	{
		ofc_sema_expr_list_delete(args);
		return NULL;
	}

	expr->intrinsic = intrinsic;
	expr->args      = args;

	expr->src = OFC_SPARSE_REF_EMPTY;
	ofc_sparse_ref_bridge(
		name->parent->src, name->src,
		&expr->src);

	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__function(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* name,
	const ofc_sema_decl_t* decl)
{
	if (!name)
		return NULL;

	if (!decl || !ofc_sema_decl_is_function(decl)
		|| (name->type != OFC_PARSE_LHS_ARRAY)
		|| !name->parent
		|| (name->parent->type != OFC_PARSE_LHS_VARIABLE))
		return NULL;

	/* TODO - Defer checking of arguments for a later pass? */
	const ofc_sema_scope_t* fscope = decl->func;
	if (fscope)
	{
		unsigned acount = 0;
		if (name->array.index)
			acount = name->array.index->count;

		if (fscope->args
			? (acount != fscope->args->count)
			: (acount != 0))
		{
			ofc_sparse_ref_error(name->src,
				"Incorrect number of arguments in function call.");
			return NULL;
		}
	}

	ofc_sema_expr_list_t* args = NULL;
	if (name->array.index
		&& (name->array.index->count > 0))
	{
		if (!name->array.index->range)
			return NULL;

		args = ofc_sema_expr_list_create();
		if (!args) return NULL;

		unsigned i;
		for (i = 0; i < name->array.index->count; i++)
		{
			const ofc_parse_array_range_t* range
				= name->array.index->range[i];

			if (!range || range->is_slice
				|| range->last || range->stride)
			{
				ofc_sema_expr_list_delete(args);
				return NULL;
			}

			ofc_sema_expr_t* expr
				= ofc_sema_expr(scope, range->first);
			if (!expr)
			{
				ofc_sema_expr_list_delete(args);
				return NULL;
			}

			if (!ofc_sema_expr_list_add(args, expr))
			{
				ofc_sema_expr_delete(expr);
				ofc_sema_expr_list_delete(args);
				return NULL;
			}
		}

		/* TODO - Validate and cast arguments. */
	}

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_FUNCTION);
	if (!expr)
	{
		ofc_sema_expr_list_delete(args);
		return NULL;
	}

	expr->function = decl;
	expr->args     = args;

	expr->src = OFC_SPARSE_REF_EMPTY;
	ofc_sparse_ref_bridge(
		name->parent->src, name->src, &expr->src);

	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__lhs(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* name)
{
	if (!name)
		return NULL;

	ofc_sema_lhs_t* lhs = ofc_sema_lhs_in_expr(
		scope, name);
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

	if (ofc_sema_lhs_is_parameter(lhs))
	{
		expr->constant
			= ofc_sema_lhs_parameter(lhs);
		if (!expr->constant)
		{
			ofc_sema_expr_delete(expr);
			return NULL;
		}
	}

	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__variable(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* name)
{
	ofc_sparse_ref_t base_name;
	if (!ofc_parse_lhs_base_name(
		*name, &base_name))
		return NULL;

	const ofc_sema_intrinsic_t* intrinsic
		= ofc_sema_intrinsic(scope, base_name.string);
	const ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find(
			scope, base_name.string, false);

	ofc_sema_expr_t* expr;
	if (intrinsic && !decl
		&& (name->type == OFC_PARSE_LHS_ARRAY))
	{
		expr = ofc_sema_expr__intrinsic(
			scope, name, intrinsic);
	}
	else if (ofc_sema_decl_is_function(decl))
	{
		expr = ofc_sema_expr__function(
			scope, name, decl);

		if (!expr)
		{
			/* FUNCTION types are only valid as arguments. */
			expr = ofc_sema_expr__lhs(
				scope, name);
		}
	}
	else
	{
		bool is_array    = false;
		bool is_function = false;
		if (decl)
		{
			is_array = ofc_sema_decl_is_array(decl);
			is_function = ofc_sema_decl_is_function(decl);
		}
		else
		{
			const ofc_sema_spec_t* spec
				= ofc_sema_scope_spec_modify(
					scope, base_name);
			if (spec)
			{
				is_array    = (spec->array != NULL);
				is_function = (spec->is_intrinsic || spec->is_external);
			}
		}

		if ((is_function || !is_array)
			&& ofc_parse_lhs_possible_function_call(*name))
		{
			if ((name->type != OFC_PARSE_LHS_ARRAY)
				|| !name->parent
				|| (name->parent->type != OFC_PARSE_LHS_VARIABLE))
			{
				/* TODO - Handle complicated functions
						  returning arrays or structures. */
				return NULL;
			}

			ofc_sema_spec_t* fspec
				= ofc_sema_scope_spec_find_final(
					scope, base_name);
			ofc_sema_decl_t* fdecl
				= ofc_sema_decl_function(
					scope, base_name, fspec);
			ofc_sema_spec_delete(fspec);
			if (!fdecl) return NULL;

			expr = ofc_sema_expr__function(
				scope, name, fdecl);
			if (!expr)
			{
				ofc_sparse_ref_error(name->src,
					"Invalid invocation of function");
			}
		}
		else
		{
			expr = ofc_sema_expr__lhs(
				scope, name);
		}
	}

	return expr;
}


static ofc_sema_expr_t* ofc_sema_expr__brackets(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr)
{
	if (!scope || !expr)
		return NULL;

	ofc_sema_expr_t* expr_bracket
		= ofc_sema_expr(scope, expr);

	if (!expr_bracket)
		return NULL;

	expr_bracket->brackets = true;
	return expr_bracket;
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
				&expr->literal);
		case OFC_PARSE_EXPR_VARIABLE:
			return ofc_sema_expr__variable(
				scope, expr->variable);
		case OFC_PARSE_EXPR_BRACKETS:
			return ofc_sema_expr__brackets(
				scope, expr->brackets.expr);
		case OFC_PARSE_EXPR_UNARY:
			return ofc_sema_expr__unary(
				scope, expr->unary.operator,
				expr->unary.a);
		case OFC_PARSE_EXPR_BINARY:
			return ofc_sema_expr__binary(
				scope, expr->binary.operator,
				expr->binary.a, expr->binary.b);
		default:
			break;
	}

	return NULL;
}

static ofc_sema_expr_t* ofc_sema_expr__repeat(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr)
{
	if ((expr->type != OFC_PARSE_EXPR_BINARY)
		|| (expr->binary.operator != OFC_PARSE_OPERATOR_MULTIPLY))
		return NULL;

	ofc_sema_expr_t* rexpr
		= ofc_sema_expr(scope, expr->binary.a);
	unsigned repeat;
	bool success = ofc_sema_expr_resolve_uint(rexpr, &repeat);
	ofc_sema_expr_delete(rexpr);
	if (!success) return NULL;

	ofc_sema_expr_t* e
		= ofc_sema_expr(scope, expr->binary.b);
	if (!e) return NULL;

	e->repeat = repeat;
	return e;
}

ofc_sema_expr_t* ofc_sema_expr_repeat(
	ofc_sema_scope_t* scope,
	const ofc_parse_expr_t* expr)
{
	if (!expr) return NULL;
	ofc_sema_expr_t* e
		= ofc_sema_expr__repeat(scope, expr);
	if (!e) e = ofc_sema_expr(scope, expr);
	return e;
}

static ofc_sema_expr_t* ofc_sema_expr__implicit_do(
	ofc_sema_scope_t* scope,
	ofc_sparse_ref_t src,
	const ofc_parse_implicit_do_t* id)
{
	if (!id || !id->init)
		return NULL;

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_IMPLICIT_DO);
	if (!expr) return NULL;
	expr->src = src;

	ofc_sema_lhs_t* iter_lhs = ofc_sema_lhs(
		scope, id->init->name);
	if (!iter_lhs)
	{
		ofc_sema_expr_delete(expr);
		return NULL;
	}

	if (iter_lhs->type != OFC_SEMA_LHS_DECL)
	{
		ofc_sparse_ref_error(id->init->name->src,
			"Implicit do loop iterator must be a variable");
		ofc_sema_lhs_delete(iter_lhs);
		ofc_sema_expr_delete(expr);
		return NULL;
	}

	if (!ofc_sema_decl_reference(iter_lhs->decl))
	{
		ofc_sema_lhs_delete(iter_lhs);
		ofc_sema_expr_delete(expr);
		return NULL;
	}

	expr->implicit_do.iter = iter_lhs->decl;
	ofc_sema_lhs_delete(iter_lhs);

	const ofc_sema_type_t* iter_type
		= ofc_sema_decl_type(expr->implicit_do.iter);
	if (!iter_type)
	{
		ofc_sema_expr_delete(expr);
		return NULL;
	}

	if (!ofc_sema_type_is_scalar(iter_type))
	{
		ofc_sparse_ref_error(id->init->name->src,
			"Implicit do loop iterator must be a scalar type");
		ofc_sema_expr_delete(expr);
		return NULL;
	}

	if (!ofc_sema_type_is_integer(iter_type))
	{
		ofc_sparse_ref_warning(id->init->name->src,
			"Using REAL in implicit do loop iterator");
	}

	expr->implicit_do.init = ofc_sema_expr(
		scope, id->init->init);
	if (!expr->implicit_do.init)
	{
		ofc_sema_expr_delete(expr);
		return NULL;
	}

	const ofc_sema_type_t* init_type
		= ofc_sema_expr_type(expr->implicit_do.init);
	if (!init_type)
	{
		ofc_sema_expr_delete(expr);
		return NULL;
	}
	else if (!ofc_sema_type_compatible(
		iter_type, init_type))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				expr->implicit_do.init, iter_type);
		if (!cast)
		{
			ofc_sema_expr_delete(expr);
			return NULL;
		}

		expr->implicit_do.init = cast;
	}

	expr->implicit_do.last = ofc_sema_expr(
		scope, id->limit);
	if (!expr->implicit_do.last)
	{
		ofc_sema_expr_delete(expr);
		return NULL;
	}

	const ofc_sema_type_t* last_type
		= ofc_sema_expr_type(expr->implicit_do.last);
	if (!last_type)
	{
		ofc_sema_expr_delete(expr);
		return NULL;
	}
	else if (!ofc_sema_type_compatible(
		iter_type, last_type))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				expr->implicit_do.last, iter_type);
		if (!cast)
		{
			ofc_sparse_ref_error(id->limit->src,
				"Expression type '%s' doesn't match iterator type '%s'",
				ofc_sema_type_str_rep(last_type),
				ofc_sema_type_str_rep(iter_type));
			ofc_sema_expr_delete(expr);
			return NULL;
		}

		expr->implicit_do.last = cast;
	}

	if (id->step)
	{
		expr->implicit_do.step
			= ofc_sema_expr(scope, id->step);
		if (!expr->implicit_do.step)
		{
			ofc_sema_expr_delete(expr);
			return NULL;
		}

		const ofc_sema_type_t* step_type
			= ofc_sema_expr_type(expr->implicit_do.step);
		if (!step_type)
		{
			ofc_sema_expr_delete(expr);
			return NULL;
		}
		else if (!ofc_sema_type_compatible(
			iter_type, step_type))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(
					expr->implicit_do.step, iter_type);
			if (!cast)
			{
				ofc_sparse_ref_error(id->step->src,
					"Expression type '%s' doesn't match iterator type '%s'",
					ofc_sema_type_str_rep(step_type),
					ofc_sema_type_str_rep(iter_type));
				ofc_sema_expr_delete(expr);
				return NULL;
			}

			expr->implicit_do.step = cast;
		}
	}

	if (id->dlist && (id->dlist->type == OFC_PARSE_LHS_IMPLICIT_DO))
	{
		expr->implicit_do.expr
			= ofc_sema_expr__implicit_do(
				scope, id->dlist->src, id->dlist->implicit_do);
		if (!expr->implicit_do.expr)
		{
			ofc_sema_expr_delete(expr);
			return NULL;
		}
	}
	else
	{
		expr->implicit_do.expr = ofc_sema_expr__create(OFC_SEMA_EXPR_LHS);
		if (!expr->implicit_do.expr)
		{
			ofc_sema_expr_delete(expr);
			return NULL;
		}
		expr->implicit_do.expr->lhs = ofc_sema_lhs(
			scope, id->dlist);
		if (!expr->implicit_do.expr->lhs)
		{
			ofc_sema_expr_delete(expr);
			return NULL;
		}
		expr->implicit_do.expr->src = id->dlist->src;
	}

	const ofc_sema_typeval_t* ctv[3];
	ctv[0] = ofc_sema_expr_constant(
			expr->implicit_do.init);
	ctv[1] = ofc_sema_expr_constant(
			expr->implicit_do.last);
	ctv[2] = ofc_sema_expr_constant(
			expr->implicit_do.step);

	long double first, last, step = 1.0;
	if (ofc_sema_typeval_get_real(ctv[0], &first)
		&& ofc_sema_typeval_get_real(ctv[1], &last)
		&& (!ctv[2] || ofc_sema_typeval_get_real(ctv[2], &step)))
	{
		long double dcount = floor((last - first) / step);
		if (dcount < 0.0)
		{
			ofc_sparse_ref_error(src,
				"Loop iterates away from limit");
			ofc_sema_expr_delete(expr);
			return NULL;
		}
		dcount += 1.0;

		expr->implicit_do.count = (unsigned)dcount;
		if ((long double)expr->implicit_do.count != dcount)
		{
			ofc_sema_expr_delete(expr);
			return NULL;
		}
		expr->implicit_do.count_var = false;
	}
	else
	{
		expr->implicit_do.count_var = true;
		expr->implicit_do.count     = 0;
	}

	return expr;
}

ofc_sema_expr_t* ofc_sema_expr_wrap_lhs(
	ofc_sema_lhs_t* lhs)
{
	if (!lhs) return NULL;

	ofc_sema_expr_t* expr
		= ofc_sema_expr__create(OFC_SEMA_EXPR_LHS);
	if (!expr) return NULL;

	if (!ofc_sema_lhs_reference(lhs))
	{
		ofc_sema_expr_delete(expr);
		return NULL;
	}

	expr->lhs = lhs;
	expr->src = expr->lhs->src;

	return expr;
}

void ofc_sema_expr_delete(
	ofc_sema_expr_t* expr)
{
	if (!expr)
		return;

	ofc_sema_typeval_delete(
		expr->constant);

	switch (expr->type)
	{
		case OFC_SEMA_EXPR_CONSTANT:
			break;
		case OFC_SEMA_EXPR_LHS:
			ofc_sema_lhs_delete(expr->lhs);
			break;
		case OFC_SEMA_EXPR_CAST:
			ofc_sema_expr_delete(expr->cast.expr);
			break;
		case OFC_SEMA_EXPR_INTRINSIC:
		case OFC_SEMA_EXPR_FUNCTION:
			ofc_sema_expr_list_delete(expr->args);
			break;
		case OFC_SEMA_EXPR_ALT_RETURN:
			ofc_sema_expr_delete(expr->alt_return.expr);
			break;
		case OFC_SEMA_EXPR_IMPLICIT_DO:
			ofc_sema_expr_delete(expr->implicit_do.expr);
			ofc_sema_decl_delete(expr->implicit_do.iter);
			ofc_sema_expr_delete(expr->implicit_do.init);
			ofc_sema_expr_delete(expr->implicit_do.last);
			ofc_sema_expr_delete(expr->implicit_do.step);
			break;
		default:
			ofc_sema_expr_delete(expr->b);
			ofc_sema_expr_delete(expr->a);
			break;
	}

	free(expr);
}



const ofc_sema_array_t* ofc_sema_expr_array(
	const ofc_sema_expr_t* expr)
{
	if (!expr) return NULL;

	/* TODO - Support for all expressions. */

	switch (expr->type)
	{
		case OFC_SEMA_EXPR_LHS:
			return ofc_sema_lhs_array(expr->lhs);

		case OFC_SEMA_EXPR_CAST:
			return ofc_sema_expr_array(expr->cast.expr);

		default:
			break;
	}

	return NULL;
}

bool ofc_sema_expr_elem_count(
	const ofc_sema_expr_t* expr,
	unsigned* count)
{
	if (!expr)
		return false;

	unsigned ecount;
	if (!ofc_sema_type_elem_count(
		ofc_sema_expr_type(expr), &ecount))
		return false;

	if (expr->type == OFC_SEMA_EXPR_IMPLICIT_DO)
	{
		if (expr->implicit_do.count_var)
			return false;

		if (expr->implicit_do.expr
			&& (expr->implicit_do.expr->type
				== OFC_SEMA_EXPR_IMPLICIT_DO))
		{
			unsigned idecount;
			if (!ofc_sema_expr_elem_count(
				expr->implicit_do.expr, &idecount))
				return false;
			ecount *= idecount;
		}

		ecount *= expr->implicit_do.count;
	}

	const ofc_sema_array_t* array
		= ofc_sema_expr_array(expr);
	if (array)
	{
		unsigned acount;
		if (!ofc_sema_array_total(
			array, &acount))
			return false;
		ecount *= acount;
	}

	if (count) *count = ecount;
	return true;
}

ofc_sema_expr_t* ofc_sema_expr_elem_get(
	const ofc_sema_expr_t* expr, unsigned offset)
{
	if (!expr)
		return NULL;

	switch (expr->type)
	{
		case OFC_SEMA_EXPR_LHS:
		{
			if (!ofc_sema_lhs_is_array(expr->lhs))
				break;

			ofc_sema_expr_t* sub_elem
				= ofc_sema_expr__create(OFC_SEMA_EXPR_LHS);
			if (!sub_elem) return NULL;

			sub_elem->lhs = ofc_sema_lhs_elem_get(
				expr->lhs, offset);
			if (!sub_elem->lhs)
			{
				ofc_sema_expr_delete(sub_elem);
				return NULL;
			}

			return sub_elem;
		}

		case OFC_SEMA_EXPR_CAST:
		{
			ofc_sema_expr_t* sexpr
				= ofc_sema_expr_elem_get(
					expr->cast.expr, offset);
			if (!sexpr) return NULL;

			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(
					sexpr, expr->cast.type);
			if (!cast)
			{
				ofc_sema_expr_delete(sexpr);
				return NULL;
			}

			return cast;
		}

		case OFC_SEMA_EXPR_IMPLICIT_DO:
		{
			if (!expr->implicit_do.iter)
				return NULL;

			unsigned sub_elem_count;
			if (!ofc_sema_expr_elem_count(
				expr->implicit_do.expr, &sub_elem_count))
				return NULL;

			if (sub_elem_count == 0)
				return NULL;

			unsigned sub_offset
				= (offset % sub_elem_count);
			offset /= sub_elem_count;

			const ofc_sema_typeval_t* ctv[2];
			ctv[0] = ofc_sema_expr_constant(
					expr->implicit_do.init);
			ctv[1] = ofc_sema_expr_constant(
					expr->implicit_do.step);

			long double first, step = 1.0;
			if (!ofc_sema_typeval_get_real(ctv[0], &first))
				return NULL;
			if (ctv[1] && !ofc_sema_typeval_get_real(ctv[1], &step))
				return NULL;

			long double doffset
				= first + ((long double)offset * step);

			ofc_sema_typeval_t* dinit
				= ofc_sema_typeval_create_real(
					doffset, OFC_SPARSE_REF_EMPTY);
			if (!dinit) return NULL;

			ofc_sema_typeval_t* init
				= ofc_sema_typeval_cast(
					dinit, expr->implicit_do.iter->type);
			ofc_sema_typeval_delete(dinit);
			if (!init) return NULL;

			ofc_sema_expr_t* iter_expr
				= ofc_sema_expr_typeval(init);
			if (!iter_expr)
			{
				ofc_sema_typeval_delete(init);
				return NULL;
			}

			ofc_sema_expr_t* body
				= ofc_sema_expr_copy_replace(
					expr->implicit_do.expr,
					expr->implicit_do.iter, iter_expr);
			ofc_sema_expr_delete(iter_expr);
			if (!body) return NULL;

			ofc_sema_expr_t* rval
				= ofc_sema_expr_elem_get(
					body, sub_offset);
			ofc_sema_expr_delete(body);
			return rval;
		}

		default:
			break;
	}

	if (offset != 0)
		return NULL;

	return ofc_sema_expr_copy(expr);
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

	if (ofc_sema_expr_is_constant(a)
		&& ofc_sema_expr_is_constant(b))
		return ofc_sema_typeval_compare(
			a->constant, b->constant);

	switch (a->type)
	{
		case OFC_SEMA_EXPR_CONSTANT:
			return false;

		case OFC_SEMA_EXPR_LHS:
			return ofc_sema_lhs_compare(a->lhs, b->lhs);

		case OFC_SEMA_EXPR_CAST:
			if (!ofc_sema_type_compatible(
				a->cast.type, b->cast.type))
				return false;

			return ofc_sema_expr_compare(
				a->cast.expr, b->cast.expr);

		case OFC_SEMA_EXPR_INTRINSIC:
			return ((a->intrinsic == b->intrinsic)
				&& ofc_sema_expr_list_compare(a->args, b->args));

		case OFC_SEMA_EXPR_FUNCTION:
			return ((a->function == b->function)
				&& ofc_sema_expr_list_compare(a->args, b->args));

		case OFC_SEMA_EXPR_ALT_RETURN:
			return ofc_sema_expr_compare(
				a->alt_return.expr, b->alt_return.expr);

		case OFC_SEMA_EXPR_IMPLICIT_DO:
			return (ofc_sema_expr_compare(
				a->implicit_do.expr, b->implicit_do.expr)
				&& (a->implicit_do.iter == b->implicit_do.iter)
				&& ofc_sema_expr_compare(
					a->implicit_do.init, b->implicit_do.init)
				&& ofc_sema_expr_compare(
					a->implicit_do.last, b->implicit_do.last)
				&& ofc_sema_expr_compare_def_one(
					a->implicit_do.step, b->implicit_do.step));

		default:
			break;
	}

	if ((a->a != b->a)
		&& !ofc_sema_expr_compare(a->a, b->a))
		return false;

	return ((a->b == b->b)
		|| ofc_sema_expr_compare(a->b, b->b));
}

bool ofc_sema_expr_compare_def_one(
	const ofc_sema_expr_t* a,
	const ofc_sema_expr_t* b)
{
	if (a && b)
		return ofc_sema_expr_compare(a, b);

	if (!a && !b)
		return true;

	const ofc_sema_expr_t* c = (a ? a : b);

	const ofc_sema_typeval_t* ctv
		= ofc_sema_expr_constant(c);
	if (!ctv) return false;

	return ofc_sema_typeval_is_one(ctv);
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
			return ofc_sema_intrinsic_type(
				expr->intrinsic, expr->args);
		case OFC_SEMA_EXPR_FUNCTION:
			return ofc_sema_decl_base_type(
				expr->function);
		case OFC_SEMA_EXPR_ALT_RETURN:
			return ofc_sema_expr_type(
				expr->alt_return.expr);
		case OFC_SEMA_EXPR_IMPLICIT_DO:
			return ofc_sema_expr_type(
				expr->implicit_do.expr);
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

bool ofc_sema_expr_resolve_int(
	const ofc_sema_expr_t* expr,
	int* value)
{
	const ofc_sema_typeval_t* tv
		= ofc_sema_expr_constant(expr);
	if (!tv) return false;

	int64_t i;
	if (!ofc_sema_typeval_get_integer(tv, &i))
		return false;

	int d = i;
	if (i != d)
		return false;

	if (value) *value = d;
	return true;
}



static ofc_sema_expr_list_t* ofc_sema_expr__list(
	ofc_sema_scope_t*            scope,
	const ofc_parse_expr_list_t* list,
	bool allow_repeat,
	bool allow_implicit_do)
{
	if (!list)
		return NULL;

	ofc_sema_expr_list_t* slist
		= ofc_sema_expr_list_create();
	if (!slist) return NULL;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		ofc_sema_expr_t* expr;
		if ((list->expr[i]->type == OFC_PARSE_EXPR_VARIABLE)
			&& (list->expr[i]->variable->type == OFC_PARSE_LHS_IMPLICIT_DO))
		{
			if (!allow_implicit_do)
			{
				ofc_sema_expr_list_delete(slist);
				return NULL;
			}

			expr = ofc_sema_expr__implicit_do(scope,
				list->expr[i]->variable->src,
				list->expr[i]->variable->implicit_do);
		}
		else
		{
			expr = (allow_repeat
				? ofc_sema_expr_repeat(scope, list->expr[i])
				: ofc_sema_expr(scope, list->expr[i]));
			if (!expr)
			{
				ofc_sema_expr_list_delete(slist);
				return NULL;
			}
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

ofc_sema_expr_list_t* ofc_sema_expr_list(
	ofc_sema_scope_t*            scope,
	const ofc_parse_expr_list_t* list)
{
	return ofc_sema_expr__list(
		scope, list, false, false);
}

ofc_sema_expr_list_t* ofc_sema_expr_list_clist(
	ofc_sema_scope_t*            scope,
	const ofc_parse_expr_list_t* clist)
{
	return ofc_sema_expr__list(
		scope, clist, true, false);
}

ofc_sema_expr_list_t* ofc_sema_expr_list_io(
	ofc_sema_scope_t*            scope,
	const ofc_parse_expr_list_t* iolist)
{
	return ofc_sema_expr__list(
		scope, iolist, false, true);
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

ofc_sema_expr_list_t* ofc_sema_expr_list_copy_replace(
	const ofc_sema_expr_list_t* list,
	const ofc_sema_decl_t* replace,
	const ofc_sema_expr_t* with)
{
	if (!list) return NULL;

	ofc_sema_expr_list_t* copy
		= (ofc_sema_expr_list_t*)malloc(
			sizeof(ofc_sema_expr_list_t)
			+ (sizeof(ofc_sema_expr_t) * list->count));
	if (!copy) return NULL;

	copy->count = list->count;

	unsigned i;
	for (i = 0; i < copy->count; i++)
	{
		copy->expr[i] = ofc_sema_expr_copy_replace(
			list->expr[i], replace, with);
	}

	return copy;
}

ofc_sema_expr_list_t* ofc_sema_expr_list_copy(
	const ofc_sema_expr_list_t* list)
{
	return ofc_sema_expr_list_copy_replace(
		list, NULL, NULL);
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

bool ofc_sema_expr_list_elem_count(
	const ofc_sema_expr_list_t* list, unsigned* count)
{
	if (!list) return false;

	unsigned len = 0;
	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		ofc_sema_expr_t* expr
			= list->expr[i];

		unsigned elem_count;
		if (!ofc_sema_expr_elem_count(
			expr, &elem_count))
			return false;

		const ofc_sema_array_t* array
			= ofc_sema_expr_array(expr);
		if (array)
		{
			unsigned acount = 1;
			if (!ofc_sema_array_total(
				array, &acount))
				return false;
			elem_count *= acount;
		}

		if (expr->repeat > 1)
			elem_count *= expr->repeat;

		len += elem_count;
	}

	if (count) *count = len;
	return true;
}

ofc_sema_expr_t* ofc_sema_expr_list_elem_get(
	const ofc_sema_expr_list_t* list, unsigned offset)
{
	if (!list)
		return NULL;

	unsigned e = offset;
	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		ofc_sema_expr_t* expr
			= list->expr[i];

		unsigned elem_count;
		if (!ofc_sema_expr_elem_count(
			expr, &elem_count))
			return NULL;

		unsigned elem_total = elem_count;
		if (expr->repeat > 1)
			elem_total *= expr->repeat;

		if (e < elem_total)
		{
			if (elem_count == 1)
				return ofc_sema_expr_copy(expr);

			return ofc_sema_expr_elem_get(
				expr, (e % elem_count));
		}
		else
		{
			e -= elem_total;
		}
	}

	return NULL;
}

bool ofc_sema_expr_list_compare(
	const ofc_sema_expr_list_t* a,
	const ofc_sema_expr_list_t* b)
{
	if (!a || !b)
		return false;

	if (a == b)
		return true;

	if (a->count != b->count)
		return false;

	unsigned i;
	for (i = 0; i < a->count; i++)
	{
		if (!ofc_sema_expr_compare(
			a->expr[i], b->expr[i]))
			return false;
	}

	return true;
}

static const char* ofc_sema_expr__operator[] =
{
	NULL, /* CONSTANT */
	NULL, /* LHS */
	NULL, /* CAST */
	NULL, /* INTRINSIC */
	NULL, /* FUNCTION */
	NULL, /* ALT_RETURN */
	NULL, /* IMPLICIT_DO */

	"**",
	"*",
	"//",
	"/",
	"+",
	"-",
	"-",
	".EQ.",
	".NE.",
	".LT.",
	".LE.",
	".GT.",
	".GE.",
	".NOT.",
	".AND.",
	".OR.",
	".EQV.",
	".NEQV.",
};

static bool ofc_sema_expr__root_is_constant(
	const ofc_sema_expr_t* expr)
{
	if (!expr) return false;

	switch (expr->type)
	{
		case OFC_SEMA_EXPR_CONSTANT:
			return true;

		case OFC_SEMA_EXPR_CAST:
			return ofc_sema_expr__root_is_constant(
				expr->cast.expr);

		default:
			break;
	}

	return false;
}

bool ofc_sema_expr_print(
	ofc_colstr_t* cs,
	const ofc_sema_expr_t* expr)
{
	if (!cs || !expr) return false;

	if (expr->brackets)
	{
		if (!ofc_colstr_atomic_writef(cs, "("))
			return false;
	}

	switch (expr->type)
	{
		case OFC_SEMA_EXPR_CONSTANT:
			if (!ofc_sema_typeval_print(cs, expr->constant))
				return false;
			break;

		case OFC_SEMA_EXPR_LHS:
			if (!ofc_sema_lhs_print(cs, expr->lhs))
				return false;
			break;

		case OFC_SEMA_EXPR_CAST:
			if (!expr->constant
				|| !ofc_sema_expr__root_is_constant(expr->cast.expr)
				|| !ofc_sema_typeval_print(cs, expr->constant))
			{
				#ifdef OFC_PRINT_SEMA_IMPLICIT_CAST
				const char* cast
					= ofc_sema_type_str_cast_rep(
						expr->cast.type);
				if (cast)
				{
					if (!ofc_colstr_atomic_writef(cs, "%s", cast)
						|| !ofc_colstr_atomic_writef(cs, "(")
						|| !ofc_sema_expr_print(cs, expr->cast.expr)
						|| !ofc_colstr_atomic_writef(cs, ")"))
						return false;
				}
				else
				#endif
				{
					if (!ofc_sema_expr_print(cs, expr->cast.expr))
						return false;
				}
			}
			break;

		case OFC_SEMA_EXPR_INTRINSIC:
			if(!ofc_sema_intrinsic_print(cs, expr->intrinsic)
				|| !ofc_colstr_atomic_writef(cs, "(")
				|| !ofc_sema_expr_list_print(cs, expr->args)
				|| !ofc_colstr_atomic_writef(cs, ")"))
				return false;
			break;

		case OFC_SEMA_EXPR_FUNCTION:
			if (!ofc_sema_decl_print_name(cs, expr->function)
				|| !ofc_colstr_atomic_writef(cs, "(")
				|| (expr->args && !ofc_sema_expr_list_print(cs, expr->args))
				|| !ofc_colstr_atomic_writef(cs, ")"))
				return false;
			break;

		case OFC_SEMA_EXPR_ALT_RETURN:
			if (!ofc_colstr_atomic_writef(cs, "*")
				|| !ofc_sema_expr_print(cs, expr->alt_return.expr))
				return false;
			break;

		case OFC_SEMA_EXPR_IMPLICIT_DO:
			if (!ofc_colstr_atomic_writef(cs, "(")
				|| !ofc_sema_expr_print(cs, expr->implicit_do.expr)
				|| !ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_sema_decl_print_name(cs, expr->implicit_do.iter)
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_atomic_writef(cs, "=")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_sema_expr_print(cs, expr->implicit_do.init)
				|| !ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_sema_expr_print(cs, expr->implicit_do.last))
				return false;

			if (expr->implicit_do.step)
			{
				if (!ofc_colstr_atomic_writef(cs, ",")
					|| !ofc_colstr_atomic_writef(cs, " ")
					|| !ofc_sema_expr_print(cs, expr->implicit_do.step))
					return false;
			}

			if (!ofc_colstr_atomic_writef(cs, ")"))
				return false;
			break;

		default:
			if (expr->type >= OFC_SEMA_EXPR_COUNT)
				return false;

			if (expr->b)
			{
				/* Print binary expression */
				if (!ofc_sema_expr_print(cs, expr->a)
					|| !ofc_colstr_atomic_writef(cs, " ")
					|| !ofc_colstr_atomic_writef(cs, "%s",
						ofc_sema_expr__operator[expr->type])
					|| !ofc_colstr_atomic_writef(cs, " ")
					|| !ofc_sema_expr_print(cs, expr->b))
						return false;
			}
			else
			{
				/* Print unary expression */
				if (!ofc_colstr_atomic_writef(cs, "%s",
						ofc_sema_expr__operator[expr->type])
					|| !ofc_sema_expr_print(cs, expr->a))
						return false;
			}
			break;
	}

	if (expr->brackets)
	{
		if (!ofc_colstr_atomic_writef(cs, ")"))
			return false;
	}

	return true;
}

bool ofc_sema_expr_list_print(
	ofc_colstr_t* cs,
	const ofc_sema_expr_list_t* expr_list)
{
	if (!cs || !expr_list) return false;

	unsigned i;
	for (i = 0; i < expr_list->count; i++)
	{
		if (!ofc_sema_expr_print(cs, expr_list->expr[i]))
			return false;

		if ((expr_list->count > 1)
			&& (i < expr_list->count - 1))
		{
			if (!ofc_colstr_atomic_writef(cs, ", "))
				return false;
		}
	}
	return true;
}
