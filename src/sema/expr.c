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

#include <ofc/sema.h>


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
	NULL, /* FUNCTION */
	NULL, /* ALT_RETURN */

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

	expr->src = OFC_STR_REF_EMPTY;

	expr->constant = NULL;

	expr->brackets = false;

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

	ofc_sema_expr_t* cast
		= ofc_sema_expr__create(
			OFC_SEMA_EXPR_CAST);
	if (!cast) return NULL;

	if (ofc_sema_expr_is_constant(expr))
	{
		cast->constant = ofc_sema_typeval_cast(
			scope, expr->constant, type);
		if (!cast->constant)
		{
			ofc_sema_expr_delete(cast);
			return NULL;
		}
	}
	else if (!ofc_sema_type_cast_is_lossless(
		ofc_sema_expr_type(expr), type))
	{
		ofc_sema_scope_warning(scope, expr->src,
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

	const ofc_sema_type_t* at
		= ofc_sema_expr_type(as);
	if (!at)
	{
		ofc_sema_expr_delete(as);
		return NULL;
	}

	if (!ofc_sema_expr_type_allowed(type, at))
	{
		ofc_sema_scope_error(scope, a->src,
			"Can't use type %s in operator '%s'",
			ofc_sema_type_str_rep(at),
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
		ofc_sema_scope_error(scope, a->src,
			"Can't use type %s in operator '%s'",
			ofc_sema_type_str_rep(bt),
			ofc_parse_operator_str_rep(op));
		ofc_sema_expr_delete(bs);
		ofc_sema_expr_delete(as);
		return NULL;
	}

	if (!ofc_sema_type_compare(at, bt))
	{
		const ofc_sema_type_t* ptype
			= ofc_sema_type_promote(at, bt);
		if (!ptype)
		{
			ofc_sema_scope_error(scope, a->src,
				"Incompatible types (%s, %s) in operator %s",
				ofc_sema_type_str_rep(at),
				ofc_sema_type_str_rep(bt),
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
			scope, as->constant, bs->constant);
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

	const ofc_sema_type_t* at
		= ofc_sema_expr_type(as);
	if (!ofc_sema_expr_type_allowed(type, at))
	{
		ofc_sema_scope_error(scope, a->src,
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
			scope, as->constant, NULL);
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
		ofc_sema_scope_error(scope, name->src,
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
			scope, name->src, intrinsic, args);
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
	expr->src       = ofc_str_ref_bridge(name->parent->src, name->src);

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
	{
		ofc_sema_scope_error(scope, name->src,
			"Invalid invocation of function.");
		return NULL;
	}

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
			ofc_sema_scope_error(scope, name->src,
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
	expr->src      = ofc_str_ref_bridge(name->parent->src, name->src);

	return expr;
}

static ofc_sema_expr_t* ofc_sema_expr__lhs(
	ofc_sema_scope_t* scope,
	const ofc_parse_lhs_t* name)
{
	if (!name)
		return NULL;

	ofc_sema_lhs_t* lhs = ofc_sema_lhs_in_expr(
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
	if (expr) return expr;

	ofc_str_ref_t base_name;
	if (!ofc_parse_lhs_base_name(
		*name, &base_name))
		return NULL;

	const ofc_sema_intrinsic_t* intrinsic
		= ofc_sema_intrinsic(scope, base_name);
	const ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find(
			scope, base_name, false);

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
			expr = ofc_sema_expr__lhs(scope, name);
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
		}
		else
		{
			expr = ofc_sema_expr__lhs(scope, name);
		}
	}

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
			return ofc_sema_expr_brackets(
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

ofc_sema_expr_t* ofc_sema_expr_brackets(
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
			if (!ofc_sema_type_compare(
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

		default:
			break;
	}

	if ((a->a != b->a)
		&& !ofc_sema_expr_compare(a->a, b->a))
		return false;

	return ((a->b == b->b)
		|| ofc_sema_expr_compare(a->b, b->b));
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

bool ofc_sema_expr_list_add_list(
	ofc_sema_expr_list_t* alist,
	ofc_sema_expr_list_t* blist)
{
	if (!alist || !blist)
		return false;

	unsigned i;
	for (i = 0; i < blist->count; i++)
	{
		if (!ofc_sema_expr_list_add(alist, blist->expr[i]))
			return false;
	}

	return true;
}

unsigned ofc_sema_expr_list_count(
	const ofc_sema_expr_list_t* list)
{
	return (list ? list->count : 0);
}

ofc_sema_expr_list_t* ofc_sema_expr_list_implicit_do(
	ofc_sema_scope_t* scope, ofc_parse_implicit_do_t* id)
{
	if (!scope || !id)
		return NULL;

	ofc_sema_scope_t* idscope
		= ofc_sema_scope_implicit_do(scope);
	if (!idscope)
		return NULL;

	ofc_sema_parameter_t* param
		= ofc_sema_parameter_assign(idscope, id->init);
	if (!param)
	{
		ofc_sema_scope_delete(idscope);
		return NULL;
	}

	if (!ofc_sema_scope_parameter_add(idscope, param))
	{
		ofc_sema_parameter_delete(param);
		ofc_sema_scope_delete(idscope);
		return NULL;
	}

	const ofc_sema_type_t* dtype
		= ofc_sema_parameter_type(param);
	if (!ofc_sema_type_is_scalar(dtype))
	{
		ofc_sema_scope_error(scope, id->init->name->src,
			"Implicit do loop iterator must be a scalar type.");
		ofc_sema_scope_delete(idscope);
		return NULL;
	}

	if (!ofc_sema_type_is_integer(dtype))
	{
		ofc_sema_scope_warning(scope,
			id->init->name->src,
				"Using REAL in implicit do loop iterator..");
	}

	ofc_sema_expr_t* limit
		= ofc_sema_expr(scope, id->limit);
	if (!limit)
	{
		ofc_sema_scope_delete(idscope);
		return NULL;
	}

	if (!ofc_sema_type_compare(dtype,
		ofc_sema_expr_type(limit)))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				scope, limit, dtype);
		if (!cast)
		{
			const ofc_sema_type_t* expr_type =
				ofc_sema_expr_type(limit);
			ofc_sema_scope_error(scope,
				id->limit->src,
					"Expression type %s doesn't match iterator type %s",
				ofc_sema_type_str_rep(expr_type),
				ofc_sema_type_str_rep(dtype));
			ofc_sema_expr_delete(limit);
			ofc_sema_scope_delete(idscope);
			return NULL;
		}
		limit = cast;
	}

	ofc_sema_expr_t* step = NULL;

	if (id->step)
	{
		step = ofc_sema_expr(
			scope, id->step);
		if (!step)
		{
			ofc_sema_expr_delete(limit);
			ofc_sema_scope_delete(idscope);
			return NULL;
		}

		if (!ofc_sema_type_compare(dtype,
			ofc_sema_expr_type(step)))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(
					scope, step, dtype);
			if (!cast)
			{
				const ofc_sema_type_t* expr_type =
					ofc_sema_expr_type(step);
				ofc_sema_scope_error(scope,
					id->step->src,
						"Expression type %s doesn't match iterator type %s",
					ofc_sema_type_str_rep(expr_type),
					ofc_sema_type_str_rep(dtype));
				ofc_sema_expr_delete(step);
				ofc_sema_expr_delete(limit);
				ofc_sema_scope_delete(idscope);
				return NULL;
			}
			step = cast;
		}
	}
	else
	{
		step = ofc_sema_expr__create(
			OFC_SEMA_EXPR_CONSTANT);
		if (!step)
		{
			ofc_sema_expr_delete(limit);
			ofc_sema_scope_delete(idscope);
			return NULL;
		}

		step->constant
			= ofc_sema_typeval_unsigned(1, OFC_STR_REF_EMPTY);
		if(!step->constant)
		{
			ofc_sema_expr_delete(step);
			ofc_sema_expr_delete(limit);
			ofc_sema_scope_delete(idscope);
			return NULL;
		}

		if (!ofc_sema_type_compare(dtype,
			ofc_sema_expr_type(step)))
		{
			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(
					scope, step, dtype);
			if (!cast)
			{
				ofc_sema_expr_delete(step);
				ofc_sema_expr_delete(limit);
				ofc_sema_scope_delete(idscope);
				return NULL;
			}

			step = cast;
		}
	}

	ofc_sema_expr_list_t* list
		= ofc_sema_expr_list_create();
	if (!list)
	{
		ofc_sema_expr_delete(step);
		ofc_sema_expr_delete(limit);
		ofc_sema_scope_delete(idscope);
		return NULL;
	}

	ofc_sema_typeval_t* value
		= ofc_sema_typeval_le(idscope,
			param->typeval, limit->constant);
	if (!value)
	{
		ofc_sema_expr_delete(step);
		ofc_sema_expr_delete(limit);
		ofc_sema_scope_delete(idscope);
		ofc_sema_expr_list_delete(list);
		return NULL;
	}

	while(value->logical)
	{
		if (id->dlist->type == OFC_PARSE_LHS_IMPLICIT_DO)
		{
			ofc_sema_expr_list_t* implicit_do
				= ofc_sema_expr_list_implicit_do(
					scope, id->dlist->implicit_do);
			if (!implicit_do)
			{
				ofc_sema_expr_delete(step);
				ofc_sema_expr_delete(limit);
				ofc_sema_expr_list_delete(list);
				ofc_sema_scope_delete(idscope);
				return NULL;
			}

			if (!ofc_sema_expr_list_add_list(list, implicit_do))
			{
				ofc_sema_expr_delete(step);
				ofc_sema_expr_delete(limit);
				ofc_sema_expr_list_delete(list);
				ofc_sema_expr_list_delete(implicit_do);
				ofc_sema_scope_delete(idscope);
				return NULL;
			}
		}
		else
		{
			ofc_sema_expr_t* expr
				= ofc_sema_expr__lhs(
					idscope, id->dlist);
			if (!ofc_sema_expr_list_add(list, expr))
			{
				ofc_sema_expr_delete(step);
				ofc_sema_expr_delete(limit);
				ofc_sema_expr_list_delete(list);
				ofc_sema_scope_delete(idscope);
				return NULL;
			}
		}

		param->typeval
			= ofc_sema_typeval_add(idscope,
				param->typeval, step->constant);

		value = ofc_sema_typeval_le(
			idscope, param->typeval, limit->constant);
		if (!value)
		{
			ofc_sema_expr_delete(step);
			ofc_sema_expr_delete(limit);
			ofc_sema_scope_delete(idscope);
			ofc_sema_expr_list_delete(list);
			return NULL;
		}
	}

	return list;
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
			if (expr->brackets)
			{
				if (!ofc_colstr_atomic_writef(cs, ")"))
					return false;
			}
			return true;

		case OFC_SEMA_EXPR_LHS:
			if (!ofc_sema_lhs_print(cs, expr->lhs))
				return false;
			if (expr->brackets)
			{
				if (!ofc_colstr_atomic_writef(cs, ")"))
					return false;
			}
			return true;

		case OFC_SEMA_EXPR_CAST:
			{
				/* TODO - Should we actually print these? */
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
				{
					if (!ofc_sema_expr_print(cs, expr->cast.expr))
						return false;
				}
			}
			return true;

		case OFC_SEMA_EXPR_INTRINSIC:
			if(!ofc_sema_intrinsic_print(cs, expr->intrinsic)
				|| !ofc_colstr_atomic_writef(cs, "(")
				|| !ofc_sema_expr_list_print(cs, expr->args)
				|| !ofc_colstr_atomic_writef(cs, ")"))
				return false;
			return true;

		case OFC_SEMA_EXPR_FUNCTION:
			if (!ofc_sema_decl_print(cs, false, expr->function)
				|| !ofc_colstr_atomic_writef(cs, "(")
				|| !ofc_sema_expr_list_print(cs, expr->args)
				|| !ofc_colstr_atomic_writef(cs, ")"))
				return false;
			return true;

		case OFC_SEMA_EXPR_ALT_RETURN:
			return (ofc_colstr_atomic_writef(cs, "*")
				&& ofc_sema_expr_print(cs, expr->alt_return.expr));

		default:
			break;
	}

	if (expr->type >= OFC_SEMA_EXPR_COUNT)
		return false;

	if (expr->b)
	{
		/* Print binary expression */
		if (!ofc_sema_expr_print(cs, expr->a)
			|| !ofc_colstr_atomic_writef(cs, " %s ",
					ofc_sema_expr__operator[expr->type])
			|| !ofc_sema_expr_print(cs, expr->b))
				return false;
	}
	else
	{
		/* Print unary expression */
		if (!ofc_colstr_atomic_writef(cs, " %s ",
				ofc_sema_expr__operator[expr->type])
			|| !ofc_sema_expr_print(cs, expr->a))
				return false;
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
