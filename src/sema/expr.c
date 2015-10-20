#include <ofc/sema.h>

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
		default:
			expr->a = NULL;
			expr->b = NULL;
			break;
	}

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
			/* TODO - Resolve unary operations. */
			return NULL;
		case OFC_PARSE_EXPR_BINARY:
			/* TODO - Resolve binary operations. */
			return NULL;
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
		default:
			ofc_sema_expr_delete(expr->b);
			ofc_sema_expr_delete(expr->a);
			break;
	}

	free(expr);
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

		/* TODO - Handle operators. */

		case OFC_SEMA_EXPR_DECL:
		default:
			break;
	}

	return NULL;
}
