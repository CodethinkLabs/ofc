#include <ofc/sema.h>

ofc_sema_stmt_t* ofc_sema_stmt_if__computed(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_IF_COMPUTED)
		|| !stmt->if_comp.cond
		|| !stmt->if_comp.label
		|| (stmt->if_comp.label->count != 3))
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IF_COMPUTED;
	s.if_comp.cond = ofc_sema_expr(
		scope, stmt->if_comp.cond);

	if (!s.if_comp.cond)
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.if_comp.cond);

	if (!ofc_sema_type_is_scalar(type))
	{
		ofc_sema_expr_delete(s.if_comp.cond);
		return NULL;
	}

	s.if_comp.label = ofc_sema_expr_list_create();
	if (!s.if_comp.label)
	{
		ofc_sema_expr_delete(s.if_comp.cond);
		return NULL;
	}

	unsigned i;
	for (i = 0; i < stmt->if_comp.label->count; i++)
	{
		ofc_sema_expr_t* label = ofc_sema_expr(
			scope, stmt->if_comp.label->expr[i]);
		if (!ofc_sema_expr_list_add(
			s.if_comp.label, label))
		{
			ofc_sema_expr_delete(label);
			ofc_sema_expr_delete(s.if_comp.cond);
			return NULL;
		}
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.if_comp.cond);
		ofc_sema_expr_list_delete(s.if_comp.label);
		return NULL;
	}

	return as;
}

ofc_sema_stmt_t* ofc_sema_stmt_if__statement(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_IF_STATEMENT)
		|| !stmt->if_stmt.cond
		|| !stmt->if_stmt.stmt)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IF_STATEMENT;

	s.if_stmt.cond = ofc_sema_expr(
		scope, stmt->if_stmt.cond);
	if (!s.if_stmt.cond)
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.if_stmt.cond);
	if (!ofc_sema_type_is_logical(type))
	{
		ofc_sema_expr_delete(s.if_stmt.cond);
		return NULL;
	}

	s.if_stmt.stmt = ofc_sema_stmt(
		scope, stmt->if_stmt.stmt);
	if (!s.if_stmt.stmt)
	{
		ofc_sema_expr_delete(s.if_stmt.cond);
		return NULL;
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.if_stmt.cond);
		ofc_sema_stmt_delete(s.if_stmt.stmt);
		return NULL;
	}

	return as;
}

ofc_sema_stmt_t* ofc_sema_stmt_if(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return NULL;

	switch(stmt->type)
	{
		case OFC_PARSE_STMT_IF_COMPUTED:
			return ofc_sema_stmt_if__computed(scope, stmt);
		case OFC_PARSE_STMT_IF_STATEMENT:
			return ofc_sema_stmt_if__statement(scope, stmt);
		default:
			break;
	}

	return NULL;
}
