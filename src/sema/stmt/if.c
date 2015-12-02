#include <ofc/sema.h>

ofc_sema_stmt_t* ofc_sema_stmt_if__computed(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_IF_COMPUTED)
		|| !stmt->if_comp.cond)
		return NULL;

	if (!stmt->if_comp.label
		|| (stmt->if_comp.label->count < 3))
	{
		ofc_sema_scope_error(scope, stmt->src,
			"Not enough targets in arithmetic IF statement.");
		return NULL;
	}
	else if (stmt->if_comp.label->count > 3)
	{
		ofc_sema_scope_error(scope, stmt->src,
			"Too many targets in arithmetic IF statement.");
		return NULL;
	}

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
		ofc_sema_scope_error(scope, stmt->if_stmt.cond->src,
			"IF condition must be a scalar type.");

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

		if (!ofc_sema_expr_validate_uint(label))
		{
			ofc_sema_scope_error(scope, label->src,
				"Target label must be a positive INTEGER.");

			ofc_sema_expr_delete(label);
			ofc_sema_expr_list_delete(s.if_comp.label);
			ofc_sema_expr_delete(s.if_comp.cond);
			return NULL;
		}

		if (!ofc_sema_expr_list_add(
			s.if_comp.label, label))
		{
			ofc_sema_expr_delete(label);
			ofc_sema_expr_list_delete(s.if_comp.label);
			ofc_sema_expr_delete(s.if_comp.cond);
			return NULL;
		}
	}

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_list_delete(s.if_comp.label);
		ofc_sema_expr_delete(s.if_comp.cond);
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
		ofc_sema_scope_error(scope, stmt->if_stmt.cond->src,
			"IF condition type must be LOGICAL.");

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

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.if_stmt.cond);
		ofc_sema_stmt_delete(s.if_stmt.stmt);
		return NULL;
	}

	return as;
}

ofc_sema_stmt_t* ofc_sema_stmt_if__then(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt
		|| (stmt->type != OFC_PARSE_STMT_IF_THEN)
		|| !stmt->if_then.cond)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IF_THEN;

	s.if_then.cond = ofc_sema_expr(
		scope, stmt->if_then.cond);
	if (!s.if_then.cond)
		return NULL;

	const ofc_sema_type_t* type
		= ofc_sema_expr_type(s.if_then.cond);
	if (!ofc_sema_type_is_logical(type))
	{
		ofc_sema_scope_error(scope, stmt->if_stmt.cond->src,
			"IF condition type must be LOGICAL.");

		ofc_sema_expr_delete(s.if_then.cond);
		return NULL;
	}

	s.if_then.scope_then = NULL;
	if (stmt->if_then.block_then)
	{
		s.if_then.scope_then = ofc_sema_scope_if(
			scope, stmt->if_then.block_then);
		if (!s.if_then.scope_then)
		{
			ofc_sema_expr_delete(s.if_then.cond);
			return NULL;
		}
	}
	else
	{
		ofc_sema_scope_warning(scope, stmt->src,
			"Empty IF THEN block");
	}

	s.if_then.scope_else = NULL;
	if (stmt->if_then.block_else)
	{
		s.if_then.scope_else = ofc_sema_scope_if(
			scope, stmt->if_then.block_else);
		if (!s.if_then.scope_else)
		{
			ofc_sema_expr_delete(s.if_then.cond);
			ofc_sema_scope_delete(s.if_then.scope_then);
			return NULL;
		}
	}

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.if_then.cond);
		ofc_sema_scope_delete(s.if_then.scope_then);
		ofc_sema_scope_delete(s.if_then.scope_else);
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
		case OFC_PARSE_STMT_IF_THEN:
			return ofc_sema_stmt_if__then(scope, stmt);
		default:
			break;
	}

	return NULL;
}

bool ofc_sema_stmt_if_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt) return false;

	if (!ofc_colstr_atomic_writef(cs, "IF")
		|| !ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_sema_expr_print(cs, stmt->if_stmt.cond)
		|| !ofc_colstr_atomic_writef(cs, ")")
		|| !ofc_colstr_newline(cs, indent, NULL)
		|| !ofc_colstr_atomic_writef(cs, "  ")
		|| !ofc_sema_stmt_print(cs, indent,
			stmt->if_stmt.stmt))
		return false;

	return true;
}

bool ofc_sema_stmt_if_comp_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt) return false;

	if (!ofc_colstr_atomic_writef(cs, "IF")
		|| !ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_sema_expr_print(cs, stmt->if_comp.cond)
		|| !ofc_colstr_atomic_writef(cs, ") ")
		|| !ofc_sema_expr_list_print(cs, stmt->if_comp.label))
		return false;

	return true;
}

bool ofc_sema_stmt_if_then_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt) return false;

	if (!ofc_colstr_atomic_writef(cs, "IF")
		|| !ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_sema_expr_print(cs, stmt->if_then.cond)
		|| !ofc_colstr_atomic_writef(cs, ")")
		|| !ofc_colstr_atomic_writef(cs, "THEN"))
		return false;

	if (stmt->if_then.scope_then)
	{
		if (!ofc_sema_scope_print(cs, indent + 1,
			stmt->if_then.scope_then))
				return false;
	}

	if (stmt->if_then.scope_else)
	{
		if (!ofc_colstr_atomic_writef(cs, "ELSE")
			|| !ofc_sema_scope_print(cs, indent + 1,
				stmt->if_then.scope_else))
					return false;
	}

	if (!ofc_colstr_newline(cs, indent, NULL)
		|| !ofc_colstr_atomic_writef(cs, "END")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "IF"))
		return false;

	return true;
}
