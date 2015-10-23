#include <ofc/sema.h>


ofc_sema_stmt_t* ofc_sema_stmt_assignment(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_ASSIGNMENT)
		|| !stmt->assignment
		|| !stmt->assignment->name
		|| !stmt->assignment->init)
		return NULL;

	/* TODO - Support more advanced LHS. */
	if (stmt->assignment->name->type
		!= OFC_PARSE_LHS_VARIABLE)
		return NULL;

	ofc_sema_stmt_t s;
	s.assignment.dest = ofc_sema_decl_list_find(
		scope->decl, stmt->assignment->name->variable);

	ofc_sema_decl_t* idecl = NULL;
	if (!s.assignment.dest)
	{
		idecl = ofc_sema_decl_implicit_lhs(
			scope, stmt->assignment->name);
		if (!idecl)
		{
			ofc_str_ref_t n = stmt->assignment->name->variable;
			ofc_sema_scope_error(scope, stmt->src,
				"No declaration for '%.*s' and no valid IMPLICIT rule.",
				n.size, n.base);
			return NULL;
		}
		s.assignment.dest = idecl;
	}

	s.assignment.expr = ofc_sema_expr(
		scope, stmt->assignment->init);
	if (!s.assignment.expr)
	{
		ofc_sema_decl_delete(idecl);
		return NULL;
	}

	const ofc_sema_type_t* dtype
		= ofc_sema_decl_type(s.assignment.dest);
	if (!ofc_sema_type_compare(dtype,
		ofc_sema_expr_type(s.assignment.expr)))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				s.assignment.expr, dtype);
		if (!cast)
		{
			const ofc_sema_type_t* expr_type =
				ofc_sema_expr_type(s.assignment.expr);
			ofc_sema_scope_error(scope, stmt->src,
				"Expression type %s doesn't match lhs type %s",
				ofc_sema_type_str_rep(expr_type->type),
				ofc_sema_type_str_rep(dtype->type));
			ofc_sema_expr_delete(s.assignment.expr);
			ofc_sema_decl_delete(idecl);
			return NULL;
		}
		s.assignment.expr = cast;
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.assignment.expr);
		ofc_sema_decl_delete(idecl);
		return NULL;
	}

	if (idecl)
	{
		if (!ofc_sema_decl_list_add(
			scope->decl, idecl))
		{
			ofc_sema_expr_delete(s.assignment.expr);
			ofc_sema_decl_delete(idecl);
			return NULL;
		}
	}

	return as;
}
