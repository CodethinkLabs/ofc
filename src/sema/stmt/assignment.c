#include <ofc/sema.h>


bool ofc_sema_stmt_assignment_decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_ASSIGNMENT)
		|| !stmt->assignment
		|| !stmt->assignment->name)
		return false;

	ofc_str_ref_t base_name;
	if (!ofc_parse_lhs_base_name(
		*(stmt->assignment->name), &base_name))
		return false;

	const ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find(
			scope, base_name);
	if (decl) return true;

	/* Can only implicitly declare variables. */
	if (stmt->assignment->name->type
		!= OFC_PARSE_LHS_VARIABLE)
	{
		ofc_str_ref_t n = base_name;
		ofc_sema_scope_error(scope, stmt->assignment->name->src,
			"Assignment to undeclared symbol '%.*s'.",
			n.size, n.base);
		return false;
	}

	ofc_sema_decl_t* idecl
		= ofc_sema_decl_implicit_name(
			scope, base_name);
	if (!idecl)
	{
		ofc_str_ref_t n = base_name;
		ofc_sema_scope_error(scope, stmt->assignment->name->src,
			"No declaration for '%.*s' and no valid IMPLICIT rule.",
			n.size, n.base);
		return false;
	}

	if (!ofc_sema_decl_list_add(
		scope->decl, idecl))
	{
		ofc_sema_decl_delete(idecl);
		return false;
	}

	return true;
}

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

	ofc_sema_stmt_t s;
	s.assignment.dest = ofc_sema_lhs(
		scope, stmt->assignment->name);
	if (!s.assignment.dest) return NULL;

	s.assignment.expr = ofc_sema_expr(
		scope, stmt->assignment->init);
	if (!s.assignment.expr) return NULL;

	const ofc_sema_type_t* dtype
		= ofc_sema_lhs_type(s.assignment.dest);
	if (!ofc_sema_type_compare(dtype,
		ofc_sema_expr_type(s.assignment.expr)))
	{
		ofc_sema_expr_t* cast
			= ofc_sema_expr_cast(
				scope, s.assignment.expr, dtype);
		if (!cast)
		{
			const ofc_sema_type_t* expr_type =
				ofc_sema_expr_type(s.assignment.expr);
			ofc_sema_scope_error(scope, stmt->src,
				"Expression type %s doesn't match lhs type %s",
				ofc_sema_type_str_rep(expr_type->type),
				ofc_sema_type_str_rep(dtype->type));
			ofc_sema_expr_delete(s.assignment.expr);
			return NULL;
		}
		s.assignment.expr = cast;
	}
	s.type = OFC_SEMA_STMT_ASSIGNMENT;
	s.src = stmt->src;

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(
			s.assignment.expr);
		return NULL;
	}

	return as;
}
