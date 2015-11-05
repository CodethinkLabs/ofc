#include <ofc/sema.h>


bool ofc_sema_stmt_is_stmt_func(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_ASSIGNMENT)
		|| !stmt->assignment
		|| !stmt->assignment->name)
		return false;

	if (stmt->assignment->name->type
		!= OFC_PARSE_LHS_ARRAY)
		return false;

	if (!stmt->assignment->name->parent
		|| (stmt->assignment->name->parent->type
			!= OFC_PARSE_LHS_VARIABLE))
		return false;

	ofc_str_ref_t base_name;
	if (!ofc_parse_lhs_base_name(
		*(stmt->assignment->name), &base_name))
		return NULL;

	const ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find(
			scope, base_name);
	return !ofc_sema_decl_is_array(decl);
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
