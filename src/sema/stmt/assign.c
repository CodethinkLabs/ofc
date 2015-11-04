#include <ofc/sema.h>


bool ofc_sema_stmt_assign_decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_ASSIGN)
		|| ofc_str_ref_empty(stmt->assign.variable))
		return false;

	const ofc_sema_decl_t* decl
		= ofc_sema_scope_decl_find(
			scope, stmt->assign.variable);
	if (decl) return true;

	const ofc_sema_type_t* type = ofc_sema_implicit_get(
		scope->implicit, stmt->assign.variable.base[0]);
	if (!type) return false;

	if (!ofc_sema_type_is_integer(type))
	{
		ofc_sema_scope_warning(scope, stmt->assign.variable,
			"IMPLICIT declaration of variable in ASSIGN destination"
			" as non-INTEGER makes no sense, declaring as INTEGER.");
		type = ofc_sema_type_integer_default();
	}

	ofc_sema_decl_t* idecl = ofc_sema_decl_create(
		type, stmt->assign.variable);
	if (!idecl) return false;

	if (!ofc_sema_decl_list_add(
		scope->decl, idecl))
	{
		ofc_sema_decl_delete(idecl);
		return false;
	}

	return true;
}

ofc_sema_stmt_t* ofc_sema_stmt_assign(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_ASSIGN))
		return NULL;

	ofc_sema_stmt_t s;
	s.assign.dest = ofc_sema_scope_decl_find(
		scope, stmt->assign.variable);
	if (!s.assign.dest) return NULL;

	s.assign.label = stmt->assign.label;

	const ofc_sema_type_t* dtype
		= ofc_sema_decl_type(s.assign.dest);
	if (!ofc_sema_type_is_integer(dtype))
	{
		ofc_sema_scope_error(scope, stmt->src,
			"ASSIGN destination must be of type INTEGER.");
		return NULL;
	}
	s.type = OFC_SEMA_STMT_ASSIGN;
	s.src = stmt->src;

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as) return NULL;

	return as;
}
