#include <ofc/sema.h>


ofc_sema_stmt_t* ofc_sema_stmt_assign(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_ASSIGN)
		|| ofc_str_ref_empty(stmt->assign.variable))
		return false;

	ofc_sema_stmt_t s;
	s.assign.dest = ofc_sema_scope_decl_find(
		scope, stmt->assign.variable, false);
	if (!s.assign.dest)
	{
		ofc_sema_spec_t* spec = ofc_sema_scope_spec_modify(
			scope, stmt->assign.variable);
		if (!spec) return false;

		if (spec->type != OFC_SEMA_TYPE_INTEGER)
		{
			if (!spec->type_implicit)
			{
				ofc_sema_scope_warning(scope, stmt->assign.variable,
					"IMPLICIT declaration of variable in ASSIGN destination"
					" as non-INTEGER makes no sense, declaring as INTEGER.");
			}
			spec->type_implicit = false;
			spec->type = OFC_SEMA_TYPE_INTEGER;
		}

		ofc_sema_decl_t* idecl = ofc_sema_decl_spec(
			scope, stmt->assign.variable, spec, NULL);
		if (!idecl) return false;

		s.assign.dest = idecl;
	}

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

bool ofc_sema_stmt_assign_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "ASSIGN")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "%d", stmt->assign.label)
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "TO")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_sema_decl_print(cs, false, stmt->assign.dest))
		return false;

	return true;
}
