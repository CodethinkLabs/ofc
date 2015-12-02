#include <ofc/sema.h>


ofc_sema_stmt_t* ofc_sema_stmt_entry(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_ENTRY))
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_ENTRY;
	s.entry.name = stmt->call_entry.name;
	s.entry.args = NULL;

	if (stmt->call_entry.args)
	{
		s.entry.args = ofc_sema_arg_list(
			scope, stmt->call_entry.args);
		if (!s.entry.args) return NULL;
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_arg_list_delete(s.entry.args);
		return NULL;
	}

    return as;
}
