#include <ofc/sema.h>


ofc_sema_stmt_t* ofc_sema_stmt_return(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_RETURN))
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_RETURN;

	/* TODO - Support RETURN argument. */
	if (stmt->stop_pause_return.value)
	{
		ofc_sema_scope_error(scope, stmt->src,
			"Alternate RETURN or RETURN with value not yet supported");
		return NULL;
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as) return NULL;

    return as;
}
