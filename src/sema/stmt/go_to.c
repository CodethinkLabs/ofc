#include <ofc/sema.h>


ofc_sema_stmt_t* ofc_sema_stmt_go_to(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_GO_TO))
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_GO_TO;
	s.go_to.label = ofc_sema_expr_label(
		scope, &stmt->go_to.label);
	if (!s.go_to.label) return NULL;

	if (!ofc_sema_expr_validate_uint(s.go_to.label))
	{
		ofc_sema_scope_error(scope, s.go_to.label->src,
			"GO TO target must be a positive integer");
		ofc_sema_expr_delete(s.go_to.label);
		return NULL;
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.go_to.label);
		return NULL;
	}

	return as;
}

ofc_sema_stmt_t* ofc_sema_stmt_go_to_assigned(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt || (stmt->type != OFC_PARSE_STMT_GO_TO_ASSIGNED))
		return NULL;

	/* TODO - Implement assigned GOTO */
	return NULL;
}

ofc_sema_stmt_t* ofc_sema_stmt_go_to_computed(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt || (stmt->type != OFC_PARSE_STMT_GO_TO_COMPUTED))
		return NULL;

	/* TODO - Implement computed GOTO */
	return NULL;
}
