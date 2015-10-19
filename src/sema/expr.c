#include <ofc/sema.h>

ofc_sema_expr_t* ofc_sema_expr(
	const ofc_parse_expr_t* expr)
{
	if (!expr)
		return NULL;

	/* TODO - Implement. */
	return NULL;
}

void ofc_sema_expr_delete(
	ofc_sema_expr_t* expr)
{
	if (!expr)
		return;

	free(expr);
}

ofc_sema_typeval_t* ofc_sema_expr_resolve(
	const ofc_sema_expr_t* expr)
{
	if (!expr)
		return NULL;

	/* TODO - Implement. */
	return NULL;
}
