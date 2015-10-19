#ifndef __ofc_sema_expr_h__
#define __ofc_sema_expr_h__

struct ofc_sema_expr_s
{

};

ofc_sema_expr_t* ofc_sema_expr(
	const ofc_parse_expr_t* expr);
void ofc_sema_expr_delete(
	ofc_sema_expr_t* expr);

ofc_sema_typeval_t* ofc_sema_expr_resolve(
	const ofc_sema_expr_t* expr);

#endif
