#include <ofc/sema.h>


ofc_sema_decl_t* ofc_sema_decl_implicit_name(
	const ofc_sema_implicit_t* implicit,
	ofc_str_ref_t* name)
{
	return NULL;
}

ofc_sema_decl_t* ofc_sema_decl_implicit_lhs(
	const ofc_sema_implicit_t* implicit,
	const ofc_parse_lhs_t* lhs)
{
	return NULL;
}

ofc_sema_decl_t* ofc_sema_decl(
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt || (stmt->type != OFC_PARSE_STMT_DECL))
		return NULL;

	if (!stmt->decl.type
		|| !stmt->decl.decl)
		return NULL;

	ofc_sema_decl_t* decl
		= (ofc_sema_decl_t*)malloc(
			sizeof(ofc_sema_decl_t));
	if (!decl) return NULL;

	decl->type = NULL;
	decl->name = OFC_STR_REF_EMPTY;
	decl->init = NULL;

	return decl;
}

void ofc_sema_decl_delete(
	ofc_sema_decl_t* decl)
{
	if (!decl)
		return;

	free(decl->init);
	free(decl);
}


unsigned ofc_sema_decl_size(
	const ofc_sema_decl_t* decl)
{
	if (!decl)
		return 0;
	return ofc_sema_type_size(
		decl->type);
}
