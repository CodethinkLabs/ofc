#ifndef __ofc_sema_implicit_h__
#define __ofc_sema_implicit_h__

ofc_sema_implicit_t* ofc_sema_implicit_create(void);
ofc_sema_implicit_t* ofc_sema_implicit_copy(
	const ofc_sema_implicit_t* implicit);
void ofc_sema_implicit_delete(
	ofc_sema_implicit_t* implicit);

bool ofc_sema_implicit_none(
	ofc_sema_implicit_t* implicit);
bool ofc_sema_implicit_set(
	ofc_sema_implicit_t* implicit,
	ofc_sema_spec_t spec, char c);
ofc_sema_spec_t* ofc_sema_implicit_apply(
	const ofc_sema_implicit_t* implicit,
	ofc_str_ref_t              name,
	const ofc_sema_spec_t*     spec);

bool ofc_sema_implicit(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);

#endif
