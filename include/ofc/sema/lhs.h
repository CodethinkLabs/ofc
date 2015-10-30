#ifndef __ofc_sema_lhs_h__
#define __ofc_sema_lhs_h__

struct ofc_sema_lhs_s
{
	ofc_sema_decl_t*       decl;
	ofc_sema_array_t*      index;
	const ofc_sema_type_t* type;
};

ofc_sema_lhs_t* ofc_sema_lhs(
	ofc_sema_scope_t* scope,
	ofc_parse_lhs_t* lhs);
void ofc_sema_lhs_delete(
	ofc_sema_lhs_t* lhs);

const ofc_sema_type_t* ofc_sema_lhs_type(
	const ofc_sema_lhs_t* lhs);

ofc_sema_lhs_t* ofc_sema_lhs_index(
	ofc_sema_lhs_t* lhs,
	ofc_sema_array_t* index);
ofc_sema_lhs_t* ofc_sema_lhs_member(
	ofc_sema_lhs_t* lhs,
	ofc_str_ref_t member);

#endif
