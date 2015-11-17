#ifndef __ofc_sema_intrinsic_h__
#define __ofc_sema_intrinsic_h__

typedef struct ofc_sema_intrinsic_s ofc_sema_intrinsic_t;

const ofc_sema_intrinsic_t* ofc_sema_intrinsic(
	const ofc_sema_scope_t* scope,
	ofc_str_ref_t name);

/* This takes ownership of args and deletes on failure. */
ofc_sema_expr_list_t* ofc_sema_intrinsic_cast(
	const ofc_sema_scope_t* scope,
	ofc_str_ref_t src,
	const ofc_sema_intrinsic_t* intrinsic,
	ofc_sema_expr_list_t* args);

const ofc_sema_type_t* ofc_sema_intrinsic_type(
	const ofc_sema_intrinsic_t* intrinsic,
	ofc_sema_expr_list_t* args);

#endif
