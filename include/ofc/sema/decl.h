#ifndef __ofc_sema_decl_h__
#define __ofc_sema_decl_h__

typedef struct
{
	ofc_sema_type_t* type;
	ofc_str_ref_t    name;
	void*            init;

    bool is_static;
	bool is_volatile;
	bool is_automatic;
	bool is_target;
} ofc_sema_decl_t;

ofc_sema_decl_t* ofc_sema_decl(
	const ofc_parse_stmt_t* stmt);
void ofc_sema_decl_delete(
	ofc_sema_decl_t* decl);

unsigned ofc_sema_decl_size(
	const ofc_sema_decl_t* decl);

#endif
