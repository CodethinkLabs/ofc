#ifndef __ofc_sema_common_h__
#define __ofc_sema_common_h__

typedef struct
{
	ofc_str_ref_t         name;
	ofc_sema_decl_list_t* decl;
} ofc_sema_common_t;

ofc_sema_common_t* ofc_sema_common_create(
	ofc_str_ref_t name, bool case_sensitive);
void ofc_sema_common_delete(
	ofc_sema_common_t* common);

bool ofc_sema_common_add(
	ofc_sema_common_t* common,
	const ofc_sema_decl_t* decl);


ofc_hashmap_t* ofc_sema_common_map_create(
	bool case_sensitive);

#endif
