#include <ofc/sema.h>


ofc_sema_common_t* ofc_sema_common_create(
	ofc_str_ref_t name, bool case_sensitive)
{
	ofc_sema_common_t* common
		= (ofc_sema_common_t*)malloc(
			sizeof(ofc_sema_common_t));
	if (!common) return NULL;

	common->decl = ofc_sema_decl_list_create_ref(case_sensitive);
	if (!common->decl)
	{
		free(common);
		return NULL;
	}

	common->name = name;
	return common;
}

void ofc_sema_common_delete(
	ofc_sema_common_t* common)
{
	if (!common)
		return;

	ofc_sema_decl_list_delete(common->decl);
	free(common);
}

bool ofc_sema_common_add(
	ofc_sema_common_t* common,
	const ofc_sema_decl_t* decl)
{
	if (!common)
		return false;

	return ofc_sema_decl_list_add_ref(
		common->decl, decl);
}


static const ofc_str_ref_t* ofc_sema_common__key(
	const ofc_sema_common_t* common)
{
	return (common ? &common->name : NULL);
}


ofc_hashmap_t* ofc_sema_common_map_create(
	bool case_sensitive)
{
	return ofc_hashmap_create(
		(void*)(case_sensitive
			? ofc_str_ref_ptr_hash
			: ofc_str_ref_ptr_hash_ci),
		(void*)(case_sensitive
			? ofc_str_ref_ptr_equal
			: ofc_str_ref_ptr_equal_ci),
		(void*)ofc_sema_common__key,
		(void*)ofc_sema_common_delete);
}
