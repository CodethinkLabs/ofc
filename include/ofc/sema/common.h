#ifndef __ofc_sema_common_h__
#define __ofc_sema_common_h__

typedef struct
{
	ofc_str_ref_t           name;

	unsigned                count;
	const ofc_sema_decl_t** decl;
	const ofc_sema_spec_t** spec;

	bool save;
} ofc_sema_common_t;

typedef struct
{
	ofc_hashmap_t* map;

	unsigned            count;
	ofc_sema_common_t** common;
} ofc_sema_common_map_t;

ofc_sema_common_t* ofc_sema_common_create(
	ofc_str_ref_t name);
void ofc_sema_common_delete(
	ofc_sema_common_t* common);

bool ofc_sema_common_add(
	ofc_sema_common_t* common,
	const ofc_sema_spec_t* spec);

bool ofc_sema_common_define(
	ofc_sema_common_t* common,
	unsigned offset,
	const ofc_sema_decl_t* decl);

bool ofc_sema_common_save(
	ofc_sema_common_t* common);


ofc_sema_common_map_t* ofc_sema_common_map_create(
	bool case_sensitive);
void ofc_sema_common_map_delete(
	ofc_sema_common_map_t* map);

ofc_sema_common_t* ofc_sema_common_map_find_modify(
	ofc_sema_common_map_t* map, ofc_str_ref_t name);

bool ofc_sema_common_map_add(
	ofc_sema_common_map_t* map,
	ofc_sema_common_t* common);

#endif
