#ifndef __ofc_sema_spec_h__
#define __ofc_sema_spec_h__

struct ofc_sema_spec_s
{
	ofc_str_ref_t name;

	bool            type_implicit;
	ofc_sema_type_e type;

	unsigned kind;

	unsigned len;
	bool     len_var;

	ofc_sema_array_t* array;

	bool is_static;
	bool is_automatic;
	bool is_volatile;
	bool is_intrinsic;
	bool is_external;

	ofc_sema_common_t* common;
	unsigned           common_offset;
};

const ofc_sema_spec_t OFC_SEMA_SPEC_DEFAULT;

ofc_sema_spec_t* ofc_sema_spec_create(ofc_str_ref_t name);
ofc_sema_spec_t* ofc_sema_spec(
	ofc_sema_scope_t*       scope,
	const ofc_parse_type_t* ptype);
ofc_sema_spec_t* ofc_sema_spec_copy(
	const ofc_sema_spec_t* spec);
void ofc_sema_spec_delete(
	ofc_sema_spec_t* spec);

ofc_hashmap_t* ofc_sema_spec_map_create(
	bool case_sensitive);

#endif
