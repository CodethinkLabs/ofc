#ifndef __ofc_sema_decl_h__
#define __ofc_sema_decl_h__

#include <ofc/hashmap.h>

typedef struct ofc_sema_decl_s ofc_sema_decl_t;

struct ofc_sema_decl_s
{
	const ofc_sema_type_t* type;
	ofc_str_ref_t          name;

	union
	{
		ofc_sema_typeval_t*  init;
		ofc_sema_typeval_t** init_array;
	};

    bool is_static;
	bool is_volatile;
	bool is_automatic;
	bool is_target;

	ofc_sema_equiv_t* equiv;

	/* Set this once the decl has been used. */
	bool lock;
};

typedef struct
{
	bool case_sensitive;
	bool is_ref;

	unsigned count;

	union
	{
		ofc_sema_decl_t**       decl;
		const ofc_sema_decl_t** decl_ref;
	};

	ofc_hashmap_t*    map;
} ofc_sema_decl_list_t;

ofc_sema_decl_t* ofc_sema_decl_create(
	const ofc_sema_type_t* type,
	ofc_str_ref_t name);

ofc_sema_decl_t* ofc_sema_decl_implicit_name(
	const ofc_sema_scope_t* scope,
	ofc_str_ref_t name);
ofc_sema_decl_t* ofc_sema_decl_implicit_lhs(
	ofc_sema_scope_t*      scope,
	const ofc_parse_lhs_t* lhs);

bool ofc_sema_decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
void ofc_sema_decl_delete(
	ofc_sema_decl_t* decl);

bool ofc_sema_decl_init(
	const ofc_sema_scope_t* scope,
	ofc_sema_decl_t* decl,
	const ofc_sema_expr_t* init);
bool ofc_sema_decl_init_array(
	const ofc_sema_scope_t* scope,
	ofc_sema_decl_t* decl,
	const ofc_sema_array_t* array,
	unsigned count,
	const ofc_sema_expr_t** init);

unsigned ofc_sema_decl_size(
	const ofc_sema_decl_t* decl);
unsigned ofc_sema_decl_elem_count(
	const ofc_sema_decl_t* decl);

bool ofc_sema_decl_is_array(
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_is_composite(
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_is_locked(
	const ofc_sema_decl_t* decl);

const ofc_sema_type_t* ofc_sema_decl_type(
	const ofc_sema_decl_t* decl);
const ofc_sema_type_t* ofc_sema_decl_base_type(
	const ofc_sema_decl_t* decl);

bool ofc_sema_decl_equiv(
	ofc_sema_decl_t* a,
	ofc_sema_decl_t* b);


ofc_sema_decl_list_t* ofc_sema_decl_list_create(bool case_sensitive);
ofc_sema_decl_list_t* ofc_sema_decl_list_create_ref(bool case_sensitive);
void ofc_sema_decl_list_delete(ofc_sema_decl_list_t* list);

bool ofc_sema_decl_list_add(
	ofc_sema_decl_list_t* list,
	ofc_sema_decl_t* decl);
bool ofc_sema_decl_list_add_ref(
	ofc_sema_decl_list_t* list,
	const ofc_sema_decl_t* decl);

const ofc_sema_decl_t* ofc_sema_decl_list_find(
	const ofc_sema_decl_list_t* list,
	ofc_str_ref_t name);
ofc_sema_decl_t* ofc_sema_decl_list_find_modify(
	ofc_sema_decl_list_t* list,
	ofc_str_ref_t name);

const ofc_hashmap_t* ofc_sema_decl_list_map(
	const ofc_sema_decl_list_t* list);

#endif
