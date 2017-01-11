/* Copyright 2015 Codethink Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef __ofc_sema_decl_h__
#define __ofc_sema_decl_h__

#include <ofc/hashmap.h>

typedef struct
{
	bool is_substring;

	union
	__attribute__((__packed__))
	{
		ofc_sema_expr_t* expr;

		struct
		{
			char* string;
			bool* mask;
		} substring;
	};
} ofc_sema_decl_init_t;

struct ofc_sema_decl_s
{
	ofc_sparse_ref_t name;

	bool type_final;
	bool type_implicit;
	const ofc_sema_type_t* type;

	ofc_sema_accessibility_e access;

	ofc_sema_scope_t*     func;
	ofc_sema_array_t*     array;
	ofc_sema_structure_t* structure;
	ofc_sema_common_t*    common;
	const ofc_sema_intrinsic_t* intrinsic;

	union
	__attribute__((__packed__))
	{
		ofc_sema_decl_init_t  init;
		ofc_sema_decl_init_t* init_array;
	};

	bool is_static;
	bool is_automatic;
	bool is_volatile;
	bool is_intrinsic;
	bool is_external;

	bool is_parameter;
	bool is_target;

	bool is_equiv;
	bool is_argument;
	bool is_return;

	bool was_read;
	bool was_written;
	bool is_stmt_func_arg;

	unsigned refcnt;
};

struct ofc_sema_decl_list_s
{
	bool case_sensitive;
	bool is_ref;

	unsigned size;
	unsigned count;

	union
	__attribute__((__packed__))
	{
		ofc_sema_decl_t**       decl;
		const ofc_sema_decl_t** decl_ref;
	};

	ofc_hashmap_t* map;
};

struct ofc_sema_decl_alias_s
{
	ofc_str_ref_t    name;
	ofc_sema_decl_t* decl;
};

struct ofc_sema_decl_alias_map_s
{
	unsigned count;

	ofc_sema_decl_alias_t** list;
	ofc_hashmap_t*          map;
};

ofc_sema_decl_alias_t* ofc_sema_decl_alias_create(
	ofc_str_ref_t name,
	ofc_sema_decl_t* decl);
void ofc_sema_decl_alias_delete(
	ofc_sema_decl_alias_t* alias);

ofc_sema_decl_alias_map_t*
	ofc_sema_decl_alias_map_create(void);
bool ofc_sema_decl_alias_map_add(
	ofc_sema_decl_alias_map_t* map,
	ofc_sema_decl_alias_t* alias);
void ofc_sema_decl_alias_map_delete(
	ofc_sema_decl_alias_map_t* map);

ofc_sema_decl_t* ofc_sema_decl_create(
	const ofc_sema_implicit_t* implicit,
	ofc_sparse_ref_t name);
ofc_sema_decl_t* ofc_sema_decl_copy(
	ofc_sema_decl_t* decl);
bool ofc_sema_decl_reference(
	ofc_sema_decl_t* decl);

bool ofc_sema_decl_type_set(
	ofc_sema_decl_t*       decl,
	const ofc_sema_type_t* type,
	ofc_sparse_ref_t       err_pos);
bool ofc_sema_decl_type_finalize(
	ofc_sema_decl_t* decl);

bool ofc_sema_decl_array_set(
	ofc_sema_decl_t*  decl,
	ofc_sema_array_t* array,
	ofc_sparse_ref_t  err_pos);

bool ofc_sema_decl_is_final(
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_mark_used(
	ofc_sema_decl_t* decl,
	bool written, bool read);

bool ofc_sema_decl(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_decl_type_scan(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
bool ofc_sema_decl_member(
	ofc_sema_scope_t* scope,
	ofc_sema_structure_t* structure,
	const ofc_parse_stmt_t* stmt);
void ofc_sema_decl_delete(
	ofc_sema_decl_t* decl);

bool ofc_sema_decl_function(
	ofc_sema_decl_t* decl);
bool ofc_sema_decl_subroutine(
	ofc_sema_decl_t* decl);

bool ofc_sema_decl_init(
	ofc_sema_decl_t* decl,
	const ofc_sema_expr_t* init);
bool ofc_sema_decl_init_offset(
	ofc_sema_decl_t* decl,
	unsigned offset,
	const ofc_sema_expr_t* init);
bool ofc_sema_decl_init_array(
	ofc_sema_decl_t* decl,
	const ofc_sema_array_t* array,
	unsigned count,
	const ofc_sema_expr_t** init);

bool ofc_sema_decl_init_substring(
	ofc_sema_decl_t* decl,
	const ofc_sema_expr_t* init,
	const ofc_sema_expr_t* first,
	const ofc_sema_expr_t* last);
bool ofc_sema_decl_init_substring_offset(
	ofc_sema_decl_t* decl,
	unsigned offset,
	const ofc_sema_expr_t* init,
	const ofc_sema_expr_t* first,
	const ofc_sema_expr_t* last);

bool ofc_sema_decl_init_func(
	ofc_sema_decl_t* decl,
	ofc_sema_scope_t* func);

bool ofc_sema_decl_size(
	const ofc_sema_decl_t* decl,
	unsigned* size);
bool ofc_sema_decl_elem_count(
	const ofc_sema_decl_t* decl,
	unsigned* count);

bool ofc_sema_decl_is_array(
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_is_structure(
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_is_composite(
	const ofc_sema_decl_t* decl);

bool ofc_sema_decl_is_external(
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_is_unknown_external(
	const ofc_sema_decl_t* decl);

bool ofc_sema_decl_is_subroutine(
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_is_function(
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_is_procedure(
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_is_stmt_func(
	const ofc_sema_decl_t* decl);

bool ofc_sema_decl_is_parameter(
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_is_common(
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_is_intrinsic(
	const ofc_sema_decl_t* decl);

bool ofc_sema_decl_has_initializer(
	const ofc_sema_decl_t* decl,
	bool* complete);
bool ofc_sema_decl_is_initialized(
	const ofc_sema_decl_t* decl,
	bool* complete);

const ofc_sema_type_t* ofc_sema_decl_type(
	const ofc_sema_decl_t* decl);
const ofc_sema_type_t* ofc_sema_decl_base_type(
	const ofc_sema_decl_t* decl);

bool ofc_sema_decl_foreach_expr(
	ofc_sema_decl_t* decl, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param));
bool ofc_sema_decl_foreach_scope(
	ofc_sema_decl_t* decl, void* param,
	bool (*func)(ofc_sema_scope_t* scope, void* param));


ofc_sema_decl_list_t* ofc_sema_decl_list_create(bool case_sensitive);
ofc_sema_decl_list_t* ofc_sema_decl_list_create_ref(bool case_sensitive);
void ofc_sema_decl_list_delete(ofc_sema_decl_list_t* list);

bool ofc_sema_decl_list_add(
	ofc_sema_decl_list_t* list,
	ofc_sema_decl_t* decl);
bool ofc_sema_decl_list_add_ref(
	ofc_sema_decl_list_t* list,
	const ofc_sema_decl_t* decl);
void ofc_sema_decl_list_remove(
	ofc_sema_decl_list_t* list,
	ofc_sema_decl_t* decl);

const ofc_sema_decl_t* ofc_sema_decl_list_find(
	const ofc_sema_decl_list_t* list,
	ofc_str_ref_t name);
ofc_sema_decl_t* ofc_sema_decl_list_find_modify(
	ofc_sema_decl_list_t* list,
	ofc_str_ref_t name);

const ofc_hashmap_t* ofc_sema_decl_list_map(
	const ofc_sema_decl_list_t* list);

bool ofc_sema_decl_print(ofc_colstr_t* cs,
	unsigned indent,
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_print_data_init(ofc_colstr_t* cs,
	unsigned indent,
	const ofc_sema_decl_t* decl);
bool ofc_sema_decl_print_name(ofc_colstr_t* cs,
	const ofc_sema_decl_t* decl);

bool ofc_sema_decl_list_stmt_func_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_decl_list_t* decl_list);
bool ofc_sema_decl_list_procedure_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_decl_list_t* decl_list);
bool ofc_sema_decl_list_procedure_spec_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_decl_list_t* decl_list);
bool ofc_sema_decl_list_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_decl_list_t* decl_list);

bool ofc_sema_decl_list_foreach(
	ofc_sema_decl_list_t* list, void* param,
	bool (*func)(ofc_sema_decl_t* decl, void* param));
bool ofc_sema_decl_list_foreach_expr(
	ofc_sema_decl_list_t* list, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param));
bool ofc_sema_decl_list_foreach_scope(
	ofc_sema_decl_list_t* list, void* param,
	bool (*func)(ofc_sema_scope_t* scope, void* param));

#endif
