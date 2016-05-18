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

#ifndef __ofc_sema_structure_h__
#define __ofc_sema_structure_h__

typedef enum
{
	OFC_SEMA_STRUCTURE_VAX_STRUCTURE,
	OFC_SEMA_STRUCTURE_VAX_UNION,
	OFC_SEMA_STRUCTURE_VAX_MAP,
	OFC_SEMA_STRUCTURE_F90_TYPE,
	OFC_SEMA_STRUCTURE_F90_TYPE_SEQUENCE,
} ofc_sema_structure_e;

typedef struct ofc_sema_structure_s ofc_sema_structure_t;

typedef struct
{
	bool is_structure;

	union
	{
		ofc_sema_structure_t* structure;
		ofc_sema_decl_t*      decl;
	};
} ofc_sema_structure_member_t;

struct ofc_sema_structure_s
{
	ofc_sparse_ref_t      name;
	ofc_sema_structure_e  type;

	ofc_sema_implicit_t* implicit;

	unsigned count;
	ofc_sema_structure_member_t** member;

	ofc_hashmap_t* map;

	unsigned refcnt;
};

ofc_sema_structure_t* ofc_sema_structure(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);

bool ofc_sema_structure_reference(
	ofc_sema_structure_t* structure);
void ofc_sema_structure_delete(
	ofc_sema_structure_t* structure);

ofc_sema_decl_t* ofc_sema_structure_decl_find_create(
	ofc_sema_structure_t* structure,
	ofc_sparse_ref_t name);
bool ofc_sema_structure_member_add_structure(
	ofc_sema_structure_t* structure,
	ofc_sema_structure_t* member);

bool ofc_sema_structure_is_union(
	const ofc_sema_structure_t* structure);
bool ofc_sema_structure_is_nested(
	const ofc_sema_structure_t* structure);
bool ofc_sema_structure_is_derived_type(
	const ofc_sema_structure_t* structure);

bool ofc_sema_structure_member_count(
	const ofc_sema_structure_t* structure,
	unsigned* count);
ofc_sema_decl_t* ofc_sema_structure_member_get_decl_offset(
	ofc_sema_structure_t* structure,
	unsigned offset);
ofc_sema_decl_t* ofc_sema_structure_member_get_decl_name(
	ofc_sema_structure_t* structure,
	ofc_str_ref_t name);
bool ofc_sema_structure_member_offset(
	const ofc_sema_structure_t* structure,
	const ofc_sema_decl_t*      member,
	unsigned*                   offset);

bool ofc_sema_structure_size(
	const ofc_sema_structure_t* structure,
	unsigned* size);

bool ofc_sema_structure_elem_count(
	const ofc_sema_structure_t* structure,
	unsigned* count);
ofc_sema_decl_t* ofc_sema_structure_elem_get(
	ofc_sema_structure_t* structure,
	unsigned offset);
bool ofc_sema_structure_elem_print(
	ofc_colstr_t* cs,
	const ofc_sema_structure_t* structure,
	unsigned offset);

bool ofc_sema_structure_print_name(
	ofc_colstr_t* cs,
	const ofc_sema_structure_t* structure);

bool ofc_sema_structure_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_structure_t* structure);

bool ofc_sema_structure_foreach_expr(
	ofc_sema_structure_t* structure, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param));


typedef struct
{
	unsigned count, size;
	ofc_sema_structure_t** structure;
	ofc_hashmap_t* map;
} ofc_sema_structure_list_t;

ofc_sema_structure_list_t* ofc_sema_structure_list_create(
	bool case_sensitive);
void ofc_sema_structure_list_delete(
	ofc_sema_structure_list_t* list);

bool ofc_sema_structure_list_add(
	ofc_sema_structure_list_t* list,
	ofc_sema_structure_t* structure);
void ofc_sema_structure_list_remove(
	ofc_sema_structure_list_t* list,
	ofc_sema_structure_t* structure);

const ofc_sema_structure_t* ofc_sema_structure_list_find(
	const ofc_sema_structure_list_t* list, ofc_str_ref_t name);
ofc_sema_structure_t* ofc_sema_structure_list_find_modify(
	ofc_sema_structure_list_t* list, ofc_str_ref_t name);

bool ofc_sema_structure_list_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_structure_list_t* list);

bool ofc_sema_structure_list_foreach(
	ofc_sema_structure_list_t* list, void* param,
	bool (*func)(ofc_sema_structure_t* structure, void* param));
bool ofc_sema_structure_list_foreach_expr(
	ofc_sema_structure_list_t* list, void* param,
	bool (*func)(ofc_sema_expr_t* expr, void* param));

#endif
