/* Copyright 2016 Codethink Ltd.
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

#ifndef __ofc_sema_external_h__
#define __ofc_sema_external_h__

typedef struct
{
	ofc_sparse_ref_t name;
	ofc_sema_decl_t* decl;
} ofc_sema_external_t;

typedef struct
{
	bool case_sensitive;
	unsigned count;
	ofc_sema_external_t** external;
	ofc_hashmap_t* map;
} ofc_sema_external_list_t;


ofc_sema_external_list_t* ofc_sema_external_list_create(
	bool case_sensitive);
const ofc_sema_external_t* ofc_sema_external_list_find(
	const ofc_sema_external_list_t* list, ofc_str_ref_t name);
ofc_sema_external_t* ofc_sema_external_list_find_modify(
	const ofc_sema_external_list_t* list, ofc_str_ref_t name);
void ofc_sema_external_list_delete(
	ofc_sema_external_list_t* list);

bool ofc_sema_stmt_external(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
void ofc_sema_external_delete(
	ofc_sema_external_t* external);

bool ofc_sema_external_is_decl(
	const ofc_sema_external_t* external);

bool ofc_sema_external_arg_print(
	ofc_colstr_t* cs,
	const ofc_sema_external_t* external);
bool ofc_sema_external_list_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_external_list_t* list);

#endif
