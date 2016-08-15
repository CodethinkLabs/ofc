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

#ifndef __ofc_sema_common_h__
#define __ofc_sema_common_h__

typedef struct
{
	ofc_str_ref_t           name;

	unsigned                count;
	const ofc_sema_decl_t** decl;

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
	const ofc_sema_decl_t* decl);

bool ofc_sema_common_save(
	ofc_sema_common_t* common);

bool ofc_sema_common_compatible(
	const ofc_sema_common_t* a,
	const ofc_sema_common_t* b);

bool ofc_sema_common_print(
	ofc_colstr_t* cs,
	unsigned indent,
	const ofc_sema_common_t* common);


ofc_sema_common_map_t* ofc_sema_common_map_create(
	bool case_sensitive);
void ofc_sema_common_map_delete(
	ofc_sema_common_map_t* map);

const ofc_sema_common_t* ofc_sema_common_map_find(
	ofc_sema_common_map_t* map, ofc_str_ref_t name);
ofc_sema_common_t* ofc_sema_common_map_find_modify(
	ofc_sema_common_map_t* map, ofc_str_ref_t name);

bool ofc_sema_common_map_add(
	ofc_sema_common_map_t* map,
	ofc_sema_common_t* common);

bool ofc_sema_common_map_print(
	ofc_colstr_t* cs,
	unsigned indent,
	const ofc_sema_common_map_t* map);

#endif
