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

#include "ofc/sema.h"


ofc_sema_common_t* ofc_sema_common_create(
	ofc_str_ref_t name)
{
	ofc_sema_common_t* common
		= (ofc_sema_common_t*)malloc(
			sizeof(ofc_sema_common_t));
	if (!common) return NULL;

	common->count = 0;
	common->decl  = NULL;
	common->save  = false;

	common->name = name;
	return common;
}

void ofc_sema_common_delete(
	ofc_sema_common_t* common)
{
	if (!common)
		return;

	free(common->decl);
	free(common);
}

bool ofc_sema_common_add(
	ofc_sema_common_t*     common,
	const ofc_sema_decl_t* decl)
{
	if (!common || !decl)
		return false;

	const ofc_sema_decl_t** ndecl
		= (const ofc_sema_decl_t**)realloc(common->decl,
			sizeof(const ofc_sema_decl_t*) * (common->count + 1));
	if (!ndecl) return false;
	common->decl = ndecl;

	common->decl[common->count] = decl;
	common->count++;
	return true;
}

bool ofc_sema_common_save(
	ofc_sema_common_t* common)
{
	if (!common)
		return false;

	common->save = true;
	return true;
}


bool ofc_sema_common_compatible(
	const ofc_sema_common_t* a,
	const ofc_sema_common_t* b)
{
	if (!a || !b)
		return false;

	if (a->count != b->count)
		return false;

	unsigned i;
	for (i = 0; i < a->count; i++)
	{
		const ofc_sema_type_t* type[2];
		type[0] = ofc_sema_decl_type(a->decl[i]);
		type[1] = ofc_sema_decl_type(b->decl[i]);
		if (!ofc_sema_type_compatible(type[0], type[1]))
			return false;

		unsigned size[2];
		if (!ofc_sema_decl_size(a->decl[i], &size[0])
			|| !ofc_sema_decl_size(a->decl[i], &size[1])
			|| (size[0] != size[1]))
			return false;
	}

	return true;
}


bool ofc_sema_common_print(
	ofc_colstr_t* cs,
	unsigned indent,
	const ofc_sema_common_t* common)
{
	if (!cs || !common)
		return false;

	if (!ofc_colstr_newline(cs, indent, NULL)
		|| !ofc_colstr_keyword_atomic_writez(cs, "COMMON")
		|| !ofc_colstr_atomic_writef(cs, " "))
		return false;

	if (!ofc_str_ref_empty(common->name))
	{
		if (!ofc_colstr_atomic_writef(cs, "/")
			|| !ofc_str_ref_print(cs, common->name)
			|| !ofc_colstr_atomic_writef(cs, "/"))
			return false;
	}

	unsigned i;
	for (i = 0; i < common->count; i++)
	{
		if (i > 0)
		{
			if (!ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " "))
			return false;
		}

		if (!ofc_sema_decl_print_name(
			cs, common->decl[i]))
			return false;
	}

	return true;
}

static const ofc_str_ref_t* ofc_sema_common__key(
	const ofc_sema_common_t* common)
{
	return (common ? &common->name : NULL);
}

ofc_sema_common_map_t* ofc_sema_common_map_create(
	bool case_sensitive)
{
	ofc_sema_common_map_t* map
		= (ofc_sema_common_map_t*)malloc(
			sizeof(ofc_sema_common_map_t));
	if (!map) return NULL;

	map->map = ofc_hashmap_create(
		(void*)(case_sensitive
			? ofc_str_ref_ptr_hash
			: ofc_str_ref_ptr_hash_ci),
		(void*)(case_sensitive
			? ofc_str_ref_ptr_equal
			: ofc_str_ref_ptr_equal_ci),
		(void*)ofc_sema_common__key, NULL);

	if (!map->map)
	{
		free(map);
		return NULL;
	}

	map->count  = 0;
	map->common = NULL;

	return map;
}

void ofc_sema_common_map_delete(
	ofc_sema_common_map_t* map)
{
	if (!map)
		return;

	unsigned i;
	for (i = 0; i < map->count; i++)
		ofc_sema_common_delete(map->common[i]);
	free(map->common);

	ofc_hashmap_delete(map->map);
	free(map);
}

const ofc_sema_common_t* ofc_sema_common_map_find(
	ofc_sema_common_map_t* map, ofc_str_ref_t name)
{
	if (!map)
		return NULL;
	return ofc_hashmap_find(
		map->map, &name);
}

ofc_sema_common_t* ofc_sema_common_map_find_modify(
	ofc_sema_common_map_t* map, ofc_str_ref_t name)
{
	if (!map)
		return NULL;
	return ofc_hashmap_find_modify(
		map->map, &name);
}

bool ofc_sema_common_map_add(
	ofc_sema_common_map_t* map,
	ofc_sema_common_t* common)
{
	if (!map || !common)
		return false;

	if (ofc_hashmap_find(
		map->map, &common->name))
		return false;

	ofc_sema_common_t** ncommon
		= (ofc_sema_common_t**)realloc(map->common,
			(sizeof(ofc_sema_common_t*) * (map->count + 1)));
	if (!ncommon) return false;
	map->common = ncommon;

	if (!ofc_hashmap_add(
		map->map, common))
		return false;

	map->common[map->count++] = common;
	return true;
}

bool ofc_sema_common_map_print(
	ofc_colstr_t* cs,
	unsigned indent,
	const ofc_sema_common_map_t* map)
{
	if (!cs || !map)
		return false;

	unsigned i;
	for (i = 0; i < map->count; i++)
	{
		if (!ofc_sema_common_print(
			cs, indent, map->common[i]))
			return false;
	}

	return true;
}
