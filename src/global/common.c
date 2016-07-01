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

#include <ofc/global.h>



typedef struct
{
	ofc_str_ref_t       name;
	unsigned            count;
	ofc_sema_common_t** block;
} ofc_common_list_t;

static ofc_common_list_t* ofc_common_list_create(
	ofc_str_ref_t name, ofc_sema_common_t* common)
{
	ofc_common_list_t* list
		= (ofc_common_list_t*)malloc(
			sizeof(ofc_common_list_t));
	if (!list) return NULL;

	list->block
		= (ofc_sema_common_t**)malloc(
			sizeof(ofc_sema_common_t*));
	if (!list->block)
	{
		free(list);
		return NULL;
	}

	list->name = name;
	list->count = 1;
	list->block[0] = common;
	return list;
}

static void ofc_common_list_delete(
	ofc_common_list_t* list)
{
	if (!list) return;
	free(list->block);
	free(list);
}

static ofc_str_ref_t* ofc_common_list_name(
	ofc_common_list_t* list)
{
	return (list ? &list->name : NULL);
}

static bool ofc_common_list_add(
	ofc_common_list_t* list, ofc_sema_common_t* block)
{
	if (!list || !block)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (list->block[i] == block)
			return true;
	}

	ofc_sema_common_t** nblock
		= (ofc_sema_common_t**)realloc(list->block,
			(sizeof(ofc_sema_common_t*) * (list->count + 1)));
	if (!nblock) return false;
	list->block = nblock;

	list->block[list->count++] = block;
	return true;
}



static bool ofc_global_pass_common__scope(
	ofc_sema_scope_t* scope,
	ofc_hashmap_t* common_table)
{
	if (!scope || !common_table)
		return false;

	if (!scope->common)
		return true;

	unsigned i;
	for (i = 0; i < scope->common->count; i++)
	{
		if (!scope->common->common[i])
			continue;

		ofc_common_list_t* list
			= ofc_hashmap_find_modify(
				common_table, &scope->common->common[i]->name);
		if (list)
		{
			if (!ofc_common_list_add(
				list, scope->common->common[i]))
				return false;
		}
		else
		{
			list = ofc_common_list_create(
				scope->common->common[i]->name,
				scope->common->common[i]);
			if (!list) return false;

			if (!ofc_hashmap_add(common_table, list))
			{
				ofc_common_list_delete(list);
				return false;
			}
		}
	}

	return true;
}


static bool ofc_global_pass_common__check(
	ofc_common_list_t* list, void* param)
{
	(void)param;

	if (!list)
		return false;

	bool conflict = false;

	unsigned i;
	for (i = 1; i < list->count; i++)
	{
		if (!ofc_sema_common_compatible(
			list->block[0], list->block[i]))
			conflict = true;
	}

	if (conflict)
	{
		ofc_file_warning(NULL, NULL,
			"Conflicting definitions for COMMON block '%.*s'",
			list->block[0]->name.size, list->block[0]->name.base);
	}

	return true;
}

bool ofc_global_pass_common(
	ofc_sema_scope_t* scope)
{
	if (!scope)
		return false;

	ofc_hashmap_t* common_table
		= ofc_hashmap_create(
			(void*)ofc_str_ref_ptr_hash,
			(void*)ofc_str_ref_ptr_equal_ci,
			(void*)ofc_common_list_name,
			(void*)ofc_common_list_delete);
	if (!common_table) return false;

	if (!ofc_sema_scope_foreach_scope(
		scope, common_table,
		(void*)ofc_global_pass_common__scope))
		return false;

	bool success = ofc_hashmap_foreach(
		common_table, NULL, (void*)ofc_global_pass_common__check);
	ofc_hashmap_delete(common_table);
	return success;
}
