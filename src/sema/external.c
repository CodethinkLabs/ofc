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

#include "ofc/sema.h"

static const ofc_str_ref_t* ofc_sema_external__key(
	const ofc_sema_external_t* external)
{
	return (external ? &external->name.string : NULL);
}

ofc_sema_external_list_t* ofc_sema_external_list_create(
	bool case_sensitive)
{
	ofc_sema_external_list_t* list
		= (ofc_sema_external_list_t*)malloc(
			sizeof(ofc_sema_external_list_t));
	if (!list) return NULL;

	list->case_sensitive = case_sensitive;
	list->count    = 0;
	list->external = NULL;

	list->map = ofc_hashmap_create(
		(void*)(list->case_sensitive
			? ofc_str_ref_ptr_hash
			: ofc_str_ref_ptr_hash_ci),
		(void*)(list->case_sensitive
			? ofc_str_ref_ptr_equal
			: ofc_str_ref_ptr_equal_ci),
		(void*)ofc_sema_external__key, NULL);
	if (!list->map)
	{
		free(list);
		return NULL;
	}

	return list;
}

const ofc_sema_external_t* ofc_sema_external_list_find(
	const ofc_sema_external_list_t* list, ofc_str_ref_t name)
{
	if (!list) return NULL;

	return ofc_hashmap_find(
		list->map, &name);
}

ofc_sema_external_t* ofc_sema_external_list_find_modify(
	const ofc_sema_external_list_t* list, ofc_str_ref_t name)
{
	if (!list) return NULL;

	return ofc_hashmap_find_modify(
		list->map, &name);
}

bool ofc_sema_external_list_add(
	ofc_sema_external_list_t* list,
	ofc_sema_external_t* external)
{
	if (!list || !external)
		return false;

	/* Check for duplicate definitions. */
	if (ofc_sema_external_list_find(
		list, external->name.string))
		return false;

	if (!ofc_hashmap_add(list->map, external))
		return false;

	ofc_sema_external_t** nexternal
		= (ofc_sema_external_t**)realloc(list->external,
			(sizeof(ofc_sema_external_t*) * (list->count + 1)));
	if (!nexternal) return false;

	list->external = nexternal;
	list->external[list->count++] = external;

	return true;
}

void ofc_sema_external_list_delete(
	ofc_sema_external_list_t* list)
{
	if (!list)
		return;

	ofc_hashmap_delete(list->map);

	unsigned i;
	for (i = 0; i < list->count; i++)
		ofc_sema_external_delete(list->external[i]);

	free(list->external);

	free(list);
}

ofc_sema_external_t* ofc_sema_external_create(
	ofc_sparse_ref_t ref, ofc_sema_decl_t* decl)
{
	ofc_sema_external_t* external
		= (ofc_sema_external_t*)malloc(
			sizeof(ofc_sema_external_t));
	if (!external) return NULL;

	external->name = ref;

	if (decl && !ofc_sema_decl_reference(decl))
	{
		free(external);
		return NULL;
	}
	external->decl = decl;

	return external;
}

void ofc_sema_external_delete(
	ofc_sema_external_t* external)
{
	if (!external)
		return;

	ofc_sema_decl_delete(external->decl);
	free(external);
}

bool ofc_sema_external_is_decl(
	const ofc_sema_external_t* external)
{
	if (!external)
		return false;

	return (external->decl != NULL);
}

bool ofc_sema_stmt_external(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt)
		return false;

	if (stmt->type != OFC_PARSE_STMT_DECL_ATTR_EXTERNAL)
		return false;

	unsigned i;
	for (i = 0; i < stmt->decl_attr.count; i++)
	{
		if (!stmt->decl_attr.name[i])
		{
			ofc_sparse_ref_error(stmt->src,
				"Attribute doesn't have a name");
			return false;
		}

		ofc_sparse_ref_t decl_name
			= *(stmt->decl_attr.name[i]);

		ofc_sema_decl_t* decl
			= ofc_sema_scope_decl_find_modify(
				scope, decl_name.string, true);
		if (decl)
		{
			if (decl->is_intrinsic)
			{
				ofc_sparse_ref_error(stmt->src,
					"Specifying '%.*s' as INTRINSIC and EXTERNAL",
					decl_name.string.size, decl_name.string.base);
				return false;
			}
			if (decl->is_external)
			{
				ofc_sparse_ref_warning(stmt->src,
					"Re-declaring '%.*s' as EXTERNAL",
					decl_name.string.size, decl_name.string.base);
			}
			decl->is_external = true;
		}

		ofc_sema_external_t* external
			= ofc_sema_external_create(decl_name, decl);
		if (!external) return false;

		if (!ofc_sema_external_list_add(
				scope->external, external))
		{
			ofc_sema_external_delete(external);
			return false;
		}
	}

	return true;
}

bool ofc_sema_external_arg_print(
	ofc_colstr_t* cs,
	const ofc_sema_external_t* external)
{
	if (!cs || !external)
		return false;

	if (external->decl)
	{
		if(!ofc_sema_decl_print_name(cs, external->decl))
			return false;
	}
	else
	{
		if (!ofc_sparse_ref_print(cs, external->name))
			return false;
	}

	return true;
}


bool ofc_sema_external_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_external_t* external)
{
	if (!cs || !external)
		return false;

	if (!ofc_colstr_newline(cs, indent, NULL))
		return false;

	if (!ofc_colstr_keyword_atomic_writef(cs, "EXTERNAL")
		|| !ofc_colstr_atomic_writef(cs, " "))
		return false;

	if (!ofc_sema_external_arg_print(cs, external))
		return false;

	return true;
}

bool ofc_sema_external_list_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_sema_external_list_t* list)
{
	if (!cs || !list)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		ofc_sema_external_t* external = list->external[i];

		if (!external) continue;

		if (!ofc_sema_external_print(cs, indent, external))
			return false;
	}

	return true;
}
