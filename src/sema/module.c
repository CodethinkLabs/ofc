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

ofc_sema_module_t* ofc_sema_module_create(
	ofc_sema_scope_t* mscope,
	ofc_sema_decl_list_t* only,
	ofc_sema_decl_alias_map_t* rename)
{
	if (!mscope) return NULL;

	ofc_sema_module_t* module
		= (ofc_sema_module_t*)malloc(
			sizeof(ofc_sema_module_t));
	if (!module) return NULL;

	module->scope  = mscope;
	module->only   = only;
	module->rename = rename;

	return module;
}

void ofc_sema_module_delete(
	ofc_sema_module_t* module)
{
	if (!module) return;

	ofc_sema_decl_alias_map_delete(module->rename);
	ofc_sema_decl_list_delete(module->only);

	free(module);
}

bool ofc_sema_module_print(
	ofc_colstr_t* cs,
	ofc_sema_module_t* module)
{
	if (!cs || !module)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "USE")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_str_ref_print(cs, module->scope->name))
		return false;

	if (module->rename)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " "))
			return false;

		ofc_sema_decl_alias_map_t* rename
			= module->rename;

		unsigned i;
		for (i = 0; i < rename->count; i++)
		{
			if (i > 0)
			{
				if (!ofc_colstr_atomic_writef(cs, ",")
					|| !ofc_colstr_atomic_writef(cs, " "))
					return false;
			}

			ofc_sema_decl_alias_t* alias
				= rename->list[i];

			if (!ofc_str_ref_print(cs, alias->name)
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_atomic_writef(cs, "=>")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_str_ref_print(cs,
					alias->decl->name.string))
				return false;
		}
	}

	if (module->only)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "ONLY")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, ":")
			|| !ofc_colstr_atomic_writef(cs, " "))
			return false;

		ofc_sema_decl_list_t* only
			= module->only;

		unsigned i;
		for (i = 0; i < only->count; i++)
		{
			if (i > 0)
			{
				if (!ofc_colstr_atomic_writef(cs, ",")
					|| !ofc_colstr_atomic_writef(cs, " "))
					return false;
			}

			ofc_sema_decl_t* decl = only->decl[i];

            if (!ofc_str_ref_print(cs, decl->name.string))
				return false;
		}
	}

	return true;
}

ofc_sema_module_list_t* ofc_sema_module_list_create()
{
	ofc_sema_module_list_t* list
		= (ofc_sema_module_list_t*)malloc(
			sizeof(ofc_sema_module_list_t));
	if (!list) return NULL;

	list->count  = 0;
	list->module = NULL;

	return list;
}

bool ofc_sema_module_list_add(
	ofc_sema_module_list_t* list,
	ofc_sema_module_t* module)
{
	if (!list || !module)
		return false;

	ofc_sema_module_t** nmodule
		= (ofc_sema_module_t**)realloc(list->module,
			(sizeof(ofc_sema_module_t*) * (list->count + 1)));
	if (!nmodule) return false;
	list->module = nmodule;

    list->module[list->count++] = module;

	return true;
}

void ofc_sema_module_list_delete(
	ofc_sema_module_list_t* list)
{
	if (!list) return;

	if (list->module)
	{
		unsigned i;
		for (i = 0; i < list->count; i++)
			ofc_sema_module_delete(list->module[i]);

		free (list->module);
	}

	free (list);
}

bool ofc_sema_module_list_print(
	ofc_colstr_t* cs, unsigned indent,
	ofc_sema_module_list_t* list)
{
	if (!cs || !list)
		return false;

	unsigned i;
	for(i = 0; i < list->count; i++)
	{
		if (!ofc_colstr_newline(cs, indent, NULL))
			return false;

		if (!ofc_sema_module_print(
			cs, list->module[i]))
				return false;
	}

	return true;
}
