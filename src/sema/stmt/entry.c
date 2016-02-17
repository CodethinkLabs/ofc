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


ofc_sema_stmt_t* ofc_sema_stmt_entry(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_ENTRY))
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_ENTRY;
	s.entry.name = stmt->call_entry.name.string;
	s.entry.args = NULL;

	if (stmt->call_entry.args)
	{
		s.entry.args = ofc_sema_arg_list(
			stmt->call_entry.args);
		if (!s.entry.args) return NULL;

		unsigned i;
		for (i = 0; i < s.entry.args->count; i++)
		{
			const ofc_sema_arg_t arg
				= s.entry.args->arg[i];
			if (arg.alt_return)
				continue;

			ofc_sema_decl_t* decl
				= ofc_sema_scope_decl_find_modify(
					scope, arg.name.string, true);
			if (decl)
			{
				decl->is_argument = true;
			}
			else
			{
				ofc_sema_spec_t* spec
					= ofc_sema_scope_spec_modify(
						scope, arg.name);
				if (!spec)
				{
					ofc_sema_arg_list_delete(s.entry.args);
					return NULL;
				}
				spec->is_argument = true;
			}
		}
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_arg_list_delete(s.entry.args);
		return NULL;
	}

	return as;
}

bool ofc_sema_stmt_entry_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if ((!cs || !stmt)
		|| (stmt->type != OFC_SEMA_STMT_ENTRY))
		return false;

	if (!ofc_colstr_atomic_writef(cs, "ENTRY")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_str_ref_print(cs, stmt->entry.name)
		|| !ofc_colstr_atomic_writef(cs, "(")
		|| (stmt->entry.args && !ofc_sema_arg_list_print(cs,
			stmt->entry.args))
		|| !ofc_colstr_atomic_writef(cs, ")"))
		return false;

	return true;
}
