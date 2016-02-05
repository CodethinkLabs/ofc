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

#include "ofc/parse.h"


unsigned ofc_parse_stmt_decl(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i;
	stmt->decl.type = ofc_parse_type(
		src, ptr, debug, &i);
	if (!stmt->decl.type)
		return 0;

	unsigned l;
	if (stmt->decl.type->type
		== OFC_PARSE_TYPE_RECORD)
	{
		stmt->decl.decl = ofc_parse_decl_list_record(
			src, &ptr[i], debug, &l);
	}
	else
	{
		bool is_f90 = ((ptr[i + 0] == ':')
			&& (ptr[i + 1] == ':'));
		if (is_f90) i += 2;

		stmt->decl.decl = ofc_parse_decl_list(
			src, &ptr[i], is_f90, debug, &l);
	}

	if (!stmt->decl.decl)
	{
		ofc_parse_type_delete(stmt->decl.type);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = OFC_PARSE_STMT_DECL;
	return i;
}

bool ofc_parse_stmt_decl_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	return (ofc_parse_type_print(cs, stmt->decl.type, true)
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_parse_decl_list_print(cs, stmt->decl.decl));
}
