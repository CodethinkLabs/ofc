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

#include <ofc/parse.h>


unsigned ofc_parse_stmt_parameter(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_PARAMETER);
	if (i == 0) return 0;

	bool has_brackets = (ptr[i] == '(');
	if (has_brackets) i += 1;

	unsigned l;
	stmt->parameter.list = ofc_parse_assign_list(
		src, &ptr[i], debug, &l);
	if (!stmt->parameter.list)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	if (has_brackets)
	{
		if (ptr[i++] != ')')
		{
			ofc_parse_assign_list_delete(
				stmt->parameter.list);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
	}

	stmt->type = OFC_PARSE_STMT_PARAMETER;
	return i;
}

bool ofc_parse_stmt_parameter_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
    if (!stmt)
		return false;

	return (ofc_colstr_atomic_writef(cs, "PARAMETER")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_colstr_atomic_writef(cs, "(")
		&& ofc_parse_assign_list_print(
			cs, stmt->parameter.list)
		&& ofc_colstr_atomic_writef(cs, ")"));
}
