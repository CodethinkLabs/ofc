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

#include "ofc/parse.h"


unsigned ofc_parse_stmt_use(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_USE);
	if (i == 0)
		return 0;

	unsigned len = ofc_parse_name(
		src, &ptr[i], debug,
		&stmt->use.module);
	if (len == 0)
		return 0;
	i += len;

	if (ptr[i] == ',')
	{
		len = 0;
		ofc_parse_lhs_list_t* rename
			= ofc_parse_lhs_alias_list(
				src, &ptr[i + 1], debug, &len);
		if (len == 0)
			rename = NULL;
		else
			i += len + 1;

		stmt->use.rename = rename;
	}
	else
	{
		stmt->use.rename = NULL;
	}

	if (ptr[i] == ',')
	{
		i++;
		len = ofc_parse_keyword(
			src, &ptr[i], debug, OFC_PARSE_KEYWORD_ONLY);
		if (len == 0)
		{
			ofc_sparse_error(src, ofc_str_ref(&ptr[i], i),
				"Expected ONLY keyword after comma");
			return 0;
		}
		i += len;

		if (ptr[i] == ':')
		{
			i++;
		}
		else
		{
			ofc_sparse_error(src, ofc_str_ref(&ptr[i], i),
				"Missing semi colon after ONLY statement");
			return 0;
		}

		ofc_parse_decl_list_t* only
			= ofc_parse_decl_list(
				src, &ptr[i], true, debug, &len);
		if (!only)
		{
			only = NULL;
			ofc_sparse_warning(src, ofc_str_ref(&ptr[i], i),
				"ONLY statement without list, ignoring");
		}
		else
		{
			i += len;
		}

		stmt->use.only = only;
	}
	else
	{
		stmt->use.only = NULL;
	}

	stmt->type = OFC_PARSE_STMT_USE;

	return i;
}

bool ofc_parse_stmt_use_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!cs || !stmt
		|| stmt->type != OFC_PARSE_STMT_USE)
		return false;

	if (!ofc_colstr_keyword_atomic_writez(cs, "USE")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_sparse_ref_print(cs, stmt->use.module))
		return false;

	if (stmt->use.rename
		&& (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_parse_lhs_list_print(cs, stmt->use.rename, false)))
		return false;

	if (stmt->use.only
		&& (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_keyword_atomic_writez(cs, "ONLY")
			|| !ofc_colstr_atomic_writef(cs, ":")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_parse_decl_list_print(cs, stmt->use.only)))
		return false;

	return true;
}
