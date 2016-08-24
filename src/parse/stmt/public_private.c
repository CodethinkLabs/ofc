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


unsigned ofc_parse_stmt__public_private(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt,
	ofc_parse_keyword_e keyword)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	bool has_list = (ptr[i] == ':') && (ptr[i + 1] == ':');
	if (has_list) i += 2;

	unsigned l;
	stmt->public_private.list = ofc_parse_lhs_list(
		src, &ptr[i], debug, &l);
	if (has_list && !stmt->public_private.list)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	if (stmt->public_private.list) i += l;

	return i;
}

unsigned ofc_parse_stmt_public(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_PUBLIC;
	return ofc_parse_stmt__public_private(src, ptr,
		debug, stmt, OFC_PARSE_KEYWORD_PUBLIC);
}

unsigned ofc_parse_stmt_private(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_PRIVATE;
	return ofc_parse_stmt__public_private(src, ptr,
		debug, stmt, OFC_PARSE_KEYWORD_PRIVATE);
}

bool ofc_parse_stmt__public_private_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_PUBLIC:
			kwstr = "PUBLIC";
			break;
		case OFC_PARSE_STMT_PRIVATE:
			kwstr = "PRIVATE";
			break;
		default:
			return false;
	}

	if (!ofc_colstr_keyword_atomic_writez(cs, kwstr)
		|| !ofc_colstr_atomic_writef(cs, " "))
		return false;

	if (stmt->public_private.list
		&& (!ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "::")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_parse_lhs_list_print(
				cs, stmt->public_private.list, true)))
		return false;

	return true;
}

bool ofc_parse_stmt_public_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	return ofc_parse_stmt__public_private_print(cs, stmt);
}

bool ofc_parse_stmt_private_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	return ofc_parse_stmt__public_private_print(cs, stmt);
}
