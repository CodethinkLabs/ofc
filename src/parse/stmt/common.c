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


static unsigned ofc_parse_stmt__common_namelist(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	unsigned l;
	stmt->common_namelist
		= ofc_parse_common_group_list(
			src, &ptr[i], debug, &l);
	if (!stmt->common_namelist)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	return i;
}

unsigned ofc_parse_stmt_common(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__common_namelist(
		src, ptr, debug, OFC_PARSE_KEYWORD_COMMON, stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_COMMON;
	return i;
}

unsigned ofc_parse_stmt_namelist(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__common_namelist(
		src, ptr, debug, OFC_PARSE_KEYWORD_NAMELIST, stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_NAMELIST;
	return i;
}

bool ofc_parse_stmt_common_namelist_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_COMMON:
			kwstr = "COMMON";
			break;
		case OFC_PARSE_STMT_NAMELIST:
			kwstr = "NAMELIST";
			break;
		default:
			return false;
	}

	return (stmt && ofc_colstr_atomic_writef(cs, "%s ", kwstr)
		&& ofc_parse_common_group_list_print(
			cs, stmt->common_namelist));
}
