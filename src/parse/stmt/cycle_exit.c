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

unsigned ofc_parse_stmt_cycle_exit(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	bool is_exit = false;
	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_CYCLE);
	if (i == 0)
	{
		i = ofc_parse_keyword(
			src, ptr, debug, OFC_PARSE_KEYWORD_EXIT);
		if (i == 0) return 0;
		is_exit = true;
	}

	stmt->type = (is_exit
		? OFC_PARSE_STMT_EXIT
		: OFC_PARSE_STMT_CYCLE);

	stmt->cycle_exit.has_keyword = false;

	unsigned len = ofc_parse_keyword(
		src, &ptr[i], debug, OFC_PARSE_KEYWORD_DO);
	if (len > 0)
	{
		stmt->cycle_exit.has_keyword = true;
		stmt->cycle_exit.keyword = OFC_PARSE_KEYWORD_DO;
		i += len;
	}

	return i;
}

bool ofc_parse_stmt_cycle_exit_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_CYCLE:
			kwstr = "CYCLE";
			break;
		case OFC_PARSE_STMT_EXIT:
			kwstr = "EXIT";
			break;
		default:
			return false;
	}

	if (!ofc_colstr_keyword_atomic_writez(cs, kwstr))
		return false;

	if (stmt->cycle_exit.has_keyword)
	{
		if (!ofc_colstr_writef(cs, " ")
			&& !ofc_colstr_keyword_atomic_writez(cs,
				ofc_parse_keyword_name(stmt->cycle_exit.keyword)))
			return false;
	}

	return true;
}
