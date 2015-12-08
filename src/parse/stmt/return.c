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

static unsigned ofc_parse_stmt__return(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	unsigned len = 0;
	stmt->stop_pause_return.value = ofc_parse_expr(
		src, &ptr[i], debug, &len);

	return (i + len);
}

unsigned ofc_parse_stmt_stop(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__return(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_STOP, stmt);
	if (i == 0) return 0;
	stmt->type = OFC_PARSE_STMT_STOP;
	return i;
}

unsigned ofc_parse_stmt_pause(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__return(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_PAUSE, stmt);
	if (i == 0) return 0;
	stmt->type = OFC_PARSE_STMT_PAUSE;
	return i;
}

unsigned ofc_parse_stmt_return(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__return(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_RETURN, stmt);
	if (i == 0) return 0;
	stmt->type = OFC_PARSE_STMT_RETURN;
	return i;
}


bool ofc_parse_stmt_stop_pause_return_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_STOP:
			kwstr = "STOP";
			break;
		case OFC_PARSE_STMT_PAUSE:
			kwstr = "PAUSE";
			break;
		case OFC_PARSE_STMT_RETURN:
			kwstr = "RETURN";
			break;
		default:
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, "%s", kwstr))
		return false;

	if (stmt->stop_pause_return.value)
	{
		if (!ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_parse_expr_print(cs,
				stmt->stop_pause_return.value))
			return false;
	}

	return true;
}
