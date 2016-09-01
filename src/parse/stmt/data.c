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



unsigned ofc_parse_stmt_data(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_DATA);
	if (i == 0) return 0;

	unsigned l;
	stmt->data = ofc_parse_data_list(
		src, &ptr[i], debug, &l);
	if (!stmt->data)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = OFC_PARSE_STMT_DATA;
	return i;
}

bool ofc_parse_stmt_data_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	return (stmt && ofc_colstr_keyword_atomic_writez(cs, "DATA")
		&& ofc_colstr_atomic_writef(cs, " ")
		&& ofc_parse_data_list_print(cs, stmt->data));
}
