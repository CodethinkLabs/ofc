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


ofc_parse_stmt_list_t* ofc_parse_file(const ofc_sparse_t* src)
{
	const char* ptr = ofc_sparse_strz(src);

	ofc_parse_debug_t* debug
		= ofc_parse_debug_create();
	if (!debug) return NULL;

	unsigned len;
	ofc_parse_stmt_list_t* list
		= ofc_parse_stmt_list(src, ptr, debug, &len);
	if (!list) return NULL;

	ofc_parse_debug_print(debug);
	ofc_parse_debug_delete(debug);

	if (ptr[len] != '\0')
	{
		ofc_sparse_error(src, &ptr[len],
			"Expected end of input");
		ofc_parse_stmt_list_delete(list);
		return NULL;
	}

	return list;
}

bool ofc_parse_file_print(
	ofc_colstr_t* cs,
	const ofc_parse_stmt_list_t* list)
{
	return (ofc_parse_stmt_list_print(cs, 0, list)
		&& ofc_colstr_writef(cs, "\n"));
}
