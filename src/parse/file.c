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


unsigned ofc_parse_stmt_program_end(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_list_t* list);

bool ofc_parse_file_include(
	const ofc_sparse_t*    src,
	ofc_parse_stmt_list_t* list,
	ofc_parse_debug_t*     debug)
{
	const char* ptr = ofc_sparse_strz(src);
	if (!list || !ptr)
		return false;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i;
	if (!ofc_parse_stmt_sublist(
		list, src, ptr, debug, &i))
		return false;

	unsigned l = ofc_parse_stmt_program_end(
		src, &ptr[i], debug, list);
	if (l > 0)
	{
		ofc_sparse_warning(src, ofc_str_ref(&ptr[i], 0),
			"Implicit PROGRAM statement");

		i += l;

		if (!ofc_parse_stmt_sublist(
			list, src, &ptr[i], debug, &l))
		{
			ofc_parse_debug_rewind(debug, dpos);
			return false;
		}
		i += l;
	}

	if (ptr[i] != '\0')
	{
		ofc_sparse_error(src, ofc_str_ref(&ptr[i], 0),
			"Expected end of input");
		ofc_parse_debug_rewind(debug, dpos);
		return false;
	}

	return true;
}

ofc_parse_file_t* ofc_parse_file(ofc_sparse_t* src)
{
	ofc_parse_debug_t* debug
		= ofc_parse_debug_create();
	if (!debug) return NULL;

	ofc_parse_stmt_list_t* list
		= ofc_parse_stmt_list_create();
	if (!list) return NULL;

	if (!ofc_parse_file_include(
		src, list, debug))
	{
		ofc_parse_debug_print(debug);
		ofc_parse_debug_delete(debug);

		ofc_parse_stmt_list_delete(list);
		return NULL;
	}

	ofc_parse_debug_print(debug);
	ofc_parse_debug_delete(debug);

	if (!list) return NULL;


	ofc_parse_file_t* file
		= (ofc_parse_file_t*)malloc(
			sizeof(ofc_parse_file_t));
	if (!file)
	{
		ofc_parse_stmt_list_delete(list);
		return NULL;
	}

	file->source = src;
	file->stmt   = list;
	return file;
}

void ofc_parse_file_delete(ofc_parse_file_t* file)
{
	if (!file)
		return;

	ofc_parse_stmt_list_delete(file->stmt);
	ofc_sparse_delete(file->source);
	free(file);
}

bool ofc_parse_file_print(
	ofc_colstr_t* cs,
	const ofc_parse_file_t* file)
{
	if (!file) return false;
	return (ofc_parse_stmt_list_print(cs, 0, file->stmt)
		&& ofc_colstr_writef(cs, "\n"));
}
