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

#include <string.h>

#include "ofc/file.h"
#include "ofc/parse/file.h"
#include "ofc/prep.h"


unsigned ofc_parse_stmt_include(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt,
	ofc_parse_stmt_list_t* list)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_INCLUDE);
	if (i == 0) return 0;

	unsigned l = 0;
	ofc_string_t* spath = ofc_parse_character(
		src, &ptr[i], debug, &l);

	if (!spath)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	if (!ofc_is_end_statement(&ptr[i], NULL))
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	/* Don't rewind debug after this point because
	   we know we have a valid include statement. */

	char path[spath->size + 1];
	memcpy(path, spath->base, spath->size);
	path[spath->size] = '\0';

	ofc_string_delete(spath);

	stmt->include.file = ofc_file_create_include(
		path, ofc_sparse_lang_opts(src), ofc_sparse_file(src), ptr);

	if (!stmt->include.file)
	{
		ofc_sparse_error(src, ofc_str_ref(ptr, i),
			"Can't open include file '%s'", path);
		return 0;
	}

	stmt->include.src = ofc_prep(stmt->include.file);

	if (!ofc_parse_file_include(
		stmt->include.src, list, debug))
	{
		ofc_sparse_delete(stmt->include.src);
		ofc_file_delete(stmt->include.file);
		return 0;
	}

	stmt->type = OFC_PARSE_STMT_INCLUDE;
	return i;
}
