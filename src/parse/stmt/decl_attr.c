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


static unsigned ofc_parse_stmt__decl_attr(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	stmt->decl_attr.count = 0;
	stmt->decl_attr.name = NULL;

	unsigned l = ofc_parse_list(
		src, &ptr[i], debug, ',',
		&stmt->decl_attr.count,
		(void***)&stmt->decl_attr.name,
		(void*)ofc_parse_name_alloc,
		free);
	if (l == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	return i;
}

unsigned ofc_parse_stmt_decl_attr_external(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__decl_attr(
		src, ptr, debug, OFC_PARSE_KEYWORD_EXTERNAL, stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_DECL_ATTR_EXTERNAL;
	return i;
}

unsigned ofc_parse_stmt_decl_attr_intrinsic(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__decl_attr(
		src, ptr, debug, OFC_PARSE_KEYWORD_INTRINSIC, stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_DECL_ATTR_INTRINSIC;
	return i;
}

unsigned ofc_parse_stmt_decl_attr_automatic(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__decl_attr(
		src, ptr, debug, OFC_PARSE_KEYWORD_AUTOMATIC, stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC;
	return i;
}

unsigned ofc_parse_stmt_decl_attr_static(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__decl_attr(
		src, ptr, debug, OFC_PARSE_KEYWORD_STATIC, stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_DECL_ATTR_STATIC;
	return i;
}

unsigned ofc_parse_stmt_decl_attr_volatile(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__decl_attr(
		src, ptr, debug, OFC_PARSE_KEYWORD_VOLATILE, stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_DECL_ATTR_VOLATILE;
	return i;
}


bool ofc_parse_stmt_decl_attr_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_DECL_ATTR_EXTERNAL:
			kwstr = "EXTERNAL";
			break;
		case OFC_PARSE_STMT_DECL_ATTR_INTRINSIC:
			kwstr = "INTRINSIC";
			break;
		case OFC_PARSE_STMT_DECL_ATTR_AUTOMATIC:
			kwstr = "AUTOMATIC";
			break;
		case OFC_PARSE_STMT_DECL_ATTR_STATIC:
			kwstr = "STATIC";
			break;
		case OFC_PARSE_STMT_DECL_ATTR_VOLATILE:
			kwstr = "VOLATILE";
			break;
		default:
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, "%s", kwstr))
		return false;

	unsigned i;
	for (i = 0; i < stmt->decl_attr.count; i++)
	{
		if (!ofc_colstr_atomic_writef(cs, "%s",
			(i == 0 ? " " : ", ")))
			return false;

		if (!ofc_sparse_ref_print(cs,
			*(stmt->decl_attr.name[i])))
			return false;
	}

	return true;
}
