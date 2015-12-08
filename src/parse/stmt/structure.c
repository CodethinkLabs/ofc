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


static unsigned ofc_parse_stmt__structure(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword, bool slash,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i;
	if (slash)
	{
		i = ofc_parse_keyword(
			src, ptr, debug, keyword);
		if (i == 0) return 0;

		stmt->structure.name = OFC_STR_REF_EMPTY;

		if (keyword == OFC_PARSE_KEYWORD_STRUCTURE)
		{
			if (ptr[i] == '/')
			{
				i += 1;

				unsigned len = ofc_parse_name(
					src, &ptr[i], debug, &stmt->structure.name);
				if (len == 0)
				{
					ofc_parse_debug_rewind(debug, dpos);
					return 0;
				}
				i += len;

				if (ptr[i++] != '/')
				{
					ofc_parse_debug_rewind(debug, dpos);
					return 0;
				}
			}

			/* TODO - Parse field list. */
		}
	}
	else
	{
		i = ofc_parse_keyword_named(
			src, ptr, debug, keyword,
			&stmt->structure.name);
		if (i == 0) return 0;
	}

	unsigned len;
	if (!ofc_is_end_statement(&ptr[i], &len))
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->structure.block = ofc_parse_stmt_list(
		src, &ptr[i], debug, &len);
	if (stmt->structure.block)
	{
		if (ofc_parse_stmt_list_contains_error(
			stmt->structure.block))
		{
			/* Don't rewind cause we want to report the error. */
			ofc_parse_stmt_list_delete(
				stmt->structure.block);
			return 0;
		}

		i += len;
	}

	if (slash)
	{
		/* TODO - Handle the following?
		   END STRUCTURE /name/ */
		len = ofc_parse_keyword_end(
			src, &ptr[i], debug, keyword, false);
		if (len == 0)
		{
			ofc_sparse_error(src, &ptr[i],
				"Invalid statement in %s body",
				ofc_parse_keyword_name(keyword));

			ofc_parse_stmt_list_delete(
				stmt->structure.block);

			stmt->type = OFC_PARSE_STMT_ERROR;
			return i;
		}
	}
	else
	{
		len = ofc_parse_keyword_end_named(
			src, &ptr[i], debug, keyword, true,
			&stmt->structure.name);
		if (len == 0)
		{
			ofc_parse_stmt_list_delete(
			stmt->structure.block);
			return 0;
		}
	}
	i += len;

	/* Make sure TYPE (struct) doesn't contain TYPE (print) statement. */
	if ((keyword == OFC_PARSE_KEYWORD_TYPE)
		&& stmt->structure.block)
	{
		unsigned j;
		for (j = 0; j < stmt->structure.block->count; j++)
		{
			ofc_parse_stmt_t* s
				= stmt->structure.block->stmt[j];
			if (!s) continue;

			if (s->type == OFC_PARSE_STMT_IO_TYPE)
			{
				ofc_parse_stmt_list_delete(
					stmt->structure.block);
				return 0;
			}
		}
	}

	return i;
}


unsigned ofc_parse_stmt_type(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_TYPE;
	unsigned i = ofc_parse_stmt__structure(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_TYPE, false, stmt);
	if (i == 0) return 0;

	return i;
}

unsigned ofc_parse_stmt_structure(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_STRUCTURE;
	unsigned i = ofc_parse_stmt__structure(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_STRUCTURE, true, stmt);
	if (i == 0) return 0;

	return i;
}

unsigned ofc_parse_stmt_union(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_UNION;
	unsigned i = ofc_parse_stmt__structure(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_UNION, true, stmt);
	if (i == 0) return 0;

	return i;
}

unsigned ofc_parse_stmt_map(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_MAP;
	unsigned i = ofc_parse_stmt__structure(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_MAP, true, stmt);
	if (i == 0) return 0;

	return i;
}

unsigned ofc_parse_stmt_record(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_RECORD);
	if (i == 0) return 0;

	unsigned l;
	stmt->record = ofc_parse_record_list(
		src, &ptr[i], debug, &l);
	if (l == 0)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = OFC_PARSE_STMT_RECORD;
	return i;
}




bool ofc_parse_stmt_structure_print(
	ofc_colstr_t* cs, unsigned indent,
	const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	const char* kwstr;
	bool slash = true;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_TYPE:
			kwstr = "TYPE";
			slash = false;
			break;
		case OFC_PARSE_STMT_STRUCTURE:
			kwstr = "STRUCTURE";
			break;
		case OFC_PARSE_STMT_UNION:
			kwstr = "UNION";
			break;
		case OFC_PARSE_STMT_MAP:
			kwstr = "MAP";
			break;
		default:
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, "%s", kwstr))
		return false;

	if (!ofc_str_ref_empty(stmt->structure.name))
	{
		if (!ofc_colstr_atomic_writef(cs, " ")
			|| (slash && !ofc_colstr_atomic_writef(cs, "/"))
			|| !ofc_str_ref_print(cs, stmt->structure.name)
			|| (slash && !ofc_colstr_atomic_writef(cs, "/")))
			return false;
	}

	if (!ofc_parse_stmt_list_print(
		cs, (indent + 1), stmt->structure.block))
		return false;

	if (!ofc_colstr_newline(cs, indent, NULL))
		return false;

	unsigned j;
	for (j = 0; j < indent; j++)
	{
		if (!ofc_colstr_atomic_writef(cs, "  "))
			return false;
	}

	return ofc_colstr_atomic_writef(cs, "END %s", kwstr);
}

bool ofc_parse_stmt_record_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	return (ofc_colstr_atomic_writef(cs, "RECORD ")
		&& ofc_parse_record_list_print(cs, stmt->record));
}
