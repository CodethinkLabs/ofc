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


unsigned ofc_parse_stmt_decl(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i;
	stmt->decl.type = ofc_parse_type(
		src, ptr, debug, &i);
	if (!stmt->decl.type)
		return 0;
	stmt->decl.save = false;
	stmt->decl.parameter  = false;
	stmt->decl.is_public  = false;
	stmt->decl.is_private = false;
	stmt->decl.dimension  = NULL;

	unsigned l;
	if (stmt->decl.type->type
		== OFC_PARSE_TYPE_RECORD)
	{
		stmt->decl.decl = ofc_parse_decl_list_record(
			src, &ptr[i], debug, &l);
	}
	else
	{
		bool is_f90 = false;

		while (ptr[i] == ',')
		{
			l = ofc_parse_keyword(
				src, &ptr[i + 1], debug,
				OFC_PARSE_KEYWORD_SAVE);
			if (l == 0) l = ofc_parse_keyword(
				src, &ptr[i + 1], debug,
				OFC_PARSE_KEYWORD_STATIC);
			if (l > 0)
			{
				i += (1 + l);
				stmt->decl.save = true;
				is_f90 = true;
				continue;
			}

			l = ofc_parse_keyword(
				src, &ptr[i + 1], debug,
				OFC_PARSE_KEYWORD_PARAMETER);
			if (l > 0)
			{
				i += (1 + l);
				stmt->decl.parameter = true;
				is_f90 = true;
				continue;
			}

			if (l == 0) l = ofc_parse_keyword(
				src, &ptr[i + 1], debug,
				OFC_PARSE_KEYWORD_PUBLIC);
			if (l > 0)
			{
				i += (1 + l);
				stmt->decl.is_public = true;
				is_f90 = true;
				continue;
			}

			if (l == 0) l = ofc_parse_keyword(
				src, &ptr[i + 1], debug,
				OFC_PARSE_KEYWORD_PRIVATE);
			if (l > 0)
			{
				i += (1 + l);
				stmt->decl.is_private = true;
				is_f90 = true;
				continue;
			}

			l = ofc_parse_keyword(
				src, &ptr[i + 1], debug,
				OFC_PARSE_KEYWORD_DIMENSION);
			if (l > 0)
			{
				i += (1 + l);

				if (stmt->decl.dimension)
				{
					ofc_parse_type_delete(stmt->decl.type);
					ofc_parse_debug_rewind(debug, dpos);
					return 0;
				}

				stmt->decl.dimension = ofc_parse_array_index(
					src, &ptr[i], debug, &l);
				if (!stmt->decl.dimension)
				{
					ofc_parse_type_delete(stmt->decl.type);
					ofc_parse_debug_rewind(debug, dpos);
					return 0;
				}
				i += l;
				is_f90 = true;
				continue;
			}

			ofc_parse_type_delete(stmt->decl.type);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}

		if ((ptr[i + 0] == ':')
			&& (ptr[i + 1] == ':'))
		{
			is_f90 = true;
			i += 2;
		}
		else if (is_f90)
		{
			ofc_parse_type_delete(stmt->decl.type);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}

		stmt->decl.decl = ofc_parse_decl_list(
			src, &ptr[i], is_f90, debug, &l);
	}

	if (!stmt->decl.decl)
	{
		ofc_parse_type_delete(stmt->decl.type);
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	stmt->type = OFC_PARSE_STMT_DECL;
	return i;
}

bool ofc_parse_stmt_decl_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt) return false;

	if (!ofc_parse_type_print(cs, stmt->decl.type, true))
		return false;

	bool is_f90 = (stmt->decl.save
		|| stmt->decl.parameter
		|| stmt->decl.dimension
		|| stmt->decl.is_public
		|| stmt->decl.is_private);
	if (is_f90)
	{
		if (stmt->decl.save)
		{
			if (!ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_keyword_atomic_writef(cs, "SAVE"))
				return false;
		}

		if (stmt->decl.parameter)
		{
			if (!ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_keyword_atomic_writef(cs, "PARAMETER"))
				return false;
		}

		if (stmt->decl.is_public)
		{
			if (!ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_keyword_atomic_writef(cs, "PUBLIC"))
				return false;
		}

		if (stmt->decl.is_private)
		{
			if (!ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_keyword_atomic_writef(cs, "PRIVATE"))
				return false;
		}

		if (stmt->decl.dimension)
		{
			if (!ofc_colstr_atomic_writef(cs, ",")
				|| !ofc_colstr_atomic_writef(cs, " ")
				|| !ofc_colstr_keyword_atomic_writef(cs, "DIMENSION")
				|| !ofc_parse_array_index_print(
					cs, stmt->decl.dimension, true))
				return false;
		}

		if (!ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_colstr_atomic_writef(cs, "::"))
			return false;
	}

	return (ofc_colstr_atomic_writef(cs, " ")
		&& ofc_parse_decl_list_print(cs, stmt->decl.decl));
}
