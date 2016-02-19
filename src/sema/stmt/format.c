/* Copyright 2016 Codethink Ltd.
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

#include "ofc/sema.h"

ofc_sema_stmt_t* ofc_sema_stmt_io_format(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_FORMAT))
		return NULL;

	if (stmt->label == 0)
	{
		ofc_sparse_ref_warning(stmt->src,
			"FORMAT statement without a label has no effect");
	}

	if (stmt->format)
	{
		unsigned i;
		for (i = 0; i < stmt->format->count; i++)
		{
			if (!ofc_sema_format_desc(
				stmt->format->desc[i]))
				return false;
		}
	}

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IO_FORMAT;
	s.io_format.src = stmt->format;
	s.io_format.format = NULL;

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as) return NULL;
	return as;
}

bool ofc_sema_stmt_io_format_print(
	ofc_colstr_t* cs,
	ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt
		|| (stmt->type != OFC_SEMA_STMT_IO_FORMAT))
		return false;

	if (!ofc_colstr_atomic_writef(cs, "FORMAT")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "("))
		return false;

	if (stmt->io_format.format)
	{
		if (!ofc_parse_format_desc_list_print(
			cs, stmt->io_format.format))
			return false;
	}
	else if (stmt->io_format.src)
	{
		if (!ofc_parse_format_desc_list_print(
			cs, stmt->io_format.src))
			return false;
	}

	return ofc_colstr_atomic_writef(cs, ")");
}
