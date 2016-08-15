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

#include <math.h>

#include "ofc/sema.h"

ofc_sema_stmt_t* ofc_sema_stmt_io_print(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| ((stmt->type != OFC_PARSE_STMT_IO_PRINT)
			&& (stmt->type != OFC_PARSE_STMT_IO_TYPE)))
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IO_PRINT;
	s.io_print.format_asterisk
		= stmt->io_print.format_asterisk;
	s.io_print.format = NULL;
	s.io_print.iolist = NULL;

	/* Check format */
	if (!s.io_print.format_asterisk)
	{
		s.io_print.format = ofc_sema_expr(
			scope, stmt->io_print.format);
		if (!s.io_print.format)
			return NULL;

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_print.format);
		if (!etype) return NULL;

		if (ofc_sema_type_is_integer(etype))
		{
			s.io_print.format->is_label  = true;
			s.io_print.format->is_format = true;
		}
		else if (etype->type != OFC_SEMA_TYPE_CHARACTER)
		{
			/* TODO - Support INTEGER array formats. */

			ofc_sparse_ref_error(s.io_print.format->src,
				"Format (FMT) must be a label or character string in PRINT");
			ofc_sema_expr_delete(s.io_print.format);
			return NULL;
		}
	}

	/* Check iolist */
	if (stmt->io_print.iolist)
	{
		s.io_print.iolist = ofc_sema_expr_list_io(
			scope, stmt->io_print.iolist);
		if (!s.io_print.iolist)
		{
			ofc_sema_expr_delete(s.io_print.format);
			return NULL;
		}
	}

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.io_print.format);
		ofc_sema_expr_list_delete(s.io_print.iolist);
		return NULL;
	}

	return as;
}

bool ofc_sema_stmt_print_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt
		|| (stmt->type != OFC_SEMA_STMT_IO_PRINT))
		return false;

	if (!ofc_colstr_keyword_atomic_writef(cs, "PRINT")
		|| !ofc_colstr_atomic_writef(cs, " "))
		return false;

	if (stmt->io_print.format_asterisk)
	{
		if (!ofc_colstr_atomic_writef(cs, "*"))
			return false;
	}
	else if (stmt->io_print.format)
	{
		if (!ofc_sema_expr_print(cs, stmt->io_print.format))
			return false;
	}
	else
	{	/* No valid format option for the print statement */
		return false;
	}

	if (stmt->io_print.iolist)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_sema_expr_list_print(cs, stmt->io_print.iolist))
			return false;
	}

	return true;
}
