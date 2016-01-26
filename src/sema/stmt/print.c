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
	s.io_print.format_expr = NULL;
	s.io_print.iolist = NULL;
	s.io_print.format = NULL;

	/* Check format */
	if (!s.io_print.format_asterisk)
	{
		s.io_print.format_expr = ofc_sema_expr(
			scope, stmt->io_print.format);
		if (!s.io_print.format_expr)
			return NULL;

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_print.format_expr);
		if (!etype) return NULL;

		if (ofc_sema_type_is_integer(etype))
		{

			const ofc_sema_typeval_t* format_label
				= ofc_sema_expr_constant(s.io_print.format_expr);
			if (format_label)
			{
				int64_t fl64 = 0;
				if (!ofc_sema_typeval_get_integer(
					format_label, &fl64) || (fl64 < 0))
				{
					ofc_sparse_ref_error(stmt->io_print.format->src,
						"Format label expression must be a positive INTEGER in PRINT");
					ofc_sema_expr_delete(s.io_print.format_expr);
					return NULL;
				}

				unsigned ulabel = (unsigned) fl64;

				if (((int64_t) ulabel) != fl64)
				{
					ofc_sema_expr_delete(s.io_print.format_expr);
					return NULL;
				}

				const ofc_sema_label_t* label
					= ofc_sema_label_map_find(
						scope->label, ulabel);
				if (!label)
				{
					ofc_sparse_ref_error(stmt->io_print.format->src,
						"Format label expression not defined in PRINT");
					ofc_sema_expr_delete(s.io_print.format_expr);
					return NULL;
				}

				if (label->type != OFC_SEMA_LABEL_FORMAT)
				{
					ofc_sparse_ref_error(stmt->io_print.format->src,
						"Label expression must be a FORMAT statement in PRINT");
					ofc_sema_expr_delete(s.io_print.format_expr);
					return NULL;
				}
				s.io_print.format = label->format;
			}
		}
		else if (etype->type == OFC_SEMA_TYPE_CHARACTER)
		{
			/* TODO - Check we can resolve this as a format descriptor. */
		}
		else
		{
			/* TODO - Support INTEGER array formats. */

			ofc_sparse_ref_error(stmt->src,
				"Format (FMT) must be a label or character string in PRINT");
			ofc_sema_expr_delete(s.io_print.format_expr);
			return NULL;
		}
	}

	/* Check iolist */
	if (stmt->io_print.iolist)
	{
		s.io_print.iolist
			= ofc_sema_expr_list_io(
				scope, stmt->io_print.iolist);
		if (!s.io_print.iolist)
		{
			ofc_sema_expr_delete(s.io_print.format_expr);
			return NULL;
		}
	}

	/* Count elements in iolist */
	unsigned iolist_len = 0;
	if (s.io_print.iolist
		&& !ofc_sema_expr_list_elem_count(
			s.io_print.iolist, &iolist_len))
	{
		ofc_sema_expr_delete(s.io_print.format_expr);
		ofc_sema_expr_list_delete(s.io_print.iolist);
		return NULL;
	}

	if (iolist_len > 0)
	{
		unsigned count = 0;
		if (!ofc_sema_io_list_has_complex(
			NULL, s.io_print.iolist, &count))
		{
			ofc_sema_expr_delete(s.io_print.format_expr);
			ofc_sema_expr_list_delete(s.io_print.iolist);
			return NULL;
		}
		iolist_len += count;
	}

	if (s.io_print.format)
	{
		unsigned data_desc_count
			= ofc_sema_io_data_format_count(s.io_print.format);

		if ((data_desc_count > 0) && (iolist_len > 0))
		{
			if (iolist_len < data_desc_count)
			{
				ofc_sparse_ref_warning(stmt->io_print.format->src,
					"IO list shorter than FORMAT list,"
					" last FORMAT data descriptors will be ignored");
			}
			else if ((iolist_len % data_desc_count) != 0)
			{
				ofc_sparse_ref_warning(stmt->io_print.format->src,
					"IO list length is not a multiple of FORMAT list length");
			}

			/* Create a format list of same length as iolist
			 * with only data edit descriptors  */
			ofc_parse_format_desc_list_t* format_list
				= ofc_sema_io_data_format(s.io_print.format, iolist_len);
			if (!format_list)
			{
				ofc_sema_expr_delete(s.io_print.format_expr);
				ofc_sema_expr_list_delete(s.io_print.iolist);
				return NULL;
			}

			/* Compare iolist with format */
			bool fail = !ofc_sema_io_format_iolist_compare(
				scope, stmt, format_list, s.io_print.iolist);
			ofc_parse_format_desc_list_delete(format_list);
			if (fail)
			{
				ofc_sema_expr_delete(s.io_print.format_expr);
				ofc_sema_expr_list_delete(s.io_print.iolist);
				return NULL;
			}
		}
		else if (iolist_len > 0)
		{
			ofc_sparse_ref_warning(stmt->io_print.format->src,
				"No data edit descriptors in FORMAT list");
		}
		else if (data_desc_count > 0)
		{
			ofc_sparse_ref_warning(stmt->io_print.format->src,
				"No IO list in PRINT statement");
		}
	}

	ofc_sema_stmt_t* as = ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_expr_delete(s.io_print.format_expr);
		ofc_sema_expr_list_delete(s.io_print.iolist);
		return NULL;
	}

	return as;
}

bool ofc_sema_stmt_print_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt || stmt->type != OFC_SEMA_STMT_IO_PRINT)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "PRINT")
		|| !ofc_colstr_atomic_writef(cs, " "))
		return false;

	if (stmt->io_print.format_asterisk)
	{
		if (!ofc_colstr_atomic_writef(cs, "*"))
			return false;
	}
	else if (stmt->io_print.format)
	{
        if (!ofc_sema_format_print(cs, stmt->io_print.format))
			return false;
	}
	else if (stmt->io_print.format_expr)
	{
		if (!ofc_sema_expr_print(cs, stmt->io_print.format_expr))
			return false;
	}
	else
	{	/* No valid format option for the print statement */
		return false;
	}

	if (!ofc_colstr_atomic_writef(cs, ",")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_sema_expr_list_print(cs, stmt->io_print.iolist))
		return false;

	return true;
}
