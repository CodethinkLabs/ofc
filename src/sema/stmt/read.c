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

void ofc_sema_stmt_io_read__cleanup(
	ofc_sema_stmt_t s)
{
	ofc_sema_expr_delete(s.io_read.unit);
	ofc_sema_expr_delete(s.io_read.format_expr);
	ofc_sema_expr_delete(s.io_read.advance);
	ofc_sema_expr_delete(s.io_read.end);
	ofc_sema_expr_delete(s.io_read.eor);
	ofc_sema_expr_delete(s.io_read.err);
	ofc_sema_expr_delete(s.io_read.iostat);
	ofc_sema_expr_delete(s.io_read.rec);
	ofc_sema_expr_delete(s.io_read.size);
}

ofc_sema_stmt_t* ofc_sema_stmt_io_read(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_IO_READ)
		|| !stmt->io_read.params)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IO_READ;
	s.io_read.unit         = NULL;
	s.io_read.stdin        = false;
	s.io_read.format_expr  = NULL;
	s.io_read.format       = NULL;
	s.io_read.format_ldio  = false;
	s.io_read.formatted    = false;
	s.io_read.iostat       = NULL;
	s.io_read.rec          = NULL;
	s.io_read.err          = NULL;
	s.io_read.iolist       = NULL;
	s.io_read.advance      = NULL;
	s.io_read.is_advancing = true;
	s.io_read.end          = NULL;
	s.io_read.eor          = NULL;
	s.io_read.size         = NULL;

	ofc_parse_call_arg_t* ca_unit    = NULL;
	ofc_parse_call_arg_t* ca_format  = NULL;
	ofc_parse_call_arg_t* ca_iostat  = NULL;
	ofc_parse_call_arg_t* ca_rec     = NULL;
	ofc_parse_call_arg_t* ca_err     = NULL;
	ofc_parse_call_arg_t* ca_advance = NULL;
	ofc_parse_call_arg_t* ca_end     = NULL;
	ofc_parse_call_arg_t* ca_eor     = NULL;
	ofc_parse_call_arg_t* ca_size    = NULL;

	if (stmt->io_read.has_brakets)
	{
		unsigned i;
		for (i = 0; i < stmt->io_read.params->count; i++)
		{
			ofc_parse_call_arg_t* param
				= stmt->io_read.params->call_arg[i];
			if (!param) continue;

			if (ofc_sparse_ref_empty(param->name))
			{
				if (i >= 2)
				{
					ofc_sparse_ref_error(param->src,
						"Un-named parameter %u has no meaning in READ.", i);
					return NULL;
				}

				if (i == 0)
				{
					ca_unit = param;
				}
				else
				{
					if (!ca_unit)
					{
						ofc_sparse_ref_error(param->src,
							"Un-named format parameter only valid after UNIT in READ.");
						return NULL;
					}

					ca_format = param;
				}
			}
			else if (ofc_str_ref_equal_strz_ci(param->name.string, "UNIT"))
			{
				if (ca_unit)
				{
					ofc_sparse_ref_error(param->src,
						"Re-definition of UNIT in READ.");
					return NULL;
				}

				ca_unit = param;
			}
			else if (ofc_str_ref_equal_strz_ci(param->name.string, "FMT"))
			{
				if (ca_format)
				{
					ofc_sparse_ref_error(param->src,
						"Re-definition of FMT in READ.");
					return NULL;
				}

				ca_format = param;
			}
			else if (ofc_str_ref_equal_strz_ci(param->name.string, "IOSTAT"))
			{
				if (ca_iostat)
				{
					ofc_sparse_ref_error(param->src,
						"Re-definition of IOSTAT in READ.");
					return NULL;
				}

				ca_iostat = param;
			}
			else if (ofc_str_ref_equal_strz_ci(param->name.string, "REC"))
			{
				if (ca_rec)
				{
					ofc_sparse_ref_error(param->src,
						"Re-definition of REC in READ.");
					return NULL;
				}

				ca_rec = param;
			}
			else if (ofc_str_ref_equal_strz_ci(param->name.string, "ERR"))
			{
				if (ca_err)
				{
					ofc_sparse_ref_error(param->src,
						"Re-definition of ERR in READ.");
					return NULL;
				}

				ca_err = param;
			}
			else if (ofc_str_ref_equal_strz_ci(param->name.string, "ADVANCE"))
			{
				if (ca_advance)
				{
					ofc_sparse_ref_error(param->src,
						"Re-definition of ADVANCE in READ.");
					return NULL;
				}

				ca_advance = param;
			}
			else if (ofc_str_ref_equal_strz_ci(param->name.string, "END"))
			{
				if (ca_end)
				{
					ofc_sparse_ref_error(param->src,
						"Re-definition of END in READ.");
					return NULL;
				}

				ca_end = param;
			}
			else if (ofc_str_ref_equal_strz_ci(param->name.string, "SIZE"))
			{
				if (ca_size)
				{
					ofc_sparse_ref_error(param->src,
						"Re-definition of SIZE in READ.");
					return NULL;
				}

				ca_size = param;
			}
			else
			{
				ofc_sparse_ref_error(param->src,
					"Unrecognized paramater %u name '%.*s' in READ.",
					i, param->name.string.size, param->name.string.base);
				return NULL;
			}
		}

		if (!ca_unit)
		{
			ofc_sparse_ref_error(stmt->src,
				"No UNIT defined in READ.");
			return NULL;
		}
	}
	else
	{
		ca_format = stmt->io_read.params->call_arg[0];
	}

	if (ca_unit && (ca_unit->type == OFC_PARSE_CALL_ARG_ASTERISK))
	{
		s.io_read.stdin = true;
	}
	else if (ca_unit && (ca_unit->type == OFC_PARSE_CALL_ARG_EXPR))
	{
		s.io_read.unit = ofc_sema_expr(
			scope, ca_unit->expr);
		if (!s.io_read.unit) return NULL;

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_read.unit);
		if (!etype)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_character(etype)
			&& (!ofc_sema_type_is_integer(etype)
				|| !ofc_sema_expr_validate_uint(s.io_read.unit)))
		{
			ofc_sparse_ref_error(stmt->src,
				   "UNIT must be a positive INTEGER "
				   "or a CHARACTER expression in READ");
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}
	}
	else if (ca_unit)
	{
		ofc_sparse_ref_error(stmt->src,
			"UNIT must be an INTEGER or CHARACTER "
			"expression, or asterisk in READ");
		return NULL;
	}
	else
	{
		s.io_read.stdin = true;
	}

	if (ca_format && (ca_format->type == OFC_PARSE_CALL_ARG_ASTERISK))
	{
		s.io_read.format_ldio = true;
		s.io_read.formatted   = true;
	}
	else if (ca_format&& (ca_format->type == OFC_PARSE_CALL_ARG_EXPR))
	{
		s.io_read.formatted   = true;

		s.io_read.format_expr = ofc_sema_expr(
			scope, ca_format->expr);
		if (!s.io_read.format_expr)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_read.format_expr);
		if (!etype)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		if (ofc_sema_type_is_integer(etype))
		{
			const ofc_sema_label_t* label;
			if (!ofc_sema_io_check_label(
				scope, stmt, true,
				s.io_read.format_expr, &label))
			{
				ofc_sema_stmt_io_read__cleanup(s);
				return NULL;
			}

			if (label && label->type != OFC_SEMA_LABEL_FORMAT)
			{
				ofc_sparse_ref_error(stmt->src,
					"Label expression must be a FORMAT statement in READ");
				ofc_sema_stmt_io_read__cleanup(s);
				return NULL;
			}
			if(label)
				s.io_read.format = label->format;
		}
		else if (etype->type == OFC_SEMA_TYPE_CHARACTER)
		{
			/* TODO - Check we can resolve this as a format descriptor. */
		}
		else
		{
			/* TODO - Support INTEGER array formats. */

			ofc_sparse_ref_error(stmt->src,
				"Format (FMT) must be a label or character string in READ");
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}
	}
	else if (ca_format)
	{
		ofc_sparse_ref_error(stmt->src,
			"Format (FMT) must be an INTEGER expression or asterisk in READ");
		ofc_sema_stmt_io_read__cleanup(s);
		return NULL;
	}

	if (ca_advance && s.io_read.stdin)
	{
		ofc_sparse_ref_error(stmt->src,
			"ADVANCE specifier can only be used with an external UNIT in READ");
		ofc_sema_stmt_io_read__cleanup(s);
		return NULL;
	}
	else if (ca_advance && (!ca_format || s.io_read.format_ldio))
	{
		ofc_sparse_ref_error(stmt->src,
			"ADVANCE specifier can only be used with a formatted input in READ");
		ofc_sema_stmt_io_read__cleanup(s);
		return NULL;
	}
	else if (ca_advance)
	{
		s.io_read.advance = ofc_sema_expr(
			scope, ca_advance->expr);
		if (!s.io_read.advance)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_read.advance);
		if (!etype)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		if (etype->type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sparse_ref_error(stmt->src,
				"ADVANCE must be a CHARACTER expression in READ");
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_read.advance);

			const char* advance_str;
			if (!ofc_sema_typeval_get_character(constant, &advance_str))
			{
				ofc_sema_stmt_io_read__cleanup(s);
				return NULL;
			}

			if (strcasecmp(advance_str, "NO") == 0)
			{
				s.io_read.is_advancing = false;
			}
			else if (strcasecmp(advance_str, "YES") != 0)
			{
				ofc_sparse_ref_error(stmt->src,
					"ADVANCE must be 'YES' or 'NO' in READ");
				ofc_sema_stmt_io_read__cleanup(s);
				return NULL;
			}
		}
	}

	if (ca_end)
	{
		s.io_read.end = ofc_sema_expr(
			scope, ca_end->expr);
		if (!s.io_read.end)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_io_check_label(
			scope, stmt, false,
			s.io_read.end, NULL))
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}
	}

	if (ca_eor)
	{
		s.io_read.eor = ofc_sema_expr(
			scope, ca_eor->expr);
		if (!s.io_read.eor)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_io_check_label(
			scope, stmt, false,
			s.io_read.eor, NULL))
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}
	}

	if (ca_err)
	{
		s.io_read.err = ofc_sema_expr(
			scope, ca_err->expr);
		if (!s.io_read.err)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_io_check_label(
			scope, stmt, false,
			s.io_read.err, NULL))
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}
	}

	if (ca_iostat)
	{
		s.io_read.iostat = ofc_sema_expr(
			scope, ca_iostat->expr);
		if (!s.io_read.iostat)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		if (s.io_read.iostat->type != OFC_SEMA_EXPR_LHS)
		{
			ofc_sparse_ref_error(stmt->src,
				"IOSTAT must be a variable in READ");
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_read.iostat);
		if (!etype)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sparse_ref_error(stmt->src,
				"IOSTAT must be of type INTEGER in READ");
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}
	}

	if (ca_rec && (s.io_read.format_ldio || ca_end))
	{
		ofc_sparse_ref_error(stmt->src,
			"REC specifier not compatible with END,"
			" NML or list-directed data transfer in READ");
		ofc_sema_stmt_io_read__cleanup(s);
		return NULL;
	}
	else if (ca_rec)
	{
		s.io_read.rec = ofc_sema_expr(
			scope, ca_rec->expr);
		if (!s.io_read.rec)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_read.rec);
		if (!etype)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sparse_ref_error(stmt->src,
				"REC must be of type INTEGER in READ");
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}
	}

	if (ca_size && s.io_read.is_advancing)
	{
		ofc_sparse_ref_error(stmt->src,
			"SIZE not compatible with advancing formatted "
			"sequential data transfer in READ");
		ofc_sema_stmt_io_read__cleanup(s);
		return NULL;
	}
	else if (ca_size)
	{
		if (s.io_read.size->type != OFC_SEMA_EXPR_LHS)
		{
			ofc_sparse_ref_error(stmt->src,
				"SIZE must be a variable in READ");
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}
		/* TODO - The variable specified in SIZE must
				  not be the same as or associated with any
				  entity in the input/output item list or in
				  the namelist group or with the variable
				  specified in the IOSTAT= specifier */
		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_read.size);
		if (!etype)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sparse_ref_error(stmt->src,
				"SIZE must be of type INTEGER in READ");
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}
	}

	/* Check iolist */
	if (stmt->io_read.iolist)
	{
		bool is_dynamic = false;
		s.io_read.iolist
			= ofc_sema_input_list(
				scope, stmt->io_read.iolist, &is_dynamic);
		if (!s.io_read.iolist && !is_dynamic)
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}
	}

	if (s.io_read.format && s.io_read.iolist)
	{
		/* Count elements in iolist */
		unsigned iolist_len = 0;
		if (s.io_read.iolist
			&& !ofc_sema_lhs_list_elem_count(
				s.io_read.iolist, &iolist_len))
		{
			ofc_sema_stmt_io_read__cleanup(s);
			return NULL;
		}

		if (iolist_len > 0)
		{
			unsigned count = 0;
			if (!ofc_sema_io_list_has_complex(
				s.io_read.iolist, NULL, &count))
			{
				ofc_sema_stmt_io_read__cleanup(s);
				return NULL;
			}
			iolist_len += count;
		}

		unsigned data_desc_count
			= ofc_sema_io_data_format_count(s.io_read.format);

		if ((data_desc_count > 0) && (iolist_len > 0))
		{
			if (iolist_len < data_desc_count)
			{
				ofc_sparse_ref_warning(stmt->src,
					"IO list shorter than FORMAT list,"
					" last FORMAT data descriptors will be ignored");
			}
			else if (fmod(iolist_len, data_desc_count) != 0)
			{
				ofc_sparse_ref_warning(stmt->src,
					"IO list length is not a multiple of FORMAT list length");
			}

			/* Create a format list of same length as iolist
			 * with only data edit descriptors  */
			ofc_parse_format_desc_list_t* format_list
				= ofc_sema_io_data_format(s.io_read.format, iolist_len);
			if (!format_list)
			{
				ofc_sema_stmt_io_read__cleanup(s);
				return NULL;
			}

			/* Compare iolist with format */
			bool fail = !ofc_sema_io_format_input_list_compare(
				scope, stmt, format_list, s.io_read.iolist);
			ofc_parse_format_desc_list_delete(format_list);
			if (fail)
			{
				ofc_sema_stmt_io_read__cleanup(s);
				return NULL;
			}
		}
		else if (iolist_len > 0)
		{
			ofc_sparse_ref_warning(stmt->src,
				"No data edit descriptors in FORMAT list");
		}
		else if (data_desc_count > 0)
		{
			ofc_sparse_ref_warning(stmt->src,
				"No IO list in READ statement");
		}

	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_stmt_io_read__cleanup(s);
		return NULL;
	}
	return as;
}

static bool ofc_sema_stmt_read__print_optional(
	ofc_colstr_t* cs, const char* name,
	const ofc_sema_expr_t* expr)
{
	if (!cs || !expr)
		return false;

	if (!ofc_colstr_atomic_writef(cs, ",")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, name)
		|| !ofc_colstr_atomic_writef(cs, "= ")
		|| !ofc_sema_expr_print(cs, expr))
		return false;

	return true;
}

bool ofc_sema_stmt_read_print(ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || (stmt->type != OFC_SEMA_STMT_IO_READ))
		return false;

	if (!ofc_colstr_atomic_writef(cs, "READ")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "("))
		return false;

	if (stmt->io_read.stdin)
	{
		if (!ofc_colstr_atomic_writef(cs, "*"))
			return false;
	}
	else
	{
		if (!ofc_sema_expr_print(cs,
			stmt->io_read.unit))
			return false;
	}

	if (stmt->io_read.format_ldio)
	{
		if (!ofc_colstr_atomic_writef(cs, ",")
			|| !ofc_colstr_atomic_writef(cs, " "))
			return false;
		if (!ofc_colstr_atomic_writef(cs, "*"))
			return false;
	}
	else if (stmt->io_read.formatted)
	{
		if (!ofc_sema_stmt_read__print_optional(
			cs,	"FMT", stmt->io_read.format_expr))
			return false;
	}

	if (stmt->io_read.iostat)
	{
		if (!ofc_sema_stmt_read__print_optional(
			cs, "IOSTAT", stmt->io_read.iostat))
			return false;
	}
	if (stmt->io_read.rec)
	{
		if (!ofc_sema_stmt_read__print_optional(
			cs, "REC", stmt->io_read.rec))
			return false;
	}
	if (stmt->io_read.err)
	{
		if (!ofc_sema_stmt_read__print_optional(
			cs,	"ERR", stmt->io_read.err))
			return false;
	}
	if (stmt->io_read.eor)
	{
		if (!ofc_sema_stmt_read__print_optional(
			cs,	"EOR", stmt->io_read.eor))
			return false;
	}
	if (stmt->io_read.end)
	{
		if (!ofc_sema_stmt_read__print_optional(
			cs,	"END", stmt->io_read.end))
			return false;
	}
	if (stmt->io_read.size)
	{
		if (!ofc_sema_stmt_read__print_optional(
			cs,	"SIZE", stmt->io_read.size))
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, ")"))
		return false;


	if (stmt->io_read.iolist)
	{
		if (!ofc_colstr_atomic_writef(cs, " ")
			|| !ofc_sema_lhs_list_print(cs,
				stmt->io_read.iolist))
			return false;
	}

	return true;
}
