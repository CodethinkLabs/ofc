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

#include "ofc/sema.h"

void ofc_sema_stmt_io_close__cleanup(
	ofc_sema_stmt_t s)
{
	ofc_sema_expr_delete(s.io_close.unit);
	ofc_sema_expr_delete(s.io_close.err);
	ofc_sema_expr_delete(s.io_close.iostat);
	ofc_sema_expr_delete(s.io_close.status);
}

ofc_sema_stmt_t* ofc_sema_stmt_io_close(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_IO_CLOSE)
		|| !stmt->io.params)
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IO_CLOSE;
	s.io_close.unit   = NULL;
	s.io_close.iostat = NULL;
	s.io_close.err    = NULL;
	s.io_close.status = NULL;

	ofc_parse_call_arg_t* ca_unit   = NULL;
	ofc_parse_call_arg_t* ca_iostat = NULL;
	ofc_parse_call_arg_t* ca_err    = NULL;
	ofc_parse_call_arg_t* ca_status = NULL;

	unsigned i;
	for (i = 0; i < stmt->io.params->count; i++)
	{
		ofc_parse_call_arg_t* param
			= stmt->io.params->call_arg[i];
		if (!param) continue;

		if (ofc_sparse_ref_empty(param->name))
		{
			if (i >= 1)
			{
				ofc_sparse_ref_error(param->src,
					"Un-named parameter %u has no meaning in CLOSE.", i);
				return NULL;
			}

			if (i == 0)
			{
				ca_unit = param;
			}
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "UNIT"))
		{
			if (ca_unit)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of UNIT in CLOSE.");
				return NULL;
			}

			ca_unit = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "IOSTAT"))
		{
			if (ca_iostat)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of IOSTAT in CLOSE.");
				return NULL;
			}

			ca_iostat = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "ERR"))
		{
			if (ca_err)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of ERR in CLOSE.");
				return NULL;
			}

			ca_err = param;
		}
		else if (ofc_str_ref_equal_strz_ci(param->name.string, "STATUS"))
		{
			if (ca_status)
			{
				ofc_sparse_ref_error(param->src,
					"Re-definition of STATUS in CLOSE.");
				return NULL;
			}

			ca_status = param;
		}
		else
		{
			ofc_sparse_ref_error(param->src,
				"Unrecognized paramater %u name '%.*s' in CLOSE.",
				i, param->name.string.size, param->name.string.base);
			return NULL;
		}
	}

	if (!ca_unit)
	{
		ofc_sparse_ref_error(stmt->src,
			"No UNIT defined in CLOSE.");
		return NULL;
	}

	if (ca_unit->type == OFC_PARSE_CALL_ARG_EXPR)
	{
		s.io_close.unit = ofc_sema_expr(
			scope, ca_unit->expr);
		if (!s.io_close.unit) return NULL;

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_close.unit);
		if (!etype)
		{
			ofc_sema_stmt_io_close__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sparse_ref_error(stmt->src,
				"UNIT must be of type INTEGER in CLOSE");
			ofc_sema_stmt_io_close__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_expr_validate_uint(s.io_close.unit))
		{
			ofc_sparse_ref_error(stmt->src,
				"UNIT must be a positive INTEGER in CLOSE");
			ofc_sema_stmt_io_close__cleanup(s);
			return NULL;
		}
	}
	else
	{
		ofc_sparse_ref_error(stmt->src,
			"UNIT must be an INTEGER expression in CLOSE");
		return NULL;
	}

	if (ca_iostat)
	{
		s.io_close.iostat = ofc_sema_expr(
			scope, ca_iostat->expr);
		if (!s.io_close.iostat)
		{
			ofc_sema_stmt_io_close__cleanup(s);
			return NULL;
		}

		if (s.io_close.iostat->type != OFC_SEMA_EXPR_LHS)
		{
			ofc_sparse_ref_error(stmt->src,
				"IOSTAT must be of a variable in CLOSE");
			ofc_sema_stmt_io_close__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_close.iostat);
		if (!etype)
		{
			ofc_sema_stmt_io_close__cleanup(s);
			return NULL;
		}

		if (!ofc_sema_type_is_integer(etype))
		{
			ofc_sparse_ref_error(stmt->src,
				"IOSTAT must be of type INTEGER in CLOSE");
			ofc_sema_stmt_io_close__cleanup(s);
			return NULL;
		}

	}

	if (ca_err)
	{
		s.io_close.err = ofc_sema_expr_label(
			scope, ca_err->expr);
		if (!s.io_close.err)
		{
			ofc_sema_stmt_io_close__cleanup(s);
			return NULL;
		}
	}

	if (ca_status)
	{
		s.io_close.status = ofc_sema_expr(
			scope, ca_status->expr);
		if (!s.io_close.status)
		{
			ofc_sema_stmt_io_close__cleanup(s);
			return NULL;
		}

		const ofc_sema_type_t* etype
			= ofc_sema_expr_type(s.io_close.status);
		if (!etype)
		{
			ofc_sema_stmt_io_close__cleanup(s);
			return NULL;
		}

		if (etype->type != OFC_SEMA_TYPE_CHARACTER)
		{
			ofc_sparse_ref_error(stmt->src,
				"STATUS must be a CHARACTER expression in CLOSE");
			ofc_sema_stmt_io_close__cleanup(s);
			return NULL;
		}
		else
		{
			const ofc_sema_typeval_t* constant
				= ofc_sema_expr_constant(s.io_close.status);

			if (constant
				&& !ofc_typeval_character_equal_strz_ci(constant, "DELETE")
				&& !ofc_typeval_character_equal_strz_ci(constant, "KEEP"))
			{
				ofc_sparse_ref_error(stmt->src,
					"STATUS must be DELETE/KEEP in CLOSE");
				ofc_sema_stmt_io_close__cleanup(s);
				return NULL;
			}
		}
	}

	ofc_sema_stmt_t* as
		= ofc_sema_stmt_alloc(s);
	if (!as)
	{
		ofc_sema_stmt_io_close__cleanup(s);
		return NULL;
	}
	return as;
}

static bool ofc_sema_stmt_close__print_optional(
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

bool ofc_sema_stmt_io_close_print(
	ofc_colstr_t* cs,
	const ofc_sema_stmt_t* stmt)
{
	if (!cs || !stmt || stmt->type != OFC_SEMA_STMT_IO_CLOSE)
		return false;

	if (!ofc_colstr_keyword_atomic_writef(cs, "CLOSE")
		|| !ofc_colstr_atomic_writef(cs, " ")
		|| !ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_sema_expr_print(cs, stmt->io_close.unit))
		return false;

	if (stmt->io_close.err)
	{
		if (!ofc_sema_stmt_close__print_optional(
			cs, "ERR", stmt->io_close.err))
			return false;
	}
	if (stmt->io_close.iostat)
	{
		if (!ofc_sema_stmt_close__print_optional(
			cs, "IOSTAT", stmt->io_close.iostat))
			return false;
	}
	if (stmt->io_close.status)
	{
		if (!ofc_sema_stmt_close__print_optional(
			cs, "STATUS", stmt->io_close.status))
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, ")"))
		return false;

	return true;
}
