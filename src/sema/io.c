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

/* Compare type to descriptor type at
 * offset position in format_list
 */
bool ofc_sema_io_compare_types(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt,
	const ofc_sema_lhs_t* lhs,
	ofc_sema_expr_t** expr,
	const ofc_sema_type_t* type,
	const ofc_sema_array_t* array,
	ofc_sema_structure_t* structure,
	ofc_parse_format_desc_list_t* format_list,
	unsigned* offset)
{
	if (!type || !format_list || (!lhs && !expr))
		return false;

	/* Compare base type of array for each
	 * element of the array
	 */
	if (array)
	{
		unsigned array_count;
		if (!ofc_sema_array_total(
			array, &array_count))
			return false;

		unsigned j;
		for (j = 0; j < array_count; j++)
		{
			if (!ofc_sema_io_compare_types(
				scope, stmt, lhs, expr, type, NULL, structure, format_list, offset))
				return false;
		}
	}
	/* Compare each member of the structure to the format list */
	else if (structure)
	{
		unsigned elem_count;
		if (!ofc_sema_structure_elem_count(
			structure, &elem_count))
			return false;

		unsigned j;
		for (j = 0; j < elem_count; j++)
		{
			const ofc_sema_decl_t* member
				= ofc_sema_structure_elem_get(structure, j);
			if (!member) return false;

			if (!ofc_sema_io_compare_types(
				scope, stmt, lhs, expr,
				member->type, member->array, member->structure,
				format_list, offset))
				return false;
		}
	}
	/* If it's not array nor structure, compare types directly */
	else
	{
		if (ofc_sema_type_is_complex(type))
		{
			unsigned j;
			for (j = 0; j < 2; j++)
			{
				ofc_parse_format_desc_t* desc
					= format_list->desc[(*offset)++];
				if (!desc)
					return false;

				if (!ofc_sema_compare_desc_expr_type(desc->type, type->type))
				{
					ofc_sparse_ref_warning((!expr ? lhs->src : (*expr)->src),
						"Trying to format a %s output  with a %s FORMAT descriptor",
						ofc_sema_format_str_rep(desc->type),
						ofc_sema_type_str_rep(type));
				}
			}
			return true;
		}

		ofc_parse_format_desc_t* desc
			= format_list->desc[(*offset)++];
		if (!desc) return false;

		if (!ofc_sema_compare_desc_expr_type(desc->type, type->type))
		{
			if (!expr)
			{
				ofc_sparse_ref_warning(lhs->src,
					"Trying to format a %s output  with a %s FORMAT descriptor",
					ofc_sema_format_str_rep(desc->type),
					ofc_sema_type_str_rep(type));
			}
			else
			{
				const ofc_sema_type_t* dtype
					= ofc_sema_format_desc_type(desc);

				ofc_sema_expr_t* cast
					= ofc_sema_expr_cast(*expr, dtype);
				if (!cast)
				{
					ofc_sparse_ref_warning((*expr)->src,
						"Trying to format a %s output  with a %s FORMAT descriptor",
						ofc_sema_format_str_rep(desc->type),
						ofc_sema_type_str_rep(type));
				}
				else
				{
					*expr = cast;
				}
			}
		}
		else if (desc->type == OFC_PARSE_FORMAT_DESC_CHARACTER)
		{
			unsigned csize;
			if (!ofc_sema_type_base_size(type, &csize)
				|| (csize != 1))
			{
				ofc_sparse_ref_t src = (lhs ? lhs->src : (*expr)->src);
				ofc_sparse_ref_error(src,
					"CHARACTER type KIND not supported in %s",
					(stmt->type == OFC_PARSE_STMT_IO_WRITE ? "WRITE" : "PRINT"));
			}
		}
	}

	return true;
}

bool ofc_sema_io_list_has_complex(
	ofc_sema_lhs_list_t* ilist,
	ofc_sema_expr_list_t* olist,
	unsigned* count)
{
	if (!count) return false;

	if (ilist)
	{
		unsigned i;
		for (i = 0; i < ilist->count; i++)
		{
			const ofc_sema_type_t* type
				= ofc_sema_lhs_type(ilist->lhs[i]);
			if (!type) return false;

			if (ofc_sema_type_is_complex(type))
			{
				unsigned elem_count;
				if (!ofc_sema_lhs_elem_count(
					ilist->lhs[i], &elem_count))
					return false;

				*count += elem_count;
			}
		}
	}
	else if (olist)
	{
		unsigned i;
		for (i = 0; i < olist->count; i++)
		{
			const ofc_sema_type_t* type
				= ofc_sema_expr_type(olist->expr[i]);
			if (!type) return false;

			if (ofc_sema_type_is_complex(type))
			{
				unsigned elem_count;
				if (!ofc_sema_expr_elem_count(
					olist->expr[i], &elem_count))
					return false;

				*count += elem_count;
			}
		}
	}
	else
	{
		return false;
	}

	return true;
}

static bool ofc_sema_io__data_format_helper_r(
	ofc_parse_format_desc_list_t* format_list,
	const ofc_parse_format_desc_list_t* format_src,
	unsigned* iolist_len);

static bool ofc_sema_io__data_format_helper_body(
	ofc_parse_format_desc_list_t* format_list,
	ofc_parse_format_desc_t* desc,
	unsigned* iolist_len)
{
	if (ofc_parse_format_is_data_desc(desc))
	{
		/* If it's a data descriptor, we add it to the list
		   and call the function again to the next offset
		   We are an element closer to have length iolist_len. */

		unsigned i;
		for (i = 0; (i < desc->n) && (*iolist_len > 0); i++)
		{
			size_t nsize = (sizeof(ofc_parse_format_desc_t*)
				* (format_list->count + 1));
			ofc_parse_format_desc_t** ndesc
				= (ofc_parse_format_desc_t**)realloc(
					format_list->desc, nsize);
			if (!ndesc) return false;

			ofc_parse_format_desc_t* cdesc
				= ofc_parse_format_desc_copy(desc);
			if (!cdesc) return false;

			format_list->desc = ndesc;
			format_list->desc[format_list->count++] = cdesc;

			(*iolist_len)--;
		}
	}
	else if (desc->type == OFC_PARSE_FORMAT_DESC_REPEAT)
	{
		/* If the descriptor is of type repeat, we call
		   the function again for the sub-format-list. */

		unsigned i;
		for (i = 0; (i < desc->n)  && (*iolist_len > 0); i++)
		{
			if (!ofc_sema_io__data_format_helper_r(
				format_list, desc->repeat, iolist_len))
				return false;
		}
	}
	else
	{
		/* If it's a different type, we ignore it
		   and continue with the next offset. */
	}

	return true;
}

static bool ofc_sema_io__data_format_helper_r(
	ofc_parse_format_desc_list_t* format_list,
	const ofc_parse_format_desc_list_t* format_src,
	unsigned* iolist_len)
{
	unsigned offset;
	for (offset = 0; (*iolist_len > 0)
		&& (offset < format_src->count); offset++)
	{
		if (!ofc_sema_io__data_format_helper_body(
			format_list, format_src->desc[offset], iolist_len))
			return false;
	}

	return true;
}

static bool ofc_sema_io__data_format_helper(
	ofc_parse_format_desc_list_t* format_list,
	const ofc_parse_format_desc_list_t* format_src,
	unsigned iolist_len)
{
	if (!format_list || !format_src)
		return false;

	/* This is to handle "forced reversion"
	   http://www.obliquity.com/computer/fortran/format.html */
	unsigned repeat_from = 0;
	unsigned offset;
	for (offset = 0; offset < format_src->count; offset++)
	{
		if (format_src->desc[offset]->type
			== OFC_PARSE_FORMAT_DESC_REPEAT)
			repeat_from = offset;
	}

	unsigned remain;
	for (offset = 0, remain = iolist_len; remain > 0; offset++)
	{
		if (offset >= format_src->count)
		{
			/* If we didn't see a data descriptor
			   then stop iterating. */
			if (remain == iolist_len)
				break;

			offset = repeat_from;

			/* Reset the original length to the current length
			   so we can detect if there's no data descriptor
			   after the repeat_from offset. */
			iolist_len = remain;
		}

		if (!ofc_sema_io__data_format_helper_body(
			format_list, format_src->desc[offset], &remain))
			return false;
	}

	return true;
}

ofc_parse_format_desc_list_t* ofc_sema_io_data_format(
	ofc_sema_format_t* format, unsigned iolist_len)
{
	if (!format) return NULL;

	ofc_parse_format_desc_list_t* format_list
		= (ofc_parse_format_desc_list_t*)malloc(
			sizeof(ofc_parse_format_desc_list_t));
	if (!format_list) return NULL;

	format_list->count = 0;
	format_list->desc = NULL;

	if (!ofc_sema_io__data_format_helper(
		format_list, format->src, iolist_len))
	{
		ofc_parse_format_desc_list_delete(format_list);
		return NULL;
	}

	return format_list;
}

static unsigned ofc_sema_io__data_format_count_helper(
	const ofc_parse_format_desc_list_t* format_src)
{
	if (!format_src) return 0;

	unsigned i, count = 0;
	for (i = 0; i < format_src->count; i++)
	{
		ofc_parse_format_desc_t* desc
			= format_src->desc[i];

		if (ofc_parse_format_is_data_desc(desc))
		{
			count += desc->n;
		}
		else if (desc->type == OFC_PARSE_FORMAT_DESC_REPEAT)
		{
			unsigned n;
			for (n = 0; n < desc->n; n++)
			{
				count += ofc_sema_io__data_format_count_helper(desc->repeat);
			}
		}
	}

	return count;
}

unsigned ofc_sema_io_data_format_count(
	ofc_sema_format_t* format)
{
	if (!format) return 0;

	return ofc_sema_io__data_format_count_helper(format->src);
}

bool ofc_sema_io_format_iolist_compare(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt,
	ofc_parse_format_desc_list_t* format_list,
	ofc_sema_expr_list_t* iolist)
{
	if (!format_list || !iolist) return false;

	unsigned i, offset = 0;
	for (i = 0; i < iolist->count; i++)
	{
		ofc_sema_expr_t** expr
			= &iolist->expr[i];

		const ofc_sema_type_t* type
			= ofc_sema_expr_type(*expr);
		const ofc_sema_array_t* array
			= ofc_sema_expr_array(*expr);
		ofc_sema_structure_t* structure
			= ofc_sema_expr_structure(*expr);

		if (!ofc_sema_io_compare_types(
			scope, stmt, NULL, expr, type, array, structure, format_list, &offset))
			return false;
	}

	return true;
}

bool ofc_sema_io_format_input_list_compare(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt,
	ofc_parse_format_desc_list_t* format_list,
	ofc_sema_lhs_list_t* iolist)
{
	if (!format_list || !iolist) return false;

	unsigned i, offset = 0;
	for (i = 0; i < iolist->count; i++)
	{
		const ofc_sema_type_t* type
			= ofc_sema_lhs_type(iolist->lhs[i]);
		const ofc_sema_array_t* array
			= ofc_sema_lhs_array(iolist->lhs[i]);
		ofc_sema_structure_t* structure
			= ofc_sema_lhs_structure(iolist->lhs[i]);

		if (!ofc_sema_io_compare_types(
			scope, stmt, iolist->lhs[i], NULL,
			type, array, structure, format_list, &offset))
			return false;
	}

	return true;
}

bool ofc_sema_io_check_label(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt,
	bool is_format, ofc_sema_expr_t* expr,
	const ofc_sema_label_t** label_dst)
{
	if (!expr) return false;

	const ofc_sema_type_t* etype
		= ofc_sema_expr_type(expr);
	if (!etype) return false;

	const ofc_sema_label_t* label_ret = NULL;
	if (ofc_sema_type_is_integer(etype))
	{
		const ofc_sema_typeval_t* label
			= ofc_sema_expr_constant(expr);
		if (label)
		{
			int64_t fl64 = 0;
			if (!ofc_sema_typeval_get_integer(
				label, &fl64) || (fl64 < 0))
			{
				ofc_sparse_ref_error(stmt->src,
					"Label expression must be a positive INTEGER");
				return false;
			}

			if (is_format)
			{
				unsigned ulabel = (unsigned)fl64;

				if (((int64_t)ulabel) != fl64)
					return false;

				label_ret = ofc_sema_label_map_find(
					scope->label, ulabel);
				if (!label_ret)
				{
					ofc_sparse_ref_error(stmt->src,
						"Label %d expression not defined", ulabel);
					return false;
				}
			}
		}
		else
		{
			ofc_sparse_ref_warning(stmt->src,
				"Using a variable for a label is deprecated");
		}
	}
	else
	{
		ofc_sparse_ref_error(stmt->src,
			"Expected label in IO argument");
		return false;
	}

	if (label_dst) *label_dst = label_ret;
	return true;
}
