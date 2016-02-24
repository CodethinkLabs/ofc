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
	const ofc_sema_stmt_t* stmt,
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
				stmt, lhs, expr, type, NULL, structure, format_list, offset))
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
				stmt, lhs, expr,
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
					(stmt->type == OFC_SEMA_STMT_IO_WRITE ? "WRITE" : "PRINT"));
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
	const ofc_parse_format_desc_list_t* format, unsigned iolist_len)
{
	if (!format) return NULL;

	ofc_parse_format_desc_list_t* format_list
		= (ofc_parse_format_desc_list_t*)malloc(
			sizeof(ofc_parse_format_desc_list_t));
	if (!format_list) return NULL;

	format_list->count = 0;
	format_list->desc = NULL;

	if (!ofc_sema_io__data_format_helper(
		format_list, format, iolist_len))
	{
		ofc_parse_format_desc_list_delete(format_list);
		return NULL;
	}

	return format_list;
}

unsigned ofc_sema_io_data_format_count(
	const ofc_parse_format_desc_list_t* format)
{
	if (!format) return 0;

	unsigned i, count = 0;
	for (i = 0; i < format->count; i++)
	{
		ofc_parse_format_desc_t* desc
			= format->desc[i];

		if (ofc_parse_format_is_data_desc(desc))
		{
			count += desc->n;
		}
		else if (desc->type == OFC_PARSE_FORMAT_DESC_REPEAT)
		{
			unsigned n;
			for (n = 0; n < desc->n; n++)
			{
				count += ofc_sema_io_data_format_count(desc->repeat);
			}
		}
	}

	return count;
}

bool ofc_sema_io_format_iolist_compare(
	const ofc_sema_stmt_t* stmt,
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
			stmt, NULL, expr, type, array, structure, format_list, &offset))
			return false;
	}

	return true;
}

bool ofc_sema_io_format_input_list_compare(
	const ofc_sema_stmt_t* stmt,
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
			stmt, iolist->lhs[i], NULL,
			type, array, structure, format_list, &offset))
			return false;
	}

	return true;
}



bool ofc_sema_stmt_io_format_validate(
	ofc_sema_stmt_t* stmt)
{
	if (!stmt) return false;

	ofc_sema_expr_t* format_expr = NULL;
	ofc_sema_lhs_list_t*  ilist = NULL;
	ofc_sema_expr_list_t* olist = NULL;
	switch (stmt->type)
	{
		case OFC_SEMA_STMT_IO_WRITE:
			olist = stmt->io_write.iolist;
			format_expr = stmt->io_write.format;
			break;

		case OFC_SEMA_STMT_IO_READ:
			ilist = stmt->io_read.iolist;
			format_expr = stmt->io_read.format;
			break;

		case OFC_SEMA_STMT_IO_PRINT:
			olist = stmt->io_print.iolist;
			format_expr = stmt->io_print.format;
			break;

		default:
			return false;
	}

	if ((!ilist && !olist)
		|| !format_expr)
		return true;

	ofc_sema_label_t* label
		= format_expr->label;
	if (!format_expr->is_label)
	{
		/* TODO - Check CHARACTER FORMAT descriptors. */
		return true;
	}
	else if (!label)
	{
		/* Label isn't resolvable at compile-time */
		return true;
	}

	if ((label->type != OFC_SEMA_LABEL_STMT)
		|| !label->stmt
		|| (label->stmt->type != OFC_SEMA_STMT_IO_FORMAT))
	{
		ofc_sparse_ref_error(stmt->src,
			"FORMAT label must point to a FORMAT statement");
		return false;
	}

	const ofc_parse_format_desc_list_t* format
		= label->stmt->io_format.format;
	if (!format) format = label->stmt->io_format.src;
	if (!format) return false;

	unsigned data_desc_count
		= ofc_sema_io_data_format_count(format);

	unsigned iolist_len = 0;
	if (ilist ? !ofc_sema_lhs_list_elem_count(ilist, &iolist_len)
		: !ofc_sema_expr_list_elem_count(olist, &iolist_len))
	{
		/* iolist has variable length; we can't check it at compile-time. */
		return true;
	}

	if (iolist_len == 0)
	{
		if (data_desc_count > 0)
		{
			ofc_sparse_ref_warning(format_expr->src,
				"No IO list in formatted IO statement");
		}

		/* iolist is empty; nothing to validate. */
		return true;
	}

	/* Complex types count as two elements for an iolist. */
	unsigned count = 0;
	if (!ofc_sema_io_list_has_complex(
		ilist, olist, &count))
		return false;
	iolist_len += count;

	if (data_desc_count == 0)
	{
		if (iolist_len > 0)
		{
			ofc_sparse_ref_warning(format_expr->src,
				"No data edit descriptors in FORMAT list");
		}

		/* No data descriptors to check. */
		return true;
	}

	if (iolist_len < data_desc_count)
	{
		ofc_sparse_ref_warning(format_expr->src,
			"IO list shorter than FORMAT list,"
			" last FORMAT data descriptors will be ignored");
	}
	else if ((iolist_len % data_desc_count) != 0)
	{
		ofc_sparse_ref_warning(format_expr->src,
			"IO list length is not a multiple of FORMAT list length");
	}

	/* Create a format list of same length as iolist
	 * with only data edit descriptors  */
	ofc_parse_format_desc_list_t* format_list
		= ofc_sema_io_data_format(format, iolist_len);
	if (!format_list) return false;

	/* Compare iolist with format */
	bool success;
	if (ilist)
	{
		success = ofc_sema_io_format_input_list_compare(
			stmt, format_list, ilist);
	}
	else
	{
		success = ofc_sema_io_format_iolist_compare(
			stmt, format_list, olist);
	}

	ofc_parse_format_desc_list_delete(format_list);
	return success;
}
