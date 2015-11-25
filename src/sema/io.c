#include <ofc/sema.h>

/* Compare type to descriptor type at
 * offset position in format_list
 */
bool ofc_sema_io_compare_types(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt,
	ofc_sema_expr_t** expr,
	const ofc_sema_type_t* type,
	ofc_parse_format_desc_list_t* format_list,
	unsigned* offset)
{
	if (!type || !format_list || !expr)
		return false;

	/* Compare base type of array for each
	 * element of the array
	 */
	if (ofc_sema_type_is_array(type))
	{
		unsigned array_count = ofc_sema_array_total(type->array);

		type = ofc_sema_type_base(type);

		unsigned j;
		for (j = 0; j < array_count; j++)
		{
			if (!ofc_sema_io_compare_types(
				scope, stmt, expr, type, format_list, offset))
				return false;
		}
	}
	/* Compare each member of the structure to the format list */
	else if (ofc_sema_type_is_structure(type))
	{
		unsigned j;
		for (j = 0; j < type->structure->member.count; j++)
		{
			if (!ofc_sema_io_compare_types(
				scope, stmt, expr, type->structure->member.type[j],
				format_list, offset))
				return false;
		}
	}
	/* If it's not array nor structure, compare types directly */
	else
	{
		ofc_parse_format_desc_t* desc
			= format_list->desc[(*offset)++];
		if (!desc) return false;

		if (!ofc_sema_compare_desc_expr_type(desc->type, type->type))
		{
			const ofc_sema_type_t* dtype
				= ofc_sema_format_desc_type(desc);

			ofc_sema_expr_t* cast
				= ofc_sema_expr_cast(scope, *expr, dtype);
			if (!cast)
			{
				ofc_sema_scope_error(scope, (*expr)->src,
					"FORMAT descriptor (%s) and output type (%s) not compatible",
					ofc_sema_format_str_rep(desc->type),
					ofc_sema_type_str_rep(type->type));
				return false;
			}

			*expr = cast;
		}

		if ((desc->type == OFC_PARSE_FORMAT_DESC_CHARACTER)
			&& (type->kind != 1))
		{
			ofc_sema_scope_error(scope, (*expr)->src,
				"CHARACTER type KIND not supported in %s",
				(stmt->type == OFC_PARSE_STMT_IO_WRITE ? "WRITE" : "PRINT"));
		}
	}

	return true;
}

ofc_sema_expr_list_t* ofc_sema_iolist(
	ofc_sema_scope_t* scope,
	ofc_parse_expr_list_t* parse_iolist)
{
	ofc_sema_expr_list_t* sema_iolist
		= ofc_sema_expr_list_create();
	if (!sema_iolist) return NULL;

	unsigned i;
	for (i = 0; i < parse_iolist->count; i++)
	{
		ofc_parse_expr_t* parse_expr
			= parse_iolist->expr[i];

		if ((parse_expr->type == OFC_PARSE_EXPR_VARIABLE)
			&& (parse_expr->variable->type == OFC_PARSE_LHS_IMPLICIT_DO))
		{
			if (!ofc_sema_expr_list_add_list(sema_iolist,
					ofc_sema_expr_list_implicit_do(scope, parse_expr->variable->implicit_do)))
			{
				ofc_parse_expr_delete(parse_expr);
				ofc_sema_expr_list_delete(sema_iolist);
				return NULL;
			}
		}
		else
		{
			ofc_sema_expr_t* expr = ofc_sema_expr(
				scope, parse_expr);
			if (!expr)
			{
				ofc_parse_expr_delete(parse_expr);
				ofc_sema_expr_list_delete(sema_iolist);
				return NULL;
			}

			if (!ofc_sema_expr_list_add(sema_iolist, expr))
			{
				ofc_parse_expr_delete(parse_expr);
				ofc_sema_expr_delete(expr);
				ofc_sema_expr_list_delete(sema_iolist);
				return NULL;
			}
		}
	}

	return sema_iolist;
}

unsigned ofc_sema_iolist_count(
	ofc_sema_expr_list_t* iolist)
{
	if (!iolist) return 0;

	unsigned iolist_len = 0;
	unsigned i = 0;
	for (i = 0; i < iolist->count; i++)
	{
		ofc_sema_expr_t* expr
			= iolist->expr[i];

		const ofc_sema_type_t* type
			= ofc_sema_expr_type(expr);
		if (!type) return -1;

		if (ofc_sema_type_is_array(type)
			|| ofc_sema_type_is_structure(type))
			iolist_len += ofc_sema_type_elem_count(type);
		else
			iolist_len++;
	}

	return iolist_len;
}

static bool ofc_sema_io__data_format_helper(
	ofc_parse_format_desc_list_t* format_list,
	const ofc_parse_format_desc_list_t* format_src,
	unsigned* iolist_len, unsigned* offset)
{
	if (!format_list || !format_src) return false;

	/* If *iolist_len == 0 then
	 * format_list has iolist_len length*/
	if (*iolist_len > 0)
	{
		if (format_src->count <= *offset) *offset = 0;

		ofc_parse_format_desc_t* desc
			= format_src->desc[*offset];

		/* If it's a data descriptor, we add it to the list
		 * and call the function again to the next offset
		 * We are an element closer to have length iolist_len
		 */
		if (ofc_parse_format_is_data_desc(desc))
		{
			unsigned i;
			for (i = 0; i < desc->n; i++)
			{
				ofc_parse_format_desc_t** ndesc
					= (ofc_parse_format_desc_t**)realloc(format_list->desc,
						(sizeof(ofc_parse_format_desc_t*) * (format_list->count + 1)));
				if (!ndesc) return false;

				ofc_parse_format_desc_t* cdesc
					= ofc_parse_format_desc_copy(
						format_src->desc[*offset]);
				if (!cdesc) return false;

				format_list->desc = ndesc;
				format_list->desc[format_list->count++] = cdesc;

				(*iolist_len)--;
			}

			(*offset)++;

			if (!ofc_sema_io__data_format_helper(
				format_list, format_src, iolist_len, offset))
				return false;
		}
		/* If the descriptor is of type repeat, we call
		 * the function again for the sub-format-list */
		else if (desc->type == OFC_PARSE_FORMAT_DESC_REPEAT)
		{
			unsigned i;
			for (i = 0; i < desc->n; i++)
			{
				unsigned repeat_offset = 0;
				if (!ofc_sema_io__data_format_helper(
					format_list, desc->repeat, iolist_len, &repeat_offset))
					return false;
			}
		}
		/* If it's a different type, we ignore it
		 * and continue with the next offset */
		else
		{
			(*offset)++;

			if (!ofc_sema_io__data_format_helper(
				format_list, format_src, iolist_len, offset))
				return false;
		}
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

	unsigned i = iolist_len;
	unsigned offset = 0;

	if (!ofc_sema_io__data_format_helper(
		format_list, format->src, &i, &offset))
	{
		ofc_parse_format_desc_list_delete(format_list);
		return NULL;
	}

	return format_list;
}

unsigned ofc_sema_io__data_format_count_helper(
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

		if (!ofc_sema_io_compare_types(
			scope, stmt, expr, type, format_list, &offset))
			return false;
	}

	return true;
}
