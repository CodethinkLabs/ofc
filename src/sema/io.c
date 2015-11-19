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

	unsigned i, j =0;
	for (i = 0; i < iolist_len; i++)
	{
		if (format->src->count <= j) j = 0;
		ofc_parse_format_desc_t* desc
			= format->src->desc[j];

		while (!ofc_parse_format_is_data_desc(desc))
		{
			j++;
			if (format->src->count <= j) j = 0;
			desc = format->src->desc[j];
		}
		ofc_parse_format_desc_t** ndesc
			= (ofc_parse_format_desc_t**)realloc(format_list->desc,
				(sizeof(ofc_parse_format_desc_t*) * (format_list->count + 1)));
		if (!ndesc) return NULL;

		format_list->desc = ndesc;
		format_list->desc[format_list->count++] = desc;

		j++;
	}

	return format_list;
}

unsigned ofc_sema_io_data_format_count(
	ofc_sema_format_t* format)
{
	if (!format) return 0;

	ofc_parse_format_desc_t* desc
		= format->src->desc[0];

	unsigned count = 0;
	unsigned i;
	for (i = 0; i < format->src->count; i++)
	{
		if (ofc_parse_format_is_data_desc(desc))
			count ++;
	}

	return count;
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
