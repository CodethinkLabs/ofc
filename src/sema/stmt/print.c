#include <ofc/sema.h>
#include <math.h>

ofc_sema_stmt_t* ofc_sema_stmt_io_print(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_IO_PRINT))
		return NULL;

	ofc_sema_stmt_t s;
	s.type = OFC_SEMA_STMT_IO_PRINT;
	s.io_print.format_asterisk
		= stmt->io_print.format_asterisk;
	s.io_print.format = NULL;
	s.io_print.iolist = NULL;
	ofc_sema_format_t* format = NULL;

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
			const ofc_sema_typeval_t* format_label
				= ofc_sema_expr_constant(s.io_print.format);
			if (!format_label)
			{
				ofc_sema_scope_error(scope, stmt->io_print.format->src,
					"Format label expression must be resolvable"
					" at compile time in PRINT");
				ofc_sema_expr_delete(s.io_print.format);
				return NULL;
			}

			int64_t fl64 = 0;
			if (!ofc_sema_typeval_get_integer(format_label, &fl64))
			{
				ofc_sema_expr_delete(s.io_print.format);
				return NULL;
			}

			if (fl64 < 0)
			{
				ofc_sema_scope_error(scope, stmt->io_print.format->src,
					"Format label expression must be a positive INTEGER in PRINT");
				ofc_sema_expr_delete(s.io_print.format);
				return NULL;
			}

			unsigned ulabel = (unsigned) fl64;

			if (((int64_t) ulabel) != fl64)
			{
				ofc_sema_expr_delete(s.io_print.format);
				return NULL;
			}

			/* TODO - support labels assigned after statement */
			const ofc_sema_label_t* label
				= ofc_sema_label_map_find(scope->label, ulabel);
			if (!label)
			{
				ofc_sema_scope_error(scope, stmt->io_print.format->src,
					"Format label expression not defined in PRINT");
				ofc_sema_expr_delete(s.io_print.format);
				return NULL;
			}

			if (label->type != OFC_SEMA_LABEL_FORMAT)
			{
				ofc_sema_scope_error(scope, stmt->io_print.format->src,
					"Label expression must be a FORMAT statement in PRINT");
				ofc_sema_expr_delete(s.io_print.format);
				return NULL;
			}
			format = label->format;
		}
		else if (etype->type == OFC_SEMA_TYPE_CHARACTER)
		{
            /* TODO - Check we can resolve this as a format descriptor. */
		}
		else
		{
			/* TODO - Support INTEGER array formats. */

			ofc_sema_scope_error(scope, stmt->src,
				"Format (FMT) must be a label or character string in PRINT");
			ofc_sema_expr_delete(s.io_print.format);
			return NULL;
		}
	}

	/* Check iolist */
	if (stmt->io_print.iolist)
	{
		s.io_print.iolist
			= ofc_sema_expr_list_create();
		if (!s.io_print.iolist)
		{
			ofc_sema_expr_delete(s.io_print.format);
			return NULL;
		}

		unsigned i;
		for (i = 0; i < stmt->io_print.iolist->count; i++)
		{
			ofc_parse_expr_t* parse_expr
				= stmt->io_print.iolist->expr[i];

			if ((parse_expr->type == OFC_PARSE_EXPR_VARIABLE)
				&& (parse_expr->variable->type == OFC_PARSE_LHS_IMPLICIT_DO))
			{
				if (!ofc_sema_expr_list_add_list(s.io_print.iolist,
						ofc_sema_expr_list_implicit_do(scope, parse_expr->variable->implicit_do)))
				{
					ofc_sema_expr_delete(s.io_print.format);
					ofc_parse_expr_delete(parse_expr);
					ofc_sema_expr_list_delete(s.io_print.iolist);
					return NULL;
				}
			}
			else
			{
				/* TODO - support non intrinsic types */
				ofc_sema_expr_t* expr = ofc_sema_expr(
					scope, parse_expr);
				if (!expr)
				{
					ofc_sema_expr_delete(s.io_print.format);
					ofc_parse_expr_delete(parse_expr);
					ofc_sema_expr_list_delete(s.io_print.iolist);
					return NULL;
				}

				if (!ofc_sema_expr_list_add(s.io_print.iolist, expr))
				{
					ofc_sema_expr_delete(s.io_print.format);
					ofc_parse_expr_delete(parse_expr);
					ofc_sema_expr_delete(expr);
					ofc_sema_expr_list_delete(s.io_print.iolist);
					return NULL;
				}
			}
		}
	}

	/* Compare iolist with format */
	if (format)
	{
		unsigned i, data_desc = 0;
		for (i = 0; i < format->src->count; i++)
		{
			if (ofc_parse_format_is_data_desc(format->src->desc[i]))
			{
				data_desc++;
			}
		}

		if (data_desc > 0)
		{
			if (s.io_print.iolist->count < data_desc)
			{
				ofc_sema_scope_warning(scope, stmt->io_print.format->src,
					"IO list shorter than FORMAT list,"
					" last FORMAT descriptors will be ignored");
			}
			else if (fmod(s.io_print.iolist->count, data_desc) != 0)
			{
				ofc_sema_scope_warning(scope, stmt->io_print.format->src,
					"IO list length is not a multiple of FORMAT list length");
			}

			unsigned i, j = 0;
			for (i = 0; i < s.io_print.iolist->count; i++)
			{
				ofc_sema_expr_t* expr
					= s.io_print.iolist->expr[i];

				if (format->src->count < j) j = 0;
				ofc_parse_format_desc_t* desc
					= format->src->desc[j];

				while (!ofc_parse_format_is_data_desc(desc))
				{
					j++;
					if (format->src->count < j) j = 0;
					desc = format->src->desc[j];
				}

				const ofc_sema_type_t* type
					= ofc_sema_expr_type(expr);

				/* TODO - support arrays and structures */
				if (!ofc_sema_compare_desc_expr_type(desc->type, type->type))
				{
					/* TODO - Specify formats */
					ofc_sema_scope_error(scope, stmt->io_print.format->src,
						"FORMAT and output type not compatible in PRINT");
					ofc_parse_format_desc_delete(desc);
					ofc_sema_expr_delete(s.io_print.format);
					ofc_sema_expr_list_delete(s.io_print.iolist);
					return NULL;
				}

				j++;
			}
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
