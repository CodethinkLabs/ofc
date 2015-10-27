#include <ofc/sema.h>


static ofc_sema_format_t* ofc_sema_format__create(
	const ofc_parse_format_desc_list_t* src)
{
	if (!src)
		return NULL;

	ofc_sema_format_t* format
		= (ofc_sema_format_t*)malloc(
			sizeof(ofc_sema_format_t));
	if (!format) return NULL;

	format->src    = src;
	format->format = NULL;
	return format;
}

bool ofc_sema_format(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_FORMAT))
		return false;

	if (stmt->label == 0)
	{
		ofc_sema_scope_warning(scope, stmt->src,
			"FORMAT statement without a label has no effect"
			" and will be ignored");
		return true;
	}

	ofc_sema_format_t* format
		= ofc_sema_format__create(stmt->format);
	if (!format) return false;

	if (!ofc_sema_label_map_add_format(
		scope, stmt, scope->label,
		stmt->label, format))
	{
		ofc_sema_format_delete(format);
		return false;
	}

	return true;
}

void ofc_sema_format_delete(
	ofc_sema_format_t* format)
{
	if (!format)
		return;

	ofc_parse_format_desc_list_delete(
		format->format);
	free(format);
}
