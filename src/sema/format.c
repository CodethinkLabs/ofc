#include <ofc/sema.h>

static bool ofc_sema__type_rule[][9] =
{
	/* OFC_SEMA_TYPE_LOGICAL, OFC_SEMA_TYPE_INTEGER, OFC_SEMA_TYPE_REAL,
	   OFC_SEMA_TYPE_COMPLEX, OFC_SEMA_TYPE_BYTE, OFC_SEMA_TYPE_CHARACTER,
	   OFC_SEMA_TYPE_STRUCTURE, OFC_SEMA_TYPE_POINTER, OFC_SEMA_TYPE_ARRAY */

	{ 0, 1, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_INTEGER */
	{ 0, 0, 1, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_REAL */
	{ 0, 0, 1, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_D */
	{ 0, 0, 1, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_E */
	{ 1, 1, 1, 1, 1, 1, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_G */
	{ 0, 0, 0, 0, 0, 1, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_CHARACTER */
	{ 1, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_LOGICAL */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_HOLLERITH */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_S */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_REAL_SCALE */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_X */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_T */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_SLASH */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_DOLLAR */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_BACKSLASH */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_Q */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_COLON */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_BN */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_BZ */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_SP */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_SS */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_TL */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_TR */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_STRING */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_REPEAT */
	{ 0, 1, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_BINARY */
	{ 0, 1, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_OCTAL */
	{ 0, 1, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_HEX */
};

bool ofc_sema_compare_desc_expr_type(
	ofc_parse_format_desc_e type_desc,
	ofc_sema_type_e type_expr)
{
	return ofc_sema__type_rule[type_desc][type_expr];
}

static ofc_sema_format_t* ofc_sema_format__create(
	const ofc_parse_format_desc_list_t* src)
{
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
