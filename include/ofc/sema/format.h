#ifndef __ofc_sema_format_h__
#define __ofc_sema_format_h__

typedef struct
{
	const ofc_parse_format_desc_list_t* src;
	ofc_parse_format_desc_list_t*       format;
} ofc_sema_format_t;

const char* ofc_sema_format_str_rep(
	const ofc_parse_format_desc_e type);

bool ofc_sema_format(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt);
void ofc_sema_format_delete(
	ofc_sema_format_t* format);

bool ofc_sema_compare_desc_expr_type(
	unsigned type_desc,
	unsigned type_expr);

const ofc_sema_type_t* ofc_sema_format_desc_type(
	const ofc_parse_format_desc_t* desc);

#endif
