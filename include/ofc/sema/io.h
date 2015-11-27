#ifndef __ofc_sema_io_h__
#define __ofc_sema_io_h__

bool ofc_sema_io_compare_types(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt,
	ofc_sema_expr_t** expr,
	const ofc_sema_type_t* type,
	ofc_parse_format_desc_list_t* format_list,
	unsigned* offset);

ofc_sema_expr_list_t* ofc_sema_iolist(
	ofc_sema_scope_t* scope,
	ofc_parse_expr_list_t* parse_iolist);
unsigned ofc_sema_iolist_count(
	ofc_sema_expr_list_t* iolist);

ofc_parse_format_desc_list_t* ofc_sema_io_data_format(
	ofc_sema_format_t* format, unsigned iolist_len);
unsigned ofc_sema_io_data_format_count(
	ofc_sema_format_t* format);

bool ofc_sema_io_format_iolist_compare(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt,
	ofc_parse_format_desc_list_t* format_list,
	ofc_sema_expr_list_t* iolist);

bool ofc_sema_io_check_label(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt,
	const char* name, unsigned name_size,
	ofc_sema_expr_t* expr,
	const ofc_sema_label_t** label_dst);
#endif
