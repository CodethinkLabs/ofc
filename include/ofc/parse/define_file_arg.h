#ifndef __ofc_parse_define_file_arg_h__
#define __ofc_parse_define_file_arg_h__

typedef struct
{
	ofc_parse_expr_t* unit;
	ofc_parse_expr_t* rec;
	ofc_parse_expr_t* len;
	ofc_parse_lhs_t*  ascv;
} ofc_parse_define_file_arg_t;

typedef struct
{
	unsigned                      count;
	ofc_parse_define_file_arg_t** define_file_arg;
} ofc_parse_define_file_arg_list_t;


ofc_parse_define_file_arg_t* ofc_parse_define_file_arg(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_define_file_arg_delete(
	ofc_parse_define_file_arg_t* define_file_arg);
bool ofc_parse_define_file_arg_print(
	ofc_colstr_t* cs, const ofc_parse_define_file_arg_t* define_file_arg);

ofc_parse_define_file_arg_list_t* ofc_parse_define_file_arg_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_define_file_arg_list_delete(
	ofc_parse_define_file_arg_list_t* define_file_arg);
bool ofc_parse_define_file_arg_list_print(
	ofc_colstr_t* cs, const ofc_parse_define_file_arg_list_t* define_file_arg);

#endif
