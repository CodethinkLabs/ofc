#ifndef __ofc_parse_call_arg_h__
#define __ofc_parse_call_arg_h__

typedef enum
{
	OFC_PARSE_CALL_ARG_EXPR,
	OFC_PARSE_CALL_ARG_RETURN,
	OFC_PARSE_CALL_ARG_ASTERISK,
} ofc_parse_call_arg_e;

typedef struct
{
	ofc_str_ref_t src;

	ofc_parse_call_arg_e type;
	ofc_str_ref_t        name;
	ofc_parse_expr_t*    expr;
} ofc_parse_call_arg_t;

typedef struct
{
	unsigned               count;
	ofc_parse_call_arg_t** call_arg;
} ofc_parse_call_arg_list_t;


ofc_parse_call_arg_t* ofc_parse_call_arg_force_named(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_call_arg_t* ofc_parse_call_arg_named(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_call_arg_t* ofc_parse_call_arg(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_call_arg_delete(
	ofc_parse_call_arg_t* call_arg);
bool ofc_parse_call_arg_print(
	ofc_colstr_t* cs, const ofc_parse_call_arg_t* call_arg);

ofc_parse_call_arg_list_t* ofc_parse_call_arg_list_force_named(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_call_arg_list_t* ofc_parse_call_arg_list_named(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_call_arg_list_t* ofc_parse_call_arg_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
ofc_parse_call_arg_list_t* ofc_parse_call_arg_list_wrap(
	ofc_parse_call_arg_t* arg);
void ofc_parse_call_arg_list_delete(
	ofc_parse_call_arg_list_t* call_arg);
bool ofc_parse_call_arg_list_print(
	ofc_colstr_t* cs, const ofc_parse_call_arg_list_t* call_arg);

#endif
