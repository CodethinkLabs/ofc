#ifndef __parse_call_arg_h__
#define __parse_call_arg_h__

typedef enum
{
	PARSE_CALL_ARG_EXPR,
	PARSE_CALL_ARG_RETURN,
	PARSE_CALL_ARG_ASTERISK,
} parse_call_arg_e;

typedef struct
{
	parse_call_arg_e type;
	str_ref_t        name;
	union
	{
		parse_expr_t* expr;
		parse_label_t label;
	};
} parse_call_arg_t;

typedef struct
{
	unsigned           count;
	parse_call_arg_t** call_arg;
} parse_call_arg_list_t;


parse_call_arg_t* parse_call_arg_force_named(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
parse_call_arg_t* parse_call_arg_named(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
parse_call_arg_t* parse_call_arg(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_call_arg_delete(
	parse_call_arg_t* call_arg);
bool parse_call_arg_print(
	colstr_t* cs, const parse_call_arg_t* call_arg);

parse_call_arg_list_t* parse_call_arg_list_force_named(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
parse_call_arg_list_t* parse_call_arg_list_named(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
parse_call_arg_list_t* parse_call_arg_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
parse_call_arg_list_t* parse_call_arg_list_wrap(
	parse_call_arg_t* arg);
void parse_call_arg_list_delete(
	parse_call_arg_list_t* call_arg);
bool parse_call_arg_list_print(
	colstr_t* cs, const parse_call_arg_list_t* call_arg);

#endif
