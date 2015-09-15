#ifndef __parse_stmt_h__
#define __parse_stmt_h__

typedef enum
{
	PARSE_STMT_EMPTY,
	PARSE_STMT_ASSIGNMENT,
	PARSE_STMT_CONTINUE,
	PARSE_STMT_STOP,
	PARSE_STMT_PAUSE,
	PARSE_STMT_GO_TO,
	PARSE_STMT_IF_COMPUTED,
	PARSE_STMT_DO,
	PARSE_STMT_WRITE,
	PARSE_STMT_FORMAT,
	PARSE_STMT_DATA,
	PARSE_STMT_ASSIGN,
} parse_stmt_e;

typedef struct
{
	parse_stmt_e type;

	const unsigned* label;

	union
	{
		struct
		{
			str_ref_t    lhs;
			parse_expr_t rhs;
		} assignment;

		struct
		{
			bool         has_code;
			parse_expr_t code;
		} stop_pause;

		struct
		{
			parse_label_t label;
		} go_to;

		struct
		{
			parse_expr_t   cond;
			unsigned       label_count;
			parse_label_t* label;
		} if_comp;

		struct
		{
			parse_label_t end_label;
			str_ref_t     iterator;
			parse_expr_t  init;
			parse_expr_t  last;
			parse_expr_t  step;
		} do_loop;

		struct
		{
			parse_expr_t  file;
			parse_label_t format_label;

			unsigned      elem_count;
			parse_expr_t* elem;
		} write;

		struct
		{
			unsigned             desc_count;
			parse_format_desc_t* desc;
		} format;

		struct
		{
			unsigned      count;
			str_ref_t*    name;
			parse_expr_t* init;
		} data;

		struct
		{
			unsigned  label;
			str_ref_t variable;
		} assign;
	};
} parse_stmt_t;


unsigned parse_stmt_assignment(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_continue(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_stop_pause(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_go_to(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_if_computed(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_do(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_write(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_format(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_data(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_assign(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);

unsigned parse_stmt(
	const sparse_t* src, const char* ptr,
	const unsigned* label,
	parse_stmt_t* stmt);

void parse_stmt_cleanup(
	parse_stmt_t stmt);

#endif
