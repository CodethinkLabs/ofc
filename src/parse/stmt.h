#ifndef __parse_stmt_h__
#define __parse_stmt_h__

typedef enum
{
	PARSE_STMT_EMPTY,
	PARSE_STMT_ASSIGN,
	PARSE_STMT_CONTINUE,
	PARSE_STMT_STOP,
	PARSE_STMT_PAUSE,
	PARSE_STMT_GO_TO,
	PARSE_STMT_IF_COMPUTED,
	PARSE_STMT_WRITE,
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
		} assign;

		struct
		{
			unsigned label;
		} go_to;

		struct
		{
			bool         has_code;
			parse_expr_t code;
		} stop_pause;

		struct
		{
			parse_expr_t cond;
			unsigned  label_count;
			unsigned* label;
		} if_comp;

		struct
		{
			parse_expr_t file;
			unsigned     format_label;

			unsigned      elem_count;
			parse_expr_t* elem;
		} write;
	};
} parse_stmt_t;


unsigned parse_stmt_continue(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_stop_pause(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_go_to(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_assign(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_if_computed(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);
unsigned parse_stmt_write(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt);

unsigned parse_stmt(
	const sparse_t* src, const char* ptr,
	const unsigned* label,
	parse_stmt_t* stmt);

void parse_stmt_cleanup(
	parse_stmt_t stmt);

#endif
