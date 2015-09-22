#ifndef __parse_stmt_h__
#define __parse_stmt_h__


typedef struct
{
	unsigned       count;
	parse_stmt_t** stmt;
} parse_stmt_list_t;

typedef enum
{
	PARSE_STMT_EMPTY,
	PARSE_STMT_PROGRAM,
	PARSE_STMT_SUBROUTINE,
	PARSE_STMT_FUNCTION,
	PARSE_STMT_BLOCK_DATA,
	PARSE_STMT_IMPLICIT_NONE,
	PARSE_STMT_IMPLICIT,
	PARSE_STMT_CALL,
	PARSE_STMT_ENTRY,
	PARSE_STMT_DECL,
	PARSE_STMT_DIMENSION,
	PARSE_STMT_EQUIVALENCE,
	PARSE_STMT_COMMON,
	PARSE_STMT_ASSIGNMENT,
	PARSE_STMT_CONTINUE,
	PARSE_STMT_STOP,
	PARSE_STMT_PAUSE,
	PARSE_STMT_RETURN,
	PARSE_STMT_EXTERNAL,
	PARSE_STMT_INTRINSIC,
	PARSE_STMT_GO_TO,
	PARSE_STMT_GO_TO_ASSIGNED,
	PARSE_STMT_GO_TO_COMPUTED,
	PARSE_STMT_IF_COMPUTED,
	PARSE_STMT_IF_STATEMENT,
	PARSE_STMT_IF_THEN,
	PARSE_STMT_DO,
	PARSE_STMT_IO_OPEN,
	PARSE_STMT_IO_REWIND,
	PARSE_STMT_IO_BACKSPACE,
	PARSE_STMT_IO_READ,
	PARSE_STMT_IO_WRITE,
	PARSE_STMT_IO_END_FILE,
	PARSE_STMT_FORMAT,
	PARSE_STMT_DATA,
	PARSE_STMT_SAVE,
	PARSE_STMT_PARAMETER,
	PARSE_STMT_ASSIGN,
} parse_stmt_e;

struct parse_stmt_s
{
	parse_stmt_e type;

	unsigned label;

	union
	{
		parse_implicit_list_t* implicit;

		struct
		{
			str_ref_t              name;
			parse_call_arg_list_t* args;
		} call_entry;

		struct
		{
			parse_type_t*      type;
			parse_decl_list_t* decl;
		} decl;

		parse_common_group_list_t* common;

		parse_lhs_list_t* dimension;

		struct
		{
			unsigned           count;
			parse_lhs_list_t** group;
		} equivalence;

		parse_assign_t* assignment;

		struct
		{
			parse_expr_t* value;
		} stop_pause_return;

		struct
		{
			unsigned    count;
			str_ref_t** name;
		} external_intrinsic;

		struct
		{
			parse_expr_t*  cond;
			unsigned       label_count;
			parse_label_t* label;
		} go_to_comp;

		struct
		{
			parse_label_t  cond;
			unsigned       label_count;
			parse_label_t* label;
		} go_to_assign;

		struct
		{
			parse_label_t label;
		} go_to;

		struct
		{
			parse_expr_t*  cond;
			unsigned       label_count;
			parse_label_t* label;
		} if_comp;

		struct
		{
			parse_expr_t*  cond;
			parse_stmt_t*  stmt;
		} if_stmt;

		struct
		{
			parse_expr_t*      cond;
			parse_stmt_list_t* block_then;
			parse_stmt_list_t* block_else;
		} if_then;

		struct
		{
			parse_label_t   end_label;
			parse_assign_t* init;
			parse_expr_t*   last;
			parse_expr_t*   step;
		} do_loop;

		struct
		{
			parse_expr_t*   unit;
			parse_expr_t*   fmt;
			parse_expr_t*   rec;
			parse_expr_t*   end;
			parse_expr_t*   iostat;
			parse_expr_t*   err;
			parse_iolist_t* args;
		} io;

		struct
		{
			parse_expr_t*   unit;
			parse_expr_t*   file;
			parse_expr_t*   access;
			parse_expr_t*   blank;
			parse_expr_t*   err;
			parse_expr_t*   form;
			parse_expr_t*   iostat;
			parse_expr_t*   recl;
			parse_expr_t*   status;
			parse_expr_t*   fileopt;
			bool            readonly;
			parse_expr_t*   action;
		} io_open;

		struct
		{
			unsigned             desc_count;
			parse_format_desc_t* desc;
		} format;

		struct
		{
			unsigned       name_count;
			parse_lhs_t**  name;
			unsigned       init_count;
			parse_expr_t** init;
		} data;

		struct
		{
			parse_save_list_t* list;
		} save;

		struct
		{
			parse_assign_list_t* list;
		} parameter;

		struct
		{
			unsigned  label;
			str_ref_t variable;
		} assign;

		struct
		{
			/* type is only set for functions. */
			parse_type_t*      type;
			str_ref_t          name;
			/* args is only set for functions and subroutines. */
			parse_lhs_list_t*  args;
			parse_stmt_list_t* body;
		} program;
	};
};


parse_stmt_t* parse_stmt(
	const sparse_t* src, const char* ptr,
	unsigned* len);
void parse_stmt_delete(
	parse_stmt_t* stmt);


parse_stmt_list_t* parse_stmt_list(
	const sparse_t* src, const char* ptr,
	unsigned* len);
void parse_stmt_list_delete(
	parse_stmt_list_t* list);

#endif
