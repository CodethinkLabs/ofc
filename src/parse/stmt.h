#ifndef __parse_stmt_h__
#define __parse_stmt_h__

typedef enum
{
	PARSE_STMT_EMPTY,
	PARSE_STMT_IMPLICIT,
	PARSE_STMT_DECL,
	PARSE_STMT_DIMENSION,
	PARSE_STMT_ASSIGNMENT,
	PARSE_STMT_CONTINUE,
	PARSE_STMT_STOP,
	PARSE_STMT_PAUSE,
	PARSE_STMT_GO_TO,
	PARSE_STMT_GO_TO_ASSIGNED,
	PARSE_STMT_GO_TO_COMPUTED,
	PARSE_STMT_IF_COMPUTED,
	PARSE_STMT_IF_STATEMENT,
	PARSE_STMT_DO,
	PARSE_STMT_WRITE,
	PARSE_STMT_READ,
	PARSE_STMT_FORMAT,
	PARSE_STMT_REWIND,
	PARSE_STMT_DATA,
	PARSE_STMT_ASSIGN,
} parse_stmt_e;


typedef struct
{
	parse_type_t c[26]; /* A-Z */
} parse_stmt_implicit_t;

const parse_stmt_implicit_t PARSE_IMPLICIT_DEFAULT;

typedef struct
{
	str_ref_t     name;
	parse_expr_t* dimension;
	parse_expr_t* init;
} parse_stmt_decl_t;


struct parse_stmt_s
{
	parse_stmt_e type;

	const unsigned* label;

	union
	{
		parse_stmt_implicit_t* implicit;

		struct
		{
			parse_type_t*      type;
			unsigned           count;
			parse_stmt_decl_t* entry;
		} decl;

		struct
		{
			unsigned       count;
			str_ref_t*     name;
			parse_expr_t** dimension;
		} dimension;

		struct
		{
			parse_lhs_t   lhs;
			parse_expr_t* rhs;
		} assignment;

		struct
		{
			parse_expr_t* code;
		} stop_pause;

		struct
		{
			parse_expr_t*  cond;
			unsigned       label_count;
			parse_label_t* label;
		} go_to_comp;

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
			parse_label_t end_label;
			parse_lhs_t   iterator;
			parse_expr_t* init;
			parse_expr_t* last;
			parse_expr_t* step;
		} do_loop;

		struct
		{
			parse_expr_t* file;
			parse_label_t format_label;

			parse_expr_list_t* elem;
		} write;

		struct
		{
			parse_expr_t* file;
			parse_label_t format_label;

			parse_expr_list_t* elem;
		} read;

		struct
		{
			unsigned             desc_count;
			parse_format_desc_t* desc;
		} format;

		struct
		{
			parse_expr_t* unit;
			parse_expr_t* iostat;
			parse_expr_t* err;
		} rewind;

		struct
		{
			unsigned       name_count;
			parse_lhs_t*   name;
			unsigned       init_count;
			parse_expr_t** init;
		} data;

		struct
		{
			unsigned  label;
			str_ref_t variable;
		} assign;
	};
};


parse_stmt_t* parse_stmt(
	const sparse_t* src, const char* ptr,
	const unsigned* label,
	unsigned* len);

void parse_stmt_delete(
	parse_stmt_t* stmt);

#endif
