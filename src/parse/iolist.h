#ifndef __parse_iolist_h__
#define __parse_iolist_h__

typedef struct parse_iolist_s parse_iolist_t;

typedef struct
{
	bool is_implicit_do;
	union
	{
		parse_implicit_do_t* id;
		parse_expr_t*        expr;
	};
} parse_ioarg_t;

struct parse_iolist_s
{
	unsigned        count;
	parse_ioarg_t** arg;
};

parse_iolist_t* parse_iolist(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_iolist_delete(
	parse_iolist_t* list);
bool parse_iolist_print(
	colstr_t* cs, const parse_iolist_t* list);

#endif
