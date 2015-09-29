#ifndef __parse_implicit_do_h__
#define __parse_implicit_do_h__

/* http://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vn85/index.html#c40000f814617 */

struct parse_implicit_do_s
{
    parse_lhs_t*    dlist;
    parse_assign_t* init;
	parse_expr_t*   limit;
	parse_expr_t*   step;
};


parse_implicit_do_t* parse_implicit_do(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);

parse_implicit_do_t* parse_implicit_do_copy(
	parse_implicit_do_t* id);

void parse_implicit_do_delete(
	parse_implicit_do_t* id);

bool parse_implicit_do_print(
	int fd, const parse_implicit_do_t* id);

#endif
