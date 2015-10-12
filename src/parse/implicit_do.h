#ifndef __ofc_parse_implicit_do_h__
#define __ofc_parse_implicit_do_h__

/* http://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vn85/index.html#c40000f814617 */

struct ofc_parse_implicit_do_s
{
    ofc_parse_lhs_t*    dlist;
    ofc_parse_assign_t* init;
	ofc_parse_expr_t*   limit;
	ofc_parse_expr_t*   step;
};


ofc_parse_implicit_do_t* ofc_parse_implicit_do(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);

ofc_parse_implicit_do_t* ofc_parse_implicit_do_copy(
	ofc_parse_implicit_do_t* id);

void ofc_parse_implicit_do_delete(
	ofc_parse_implicit_do_t* id);

bool ofc_parse_implicit_do_print(
	ofc_colstr_t* cs, const ofc_parse_implicit_do_t* id);

#endif
