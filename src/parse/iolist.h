#ifndef __ofc_parse_iolist_h__
#define __ofc_parse_iolist_h__

typedef struct ofc_parse_iolist_s ofc_parse_iolist_t;

typedef struct
{
	bool is_implicit_do;
	union
	{
		ofc_parse_implicit_do_t* id;
		ofc_parse_expr_t*        expr;
	};
} ofc_parse_ioarg_t;

struct ofc_parse_iolist_s
{
	unsigned        count;
	ofc_parse_ioarg_t** arg;
};

ofc_parse_iolist_t* ofc_parse_iolist(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_iolist_delete(
	ofc_parse_iolist_t* list);
bool ofc_parse_iolist_print(
	ofc_colstr_t* cs, const ofc_parse_iolist_t* list);

#endif
