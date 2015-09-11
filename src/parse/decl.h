#ifndef __parse_decl_h__
#define __parse_decl_h__

typedef struct
{
	parse_type_t type;
	str_ref_t    name;

	bool         has_init;
	parse_expr_t init;
} parse_decl_t;


unsigned parse_decl(
	const sparse_t* src, const char* ptr,
	parse_implicit_t* implicit,
	parse_decl_t* decl);

#endif
