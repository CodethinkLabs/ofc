#ifndef __parse_decl_h__
#define __parse_decl_h__

typedef struct parse_decl_s parse_decl_t;

struct parse_decl_s
{
	bool          type_implicit;
	parse_type_t  type;

	str_ref_t     name;

	const parse_decl_t* redecl;

	bool          has_init;
	parse_expr_t  init;
};


unsigned parse_decl(
	const sparse_t* src, const char* ptr,
	const parse_decl_t* decl_list, unsigned decl_list_count,
	parse_implicit_t* implicit,
	parse_decl_t* decl);

#endif
