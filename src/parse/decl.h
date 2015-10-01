#ifndef __parse_decl_h__
#define __parse_decl_h__

typedef struct
{
	parse_lhs_t*  lhs;

	parse_expr_t*  init_expr;
	parse_clist_t* init_clist;
} parse_decl_t;

typedef struct
{
	unsigned       count;
	parse_decl_t** decl;
} parse_decl_list_t;


parse_decl_t* parse_decl(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_decl_delete(
	parse_decl_t* decl);
bool parse_decl_print(
	string_t* tree_output, const parse_decl_t* decl);

parse_decl_list_t* parse_decl_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_decl_list_delete(
	parse_decl_list_t* decl);
bool parse_decl_list_print(
	string_t* tree_output, const parse_decl_list_t* list);

#endif
