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
	hashmap_t* decl_map);

bool parse_decl_create_implicit(
	const str_ref_t name,
	const parse_implicit_t* implicit,
	parse_decl_t* decl);

void parse_decl_cleanup(
	parse_decl_t decl);

/* Heap allocation functions. */
parse_decl_t* parse_decl_alloc(
	parse_decl_t decl);
void parse_decl_delete(
	parse_decl_t* decl);

/* Hash table functions. */
uint8_t parse_decl_hash_ci(
	const str_ref_t* key);
uint8_t parse_decl_hash(
	const str_ref_t* key);
const str_ref_t* parse_decl_key(
	const parse_decl_t* decl);
bool parse_decl_key_compare_ci(
	const str_ref_t* a, const str_ref_t* b);
bool parse_decl_key_compare(
	const str_ref_t* a, const str_ref_t* b);

#endif
