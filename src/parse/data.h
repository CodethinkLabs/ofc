#ifndef __parse_data_h__
#define __parse_data_h__

typedef struct
{
	unsigned      repeat;
	parse_expr_t* expr;
} parse_clist_entry_t;

typedef struct
{
	unsigned              count;
	parse_clist_entry_t** entry;
} parse_clist_t;

typedef struct
{
	parse_lhs_list_t* nlist;
	parse_clist_t*    clist;
} parse_data_entry_t;

typedef struct
{
	unsigned             count;
	parse_data_entry_t** entry;
} parse_data_list_t;

parse_clist_t* parse_clist(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_clist_delete(
	parse_clist_t* clist);
bool parse_clist_print(
	string_t* tree_output, const parse_clist_t* list);

parse_data_list_t* parse_data_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_data_list_delete(
	parse_data_list_t* list);
bool parse_data_list_print(
	string_t* tree_output, const parse_data_list_t* list);

#endif
