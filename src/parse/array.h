#ifndef __parse_array_h__
#define __parse_array_h__

typedef struct
{
	bool          is_slice;
	parse_expr_t* first;
	parse_expr_t* last;
	parse_expr_t* stride;
} parse_array_range_t;

typedef struct
{
	unsigned              count;
	parse_array_range_t** range;
} parse_array_index_t;


parse_array_index_t* parse_array_index(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);

parse_array_index_t* parse_array_index_copy(
	const parse_array_index_t* index);

void parse_array_index_delete(
	parse_array_index_t* index);

bool parse_array_index_print(
	string_t* tree_output, const parse_array_index_t* index);


#endif
