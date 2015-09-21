#ifndef __parse_array_h__
#define __parse_array_h__

typedef enum
{
	PARSE_ARRAY_RANGE_ALL,
	PARSE_ARRAY_RANGE_SINGLE,
	PARSE_ARRAY_RANGE_SLICE,
} parse_array_range_e;

typedef struct
{
	bool          is_slice;
	parse_expr_t* from;
	parse_expr_t* to;
} parse_array_range_t;

typedef struct
{
	unsigned              count;
	parse_array_range_t** range;
} parse_array_index_t;


parse_array_index_t* parse_array_index(
	const sparse_t* src, const char* ptr,
	unsigned* len);

parse_array_index_t* parse_array_index_copy(
	const parse_array_index_t* index);

void parse_array_index_delete(
	parse_array_index_t* index);


#endif
