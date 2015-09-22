#ifndef __parse_assign_h__
#define __parse_assign_h__

typedef struct
{
	parse_lhs_t*  name;
	parse_expr_t* init;
} parse_assign_t;

typedef struct
{
	unsigned         count;
	parse_assign_t** assign;
} parse_assign_list_t;


parse_assign_t* parse_assign(
	const sparse_t* src, const char* ptr,
	unsigned* len);

parse_assign_t* parse_assign_init(
	const sparse_t* src, const char* ptr,
	unsigned* len);

parse_assign_t* parse_assign_copy(
	const parse_assign_t* assign);

void parse_assign_delete(
	parse_assign_t* assign);


parse_assign_list_t* parse_assign_list(
	const sparse_t* src, const char* ptr,
	unsigned* len);

parse_assign_list_t* parse_assign_list_copy(
	const parse_assign_list_t* list);

void parse_assign_list_delete(
	parse_assign_list_t* list);


#endif
