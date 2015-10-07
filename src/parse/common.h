#ifndef __parse_common_h__
#define __parse_common_h__

typedef struct
{
	str_ref_t         group; /* May be empty. */
	parse_lhs_list_t* names;
} parse_common_group_t;

typedef struct
{
	unsigned               count;
	parse_common_group_t** group;
} parse_common_group_list_t;

parse_common_group_t* parse_common_group(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_common_group_delete(
	parse_common_group_t* group);
bool parse_common_group_print(
	colstr_t* cs, const parse_common_group_t* group);

parse_common_group_list_t* parse_common_group_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_common_group_list_delete(
	parse_common_group_list_t* list);
bool parse_common_group_list_print(
	colstr_t* cs, const parse_common_group_list_t* list);

#endif
