#ifndef __parse_pointer_h__
#define __parse_pointer_h__

typedef struct
{
	str_ref_t name;
	str_ref_t target;
} parse_pointer_t;

typedef struct
{
	unsigned          count;
	parse_pointer_t** pointer;
} parse_pointer_list_t;

parse_pointer_list_t* parse_pointer_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_pointer_list_delete(
	parse_pointer_list_t* list);
bool parse_pointer_list_print(
	string_t* tree_output, const parse_pointer_list_t* list);

#endif
