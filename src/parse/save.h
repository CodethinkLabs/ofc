#ifndef __parse_save_h__
#define __parse_save_h__

typedef struct
{
	bool is_common;
	union
	{
		parse_lhs_t* lhs;
		str_ref_t    common;
	};
} parse_save_t;

typedef struct
{
	unsigned       count;
	parse_save_t** save;
} parse_save_list_t;


parse_save_t* parse_save(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_save_delete(
	parse_save_t* save);
bool parse_save_print(
	colstr_t* cs, const parse_save_t* save);

parse_save_list_t* parse_save_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_save_list_delete(
	parse_save_list_t* save);
bool parse_save_list_print(
	colstr_t* cs, const parse_save_list_t* list);

#endif
