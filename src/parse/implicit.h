#ifndef __parse_implicit_h__
#define __parse_implicit_h__

typedef struct
{
	parse_type_t* type;
	uint32_t      mask; /* A..Z -> 0..25 */
} parse_implicit_t;

typedef struct
{
	unsigned           count;
	parse_implicit_t** rule;
} parse_implicit_list_t;


parse_implicit_t* parse_implicit(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_implicit_delete(
	parse_implicit_t* implicit);
bool parse_implicit_print(
	int fd, const parse_implicit_t* implicit);

parse_implicit_list_t* parse_implicit_list(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	unsigned* len);
void parse_implicit_list_delete(
	parse_implicit_list_t* list);
bool parse_implicit_list_print(
	int fd, const parse_implicit_list_t* list);

#endif
