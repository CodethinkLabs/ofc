#include "parse.h"


parse_stmt_list_t* parse_file(const sparse_t* src)
{
	const char* ptr = sparse_strz(src);

	parse_debug_t* debug
		= parse_debug_create();
	if (!debug) return NULL;

	unsigned len;
	parse_stmt_list_t* list
		= parse_stmt_list(src, ptr, debug, &len);
	if (!list) return NULL;

	parse_debug_print(debug);
	parse_debug_delete(debug);

	if (ptr[len] != '\0')
	{
		sparse_error(src, &ptr[len],
			"Expected end of input");
		parse_stmt_list_delete(list);
		return NULL;
	}

	return list;
}

bool parse_file_print(
	colstr_t* cs,
	const parse_stmt_list_t* list)
{
	return (parse_stmt_list_print(cs, list, 0)
		&& colstr_writef(cs, "\n"));
}
