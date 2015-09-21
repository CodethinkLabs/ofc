#include "parse.h"


parse_stmt_list_t* parse_file(const sparse_t* src)
{
	const char* ptr = sparse_strz(src);

	unsigned len;
	parse_stmt_list_t* list
		= parse_stmt_list(src, ptr, &len);
	if (!list) return NULL;

	if (ptr[len] != '\0')
	{
		sparse_error(src, &ptr[len],
			"Expected end of input");
		parse_stmt_list_delete(list);
		return NULL;
	}

	return list;
}
