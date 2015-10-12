#include <ofc/parse.h>


ofc_parse_stmt_list_t* ofc_parse_file(const ofc_sparse_t* src)
{
	const char* ptr = ofc_sparse_strz(src);

	ofc_parse_debug_t* debug
		= ofc_parse_debug_create();
	if (!debug) return NULL;

	unsigned len;
	ofc_parse_stmt_list_t* list
		= ofc_parse_stmt_list(src, ptr, debug, &len);
	if (!list) return NULL;

	ofc_parse_debug_print(debug);
	ofc_parse_debug_delete(debug);

	if (ptr[len] != '\0')
	{
		ofc_sparse_error(src, &ptr[len],
			"Expected end of input");
		ofc_parse_stmt_list_delete(list);
		return NULL;
	}

	return list;
}

bool ofc_parse_file_print(
	ofc_colstr_t* cs,
	const ofc_parse_stmt_list_t* list)
{
	return (ofc_parse_stmt_list_print(cs, list, 0)
		&& ofc_colstr_writef(cs, "\n"));
}
