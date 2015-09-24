#include "../parse.h"



unsigned parse_stmt_data(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_DATA);
	if (i == 0) return 0;

	unsigned l;
	stmt->data = parse_data_list(
		src, &ptr[i], &l);
	if (!stmt->data) return 0;
	i += l;

	stmt->type = PARSE_STMT_DATA;
	return i;
}
