#include "../parse.h"


unsigned parse_stmt_dimension(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_DIMENSION);
	if (i == 0) return 0;

	unsigned len;
	stmt->dimension = parse_lhs_list(
		src, &ptr[i], &len);
	if (!stmt->dimension) return 0;
	i += len;

	stmt->type = PARSE_STMT_DIMENSION;
	return i;
}
