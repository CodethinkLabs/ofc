#include "../parse.h"

unsigned parse_stmt_go_to(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_GO_TO);
	if (i == 0) return 0;

	unsigned label = 0;
	unsigned len = parse_unsigned(
		src, &ptr[i], &label);
	if (len == 0)
	{
		sparse_error(src, &ptr[i],
			"Expected label in GO TO statement");
		return 0;
	}

	stmt->type = PARSE_STMT_GO_TO;
	return (i + len);
}
