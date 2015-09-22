#include "../parse.h"


unsigned parse_stmt_save(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_SAVE);
	if (i == 0) return 0;

	unsigned l;
	stmt->save.list = parse_save_list(
		src, &ptr[i], &l);
	if (!stmt->save.list) return 0;
	i += l;

	stmt->type = PARSE_STMT_SAVE;
	return i;
}
