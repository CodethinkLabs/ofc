#include "../parse.h"

unsigned parse_stmt_format(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug,
		PARSE_KEYWORD_FORMAT);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	/* Empty FORMAT statements are valid as in nist/FM110.FOR */
	unsigned l;
	stmt->format = parse_format_desc_list(
		src, &ptr[i], debug, &l);
	if (stmt->format) i += l;

	if (ptr[i++] != ')')
	{
		parse_format_desc_list_delete(
			stmt->format);
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	stmt->type = PARSE_STMT_FORMAT;
	return i;
}

bool parse_stmt_format_print(
	string_t* tree_output, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	if (!string_printf(tree_output, "FORMAT("))
		return false;

	if (stmt->format
		&& !parse_format_desc_list_print(
			tree_output, stmt->format))
		return false;

	return string_printf(tree_output, ")");
}
