#include "../file.h"
#include "../../prep.h"
#include <string.h>


unsigned parse_stmt_include(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, PARSE_KEYWORD_INCLUDE);
	if (i == 0) return 0;

	string_t spath;
	unsigned l = parse_character(
		src, &ptr[i], debug, &spath);
	if (l == 0)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	if (!is_end_statement(&ptr[i], NULL))
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}

	/* Don't rewind debug after this point because
	   we know we have a valid include statement. */

	char path[spath.size + 1];
	memcpy(path, spath.base, spath.size);
	path[spath.size] = '\0';

	stmt->include.file = file_create(
		path, sparse_lang_opts(src));
	if (!stmt->include.file)
	{
		sparse_error(src, ptr, "Can't open include file '%s'", path);
		return 0;
	}

	stmt->include.src = prep(stmt->include.file);
	stmt->include.include = parse_file(stmt->include.src);
	if (!stmt->include.include)
	{
		parse_stmt_list_delete(stmt->include.include);
		sparse_delete(stmt->include.src);
		file_delete(stmt->include.file);
		return 0;
	}

	stmt->type = PARSE_STMT_INCLUDE;
	return i;
}

bool parse_stmt_include_print(
	int fd, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	/* TODO - Print included code? */

	return dprintf_bool(
		fd, "INCLUDE \'%s\'",
		file_get_path(stmt->include.file));
}
