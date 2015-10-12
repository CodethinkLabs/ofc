#include <ofc/parse/file.h>
#include <ofc/file.h>
#include <ofc/prep.h>
#include <string.h>


unsigned ofc_parse_stmt_include(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_INCLUDE);
	if (i == 0) return 0;

	unsigned l = 0;
	ofc_string_t* spath = ofc_parse_character(
		src, &ptr[i], debug, &l);

	if (!spath)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += l;

	if (!ofc_is_end_statement(&ptr[i], NULL))
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	/* Don't rewind debug after this point because
	   we know we have a valid include statement. */

	char path[spath->size + 1];
	memcpy(path, spath->base, spath->size);
	path[spath->size] = '\0';

	const char* include_path = ofc_sparse_get_include(src);
	char* rpath = ofc_sparse_include_path(src, path);
	stmt->include.file = ofc_file_create_include(
		rpath, ofc_sparse_lang_opts(src), include_path);

	if (!stmt->include.file)
	{
		ofc_sparse_error(src, ptr, "Can't open include file '%s'", rpath);
		free(rpath);
		return 0;
	}
	free(rpath);

	stmt->include.src = ofc_prep(stmt->include.file);
	stmt->include.include = ofc_parse_file(stmt->include.src);
	if (!stmt->include.include)
	{
		ofc_parse_stmt_list_delete(stmt->include.include);
		ofc_sparse_delete(stmt->include.src);
		ofc_file_delete(stmt->include.file);
		return 0;
	}

	stmt->type = OFC_PARSE_STMT_INCLUDE;
	return i;
}

bool ofc_parse_stmt_include_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	/* TODO - Print included code? */

	const char* path = ofc_file_get_path(
		stmt->include.file);
	if (!path) return false;

	return ofc_colstr_writef(
		cs, "INCLUDE \'%s\'", path);
}
