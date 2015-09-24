#include "../parse.h"



static unsigned parse_stmt__io(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword, bool iolist,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, keyword);
	if (i == 0) return 0;

	stmt->io.params = NULL;
	if (ptr[i] == '(')
	{
		i += 1;

		unsigned len;
		stmt->io.params = parse_call_arg_list_named(
			src, &ptr[i], &len);
		if (stmt->io.params) i += len;

		if (ptr[i++] != ')')
		{
			parse_call_arg_list_delete(
				stmt->io.params);
			return 0;
		}
	}
	else
	{
		unsigned len;
		parse_call_arg_t* unit
			= parse_call_arg(src, &ptr[i], &len);
		if (!unit) return 0;
		i += len;

		if (ptr[i] == ',')
			i += 1;

		stmt->io.params = parse_call_arg_list_wrap(unit);
		if (!stmt->io.params)
		{
			parse_call_arg_delete(unit);
			return 0;
		}
	}

	stmt->io.iolist = NULL;
	if (iolist)
	{
		unsigned len;
		stmt->io.iolist = parse_iolist(
			src, &ptr[i], &len);
		if (stmt->io.iolist) i += len;
	}

	return i;
}


unsigned parse_stmt_io_open(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr,
		PARSE_KEYWORD_OPEN, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_OPEN;
	return i;
}

unsigned parse_stmt_io_inquire(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr,
		PARSE_KEYWORD_INQUIRE, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_INQUIRE;
	return i;
}

unsigned parse_stmt_io_rewind(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr,
		PARSE_KEYWORD_REWIND, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_REWIND;
	return i;
}

unsigned parse_stmt_io_backspace(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr,
		PARSE_KEYWORD_BACKSPACE, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_BACKSPACE;
	return i;
}

unsigned parse_stmt_io_read(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr,
		PARSE_KEYWORD_READ, true,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_READ;
	return i;
}

unsigned parse_stmt_io_write(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr,
		PARSE_KEYWORD_WRITE, true,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_WRITE;
	return i;
}

unsigned parse_stmt_io_end_file(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr,
		PARSE_KEYWORD_END_FILE, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_END_FILE;
	return i;
}

unsigned parse_stmt_io_close(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr,
		PARSE_KEYWORD_CLOSE, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_CLOSE;
	return i;
}

static unsigned parse_stmt_io__print_type(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, keyword);
	if (i == 0) return 0;

	stmt->io_print.format_asterisk = false;
	unsigned len = parse_label(
		src, &ptr[i], &stmt->io_print.format);
	if (len == 0)
	{
		if (ptr[i] == '*')
		{
			stmt->io_print.format_asterisk = true;
			len = 1;
		}
		else
		{
			return 0;
		}
	}
	i += len;

	stmt->io_print.iolist = NULL;
	if (ptr[i] == ',')
	{
		i += 1;

		stmt->io.iolist = parse_iolist(
			src, &ptr[i], &len);
		if (!stmt->io.iolist) return 0;
		i += len;
	}

	stmt->type = PARSE_STMT_IO_PRINT;
	return i;
}

unsigned parse_stmt_io_print(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	return parse_stmt_io__print_type(
		src, ptr, PARSE_KEYWORD_PRINT, stmt);
}

/* http://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnbi/index.html
   This won't conflict with the TYPE (name) declaration,
   because the brackets disambiguate it. */
unsigned parse_stmt_io_type(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	return parse_stmt_io__print_type(
		src, ptr, PARSE_KEYWORD_TYPE, stmt);
}
