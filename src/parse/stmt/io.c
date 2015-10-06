#include "../parse.h"



struct entry {
    char *keyword;
    int key_val;
};

static const struct entry io_stmt__keyword[] =
{
		{ "OPEN ",      PARSE_STMT_IO_OPEN      },
		{ "INQUIRE ",   PARSE_STMT_IO_INQUIRE   },
		{ "REWIND ",    PARSE_STMT_IO_REWIND    },
		{ "BACKSPACE ", PARSE_STMT_IO_BACKSPACE },
		{ "READ ",      PARSE_STMT_IO_READ      },
		{ "WRITE ",     PARSE_STMT_IO_WRITE     },
		{ "ENDFILE ",   PARSE_STMT_IO_END_FILE  },
		{ "CLOSE ",     PARSE_STMT_IO_CLOSE     },
		{ 0, 0 }
};

static bool parse_io__print_keyword(
int fd, int key_val)
{
	int i;
	for(i = 0; io_stmt__keyword[i].keyword != 0; i++)
	{
		if (io_stmt__keyword[i].key_val == key_val)
		{
			return dprintf_bool(fd, "%s",
        io_stmt__keyword[i].keyword);
		}
	}
	return false;
}

static unsigned parse_stmt__io(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_keyword_e keyword, bool iolist,
	bool force_brackets,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	stmt->io.params = NULL;
	if (ptr[i] == '(')
	{
		i += 1;

		unsigned len;
		stmt->io.params = parse_call_arg_list_named(
			src, &ptr[i], debug, &len);
		if (stmt->io.params) i += len;

		if (ptr[i++] != ')')
		{
			parse_call_arg_list_delete(
				stmt->io.params);
			parse_debug_rewind(debug, dpos);
			return 0;
		}
	}
	else if (force_brackets)
	{
		parse_debug_rewind(debug, dpos);
		return 0;
	}
	else
	{
		unsigned len;
		parse_call_arg_t* unit
			= parse_call_arg(src, &ptr[i], debug, &len);
		if (!unit)
		{
			parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += len;

		if (ptr[i] == ',')
			i += 1;

		stmt->io.params = parse_call_arg_list_wrap(unit);
		if (!stmt->io.params)
		{
			parse_call_arg_delete(unit);
			parse_debug_rewind(debug, dpos);
			return 0;
		}
	}

	stmt->io.iolist = NULL;
	if (iolist)
	{
		unsigned len;
		stmt->io.iolist = parse_iolist(
			src, &ptr[i], debug, &len);
		if (stmt->io.iolist) i += len;
	}

	return i;
}

bool parse_stmt_io_print(
	int fd, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	if (!parse_io__print_keyword(fd, stmt->type))
		return false;

	if (!dprintf_bool(fd, "(")
		|| !parse_call_arg_list_print(fd, stmt->io.params)
		|| !dprintf_bool(fd, ")"))
		return false;

	if (stmt->io.iolist)
		parse_iolist_print(fd, stmt->io.iolist);
	return true;
}

bool parse_stmt_print_accept_print(
	int fd, const parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case PARSE_STMT_IO_PRINT:
			kwstr = "PRINT";
			break;
		case PARSE_STMT_IO_ACCEPT:
			kwstr = "ACCEPT";
			break;
		default:
			return false;
	}

	if (!dprintf_bool(fd, "%s ", kwstr))
		return false;

	if (!(stmt->io_print.format_asterisk
		? dprintf_bool(fd, "*")
		: parse_label_print(fd, stmt->io_print.format)))
		return false;

	if (stmt->io_print.iolist)
	{
		if (!dprintf_bool(fd, ", ")
			|| !parse_iolist_print(
				fd, stmt->io_print.iolist))
			return false;
	}

	return true;
}

unsigned parse_stmt_io_open(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr, debug,
		PARSE_KEYWORD_OPEN, false, true,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_OPEN;
	return i;
}

unsigned parse_stmt_io_inquire(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr, debug,
		PARSE_KEYWORD_INQUIRE, false, true,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_INQUIRE;
	return i;
}

unsigned parse_stmt_io_rewind(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr, debug,
		PARSE_KEYWORD_REWIND, false, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_REWIND;
	return i;
}

unsigned parse_stmt_io_backspace(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr, debug,
		PARSE_KEYWORD_BACKSPACE, false, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_BACKSPACE;
	return i;
}

unsigned parse_stmt_io_read(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr, debug,
		PARSE_KEYWORD_READ, true, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_READ;
	return i;
}

unsigned parse_stmt_io_write(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr, debug,
		PARSE_KEYWORD_WRITE, true, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_WRITE;
	return i;
}

unsigned parse_stmt_io_end_file(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr, debug,
		PARSE_KEYWORD_END_FILE, false, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_END_FILE;
	return i;
}

unsigned parse_stmt_io_close(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr, debug,
		PARSE_KEYWORD_CLOSE, false, true,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_CLOSE;
	return i;
}

unsigned parse_stmt_io_encode(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr, debug,
		PARSE_KEYWORD_ENCODE, true, true,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_ENCODE;
	return i;
}

unsigned parse_stmt_io_decode(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr, debug,
		PARSE_KEYWORD_DECODE, true, true,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_DECODE;
	return i;
}

static unsigned parse_stmt_io__print_type_accept(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_keyword_e keyword,
	parse_stmt_t* stmt)
{
	unsigned dpos = parse_debug_position(debug);

	unsigned i = parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	stmt->io_print.format_asterisk = false;
	unsigned len = parse_label(
		src, &ptr[i], debug,
		&stmt->io_print.format);
	if (len == 0)
	{
		if (ptr[i] == '*')
		{
			stmt->io_print.format_asterisk = true;
			len = 1;
		}
		else
		{
			parse_debug_rewind(debug, dpos);
			return 0;
		}
	}
	i += len;

	stmt->io_print.iolist = NULL;
	if (ptr[i] == ',')
	{
		i += 1;

		stmt->io_print.iolist = parse_iolist(
			src, &ptr[i], debug, &len);
		if (!stmt->io_print.iolist)
		{
			parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += len;
	}

	return i;
}

/* http://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnbi/index.html
   This won't conflict with the TYPE (name) declaration,
   because the brackets disambiguate it. */
unsigned parse_stmt_io_print_type(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt_io__print_type_accept(
		src, ptr, debug, (ptr[0] == 'P'
			? PARSE_KEYWORD_PRINT : PARSE_KEYWORD_TYPE), stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_PRINT;
	return i;
}

unsigned parse_stmt_io_accept(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt_io__print_type_accept(
		src, ptr, debug, PARSE_KEYWORD_ACCEPT, stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_ACCEPT;
	return i;
}
