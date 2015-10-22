#include <ofc/parse.h>



struct entry {
    char *keyword;
    int key_val;
};

static const struct entry io_stmt__keyword[] =
{
		{ "OPEN ",      OFC_PARSE_STMT_IO_OPEN      },
		{ "INQUIRE ",   OFC_PARSE_STMT_IO_INQUIRE   },
		{ "REWIND ",    OFC_PARSE_STMT_IO_REWIND    },
		{ "BACKSPACE ", OFC_PARSE_STMT_IO_BACKSPACE },
		{ "READ ",      OFC_PARSE_STMT_IO_READ      },
		{ "WRITE ",     OFC_PARSE_STMT_IO_WRITE     },
		{ "ENDFILE ",   OFC_PARSE_STMT_IO_END_FILE  },
		{ "CLOSE ",     OFC_PARSE_STMT_IO_CLOSE     },
		{ 0, 0 }
};

static bool ofc_parse_io__print_keyword(
ofc_colstr_t* cs, int key_val)
{
	int i;
	for(i = 0; io_stmt__keyword[i].keyword != 0; i++)
	{
		if (io_stmt__keyword[i].key_val == key_val)
		{
			return ofc_colstr_atomic_writef(cs, "%s",
        io_stmt__keyword[i].keyword);
		}
	}
	return false;
}

static unsigned ofc_parse_stmt__io(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword, bool iolist,
	bool force_brackets,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	stmt->io.params = NULL;
	if (ptr[i] == '(')
	{
		i += 1;

		unsigned len;
		stmt->io.params = ofc_parse_call_arg_list_named(
			src, &ptr[i], debug, &len);
		if (stmt->io.params) i += len;

		if (ptr[i++] != ')')
		{
			ofc_parse_call_arg_list_delete(
				stmt->io.params);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
	}
	else if (force_brackets)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	else
	{
		unsigned len;
		ofc_parse_call_arg_t* unit
			= ofc_parse_call_arg(src, &ptr[i], debug, &len);
		if (!unit)
		{
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += len;

		if (ptr[i] == ',')
			i += 1;

		stmt->io.params = ofc_parse_call_arg_list_wrap(unit);
		if (!stmt->io.params)
		{
			ofc_parse_call_arg_delete(unit);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
	}

	stmt->io.iolist = NULL;
	if (iolist)
	{
		unsigned len;
		stmt->io.iolist = ofc_parse_iolist(
			src, &ptr[i], debug, &len);
		if (stmt->io.iolist) i += len;
	}

	return i;
}

bool ofc_parse_stmt_io_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	if (!ofc_parse_io__print_keyword(cs, stmt->type))
		return false;

	if (!ofc_colstr_atomic_writef(cs, "(")
		|| !ofc_parse_call_arg_list_print(cs, stmt->io.params)
		|| !ofc_colstr_atomic_writef(cs, ")")
		|| !ofc_colstr_atomic_writef(cs, " "))
		return false;

	if (stmt->io.iolist)
		ofc_parse_iolist_print(cs, stmt->io.iolist);
	return true;
}

bool ofc_parse_stmt_print_accept_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	const char* kwstr;
	switch (stmt->type)
	{
		case OFC_PARSE_STMT_IO_PRINT:
			kwstr = "PRINT";
			break;
		case OFC_PARSE_STMT_IO_TYPE:
			kwstr = "TYPE";
			break;
		case OFC_PARSE_STMT_IO_ACCEPT:
			kwstr = "ACCEPT";
			break;
		default:
			return false;
	}

	if (!ofc_colstr_atomic_writef(cs, "%s ", kwstr))
		return false;

	if (!(stmt->io_print.format_asterisk
		? ofc_colstr_atomic_writef(cs, "*")
		: ofc_parse_label_print(cs, stmt->io_print.format)))
		return false;

	if (stmt->io_print.iolist)
	{
		if (!ofc_colstr_atomic_writef(cs, ", ")
			|| !ofc_parse_iolist_print(
				cs, stmt->io_print.iolist))
			return false;
	}

	return true;
}

bool ofc_parse_stmt_define_file_print(
	ofc_colstr_t* cs, const ofc_parse_stmt_t* stmt)
{
	if (!stmt)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "DEFINE FILE ")
		&& !ofc_parse_define_file_arg_list_print(
			cs, stmt->io_define_file.args))
		return false;
}

unsigned ofc_parse_stmt_io_open(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_OPEN, false, true,
		stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IO_OPEN;
	return i;
}

unsigned ofc_parse_stmt_io_inquire(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_INQUIRE, false, true,
		stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IO_INQUIRE;
	return i;
}

unsigned ofc_parse_stmt_io_rewind(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_REWIND, false, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IO_REWIND;
	return i;
}

unsigned ofc_parse_stmt_io_backspace(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_BACKSPACE, false, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IO_BACKSPACE;
	return i;
}

unsigned ofc_parse_stmt_io_read(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_READ, true, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IO_READ;
	return i;
}

unsigned ofc_parse_stmt_io_write(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_WRITE, true, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IO_WRITE;
	return i;
}

unsigned ofc_parse_stmt_io_end_file(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_END_FILE, false, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IO_END_FILE;
	return i;
}

unsigned ofc_parse_stmt_io_close(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_CLOSE, false, true,
		stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IO_CLOSE;
	return i;
}

unsigned ofc_parse_stmt_io_encode(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_ENCODE, true, true,
		stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IO_ENCODE;
	return i;
}

unsigned ofc_parse_stmt_io_decode(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_DECODE, true, true,
		stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IO_DECODE;
	return i;
}

static unsigned ofc_parse_stmt_io__print_type_accept(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	stmt->io_print.format_asterisk = false;
	unsigned len = ofc_parse_label(
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
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
	}
	i += len;

	stmt->io_print.iolist = NULL;
	if (ptr[i] == ',')
	{
		i += 1;

		stmt->io_print.iolist = ofc_parse_iolist(
			src, &ptr[i], debug, &len);
		if (!stmt->io_print.iolist)
		{
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
		i += len;
	}

	return i;
}

/* http://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnbi/index.html
   This won't conflict with the TYPE (name) declaration,
   because the brackets disambiguate it. */
unsigned ofc_parse_stmt_io_print_type(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt_io__print_type_accept(
		src, ptr, debug, (toupper(ptr[0]) == 'P'
			? OFC_PARSE_KEYWORD_PRINT : OFC_PARSE_KEYWORD_TYPE), stmt);
	if (i == 0) return 0;

	if (toupper(ptr[0]) != 'P')
	{
		ofc_parse_debug_warning(debug, src, ptr,
			"Use of TYPE as an IO statement is deprecated and ambiguous"
			", PRINT is preferred");
		stmt->type = OFC_PARSE_STMT_IO_TYPE;
	}
	else
	{
		stmt->type = OFC_PARSE_STMT_IO_PRINT;
	}

	return i;
}

unsigned ofc_parse_stmt_io_accept(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned i = ofc_parse_stmt_io__print_type_accept(
		src, ptr, debug, OFC_PARSE_KEYWORD_ACCEPT, stmt);
	if (i == 0) return 0;

	stmt->type = OFC_PARSE_STMT_IO_ACCEPT;
	return i;
}

unsigned ofc_parse_stmt_io_define_file(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_DEFINE_FILE);
	if (i == 0) return 0;

	unsigned len;
	stmt->io_define_file.args = NULL;

	stmt->io_define_file.args = ofc_parse_define_file_arg_list(
		src, &ptr[i], debug, &len);
	if (!stmt->io_define_file.args)
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}
	i += len;

	stmt->type = OFC_PARSE_STMT_IO_DEFINE_FILE;

	return i;
}
