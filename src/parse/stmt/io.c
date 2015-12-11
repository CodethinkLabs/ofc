/* Copyright 2015 Codethink Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <ofc/parse.h>



struct entry {
    const char *keyword;
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
	ofc_parse_keyword_e keyword, bool has_iolist,
	bool force_brackets,
	ofc_parse_stmt_t* stmt)
{
	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, keyword);
	if (i == 0) return 0;

	stmt->io.has_brakets = false;
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
		stmt->io.has_brakets = true;
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
	unsigned len = 0;
	stmt->io.iolist = ofc_parse_expr_list(
		src, &ptr[i], debug, &len);

	if (stmt->io.iolist)
	{
		if (!has_iolist)
		{
			ofc_sparse_error(src, ofc_str_ref(&ptr[i], len),
				"%s statement can't have iolist",
				ofc_parse_keyword_name(keyword));
			ofc_parse_call_arg_list_delete(
				stmt->io.params);
			ofc_parse_expr_list_delete(
				stmt->io.iolist);

			stmt->type = OFC_PARSE_STMT_ERROR;
			return (i + len);
		}

		i += len;

		/* We're only sure it's an IO statement if there's an IO list,
		   otherwise this could be an implicit FUNCTION call. */
		if (!ofc_is_end_statement(
			&ptr[i], NULL))
		{
			ofc_sparse_error_ptr(src, &ptr[i],
				"Expected end of %s statement",
				ofc_parse_keyword_name(keyword));
			ofc_parse_call_arg_list_delete(
				stmt->io.params);
			ofc_parse_expr_list_delete(
				stmt->io.iolist);

			stmt->type = OFC_PARSE_STMT_ERROR;
			return i;
		}
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
		ofc_parse_expr_list_print(cs, stmt->io.iolist);
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
		: ofc_parse_expr_print(cs, stmt->io_print.format)))
		return false;

	if (stmt->io_print.iolist)
	{
		if (!ofc_colstr_atomic_writef(cs, ", ")
			|| !ofc_parse_expr_list_print(
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

	return (ofc_colstr_atomic_writef(cs, "DEFINE FILE ")
		&& ofc_parse_define_file_arg_list_print(
			cs, stmt->io_define_file.args));
}

unsigned ofc_parse_stmt_io_open(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_IO_OPEN;

	return ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_OPEN, false, true,
		stmt);
}

unsigned ofc_parse_stmt_io_inquire(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_IO_INQUIRE;
	return ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_INQUIRE, false, true,
		stmt);
}

unsigned ofc_parse_stmt_io_rewind(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_IO_REWIND;
	return ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_REWIND, false, false,
		stmt);
}

unsigned ofc_parse_stmt_io_backspace(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_IO_BACKSPACE;
	return ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_BACKSPACE, false, false,
		stmt);
}

unsigned ofc_parse_stmt_io_read(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	/* TODO - Handle unbracketed READ statements */
	stmt->type = OFC_PARSE_STMT_IO_READ;
	return ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_READ, true, false,
		stmt);
}

unsigned ofc_parse_stmt_io_write(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_IO_WRITE;
	return ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_WRITE, true, false,
		stmt);
}

unsigned ofc_parse_stmt_io_end_file(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_IO_END_FILE;
	return ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_END_FILE, false, false,
		stmt);
}

unsigned ofc_parse_stmt_io_close(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_IO_CLOSE;
	return ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_CLOSE, false, true,
		stmt);
}

unsigned ofc_parse_stmt_io_encode(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_IO_ENCODE;
	return ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_ENCODE, true, true,
		stmt);
}

unsigned ofc_parse_stmt_io_decode(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_stmt_t* stmt)
{
	stmt->type = OFC_PARSE_STMT_IO_DECODE;
	return ofc_parse_stmt__io(
		src, ptr, debug,
		OFC_PARSE_KEYWORD_DECODE, true, true,
		stmt);
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

	unsigned len;
	stmt->io_print.format
		= ofc_parse_expr_integer_variable(
			src, &ptr[i], debug, &len);
	if (!stmt->io_print.format)
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

		stmt->io_print.iolist = ofc_parse_expr_list(
			src, &ptr[i], debug, &len);
		if (!stmt->io_print.iolist)
		{
			ofc_parse_expr_delete(stmt->io_print.format);
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
		ofc_parse_debug_warning(debug, ofc_sparse_ref(src, ptr, i),
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
