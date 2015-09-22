#include "../parse.h"


static parse_expr_t* parse_stmt__io_optarg(
	const sparse_t* src, const char* ptr,
	const parse_keyword_e name,
	unsigned* len)
{
	unsigned i = 0;
	unsigned l = parse_keyword(
		src, &ptr[i], name);
	if (l == 0) return NULL;
	i += l;

	if (ptr[i++] != '=')
		return NULL;

	parse_expr_t* expr
		= parse_expr(src, &ptr[i], &l);
	if (!expr) return NULL;
	i += l;

	if (len) *len = i;
	return expr;
}

static unsigned parse_stmt__io(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword,
	bool fmt, bool rec, bool end, bool status, bool args,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, keyword);
	if (i == 0) return 0;

	stmt->io.unit   = NULL;
	stmt->io.fmt    = NULL;
	stmt->io.fmt_asterisk = false;
	stmt->io.rec    = NULL;
	stmt->io.end    = NULL;
	stmt->io.iostat = NULL;
	stmt->io.err    = NULL;
	stmt->io.status = NULL;
	stmt->io.args   = NULL;

	unsigned len;
	if (ptr[i] != '(')
	{
		stmt->io.unit = parse_expr(src, &ptr[i], &len);
		if (!stmt->io.unit) return 0;
		i += len;
	}
	else
	{
		i += 1;

		bool initial = true;

		len = parse_name(src, &ptr[i], NULL);
		if (len == 0)
			len = parse_keyword(
				src, &ptr[i], PARSE_KEYWORD_END);
		if ((len == 0) || (ptr[i + len] != '='))
		{
			stmt->io.unit = parse_expr(src, &ptr[i], &len);
			if (!stmt->io.unit) return 0;
			i += len;

			if (fmt && (ptr[i] == ','))
			{
				len = parse_name(src, &ptr[i + 1], NULL);
				if (len == 0)
					len = parse_keyword(
						src, &ptr[i + 1], PARSE_KEYWORD_END);
				if ((len == 0) || (ptr[i + 1 + len] != '='))
				{
					i += 1;

					stmt->io.fmt = parse_expr(src, &ptr[i], &len);
					if (!stmt->io.fmt)
					{
						if (ptr[i] == '*')
						{
							stmt->io.fmt_asterisk = true;
							len = 1;
						}
						else
						{
							parse_expr_delete(stmt->io.unit);
							return 0;
						}
					}
					i += len;
				}
			}

			initial = false;
		}

		bool failed = false;
		while (true)
		{
			if (!initial)
			{
				if (ptr[i] != ',')
					break;
				i += 1;
			}
			initial = false;

			if (fmt && !stmt->io.fmt
				&& !stmt->io.fmt_asterisk)
			{
				/* FMT is a special case because of its asterisk. */
				len = parse_keyword(
					src, &ptr[i], PARSE_KEYWORD_FMT);
				if ((len > 0) && (ptr[i + len] == '='))
				{
					i += len + 1;
					stmt->io.fmt = parse_expr(
						src, &ptr[i], &len);
					if (!stmt->io.fmt)
					{
						if (ptr[i] == '*')
						{
							stmt->io.fmt_asterisk = true;
							len = 1;
						}
						else
						{
							failed = true;
							break;
						}
					}
					i += len;
					continue;
				}
			}

			if (!stmt->io.unit)
			{
				stmt->io.unit = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_UNIT, &len);
				if (stmt->io.unit)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io.iostat)
			{
				stmt->io.iostat = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_IOSTAT, &len);
				if (stmt->io.iostat)
				{
					i += len;
					continue;
				}
			}

			if (rec && !stmt->io.rec)
			{
				stmt->io.rec = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_REC, &len);
				if (stmt->io.rec)
				{
					i += len;
					continue;
				}
			}

			if (end && !stmt->io.end)
			{
				stmt->io.end = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_END, &len);
				if (stmt->io.end)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io.err)
			{
				stmt->io.err = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_ERR, &len);
				if (stmt->io.err)
				{
					i += len;
					continue;
				}
			}

			if (status && !stmt->io.status)
			{
				stmt->io.status = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_STATUS, &len);
				if (stmt->io.status)
				{
					i += len;
					continue;
				}
			}

			failed = true;
			break;
		}

		if (failed || (ptr[i++] != ')'))
		{
			parse_expr_delete(stmt->io.unit);
			parse_expr_delete(stmt->io.fmt);
			parse_expr_delete(stmt->io.rec);
			parse_expr_delete(stmt->io.end);
			parse_expr_delete(stmt->io.iostat);
			parse_expr_delete(stmt->io.err);
			return 0;
		}
	}

	stmt->io.args = NULL;
	if (args)
	{
		stmt->io.args = parse_iolist(
			src, &ptr[i], &len);
		if (stmt->io.args) i += len;
	}

	return i;
}

unsigned parse_stmt_io_open(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_OPEN);
	if (i == 0) return 0;

	stmt->io_open.unit     = NULL;
	stmt->io_open.file     = NULL;
	stmt->io_open.access   = NULL;
	stmt->io_open.blank    = NULL;
	stmt->io_open.err      = NULL;
	stmt->io_open.form     = NULL;
	stmt->io_open.iostat   = NULL;
	stmt->io_open.recl     = NULL;
	stmt->io_open.status   = NULL;
	stmt->io_open.fileopt  = NULL;
	stmt->io_open.readonly = false;
	stmt->io_open.action   = NULL;

	unsigned len;
	if (ptr[i] != '(')
	{
		stmt->io_open.unit = parse_expr(src, &ptr[i], &len);
		if (!stmt->io_open.unit) return 0;
		i += len;
	}
	else
	{
		i += 1;

		bool initial = true;

		len = parse_name(src, &ptr[i], NULL);
		if (len == 0)
			len = parse_keyword(
				src, &ptr[i], PARSE_KEYWORD_END);
		if ((len == 0) || (ptr[i + len] != '='))
		{
			stmt->io_open.unit = parse_expr(src, &ptr[i], &len);
			if (!stmt->io_open.unit) return 0;
			i += len;
			initial = false;
		}

		while (true)
		{
			if (!initial)
			{
				if (ptr[i] != ',')
					break;
				i += 1;
			}
			initial = false;

			if (!stmt->io_open.readonly)
			{
				len = parse_keyword(
					src, &ptr[i + 1], PARSE_KEYWORD_READONLY);
				if (len > 0)
				{
					stmt->io_open.readonly = true;
					i += (1 + len);
					continue;
				}
			}

			if (!stmt->io_open.unit)
			{
				stmt->io_open.unit = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_UNIT, &len);
				if (stmt->io_open.unit)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_open.file)
			{
				stmt->io_open.file = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_FILE, &len);
				if (stmt->io_open.file)
				{
					i += len;
					continue;
				}
				else
				{
					stmt->io_open.file = parse_stmt__io_optarg(
						src, &ptr[i], PARSE_KEYWORD_NAME, &len);
					if (stmt->io_open.file)
					{
						i += len;
						continue;
					}
				}
			}

			if (!stmt->io_open.access)
			{
				stmt->io_open.access = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_ACCESS, &len);
				if (stmt->io_open.access)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_open.blank)
			{
				stmt->io_open.blank = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_BLANK, &len);
				if (stmt->io_open.blank)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_open.err)
			{
				stmt->io_open.err = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_ERR, &len);
				if (stmt->io_open.err)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_open.form)
			{
				stmt->io_open.form = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_FORM, &len);
				if (stmt->io_open.form)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_open.iostat)
			{
				stmt->io_open.iostat = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_IOSTAT, &len);
				if (stmt->io_open.iostat)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_open.recl)
			{
				stmt->io_open.recl = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_RECL, &len);
				if (stmt->io_open.recl)
				{
					i += len;
					continue;
				}
				else
				{
					stmt->io_open.recl = parse_stmt__io_optarg(
						src, &ptr[i], PARSE_KEYWORD_RECORDSIZE, &len);
					if (stmt->io_open.recl)
					{
						i += len;
						continue;
					}
				}
			}

			if (!stmt->io_open.status)
			{
				stmt->io_open.status = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_STATUS, &len);
				if (stmt->io_open.status)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_open.fileopt)
			{
				stmt->io_open.fileopt = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_FILEOPT, &len);
				if (stmt->io_open.fileopt)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_open.action)
			{
				stmt->io_open.action = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_ACTION, &len);
				if (stmt->io_open.action)
				{
					i += len;
					continue;
				}
			}

			break;
		}

		if (ptr[i++] != ')')
		{
			parse_expr_delete(stmt->io_open.unit);
			parse_expr_delete(stmt->io_open.file);
			parse_expr_delete(stmt->io_open.access);
			parse_expr_delete(stmt->io_open.blank);
			parse_expr_delete(stmt->io_open.err);
			parse_expr_delete(stmt->io_open.form);
			parse_expr_delete(stmt->io_open.iostat);
			parse_expr_delete(stmt->io_open.recl);
			parse_expr_delete(stmt->io_open.status);
			parse_expr_delete(stmt->io_open.fileopt);
			parse_expr_delete(stmt->io_open.action);
			return 0;
		}
	}

	stmt->type = PARSE_STMT_IO_OPEN;
	return i;
}

unsigned parse_stmt_io_inquire(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_INQUIRE);
	if (i == 0) return 0;

	stmt->io_inquire.unit        = NULL;
	stmt->io_inquire.file        = NULL;
	stmt->io_inquire.err         = NULL;
	stmt->io_inquire.exist       = NULL;
	stmt->io_inquire.opened      = NULL;
	stmt->io_inquire.named       = NULL;
	stmt->io_inquire.access      = NULL;
	stmt->io_inquire.sequential  = NULL;
	stmt->io_inquire.direct      = NULL;
	stmt->io_inquire.form        = NULL;
	stmt->io_inquire.formatted   = NULL;
	stmt->io_inquire.unformatted = NULL;
	stmt->io_inquire.name        = NULL;
	stmt->io_inquire.blank       = NULL;
	stmt->io_inquire.iostat      = NULL;
	stmt->io_inquire.number      = NULL;
	stmt->io_inquire.recl        = NULL;
	stmt->io_inquire.nextrec     = NULL;

	unsigned len;
	if (ptr[i] != '(')
	{
		stmt->io_inquire.unit = parse_expr(src, &ptr[i], &len);
		if (!stmt->io_inquire.unit) return 0;
		i += len;
	}
	else
	{
		i += 1;

		bool initial = true;

		len = parse_name(src, &ptr[i], NULL);
		if (len == 0)
			len = parse_keyword(
				src, &ptr[i], PARSE_KEYWORD_END);
		if ((len == 0) || (ptr[i + len] != '='))
		{
			stmt->io_inquire.unit = parse_expr(src, &ptr[i], &len);
			if (!stmt->io_inquire.unit) return 0;
			i += len;
			initial = false;
		}

		while (true)
		{
			if (!initial)
			{
				if (ptr[i] != ',')
					break;
				i += 1;
			}
			initial = false;

			if (!stmt->io_inquire.unit)
			{
				stmt->io_inquire.unit = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_UNIT, &len);
				if (stmt->io_inquire.unit)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.file)
			{
				stmt->io_inquire.file = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_FILE, &len);
				if (stmt->io_inquire.file)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.err)
			{
				stmt->io_inquire.err = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_ERR, &len);
				if (stmt->io_inquire.err)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.exist)
			{
				stmt->io_inquire.exist = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_EXIST, &len);
				if (stmt->io_inquire.exist)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.opened)
			{
				stmt->io_inquire.opened = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_OPENED, &len);
				if (stmt->io_inquire.opened)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.named)
			{
				stmt->io_inquire.named = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_NAMED, &len);
				if (stmt->io_inquire.named)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.access)
			{
				stmt->io_inquire.access = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_ACCESS, &len);
				if (stmt->io_inquire.access)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.sequential)
			{
				stmt->io_inquire.sequential = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_SEQUENTIAL, &len);
				if (stmt->io_inquire.sequential)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.direct)
			{
				stmt->io_inquire.direct = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_DIRECT, &len);
				if (stmt->io_inquire.direct)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.form)
			{
				stmt->io_inquire.form = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_FORM, &len);
				if (stmt->io_inquire.form)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.formatted)
			{
				stmt->io_inquire.formatted = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_FORMATTED, &len);
				if (stmt->io_inquire.formatted)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.unformatted)
			{
				stmt->io_inquire.unformatted = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_UNFORMATTED, &len);
				if (stmt->io_inquire.unformatted)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.name)
			{
				stmt->io_inquire.name = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_NAME, &len);
				if (stmt->io_inquire.name)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.blank)
			{
				stmt->io_inquire.blank = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_BLANK, &len);
				if (stmt->io_inquire.blank)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.iostat)
			{
				stmt->io_inquire.iostat = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_IOSTAT, &len);
				if (stmt->io_inquire.iostat)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.number)
			{
				stmt->io_inquire.number = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_NUMBER, &len);
				if (stmt->io_inquire.number)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.recl)
			{
				stmt->io_inquire.recl = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_RECL, &len);
				if (stmt->io_inquire.recl)
				{
					i += len;
					continue;
				}
			}

			if (!stmt->io_inquire.nextrec)
			{
				stmt->io_inquire.nextrec = parse_stmt__io_optarg(
					src, &ptr[i], PARSE_KEYWORD_NEXTREC, &len);
				if (stmt->io_inquire.nextrec)
				{
					i += len;
					continue;
				}
			}

			break;
		}

		if (ptr[i++] != ')')
		{
			parse_expr_delete(stmt->io_inquire.unit       );
			parse_expr_delete(stmt->io_inquire.file       );
			parse_expr_delete(stmt->io_inquire.err        );
			parse_expr_delete(stmt->io_inquire.exist      );
			parse_expr_delete(stmt->io_inquire.opened     );
			parse_expr_delete(stmt->io_inquire.named      );
			parse_expr_delete(stmt->io_inquire.access     );
			parse_expr_delete(stmt->io_inquire.sequential );
			parse_expr_delete(stmt->io_inquire.direct     );
			parse_expr_delete(stmt->io_inquire.form       );
			parse_expr_delete(stmt->io_inquire.formatted  );
			parse_expr_delete(stmt->io_inquire.unformatted);
			parse_expr_delete(stmt->io_inquire.name       );
			parse_expr_delete(stmt->io_inquire.blank      );
			parse_expr_delete(stmt->io_inquire.iostat     );
			parse_expr_delete(stmt->io_inquire.number     );
			parse_expr_delete(stmt->io_inquire.recl       );
			parse_expr_delete(stmt->io_inquire.nextrec    );
			return 0;
		}
	}

	stmt->type = PARSE_STMT_IO_INQUIRE;
	return i;
}

unsigned parse_stmt_io_rewind(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr,
		PARSE_KEYWORD_REWIND,
		false, false, false, false, false,
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
		PARSE_KEYWORD_BACKSPACE,
		false, false, false, false, false,
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
		PARSE_KEYWORD_READ,
		true, true, true, false, true,
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
		PARSE_KEYWORD_WRITE,
		true, true, false, false, true,
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
		PARSE_KEYWORD_END_FILE,
		false, false, false, false, false,
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
		PARSE_KEYWORD_CLOSE,
		false, false, false, true, false,
		stmt);
	if (i == 0) return 0;

	/* TODO - Support STATUS. */

	stmt->type = PARSE_STMT_IO_CLOSE;
	return i;
}
