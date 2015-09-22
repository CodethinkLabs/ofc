#include "../parse.h"


static parse_expr_t* parse_stmt__io_optarg(
	const sparse_t* src, const char* ptr,
	const parse_keyword_e name,
	unsigned* len)
{
	unsigned i = 0;
	if (ptr[i++] != ',')
		return NULL;

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
	bool fmt, bool rec, bool end, bool args,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, keyword);
	if (i == 0) return 0;

	stmt->io.unit   = NULL;
	stmt->io.fmt    = NULL;
	stmt->io.rec    = NULL;
	stmt->io.end    = NULL;
	stmt->io.iostat = NULL;
	stmt->io.err    = NULL;
	stmt->io.args   = NULL;

	bool bracketed = (ptr[i] == '(');
	if (bracketed) i += 1;

	unsigned len;
	if (bracketed)
	{
		len = parse_keyword(
			src, &ptr[i], PARSE_KEYWORD_UNIT);
		if ((len > 0) && (ptr[i + len] == '='))
			i += (len + 1);
	}

	stmt->io.unit = parse_expr(src, &ptr[i], &len);
	if (!stmt->io.unit) return 0;
	i += len;

	if (bracketed)
	{
		if (fmt && (ptr[i] == ','))
		{
			bool has_fmt = false;
			unsigned j = (i + 1);

			len = parse_keyword(
				src, &ptr[j], PARSE_KEYWORD_FMT);
			if ((len > 0) && (ptr[j + len] == '='))
			{
				j += (len + 1);
				has_fmt = true;
			}
			else
			{
				parse_assign_t* a = parse_assign_init(
					src, &ptr[j], &len);
				parse_assign_delete(a);
				has_fmt = (a == NULL);
			}

			if (has_fmt)
			{
				stmt->io.fmt = parse_expr(
					src, &ptr[j], &len);
				if (!stmt->io.fmt)
				{
					parse_expr_delete(stmt->io.unit);
					return 0;
				}
				i = (j + len);
			}
		}

		stmt->io.iostat = parse_stmt__io_optarg(
			src, &ptr[i], PARSE_KEYWORD_IOSTAT, &len);
		if (stmt->io.iostat) i += len;

		if (rec)
		{
			stmt->io.rec = parse_stmt__io_optarg(
				src, &ptr[i], PARSE_KEYWORD_REC, &len);
			if (stmt->io.rec) i += len;
		}

		if (end)
		{
			stmt->io.end = parse_stmt__io_optarg(
				src, &ptr[i], PARSE_KEYWORD_END, &len);
			if (stmt->io.end) i += len;
		}

		stmt->io.err = parse_stmt__io_optarg(
			src, &ptr[i], PARSE_KEYWORD_ERR, &len);
		if (stmt->io.err) i += len;

		if (ptr[i++] != ')')
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


unsigned parse_stmt_io_rewind(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_stmt__io(
		src, ptr,
		PARSE_KEYWORD_REWIND,
		false, false, false, false,
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
		false, false, false, false,
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
		true, true, true, true,
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
		true, true, false, true,
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
		false, false, false, false,
		stmt);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_IO_END_FILE;
	return i;
}
