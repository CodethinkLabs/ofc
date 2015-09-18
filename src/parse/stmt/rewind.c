#include "../parse.h"

static parse_expr_t* parse_stmt_rewind__optional(
	const sparse_t* src, const char* ptr,
	const parse_keyword_e opt_keyword,
	unsigned* len)
{
	unsigned i = 0;
	if (ptr[i++] != ',')
		return NULL;

	unsigned l = parse_keyword(
		src, &ptr[i], opt_keyword);
	if (i == 0) return NULL;
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

unsigned parse_stmt_rewind(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
  unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_REWIND);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_REWIND;

	bool bracketed = (ptr[i] == '(');
	if (bracketed) i += 1;

	unsigned len = parse_keyword(
		src, &ptr[i], PARSE_KEYWORD_UNIT);
	if (len != 0)
	{
		i += len;
		len = 0;
		if (ptr[i++] != '=')
			return 0;
	}

	stmt->rewind.unit	= parse_expr(	src, &ptr[i], &len);
	if (!stmt->rewind.unit) return 0;
	i += len;

	stmt->rewind.iostat = parse_stmt_rewind__optional(
					src, &ptr[i], PARSE_KEYWORD_IOSTAT, &len);
	if (stmt->rewind.iostat) i += len;

	stmt->rewind.err = parse_stmt_rewind__optional(
					src, &ptr[i], PARSE_KEYWORD_ERR, &len);
	if (stmt->rewind.err) i += len;
	if (len != 0)

	if (bracketed)
	{
		if (ptr[i++] != ')')
		{
			parse_stmt_cleanup(*stmt);
			return 0;
		}
	}

	return i;

}
