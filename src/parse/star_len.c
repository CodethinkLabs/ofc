#include "parse.h"


unsigned parse_star_len(
	const sparse_t* src, const char* ptr,
	parse_debug_t* debug,
	parse_expr_t** count, bool* is_variable)
{
	unsigned i = 0;

	if (ptr[i++] != '*')
		return 0;

	unsigned dpos = parse_debug_position(debug);

	unsigned l;
	parse_expr_t* expr;
	if (ptr[i] == '(')
	{
		i += 1;

		expr = parse_expr(
			src, &ptr[i], debug, &l);
		if (!expr)
		{
			if (ptr[i] != '*')
				return 0;
			l = 1;
		}
		i += l;

		if (ptr[i++] != ')')
		{
			parse_expr_delete(expr);
			parse_debug_rewind(debug, dpos);
			return 0;
		}
	}
	else
	{
		expr = parse_expr_number(
			src, &ptr[i], debug, &l);
		if (!expr) return 0;
		i += l;
		*count = expr;
		*is_variable = false;
	}

	*count = expr;
	*is_variable = (expr == NULL);
	return i;
}
