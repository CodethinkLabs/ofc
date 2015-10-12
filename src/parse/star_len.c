#include <ofc/parse.h>


unsigned ofc_parse_star_len(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_expr_t** count, bool* is_variable)
{
	unsigned i = 0;

	if (ptr[i++] != '*')
		return 0;

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned l;
	ofc_parse_expr_t* expr;
	if (ptr[i] == '(')
	{
		i += 1;

		expr = ofc_parse_expr(
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
			ofc_parse_expr_delete(expr);
			ofc_parse_debug_rewind(debug, dpos);
			return 0;
		}
	}
	else
	{
		expr = ofc_parse_expr_integer(
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
