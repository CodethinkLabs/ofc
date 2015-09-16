#include "parse.h"

unsigned parse_dimension(
	const sparse_t* src, const char* ptr,
	hashmap_t* decl_map)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_DIMENSION);
	if (i == 0) return 0;

	unsigned c;
	for (c = 0; true; c++)
	{
		unsigned j = i;
		if (c > 0)
		{
			if (ptr[j] != ',')
				break;
			j += 1;
		}

		str_ref_t name;
		unsigned len = parse_name(
			src, &ptr[j], &name);
		if (len == 0) break;
		j += len;

		if (ptr[j++] != '(')
			break;

		parse_expr_t count;
		len = parse_expr(
			src, &ptr[j], &count);
		if (len == 0) break;
		j += len;

		if (ptr[j++] != ')')
		{
			parse_expr_cleanup(count);
			break;
		}

		parse_decl_t* existing
			= hashmap_find_modify(decl_map, &name);
		if (!existing)
		{
			if (c > 0)
			{
				sparse_error(src, &ptr[i + 1],
					"Implicit declarations in DIMENSION not supported");
			}

			/* TODO - Support implicit declarations in DIMENSION. */

			parse_expr_cleanup(count);
			break;
		}

		if (existing->type.count_expr)
		{
			parse_expr_cleanup(count);

			if (c > 0)
			{
				sparse_error(src, &ptr[i + 1],
					"Multi-dimensional arrays not yet supported");
			}

			/* TODO - Support multi-dimensional arrays. */

			break;
		}
		else
		{
			existing->type.count_expr
				= parse_expr_alloc(count);
			if (!existing->type.count_expr)
			{
				parse_expr_cleanup(count);
				break;
			}
		}

		i = j;
	}

	if (c == 0)
		return 0;

	/* TODO - Make this more atomic so that a
	          failure part-way through is recoverable. */

	if ((ptr[i] == '\r')
		|| (ptr[i] == '\n')
		|| (ptr[i] == ';'))
	{
		i += 1;
	}
	else
	{
		sparse_warning(src, &ptr[i],
			"Expected newline or semicolon after DIMENSION statement");
	}

	return i;
}
