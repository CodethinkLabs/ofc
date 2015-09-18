#include "../parse.h"

unsigned parse_stmt_dimension__entry(
	const sparse_t* src, const char* ptr,
	unsigned* count, str_ref_t** name,
	parse_expr_t*** dimension)
{
	str_ref_t ename;
	unsigned i = parse_name(src, ptr, &ename);
	if (i == 0) return 0;

	if (ptr[i++] != '(')
		return 0;

	parse_expr_t edim;
	unsigned len = parse_expr(src, &ptr[i], &edim);
	if (len == 0) return 0;
	i += len;

	if (ptr[i++] != ')')
	{
		parse_expr_cleanup(edim);
		return 0;
	}

	parse_expr_t* aedim = parse_expr_alloc(edim);
	if (!aedim)
	{
		parse_expr_cleanup(edim);
		return 0;
	}

	str_ref_t* nname
		= (str_ref_t*)realloc(*name,
			((*count + 1) * sizeof(str_ref_t)));
	if (nname) *name = nname;

	parse_expr_t** ndim
		= (parse_expr_t**)realloc(*dimension,
			((*count + 1) * sizeof(parse_expr_t*)));
	if (ndim) *dimension = ndim;

	if (!nname || !ndim)
	{
		parse_expr_delete(aedim);
		return 0;
	}

	(*name)[*count] = ename;
	(*dimension)[*count] = aedim;
	*count += 1;
	return i;
}

unsigned parse_stmt_dimension(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_DIMENSION);
	if (i == 0) return 0;

	stmt->type = PARSE_STMT_DIMENSION;
	stmt->dimension.count = 0;
	stmt->dimension.name = NULL;
	stmt->dimension.dimension = NULL;

	unsigned len = parse_stmt_dimension__entry(
		src, &ptr[i],
		&stmt->dimension.count,
		&stmt->dimension.name,
		&stmt->dimension.dimension);
	if (len == 0) return 0;
	i += len;

	while (ptr[i] == ',')
	{
		unsigned j = (i + 1);
		len = parse_stmt_dimension__entry(
			src, &ptr[j],
			&stmt->dimension.count,
			&stmt->dimension.name,
			&stmt->dimension.dimension);
		if (len == 0) break;
		i = (j + len);
	}

	return i;
}
