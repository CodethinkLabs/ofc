#include "../parse.h"


static unsigned parse_stmt__decl(
	const sparse_t* src, const char* ptr,
	parse_stmt_decl_t* entry)
{
	unsigned i = parse_name(
		src, ptr, &entry->name);
	if (i == 0) return 0;

	entry->dimension = NULL;
	if (ptr[i] == '(')
	{
		i += 1;

		unsigned len;
		entry->dimension = parse_expr(
			src, &ptr[i], &len);
		if (!entry->dimension) return 0;
		i += len;

		if (ptr[i++] != ')')
		{
			parse_expr_delete(entry->dimension);
			return 0;
		}
	}

	entry->init = NULL;
	if (ptr[i] == '=')
	{
		i += 1;

		unsigned len;
		entry->init = parse_expr(
			src, &ptr[i], &len);
		if (!entry->init)
		{
			parse_expr_delete(entry->dimension);
			return 0;
		}
		i += len;
	}

	return i;
}

static unsigned parse_stmt__decl_entry(
	const sparse_t* src, const char* ptr,
	unsigned* count, unsigned* max_count,
	parse_stmt_decl_t** entry)
{
	parse_stmt_decl_t e;
	unsigned i = parse_stmt__decl(src, ptr, &e);
	if (i == 0) return 0;

	if (*count >= *max_count)
	{
		unsigned mc = (*max_count << 1);
		if (mc == 0) mc = 4;
		parse_stmt_decl_t* nentry
			= (parse_stmt_decl_t*)realloc(*entry,
				(mc * sizeof(parse_stmt_decl_t)));
		if (!nentry)
		{
			parse_expr_delete(e.dimension);
			parse_expr_delete(e.init);
			return 0;
		}
		*entry = nentry;
		*max_count = mc;
	}

	(*entry)[(*count)++] = e;

	if (ptr[i] == ',')
	{
		unsigned len = parse_stmt__decl_entry(
			src, &ptr[i + 1], count, max_count, entry);
		if (len > 0) i += (1 + len);
	}

	return i;
}

unsigned parse_stmt_decl(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	parse_type_t type;
	unsigned i = parse_type(
		src, ptr, &type);
	if (i == 0) return 0;

	stmt->decl.type = parse_type_alloc(type);
	if (!stmt->decl.type)
	{
		parse_type_cleanup(type);
		return 0;
	}

	stmt->type = PARSE_STMT_DECL;
	stmt->decl.count = 0;
	stmt->decl.entry = NULL;

	unsigned max_count = 0;

	unsigned len = parse_stmt__decl_entry(
		src, &ptr[i], &stmt->decl.count, &max_count, &stmt->decl.entry);
	if (len == 0)
	{
		parse_type_delete(stmt->decl.type);
		return 0;
	}
	i += len;

	return i;
}
