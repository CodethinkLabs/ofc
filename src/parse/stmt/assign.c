#include "../parse.h"

unsigned parse_stmt_assignment(
	const sparse_t* src, const char* ptr,
	const parse_implicit_t* implicit,
	hashmap_t* decl,
	parse_stmt_t* stmt)
{
	unsigned i = parse_lhs(src, ptr,
		&stmt->assignment.lhs);
	if (i == 0) return 0;

	if (ptr[i++] != '=')
		return 0;

	unsigned len = parse_expr(
		src, &ptr[i], &stmt->assignment.rhs);
	if (len == 0)
	{
		sparse_warning(src, &ptr[i],
			"Expected expression on right hand side of assignment");
		return 0;
	}
	i += len;

	stmt->type = PARSE_STMT_ASSIGNMENT;

	str_ref_t base_name;
	if (parse_lhs_base_name(stmt->assignment.lhs, &base_name)
		&& !hashmap_find(decl, &base_name))
	{
		parse_decl_t idecl;
		if (!parse_decl_create_implicit(
			base_name, implicit, &idecl))
		{
			sparse_error(src, ptr,
				"Failed to create implicit declaration for variable '%.*s'",
				base_name.size, base_name.base);
			parse_stmt_cleanup(*stmt);
			return 0;
		}

		parse_decl_t* aidecl
			= parse_decl_alloc(idecl);
		if (!aidecl)
		{
			parse_decl_cleanup(idecl);
			parse_stmt_cleanup(*stmt);
			return 0;
		}

		if (!hashmap_add(decl, aidecl))
		{
			parse_decl_delete(aidecl);
			parse_stmt_cleanup(*stmt);
			return 0;
		}
	}

	return i;
}

unsigned parse_stmt_assign(
	const sparse_t* src, const char* ptr,
	parse_stmt_t* stmt)
{
	unsigned i = parse_keyword(
		src, ptr, PARSE_KEYWORD_ASSIGN);
	if (i == 0) return 0;

	unsigned len = parse_unsigned(
		src, &ptr[i], &stmt->assign.label);
	if (len == 0) return 0;
	i += len;

	len = parse_keyword(
		src, &ptr[i], PARSE_KEYWORD_TO);
	if (len == 0) return 0;
	i += 2;

	len = parse_name(src, &ptr[i],
		&stmt->assign.variable);
	if (len == 0) return 0;
	i += len;

	stmt->type = PARSE_STMT_ASSIGN;
	return i;
}
