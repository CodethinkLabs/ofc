#include "parse.h"
#include <ctype.h>


unsigned parse_decl(
	const sparse_t* src, const char* ptr,
	parse_implicit_t* implicit,
	parse_decl_t* decl)
{
	unsigned i = parse_type(
		src, ptr, &decl->type);

	bool is_implicit = (i == 0);

	unsigned len = parse_name(src, ptr);
	if (len == 0) return 0;

	decl->name.base = &ptr[i];
	decl->name.size = len;
	i += len;

	if (is_implicit)
	{
		if (!implicit)
			return 0;

		unsigned index = (toupper(decl->name.base[0]) - 'A');
		if (index > 26) return 0;

		if (implicit->c[index].type == PARSE_TYPE_NONE)
			return 0;

		decl->type = implicit->c[index];
	}

	decl->has_init = (ptr[i] == '=');
	if (decl->has_init)
	{
		i += 1;

		len = parse_expr(
			src, &ptr[i], &decl->init);
		if (len == 0) return 0;

		i += len;
	}

	if ((ptr[i] == '\r')
		|| (ptr[i] == '\n')
		|| (ptr[i] == ';'))
	{
		i += 1;
	}
	else
	{
		sparse_warning(src, &ptr[i],
			"Expected newline or semicolon at end of declaration");
	}

	return i;
}
