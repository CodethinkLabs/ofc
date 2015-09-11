#include "parse.h"
#include <ctype.h>


unsigned parse_decl(
	const sparse_t* src, const char* ptr,
	parse_implicit_t* implicit,
	parse_decl_t* decl)
{
	parse_type_t type;
	unsigned i = parse_type(
		src, ptr, &type);

	bool is_implicit = (i == 0);

	str_ref_t name;
	unsigned len = parse_name(src, ptr);
	if (len == 0) return 0;

	name.base = &ptr[i];
	name.size = len;
	i += len;

	if (is_implicit)
	{
		if (!implicit)
			return 0;

		unsigned index = (toupper(name.base[0]) - 'A');
		if (index > 26) return 0;

		if (implicit->c[index].type == PARSE_TYPE_NONE)
			return 0;

		type = implicit->c[index];
	}

	parse_literal_t literal
		= PARSE_LITERAL_NONE_DEFAULT;
	if (ptr[i] == '=')
	{
		i += 1;

		len = parse_literal(
			src, &ptr[i], &literal);
		if (len == 0) return 0;

		i += len;
	}

	if ((ptr[i] == '\r')
		|| (ptr[i] == '\n'))
	{
		i += 1;
	}
	else
	{
		sparse_warning(src, &ptr[i],
			"Expected newline at end of declaration");
	}

	decl->type = type;
	decl->name = name;
	decl->init = literal;
	return i;
}
