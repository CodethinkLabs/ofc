#include <ofc/fctype.h>

bool ofc_is_vspace(char c)
{
	return ((c == '\r') || (c == '\n'));
}

bool ofc_is_hspace(char c)
{
	switch (c)
	{
		case ' ':
		case '\t':
		case '\f':
		case '\v':
			return true;
		default:
			break;
	}
	return false;
}

bool ofc_is_ident(char c)
{
	return (isalnum(c) || (c == '_'));
}

bool ofc_is_end_statement(const char* c, unsigned* len)
{
	if (ofc_is_vspace(c[0]) || (c[0] == ';'))
	{
		if (len) *len = 1;
		return true;
	}

	if (len) *len = 0;
	return (c[0] == '\0');
}
