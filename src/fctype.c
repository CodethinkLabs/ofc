#include "fctype.h"

bool is_vspace(char c)
{
	return ((c == '\r') || (c == '\n'));
}

bool is_hspace(char c)
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

bool is_ident(char c)
{
	return (isalnum(c) || (c == '_'));
}
