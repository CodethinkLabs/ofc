#include "str_ref.h"
#include <string.h>


bool str_ref_empty(const str_ref_t ref)
{
	return (ref.size == 0);
}

bool str_ref_equal(const str_ref_t a, const str_ref_t b)
{
	if (a.size != b.size)
		return false;

	if (a.base == b.base)
		return true;

	return (strncmp(a.base, b.base, a.size) == 0);
}

bool str_ref_equal_ci(const str_ref_t a, const str_ref_t b)
{
	if (a.size != b.size)
		return false;

	if (a.base == b.base)
		return true;

	return (strncasecmp(a.base, b.base, a.size) == 0);
}
