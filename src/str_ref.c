#include <ofc/str_ref.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>


bool ofc_str_ref_empty(const ofc_str_ref_t ref)
{
	return (ref.size == 0);
}

uint8_t ofc_str_ref_hash(const ofc_str_ref_t ref)
{
	if (!ref.base)
		return 0;

	uint8_t hash;
	unsigned i;
	for (i = 0, hash = 0; i < ref.size; i++)
		hash += ref.base[i];
	return hash;
}

uint8_t ofc_str_ref_hash_ci(const ofc_str_ref_t ref)
{
	if (!ref.base)
		return 0;

	uint8_t hash;
	unsigned i;
	for (i = 0, hash = 0; i < ref.size; i++)
		hash += toupper(ref.base[i]);
	return hash;
}

bool ofc_str_ref_bridge(
	ofc_str_ref_t start, ofc_str_ref_t end,
	ofc_str_ref_t* ret)
{
	if (end.base > start.base)
		return false;

    ofc_str_ref_t result =
	{
		.base = start.base,
		.size = (end.base - start.base) + end.size
	};

	*ret = result;
	return true;
}

bool ofc_str_ref_equal(const ofc_str_ref_t a, const ofc_str_ref_t b)
{
	if (a.size != b.size)
		return false;

	if (a.base == b.base)
		return true;

	return (strncmp(a.base, b.base, a.size) == 0);
}

bool ofc_str_ref_equal_ci(const ofc_str_ref_t a, const ofc_str_ref_t b)
{
	if (a.size != b.size)
		return false;

	if (a.base == b.base)
		return true;

	return (strncasecmp(a.base, b.base, a.size) == 0);
}

bool ofc_str_ref_equal_strz(
	const ofc_str_ref_t a,
	const char* b)
{
	return ofc_str_ref_equal(
		a, ofc_str_ref_from_strz(b));
}

bool ofc_str_ref_equal_strz_ci(
	const ofc_str_ref_t a,
	const char* b)
{
	return ofc_str_ref_equal_ci(
		a, ofc_str_ref_from_strz(b));
}

bool ofc_str_ref_print(ofc_colstr_t* cs, const ofc_str_ref_t str_ref)
{
	return ofc_colstr_atomic_writef(cs, "%.*s",
		str_ref.size, str_ref.base);
}
