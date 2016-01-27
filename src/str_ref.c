/* Copyright 2015 Codethink Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "ofc/str_ref.h"


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

ofc_str_ref_t ofc_str_ref_bridge(
	ofc_str_ref_t start, ofc_str_ref_t end)
{
	if ((start.base == NULL)
		|| (start.size == 0))
		return end;
	if ((end.base == NULL)
		|| (end.size == 0))
		return start;

	ofc_str_ref_t result =
	{
		.base = start.base,
		.size = ((uintptr_t)end.base - (uintptr_t)start.base) + end.size
	};

	return result;
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
