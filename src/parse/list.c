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

#include "ofc/parse.h"


static unsigned ofc_parse_list__seperator_optional(
	const ofc_sparse_t* sparse, const char* ptr,
	ofc_parse_debug_t* debug,
	char seperator, bool optional,
	unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const ofc_sparse_t*, const char*, ofc_parse_debug_t*, unsigned*),
	void (*elem_delete)(void*))
{
	unsigned orig_count = *elem_count;
	unsigned max_count = *elem_count;

	unsigned i = 0;

	bool initial = true;
	while (true)
	{
		unsigned j = i;

		if (!initial && (seperator != '\0'))
		{
			if (ptr[j] == seperator)
				j += 1;
			else if (!optional)
				break;
		}
		initial = false;

		unsigned l;
		void* e = elem_parse(
			sparse, &ptr[j], debug, &l);
		if (!e) break;
		/* Do not allow assignments on the list */
		if (ptr[j+l] == '=')
		{
			elem_delete(e);
			break;
		}
		j += l;

		if (*elem_count >= max_count)
		{
			max_count <<= 1;
			if (max_count == 0)
				max_count = 4;
			void** nelem = realloc(*elem,
				(max_count * sizeof(void*)));
			if (!nelem)
			{
				elem_delete(e);

				unsigned n;
				for (n = orig_count; n < *elem_count; n++)
					elem_delete((*elem)[n]);
				*elem_count = orig_count;

				if (orig_count == 0)
				{
					free(*elem);
					*elem = NULL;
				}

				return 0;
			}
			*elem = nelem;
		}

		(*elem)[(*elem_count)++] = e;
		i = j;
	}

	return i;
}

unsigned ofc_parse_list(
	const ofc_sparse_t* sparse, const char* ptr,
	ofc_parse_debug_t* debug,
	char seperator, unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const ofc_sparse_t*, const char*, ofc_parse_debug_t*, unsigned*),
	void (*elem_delete)(void*))
{
	return ofc_parse_list__seperator_optional(
		sparse, ptr, debug, seperator, false,
		elem_count, elem, elem_parse, elem_delete);
}

unsigned ofc_parse_list_seperator_optional(
	const ofc_sparse_t* sparse, const char* ptr,
	ofc_parse_debug_t* debug,
	char seperator, unsigned* elem_count, void*** elem,
	void* (*elem_parse)(const ofc_sparse_t*, const char*, ofc_parse_debug_t*, unsigned*),
	void (*elem_delete)(void*))
{
	return ofc_parse_list__seperator_optional(
		sparse, ptr, debug, seperator, true,
		elem_count, elem, elem_parse, elem_delete);
}


bool ofc_parse_list_copy(
	unsigned* dst_count, void*** dst,
	unsigned  src_count, const void** src,
	void* (*elem_copy)(const void*),
	void (*elem_delete)(void*))
{
	if (!elem_copy || !src || !dst || !dst_count)
		return false;

	void** copy = (void**)malloc(
		src_count * sizeof(void*));
	if (!copy) return false;

	unsigned i;
	for (i = 0; i < src_count; i++)
	{
		copy[i] = elem_copy(src[i]);
		if (!copy[i])
		{
			if (elem_delete)
			{
				unsigned j;
				for (j = 0; j < i; j++)
					elem_delete(copy[j]);
			}
			free(copy);
			return false;
		}
	}

	*dst = copy;
	*dst_count = src_count;
	return true;
}

void ofc_parse_list_delete(
	unsigned elem_count, void** elem,
	void (*elem_delete)(void*))
{
	if (!elem)
		return;

	unsigned i;
	for (i = 0; i < elem_count; i++)
		elem_delete(elem[i]);
	free(elem);
}


bool ofc_parse_list_print(
	ofc_colstr_t* cs,
	unsigned elem_count, const void** elem,
	bool (*elem_print)(ofc_colstr_t*, const void*))
{
	if (!elem || !elem_print)
		return false;

	unsigned i;
	for (i = 0; i < elem_count; i++)
	{
		if ((i > 0) && !ofc_colstr_atomic_writef(cs, ", "))
			return false;

		if (!elem_print(cs, elem[i]))
			return false;
	}

	return true;
}
