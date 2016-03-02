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


static unsigned ofc_parse_implicit__mask(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	uint32_t* mask)
{
	if (!isalpha(ptr[0]))
		return false;

	unsigned len = 1;
	char start = toupper(ptr[0]);
	char end   = start;

	if (ptr[len] == '-')
	{
		len += 1;
		end = 'Z';

		if (isalpha(ptr[len]))
			end = toupper(ptr[len++]);

		if (end < start)
		{
			ofc_parse_debug_warning(debug,
				ofc_sparse_ref(src, ptr, len),
				"Implicit character rule backwards");

			unsigned swap = start;
			start = end; end = swap;
		}

		if (end == start)
		{
			ofc_parse_debug_warning(debug,
				ofc_sparse_ref(src, ptr, len),
				"Implicit rule has redundant range");
		}
	}

	char j;
	uint32_t m;
	for (j = start, m = (1U << (start - 'A')); j <= end; j++, m <<= 1)
		*mask |= m;
	return len;
}

static unsigned ofc_parse_implicit__mask_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	uint32_t* mask)
{
	unsigned i = 0;
	if (ptr[i++] != '(')
		return 0;

	unsigned dpos = ofc_parse_debug_position(debug);

	uint32_t m = 0;
	bool initial;
	for (initial = true; initial || (ptr[i] == ','); initial = false)
	{
		unsigned j = i + (initial ? 0 : 1);
		unsigned len = ofc_parse_implicit__mask(
			src, &ptr[j], debug, &m);
		if (len == 0) break;
		i = j + len;
	}

	if (ptr[i++] != ')')
	{
		ofc_parse_debug_rewind(debug, dpos);
		return 0;
	}

	if (m == 0)
	{
		ofc_parse_debug_warning(debug,
			ofc_sparse_ref(src, ptr, i),
			"Implicit rule is empty");
	}

	if (mask) *mask = m;
	return i;
}

ofc_parse_implicit_t* ofc_parse_implicit(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_implicit_t implicit =
	{
		.undefined = false,
		.type = NULL,
		.attr = ofc_parse_decl_attr_default,
		.mask = 0,
	};

	unsigned dpos = ofc_parse_debug_position(debug);

	unsigned i = ofc_parse_keyword(
		src, ptr, debug, OFC_PARSE_KEYWORD_UNDEFINED);
	implicit.undefined = (i != 0);
	if (i == 0)
	{
		i = ofc_parse_decl_attr(
			src, ptr, debug, &implicit.attr);
	}
	if (i == 0)
	{
		implicit.type = ofc_parse_type(src, ptr, debug, &i);
		if (!implicit.type) return NULL;
	}

	unsigned l = ofc_parse_implicit__mask_list(
		src, &ptr[i], debug, &implicit.mask);
	if (l == 0)
	{
		ofc_parse_type_delete(implicit.type);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	ofc_parse_implicit_t* aimplicit
		= (ofc_parse_implicit_t*)malloc(
			sizeof(ofc_parse_implicit_t));
	if (!aimplicit)
	{
		ofc_parse_type_delete(implicit.type);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	*aimplicit = implicit;

	if (len) *len = i;
	return aimplicit;
}

void ofc_parse_implicit_delete(
	ofc_parse_implicit_t* implicit)
{
	if (!implicit)
		return;

	ofc_parse_type_delete(implicit->type);
	free(implicit);
}

bool ofc_parse_implicit_print(
	ofc_colstr_t* cs, const ofc_parse_implicit_t* implicit)
{
	if (!implicit)
		return false;

	if (!ofc_parse_decl_attr_print(
		cs, &implicit->attr))
		return false;

	if (implicit->type && !ofc_parse_type_print_f77(
		cs, implicit->type))
		return false;

	if (!ofc_colstr_atomic_writef(cs, " ("))
		return false;

	bool first = true;
	bool on = false;
	unsigned i, m;
	for (i = 0, m = 1; i < 26; i++, m <<= 1)
	{
		if ((implicit->mask & m) == 0)
			continue;

		if (on)
		{
			on = ((implicit->mask & (m << 1)) != 0);
			if (!on && !ofc_colstr_atomic_writef(cs, "%c", ('A' + i)))
				return false;
		}
		else
		{
			if (!first && !ofc_colstr_atomic_writef(cs, ", "))
				return false;

			if (!ofc_colstr_atomic_writef(cs, "%c", ('A' + i)))
				return false;

			on = ((implicit->mask & (m << 1)) != 0);
			if (on && !ofc_colstr_atomic_writef(cs, "-"))
				return false;
		}

		first = false;
	}

	return ofc_colstr_atomic_writef(cs, ")");
}


ofc_parse_implicit_list_t* ofc_parse_implicit_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_implicit_list_t* list
		= (ofc_parse_implicit_list_t*)malloc(
			sizeof(ofc_parse_implicit_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->rule = NULL;
	unsigned i = ofc_parse_list(
		src, ptr, debug, ',',
		&list->count, (void***)&list->rule,
		(void*)ofc_parse_implicit,
		(void*)ofc_parse_implicit_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void ofc_parse_implicit_list_delete(
	ofc_parse_implicit_list_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->rule,
		(void*)ofc_parse_implicit_delete);
	free(list);
}

bool ofc_parse_implicit_list_print(
	ofc_colstr_t* cs, const ofc_parse_implicit_list_t* list)
{
	if (!list)
		return false;

	return ofc_parse_list_print(cs,
		list->count, (const void**)list->rule,
		(void*)ofc_parse_implicit_print);
}
