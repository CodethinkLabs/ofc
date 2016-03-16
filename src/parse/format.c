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
#include <stdlib.h>
#include <string.h>

#include "ofc/parse.h"


typedef struct
{
	const char*         name;
	ofc_parse_format_desc_e type;
	bool                w, d, e;
} ofc_parse_format_desc__map_t;


static const ofc_parse_format_desc__map_t ofc_parse_format_desc__map[] =
{
	{ "I" , OFC_PARSE_FORMAT_DESC_INTEGER   , 1, 1, 0 },
	{ "F" , OFC_PARSE_FORMAT_DESC_REAL      , 1, 1, 0 },
	{ "E" , OFC_PARSE_FORMAT_DESC_E         , 1, 1, 1 },
	{ "D" , OFC_PARSE_FORMAT_DESC_D         , 1, 1, 0 },
	{ "G" , OFC_PARSE_FORMAT_DESC_G         , 1, 1, 1 },
	{ "A" , OFC_PARSE_FORMAT_DESC_CHARACTER , 1, 0, 0 },
	{ "L" , OFC_PARSE_FORMAT_DESC_LOGICAL   , 1, 0, 0 },
	{ "BZ", OFC_PARSE_FORMAT_DESC_BZ        , 0, 0, 0 },
	{ "BN", OFC_PARSE_FORMAT_DESC_BN        , 0, 0, 0 },
	{ "SP", OFC_PARSE_FORMAT_DESC_SP        , 0, 0, 0 },
	{ "SS", OFC_PARSE_FORMAT_DESC_SS        , 0, 0, 0 },
	{ "S" , OFC_PARSE_FORMAT_DESC_S         , 0, 0, 0 },
	{ "P" , OFC_PARSE_FORMAT_DESC_REAL_SCALE, 0, 0, 0 },
	{ "X" , OFC_PARSE_FORMAT_DESC_X         , 0, 0, 0 },
	{ "TL", OFC_PARSE_FORMAT_DESC_TL        , 1, 0, 0 },
	{ "TR", OFC_PARSE_FORMAT_DESC_TR        , 1, 0, 0 },
	{ "T" , OFC_PARSE_FORMAT_DESC_T         , 1, 0, 0 },
	{ "/" , OFC_PARSE_FORMAT_DESC_SLASH     , 0, 0, 0 },
	{ ":" , OFC_PARSE_FORMAT_DESC_COLON     , 0, 0, 0 },

	/* Non standard. */
	{ "$" , OFC_PARSE_FORMAT_DESC_DOLLAR    , 0, 0, 0 },
	{ "\\", OFC_PARSE_FORMAT_DESC_BACKSLASH , 0, 0, 0 },
	{ "Q" , OFC_PARSE_FORMAT_DESC_Q         , 0, 0, 0 },

	/* Extended descriptors. */
	{ "B" , OFC_PARSE_FORMAT_DESC_BINARY    , 1, 1, 0 },
	{ "O" , OFC_PARSE_FORMAT_DESC_OCTAL     , 1, 1, 0 },
	{ "Z" , OFC_PARSE_FORMAT_DESC_HEX       , 1, 1, 0 },

	{ NULL, 0, 0, 0, 0 }
};

bool ofc_parse_format_desc_has_w(
	const ofc_parse_format_desc_t* desc)
{
	unsigned i;
	for (i = 0; ofc_parse_format_desc__map[i].name; i++)
	{
		if (ofc_parse_format_desc__map[i].type == desc->type)
			return ofc_parse_format_desc__map[i].w;
	}
	return false;
}

bool ofc_parse_format_desc_has_d(
	const ofc_parse_format_desc_t* desc)
{
	unsigned i;
	for (i = 0; ofc_parse_format_desc__map[i].name; i++)
	{
		if (ofc_parse_format_desc__map[i].type == desc->type)
			return ofc_parse_format_desc__map[i].d;
	}
	return false;
}

bool ofc_parse_format_desc_has_e(
	const ofc_parse_format_desc_t* desc)
{
	unsigned i;
	for (i = 0; ofc_parse_format_desc__map[i].name; i++)
	{
		if (ofc_parse_format_desc__map[i].type == desc->type)
			return ofc_parse_format_desc__map[i].e;
	}
	return false;
}

ofc_parse_format_desc_t* ofc_parse_format_desc(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_format_desc_t* desc
		= (ofc_parse_format_desc_t*)malloc(
			sizeof(ofc_parse_format_desc_t));
	if (!desc) return NULL;

	unsigned dpos = ofc_parse_debug_position(debug);

	desc->n_set = false;

	unsigned i = 0;
	desc->string = ofc_parse_hollerith(
		src, ptr, debug, &i);

	if (i > 0)
	{
		desc->src = ofc_sparse_ref(
			src, ptr, i);
		desc->neg  = false;
		desc->n    = 1;
		desc->type = OFC_PARSE_FORMAT_DESC_HOLLERITH;

		if (len) *len = i;
		return desc;
	}

	bool negative = (ptr[i] == '-');
	if (negative || (ptr[i] == '+'))
		i += 1;

	unsigned l, n = 1;
	l = ofc_parse_unsigned(
		src, &ptr[i], debug, &n);
	i = (l > 0 ? i + l : 0);
	desc->n_set = (l > 0);

	desc->string = ofc_parse_character(
		src, &ptr[i], debug, &l);
	if (desc->string)
	{
		desc->src = ofc_sparse_ref(
			src, ptr, i);
		desc->neg  = negative;
		desc->n    = n;
		desc->type = OFC_PARSE_FORMAT_DESC_STRING;

		if (len) *len = (i + l);
		return desc;
	}

	if (ptr[i] == '(')
	{
		i += 1;

		desc->src = ofc_sparse_ref(
			src, ptr, i);

		unsigned k;
		desc->repeat = ofc_parse_format_desc_list(
			src, &ptr[i], debug, &k);
		if (desc->repeat) i += k;

		desc->neg  = negative;
		desc->n    = (n == 0 ? 1 : n);
		desc->type = OFC_PARSE_FORMAT_DESC_REPEAT;

		if (ptr[i++] != ')')
		{
			ofc_parse_format_desc_list_delete(
				desc->repeat);
			free(desc);
			ofc_parse_debug_rewind(debug, dpos);
			return NULL;
		}

		if (len) *len = i;
		return desc;
	}

	unsigned m;
	for (m = 0; ofc_parse_format_desc__map[m].name; m++)
	{
		l = strlen(ofc_parse_format_desc__map[m].name);
		if (strncasecmp(&ptr[i],
			ofc_parse_format_desc__map[m].name, l) == 0)
			break;
	}
	ofc_parse_format_desc__map_t map
		= ofc_parse_format_desc__map[m];
	if (!map.name)
	{
		free(desc);
		ofc_parse_debug_rewind(debug, dpos);
		return NULL;
	}
	i += l;

	unsigned k;
	desc->w_set = false;
	desc->d_set = false;
	desc->e_set = false;

	unsigned w = 1;
	if (map.w)
	{
		k = ofc_parse_unsigned(
			src, &ptr[i], debug, &w);
		desc->w_set = (k > 0);

		i += k;
	}

	unsigned d = 0;
	if (map.d && (ptr[i] == '.'))
	{
		i += 1;
		k = ofc_parse_unsigned(
			src, &ptr[i], debug, &d);
		desc->d_set = (k > 0);

		i += k;
	}

	unsigned e = 0;
	if (map.e && (toupper(ptr[i]) == 'E'))
	{
		i += 1;
		k = ofc_parse_unsigned(
			src, &ptr[i], debug, &e);
		desc->e_set = (k > 0);

		i += k;
	}

	desc->src = ofc_sparse_ref(
		src, ptr, i);
	desc->type = map.type;
	desc->neg  = negative;
	desc->n = n;
	desc->w = w;
	desc->d = d;
	desc->e = e;

	if (len) *len = i;
	return desc;
}

bool ofc_parse_format_desc_compare(
	const ofc_parse_format_desc_t* a,
	const ofc_parse_format_desc_t* b)
{
	if (!a || !b) return false;

	if (a->type != b->type) return false;

	if (a->n != b->n) return false;

	if (a->type == OFC_PARSE_FORMAT_DESC_STRING)
		return ofc_string_equal(*a->string, *b->string);

	if (a->type == OFC_PARSE_FORMAT_DESC_REPEAT)
	{
		if (a->repeat->count != b->repeat->count)
			return false;

		unsigned i;
		for (i = 0; i < a->repeat->count; i++)
		{
			if (!ofc_parse_format_desc_compare(
				a->repeat->desc[i], b->repeat->desc[i]))
				return false;
		}
		return true;
	}

	if ((a->w != b->w)
		|| (a->d != b->d)
		|| (a->e != b->e))
		return false;

	return true;
}

void ofc_parse_format_desc_delete(
	ofc_parse_format_desc_t* desc)
{
	if (!desc)
		return;

	switch (desc->type)
	{
		case OFC_PARSE_FORMAT_DESC_HOLLERITH:
		case OFC_PARSE_FORMAT_DESC_STRING:
			ofc_string_delete(desc->string);
			break;
		case OFC_PARSE_FORMAT_DESC_REPEAT:
			ofc_parse_format_desc_list_delete(desc->repeat);
			break;
		default:
			break;
	}
	free(desc);
}

bool ofc_parse_format_desc_elem_count(
	const ofc_parse_format_desc_t* desc,
	unsigned* count)
{
	if (!desc)
		return false;

	if (desc->type == OFC_PARSE_FORMAT_DESC_REPEAT)
	{
		unsigned counter;
		if (!ofc_parse_format_desc_list_elem_count(
			desc->repeat, &counter))
			return false;

		unsigned times = (desc->n_set ? desc->n : 1);
		if (count) *count = times * counter;
	}
	else if (ofc_parse_format_is_data_desc(desc))
	{
		if (count) *count = (desc->n_set ? desc->n : 1);
	}
	else
	{
		if (count) *count = 1;
	}

	return true;
}

ofc_parse_format_desc_t* ofc_parse_format_desc_elem_get(
	const ofc_parse_format_desc_t* desc, unsigned offset)
{
	if (!desc)
		return NULL;

	if (desc->type == OFC_PARSE_FORMAT_DESC_REPEAT)
		return ofc_parse_format_desc_list_elem_get(desc->repeat, offset);

	return ofc_parse_format_desc_copy(desc);
}

const char* ofc_parse_format_desc__name[] =
{
	"I",
	"F",
	"D",
	"E",
	"G",
	"A",
	"L",
	NULL,
	"S",
	"P",
	"X",
	"T",
	"/",
	"$",
	"\\",
	"Q",
	":",
	"BN",
	"BZ",
	"SP",
	"SS",
	"TL",
	"TR",
	NULL,
	NULL,
	"B",
	"O",
	"Z",
};


bool ofc_parse_format_desc_print(
	ofc_colstr_t* cs, const ofc_parse_format_desc_t* desc)
{
	if (!desc)
		return false;

	switch (desc->type)
	{
		case OFC_PARSE_FORMAT_DESC_HOLLERITH:
			if (ofc_string_empty(desc->string))
				return false;
			if (!ofc_colstr_atomic_writef(cs, "%uH%s",
				ofc_string_length(desc->string),
				ofc_string_strz(desc->string)))
				return false;
			break;
		case OFC_PARSE_FORMAT_DESC_STRING:
			if (!ofc_colstr_write_escaped(cs, '\"',
					ofc_string_strz(desc->string),
					ofc_string_length(desc->string)))
				return false;
			break;
		case OFC_PARSE_FORMAT_DESC_REPEAT:
			if ((desc->n > 1)
				&& !ofc_colstr_atomic_writef(cs, "%u", desc->n))
				return false;
			if (!ofc_colstr_atomic_writef(cs, "(")
				|| !ofc_parse_format_desc_list_print(
					cs, desc->repeat)
				|| !ofc_colstr_atomic_writef(cs, ")"))
				return false;
			break;
		case OFC_PARSE_FORMAT_DESC_REAL_SCALE:
			if (desc->neg
				&& !ofc_colstr_atomic_writef(cs, "-"))
				return false;
			if (desc->n_set
				&& !ofc_colstr_atomic_writef(cs, "%u", desc->n))
				return false;
			if (!ofc_colstr_atomic_writef(cs, "P"))
				return false;
			break;
		case OFC_PARSE_FORMAT_DESC_X:
			if (desc->n_set
				&& !ofc_colstr_atomic_writef(cs, "%u", desc->n))
				return false;
			if (!ofc_colstr_atomic_writef(cs, "X"))
				return false;
			break;
		default:
			if ((desc->n > 1)
				&& !ofc_colstr_atomic_writef(cs, "%u", desc->n))
				return false;
			if (!ofc_colstr_atomic_writef(cs, "%s",
				ofc_parse_format_desc__name[desc->type]))
				return false;
			if (desc->w_set
				&& !ofc_colstr_atomic_writef(cs, "%u", desc->w))
				return false;
			if (desc->d_set
				&& !ofc_colstr_atomic_writef(cs, ".%u", desc->d))
				return false;
			if (desc->e_set
				&& !ofc_colstr_atomic_writef(cs, "E%u", desc->e))
				return false;
			break;
	}

	return true;
}

static void ofc_parse_format_desc__cleanup(
	ofc_parse_format_desc_t desc)
{
	switch (desc.type)
	{
		case OFC_PARSE_FORMAT_DESC_STRING:
		case OFC_PARSE_FORMAT_DESC_HOLLERITH:
			ofc_string_delete(desc.string);
			break;
		case OFC_PARSE_FORMAT_DESC_REPEAT:
			ofc_parse_format_desc_list_delete(desc.repeat);
			break;

		default:
			break;
	}
}

static ofc_parse_format_desc_t* ofc_parse_format_desc__create(
	ofc_parse_format_desc_e type)
{
	if (type >= OFC_PARSE_FORMAT_DESC_COUNT)
		return NULL;

	ofc_parse_format_desc_t* desc
		= (ofc_parse_format_desc_t*)malloc(
			sizeof(ofc_parse_format_desc_t));
	if (!desc) return NULL;

	desc->type = type;
	desc->src  = OFC_SPARSE_REF_EMPTY;

	switch(desc->type)
	{
		case OFC_PARSE_FORMAT_DESC_STRING:
		case OFC_PARSE_FORMAT_DESC_HOLLERITH:
			desc->string = NULL;
			break;
		case OFC_PARSE_FORMAT_DESC_REPEAT:
			desc->repeat = NULL;
			break;
		default:
			break;
	}

	return desc;
}

static bool ofc_parse_format_desc__clone(
	ofc_parse_format_desc_t* dst, const ofc_parse_format_desc_t* src)
{
	if (!src || !dst)
		return false;

	ofc_parse_format_desc_t clone = *src;
	switch(clone.type)
	{
		case OFC_PARSE_FORMAT_DESC_STRING:
		case OFC_PARSE_FORMAT_DESC_HOLLERITH:
			clone.string = ofc_string_copy(src->string);
			if (src->string && !clone.string)
				return false;
			break;
		case OFC_PARSE_FORMAT_DESC_REPEAT:
			clone.repeat = ofc_parse_format_desc_list_copy(src->repeat);
			if (src->repeat && !clone.repeat)
				return false;
			break;
		default:
			break;
	}
	*dst = clone;
	return true;
}

static ofc_parse_format_desc_t* ofc_parse_format_desc__alloc(
	ofc_parse_format_desc_t desc)
{
	ofc_parse_format_desc_t* adesc
		= (ofc_parse_format_desc_t*)malloc(
			sizeof(ofc_parse_format_desc_t));
	if (!adesc) return NULL;
	*adesc = desc;
	return adesc;
}

ofc_parse_format_desc_t* ofc_parse_format_desc_create_repeat(
	ofc_parse_format_desc_list_t* list, unsigned n)
{
	ofc_parse_format_desc_t* desc
		= ofc_parse_format_desc__create(
			OFC_PARSE_FORMAT_DESC_REPEAT);
	if (!desc) return NULL;

	desc->n_set  = true;
	desc->n      = n;
	desc->repeat = list;

	return desc;
}

ofc_parse_format_desc_t* ofc_parse_format_desc_copy(
	const ofc_parse_format_desc_t* desc)
{
	ofc_parse_format_desc_t copy;
	if (!ofc_parse_format_desc__clone(&copy, desc))
		return NULL;

	ofc_parse_format_desc_t* acopy
		= ofc_parse_format_desc__alloc(copy);
	if (!acopy)
		ofc_parse_format_desc__cleanup(copy);
	return acopy;
}

ofc_parse_format_desc_list_t* ofc_parse_format_desc_list_copy(
	const ofc_parse_format_desc_list_t* list)
{
	if (!list)
		return NULL;

	ofc_parse_format_desc_list_t* copy
		= (ofc_parse_format_desc_list_t*)malloc(
			sizeof(ofc_parse_format_desc_list_t));
	if (!copy) return NULL;

	copy->count = 0;
	copy->desc = NULL;

	if (!ofc_parse_list_copy(
		&copy->count, (void***)&copy->desc,
		list->count, (const void**)list->desc,
		(void*)ofc_parse_format_desc_copy,
		(void*)ofc_parse_format_desc_delete))
	{
		free(copy);
		return NULL;
	}

	return copy;
}

ofc_parse_format_desc_list_t* ofc_parse_format_desc_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_parse_format_desc_list_t* list
		= (ofc_parse_format_desc_list_t*)malloc(
			sizeof(ofc_parse_format_desc_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->desc  = NULL;

	unsigned i = ofc_parse_list_seperator_optional(
		src, ptr, debug, ',',
		&list->count, (void***)&list->desc,
		(void*)ofc_parse_format_desc,
		(void*)ofc_parse_format_desc_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}


void ofc_parse_format_desc_list_delete(
	ofc_parse_format_desc_list_t* list)
{
	if (!list)
		return;

	ofc_parse_list_delete(
		list->count, (void**)list->desc,
		(void*)ofc_parse_format_desc_delete);
	free(list);
}

bool ofc_parse_format_desc_list_print(
	ofc_colstr_t* cs, const ofc_parse_format_desc_list_t* list)
{
	if (!list)
		return false;

	return ofc_parse_list_print(cs,
		list->count, (const void**)list->desc,
		(void*)ofc_parse_format_desc_print);


}

ofc_parse_format_desc_list_t* ofc_parse_format_desc_list_create(void)
{
	ofc_parse_format_desc_list_t* list
		= (ofc_parse_format_desc_list_t*)malloc(
			sizeof(ofc_parse_format_desc_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->desc  = NULL;
	return list;
}

bool ofc_parse_format_desc_list_add(
	ofc_parse_format_desc_list_t* list,
	ofc_parse_format_desc_t* desc)
{
	if (!list || !desc)
		return false;

	ofc_parse_format_desc_t** ndesc
		= (ofc_parse_format_desc_t**)realloc(list->desc,
			(sizeof(ofc_parse_format_desc_t*) * (list->count + 1)));
	if (!ndesc) return NULL;

	list->desc = ndesc;
	list->desc[list->count++] = desc;
	return true;
}

ofc_parse_format_desc_t* ofc_parse_format_desc_list_elem_get(
	const ofc_parse_format_desc_list_t* list, unsigned offset)
{
	if (!list)
		return NULL;

	unsigned e = offset;
	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		ofc_parse_format_desc_t* desc
			= list->desc[i];

		unsigned elem_count;
		if (!ofc_parse_format_desc_elem_count(
			desc, &elem_count))
			return NULL;

		if (e < elem_count)
		{
			if (elem_count == 1
				&& desc->type != OFC_PARSE_FORMAT_DESC_REPEAT)
				return ofc_parse_format_desc_copy(desc);

			if (desc->type == OFC_PARSE_FORMAT_DESC_REPEAT)
			{
				if (!ofc_parse_format_desc_list_elem_count(
					desc->repeat, &elem_count))
					return NULL;
			}

			return ofc_parse_format_desc_elem_get(
				desc, (e % elem_count));
		}
		else
		{
			e -= elem_count;
		}
	}

	return NULL;
}

bool ofc_parse_format_desc_list_elem_count(
	const ofc_parse_format_desc_list_t* list,
	unsigned* count)
{
	if (!list)
		return false;

	unsigned i, re_count, counter = 0;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_parse_format_desc_elem_count(
			list->desc[i], &re_count))
			return false;
		counter += re_count;
	}

	if (count) *count = counter;

	return true;
}

bool ofc_parse_format_is_data_desc(
	const ofc_parse_format_desc_t* desc)
{
	if (!desc)
		return false;

	switch(desc->type)
	{
		case OFC_PARSE_FORMAT_DESC_INTEGER:
		case OFC_PARSE_FORMAT_DESC_BINARY:
		case OFC_PARSE_FORMAT_DESC_OCTAL:
		case OFC_PARSE_FORMAT_DESC_HEX:
		case OFC_PARSE_FORMAT_DESC_REAL:
		case OFC_PARSE_FORMAT_DESC_D:
		case OFC_PARSE_FORMAT_DESC_E:
		case OFC_PARSE_FORMAT_DESC_G:
		case OFC_PARSE_FORMAT_DESC_CHARACTER:
		case OFC_PARSE_FORMAT_DESC_LOGICAL:
			return true;
		default:
			break;
	}

	return false;
}
