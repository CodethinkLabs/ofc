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

#include <ofc/parse.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


typedef struct
{
	const char*         name;
	ofc_parse_format_desc_e type;
	bool                w, d, e;
} ofc_parse_format_desc__map_t;


static const ofc_parse_format_desc__map_t ofc_parse_format_desc__map[] =
{
	{ "I" , OFC_PARSE_FORMAT_DESC_INTEGER   , 1, 1, 0 },
	{ "F" , OFC_PARSE_FORMAT_DESC_REAL      , 1, 1, 1 },
	{ "E" , OFC_PARSE_FORMAT_DESC_E         , 1, 1, 1 },
	{ "D" , OFC_PARSE_FORMAT_DESC_D         , 1, 1, 1 },
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

	unsigned i = 0;
	desc->string = ofc_parse_hollerith(
		src, ptr, debug, &i);

	if (i > 0)
	{
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

	desc->string = ofc_parse_character(
		src, &ptr[i], debug, &l);
	if (desc->string)
	{
		desc->neg  = negative;
		desc->n    = n;
		desc->type = OFC_PARSE_FORMAT_DESC_STRING;

		if (len) *len = (i + l);
		return desc;
	}

	if (ptr[i] == '(')
	{
		i += 1;

		unsigned l;
		desc->repeat = ofc_parse_format_desc_list(
			src, &ptr[i], debug, &l);
		if (desc->repeat) i += l;

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

	unsigned w = 0;
	if (map.w)
	{
		i += ofc_parse_unsigned(
			src, &ptr[i], debug, &w);
	}

	unsigned d = 0;
	if (map.d && (ptr[i] == '.'))
	{
		i += 1;
		i += ofc_parse_unsigned(
			src, &ptr[i], debug, &d);
	}

	unsigned e = 0;
	if (map.e && (toupper(ptr[i]) == 'E'))
	{
		i += 1;
		i += ofc_parse_unsigned(
			src, &ptr[i], debug, &e);
	}

	desc->type = map.type;
	desc->neg = negative;
	desc->n = n;
	desc->w = w;
	desc->d = d;
	desc->e = e;

	if (len) *len = i;
	return desc;
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


static bool ofc_parse_format_desc_print__w(
	ofc_parse_format_desc_e type, unsigned w)
{
	switch (type)
	{
		case OFC_PARSE_FORMAT_DESC_CHARACTER:
			return (w > 0);
		case OFC_PARSE_FORMAT_DESC_INTEGER:
		case OFC_PARSE_FORMAT_DESC_REAL:
		case OFC_PARSE_FORMAT_DESC_D:
		case OFC_PARSE_FORMAT_DESC_E:
		case OFC_PARSE_FORMAT_DESC_G:
		case OFC_PARSE_FORMAT_DESC_LOGICAL:
		case OFC_PARSE_FORMAT_DESC_REAL_SCALE:
		case OFC_PARSE_FORMAT_DESC_T:
		case OFC_PARSE_FORMAT_DESC_TL:
		case OFC_PARSE_FORMAT_DESC_TR:
		case OFC_PARSE_FORMAT_DESC_BINARY:
		case OFC_PARSE_FORMAT_DESC_OCTAL:
		case OFC_PARSE_FORMAT_DESC_HEX:
			return true;
		default:
			break;
	}

	return false;
}

static bool ofc_parse_format_desc_print__d(
	ofc_parse_format_desc_e type, unsigned d)
{
	switch (type)
	{
		case OFC_PARSE_FORMAT_DESC_REAL:
		case OFC_PARSE_FORMAT_DESC_D:
		case OFC_PARSE_FORMAT_DESC_E:
		case OFC_PARSE_FORMAT_DESC_G:
			return true;
		case OFC_PARSE_FORMAT_DESC_INTEGER:
		case OFC_PARSE_FORMAT_DESC_BINARY:
		case OFC_PARSE_FORMAT_DESC_OCTAL:
		case OFC_PARSE_FORMAT_DESC_HEX:
			return (d > 0);
		default:
			break;
	}

	return false;
}

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
			if (!ofc_colstr_writef(cs, "\""))
				return false;
			if (!ofc_string_empty(desc->string)
				&& !ofc_colstr_write_escaped(cs,
					ofc_string_strz(desc->string),
					ofc_string_length(desc->string)))
				return false;
			if (!ofc_colstr_writef(cs, "\""))
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
			if (!ofc_colstr_atomic_writef(cs, "%s%uP",
				(desc->neg ? "-" : ""), desc->n))
				return false;
			break;
		case OFC_PARSE_FORMAT_DESC_X:
			if (!ofc_colstr_atomic_writef(cs, "%uX", desc->n))
				return false;
			break;
		default:
			if ((desc->n > 1)
				&& !ofc_colstr_atomic_writef(cs, "%u", desc->n))
				return false;
			if (!ofc_colstr_atomic_writef(cs, "%s",
				ofc_parse_format_desc__name[desc->type]))
				return false;
			if (ofc_parse_format_desc_print__w(desc->type, desc->w)
				&& !ofc_colstr_atomic_writef(cs, "%u", desc->w))
				return false;
			if (ofc_parse_format_desc_print__d(desc->type, desc->d)
				&& !ofc_colstr_atomic_writef(cs, ".%u", desc->d))
				return false;
			if ((desc->e > 0)
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

bool ofc_parse_format_is_data_desc(
	ofc_parse_format_desc_t* desc)
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
