#include "parse.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


typedef struct
{
	const char*         name;
	parse_format_desc_e type;
	bool                w, d, e;
} parse_format_desc__map_t;


static const parse_format_desc__map_t parse_format_desc__map[] =
{
	{ "I" , PARSE_FORMAT_DESC_INTEGER   , 1, 1, 0 },
	{ "F" , PARSE_FORMAT_DESC_REAL      , 1, 1, 1 },
	{ "D" , PARSE_FORMAT_DESC_E         , 1, 1, 1 },
	{ "E" , PARSE_FORMAT_DESC_D         , 1, 1, 1 },
	{ "G" , PARSE_FORMAT_DESC_G         , 1, 1, 1 },
	{ "A" , PARSE_FORMAT_DESC_CHARACTER , 1, 0, 0 },
	{ "L" , PARSE_FORMAT_DESC_LOGICAL   , 1, 0, 0 },
	{ "BZ", PARSE_FORMAT_DESC_BZ        , 0, 0, 0 },
	{ "BN", PARSE_FORMAT_DESC_BN        , 0, 0, 0 },
	{ "SP", PARSE_FORMAT_DESC_SP        , 0, 0, 0 },
	{ "SS", PARSE_FORMAT_DESC_SS        , 0, 0, 0 },
	{ "S" , PARSE_FORMAT_DESC_S         , 0, 0, 0 },
	{ "P" , PARSE_FORMAT_DESC_REAL_SCALE, 0, 0, 0 },
	{ "X" , PARSE_FORMAT_DESC_X         , 0, 0, 0 },
	{ "TL", PARSE_FORMAT_DESC_TL        , 1, 0, 0 },
	{ "TR", PARSE_FORMAT_DESC_TR        , 1, 0, 0 },
	{ "T" , PARSE_FORMAT_DESC_T         , 1, 0, 0 },
	{ "/" , PARSE_FORMAT_DESC_SLASH     , 0, 0, 0 },
	{ ":" , PARSE_FORMAT_DESC_DOLLAR    , 0, 0, 0 },
	{ "$" , PARSE_FORMAT_DESC_COLON     , 0, 0, 0 },

	/* Extended descriptors. */
	{ "B" , PARSE_FORMAT_DESC_BINARY    , 1, 1, 0 },
	{ "O" , PARSE_FORMAT_DESC_OCTAL     , 1, 1, 0 },
	{ "Z" , PARSE_FORMAT_DESC_HEX       , 1, 1, 0 },

	{ NULL, 0, 0, 0, 0 }
};


parse_format_desc_t* parse_format_desc(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_format_desc_t* desc
		= (parse_format_desc_t*)malloc(
			sizeof(parse_format_desc_t));
	if (!desc) return NULL;

	unsigned i = parse_hollerith(
		src, ptr, &desc->string);
	if (i > 0)
	{
		desc->neg  = false;
		desc->n    = 0;
		desc->type = PARSE_FORMAT_DESC_HOLLERITH;

		if (len) *len = i;
		return desc;
	}

	bool negative = (ptr[i] == '-');
	if (negative || (ptr[i] == '+'))
		i += 1;

	unsigned l, n = 0;
	l = parse_unsigned(src, &ptr[i], &n);
	i = (l > 0 ? i + l : 0);

	l = parse_character(
		src, &ptr[i], &desc->string);
	if (l > 0)
	{
		desc->neg  = negative;
		desc->n    = n;
		desc->type = PARSE_FORMAT_DESC_STRING;

		if (len) *len = (i + l);
		return desc;
	}

	if (ptr[i] == '(')
	{
		i += 1;

		unsigned l;
		desc->repeat = parse_format_desc_list(
			src, &ptr[i], &l);
		if (desc->repeat) i += l;

		desc->neg = negative;
		desc->n    = n;
		desc->type = PARSE_FORMAT_DESC_REPEAT;

		if (ptr[i++] != ')')
		{
			parse_format_desc_list_delete(
				desc->repeat);
			free(desc);
			return NULL;
		}

		if (len) *len = i;
		return desc;
	}

	unsigned m;
	for (m = 0; parse_format_desc__map[m].name; m++)
	{
		l = strlen(parse_format_desc__map[m].name);
		if (strncasecmp(&ptr[i],
			parse_format_desc__map[m].name, l) == 0)
			break;
	}
	parse_format_desc__map_t map
		= parse_format_desc__map[m];
	if (!map.name)
	{
		free(desc);
		return NULL;
	}
	i += l;

	unsigned w = 0;
	if (map.w)
	{
		i += parse_unsigned(
			src, &ptr[i], &w);
	}

	unsigned d = 0;
	if (map.d && (ptr[i] == '.'))
	{
		i += 1;
		i += parse_unsigned(
			src, &ptr[i], &d);
	}

	unsigned e = 0;
	if (map.e && (toupper(ptr[i]) == 'E'))
	{
		i += 1;
		i += parse_unsigned(
			src, &ptr[i], &e);
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

void parse_format_desc_delete(
	parse_format_desc_t* desc)
{
	if (!desc)
		return;

	switch (desc->type)
	{
		case PARSE_FORMAT_DESC_HOLLERITH:
		case PARSE_FORMAT_DESC_STRING:
			string_delete(desc->string);
			break;
		case PARSE_FORMAT_DESC_REPEAT:
			parse_format_desc_list_delete(desc->repeat);
			break;
		default:
			break;
	}
	free(desc);
}


parse_format_desc_list_t* parse_format_desc_list(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	parse_format_desc_list_t* list
		= (parse_format_desc_list_t*)malloc(
			sizeof(parse_format_desc_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->desc  = NULL;

	unsigned i = parse_list_seperator_optional(
		src, ptr, ',',
		&list->count, (void***)&list->desc,
		(void*)parse_format_desc,
		(void*)parse_format_desc_delete);
	if (i == 0)
	{
		free(list);
		return NULL;
	}

	if (len) *len = i;
	return list;
}

void parse_format_desc_list_delete(
	parse_format_desc_list_t* list)
{
	if (!list)
		return;

	parse_list_delete(
		list->count, (void**)list->desc,
		(void*)parse_format_desc_delete);
	free(list);
}
