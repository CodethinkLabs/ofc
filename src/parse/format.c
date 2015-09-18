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

	{ NULL, 0, 0, 0, 0 }
};


unsigned parse_format_desc(
	const sparse_t* src, const char* ptr,
	parse_format_desc_t* desc)
{
	unsigned len = parse_hollerith(
		src, ptr, &desc->string);
	if (len > 0)
	{
		desc->n    = 0;
		desc->type = PARSE_FORMAT_DESC_HOLLERITH;
		return len;
	}

	unsigned i, n = 0;
	i = parse_unsigned(src, ptr, &n);

	len = parse_character(
		src, &ptr[i], &desc->string);
	if (len > 0)
	{
		desc->n    = n;
		desc->type = PARSE_FORMAT_DESC_STRING;
		return (i + len);
	}

	unsigned m;
	for (m = 0; parse_format_desc__map[m].name; m++)
	{
		len = strlen(parse_format_desc__map[m].name);
		if (strncasecmp(&ptr[i],
			parse_format_desc__map[m].name, len) == 0)
			break;
	}
	parse_format_desc__map_t map
		= parse_format_desc__map[m];
	if (!map.name)
		return 0;
	i += len;

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
	desc->n = n;
	desc->w = w;
	desc->d = d;
	desc->e = e;
	return i;
}

void parse_format_desc_cleanup(
	parse_format_desc_t desc)
{
	switch (desc.type)
	{
		case PARSE_FORMAT_DESC_HOLLERITH:
		case PARSE_FORMAT_DESC_STRING:
			string_delete(desc.string);
			break;
		default:
			break;
	}
}


unsigned parse_format_desc_list(
	const sparse_t* src, const char* ptr,
	char terminator,
	parse_format_desc_t** list, unsigned* count)
{
	unsigned i = 0;

	parse_format_desc_t* dlist  = NULL;
	unsigned             dcount = 0;
	unsigned             dmax   = 0;

	bool was_slash = false;
	while (ptr[i] != terminator)
	{
		unsigned comma = 0;
		if (dcount > 0)
		{
			if (ptr[i] == ',')
				comma = 1;
			else if (!was_slash
				&& (ptr[i] != '/'))
				break;
		}

		parse_format_desc_t desc;
		unsigned len = parse_format_desc(
			src, &ptr[i + comma], &desc);
		if (len == 0) break;

		was_slash = (desc.type == PARSE_FORMAT_DESC_SLASH);

		if (dcount >= dmax)
		{
			dmax = (dmax ? (dmax << 1) : 16);
			parse_format_desc_t* ndlist
				= (parse_format_desc_t*)realloc(dlist,
					(dmax * sizeof(parse_format_desc_t)));
			if (!ndlist)
			{
				parse_format_desc_list_delete(
					dlist, dcount);
				return 0;
			}
			dlist = ndlist;
			dlist[dcount++] = desc;
		}

		i += (comma + len);
	}

	if ((terminator != '\0')
		&& (ptr[i] != terminator))
	{
		sparse_error(src, &ptr[i],
			"Expected '%c' after FORMAT descriptor list", terminator);
		parse_format_desc_list_delete(
			dlist, dcount);
		return 0;
	}

	if (dcount == 0)
		return 0;

	*list  = dlist;
	*count = dcount;
	return i;
}

void parse_format_desc_list_delete(
	parse_format_desc_t* list, unsigned count)
{
	if (!list)
		return;

	unsigned i;
	for (i = 0; i < count; i++)
		parse_format_desc_cleanup(list[i]);
	free(list);
}
