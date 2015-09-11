#include "parse.h"

typedef struct
{
	parse_type_e    type;
	bool            dbl;
	parse_keyword_e keyword;
} parse_type__keyword_t;

static const parse_type__keyword_t parse_type__keyword_map[] =
{
	{ PARSE_TYPE_LOGICAL  , 0, PARSE_KEYWORD_LOGICAL          },
	{ PARSE_TYPE_CHARACTER, 0, PARSE_KEYWORD_CHARACTER        },
	{ PARSE_TYPE_INTEGER  , 0, PARSE_KEYWORD_INTEGER          },
	{ PARSE_TYPE_REAL     , 0, PARSE_KEYWORD_REAL             },
	{ PARSE_TYPE_COMPLEX  , 0, PARSE_KEYWORD_COMPLEX          },
	{ PARSE_TYPE_BYTE     , 0, PARSE_KEYWORD_BYTE             },
	{ PARSE_TYPE_REAL     , 1, PARSE_KEYWORD_DOUBLE_PRECISION },
	{ PARSE_TYPE_COMPLEX  , 1, PARSE_KEYWORD_DOUBLE_COMPLEX   },
	{ PARSE_TYPE_NONE     , 0, 0 },
};


unsigned parse_type(
	const sparse_t* src, const char* ptr,
	parse_type_t* type)
{
	unsigned i = 0;

	parse_type_e t   = PARSE_TYPE_NONE;
	bool         dbl = false;

	unsigned j;
	for (j = 0; parse_type__keyword_map[j].type != PARSE_TYPE_NONE; j++)
	{
		i = parse_keyword(src, &ptr[i],
			parse_type__keyword_map[j].keyword);

		t   = parse_type__keyword_map[j].type;
		dbl = parse_type__keyword_map[j].dbl;

		if (i > 0) break;
	}

	if (i == 0)
		return 0;

	unsigned k = 4;
	if (dbl) k *= 2;

	type->type  = t;
	type->kind  = k;
	type->count = 0;
	return i;
}
