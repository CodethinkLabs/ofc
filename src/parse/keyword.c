#include "parse.h"
#include <string.h>
#include <ctype.h>



static const char* parse_keyword__name[] =
{
	"PROGRAM",
	"SUBROUTINE",
	"FUNCTION",
	"BLOCK DATA",

	"IF",
	"THEN",
	"ELSE IF",
	"ELSE",
	"GO TO",
	"DO",
	"CONTINUE",
	"STOP",
	"PAUSE",

	"LOGICAL",
	"CHARACTER",
	"INTEGER",
	"REAL",
	"COMPLEX",
	"BYTE",
	"DOUBLE PRECISION",
	"DOUBLE COMPLEX",

	"TRUE",
	"FALSE",

	"IMPLICIT",
	"IMPLICIT NONE",

	"COMMON",
	"DIMENSION",
	"EQUIVALENCE",

	"KIND",

	"ASSIGN",
	"TO",

	"CALL",
	"RETURN",
	"EXTERNAL",
	"INTRINSIC",

	"DATA",
	"PARAMETER",
	"SAVE",

	"FORMAT",

	"OPEN",
	"REWIND",
	"BACKSPACE",
	"READ",
	"WRITE",
	"END FILE",

	"UNIT",
	"FILE",
	"NAME",
	"ACCESS",
	"BLANK",
	"ERR",
	"FORM",
	"IOSTAT",
	"RECL",
	"RECORDSIZE",
	"STATUS",
	"READONLY",
	"ACTION",
	"FMT",
	"REC",
	"END",

	NULL
};


unsigned parse_name(
	const sparse_t* src, const char* ptr,
	str_ref_t* name)
{
	if (!isalpha(ptr[0]))
		return 0;

	unsigned i;
	for (i = 1; isalnum(ptr[i]) || (ptr[i] == '_'); i++);

	if (!sparse_sequential(src, ptr, i))
	{
		sparse_warning(src, ptr,
			"Unexpected whitespace in name");
	}

	/* END is reserved, an identifier may never begin with these letters. */
	if ((i >= 3) && (strncasecmp(ptr, "END", 3) == 0))
		return 0;

	if (name) *name = str_ref(ptr, i);
	return i;
}

str_ref_t* parse_name_alloc(
	const sparse_t* src, const char* ptr,
	unsigned* len)
{
	str_ref_t name;
	unsigned i = parse_name(
		src, ptr, &name);
	if (i == 0) return NULL;

	str_ref_t* aname
		= (str_ref_t*)malloc(
			sizeof(str_ref_t));
	if (!aname) return NULL;
	*aname = name;

	if (len) *len = i;
	return aname;
}


const char* parse_keyword_name(
	parse_keyword_e keyword)
{
	if (keyword >= PARSE_KEYWORD_COUNT)
		return NULL;
	return parse_keyword__name[keyword];
}

unsigned parse_keyword_named(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword,
	str_ref_t* name)
{
	if (keyword >= PARSE_KEYWORD_COUNT)
		return 0;

	const char* kwstr = parse_keyword__name[keyword];

	/* TODO - Make handling spaced keywords less manual. */
	/* TODO - Differentiate between expected and optional spaces. */
	unsigned expect_space = 0;
	switch (keyword)
	{
		case PARSE_KEYWORD_BLOCK_DATA:
			kwstr = "BLOCKDATA";
			expect_space = 5;
			break;
		case PARSE_KEYWORD_ELSE_IF:
			kwstr = "ELSEIF";
			expect_space = 4;
			break;
		case PARSE_KEYWORD_GO_TO:
			kwstr = "GOTO";
			expect_space = 2;
			break;
		case PARSE_KEYWORD_DOUBLE_PRECISION:
			kwstr = "DOUBLEPRECISION";
			expect_space = 6;
			break;
		case PARSE_KEYWORD_DOUBLE_COMPLEX:
			kwstr = "DOUBLECOMPLEX";
			expect_space = 6;
			break;
		case PARSE_KEYWORD_IMPLICIT_NONE:
			kwstr = "IMPLICITNONE";
			expect_space = 8;
			break;
		case PARSE_KEYWORD_END_FILE:
			kwstr = "ENDFILE";
			expect_space = 3;
			break;
		default:
			break;
	}

	unsigned len = strlen(kwstr);
	if (strncasecmp(ptr, kwstr, len) != 0)
		return 0;

	bool entirely_sequential
		= sparse_sequential(src, ptr, len);

	bool unexpected_space = !entirely_sequential;
	if (expect_space > 0)
	{
		unsigned remain = (len - expect_space);
		unexpected_space = (!sparse_sequential(src, ptr, expect_space)
			|| !sparse_sequential(src, &ptr[expect_space], remain));

		if (entirely_sequential)
		{
			sparse_warning(src, ptr,
				"Expected a space between keywords '%.*s' and '%.*s'",
				expect_space, ptr, remain, &ptr[expect_space]);
		}
	}

	if (unexpected_space)
	{
		sparse_warning(src, ptr,
			"Unexpected a space in %s", kwstr);
	}

	if (name != NULL)
	{
		unsigned nlen = parse_name(
			src, &ptr[len], name);

		if ((nlen > 0) && sparse_sequential(
			src, &ptr[len - 1], 2))
		{
			sparse_warning(src, &ptr[len],
				"Expected whitespace between %s and name", kwstr);
		}

		len += nlen;
	}

	return len;
}

unsigned parse_keyword(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword)
{
	return parse_keyword_named(
		src, ptr, keyword, NULL);
}


unsigned parse_keyword_end_named(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword,
	str_ref_t* name)
{
	if (keyword >= PARSE_KEYWORD_COUNT)
		return 0;

	unsigned i = 0;
	if (strncasecmp(&ptr[i], "END", 3) != 0)
		return 0;
	i += 3;

	unsigned warn_end_kw_space = 0;

	str_ref_t kname = STR_REF_EMPTY;
	unsigned len = parse_keyword_named(
		src, &ptr[i], keyword,
		(name ? &kname : NULL));
	if (len > 0)
	{
		if (sparse_sequential(src, &ptr[i - 1], 2))
			warn_end_kw_space = i;
	}
	else if (name)
	{
		len = parse_name(src, &ptr[i], &kname);
	}
	i += len;

	/* Expect but don't consume statement end. */
	if (!is_end_statement(&ptr[i], &len))
		return 0;

	if (name && !str_ref_empty(kname)
		&& !str_ref_equal(*name, kname))
	{
		sparse_warning(src, &ptr[i],
			"END %s name '%.*s' doesn't match %s name '%.*s'",
			parse_keyword__name[keyword],
			kname.size, kname.base,
			parse_keyword__name[keyword],
			name->size, name->base);
	}

	if (!sparse_sequential(src, ptr, 3))
	{
		sparse_warning(src, ptr,
			"Unexpected a space in END keyword");
	}

	if (warn_end_kw_space > 0)
	{
		sparse_warning(src, &ptr[warn_end_kw_space],
			"Expected space between END and %s",
			parse_keyword__name[keyword]);
	}

	return i;
}

unsigned parse_keyword_end(
	const sparse_t* src, const char* ptr,
	parse_keyword_e keyword)
{
	return parse_keyword_end_named(
		src, ptr, keyword, NULL);
}
