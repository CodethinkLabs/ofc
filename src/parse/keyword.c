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
	"ENTRY",
	"RETURN",
	"EXTERNAL",
	"INTRINSIC",

	"DATA",
	"PARAMETER",
	"SAVE",

	"FORMAT",

	"OPEN",
	"INQUIRE",
	"REWIND",
	"BACKSPACE",
	"READ",
	"WRITE",
	"END FILE",
	"CLOSE",
	"PRINT",
	"TYPE",

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
	"FILEOPT",
	"READONLY",
	"ACTION",
	"FMT",
	"REC",
	"END",

	"EXIST",
	"OPENED",
	"NAMED",
	"SEQUENTIAL",
	"DIRECT",
	"FORMATTED",
	"UNFORMATTED",
	"NUMBER",
	"NEXTREC",

	NULL
};



unsigned parse_ident(
	const sparse_t* src, const char* ptr,
	str_ref_t* ident)
{
	if (!isalpha(ptr[0]))
		return 0;

	unsigned i;
	for (i = 1; isalnum(ptr[i]) || (ptr[i] == '_'); i++);

	if (!sparse_sequential(src, ptr, i))
	{
		sparse_warning(src, ptr,
			"Unexpected whitespace in ident");
	}

	if (ident) *ident = str_ref(ptr, i);
	return i;
}

unsigned parse_name(
	const sparse_t* src, const char* ptr,
	str_ref_t* name)
{
	/* END is reserved, an identifier may never begin with these letters. */
	if (strncasecmp(ptr, "END", 3) == 0)
		return 0;

	return parse_ident(src, ptr, name);
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
	unsigned space = 0;
	bool space_optional = false;
	switch (keyword)
	{
		case PARSE_KEYWORD_BLOCK_DATA:
			kwstr = "BLOCKDATA";
			space = 5;
			break;
		case PARSE_KEYWORD_ELSE_IF:
			kwstr = "ELSEIF";
			space = 4;
			break;
		case PARSE_KEYWORD_GO_TO:
			kwstr = "GOTO";
			space = 2;
			space_optional = true;
			break;
		case PARSE_KEYWORD_DOUBLE_PRECISION:
			kwstr = "DOUBLEPRECISION";
			space = 6;
			break;
		case PARSE_KEYWORD_DOUBLE_COMPLEX:
			kwstr = "DOUBLECOMPLEX";
			space = 6;
			break;
		case PARSE_KEYWORD_IMPLICIT_NONE:
			kwstr = "IMPLICITNONE";
			space = 8;
			break;
		case PARSE_KEYWORD_END_FILE:
			kwstr = "ENDFILE";
			space = 3;
			space_optional = true;
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
	if (space > 0)
	{
		unsigned remain = (len - space);
		unexpected_space = (!sparse_sequential(src, ptr, space)
			|| !sparse_sequential(src, &ptr[space], remain));

		if (entirely_sequential && !space_optional)
		{
			sparse_warning(src, ptr,
				"Expected a space between keywords '%.*s' and '%.*s'",
				space, ptr, remain, &ptr[space]);
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
