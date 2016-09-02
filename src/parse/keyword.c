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
#include <string.h>

#include "ofc/parse.h"



static const char* ofc_parse_keyword__name[] =
{
	"INCLUDE",
	"USE",
	"ONLY",

	"PROGRAM",
	"SUBROUTINE",
	"FUNCTION",
	"MODULE",
	"BLOCK DATA",

	"CONTAINS",

	"STRUCTURE",
	"UNION",
	"MAP",
	"RECORD",
	"SEQUENCE",

	"IF",
	"THEN",
	"ELSE",
	"GO TO",
	"DO",
	"WHILE",
	"SELECT",
	"CASE",
	"DEFAULT",
	"CONTINUE",
	"STOP",
	"PAUSE",
	"CYCLE",
	"EXIT",

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
	"UNDEFINED",

	"COMMON",
	"NAMELIST",
	"DIMENSION",
	"VIRTUAL",
	"EQUIVALENCE",

	"KIND",

	"ASSIGN",
	"TO",

	"CALL",
	"ENTRY",
	"RETURN",

	"EXTERNAL",
	"INTRINSIC",
	"AUTOMATIC",
	"STATIC",
	"VOLATILE",
	"POINTER",

	"DATA",
	"PARAMETER",
	"SAVE",

	"PUBLIC",
	"PRIVATE",

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
	"ENCODE",
	"DECODE",
	"ACCEPT",
	"DEFINE FILE",

	"RESHAPE",

	NULL
};



bool ofc_sparse_ref_begins_with_keyword(
	ofc_sparse_ref_t ref, bool* space, bool* is)
{
	unsigned i;
	for (i = 0; ofc_parse_keyword__name[i]; i++)
	{
		unsigned len = strlen(ofc_parse_keyword__name[i]);
		if (len > ref.string.size) continue;

		if (strncasecmp(ref.string.base,
			ofc_parse_keyword__name[i], len) == 0)
		{
			bool has_space = false;
			if (ref.string.size > len)
			{
				has_space = !ofc_sparse_sequential(
					ref.sparse, ref.string.base, (len + 1));
			}

			if (space) *space = has_space;
			if (is) *is = (ref.string.size == len);
			return true;
		}
	}

	return false;
}

unsigned ofc_parse_ident(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_sparse_ref_t* ident)
{
	if (!isalpha(ptr[0])
		&& (ptr[0] != '_'))
		return 0;

	unsigned i;
	for (i = 1; ofc_is_ident(ptr[i]); i++);

	if (!ofc_sparse_sequential(src, ptr, i))
	{
		ofc_parse_debug_warning(debug,
			ofc_sparse_ref(src, ptr, i),
			"Unexpected whitespace within identifier '%.*s'",
			i, ptr);
	}

	if (ident) *ident = ofc_sparse_ref(src, ptr, i);
	return i;
}

unsigned ofc_parse_name(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_sparse_ref_t* name)
{
	if (strncasecmp(ptr, "END", 3) == 0)
	{
		ofc_parse_debug_warning(debug,
			ofc_sparse_ref(src, ptr, 3),
			"Using END at the beginning of an identifier is incompatible with Fortran 90");
	}

	return ofc_parse_ident(src, ptr, debug, name);
}

ofc_sparse_ref_t* ofc_parse_name_alloc(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len)
{
	ofc_sparse_ref_t name;
	unsigned i = ofc_parse_name(
		src, ptr, debug, &name);
	if (i == 0) return NULL;

	ofc_sparse_ref_t* aname
		= (ofc_sparse_ref_t*)malloc(
			sizeof(ofc_sparse_ref_t));
	if (!aname) return NULL;
	*aname = name;

	if (len) *len = i;
	return aname;
}


const char* ofc_parse_keyword_name(
	ofc_parse_keyword_e keyword)
{
	if (keyword >= OFC_PARSE_KEYWORD_COUNT)
		return NULL;
	return ofc_parse_keyword__name[keyword];
}

unsigned ofc_parse_keyword_named(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword,
	ofc_sparse_ref_t* name)
{
	if (keyword >= OFC_PARSE_KEYWORD_COUNT)
		return 0;

	const char* kwstr = ofc_parse_keyword__name[keyword];

	/* TODO - Make handling spaced keywords less manual. */
	unsigned space = 0;
	/* Use this to make spaces in F90 keywords non-optional. */
	bool space_optional = true;
	switch (keyword)
	{
		case OFC_PARSE_KEYWORD_BLOCK_DATA:
			kwstr = "BLOCKDATA";
			space = 5;
			break;
		case OFC_PARSE_KEYWORD_GO_TO:
			kwstr = "GOTO";
			space = 2;
			break;
		case OFC_PARSE_KEYWORD_DOUBLE_PRECISION:
			kwstr = "DOUBLEPRECISION";
			space = 6;
			break;
		case OFC_PARSE_KEYWORD_DOUBLE_COMPLEX:
			kwstr = "DOUBLECOMPLEX";
			space = 6;
			break;
		case OFC_PARSE_KEYWORD_IMPLICIT_NONE:
			kwstr = "IMPLICITNONE";
			space = 8;
			break;
		case OFC_PARSE_KEYWORD_END_FILE:
			kwstr = "ENDFILE";
			space = 3;
			break;
		case OFC_PARSE_KEYWORD_DEFINE_FILE:
			kwstr = "DEFINEFILE";
			space = 6;
			break;

		default:
			break;
	}

	unsigned len = strlen(kwstr);
	if (strncasecmp(ptr, kwstr, len) != 0)
		return 0;

	bool entirely_sequential
		= ofc_sparse_sequential(src, ptr, len);

	bool unexpected_space = !entirely_sequential;
	if (space > 0)
	{
		unsigned remain = (len - space);
		unexpected_space = (!ofc_sparse_sequential(src, ptr, space)
			|| !ofc_sparse_sequential(src, &ptr[space], remain));

		if (entirely_sequential && !space_optional)
		{
			ofc_parse_debug_warning(debug,
				ofc_sparse_ref(src, &ptr[space], 0),
				"Expected a space between keywords '%.*s' and '%.*s'",
				space, ptr, remain, &ptr[space]);
		}
	}

	if (unexpected_space)
	{
		ofc_parse_debug_warning(debug,
			ofc_sparse_ref(src, ptr, len),
			"Unexpected space in %s keyword", kwstr);
	}

	if (name != NULL)
	{
		unsigned nlen = ofc_parse_name(
			src, &ptr[len], debug, name);

		if ((nlen > 0) && ofc_sparse_sequential(
			src, &ptr[len - 1], 2))
		{
			ofc_parse_debug_warning(debug,
				ofc_sparse_ref(src, &ptr[len], 0),
				"Expected whitespace between %s and name", kwstr);
		}

		len += nlen;
	}

	bool is_number = isdigit(ptr[len]);
	bool ofc_is_ident  = (isalpha(ptr[len]) || (ptr[len] == '_'));

	if ((is_number || ofc_is_ident)
		&& ofc_sparse_sequential(src, &ptr[len - 1], 2))
	{
		if (!name && (keyword == OFC_PARSE_KEYWORD_ELSE)
			&& (strncasecmp(&ptr[len], "IF", 2) == 0))
		{
			/* Treat this as a special case because of how we handle
			   else if statements. */
		}
		else
		{
			ofc_parse_debug_warning(debug,
				ofc_sparse_ref(src, &ptr[len], 0),
				"Expected whitespace between %s and %s", kwstr,
				(is_number ? "number" : "identifier"));
		}
	}

	return len;
}

unsigned ofc_parse_keyword(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword)
{
	return ofc_parse_keyword_named(
		src, ptr, debug, keyword, NULL);
}


unsigned ofc_parse_keyword_end_named(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword, bool force,
	ofc_sparse_ref_t* name)
{
	if (keyword >= OFC_PARSE_KEYWORD_COUNT)
		return 0;

	unsigned i = 0;
	if (strncasecmp(&ptr[i], "END", 3) != 0)
		return 0;
	i += 3;

	unsigned warn_end_kw_space = 0;

	ofc_sparse_ref_t kname = OFC_SPARSE_REF_EMPTY;
	unsigned len = ofc_parse_keyword_named(
		src, &ptr[i], debug, keyword,
		(name ? &kname : NULL));
	if (len > 0)
	{
		if (ofc_sparse_sequential(src, &ptr[i - 1], 2))
			warn_end_kw_space = i;
	}
	else if (force)
	{
		return 0;
	}
	else if (name)
	{
		len = ofc_parse_name(
			src, &ptr[i], debug, &kname);
	}
	i += len;

	/* Expect but don't consume statement end. */
	if (!ofc_is_end_statement(&ptr[i], &len))
		return 0;

	if (name && !ofc_sparse_ref_empty(kname)
		&& !ofc_str_ref_equal(name->string, kname.string))
	{
		ofc_parse_debug_warning(debug, kname,
			"END %s name '%.*s' doesn't match %s name '%.*s'",
			ofc_parse_keyword__name[keyword],
			kname.string.size, kname.string.base,
			ofc_parse_keyword__name[keyword],
			name->string.size, name->string.base);
	}

	if (!ofc_sparse_sequential(src, ptr, 3))
	{
		ofc_parse_debug_warning(debug,
			ofc_sparse_ref(src, ptr, 3),
			"Unexpected space in END keyword");
	}

	if (warn_end_kw_space > 0)
	{
		/* Spaces are optional between END and a keyword. */
	}

	return i;
}

unsigned ofc_parse_keyword_end(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword, bool force)
{
	return ofc_parse_keyword_end_named(
		src, ptr, debug, keyword, force, NULL);
}
