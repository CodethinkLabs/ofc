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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "ofc/colstr.h"
#include "ofc/util/dprintf.h"


struct ofc_colstr_s
{
	ofc_print_opts_t print_opts;

	unsigned size, max;
	char* base;
	unsigned col, col_max, col_ext;
	bool oversize;
	unsigned oversize_off;
};


ofc_colstr_t* ofc_colstr_create(
	const ofc_print_opts_t print_opts,
	unsigned cols, unsigned ext)
{
	if (cols == 0)
		cols = 72;
	if (ext == 0)
		ext = 132;
	if ((cols <= 6) || (ext < cols))
		return NULL;

	ofc_colstr_t* cstr
		= (ofc_colstr_t*)malloc(
			sizeof(ofc_colstr_t));
	if (!cstr) return NULL;

	cstr->print_opts = print_opts;
	cstr->size       = 0;
	cstr->max        = 0;
	cstr->base       = NULL;
	cstr->col        = 0;
	cstr->col_max    = cols;
	cstr->col_ext    = ext;
	cstr->oversize   = false;

	return cstr;
}

void ofc_colstr_delete(ofc_colstr_t* cstr)
{
	if (!cstr)
		return;

	free(cstr->base);
	free(cstr);
}


static bool ofc_colstr__enlarge(
	ofc_colstr_t* cstr, unsigned size)
{
	if (!cstr)
		return false;

	unsigned nsize = cstr->size + size;
	if (nsize > cstr->max)
	{
		unsigned nmax = (cstr->max << 1);
		if (nmax == 0)
			nmax = 16;
		if (nmax < nsize)
			nmax = nsize;

		char* nbase
			= (char*)realloc(
				cstr->base, nmax);
		if (!nbase) return false;

		cstr->base = nbase;
		cstr->max  = nmax;
	}

	return true;
}


bool ofc_colstr_newline(
	ofc_colstr_t* cstr, unsigned indent,
	const unsigned* label)
{
	bool first = (cstr->size == 0);

	if (!ofc_colstr__enlarge(cstr, (first ? 6 : 7)))
		return false;

	if (!first)
		cstr->base[cstr->size++] = '\n';

	unsigned i;
	if (label)
	{
		unsigned d = 10000;
		for (i = 0; (i < 4) && ((*label / d) == 0); i++, d /= 10)
			cstr->base[cstr->size + i] = ' ';
		for (; i < 5; i++, d /= 10)
			cstr->base[cstr->size + i] = '0' + ((*label / d) % 10);
		cstr->base[cstr->size + i] = ' ';
	}
	else
	{
		for (i = 0; i < 6; i++)
			cstr->base[cstr->size + i] = ' ';
	}
	cstr->size += 6;
	cstr->col = 6;

	if (indent > cstr->print_opts.indent_max_level)
		indent = cstr->print_opts.indent_max_level;

	for (i = 0; i < indent; i++)
	{
		unsigned j;
		for (j = 0; j < cstr->print_opts.indent_width; j++)
		{
			if (!ofc_colstr_atomic_writef(cstr, " "))
				return false;
		}
	}

	cstr->oversize = false;

	return true;
}


static const char* is_escape(char c)
{
	switch (c)
	{
		case '\n':
			return "n";
		case '\r':
			return "r";
		case '\v':
			return "v";
		case '\t':
			return "t";
		case '\0':
			return "0";
		case '\"':
			return "\"";
		case '\\':
			return "\\";
		default:
			break;
	}

	return NULL;
}

bool ofc_colstr_write_quoted(
	ofc_colstr_t* cstr,
	const char* prefix, char quote,
	const char* base, unsigned size)
{
	if (size < 2)
	{
		return ofc_colstr_atomic_writef(
			cstr, "%s%c%.*s%c", (prefix ? prefix : ""),
			quote, size, base, quote);
	}

	if (!ofc_colstr_atomic_writef(
		cstr, "%s%c%c", (prefix ? prefix : ""), quote, base[0]))
		return false;

	return ofc_colstr_writef(
		cstr, "%.*s%c", (size - 1), &base[1], quote);
}

bool ofc_colstr_write_escaped(
	ofc_colstr_t* cstr, char quote,
	const char* base, unsigned size)
{
	if (size == 0)
		return ofc_colstr_write_quoted(
			cstr, NULL, quote, NULL, 0);

	if (!base)
		return false;

	bool quote_normal = ((quote == '\'') || (quote == '\"'));

	unsigned esize = size;
	unsigned i;
	for (i = 0; i < size; i++)
	{
		if ((base[i] == quote)
			&& quote_normal)
		{
			esize++;
		}
		else
		{
			const char* e = is_escape(base[i]);
			if (e) esize += strlen(e);
		}
	}

	char estr[esize];

	unsigned j;
	for (i = 0, j = 0; i < size; i++)
	{
		if ((base[i] == quote)
			&& quote_normal)
		{
			/* Use fortran style escaping for quotes where possible. */
			estr[j++] = base[i];
			estr[j++] = base[i];
		}
		else if (quote_normal
			&& ((base[i] == '\'')
				|| (base[i] == '\"')))
		{
			/* Don't escape quotes if we don't need to. */
			estr[j++] = base[i];
		}
		else
		{
			const char* e = is_escape(base[i]);
			if (e)
			{
				estr[j++] = '\\';
				unsigned elen = strlen(e);
				memcpy(&estr[j], e, elen);
				j += elen;
			}
			else
			{
				estr[j++] = base[i];
			}
		}
	}

	return ofc_colstr_write_quoted(
		cstr, NULL, quote, estr, esize);
}


bool ofc_colstr_write(
	ofc_colstr_t* cstr, const char* base, unsigned size)
{
	if (!cstr || !base || (size == 0))
		return false;

	unsigned orig_size     = cstr->size;
	unsigned orig_col      = cstr->col;
	bool     orig_oversize = cstr->oversize;

	if (cstr->oversize)
	{
		if (!ofc_colstr__enlarge(cstr, 7))
			return false;

		cstr->base[cstr->oversize_off] = '&';
		cstr->oversize = false;

		cstr->base[cstr->size++] = '\n';
		unsigned i;
		for (i = 0; i < 5; i++)
			cstr->base[cstr->size++] = ' ';
		cstr->base[cstr->size++] = '&';
	}

	unsigned remain
		= (cstr->col_max - cstr->col);

	if (remain > 0)
	{
		if (size <= remain)
		{
			if (ofc_colstr_atomic_write(
				cstr, base, size))
				return true;

			cstr->size     = orig_size;
			cstr->col      = orig_col;
			cstr->oversize = orig_oversize;
			return false;
		}

		if (!ofc_colstr__enlarge(cstr, remain))
		{
			cstr->size     = orig_size;
			cstr->col      = orig_col;
			cstr->oversize = orig_oversize;
			return false;
		}

		memcpy(&cstr->base[cstr->size], base, remain);
		cstr->size += remain;
		base += remain;
		size -= remain;
	}

	unsigned code_len = (cstr->col_max - 6);

	unsigned continuations
		= ((size + (code_len - 1)) / code_len);

	if (!ofc_colstr__enlarge(cstr,
		((continuations * 8) + size)))
	{
		cstr->size     = orig_size;
		cstr->col      = orig_col;
		cstr->oversize = orig_oversize;
		return false;
	}

	while (size > 0)
	{
		cstr->base[cstr->size++] = '&';
		cstr->base[cstr->size++] = '\n';

		unsigned i;
		for (i = 0; i < 5; i++)
			cstr->base[cstr->size++] = ' ';
		cstr->base[cstr->size++] = '&';

		unsigned lsize = (size < code_len ? size : code_len);
		memcpy(&cstr->base[cstr->size], base, lsize);
		cstr->size += lsize;
		base += lsize;
		size -= lsize;

		cstr->col = 6 + lsize;
	}

	return true;
}

bool ofc_colstr_writef(
	ofc_colstr_t* cstr,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);

	va_list largs;
	va_copy(largs, args);
	int len = vsnprintf(NULL, 0, format, largs);
	va_end(largs);

	if (len <= 0)
	{
		va_end(args);
		return false;
	}

	char buff[len + 1];
	int plen = vsnprintf(buff, (len + 1), format, args);
	va_end(args);

	if (len != plen)
		return false;

	return ofc_colstr_write(
		cstr, buff, len);
}

static bool ofc_colstr_atomic_write__oversized(
	ofc_colstr_t* cstr, const char* base, unsigned size)
{
	if (size > (cstr->col_ext - 7))
		return false;

	unsigned orig_size     = cstr->size;
	unsigned orig_col      = cstr->col;
	bool     orig_oversize = cstr->oversize;

	if (cstr->oversize)
	{
		if (!ofc_colstr__enlarge(cstr, 7))
			return false;

		cstr->base[cstr->oversize_off] = '&';
		cstr->oversize = false;

		cstr->base[cstr->size++] = '\n';
		unsigned i;
		for (i = 0; i < 5; i++)
			cstr->base[cstr->size++] = ' ';
		cstr->base[cstr->size++] = '&';
	}

	unsigned remain
		= (cstr->col_max - cstr->col);

	if (!ofc_colstr__enlarge(
		cstr, (remain + 8)))
	{
		cstr->size     = orig_size;
		cstr->col      = orig_col;
		cstr->oversize = orig_oversize;
		return false;
	}

	for (; cstr->col < cstr->col_max; cstr->col++)
		cstr->base[cstr->size++] = ' ';
	cstr->base[cstr->size++] = '&';
	cstr->base[cstr->size++] = '\n';

	unsigned i;
	for (i = 0; i < 5; i++)
		cstr->base[cstr->size++] = ' ';
	cstr->base[cstr->size++] = '&';

	cstr->col = 6;

	unsigned fixed_len = (cstr->col_max - 6);
	unsigned bangs = ((size - 1) / fixed_len);

	unsigned nsize = (size + 1) + (bangs * 7) + ((bangs - 1) * fixed_len) + (size % fixed_len);

	if (!ofc_colstr__enlarge(cstr, nsize))
	{
		cstr->size     = orig_size;
		cstr->col      = orig_col;
		cstr->oversize = orig_oversize;
		return false;
	}

	memcpy(&cstr->base[cstr->size], base, size);
	cstr->size += size;
	cstr->oversize = true;
	cstr->oversize_off = cstr->size;

	size -= fixed_len;
	base += fixed_len;

	/* Reserved for potential ampersand. */
	cstr->base[cstr->size++] = ' ';


	while (size > 0)
	{
		cstr->base[cstr->size++] = '\n';
		for (i = 0; i < 5; i++)
			cstr->base[cstr->size++] = ' ';

		/* Continuation for fixed form
		   Comment for free form */
		cstr->base[cstr->size++] = '!';

		unsigned lsize = (size > fixed_len ? fixed_len : size);
		memcpy(&cstr->base[cstr->size], base, lsize);
		cstr->size += lsize;

		size -= lsize;
		base += lsize;

		cstr->col = 6 + lsize;
	}

	return true;
}

bool ofc_colstr_atomic_write(
	ofc_colstr_t* cstr, const char* base, unsigned size)
{
	if (!cstr || !base || (size == 0))
		return false;

	unsigned orig_size     = cstr->size;
	unsigned orig_col      = cstr->col;
	bool     orig_oversize = cstr->oversize;

	if (cstr->oversize)
	{
		if (!ofc_colstr__enlarge(cstr, 7))
			return false;

		cstr->base[cstr->oversize_off] = '&';
		cstr->oversize = false;

		cstr->base[cstr->size++] = '\n';
		unsigned i;
		for (i = 0; i < 5; i++)
			cstr->base[cstr->size++] = ' ';
		cstr->base[cstr->size++] = '&';
	}

	/* We can't atomically print wider than a line. */
	if (size > (cstr->col_max - 6))
	{
		if (ofc_colstr_atomic_write__oversized(
			cstr, base, size))
			return true;

		cstr->size     = orig_size;
		cstr->col      = orig_col;
		cstr->oversize = orig_oversize;
		return false;
	}

	unsigned remain
		= (cstr->col_max - cstr->col);

	if (remain < size)
	{
		if (!ofc_colstr__enlarge(
			cstr, (remain + 8)))
		{
			cstr->size     = orig_size;
			cstr->col      = orig_col;
			cstr->oversize = orig_oversize;
			return false;
		}

		for (; cstr->col < cstr->col_max; cstr->col++)
			cstr->base[cstr->size++] = ' ';
		cstr->base[cstr->size++] = '&';
		cstr->base[cstr->size++] = '\n';

		unsigned i;
		for (i = 0; i < 5; i++)
			cstr->base[cstr->size++] = ' ';
		cstr->base[cstr->size++] = '&';

		cstr->col = 6;
	}

	if (!ofc_colstr__enlarge(cstr, size))
	{
		cstr->size     = orig_size;
		cstr->col      = orig_col;
		cstr->oversize = orig_oversize;
		return false;
	}

	memcpy(&cstr->base[cstr->size], base, size);
	cstr->size += size;
	cstr->col  += size;
	return true;
}

bool ofc_colstr_atomic_writef(
	ofc_colstr_t* cstr,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);

	va_list largs;
	va_copy(largs, args);
	int len = vsnprintf(NULL, 0, format, largs);
	va_end(largs);

	if (len <= 0)
	{
		va_end(args);
		return false;
	}

	char buff[len + 1];
	int plen = vsnprintf(buff, (len + 1), format, args);
	va_end(args);

	if (len != plen)
		return false;

	return ofc_colstr_atomic_write(
		cstr, buff, len);
}

bool ofc_colstr_keyword_atomic_writez(
	ofc_colstr_t* cstr, const char* keyword)
{
	if (!keyword)
		return false;

	size_t len = strlen(keyword);
	if (len <= 0) return false;

	if (cstr->print_opts.lowercase_keyword)
	{
		char buff[len + 1];
		unsigned i;
		for(i = 0; keyword[i]; i++)
			buff[i] = tolower(keyword[i]);
		buff[len] = '\0';

		return ofc_colstr_atomic_write(
			cstr, buff, len);
	}

	return ofc_colstr_atomic_write(
		cstr, keyword, len);
}

bool ofc_colstr_keyword_atomic_writef(
	ofc_colstr_t* cstr,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);

	va_list largs;
	va_copy(largs, args);
	int len = vsnprintf(NULL, 0, format, largs);
	va_end(largs);

	if (len <= 0)
	{
		va_end(args);
		return false;
	}

	char buff[len + 1];
	int plen = vsnprintf(buff, (len + 1), format, args);
	va_end(args);

	if (len != plen)
		return false;

	return ofc_colstr_keyword_atomic_writez(cstr, buff);
}


bool ofc_colstr_fdprint(ofc_colstr_t* cstr, int fd)
{
	if (!cstr || !cstr->base)
		return false;

	return (dprintf(fd, "%.*s\n",
		cstr->size, cstr->base) > 0);
}



const ofc_print_opts_t* ofc_colstr_print_opts_get(const ofc_colstr_t* cstr)
{
	if (!cstr)
		return NULL;
	return &cstr->print_opts;
}
