#include "colstr.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


struct colstr_s
{
	unsigned size, max;
	char* base;
	unsigned col, col_max, col_ext;
	bool oversize;
	unsigned oversize_off;
};


colstr_t* colstr_create(
	unsigned cols, unsigned ext)
{
	if (cols == 0)
		cols = 72;
	if (ext == 0)
		ext = 132;
	if ((cols <= 6) || (ext < cols))
		return NULL;

	colstr_t* cstr
		= (colstr_t*)malloc(
			sizeof(colstr_t));
	if (!cstr) return NULL;

	cstr->size     = 0;
	cstr->max      = 0;
	cstr->base     = NULL;
	cstr->col      = 0;
	cstr->col_max  = cols;
	cstr->col_ext  = ext;
	cstr->oversize = false;

	return cstr;
}

void colstr_delete(colstr_t* cstr)
{
	if (!cstr)
		return;

	free(cstr->base);
	free(cstr);
}


static bool colstr__enlarge(
	colstr_t* cstr, unsigned size)
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


bool colstr_newline(
	colstr_t* cstr, unsigned* label)
{
	bool first = (cstr->size == 0);

	if (!colstr__enlarge(cstr, (first ? 6 : 7)))
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
		case '\'':
			return "\'";
		case '\"':
			return "\"";
		case '\\':
			return "\\";
		default:
			break;
	}

	return NULL;
}

bool colstr_write_escaped(
	colstr_t* cstr, const char* base, unsigned size)
{
	if (!base || (size == 0))
		return false;

	unsigned esize = size;
	unsigned i;
	for (i = 0; i < (size - 1); i++)
	{
		const char* e = is_escape(base[i]);
		if (e) esize += strlen(e);
	}

	char estr[esize];

	unsigned j;
	for (i = 0, j = 0; i < size; i++)
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

	return colstr_write(
		cstr, estr, esize);
}


bool colstr_write(
	colstr_t* cstr, const char* base, unsigned size)
{
	if (!cstr || !base || (size == 0))
		return false;

	unsigned orig_size     = cstr->size;
	unsigned orig_col      = cstr->col;
	bool     orig_oversize = cstr->oversize;

	if (cstr->oversize)
	{
		if (!colstr__enlarge(cstr, 7))
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
			if (colstr_atomic_write(
				cstr, base, size))
				return true;

			cstr->size     = orig_size;
			cstr->col      = orig_col;
			cstr->oversize = orig_oversize;
			return false;
		}

		if (!colstr__enlarge(cstr, remain))
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

	if (!colstr__enlarge(cstr,
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

bool colstr_writef(
	colstr_t* cstr,
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

	return colstr_write(
		cstr, buff, len);
}

static bool colstr_atomic_write__oversized(
	colstr_t* cstr, const char* base, unsigned size)
{
	if (size > (cstr->col_ext - 7))
		return false;

	unsigned orig_size     = cstr->size;
	unsigned orig_col      = cstr->col;
	bool     orig_oversize = cstr->oversize;

	if (cstr->oversize)
	{
		if (!colstr__enlarge(cstr, 7))
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

	if (!colstr__enlarge(
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

	if (!colstr__enlarge(cstr, nsize))
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
		unsigned i;
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

bool colstr_atomic_write(
	colstr_t* cstr, const char* base, unsigned size)
{
	if (!cstr || !base || (size == 0))
		return false;

	unsigned orig_size     = cstr->size;
	unsigned orig_col      = cstr->col;
	bool     orig_oversize = cstr->oversize;

	if (cstr->oversize)
	{
		if (!colstr__enlarge(cstr, 7))
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
		if (colstr_atomic_write__oversized(
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
		if (!colstr__enlarge(
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

	if (!colstr__enlarge(cstr, size))
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

bool colstr_atomic_writef(
	colstr_t* cstr,
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

	return colstr_atomic_write(
		cstr, buff, len);
}

bool colstr_fdprint(colstr_t* cstr, int fd)
{
	if (!cstr || !cstr->base)
		return false;

	return (dprintf(fd, "%.*s",
		cstr->size, cstr->base) > 0);
}
