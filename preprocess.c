#include "preprocess.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>




static bool is_vspace(char c)
{
	return ((c == '\r') || (c == '\n'));
}

static bool is_hspace(char c)
{
	switch (c)
	{
		case ' ':
		case '\t':
		case '\f':
		case '\v':
			return true;
		default:
			break;
	}
	return false;
}

static unsigned preprocess_fixed_form__blank_or_comment(
	const char* src, lang_opts_t opts)
{
	if (!src)
		return 0;

	if ((toupper(src[0]) == 'C')
		|| (src[0] == '*')
		|| (src[0] == '!')
		|| (opts.debug && (toupper(src[0]) == 'D')))
	{
		unsigned i;
		for (i = 1; (src[i] != '\0') && !is_vspace(src[i]); i++);
		if (is_vspace) i++;
		return i;
	}
	else
	{
		unsigned i;
		for (i = 0; (i < opts.columns) && (src[i] !='\0')
			&& !is_vspace(src[i]) && is_hspace(src[i]); i++);
		if ((i >= opts.columns) || (src[i] == '!'))
			for (; (src[i] != '\0') && !is_vspace(src[i]); i++);
		else if ((src[i] != '\0') && !is_vspace(src[i]))
			return 0;

		return (is_vspace(src[i]) ? (i + 1) : i);
	}

	return 0;
}

static unsigned preprocess_fixed_form__label(
	const char* src, lang_opts_t opts,
	bool* has_label, unsigned* label, bool* continuation)
{
	if (!src)
		return 0;

	bool seen_digit = false;
	unsigned label_value = 0;
	bool is_cont = false;

	unsigned i = 0;
	if ((toupper(src[i]) == 'D') && !opts.debug)
		i += 1;

	if (opts.form == LANG_FORM_TAB)
	{
		for (; (src[i] != '\0') && !is_vspace(src[i]) && (src[i] != '\t') && (src[i] != '!'); i++)
		{
			if (isdigit(src[i]))
			{
				seen_digit = true;
				unsigned nvalue = (label_value * 10) + (src[i] - '0');
				if (((nvalue / 10) != label_value)
					|| ((nvalue % 10) != (src[i] - '0')))
				{
					/* TODO - Positional error. */
					fprintf(stderr, "Error: Label number too large.\n");
					return 0;
				}
			}
			else if (!is_hspace(src[i]))
			{
				/* TODO - Positional error. */
				fprintf(stderr, "Error: Unexpected character in label.\n");
				return 0;
			}
		}

		if ((src[i] == '\0')
			|| is_vspace(src[i])
			|| (src[i] == '!'))
			return i;

		/* Skip tab. */
		i += 1;

		is_cont = (isdigit(src[i]) && (src[i] != '0'));
		if (is_cont) i += 1;
	}
	else
	{
		for (; (i < 5) && (src[i] != '\0') && !is_vspace(src[i]) && (src[i] != '!'); i++)
		{
			if (isdigit(src[i]))
			{
				seen_digit = true;
				label_value = (label_value * 10) + (src[i] - '0');
			}
			else if (!is_hspace(src[i]))
			{
				/* TODO - Positional error. */
				fprintf(stderr, "Error: Unexpected character in label.\n");
				return 0;
			}
		}

		/* Empty maybe labelled statement */
		if ((i < 5)
			|| (src[i] == '\0')
			|| is_vspace(src[i])
			|| (src[i] == '!'))
			return i;

		is_cont = (!is_hspace(src[i]) && (src[i] != '0'));

		/* Skip contuation character. */
		i += 1;
	}

	if (has_label   ) *has_label	= seen_digit;
	if (label       ) *label        = label_value;
	if (continuation) *continuation = is_cont;
	return i;
}

static unsigned preprocess_fixed_form__code(
	const char* path, unsigned row, unsigned col,
	const char* src, lang_opts_t opts, rope_t** rope)
{
	if (!src)
		return 0;

	unsigned remain = (col < opts.columns ? (opts.columns - col) : 0);

	rope_t* r = (rope ? rope_create() : NULL);
	if (!r) return 0;

	const char* base = src;
	unsigned    size = 0;

	unsigned i;
	for (i = 0; i < remain; i++)
	{
		bool eol = ((src[i] == '\0')
			|| (src[i] == '!')
			|| is_vspace(src[i]));

		if (eol || is_hspace(src[i]))
		{
			if ((size > 0) && rope && !rope_append_strn(
				r, path, row, col, base, size))
			{
				rope_delete(r);
				return 0;
			}

			size = 0;
			if (eol) break;
			continue;
		}

		if (size == 0)
			base = &src[i];
		size += 1;
	}

	if (rope) *rope = r;
	return i;
}

bool preprocess_fixed_form(file_t* file)
{
	if (!file)
		return false;

	const char* path = file_get_path(file);
	const char* src  = file_get_strz(file);
	lang_opts_t opts = file_get_lang_opts(file);

	bool first_code_line = true;

	if (!src)
		return false;

	unsigned row, pos;
	for (row = 0, pos = 0; src[pos] != '\0'; row++)
	{
		unsigned len, col;

		len = preprocess_fixed_form__blank_or_comment(
			&src[pos], opts);
		pos += len;
		if (len > 0) continue;

		bool has_label = false;
		unsigned label = 0;
		bool continuation = false;

		len = preprocess_fixed_form__label(
			&src[pos], opts,
			&has_label, &label, &continuation);
		if (len == 0) return false;
		col  = len;
		pos += len;

		for(; is_hspace(src[pos]); pos++, col++);

		rope_t* rope = NULL;
		if ((col < opts.columns)
			&& (src[pos] != '\0')
			&& !is_vspace(src[pos])
			&& (src[pos] != '!'))
		{
			len = preprocess_fixed_form__code(
				path, row, col, &src[pos], opts, &rope);
			pos += len;
			if (len == 0)
				return false;
		}

		for (; (src[pos] != '\0') && !is_vspace(src[pos]); pos++);
		if (is_vspace(src[pos])) pos++;

		if (first_code_line && continuation)
		{
			fprintf(stderr, "Warning:%s:%u: Initial line shouldn't be a continuation, ignoring.\n", path, (row + 1));
		}

		if (has_label && continuation)
		{
			fprintf(stderr, "Warning:%s:%u: Labeling a continuation line doesn't make sense, ignoring.\n", path, (row + 1));
		}

		/* Skip empty line. */
		if (!rope)
			continue;

		if (first_code_line || !continuation)
		{
			line_t* line = line_create((has_label ? &label : NULL), rope);
			if (!line) return false;

			if (!file_append_line(file, line))
			{
				line_delete(line);
				return false;
			}

			first_code_line = false;
		}
		else
		{
			bool append_success = file_append_rope(file, rope);
			rope_delete(rope);
			if (!append_success)
				return false;
		}


	}

	return true;
}
