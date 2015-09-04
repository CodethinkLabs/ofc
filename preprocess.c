#include "preprocess.h"
#include "rope.h"
#include "label_table.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


struct preprocess_s
{
	file_t*     file;
	lang_opts_t opts;

	rope_t*        output;
	label_table_t* labels;
};



static preprocess_t* preprocess__create(
	file_t* file, lang_opts_t opts)
{
	preprocess_t* context
		= (preprocess_t*)malloc(
			sizeof(preprocess_t));
	if (!context) return NULL;

	context->file = file;
	context->opts = opts;

	context->output = rope_create();
	context->labels = label_table_create();

	if (!context->output
		|| !context->labels)
	{
		rope_delete(context->output);
		label_table_delete(context->labels);
		free(context);
		return NULL;
	}

	return context;
}

void preprocess_delete(preprocess_t* context)
{
	if (!context)
		return;

	label_table_delete(context->labels);
	rope_delete(context->output);
	file_delete(context->file);
	free(context);
}



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

bool preprocess__fixed_form(preprocess_t* context)
{
	const char* path = file_get_path(context->file);
	const char* src  = file_get_strz(context->file);
	lang_opts_t opts = context->opts;

	bool first_code_line = true;

	unsigned newline_row = 0;
	unsigned newline_col = 0;

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
			col += len;
			if (len == 0)
				return false;
		}

		/* Skip to the actual end of the line, including all ignored characters. */
		for (; (src[pos] != '\0') && !is_vspace(src[pos]); pos++);

		if (first_code_line && continuation)
		{
			fprintf(stderr, "Warning:%s:%u: Initial line shouldn't be a continuation, ignoring.\n", path, (row + 1));
		}

		if (has_label)
		{
			if (continuation)
			{
				fprintf(stderr, "Warning:%s:%u: Labeling a continuation line doesn't make sense, ignoring.\n", path, (row + 1));
			}
			else
			{
				/* Mark current position in rope as label. */
				unsigned position = rope_len(context->output);
				if (!label_table_add(context->labels, position, label))
					return false;
			}
		}

		if (rope)
		{
			static const char* PREPROCESS_NEWLINE = "\n";

			/* Insert single newline character at the end of each line in output. */
			if (!first_code_line && !continuation
				&& !rope_append_strn(context->output,
					path, newline_row, newline_col, PREPROCESS_NEWLINE, 1))
				return false;

			/* Append non-empty line to output. */
			bool append_success = rope_append_rope(context->output, rope);
			rope_delete(rope);
			if (!append_success)
				return false;

			newline_row = row;
			newline_col = col;

			first_code_line = false;
		}

		/* Eat vspace in input code. */
		if (is_vspace(src[pos])) pos++;
	}

	return true;
}


preprocess_t* preprocess(file_t* file, lang_opts_t opts)
{
	if (!file)
		return NULL;

	preprocess_t* context = preprocess__create(file, opts);
	if (!context) return NULL;

	bool success;
	switch (opts.form)
	{
		case LANG_FORM_FIXED:
		case LANG_FORM_TAB:
			success = preprocess__fixed_form(context);
			break;
		default:
			/* TODO - Parse free-form. */
			success = false;
			break;
	}

	if (!success)
	{
		preprocess_delete(context);
		return NULL;
	}

	return context;
}


const char* preprocess_strz(const preprocess_t* context)
{
	if (!context)
		return NULL;
	return rope_strz(context->output);
}

bool preprocess_debug_position(
	const preprocess_t* context, unsigned position,
	const char** file, unsigned *row, unsigned* col)
{
	if (!context)
		return false;
	return rope_position(
		context->output, position,
		file, row, col);
}

bool preprocess_has_label(
	const preprocess_t* context, unsigned position,
	unsigned* number)
{
	if (!context)
		return false;
	return label_table_find(
		context->labels,
		position, number);
}
