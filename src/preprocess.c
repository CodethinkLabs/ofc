#include "preprocess.h"
#include "label_table.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


struct preprocess_s
{
	file_t*     file;
	lang_opts_t opts;

	rope_t*        unformat;
	rope_t*        condense;
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

	context->unformat = rope_create();
	context->condense = rope_create();
	context->labels   = label_table_create();

	if (!context->unformat
		|| !context->condense
		|| !context->labels)
	{
		rope_delete(context->unformat);
		rope_delete(context->condense);
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
	rope_delete(context->condense);
	rope_delete(context->unformat);
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

static unsigned preprocess__unformat_blank_or_comment(
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
		if (is_vspace(src[i])) i++;
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

static unsigned preprocess__unformat_fixed_form_label(
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
					|| ((nvalue % 10U) != (unsigned)(src[i] - '0')))
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
			|| is_vspace(src[i]))
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

static unsigned preprocess__unformat_code(
	const char* path, unsigned row, unsigned col, unsigned pos,
	const char* src, lang_opts_t opts, rope_t** rope)
{
	if (!src)
		return 0;

	unsigned remain = (col < opts.columns ? (opts.columns - col) : 0);

	unsigned i;
	for (i = 0; (i < remain) && !is_vspace(src[i]) && (src[i] != '\0'); i++);

	if (rope && !rope_append_strn(
		*rope, path, row, col, pos, src, i))
		return 0;

	return i;
}

static bool preprocess__unformat_fixed_form(preprocess_t* context)
{
	const char* path = file_get_path(context->file);
	const char* src  = file_get_strz(context->file);
	lang_opts_t opts = context->opts;

	bool first_code_line = true;

	unsigned newline_row = 0;
	unsigned newline_col = 0;
	unsigned newline_pos = 0;

	if (!src)
		return false;

	unsigned row, pos;
	for (row = 0, pos = 0; src[pos] != '\0'; row++)
	{
		unsigned len, col;

		len = preprocess__unformat_blank_or_comment(
			&src[pos], opts);
		pos += len;
		if (len > 0) continue;

		bool has_label = false;
		unsigned label = 0;
		bool continuation = false;

		len = preprocess__unformat_fixed_form_label(
			&src[pos], opts,
			&has_label, &label, &continuation);
		if (len == 0) return false;
		col  = len;
		pos += len;

		if (first_code_line && continuation)
		{
			fprintf(stderr, "Warning:%s:%u: Initial line can't be a continuation"
				", treating as non-continuation line.\n", path, (row + 1));
		}

		if (has_label)
		{
			if (continuation)
			{
				fprintf(stderr, "Warning:%s:%u: Labeling a continuation line doesn't make sense"
					", ignoring label.\n", path, (row + 1));
			}
			else
			{
				/* Mark current position in rope as label. */
				unsigned position = rope_len(context->unformat);
				if (!label_table_add(context->labels, position, label))
					return false;
			}
		}

		/* Skip initial space. */
		for(; is_hspace(src[pos]); pos++, col++);

		bool has_code = ((col < opts.columns)
			&& (src[pos] != '\0')
			&& !is_vspace(src[pos]));

		if (has_code)
		{
			static const char* PREPROCESS_NEWLINE = "\n";

			/* Insert single newline character at the end of each line in output. */
			if (!first_code_line && !continuation
				&& !rope_append_strn(context->unformat,
					path, newline_row, newline_col, newline_pos, PREPROCESS_NEWLINE, 1))
				return false;

			/* Append non-empty line to output. */
			len = preprocess__unformat_code(
				path, row, col, pos, &src[pos], opts, &context->unformat);
			pos += len;
			col += len;
			if (len == 0)
				return false;

			first_code_line = false;
		}
		else
		{
			fprintf(stderr, "Warning:%s:%u: Blank or comment line with non-empty first column.\n", path, (row + 1));
		}

		/* Skip to the actual end of the line, including all ignored characters. */
		for (; (src[pos] != '\0') && !is_vspace(src[pos]); pos++);

		if (has_code)
		{
			newline_row = row;
			newline_col = col;
			newline_pos = pos;
		}

		/* Eat vspace in input code. */
		if (is_vspace(src[pos])) pos++;
	}

	return true;
}

static bool preprocess__condense(preprocess_t* context)
{
	if (!context)
		return false;

	const char* src
		= rope_strz(context->unformat);
	if (!src) return false;

	unsigned i = 0;
	while (src[i] != '\0')
	{
		/* Skip whitespace. */
		for (; (src[i] != '\0') && is_hspace(src[i]); i++);

		if (src[i] == '\0')
			break;

		/* Parse non-whitespace. */
		const char* base = &src[i];
		unsigned pos = i;
		unsigned size;
		for(size = 0; (src[i] != '\0') && !is_hspace(src[i]); size++, i++);

		/* Append non-whitespace to condense rope. */
		if (!rope_append_strn(
			context->condense,
			NULL, 0, 0, pos,
			base, size))
			return false;
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
			success = preprocess__unformat_fixed_form(context);
			break;
		default:
			/* TODO - Parse free-form. */
			success = false;
			/* preprocess__unformat_free_form(context); */
			break;
	}

	success = success && preprocess__condense(context);

	if (!success)
	{
		/* We don't take ownership of the file on failure. */
		context->file = NULL;

		preprocess_delete(context);
		return NULL;
	}

	return context;
}



const file_t* preprocess_original_file(const preprocess_t* context)
{
	return (context ? context->file : NULL);
}

const rope_t* preprocess_unformat_rope(const preprocess_t* context)
{
	return (context ? context->unformat : NULL);
}

const rope_t* preprocess_condense_rope(const preprocess_t* context)
{
	return (context ? context->condense : NULL);
}



bool preprocess_unformat_has_label(
	const preprocess_t* context, unsigned position,
	unsigned* number)
{
	if (!context)
		return false;
	return label_table_find(
		context->labels,
		position, number);
}

bool preprocess_condense_has_label(
	const preprocess_t* context, unsigned position,
	unsigned* number)
{
	if (!context
		|| !context->unformat
		|| !context->condense)
		return false;

	unsigned unformat_position;
	if (!rope_position(
		context->condense, position,
		NULL, NULL, NULL, &unformat_position))
		return false;

	return preprocess_unformat_has_label(
		context, unformat_position, number);
}
