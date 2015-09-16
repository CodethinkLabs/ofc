#include "preprocess.h"
#include "label_table.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


struct preprocess_s
{
	file_t*     file;

	sparse_t*      unformat;
	sparse_t*      condense;
	label_table_t* labels;
};


static preprocess_t* preprocess__create(
	file_t* file)
{
	preprocess_t* context
		= (preprocess_t*)malloc(
			sizeof(preprocess_t));
	if (!context) return NULL;

	context->file = file;

	context->unformat = sparse_create_file(file);
	context->condense = NULL;
	context->labels   = label_table_create();

	if (!context->unformat
		|| !context->labels)
	{
		label_table_delete(context->labels);
		sparse_delete(context->unformat);
		free(context);
		return NULL;
	}

	return context;
}

void preprocess_delete(preprocess_t* context)
{
	if (!context)
		return;

	sparse_delete(context->condense);
	label_table_delete(context->labels);
	sparse_delete(context->unformat);
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

static bool is_ident(char c)
{
	return isalnum(c) || (c == '_');
}

static unsigned preprocess__unformat_blank_or_comment(
	const char* src, lang_opts_t opts)
{
	if (!src)
		return 0;

	bool is_comment
		= ((toupper(src[0]) == 'C') || (src[0] == '*')
			|| (opts.debug && (toupper(src[0]) == 'D')));

	if ((opts.form != LANG_FORM_FIXED)
		&& (opts.form != LANG_FORM_TAB))
		is_comment = false;

	if (is_comment || (src[0] == '!'))
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

static unsigned preprocess__unformat_free_form_label(
	const char* src, unsigned* label)
{
  if (!src)
	  return 0;

	unsigned label_value = 0;
	unsigned i;
	for (i = 0; (src[i] != '\0') && isdigit(src[i]); i++)
	{
		unsigned nvalue = (label_value * 10) + (src[i] - '0');
		if (((nvalue / 10) != label_value)
			|| ((nvalue % 10U) != (unsigned)(src[i] - '0')))
		{
			/* TODO - Positional error. */
			fprintf(stderr, "Error: Label number too large.\n");
			return 0;
		}
	}

	if ((i > 0) || is_hspace(src[i]))
	{
		if (label) *label = label_value;
		return (i + 1);
	}

	return 0;
}

static unsigned preprocess__unformat_fixed_form_code(
	unsigned col, const char* src, lang_opts_t opts, sparse_t** sparse)
{
	if (!src)
		return 0;

	unsigned remain = (col < opts.columns ? (opts.columns - col) : 0);

	unsigned i;
	for (i = 0; (i < remain) && !is_vspace(src[i]) && (src[i] != '\0'); i++);

	if (sparse && !sparse_append_strn(*sparse, src, i))
		return 0;

	return i;
}


typedef struct
{
	char string_delim;
	bool was_escape;

	/* We've seen an ident starting character. */
	bool in_ident;
	bool in_number;
} pre_state_t;

static const pre_state_t PRE_STATE_DEFAULT =
{
	.string_delim = '\0',
	.was_escape = false,

	.in_ident = false,
	.in_number = false,
};

static unsigned preprocess__unformat_free_form_code(
	unsigned col, pre_state_t* state,
	const char* src, lang_opts_t opts,
	sparse_t** sparse, bool* continuation)
{
	if (!src)
		return 0;

	unsigned remain = (col < opts.columns ? (opts.columns - col) : 0);

	bool     valid_ampersand = false;
	unsigned last_ampersand = 0;

	unsigned hollerith_size   = 0;
	unsigned hollerith_remain = 0;

	if (*continuation && (*src == '&'))
		src++;

	unsigned i;
	for (i = 0; (i < remain) && !is_vspace(src[i]) && (src[i] != '\0'); i++)
	{
		/* Allow the last ampersand prior to a bang comment as continuation. */
		if (!is_hspace(src[i]) && (src[i] != '!'))
			valid_ampersand = false;

		if (state->string_delim != '\0')
		{
			if (!state->was_escape
				&& (src[i] == state->string_delim))
				state->string_delim = '\0';

			/* String continuations are valid. */
			if (!state->was_escape && (src[i] == '&'))
			{
				last_ampersand = i;
				valid_ampersand = true;
			}

			state->was_escape = (src[i] == '\\');
		}
		else if (hollerith_remain > 0)
		{
			/* Ignore hollerith characters. */
			hollerith_remain--;
		}
		else
		{
			/* Break if we see the start of a bang comment. */
			if (src[i] == '!')
				break;

			if (src[i] == '&')
			{
				last_ampersand = i;
				valid_ampersand = true;
			}

			if ((src[i] == '\"')
				|| (src[i] == '\''))
			{
				state->string_delim = src[i];
				state->in_ident = false;
				state->in_number = false;
			}
			else if (state->in_ident)
			{
				state->in_ident = is_ident(src[i]);
			}
			else if (state->in_number)
			{
				if (toupper(src[i]) == 'H')
				{
					hollerith_remain = hollerith_size;
					state->in_ident = false;
				}
				else
				{
					state->in_ident  = isalpha(src[i]);
				}

				state->in_number = isdigit(src[i]);
				if (state->in_number)
				{
					unsigned nsize = (hollerith_size * 10) + (src[i] - '0');
					if (((nsize / 10) != hollerith_size)
						|| ((nsize % 10U) != (unsigned)(src[i] - '0')))
					{
						/* TODO - Positional error. */
						fprintf(stderr, "Error: Hollerith too long.\n");
						return 0;
					}

					hollerith_size = nsize;
				}
				else
				{
					hollerith_size = 0;
				}
			}
			else
			{
				state->in_number = isdigit(src[i]);
				state->in_ident  = isalpha(src[i]);

				if (state->in_number)
					hollerith_size = (src[i] - '0');
			}
		}
	}

	unsigned code_len = i;

	/* If we saw an ampersand not in a string or hollerith
	   and there's been nothing but whitespace since,
	   then this is a free form continuation. */
	*continuation = valid_ampersand;
	if (valid_ampersand)
		code_len = last_ampersand;

	if (sparse && !sparse_append_strn(
		*sparse, src, code_len))
		return 0;

	return i;
}

static bool preprocess__unformat_fixed_form(preprocess_t* context)
{
	const char* path = file_get_path(context->file);
	const char* src  = file_get_strz(context->file);
	lang_opts_t opts = file_get_lang_opts(context->file);

	bool first_code_line = true;

	const char* newline = NULL;

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

		if (first_code_line && continuation)
		{
			fprintf(stderr, "Warning:%s:%u: Initial line can't be a continuation"
				", treating as non-continuation line\n", path, (row + 1));
		}

		col  = len;
		pos += len;

		/* Skip initial space. */
		for(; is_hspace(src[pos]); pos++, col++);

		bool has_code = ((col < opts.columns)
			&& (src[pos] != '\0')
			&& !is_vspace(src[pos]));

		if (has_code)
		{
			/* Insert single newline character at the end of each line in output. */
			if (!first_code_line && !continuation
				&& !sparse_append_strn(context->unformat, newline, 1))
				return false;

			if (has_label)
			{
				if (continuation)
				{
					fprintf(stderr, "Warning:%s:%u: "
						"Labeling a continuation line doesn't make sense"
						", ignoring label", path, (row + 1));
				}
				else
				{
					/* Mark current position in file as label. */
					if (!label_table_add(context->labels, &src[pos], label))
						return false;
				}
			}

			/* Append non-empty line to output. */
			len = preprocess__unformat_fixed_form_code(
				col, &src[pos], opts, &context->unformat);
			pos += len;
			col += len;
			if (len == 0)
				return false;

			first_code_line = false;
		}
		else
		{
			fprintf(stderr, "Warning:%s:%u: "
				"Blank or comment line with non-empty first column", path, (row + 1));

			if (has_label)
			{
				/* TODO - Attach label to next code line, this leads to the possibility
				          of duplicate labels on a line, which we can't yet handle. */
				fprintf(stderr, "Warning:%s:%u: "
					"Label attached to blank line, will be ignored", path, (row + 1));
			}
		}

		/* Skip to the actual end of the line, including all ignored characters. */
		for (; (src[pos] != '\0') && !is_vspace(src[pos]); pos++);

		if (has_code)
			newline = &src[pos];

		/* Eat vspace in input code. */
		if (is_vspace(src[pos])) pos++;
	}

	return true;
}

static bool preprocess__unformat_free_form(preprocess_t* context)
{
	const char* src  = file_get_strz(context->file);
	lang_opts_t opts = file_get_lang_opts(context->file);
	pre_state_t state = PRE_STATE_DEFAULT;

	bool first_code_line = true;
	bool continuation = false;

	const char* newline = NULL;

	if (!src)
		return false;

	unsigned row, pos;
	for (row = 0, pos = 0; src[pos] != '\0'; row++)
	{
		unsigned len, col;

		len = preprocess__unformat_blank_or_comment(
			&src[pos], opts);
		pos += len;
		if (len > 0) {
			continue;
		}

		bool has_label;
		unsigned label = 0;

		len = preprocess__unformat_free_form_label(
			&src[pos], &label);
		has_label = (len > 0);

		if (has_label && !label_table_add(
			context->labels, &src[pos], label))
			return false;

		col  = len;
		pos += len;

		for(; is_hspace(src[pos]); pos++);

		bool has_code = ((col < opts.columns)
			&& (src[pos] != '\0')
			&& !is_vspace(src[pos]));

		if (has_code)
		{
			if (!first_code_line && !continuation
				&& !sparse_append_strn(context->unformat, newline, 1))
				return false;

			len = preprocess__unformat_free_form_code(
				col, &state, &src[pos], opts,
				&context->unformat, &continuation);
			pos += len;
			col += len;
			if (len == 0) return false;

			if (!continuation)
				state = PRE_STATE_DEFAULT;

			first_code_line = false;
		}

		for (; (src[pos] != '\0') && !is_vspace(src[pos]); pos++);

		if (has_code)
			newline = &src[pos];

		/* Eat vspace in input code. */
		if (is_vspace(src[pos])) pos++;

	}
	return true;
}

static sparse_t* preprocess__condense(preprocess_t* context)
{
	if (!context)
		return NULL;

	sparse_lock(context->unformat);

	const char* src
		= sparse_strz(context->unformat);
	if (!src) return NULL;

	sparse_t* condense
		= sparse_create_child(context->unformat);
	if (!condense) return NULL;

	unsigned i = 0;
	while (src[i] != '\0')
	{
		/* Skip whitespace. */
		for (; (src[i] != '\0') && is_hspace(src[i]); i++);

		if (src[i] == '\0')
			break;

		/* Parse non-whitespace. */
		const char* base = &src[i];
		unsigned size;
		for(size = 0; (src[i] != '\0') && !is_hspace(src[i]); size++, i++);

		/* Append non-whitespace to condense sparse. */
		if (!sparse_append_strn(condense, base, size))
		{
			sparse_delete(condense);
			return NULL;
		}
	}

	sparse_lock(condense);
	return condense;
}


preprocess_t* preprocess(file_t* file)
{
	if (!file)
		return NULL;

	preprocess_t* context = preprocess__create(file);
	lang_opts_t opts = file_get_lang_opts(file);
	if (!context) return NULL;

	bool success;
	switch (opts.form)
	{
		case LANG_FORM_FIXED:
		case LANG_FORM_TAB:
			success = preprocess__unformat_fixed_form(context);
			break;
		case LANG_FORM_FREE:
			success = preprocess__unformat_free_form(context);
			break;
		default:
			success = false;
			break;
	}

	if (success)
		context->condense = preprocess__condense(context);

	if (!context->condense)
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

const sparse_t* preprocess_unformat_sparse(const preprocess_t* context)
{
	return (context ? context->unformat : NULL);
}

const sparse_t* preprocess_condense_sparse(const preprocess_t* context)
{
	return (context ? context->condense : NULL);
}

const label_table_t* preprocess_labels(const preprocess_t* context)
{
	return (context ? context->labels : NULL);
}
