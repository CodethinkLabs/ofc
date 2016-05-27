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

#include "ofc/fctype.h"
#include "ofc/prep.h"


static unsigned ofc_prep_unformat__blank_or_comment(
	const ofc_file_t* file, const char* src,
	const ofc_lang_opts_t* opts)
{
	if (!src || !opts)
		return 0;

	bool is_comment = (src[0] == '!');
	if (opts->form == OFC_LANG_FORM_FIXED)
	{
		if ((toupper(src[0]) == 'C') || (src[0] == '*')
			|| (!opts->debug && (toupper(src[0]) == 'D')))
			is_comment = true;
	}

	if (is_comment && strncasecmp(&src[1], "$PRAGMA", 7) == 0)
	{
		/* TODO - PRAGMA - Store these for processing later. */
		ofc_file_warning(file, &src[1],
			"$PRAGMA not supported, ignoring");
	}

	bool prep_hash_warn = false;
	unsigned i = 0, t = 0;
	if (!is_comment)
	{
		for (; (i < opts->columns) && (src[i] != '\0')
			&& !ofc_is_vspace(src[i]) && ofc_is_hspace(src[i]); i++)
		{
			if (src[i] == '\t')
				t += 1;
		}

		if ((opts->form == OFC_LANG_FORM_FIXED)
			&& (i == 5) && (t == 0)
			&& (src[i] != '\0')
			&& !ofc_is_vspace(src[i]))
		{
			if (src[i] == '#')
				prep_hash_warn = true;

			for (i++; (i < opts->columns) && (src[i] != '\0')
				&& !ofc_is_vspace(src[i]) && ofc_is_hspace(src[i]); i++)
			{
				if (src[i] == '\t')
					t += 1;
			}
		}

		is_comment = (src[i] == '!');
	}

	if (!is_comment && (i < opts->columns)
		&& (src[i] != '\0') && !ofc_is_vspace(src[i]))
		return 0;

	if (prep_hash_warn)
	{
		ofc_file_warning(file, &src[i],
			"Using '#' as a continuation character"
			" may clash with the preprocessor");
	}

	for (; (src[i] != '\0') && !ofc_is_vspace(src[i]); i++);
	return (ofc_is_vspace(src[i]) ? (i + 1) : i);
}

static unsigned ofc_prep_unformat__fixed_form_label(
	const ofc_file_t* file, const char* src,
	const ofc_lang_opts_t* opts,
	bool* has_label, unsigned* label, bool* continuation,
	bool* extend)
{
	if (!src || !opts)
		return 0;

	bool seen_digit = false;
	unsigned label_value = 0;
	bool is_cont = false;
	bool was_tab = false;

	/* Infinite length continuation line.
		https://gcc.gnu.org/onlinedocs/gcc-3.4.5/g77/Ampersands.html#Ampersands */
	if (src[0] == '&')
	{
		if (has_label   ) *has_label	= false;
		if (label       ) *label        = 0;
		if (continuation) *continuation = true;
		if (extend      ) *extend       = true;
		return 1;
	}

	unsigned i = 0;
	if ((toupper(src[i]) == 'D') && !opts->debug)
		i += 1;

	for (; (i < 5) && !was_tab && (src[i] != '\0')
		&& !ofc_is_vspace(src[i]) && (src[i] != '!'); i++)
	{
		if (isdigit(src[i]))
		{
			seen_digit = true;
			label_value = (label_value * 10) + (src[i] - '0');
		}
		else if (src[i] == '\t')
		{
			was_tab = true;
		}
		else if (!ofc_is_hspace(src[i]))
		{
			ofc_file_error(file, &src[i],
				"Unexpected character in label");
			return 0;
		}
	}

	if (!was_tab && (i == 5)
		&& (src[i] == '\t'))
	{
		i++;
		was_tab = true;
	}

	/* Empty maybe labelled statement */
	if (was_tab)
	{
		/* After a tab, only a non-zero digit can represent a continuation
			https://docs.oracle.com/cd/E19205-01/819-5263/aevlv/index.html */
		is_cont = (isdigit(src[i]) && (src[i] != '0'));
		if (is_cont) i++;
	}
	else if (i == 5)
	{
		is_cont = ((src[i] != '\0') && !ofc_is_vspace(src[i])
			&& !ofc_is_hspace(src[i]) && (src[i] != '0'));

		if (src[i] == '#')
		{
			ofc_file_warning(file, &src[i],
				"Using '#' as a continuation character"
				" may clash with the preprocessor");
		}

		i++;
	}

	if (has_label   ) *has_label	= seen_digit;
	if (label       ) *label        = label_value;
	if (continuation) *continuation = is_cont;
	if (extend      ) *extend       = was_tab;
	return i;
}

static unsigned ofc_prep_unformat__free_form_label(
	const ofc_file_t* file, const char* src, unsigned* label)
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
			ofc_file_error(file, &src[i],
				"Label number too large");
			return 0;
		}
	}

	if ((i > 0) && ofc_is_hspace(src[i]))
	{
		if (label) *label = label_value;
		return i;
	}

	return 0;
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

static unsigned ofc_prep_unformat__fixed_form_code(
	unsigned* col, pre_state_t* state,
	const ofc_file_t* file, const char* src,
	const ofc_lang_opts_t* opts, bool extend,
	ofc_sparse_t* sparse)
{
	if (!src || !opts)
		return 0;

	unsigned hollerith_size   = 0;
	unsigned hollerith_remain = 0;
	bool     hollerith_too_long = false;

	unsigned i;
	for (i = 0; (extend || (*col < opts->columns))
		&& !ofc_is_vspace(src[i]) && (src[i] != '\0'); i++)
	{
		if (state->string_delim != '\0')
		{
			if (!state->was_escape
				&& (src[i] == state->string_delim))
				state->string_delim = '\0';

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

			if ((src[i] == '\"')
				|| (src[i] == '\''))
			{
				state->string_delim = src[i];
				state->in_ident = false;
				state->in_number = false;
			}
			else if (state->in_ident)
			{
				state->in_ident = ofc_is_ident(src[i])
					|| ofc_is_hspace(src[i]);
			}
			else if (state->in_number)
			{
				if (toupper(src[i]) == 'H')
				{
					if (hollerith_too_long)
					{
						ofc_file_warning(file, &src[i],
							"Hollerith too long, ignoring");
					}
					else
					{
						hollerith_remain = hollerith_size;
						state->in_ident = false;
					}
				}
				else
				{
					state->in_ident = isalpha(src[i]);
				}

				state->in_number = isdigit(src[i]);
				if (state->in_number)
				{
					if (!hollerith_too_long)
					{
						unsigned nsize = (hollerith_size * 10) + (src[i] - '0');
						hollerith_too_long = (((nsize / 10) != hollerith_size)
							|| ((nsize % 10U) != (unsigned)(src[i] - '0')));

						hollerith_size = nsize;
					}
				}
				else
				{
					hollerith_too_long = false;
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

		if (src[i] == '\t')
			extend = true;
		(*col)++;
	}

	if (sparse && !ofc_sparse_append_strn(
		sparse, src, i))
		return 0;

	return i;
}

static unsigned ofc_prep_unformat__free_form_code(
	unsigned* col, pre_state_t* state,
	const ofc_file_t* file, const char* src,
	const ofc_lang_opts_t* opts,
	ofc_sparse_t* sparse, bool* continuation)
{
	if (!src)
		return 0;

	bool     valid_ampersand = false;
	unsigned last_ampersand = 0;

	unsigned hollerith_size   = 0;
	unsigned hollerith_remain = 0;
	bool     hollerith_too_long = false;

	if (*continuation && (*src == '&'))
	{
		src += 1;
		*col += 1;
	}

	unsigned i;
	for (i = 0; (*col < opts->columns)
		&& !ofc_is_vspace(src[i]) && (src[i] != '\0'); i++)
	{
		/* Allow the last ampersand prior to a bang comment as continuation. */
		if (!ofc_is_hspace(src[i]) && (src[i] != '!'))
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
				state->in_ident = ofc_is_ident(src[i]);
			}
			else if (state->in_number)
			{
				if (toupper(src[i]) == 'H')
				{
					if (hollerith_too_long)
					{
						ofc_file_warning(file, &src[i],
							"Hollerith too long, ignoring");
					}
					else
					{
						hollerith_remain = hollerith_size;
						state->in_ident = false;
					}
				}
				else
				{
					state->in_ident  = isalpha(src[i]);
				}

				state->in_number = isdigit(src[i]);
				if (state->in_number)
				{
					if (!hollerith_too_long)
					{
						unsigned nsize = (hollerith_size * 10) + (src[i] - '0');
						hollerith_too_long = (((nsize / 10) != hollerith_size)
							|| ((nsize % 10U) != (unsigned)(src[i] - '0')));

						hollerith_size = nsize;
					}
				}
				else
				{
					hollerith_too_long = false;
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

		*col += (src[i] == '\t' ? opts->tab_width : 1);
	}

	unsigned code_len = i;

	/* If we saw an ampersand not in a string or hollerith
	   and there's been nothing but whitespace since,
	   then this is a free form continuation. */
	*continuation = valid_ampersand;
	if (valid_ampersand)
		code_len = last_ampersand;

	if (sparse && !ofc_sparse_append_strn(
		sparse, src, code_len))
		return 0;

	return i;
}

static bool ofc_prep_unformat__fixed_form(
	const ofc_file_t* file, ofc_sparse_t* sparse)
{
	const char*     src   = ofc_file_get_strz(file);
	pre_state_t     state = PRE_STATE_DEFAULT;

	const ofc_lang_opts_t* opts
		= ofc_file_get_lang_opts(file);
	if (!opts) return false;

	bool first_code_line = true;

	const char* newline = NULL;

	if (!src)
		return false;

	bool had_label = false;
	unsigned label_prev = 0;
	unsigned label_pos = 0;

	unsigned row, pos;
	for (row = 0, pos = 0; src[pos] != '\0'; row++)
	{
		unsigned len, col;

		len = ofc_prep_unformat__blank_or_comment(
			file, &src[pos], opts);
		pos += len;
		if (len > 0) continue;

		bool has_label = false;
		unsigned label = 0;
		bool continuation = false;

		/* Extend lines are unlimited in length.
			Fixed-form lines containing a tab are extended.
			https://gcc.gnu.org/onlinedocs/gcc-3.4.5/g77/Tabs.html#Tabs */
		bool extend = false;

		col = ofc_prep_unformat__fixed_form_label(
			file, &src[pos], opts,
			&has_label, &label, &continuation, &extend);
		if (col == 0) return false;

		if (first_code_line && continuation)
		{
			ofc_file_warning(file, &src[pos],
				"Initial line can't be a continuation"
				", treating as non-continuation line");
			continuation = false;
		}

		if (continuation)
		{
			if (has_label)
			{
				ofc_file_warning(file, &src[pos],
					"Labeling a continuation line doesn't make sense"
					", ignoring label");
			}
			has_label = had_label;
			label = label_prev;
			had_label = false;
		}
		else if (had_label)
		{
			ofc_file_warning(file, &src[label_pos],
				"Label attached to blank line, will be ignored");
		}

		pos += col;

		/* Skip initial space. */
		if (!continuation)
		{
			for(; ofc_is_hspace(src[pos]); pos++, col++)
			{
				if (src[pos] == '\t')
					extend = true;
			}
			state = PRE_STATE_DEFAULT;
		}

		bool has_code = ((extend || (col < opts->columns))
			&& (src[pos] != '\0')
			&& !ofc_is_vspace(src[pos]));

		/* Insert single newline character at the end of each line of output. */
		if ((has_code || has_label)
			&& !first_code_line && !continuation
			&& !ofc_sparse_append_strn(sparse, newline, 1))
			return false;

		if (has_code)
		{
			if (has_label)
			{
				/* Mark current position in unformat stream as label. */
				if (!ofc_sparse_label_add(sparse, label))
					return false;
			}

			/* Append non-empty line to output. */
			len = ofc_prep_unformat__fixed_form_code(
				&col, &state, file, &src[pos], opts, extend, sparse);
			pos += len;
			if (len == 0) return false;

			first_code_line = false;
		}
		else if (has_label)
		{
			/* Label is on blank line, attach to next code line. */
			had_label  = true;
			label_prev = label;
			label_pos  = pos;
		}

		/* Skip to the actual end of the line, including all ignored characters. */
		for (; (src[pos] != '\0') && !ofc_is_vspace(src[pos]); pos++);

		if (has_code)
			newline = &src[pos];

		/* Eat vspace in input code. */
		if (ofc_is_vspace(src[pos])) pos++;
	}

	return true;
}

static bool ofc_prep_unformat__free_form(
	const ofc_file_t* file, ofc_sparse_t* sparse)
{
	const char*     src   = ofc_file_get_strz(file);
	pre_state_t     state = PRE_STATE_DEFAULT;

	const ofc_lang_opts_t* opts
		= ofc_file_get_lang_opts(file);
	if (!opts) return false;

	bool first_code_line = true;
	bool continuation = false;

	const char* newline = NULL;

	if (!src)
		return false;

	bool had_label = false;
	unsigned label_prev = 0;
	unsigned label_pos = 0;

	unsigned row, pos;
	for (row = 0, pos = 0; src[pos] != '\0'; row++)
	{
		unsigned len, col;

		len = ofc_prep_unformat__blank_or_comment(
			file, &src[pos], opts);
		pos += len;
		if (len > 0) {
			continue;
		}

		bool has_label;
		unsigned label = 0;

		len = ofc_prep_unformat__free_form_label(
			file, &src[pos], &label);
		has_label = (len > 0);

		if (continuation)
		{
			if (has_label)
			{
				ofc_file_warning(file, &src[pos],
					"Labeling a continuation line doesn't make sense"
					", ignoring label");
			}
			has_label = had_label;
			label = label_prev;
			had_label = false;
		}
		else if (had_label)
		{
			ofc_file_warning(file, &src[label_pos],
				"Label attached to blank line, will be ignored");
		}

		/* We can increment col by len, because
		   a free-form label can't contain a tab. */
		col  = len;
		pos += len;

		if (!continuation)
		{
			for(; ofc_is_hspace(src[pos]); pos++)
				col += (src[pos] == '\t' ? opts->tab_width : 1);
		}

		bool has_code = ((col < opts->columns)
			&& (src[pos] != '\0')
			&& !ofc_is_vspace(src[pos]));

		if (has_code)
		{
			if (has_label)
			{
				/* Mark current position in unformat stream as label. */
				if (!ofc_sparse_label_add(sparse, label))
					return false;
			}

			if (!first_code_line && !continuation
				&& !ofc_sparse_append_strn(sparse, newline, 1))
				return false;

			len = ofc_prep_unformat__free_form_code(
				&col, &state, file, &src[pos], opts,
				sparse, &continuation);
			pos += len;
			if (len == 0) return false;

			if (!continuation)
				state = PRE_STATE_DEFAULT;

			first_code_line = false;
		}
		else if (has_label)
		{
			/* Label is on blank line, attach to next code line. */
			had_label  = true;
			label_prev = label;
			label_pos  = pos;
		}

		/* Skip to the actual end of the line, including all ignored characters. */
		for (; (src[pos] != '\0') && !ofc_is_vspace(src[pos]); pos++);

		if (has_code)
			newline = &src[pos];

		/* Eat vspace in input code. */
		if (ofc_is_vspace(src[pos])) pos++;

	}
	return true;
}

ofc_sparse_t* ofc_prep_unformat(ofc_file_t* file)
{
	const ofc_lang_opts_t* lang_opts
		= ofc_file_get_lang_opts(file);
	if (!lang_opts) return NULL;

	ofc_sparse_t* unformat
		= ofc_sparse_create_file(file);
	if (!unformat) return NULL;

	bool success = false;
	switch (lang_opts->form)
	{
		case OFC_LANG_FORM_FIXED:
			success = ofc_prep_unformat__fixed_form(file, unformat);
			break;
		case OFC_LANG_FORM_FREE:
			success = ofc_prep_unformat__free_form(file, unformat);
			break;
		default:
			break;
	}

	if (!success)
	{
		ofc_sparse_delete(unformat);
		return NULL;
	}

	ofc_sparse_lock(unformat);
	return unformat;
}
