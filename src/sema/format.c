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

#include "ofc/sema.h"

static bool ofc_sema__type_rule[][9] =
{
	/* OFC_SEMA_TYPE_LOGICAL, OFC_SEMA_TYPE_INTEGER, OFC_SEMA_TYPE_REAL,
	   OFC_SEMA_TYPE_COMPLEX, OFC_SEMA_TYPE_BYTE, OFC_SEMA_TYPE_CHARACTER,
	   OFC_SEMA_TYPE_STRUCTURE, OFC_SEMA_TYPE_POINTER, OFC_SEMA_TYPE_ARRAY */

	{ 0, 1, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_INTEGER */
	{ 0, 0, 1, 1, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_REAL */
	{ 0, 0, 1, 1, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_D */
	{ 0, 0, 1, 1, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_E */
	{ 1, 1, 1, 1, 1, 1, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_G */
	{ 0, 0, 0, 0, 0, 1, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_CHARACTER */
	{ 1, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_LOGICAL */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_HOLLERITH */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_S */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_REAL_SCALE */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_X */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_T */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_SLASH */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_DOLLAR */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_BACKSLASH */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_Q */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_COLON */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_BN */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_BZ */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_SP */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_SS */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_TL */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_TR */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_STRING */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_REPEAT */
	{ 0, 1, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_BINARY */
	{ 0, 1, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_OCTAL */
	{ 0, 1, 0, 0, 0, 0, 0, 0, 0 }, /* OFC_PARSE_FORMAT_DESC_HEX */
};

static const char* ofc_sema_type__name[] =
{
	"INTEGER",
	"REAL",
	"D",
	"E",
	"G",
	"CHARACTER",
	"LOGICAL",
	"HOLLERITH",
	"S",
	"REAL_SCALE",
	"X",
	"T",
	"SLASH",
	"DOLLAR",
	"BACKSLASH",
	"Q",
	"COLON",
	"BN",
	"BZ",
	"SP",
	"SS",
	"TL",
	"TR",
	"STRING",
	"REPEAT",
	"BINARY",
	"OCTAL",
	"HEX",

	NULL
};

const char* ofc_sema_format_str_rep(
	const ofc_parse_format_desc_e type)
{
	if (type >= OFC_PARSE_FORMAT_DESC_COUNT)
		return NULL;

	return ofc_sema_type__name[type];
}

bool ofc_sema_compare_desc_expr_type(
	ofc_parse_format_desc_e type_desc,
	ofc_sema_type_e type_expr)
{
	return ofc_sema__type_rule[type_desc][type_expr];
}

bool ofc_sema_format_desc(
	const ofc_parse_format_desc_t* desc)
{
	if (!desc)
		return false;

	switch (desc->type)
	{
		case OFC_PARSE_FORMAT_DESC_E:
		case OFC_PARSE_FORMAT_DESC_G:
			if (desc->e_set && (desc->e == 0))
			{
				ofc_sparse_ref_error(desc->src,
					"Format descriptor requires non-zero exponent width");
				return false;
			}
			break;

		default:
			break;
	}

	switch (desc->type)
	{
		case OFC_PARSE_FORMAT_DESC_E:
		case OFC_PARSE_FORMAT_DESC_G:
		case OFC_PARSE_FORMAT_DESC_REAL:
		case OFC_PARSE_FORMAT_DESC_D:
			if (!desc->d_set)
			{
				ofc_sparse_ref_warning(desc->src,
					"Format descriptor should have decimal width");
			}
			break;

		default:
			break;
	}

	switch (desc->type)
	{
		case OFC_PARSE_FORMAT_DESC_E:
		case OFC_PARSE_FORMAT_DESC_G:
		case OFC_PARSE_FORMAT_DESC_REAL:
		case OFC_PARSE_FORMAT_DESC_D:
		case OFC_PARSE_FORMAT_DESC_INTEGER:
		case OFC_PARSE_FORMAT_DESC_BINARY:
		case OFC_PARSE_FORMAT_DESC_OCTAL:
		case OFC_PARSE_FORMAT_DESC_HEX:
			if (!desc->w_set)
			{
				ofc_sparse_ref_warning(desc->src,
					"Format descriptor should have field width");
			}
			break;

		default:
			break;
	}

	switch (desc->type)
	{
		case OFC_PARSE_FORMAT_DESC_E:
		case OFC_PARSE_FORMAT_DESC_G:
		case OFC_PARSE_FORMAT_DESC_D:
		case OFC_PARSE_FORMAT_DESC_LOGICAL:
		case OFC_PARSE_FORMAT_DESC_CHARACTER:
			if (desc->w_set && (desc->w == 0))
			{
				ofc_sparse_ref_error(desc->src,
					"Format descriptor requires non-zero field width");
				return false;
			}
			break;

		case OFC_PARSE_FORMAT_DESC_REAL_SCALE:
			if (!desc->n_set)
			{
				ofc_sparse_ref_warning(desc->src,
					"Format descriptor should have scale factor");
			}
			break;

		case OFC_PARSE_FORMAT_DESC_X:
			if (!desc->n_set)
				ofc_sparse_ref_warning(desc->src,
					"Format descriptor should have count");
			else if (desc->n == 0)
			{
				ofc_sparse_ref_error(desc->src,
					"Format descriptor requires non-zero count");
				return false;
			}
			break;

		case OFC_PARSE_FORMAT_DESC_TL:
		case OFC_PARSE_FORMAT_DESC_TR:
			if (!desc->w_set)
			{
				ofc_sparse_ref_error(desc->src,
					"Format descriptor requires count");
				return false;
			}
			else if (desc->w == 0)
			{
				ofc_sparse_ref_error(desc->src,
					"Format descriptor requires non-zero count");
				return false;
			}
			break;

		case OFC_PARSE_FORMAT_DESC_REPEAT:
		{
			unsigned i;
			for (i = 0; i < desc->repeat->count; i++)
			{
				if (!ofc_sema_format_desc(desc->repeat->desc[i]))
					return false;
			}
			return true;
		}

		default:
			return true;
	}

	if ((desc->w_set && !ofc_parse_format_desc_has_w(desc))
		|| (desc->d_set && !ofc_parse_format_desc_has_d(desc))
		|| (desc->e_set && !ofc_parse_format_desc_has_e(desc)))
	{
		ofc_sparse_ref_warning(desc->src,
			"Unexpected element in format descriptor");
	}

	/* The values of m, d, and e must not exceed
	   the value of w, the field width */
	if (desc->w_set)
	{
		unsigned i = 0;

		if (desc->d_set)
		{
			/* Decimal point */
			i += 1;
			i += desc->d;
		}

		if (desc->e_set)
		{
			/* E+ */
			i += 2;
			i += desc->e;
		}

		if (i > desc->w)
			ofc_sparse_ref_warning(desc->src,
				"Field width too small");
	}
	return true;
}

/* Defaults from https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnc3/index.html#z4000743817c */
ofc_parse_format_desc_t* ofc_sema_format_desc_set_def(
	const ofc_parse_format_desc_t* desc,
	const ofc_sema_expr_t* expr,
	const ofc_sema_lhs_t* lhs)
{
	if (!desc)
		return NULL;

	const ofc_sema_type_t* type;
	if (expr)
	{
		type = ofc_sema_expr_type(expr);
		if (!type) return NULL;
	}
	else
	{
		type = ofc_sema_lhs_type(lhs);
		if (!type) return NULL;
	}

	ofc_sema_kind_e kind
		= ofc_sema_type_get_kind(type);

	ofc_parse_format_desc_t* copy
		= ofc_parse_format_desc_copy(desc);
	if (!copy) return NULL;

	switch (desc->type)
	{
		case OFC_PARSE_FORMAT_DESC_E:
		case OFC_PARSE_FORMAT_DESC_G:
		case OFC_PARSE_FORMAT_DESC_REAL:
		case OFC_PARSE_FORMAT_DESC_D:
		{
			if ((type->type != OFC_SEMA_TYPE_REAL)
				&& (type->type != OFC_SEMA_TYPE_COMPLEX))
				break;

			switch (kind)
			{
				case OFC_SEMA_KIND_4_BYTE:
					if (!desc->e_set)
					{
						copy->e_set = true;
						copy->e = 2;
					}
					if (!desc->d_set)
					{
						copy->d_set = true;
						copy->d = 7;
					}
					if (!desc->w_set)
					{
						copy->w_set = true;
						copy->w = 15;
					}
					break;
				case OFC_SEMA_KIND_8_BYTE:
					if (!desc->e_set)
					{
						copy->e_set = true;
						copy->e = 2;
					}
					if (!desc->d_set)
					{
						copy->d_set = true;
						copy->d = 16;
					}
					if (!desc->w_set)
					{
						copy->w_set = true;
						copy->w = 25;
					}
					break;
				case OFC_SEMA_KIND_16_BYTE:
					if (!desc->e_set)
					{
						copy->e_set = true;
						copy->e = 3;
					}
					if (!desc->d_set)
					{
						copy->d_set = true;
						copy->d = 33;
					}
					if (!desc->w_set)
					{
						copy->w_set = true;
						copy->w = 42;
					}
					break;

				default:
					break;
			}
			break;
		}
		case OFC_PARSE_FORMAT_DESC_BINARY:
		case OFC_PARSE_FORMAT_DESC_OCTAL:
		case OFC_PARSE_FORMAT_DESC_HEX:
		{
			if ((type->type == OFC_SEMA_TYPE_REAL)
				&& (kind == OFC_SEMA_KIND_4_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 12;
				}
			}
			else if ((type->type == OFC_SEMA_TYPE_REAL)
				&& (kind == OFC_SEMA_KIND_8_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 23;
				}
			}
			else if (((type->type == OFC_SEMA_TYPE_REAL)
					|| (type->type == OFC_SEMA_TYPE_COMPLEX))
				&& (kind == OFC_SEMA_KIND_16_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 44;
				}
			}
			else if ((type->type == OFC_SEMA_TYPE_BYTE)
				&& (kind == OFC_SEMA_KIND_1_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 7;
				}
			}
			else if (((type->type == OFC_SEMA_TYPE_INTEGER)
					|| (type->type == OFC_SEMA_TYPE_LOGICAL))
				&& (kind == OFC_SEMA_KIND_2_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 7;
				}
			}
			else if (((type->type == OFC_SEMA_TYPE_INTEGER)
					|| (type->type == OFC_SEMA_TYPE_LOGICAL))
				&& (kind == OFC_SEMA_KIND_4_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 12;
				}
			}
			break;
		}
		case OFC_PARSE_FORMAT_DESC_INTEGER:
		{
			if ((type->type == OFC_SEMA_TYPE_BYTE)
				&& (kind == OFC_SEMA_KIND_1_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 7;
				}
			}
			else if (((type->type == OFC_SEMA_TYPE_INTEGER)
					|| (type->type == OFC_SEMA_TYPE_LOGICAL))
				&& (kind == OFC_SEMA_KIND_2_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 7;
				}
			}
			else if (((type->type == OFC_SEMA_TYPE_INTEGER)
					|| (type->type == OFC_SEMA_TYPE_LOGICAL))
				&& (kind == OFC_SEMA_KIND_4_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 12;
				}
			}
			break;
		}
		case OFC_PARSE_FORMAT_DESC_LOGICAL:
		{
			/* Oracle default for L is 2, but gfortran default is 1 */
			break;
		}
		case OFC_PARSE_FORMAT_DESC_CHARACTER:
		{
			if ((type->type == OFC_SEMA_TYPE_LOGICAL)
				&& (kind == OFC_SEMA_KIND_1_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 1;
				}
			}
			else if (((type->type == OFC_SEMA_TYPE_INTEGER)
					|| (type->type == OFC_SEMA_TYPE_LOGICAL))
				&& (kind == OFC_SEMA_KIND_2_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 2;
				}
			}
			else if (((type->type == OFC_SEMA_TYPE_INTEGER)
					|| (type->type == OFC_SEMA_TYPE_LOGICAL))
				&& (kind == OFC_SEMA_KIND_4_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 4;
				}
			}
			else if (((type->type == OFC_SEMA_TYPE_REAL)
					|| (type->type == OFC_SEMA_TYPE_COMPLEX))
				&& (kind == OFC_SEMA_KIND_4_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 4;
				}
			}
			else if (((type->type == OFC_SEMA_TYPE_REAL)
					|| (type->type == OFC_SEMA_TYPE_COMPLEX))
				&& (kind == OFC_SEMA_KIND_8_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 8;
				}
			}
			else if (((type->type == OFC_SEMA_TYPE_REAL)
					|| (type->type == OFC_SEMA_TYPE_COMPLEX))
				&& (kind == OFC_SEMA_KIND_16_BYTE))
			{
				if (!desc->w_set)
				{
					copy->w_set = true;
					copy->w = 16;
				}
			}
			break;
		}
		case OFC_PARSE_FORMAT_DESC_REAL_SCALE:
			if (!desc->n_set)
			{
				copy->n_set = true;
				copy->n = 0;
			}
			break;

		case OFC_PARSE_FORMAT_DESC_X:
			if (!desc->n_set)
			{
				copy->n_set = true;
				copy->n = 1;
			}
			break;

		case OFC_PARSE_FORMAT_DESC_TL:
		case OFC_PARSE_FORMAT_DESC_TR:
			if (!desc->w_set)
			{
				copy->w_set = true;
				copy->w = 1;
			}
			break;

		default:
			break;
	}

	return copy;
}


const ofc_sema_type_t* ofc_sema_format_desc_type(
	const ofc_parse_format_desc_t* desc)
{
	if (!desc) return NULL;

	switch (desc->type)
	{
		case OFC_PARSE_FORMAT_DESC_INTEGER:
		case OFC_PARSE_FORMAT_DESC_BINARY:
		case OFC_PARSE_FORMAT_DESC_OCTAL:
		case OFC_PARSE_FORMAT_DESC_HEX:
			return ofc_sema_type_integer_default();
		case OFC_PARSE_FORMAT_DESC_REAL:
		case OFC_PARSE_FORMAT_DESC_D:
		case OFC_PARSE_FORMAT_DESC_E:
		case OFC_PARSE_FORMAT_DESC_REAL_SCALE:
			return ofc_sema_type_real_default();
		case OFC_PARSE_FORMAT_DESC_LOGICAL:
			return ofc_sema_type_logical_default();
		default:
			break;

	}

	return NULL;
}
