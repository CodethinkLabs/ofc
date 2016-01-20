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

static ofc_sema_format_t* ofc_sema_format__create(
	const ofc_parse_format_desc_list_t* src)
{
	ofc_sema_format_t* format
		= (ofc_sema_format_t*)malloc(
			sizeof(ofc_sema_format_t));
	if (!format) return NULL;

	format->src    = src;
	format->format = NULL;
	return format;
}

bool ofc_sema_format(
	ofc_sema_scope_t* scope,
	const ofc_parse_stmt_t* stmt)
{
	if (!scope || !stmt
		|| (stmt->type != OFC_PARSE_STMT_FORMAT))
		return false;

	if (stmt->label == 0)
	{
		ofc_sparse_ref_warning(stmt->src,
			"FORMAT statement without a label has no effect"
			" and will be ignored");
		return true;
	}

	ofc_sema_format_t* format
		= ofc_sema_format__create(stmt->format);
	if (!format) return false;

	if (!ofc_sema_label_map_add_format(
		stmt, scope->label,
		stmt->label, format))
	{
		ofc_sema_format_delete(format);
		return false;
	}

	return true;
}

bool ofc_sema_format_print(ofc_colstr_t* cs,
	ofc_sema_format_t* format)
{
	if (!cs || !format)
		return false;

	if (!ofc_colstr_atomic_writef(cs, "FORMAT ("))
		return false;
	if (format->src && !ofc_parse_format_desc_list_print(
		cs, format->src))
			return false;
	if (!ofc_colstr_atomic_writef(cs, ")"))
		return false;

	return true;
}

void ofc_sema_format_delete(
	ofc_sema_format_t* format)
{
	if (!format)
		return;

	ofc_parse_format_desc_list_delete(
		format->format);
	free(format);
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
