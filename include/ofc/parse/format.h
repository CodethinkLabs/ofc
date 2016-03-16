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

#ifndef __ofc_parse_format_h__
#define __ofc_parse_format_h__

typedef enum
{
	OFC_PARSE_FORMAT_DESC_INTEGER = 0,
	OFC_PARSE_FORMAT_DESC_REAL,
	OFC_PARSE_FORMAT_DESC_D,
	OFC_PARSE_FORMAT_DESC_E,
	OFC_PARSE_FORMAT_DESC_G,
	OFC_PARSE_FORMAT_DESC_CHARACTER,
	OFC_PARSE_FORMAT_DESC_LOGICAL,
	OFC_PARSE_FORMAT_DESC_HOLLERITH,
	OFC_PARSE_FORMAT_DESC_S,
	OFC_PARSE_FORMAT_DESC_REAL_SCALE,
	OFC_PARSE_FORMAT_DESC_X,
	OFC_PARSE_FORMAT_DESC_T,
	OFC_PARSE_FORMAT_DESC_SLASH,
	OFC_PARSE_FORMAT_DESC_DOLLAR,
	OFC_PARSE_FORMAT_DESC_BACKSLASH,
	OFC_PARSE_FORMAT_DESC_Q,
	OFC_PARSE_FORMAT_DESC_COLON,
	OFC_PARSE_FORMAT_DESC_BN,
	OFC_PARSE_FORMAT_DESC_BZ,
	OFC_PARSE_FORMAT_DESC_SP,
	OFC_PARSE_FORMAT_DESC_SS,
	OFC_PARSE_FORMAT_DESC_TL,
	OFC_PARSE_FORMAT_DESC_TR,
	OFC_PARSE_FORMAT_DESC_STRING,
	OFC_PARSE_FORMAT_DESC_REPEAT,
	OFC_PARSE_FORMAT_DESC_BINARY,
	OFC_PARSE_FORMAT_DESC_OCTAL,
	OFC_PARSE_FORMAT_DESC_HEX,

	OFC_PARSE_FORMAT_DESC_COUNT
} ofc_parse_format_desc_e;

typedef struct ofc_parse_format_desc_s ofc_parse_format_desc_t;
typedef struct ofc_parse_format_desc_list_s ofc_parse_format_desc_list_t;

struct ofc_parse_format_desc_s
{
	ofc_parse_format_desc_e type;

	ofc_sparse_ref_t src;

	bool neg;
	bool n_set;
	unsigned n;
	union
	{
		struct
		{
			bool w_set, d_set, e_set;
			unsigned w, d, e;
		};

		ofc_string_t* string;

		ofc_parse_format_desc_list_t* repeat;
	};
};

struct ofc_parse_format_desc_list_s
{
	unsigned                  count;
	ofc_parse_format_desc_t** desc;
};

bool ofc_parse_format_desc_has_w(
	const ofc_parse_format_desc_t* desc);
bool ofc_parse_format_desc_has_d(
	const ofc_parse_format_desc_t* desc);
bool ofc_parse_format_desc_has_e(
	const ofc_parse_format_desc_t* desc);

ofc_parse_format_desc_t* ofc_parse_format_desc(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
bool ofc_parse_format_desc_compare(
	const ofc_parse_format_desc_t* a,
	const ofc_parse_format_desc_t* b);
void ofc_parse_format_desc_delete(
	ofc_parse_format_desc_t* desc);
bool ofc_parse_format_desc_print(
	ofc_colstr_t* cs, const ofc_parse_format_desc_t* desc);

bool ofc_parse_format_is_data_desc(
	const ofc_parse_format_desc_t* desc);

ofc_parse_format_desc_list_t* ofc_parse_format_desc_list(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);
void ofc_parse_format_desc_list_delete(
	ofc_parse_format_desc_list_t* list);
bool ofc_parse_format_desc_list_print(
	ofc_colstr_t* cs, const ofc_parse_format_desc_list_t* list);
ofc_parse_format_desc_list_t* ofc_parse_format_desc_list_create(void);
bool ofc_parse_format_desc_list_add(
	ofc_parse_format_desc_list_t* list,
	ofc_parse_format_desc_t* desc);

ofc_parse_format_desc_t* ofc_parse_format_desc_create_repeat(
	ofc_parse_format_desc_list_t* list, unsigned n);

ofc_parse_format_desc_t* ofc_parse_format_desc_copy(
	const ofc_parse_format_desc_t* desc);
ofc_parse_format_desc_list_t* ofc_parse_format_desc_list_copy(
	const ofc_parse_format_desc_list_t* list);

bool ofc_parse_format_desc_elem_count(
	const ofc_parse_format_desc_t* desc,
	unsigned* count);
bool ofc_parse_format_desc_list_elem_count(
	const ofc_parse_format_desc_list_t* list,
	unsigned* count);

ofc_parse_format_desc_t* ofc_parse_format_desc_elem_get(
	const ofc_parse_format_desc_t* desc, unsigned offset);
ofc_parse_format_desc_t* ofc_parse_format_desc_list_elem_get(
	const ofc_parse_format_desc_list_t* list, unsigned offset);

#endif
