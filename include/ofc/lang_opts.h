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

#ifndef __ofc_lang_opts_h__
#define __ofc_lang_opts_h__

#include <stdbool.h>

typedef enum
{
	OFC_LANG_FORM_FIXED = 0,
	OFC_LANG_FORM_FREE,
} ofc_lang_form_e;

typedef struct
{
	ofc_lang_form_e form;
	unsigned        tab_width;
	bool            debug;
	unsigned        columns;
} ofc_lang_opts_t;

static const ofc_lang_opts_t OFC_LANG_OPTS_F77 =
{
	.form           = OFC_LANG_FORM_FIXED,
	.tab_width      = 8,
	.debug          = false,
	.columns        = 72,
};

static const ofc_lang_opts_t OFC_LANG_OPTS_F90 =
{
	.form           = OFC_LANG_FORM_FREE,
	.tab_width      = 8,
	.debug          = false,
	.columns        = 132,
};

#endif
