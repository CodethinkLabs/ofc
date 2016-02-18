/* Copyright 2016 Codethink Ltd.
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

#ifndef __ofc_global_opts_h__
#define __ofc_global_opts_h__

#include <stdbool.h>

typedef struct
{
	bool no_warn;
	bool no_warn_equiv_type;
	bool no_warn_name_keyword;
	bool no_warn_namespace_col;
	bool parse_only;
	bool parse_print;
	bool sema_print;

} ofc_global_opts_t;

static const ofc_global_opts_t
	OFC_GLOBAL_OPTS_DEFAULT =
{
	.no_warn               = false,
	.no_warn_equiv_type    = false,
	.no_warn_name_keyword  = false,
	.no_warn_namespace_col = false,
	.parse_only            = false,
	.parse_print           = false,
	.sema_print            = false,
};

#endif
