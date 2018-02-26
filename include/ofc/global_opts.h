/* Copyright 2016-2018 Codethink Ltd.
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
	bool case_sensitive;

	bool no_warn;
	bool no_warn_equiv_type;
	bool no_warn_name_keyword;
	bool warn_name_keyword_all;
	bool no_warn_namespace_col;
	bool no_warn_type_io;
	bool no_warn_no_logical_if;
	bool no_warn_star_in_lhs;
	bool warn_unused_procedure;
	bool parse_only;
	bool parse_print;
	bool sema_print;
	bool no_escape;
	bool common_usage_print;
} ofc_global_opts_t;

static const ofc_global_opts_t
	OFC_GLOBAL_OPTS_DEFAULT =
{
	.case_sensitive        = false,

	.no_warn               = false,
	.no_warn_equiv_type    = false,
	.no_warn_name_keyword  = false,
	.warn_name_keyword_all = false,
	.no_warn_namespace_col = false,
	.no_warn_type_io       = false,
	.no_warn_no_logical_if = false,
	.no_warn_star_in_lhs   = false,
	.warn_unused_procedure = false,
	.parse_only            = false,
	.parse_print           = false,
	.sema_print            = false,
	.common_usage_print    = false,
	.no_escape             = false,
};

extern ofc_global_opts_t global_opts;

#endif
