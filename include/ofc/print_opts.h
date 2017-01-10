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

#ifndef __ofc_print_opts_h__
#define __ofc_print_opts_h__

#include <stdbool.h>

typedef struct
{
	unsigned indent_width;
	unsigned indent_max_level;

	bool     f77_parameter;
	bool     automatic;
	bool     init_zero;
	bool     lowercase_keyword;
} ofc_print_opts_t;

static const ofc_print_opts_t
	OFC_PRINT_OPTS_DEFAULT =
{
	.indent_width      = 2,
	.indent_max_level  = 4,
	.f77_parameter     = false,
	.automatic         = false,
	.init_zero         = false,
	.lowercase_keyword = false,
};

#endif
