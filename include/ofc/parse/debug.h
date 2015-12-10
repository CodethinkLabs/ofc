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

#ifndef __ofc_parse_debug_h__
#define __ofc_parse_debug_h__

#include <ofc/sparse.h>

typedef struct ofc_parse_debug_s ofc_parse_debug_t;

ofc_parse_debug_t* ofc_parse_debug_create(void);
void ofc_parse_debug_delete(ofc_parse_debug_t* stack);

unsigned ofc_parse_debug_position(const ofc_parse_debug_t* stack);
void ofc_parse_debug_rewind(ofc_parse_debug_t* stack, unsigned position);

void ofc_parse_debug_print(const ofc_parse_debug_t* stack);

#include <stdarg.h>

void ofc_parse_debug_warning(
	ofc_parse_debug_t* stack,
	ofc_sparse_ref_t ref,
	const char* format, ...)
	__attribute__ ((format (printf, 3, 4)));

#endif
