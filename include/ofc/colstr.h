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

#ifndef __ofc_colstr_h__
#define __ofc_colstr_h__

#include <stdbool.h>
#include "ofc/print_opts.h"

typedef struct ofc_colstr_s ofc_colstr_t;

ofc_colstr_t* ofc_colstr_create(
	const ofc_print_opts_t print_opts,
	unsigned cols, unsigned ext);
void ofc_colstr_delete(ofc_colstr_t* cstr);

bool ofc_colstr_newline(
	ofc_colstr_t* cstr, unsigned indent,
	const unsigned* label);

#include <stdarg.h>

bool ofc_colstr_write_quoted(
	ofc_colstr_t* cstr,
	const char* prefix, char quote,
	const char* base, unsigned size);

bool ofc_colstr_write_escaped(
	ofc_colstr_t* cstr, char quote,
	const char* base, unsigned size);

bool ofc_colstr_write(
	ofc_colstr_t* cstr, const char* base, unsigned size);

bool ofc_colstr_writef(
	ofc_colstr_t* cstr,
	const char* format, ...)
	__attribute__ ((format (printf, 2, 3)));

bool ofc_colstr_atomic_write(
	ofc_colstr_t* cstr, const char* base, unsigned size);

bool ofc_colstr_atomic_writef(
	ofc_colstr_t* cstr,
	const char* format, ...)
	__attribute__ ((format (printf, 2, 3)));

bool ofc_colstr_keyword_atomic_writez(
	ofc_colstr_t* cstr, const char* keyword);

bool ofc_colstr_keyword_atomic_writef(
	ofc_colstr_t* cstr,
	const char* format, ...)
	__attribute__ ((format (printf, 2, 3)));


bool ofc_colstr_fdprint(ofc_colstr_t* cstr, int fd);


const ofc_print_opts_t* ofc_colstr_print_opts_get(const ofc_colstr_t* cstr);

#endif
