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

#ifndef __ofc_file_h__
#define __ofc_file_h__

#include <stdbool.h>
#include "lang_opts.h"

typedef struct
{
	char*           path;
	char*           include;
	char*           strz;
	ofc_lang_opts_t opts;
	unsigned        size;
	unsigned        ref;
} ofc_file_t;

/* Path must be valid for as long as the ofc_file_t* is */
ofc_file_t* ofc_file_create(const char* path, ofc_lang_opts_t opts);
ofc_file_t* ofc_file_create_include(const char* path, ofc_lang_opts_t opts, const char* include);
bool        ofc_file_reference(ofc_file_t* file);
void        ofc_file_delete(ofc_file_t* file);

const char*     ofc_file_get_path(const ofc_file_t* file);
const char*     ofc_file_get_include(const ofc_file_t* file);
const char*     ofc_file_get_strz(const ofc_file_t* file);
ofc_lang_opts_t ofc_file_get_lang_opts(const ofc_file_t* file);

char* ofc_file_include_path(
	const ofc_file_t* file, const char* path);

bool ofc_file_get_position(
	const ofc_file_t* file, const char* ptr,
	unsigned* row, unsigned* col);


#include <stdarg.h>

void ofc_file_error(
	const ofc_file_t* file, const char* ptr,
	const char* format, ...)
	__attribute__ ((format (printf, 3, 4)));
void ofc_file_warning(
	const ofc_file_t* file, const char* ptr,
	const char* format, ...)
	__attribute__ ((format (printf, 3, 4)));

void ofc_file_error_va(
	const ofc_file_t* file,
	const char* sol, const char* ptr,
	const char* format, va_list args);
void ofc_file_warning_va(
	const ofc_file_t* file,
	const char* sol, const char* ptr,
	const char* format, va_list args);

#endif
