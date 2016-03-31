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
	char**   path;
	unsigned count;
} ofc_file_include_list_t;

typedef struct ofc_file_s ofc_file_t;

typedef struct
{
	unsigned     count;
	ofc_file_t** file;
} ofc_file_list_t;

#include "sparse.h"

/* Path must be valid for as long as the ofc_file_t* is */
ofc_file_t* ofc_file_create(const char* path, ofc_lang_opts_t opts);
ofc_file_t* ofc_file_create_include(
	const char* path, ofc_lang_opts_t opts,
	const ofc_file_t* parent_file, ofc_sparse_ref_t include_stmt);
bool ofc_file_reference(ofc_file_t* file);
void ofc_file_delete(ofc_file_t* file);

const char* ofc_file_get_path(const ofc_file_t* file);
const char* ofc_file_get_include(const ofc_file_t* file);
const char* ofc_file_get_strz(const ofc_file_t* file);

const ofc_lang_opts_t* ofc_file_get_lang_opts(const ofc_file_t* file);
ofc_lang_opts_t* ofc_file_modify_lang_opts(ofc_file_t* file);

char* ofc_file_include_path(
	const ofc_file_t* file, const char* path);

bool ofc_file_get_position(
	const ofc_file_t* file, const char* ptr,
	unsigned* row, unsigned* col);

ofc_file_list_t* ofc_file_list_create(void);
bool ofc_file_list_add(
	ofc_file_list_t* list, ofc_file_t* file);
void ofc_file_list_delete(ofc_file_list_t* list);

bool ofc_file_include_list_add_create(
	ofc_file_t* file, char* path);
ofc_file_include_list_t* ofc_file_include_list_dup(
	const ofc_file_include_list_t* source);
void ofc_file_include_list_delete(
	ofc_file_include_list_t* list);

#include <stdarg.h>

bool ofc_file_no_errors(void);

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
