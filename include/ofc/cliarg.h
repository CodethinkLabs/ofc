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

#ifndef __cliarg_h__
#define __cliarg_h__

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "ofc/lang_opts.h"
#include "ofc/global_opts.h"
#include "ofc/file.h"

typedef enum
{
	NO_WARN = 0,
	NO_WARN_EQUIV_TYPE,
	PARSE_ONLY,
	PARSE_TREE,
	SEMA_TREE,
	FIXED_FORM,
	FREE_FORM,
	TAB_FORM,
	TAB_WIDTH,
	DEBUG,
	COLUMNS,
	CASE_SEN,

	INVALID
} ofc_cliarg_e;

typedef enum
{
	GLOB_NONE = 0,
	LANG_NONE,
	LANG_INT

} ofc_cliarg_param_e;

typedef struct
{
	ofc_cliarg_e       type;
	const char*        name;
	const char         flag;
	const char*        desc;
	ofc_cliarg_param_e param_type;
	unsigned           param_num;
	bool               exclusive;
} ofc_cliarg_body_t;

typedef struct
{
	const ofc_cliarg_body_t* body;
	int  value;
	/* This will become a union when more argument
	   value types are needed. */
} ofc_cliarg_t;

typedef struct
{
	unsigned       count;
	ofc_cliarg_t** arg;
} ofc_cliarg_list_t;

bool ofc_cliarg_parse(
	int argc,
    const char* argv[],
	ofc_file_t** file,
	ofc_lang_opts_t* lang_opts,
	ofc_global_opts_t* global_opts);

void print_usage(const char* name);

ofc_cliarg_t* ofc_cliarg_create(
	const ofc_cliarg_body_t* arg_prop,
	int value);
void ofc_cliarg_delete(ofc_cliarg_t* arg);

ofc_cliarg_list_t* ofc_cliarg_list_create(void);
bool ofc_cliarg_list_add(
	ofc_cliarg_list_t* list,
	ofc_cliarg_t* arg);
void ofc_cliarg_list_delete(
	ofc_cliarg_list_t* list);

#endif
