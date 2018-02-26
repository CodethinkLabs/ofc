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

#ifndef __cliarg_h__
#define __cliarg_h__

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "ofc/lang_opts.h"
#include "ofc/global_opts.h"
#include "ofc/print_opts.h"
#include "ofc/sema_pass_opts.h"
#include "ofc/file.h"

typedef enum
{
	OFC_CLIARG_NO_WARN = 0,
	OFC_CLIARG_NO_WARN_EQUIV_TYPE,
	OFC_CLIARG_NO_WARN_NAME_KEYWORD,
	OFC_CLIARG_WARN_NAME_KEYWORD_ALL,
	OFC_CLIARG_NO_WARN_NAMESPACE_COL,
	OFC_CLIARG_NO_WARN_TYPE_IO,
	OFC_CLIARG_NO_WARN_NO_LOGICAL_IF,
	OFC_CLIARG_NO_WARN_STAR_IN_LHS,
	OFC_CLIARG_NO_WARN_PEDANTIC,
	OFC_CLIARG_WARN_UNUSED_PROCEDURE,
	OFC_CLIARG_PARSE_ONLY,
	OFC_CLIARG_PARSE_TREE,
	OFC_CLIARG_SEMA_TREE,
	OFC_CLIARG_FIXED_FORM,
	OFC_CLIARG_FREE_FORM,
	OFC_CLIARG_TAB_WIDTH,
	OFC_CLIARG_INDENT_WIDTH,
	OFC_CLIARG_INDENT_MAX_LEVEL,
	OFC_CLIARG_PRINT_F77_PARAMETER,
	OFC_CLIARG_PRINT_AUTOMATIC,
	OFC_CLIARG_INIT_LOCAL_ZERO,
	OFC_CLIARG_LOWERCASE_KEYWORD,
	OFC_CLIARG_DEBUG,
	OFC_CLIARG_COLUMNS,
	OFC_CLIARG_CASE_SEN,
	OFC_CLIARG_INCLUDE,
	OFC_CLIARG_SEMA_STRUCT_TYPE,
	OFC_CLIARG_SEMA_CHAR_TRANSFER,
	OFC_CLIARG_SEMA_UNREF_LABEL,
	OFC_CLIARG_SEMA_UNLAB_FORMAT,
	OFC_CLIARG_SEMA_UNLAB_CONT,
	OFC_CLIARG_SEMA_INTEGER_LOGICAL,
	OFC_CLIARG_SEMA_UNUSED_DECL,
	OFC_CLIARG_NO_ESCAPE,
	OFC_CLIARG_COMMON_USAGE,

	OFC_CLIARG_INVALID
} ofc_cliarg_e;

typedef enum
{
	OFC_CLIARG_PARAM_GLOB_NONE = 0,
	OFC_CLIARG_PARAM_PRIN_NONE,
	OFC_CLIARG_PARAM_PRIN_INT,
	OFC_CLIARG_PARAM_LANG_NONE,
	OFC_CLIARG_PARAM_LANG_INT,
	OFC_CLIARG_PARAM_FILE_STR,
	OFC_CLIARG_PARAM_SEMA_PASS,
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

	union
	{
		int value;
		char* str;
	};
} ofc_cliarg_t;

typedef struct
{
	unsigned       count;
	ofc_cliarg_t** arg;
} ofc_cliarg_list_t;

bool ofc_cliarg_parse(
	int argc,
    const char* argv[],
	ofc_file_list_t** file,
	ofc_print_opts_t* print_opts,
	ofc_global_opts_t* global_opts,
	ofc_sema_pass_opts_t* sema_pass_opts);

void ofc_cliarg_print_usage(const char* name);

ofc_cliarg_t* ofc_cliarg_create(
	const ofc_cliarg_body_t* arg_prop,
	const void* param);
void ofc_cliarg_delete(ofc_cliarg_t* arg);

ofc_cliarg_list_t* ofc_cliarg_list_create(void);
bool ofc_cliarg_list_add(
	ofc_cliarg_list_t* list,
	ofc_cliarg_t* arg);
void ofc_cliarg_list_delete(
	ofc_cliarg_list_t* list);

#endif
