#ifndef __cliarg_h__
#define __cliarg_h__

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "ofc/lang_opts.h"
#include "ofc/file.h"

typedef enum
{
	PARSE_TREE = 0,
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

typedef struct
{
	ofc_cliarg_e type;
	const char*  name;
	const char   flag;
	const char*  desc;
	unsigned     param_num;
	bool         exclusive;
} ofc_cliarg_body_t;

typedef struct
{
	const ofc_cliarg_body_t* body;
	int  value;
} ofc_cliarg_t;

typedef struct
{
	unsigned       count;
	ofc_cliarg_t** arg;
} ofc_cliarg_list_t;

typedef struct
{
	bool parse_print;
	bool sema_print;

} ofc_global_opts_t;

static const ofc_global_opts_t
	OFC_GLOBAL_OPTS_DEFAULT =
{
	.parse_print = false,
	.sema_print  = false,
};

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
