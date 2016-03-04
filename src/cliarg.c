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

#include <ofc/cliarg.h>


static bool set_global_opts__flag(
	ofc_global_opts_t* global,
	int arg_type)
{
	if (!global)
		return false;

	switch (arg_type)
	{
		case CASE_SEN:
			global->case_sensitive = true;
			break;
		case NO_WARN:
			global->no_warn = true;
			break;
		case NO_WARN_EQUIV_TYPE:
			global->no_warn_equiv_type = true;
			break;
		case NO_WARN_NAME_KEYWORD:
			global->no_warn_name_keyword = true;
			break;
		case NO_WARN_NAMESPACE_COL:
			global->no_warn_namespace_col = true;
			break;
		case NO_WARN_PEDANTIC:
			global->no_warn_equiv_type    = true;
			global->no_warn_name_keyword  = true;
			global->no_warn_namespace_col = true;
			break;
		case PARSE_ONLY:
			global->parse_only = true;
			break;
		case PARSE_TREE:
			global->parse_print = true;
			break;
		case SEMA_TREE:
			global->sema_print = true;
			break;

		default:
			return false;
	}

	return true;
}

static bool set_print_opts__num(
	ofc_print_opts_t* print_opts,
	int arg_type, unsigned value)
{
	if (!print_opts)
		return false;

    switch (arg_type)
	{
		case INDENT_WIDTH:
			print_opts->indent_width = value;
			break;
		case INDENT_MAX_LEVEL:
			print_opts->indent_max_level = value;
			break;

		default:
			return false;
	}

	return true;
}

static bool set_lang_opts__flag(
	ofc_lang_opts_t* lang_opts,
	int arg_type)
{
	if (!lang_opts)
		return false;

	switch (arg_type)
	{
		case DEBUG:
			lang_opts->debug = true;
			break;

		case FIXED_FORM:
			lang_opts->form = OFC_LANG_FORM_FIXED;
			break;
		case FREE_FORM:
			lang_opts->form = OFC_LANG_FORM_FREE;
			break;

		default:
			return false;
	}

	return true;
}

static bool set_lang_opts__num(
	ofc_lang_opts_t* lang_opts,
	int arg_type, unsigned value)
{
	if (!lang_opts)
		return false;

	switch (arg_type)
	{
		case TAB_WIDTH:
			lang_opts->tab_width = value;
			break;
		case COLUMNS:
			lang_opts->columns = value;
			break;

		default:
			return false;
	}

	return true;
}

static bool set_file__str(
	ofc_file_t* file,
	int arg_type, char* str)
{
	if (!file)
		return false;

	switch (arg_type)
	{
		case INCLUDE:
			if (!ofc_file_include_list_add_create(file, str))
				return false;
			break;

		default:
			return false;
	}

	return true;
}


static const ofc_cliarg_body_t cliargs[] =
{
	/*ENUM                  NAME                    FLAG  DESCRIPTION                            PARAM_TYPE PARAMS EXCLUSIVE */
	{ NO_WARN,               "no-warn",               '\0', "Suppress OFC warnings",                      GLOB_NONE, 0, true  },
	{ NO_WARN_EQUIV_TYPE,    "no-warn-equiv-type",    '\0', "Suppress EQUIVALENCE type mismatch warning", GLOB_NONE, 0, true  },
	{ NO_WARN_NAME_KEYWORD,  "no-warn-name-keyword",  '\0', "Suppress language keyword in name warning",  GLOB_NONE, 0, true  },
	{ NO_WARN_NAMESPACE_COL, "no-warn-namespace-col", '\0', "Suppress namespace collision warning",       GLOB_NONE, 0, true  },
	{ NO_WARN_PEDANTIC,      "no-warn-pedantic",      '\0', "Suppress all pedantic warnings",             GLOB_NONE, 0, true  },
	{ PARSE_ONLY,            "parse-only",            '\0', "Runs the parser only",                       GLOB_NONE, 0, true  },
	{ PARSE_TREE,            "parse-tree",            '\0', "Prints the parse tree",                      GLOB_NONE, 0, true  },
	{ SEMA_TREE,             "sema-tree",             '\0', "Prints the semantic analysis tree",          GLOB_NONE, 0, true  },
	{ FIXED_FORM,            "free-form",             '\0', "Sets free form type",                        LANG_NONE, 0, true  },
	{ FREE_FORM,             "fixed-form",            '\0', "Sets fixed form type",                       LANG_NONE, 0, true  },
	{ TAB_WIDTH,             "tab-width",             '\0', "Sets tab width <n>",                         LANG_INT,  1, true  },
	{ DEBUG,                 "debug",                 '\0', "Sets debug mode",                            LANG_NONE, 0, true  },
	{ COLUMNS,               "columns",               '\0', "Sets number of columns to <n>",              LANG_INT,  1, true  },
	{ CASE_SEN,              "case-sen",              '\0', "Sets case sensitive mode",                   GLOB_NONE, 0, true  },
	{ INDENT_WIDTH,          "indent-width",          '\0', "Sets indent width <n>",                      PRIN_INT,  1, true  },
	{ INDENT_MAX_LEVEL,      "indent-max-level",      '\0', "Sets maximum indent level <n>",              PRIN_INT,  1, true  },
	{ INCLUDE,               "include",               '\0', "Set include paths <s>",                      FILE_STR,  1, false },
};

static const char *get_file_ext(const char *path)
{
	if (!path)
		return NULL;

	const char *dot = NULL;
	unsigned i;
	for (i = 0; path[i] != '\0'; i++)
	{
		if (path[i] == '/')
			dot = NULL;
		else if (path[i] == '.')
			dot = &path[i];
	}
  return (dot ? &dot[1] : NULL);
}

static const ofc_cliarg_body_t* resolve_arg_name(const char* arg_string)
{
	unsigned i;
	for (i = 0; i < INVALID; i++)
	{
		if (strcasecmp(cliargs[i].name, arg_string) == 0)
			return &cliargs[i];
	}

	return NULL;
}

static const ofc_cliarg_body_t* resolve_arg_flag(const char arg_flag)
{
	unsigned i;
	for (i = 0; i < INVALID; i++)
	{
		if (cliargs[i].flag == arg_flag)
			return &cliargs[i];
	}

	return NULL;
}

static bool resolve_param_pos_int(const char* arg_string, int* param_int)
{
	unsigned i;
	for (i = 0; i < strlen(arg_string); i++)
	{
		if (!isdigit(arg_string[i]))
			return false;
	}

	int arg_int = atoi(arg_string);

	if (arg_int > 0)
	{
		*param_int = arg_int;
		return true;
	}

	return false;
}

static bool resolve_param_str(const char* arg_string)
{
	if (!arg_string || (arg_string[0] == '-'))
		return false;

	return true;
}

static bool ofc_cliarg__apply(
	ofc_global_opts_t* global_opts,
	ofc_print_opts_t* print_opts,
	ofc_lang_opts_t* lang_opts,
	ofc_file_t* file,
	const ofc_cliarg_t* arg)
{
	ofc_cliarg_e       arg_type   = arg->body->type;
	ofc_cliarg_param_e param_type = arg->body->param_type;

	switch (param_type)
	{
		case GLOB_NONE:
			return set_global_opts__flag(global_opts, arg_type);

		case LANG_NONE:
			return set_lang_opts__flag(lang_opts, arg_type);

		case LANG_INT:
			return set_lang_opts__num(lang_opts, arg_type, arg->value);

		case PRIN_INT:
			return set_print_opts__num(print_opts, arg_type, arg->value);

		case FILE_STR:
			return set_file__str(file, arg_type, arg->str);

		default:
			break;;
	}

	return false;
}

static bool ofc_cliarg_list__apply(
	ofc_global_opts_t* global_opts,
	ofc_print_opts_t* print_opts,
	ofc_lang_opts_t* lang_opts,
	ofc_file_t* file,
	ofc_cliarg_list_t* list)
{
	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_cliarg__apply(global_opts, print_opts,
			lang_opts, file, list->arg[i]))
			return false;
	}

	return true;
}

bool ofc_cliarg_parse(
	int argc,
    const char* argv[],
	ofc_file_t** file,
	ofc_print_opts_t* print_opts,
	ofc_global_opts_t* global_opts)
{
	const char* program_name = argv[0];

	if (argc < 2)
	{
		fprintf(stderr, "Error: Expected source path\n");
		print_usage(program_name);
		return false;
	}

	ofc_cliarg_list_t* args_list = ofc_cliarg_list_create();

	unsigned i = 1;
	while (i < (unsigned)argc - 1)
	{
		if (argv[i][0] == '-')
		{
			if (argv[i][1] == '-')
			{
				ofc_cliarg_t* resolved_arg = NULL;
				const char* arg_str = argv[i] + 2;
				const ofc_cliarg_body_t* arg_body = resolve_arg_name(arg_str);
				if (!arg_body)
				{
					fprintf(stderr, "Error: Unable to resolve argument: %s\n", argv[i]);
					print_usage(program_name);
					return false;
				}
				i++;

				switch (arg_body->param_type)
				{
					case GLOB_NONE:
					case LANG_NONE:
						resolved_arg = ofc_cliarg_create(arg_body, NULL);
						break;

					case LANG_INT:
					case PRIN_INT:
					{
						int param = -1;
						if (!resolve_param_pos_int(argv[i++], &param))
						{
							fprintf(stderr, "Error: Expected parameter for argument: %s\n", argv[i]);
							print_usage(program_name);
							return false;
						}
						resolved_arg = ofc_cliarg_create(arg_body, &param);
						break;
					}

					case FILE_STR:
					{
						if (resolve_param_str(argv[i]))
						{
							resolved_arg = ofc_cliarg_create(arg_body, argv[i]);
							i++;
						}
						else
						{
							fprintf(stderr, "Error: Expected parameter for argument: %s\n", argv[i]);
							print_usage(program_name);
							return false;
						}
						break;
					}

					default:
						break;
				}

				if (!resolved_arg
					|| !ofc_cliarg_list_add(args_list, resolved_arg))
					return false;
			}
			else
			{
				const char* arg_str = argv[i] + 1;
				unsigned flag;
				for (flag = 0; flag < strlen(arg_str); flag++)
				{
					const ofc_cliarg_body_t* arg_body = resolve_arg_flag(arg_str[flag]);
					if (!arg_body)
					{
						fprintf(stderr, "Error: Cannot resolve flag: %s\n", argv[i]);
						print_usage(program_name);
					}
					if (arg_body->param_num > 0)
					{
						fprintf(stderr, "Error: Cannot group flags that require a parameter: %s\n", argv[i]);
						print_usage(program_name);
						return false;
					}
					/* TODO - Add support for single character flags. */
				}
				i++;
			}
		}
		else
		{
			fprintf(stderr, "Error: Expected flags followed by file path: %s\n", argv[i]);
			print_usage(program_name);
			return false;
		}
	}

	const char* path = argv[argc - 1];
	const char* source_file_ext = get_file_ext(path);

	ofc_lang_opts_t lang_opts = OFC_LANG_OPTS_F77;

	if (source_file_ext
		&& (strcasecmp(source_file_ext, "F90") == 0))
		lang_opts = OFC_LANG_OPTS_F90;

	*file = ofc_file_create(path, lang_opts);
	if (!*file)
	{
		fprintf(stderr, "\nError: Failed read source file '%s'\n", path);
		return false;
	}

	ofc_lang_opts_t* lang_opts_ptr
		= ofc_file_modify_lang_opts(*file);
	if (!lang_opts_ptr) return false;

	if (!ofc_cliarg_list__apply(
		global_opts, print_opts, lang_opts_ptr,
		*file, args_list))
		return false;

	ofc_cliarg_list_delete(args_list);

	return true;
}

static unsigned ofc_cliarg_longest_name_len()
{
	unsigned longest_name_len = 0;

	unsigned i;
	for (i = 0; i < INVALID; i++)
	{
		if (strlen(cliargs[i].name) > longest_name_len)
			longest_name_len = strlen(cliargs[i].name);
	}

	return longest_name_len;
}

void print_usage(const char* name)
{
	printf("%s [OPTIONS] FILE\n", name);
	printf("Options:\n");

	unsigned name_len = ofc_cliarg_longest_name_len() + 5;

	unsigned i;
	for (i = 0; i < INVALID; i++)
	{
		unsigned line_len = 0;

		switch (cliargs[i].param_type)
		{
			case LANG_INT:
				line_len = printf("  --%s <n>", cliargs[i].name);
				break;

			case FILE_STR:
				line_len = printf("  --%s <s>", cliargs[i].name);
				break;

			default:
				line_len = printf("  --%s", cliargs[i].name);
				break;
		}

		for (; line_len < name_len; line_len++) printf(" ");

		if (cliargs[i].flag != '\0')
			printf("-%c  ", cliargs[i].flag);
		else
			printf("    ");

		printf("%s\n", cliargs[i].desc);
	}
}


ofc_cliarg_t* ofc_cliarg_create(
	const ofc_cliarg_body_t* arg_body,
	const void* param)
{
	ofc_cliarg_t* arg
		= (ofc_cliarg_t*)malloc(
			sizeof(ofc_cliarg_t));
	if (!arg) return NULL;

	arg->body = arg_body;

	if (param)
	{
		switch (arg_body->param_type)
		{
			case LANG_INT:
			case PRIN_INT:
				arg->value = *((int*)param);
				break;

			case FILE_STR:
				arg->str = strdup((char*)param);
				break;

			default:
				free(arg);
				return NULL;
		}
	}

	return arg;
}

void ofc_cliarg_delete(ofc_cliarg_t* arg)
{
	if (!arg)
		return;

	if (arg->body->param_type == FILE_STR)
		free (arg->str);

	free(arg);
}

ofc_cliarg_list_t* ofc_cliarg_list_create(void)
{
	ofc_cliarg_list_t* list
		= (ofc_cliarg_list_t*)malloc(
			sizeof(ofc_cliarg_list_t));
	if (!list) return NULL;

	list->count = 0;
	list->arg   = NULL;
	return list;
}

static bool ofc_cliarg_contains(
	const ofc_cliarg_list_t* list,
	const ofc_cliarg_t* arg)
{
	if (!list || !arg)
		return false;

	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (list->arg[i]->body == arg->body)
			return true;
	}

	return false;
}

bool ofc_cliarg_list_add(
	ofc_cliarg_list_t* list,
	ofc_cliarg_t* arg)
{
	if (!list || !arg)
		return false;

	if (arg->body->exclusive
		&& ofc_cliarg_contains(list, arg))
	{
		fprintf(stderr, "Error: Argument is exclusive: %s\n",
			arg->body->name);
		return EXIT_FAILURE;
	}

	ofc_cliarg_t** narg
		= (ofc_cliarg_t**)realloc(list->arg,
			(sizeof(ofc_cliarg_t*) * (list->count + 1)));
	if (!narg) return NULL;

	list->arg = narg;
	list->arg[list->count++] = arg;

	return true;
}

void ofc_cliarg_list_delete(
	ofc_cliarg_list_t* list)
{
	if (!list)
		return;

	unsigned i;
	for (i = 0; i < list->count; i++)
		ofc_cliarg_delete(list->arg[i]);
	free(list->arg);

	free(list);
}
