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

#include <ofc/cliarg.h>


static bool ofc_cliarg_global_opts__set_flag(
	ofc_global_opts_t* global,
	int arg_type)
{
	if (!global)
		return false;

	switch (arg_type)
	{
		case OFC_CLIARG_CASE_SEN:
			global->case_sensitive = true;
			break;
		case OFC_CLIARG_NO_WARN:
			global->no_warn = true;
			break;
		case OFC_CLIARG_NO_WARN_EQUIV_TYPE:
			global->no_warn_equiv_type = true;
			break;
		case OFC_CLIARG_NO_WARN_NAME_KEYWORD:
			global->no_warn_name_keyword = true;
			break;
		case OFC_CLIARG_WARN_NAME_KEYWORD_ALL:
			global->warn_name_keyword_all = true;
			break;
		case OFC_CLIARG_NO_WARN_NAMESPACE_COL:
			global->no_warn_namespace_col = true;
			break;
		case OFC_CLIARG_NO_WARN_TYPE_IO:
			global->no_warn_type_io = true;
			break;
		case OFC_CLIARG_NO_WARN_NO_LOGICAL_IF:
			global->no_warn_no_logical_if = true;
			break;
		case OFC_CLIARG_NO_WARN_STAR_IN_LHS:
			global->no_warn_star_in_lhs = true;
			break;
		case OFC_CLIARG_NO_WARN_PEDANTIC:
			global->no_warn_equiv_type    = true;
			global->no_warn_name_keyword  = true;
			global->no_warn_namespace_col = true;
			break;
		case OFC_CLIARG_WARN_UNUSED_PROCEDURE:
			global->warn_unused_procedure = true;
			break;
		case OFC_CLIARG_PARSE_ONLY:
			global->parse_only = true;
			break;
		case OFC_CLIARG_PARSE_TREE:
			global->parse_print = true;
			break;
		case OFC_CLIARG_SEMA_TREE:
			global->sema_print = true;
			break;
		case OFC_CLIARG_NO_ESCAPE:
			global->no_escape = true;
			break;
		case OFC_CLIARG_COMMON_USAGE:
			global->common_usage_print = true;
			break;

		default:
			return false;
	}

	return true;
}

static bool ofc_cliarg_print_opts__set_flag(
	ofc_print_opts_t* print_opts,
	int arg_type)
{
	if (!print_opts)
		return false;

    switch (arg_type)
	{
		case OFC_CLIARG_PRINT_F77_PARAMETER:
			print_opts->f77_parameter = true;
			break;
		case OFC_CLIARG_PRINT_AUTOMATIC:
			print_opts->automatic = true;
			break;
		case OFC_CLIARG_INIT_LOCAL_ZERO:
			print_opts->init_zero = true;
			break;
		case OFC_CLIARG_LOWERCASE_KEYWORD:
			print_opts->lowercase_keyword = true;
			break;

		default:
			return false;
	}

	return true;
}

static bool ofc_cliarg_print_opts__set_num(
	ofc_print_opts_t* print_opts,
	int arg_type, unsigned value)
{
	if (!print_opts)
		return false;

    switch (arg_type)
	{
		case OFC_CLIARG_INDENT_WIDTH:
			print_opts->indent_width = value;
			break;
		case OFC_CLIARG_INDENT_MAX_LEVEL:
			print_opts->indent_max_level = value;
			break;

		default:
			return false;
	}

	return true;
}

static bool ofc_cliarg_lang_opts__set_flag(
	ofc_lang_opts_t* lang_opts,
	int arg_type)
{
	if (!lang_opts)
		return false;

	switch (arg_type)
	{
		case OFC_CLIARG_DEBUG:
			lang_opts->debug = true;
			break;

		case OFC_CLIARG_FIXED_FORM:
			lang_opts->form = OFC_LANG_FORM_FIXED;
			break;
		case OFC_CLIARG_FREE_FORM:
			lang_opts->form = OFC_LANG_FORM_FREE;
			break;

		default:
			return false;
	}

	return true;
}

static bool ofc_cliarg_lang_opts__set_num(
	ofc_lang_opts_t* lang_opts,
	int arg_type, unsigned value)
{
	if (!lang_opts)
		return false;

	switch (arg_type)
	{
		case OFC_CLIARG_TAB_WIDTH:
			lang_opts->tab_width = value;
			break;
		case OFC_CLIARG_COLUMNS:
			lang_opts->columns = value;
			break;

		default:
			return false;
	}

	return true;
}

static bool ofc_cliarg_sema_pass_opts__set_flag(
	ofc_sema_pass_opts_t* sema_pass_opts,
	int arg_type)
{
	if (!sema_pass_opts)
		return false;

	switch (arg_type)
	{
		case OFC_CLIARG_SEMA_STRUCT_TYPE:
			sema_pass_opts->struct_type = false;
			break;
		case OFC_CLIARG_SEMA_CHAR_TRANSFER:
			sema_pass_opts->char_transfer = false;
			break;
		case OFC_CLIARG_SEMA_UNREF_LABEL:
			sema_pass_opts->unref_label = false;
			break;
		case OFC_CLIARG_SEMA_UNLAB_FORMAT:
			sema_pass_opts->unlabelled_format = false;
			break;
		case OFC_CLIARG_SEMA_UNLAB_CONT:
			sema_pass_opts->unlabelled_continue = false;
			break;
		case OFC_CLIARG_SEMA_INTEGER_LOGICAL:
			sema_pass_opts->integer_logical = false;
			break;

		case OFC_CLIARG_SEMA_UNUSED_DECL:
			sema_pass_opts->unused_decl = true;
			break;

		default:
			return false;
	}

	return true;
}

static bool ofc_cliarg_file__set_str(
	ofc_file_t* file,
	int arg_type, char* str)
{
	if (!file)
		return false;

	switch (arg_type)
	{
		case OFC_CLIARG_INCLUDE:
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
	/*ENUM                              NAME                     FLAG  DESCRIPTION                                   PARAM_TYPE          PARAMS EXCLUSIVE */
	{ OFC_CLIARG_NO_WARN,               "no-warn",               'n',  "Suppress OFC warnings",                      OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_NO_WARN_EQUIV_TYPE,    "no-warn-equiv-type",    '\0', "Suppress EQUIVALENCE type mismatch warning", OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_NO_WARN_NAME_KEYWORD,  "no-warn-name-keyword",  '\0', "Suppress language keyword in name warning",  OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_WARN_NAME_KEYWORD_ALL, "warn-name-keyword-all", '\0', "Warn about all keyword name collisions",     OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_NO_WARN_NAMESPACE_COL, "no-warn-namespace-col", '\0', "Suppress namespace collision warning",       OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_NO_WARN_TYPE_IO,       "no-warn-type-io",       '\0', "Suppress TYPE as IO statement warning",      OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_NO_WARN_NO_LOGICAL_IF, "no-warn-no-logical-if", '\0', "Suppress No logical if condition warning",   OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_NO_WARN_STAR_IN_LHS,   "no-warn-star-in-lhs",   '\0', "Suppress star in lhs warning",               OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_NO_WARN_PEDANTIC,      "no-warn-pedantic",      'p',  "Suppress all pedantic warnings",             OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_WARN_UNUSED_PROCEDURE, "warn-unused-procedure", '\0', "Enable unused procedure warnings",           OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_PARSE_ONLY,            "parse-only",            '\0', "Runs the parser only",                       OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_PARSE_TREE,            "parse-tree",            '\0', "Prints the parse tree",                      OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_SEMA_TREE,             "sema-tree",             's',  "Prints the semantic analysis tree",          OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_FIXED_FORM,            "free-form",             '\0', "Sets free form type",                        OFC_CLIARG_PARAM_LANG_NONE, 0, true  },
	{ OFC_CLIARG_FREE_FORM,             "fixed-form",            '\0', "Sets fixed form type",                       OFC_CLIARG_PARAM_LANG_NONE, 0, true  },
	{ OFC_CLIARG_TAB_WIDTH,             "tab-width",             '\0', "Sets tab width <n>",                         OFC_CLIARG_PARAM_LANG_INT,  1, true  },
	{ OFC_CLIARG_DEBUG,                 "debug",                 '\0', "Sets debug mode",                            OFC_CLIARG_PARAM_LANG_NONE, 0, true  },
	{ OFC_CLIARG_COLUMNS,               "columns",               '\0', "Sets number of columns to <n>",              OFC_CLIARG_PARAM_LANG_INT,  1, true  },
	{ OFC_CLIARG_CASE_SEN,              "case-sen",              '\0', "Sets case sensitive mode",                   OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_INDENT_WIDTH,          "indent-width",          '\0', "Sets indent width <n>",                      OFC_CLIARG_PARAM_PRIN_INT,  1, true  },
	{ OFC_CLIARG_INDENT_MAX_LEVEL,      "indent-max-level",      '\0', "Sets maximum indent level <n>",              OFC_CLIARG_PARAM_PRIN_INT,  1, true  },
	{ OFC_CLIARG_PRINT_F77_PARAMETER,   "print-f77-parameter",   '\0', "Print PARAMETER statement separately",       OFC_CLIARG_PARAM_PRIN_NONE, 0, true  },
	{ OFC_CLIARG_PRINT_AUTOMATIC,       "print-automatic",       '\0', "Print AUTOMATIC attribute in output",        OFC_CLIARG_PARAM_PRIN_NONE, 0, true  },
	{ OFC_CLIARG_INIT_LOCAL_ZERO,       "init-local-zero",       '\0', "Initialize undefined variables to zero",     OFC_CLIARG_PARAM_PRIN_NONE, 0, true  },
	{ OFC_CLIARG_LOWERCASE_KEYWORD,     "lowercase-keyword",     '\0', "Print lower case fortran keywords",          OFC_CLIARG_PARAM_PRIN_NONE, 0, true  },
	{ OFC_CLIARG_INCLUDE,               "include",               'I',  "Add include path (--include <s> or -I<s>)",  OFC_CLIARG_PARAM_FILE_STR,  1, false },
	{ OFC_CLIARG_SEMA_STRUCT_TYPE,      "no-sema-struct-type",   '\0', "Disable struct to type semantic pass",       OFC_CLIARG_PARAM_SEMA_PASS, 0, true  },
	{ OFC_CLIARG_SEMA_CHAR_TRANSFER,    "no-sema-char-transfer", '\0', "Disable char to transfer semantic pass",     OFC_CLIARG_PARAM_SEMA_PASS, 0, true  },
	{ OFC_CLIARG_SEMA_UNREF_LABEL,      "no-sema-unref-label",   '\0', "Disable unreferenced label semantic pass",   OFC_CLIARG_PARAM_SEMA_PASS, 0, true  },
	{ OFC_CLIARG_SEMA_UNLAB_FORMAT,     "no-sema-unref-format",  '\0', "Disable unreferenced format semantic pass",  OFC_CLIARG_PARAM_SEMA_PASS, 0, true  },
	{ OFC_CLIARG_SEMA_UNLAB_CONT,       "no-sema-unlab-cont",    '\0', "Disable struct to type semantic pass",       OFC_CLIARG_PARAM_SEMA_PASS, 0, true  },
	{ OFC_CLIARG_SEMA_INTEGER_LOGICAL,  "no-sema-int-logical",   '\0', "Disable integer to logical semantic pass",   OFC_CLIARG_PARAM_SEMA_PASS, 0, true  },
	{ OFC_CLIARG_SEMA_UNUSED_DECL,      "sema-unused-decl",      '\0', "Enable unused declarations semantic pass",   OFC_CLIARG_PARAM_SEMA_PASS, 0, true  },
	{ OFC_CLIARG_NO_ESCAPE,             "no-escape",             '\0', "Treat backslash as an ordinary character",   OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
	{ OFC_CLIARG_COMMON_USAGE,          "common-usage",          '\0', "Print COMMON block usage for a file list",   OFC_CLIARG_PARAM_GLOB_NONE, 0, true  },
};

static const char* ofc_cliarg_file_ext__get(
	const char* path)
{
	if (!path)
		return NULL;

	const char* ext = NULL;
	unsigned i;
	for (i = 0; path[i] != '\0'; i++)
	{
		if (path[i] == '/')
			ext = NULL;
		else if (path[i] == '.')
			ext = &path[i + 1];
	}
	return ext;
}

static const ofc_cliarg_body_t*
	ofc_cliarg_arg__resolve_name(const char* arg_string)
{
	unsigned i;
	for (i = 0; i < OFC_CLIARG_INVALID; i++)
	{
		if (strcasecmp(cliargs[i].name, arg_string) == 0)
			return &cliargs[i];
	}

	return NULL;
}

static const ofc_cliarg_body_t*
	ofc_cliarg_arg__resolve_flag(const char arg_flag)
{
	unsigned i;
	for (i = 0; i < OFC_CLIARG_INVALID; i++)
	{
		if (cliargs[i].flag == arg_flag)
			return &cliargs[i];
	}

	return NULL;
}

static bool ofc_cliarg_param__resolve_int(
	const char* arg_string, int* param_int)
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

static bool ofc_cliarg_param__resolve_str(
	const char* arg_string)
{
	if (!arg_string || (arg_string[0] == '-'))
		return false;

	return true;
}

static bool ofc_cliarg__apply(
	ofc_global_opts_t* global_opts,
	ofc_print_opts_t* print_opts,
	ofc_lang_opts_t* lang_opts,
	ofc_sema_pass_opts_t* sema_pass_opts,
	ofc_file_t* file,
	const ofc_cliarg_t* arg)
{
	ofc_cliarg_e       arg_type   = arg->body->type;
	ofc_cliarg_param_e param_type = arg->body->param_type;

	switch (param_type)
	{
		case OFC_CLIARG_PARAM_GLOB_NONE:
			return ofc_cliarg_global_opts__set_flag(global_opts, arg_type);
		case OFC_CLIARG_PARAM_LANG_NONE:
			return ofc_cliarg_lang_opts__set_flag(lang_opts, arg_type);
		case OFC_CLIARG_PARAM_LANG_INT:
			return ofc_cliarg_lang_opts__set_num(lang_opts, arg_type, arg->value);
		case OFC_CLIARG_PARAM_PRIN_NONE:
			return ofc_cliarg_print_opts__set_flag(print_opts, arg_type);
		case OFC_CLIARG_PARAM_PRIN_INT:
			return ofc_cliarg_print_opts__set_num(print_opts, arg_type, arg->value);
		case OFC_CLIARG_PARAM_FILE_STR:
			return ofc_cliarg_file__set_str(file, arg_type, arg->str);
		case OFC_CLIARG_PARAM_SEMA_PASS:
			return ofc_cliarg_sema_pass_opts__set_flag(sema_pass_opts, arg_type);

		default:
			break;;
	}

	return false;
}

static bool ofc_cliarg_list__apply(
	ofc_global_opts_t* global_opts,
	ofc_print_opts_t* print_opts,
	ofc_lang_opts_t* lang_opts,
	ofc_sema_pass_opts_t* sema_pass_opts,
	ofc_file_t* file,
	ofc_cliarg_list_t* list)
{
	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_cliarg__apply(global_opts, print_opts,
			lang_opts, sema_pass_opts, file, list->arg[i]))
			return false;
	}

	return true;
}

typedef struct
{
	unsigned count;
	char**   path;
} ofc_cliarg_path_list_t;

static ofc_cliarg_path_list_t* ofc_cliarg_path_list_create(void)
{
	ofc_cliarg_path_list_t* path_list
		= (ofc_cliarg_path_list_t*)malloc(
			sizeof(ofc_cliarg_path_list_t));
	if (!path_list) return NULL;

	path_list->count = 0;
	path_list->path  = NULL;

	return path_list;
}

static bool ofc_cliarg_path_list_add(ofc_cliarg_path_list_t* path_list,
	const char* path)
{
	if (!path_list || !path) return false;

	char** nlist = (char**)realloc(path_list->path,
		(sizeof(char*) * (path_list->count + 1)));
	if (!nlist) return NULL;

	path_list->path = nlist;
	path_list->path[path_list->count++] = strdup(path);

	return true;
}

static void ofc_cliarg_path_list_delete(ofc_cliarg_path_list_t* path_list)
{
	if (!path_list) return;

	unsigned i;
	for (i = 0; i < path_list->count; i++)
		free(path_list->path[i]);

	free(path_list->path);
	free(path_list);
}


bool ofc_cliarg_parse(
	int argc,
    const char* argv[],
	ofc_file_list_t** file_list,
	ofc_print_opts_t* print_opts,
	ofc_global_opts_t* global_opts,
	ofc_sema_pass_opts_t* sema_pass_opts)
{
	const char* program_name = argv[0];

	if (argc < 2)
	{
		fprintf(stderr, "Error: Expected source path\n");
		ofc_cliarg_print_usage(program_name);
		return false;
	}

	ofc_cliarg_path_list_t* path_list = ofc_cliarg_path_list_create();
	ofc_cliarg_list_t*      args_list = ofc_cliarg_list_create();

	unsigned i = 1;
	while (i < (unsigned)argc)
	{
		if (argv[i][0] == '-')
		{
			if (argv[i][1] == '-')
			{
				ofc_cliarg_t* resolved_arg = NULL;
				const char* arg_str = argv[i] + 2;
				const ofc_cliarg_body_t* arg_body = ofc_cliarg_arg__resolve_name(arg_str);
				if (!arg_body)
				{
					fprintf(stderr, "Error: Unable to resolve argument: %s\n", argv[i]);
					ofc_cliarg_print_usage(program_name);
					return false;
				}
				i++;

				switch (arg_body->param_type)
				{
					case OFC_CLIARG_PARAM_GLOB_NONE:
					case OFC_CLIARG_PARAM_LANG_NONE:
					case OFC_CLIARG_PARAM_PRIN_NONE:
					case OFC_CLIARG_PARAM_SEMA_PASS:
						resolved_arg = ofc_cliarg_create(arg_body, NULL);
						break;

					case OFC_CLIARG_PARAM_LANG_INT:
					case OFC_CLIARG_PARAM_PRIN_INT:
					{
						int param = -1;
						if (!ofc_cliarg_param__resolve_int(argv[i++], &param))
						{
							fprintf(stderr, "Error: Expected parameter for argument: %s\n", argv[i]);
							ofc_cliarg_print_usage(program_name);
							return false;
						}
						resolved_arg = ofc_cliarg_create(arg_body, &param);
						break;
					}

					case OFC_CLIARG_PARAM_FILE_STR:
					{
						if (ofc_cliarg_param__resolve_str(argv[i]))
						{
							resolved_arg = ofc_cliarg_create(arg_body, argv[i]);
							i++;
						}
						else
						{
							fprintf(stderr, "Error: Expected parameter for argument: %s\n", argv[i]);
							ofc_cliarg_print_usage(program_name);
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
				unsigned arglen = strlen(arg_str);
				for (flag = 0; flag < arglen; flag++)
				{
					ofc_cliarg_t* resolved_arg = NULL;

					const ofc_cliarg_body_t* arg_body
						= ofc_cliarg_arg__resolve_flag(arg_str[flag]);
					if (!arg_body)
					{
						fprintf(stderr, "Error: Cannot resolve flag: %s\n", argv[i]);
						ofc_cliarg_print_usage(program_name);
						return false;
					}
					switch (arg_body->param_type)
					{
						case OFC_CLIARG_PARAM_GLOB_NONE:
						case OFC_CLIARG_PARAM_LANG_NONE:
						case OFC_CLIARG_PARAM_SEMA_PASS:
							resolved_arg = ofc_cliarg_create(arg_body, NULL);
							break;

						case OFC_CLIARG_PARAM_FILE_STR:
							if (flag == 0)
							{
								if (arg_str[1] == '\0')
								{
									i++;
									if (i < (unsigned)argc && ofc_cliarg_param__resolve_str(argv[i]))
									{
										resolved_arg = ofc_cliarg_create(arg_body, argv[i]);
									}
									else
									{
										fprintf(stderr, "Error: Expected parameter for argument: %s\n", argv[i - 1]);
										ofc_cliarg_print_usage(program_name);
										return false;
									}
								}
								else
								{
									resolved_arg = ofc_cliarg_create(arg_body, arg_str + 1);
								}
								flag = arglen;  /* Leave 'flag' loop as well. */
								break;
						}
						/* Fall through. */

						default:
							fprintf(stderr, "Error: Cannot group flags that require a parameter: %s\n", argv[i]);
							ofc_cliarg_print_usage(program_name);
							return false;
					}

					if (!resolved_arg
						|| !ofc_cliarg_list_add(args_list, resolved_arg))
						return false;
				}
				i++;
			}
		}
		else
		{
			/* Argument with no preceding dash is assumed to be a source file path */
			if (!ofc_cliarg_path_list_add(path_list, argv[i]))
				return false;
			i++;
		}
	}

	unsigned j;
	for (j = 0; j < path_list->count; j++)
	{
		char* path = path_list->path[j];
		const char* source_file_ext = ofc_cliarg_file_ext__get(path);

		ofc_lang_opts_t lang_opts = OFC_LANG_OPTS_F77;
		if (source_file_ext && (strcasecmp(source_file_ext, "F90") == 0))
			lang_opts = OFC_LANG_OPTS_F90;

		ofc_file_t* file = ofc_file_create(path, lang_opts);
		if (!file)
		{
			fprintf(stderr, "\nError: Failed read source file '%s'\n", path);
			ofc_cliarg_list_delete(args_list);
			ofc_cliarg_path_list_delete(path_list);
			return false;
		}

		ofc_lang_opts_t* lang_opts_ptr = ofc_file_modify_lang_opts(file);
		if (!lang_opts_ptr)
		{
			ofc_cliarg_list_delete(args_list);
			ofc_cliarg_path_list_delete(path_list);
			return false;
		}

		if (!ofc_cliarg_list__apply(global_opts, print_opts,
			lang_opts_ptr, sema_pass_opts, file, args_list))
		{
			ofc_cliarg_list_delete(args_list);
			ofc_cliarg_path_list_delete(path_list);
			return false;
		}

		if (!ofc_file_list_add(*file_list, file))
		{
			ofc_cliarg_list_delete(args_list);
			ofc_cliarg_path_list_delete(path_list);
			return false;
		}
	}

	ofc_cliarg_list_delete(args_list);
	ofc_cliarg_path_list_delete(path_list);

	return true;
}

static unsigned ofc_cliarg_longest_name_len(void)
{
	unsigned longest_name_len = 0;

	unsigned i;
	for (i = 0; i < OFC_CLIARG_INVALID; i++)
	{
		if (strlen(cliargs[i].name) > longest_name_len)
			longest_name_len = strlen(cliargs[i].name);
	}

	return longest_name_len;
}

void ofc_cliarg_print_usage(const char* name)
{
	printf("%s [OPTIONS] FILE\n", name);
	printf("Options:\n");

	unsigned name_len = ofc_cliarg_longest_name_len() + 5;

	unsigned i;
	for (i = 0; i < OFC_CLIARG_INVALID; i++)
	{
		unsigned line_len = 0;

		switch (cliargs[i].param_type)
		{
			case OFC_CLIARG_PARAM_LANG_INT:
				line_len = printf("  --%s <n>", cliargs[i].name);
				break;

			case OFC_CLIARG_PARAM_FILE_STR:
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
			case OFC_CLIARG_PARAM_LANG_INT:
			case OFC_CLIARG_PARAM_PRIN_INT:
				arg->value = *((int*)param);
				break;

			case OFC_CLIARG_PARAM_FILE_STR:
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

	if (arg->body->param_type == OFC_CLIARG_PARAM_FILE_STR)
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
