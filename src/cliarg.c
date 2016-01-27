#include <ofc/cliarg.h>


static bool set_global_opts__flag(
	ofc_global_opts_t* global,
	int arg_type)
{
	if (!global)
		return false;

	switch (arg_type)
	{
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
		case CASE_SEN:
			lang_opts->case_sensitive = true;
			break;

		case FIXED_FORM:
			lang_opts->form = OFC_LANG_FORM_FIXED;
			break;
		case FREE_FORM:
			lang_opts->form = OFC_LANG_FORM_FREE;
			break;
		case TAB_FORM:
			lang_opts->form = OFC_LANG_FORM_TAB;
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

static const ofc_cliarg_body_t cliargs[] =
{
	/*ENUM        NAME          FLAG  DESCRIPTION                 PARAM_TYPE PARAMS EXCLUSIVE */
	{ PARSE_TREE, "parse-tree", '\0', "Prints the parse tree",             GLOB_NONE, 0, true },
	{ SEMA_TREE,  "sema-tree",  '\0', "Prints the semantic analysis tree", GLOB_NONE, 0, true },
	{ FIXED_FORM, "free-form",  '\0', "Sets free form type",               LANG_NONE, 0, true },
	{ FREE_FORM,  "fixed-form", '\0', "Sets fixed form type",              LANG_NONE, 0, true },
	{ TAB_FORM,   "tab-form",   '\0', "Sets tabbed form type",             LANG_NONE, 0, true },
	{ TAB_WIDTH,  "tab-width",  '\0', "Sets tab width",                    LANG_INT,  1, true },
	{ DEBUG,      "debug",      '\0', "Sets debug mode",                   LANG_NONE, 0, true },
	{ COLUMNS,    "columns",    '\0', "sets number of columns to <n>",     LANG_INT,  1, true },
	{ CASE_SEN,   "case-sen",   '\0', "Sets case sensitive mode",          LANG_NONE, 0, true },
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

static bool ofc_cliarg__apply(
	ofc_global_opts_t* global_opts,
	ofc_lang_opts_t* lang_opts,
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

		default:
			break;;
	}

	return false;
}

static bool ofc_cliarg_list__apply(
	ofc_global_opts_t* global_opts,
	ofc_lang_opts_t* lang_opts,
	ofc_cliarg_list_t* list)
{
	unsigned i;
	for (i = 0; i < list->count; i++)
	{
		if (!ofc_cliarg__apply(global_opts, lang_opts, list->arg[i]))
			return false;
	}

	return true;
}

bool ofc_cliarg_parse(
	int argc,
    const char* argv[],
	ofc_file_t** file,
	ofc_lang_opts_t* lang_opts,
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

				if (arg_body->param_num > 1)
				{
					/* TODO: Handle this if we have args with multiple values */
					return false;
				}

				int param = -1;
				if (arg_body->param_num > 0)
				{
					if (!resolve_param_pos_int(argv[i++], &param))
					{
						fprintf(stderr, "Error: Expected parameter for argument: %s\n", argv[i]);
						print_usage(program_name);
						return false;
					}
				}
				resolved_arg = ofc_cliarg_create(arg_body, param);

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
					if (!arg_body) return false;
					if (arg_body->param_num > 0)
					{
						fprintf(stderr, "Error: Cannot group flags that require a parameter: %s\n", argv[i]);
						print_usage(program_name);
						return false;
					}
					// TODO - Add support for single character flags.
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

	*file = ofc_file_create(path, *lang_opts);
	if (!file)
	{
		fprintf(stderr, "\nError: Failed read source file '%s'\n", path);
		return false;
	}

	if (source_file_ext
		&& (strcasecmp(source_file_ext, "F90") == 0))
		*lang_opts = OFC_LANG_OPTS_F90;

	if (!ofc_cliarg_list__apply(global_opts, lang_opts, args_list))
		return false;

	ofc_cliarg_list_delete(args_list);

	return true;
}


void print_usage(const char* name)
{
		printf("%s [OPTIONS] FILE\n", name);
		printf("Options:\n");

		unsigned i;
		for (i = 0; i < INVALID; i++)
		{
			printf("  --%s\t\t%s\n",
				cliargs[i].name, cliargs[i].desc);
		}
}

ofc_cliarg_t* ofc_cliarg_create(
	const ofc_cliarg_body_t* arg_body,
	int value)
{
	ofc_cliarg_t* arg = (ofc_cliarg_t*)malloc(sizeof(ofc_cliarg_t));
	if (!arg)
		return NULL;

	arg->body = arg_body;
	arg->value = value;

	return arg;
}

void ofc_cliarg_delete(ofc_cliarg_t* arg)
{
	if (!arg)
		return;

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
