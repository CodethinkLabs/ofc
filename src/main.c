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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <ofc/file.h>
#include <ofc/prep.h>
#include <ofc/parse/file.h>
#include <ofc/sema.h>


static void print_usage(const char* name)
{
	printf("%s [OPTIONS] FILE\n", name);
	printf("Options:\n");
	printf("  -free-form, -fixed-form, -tab-form    selects form type\n");
	printf("  -tab-width-<n>                        sets tab with to <n>\n");
	printf("  -d, -debug                            selects debug mode, defaults to false\n");
	printf("  -columns-<n>                          sets number of columns to <n>\n");
	printf("  -case-sen                             selects case sensitivity, defaults to false\n");
}

static const char *get_file_ext(const char *path) {
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

typedef enum
{
	FIXED_FORM,
	FREE_FORM,
	TAB_FORM,
	TAB_WIDTH,
	DEBUG,
	COLUMNS,
	CASE_SEN,
	INVALID
} args_e;

static args_e get_options(char* arg, int* num)
{
	char* option[4];
	char* token = strtok(arg, "-");
	int count = 0;

	/* Get options and values */
	while (token)
	{
		option[count++] = token;
		token = strtok(NULL, "-");
	}

	/* Parse -free-form, -tab-form, -fixed-form*/
	if ((count == 2) && (strcmp(option[1], "form") == 0))
	{
		if (strcmp(option[0], "fixed") == 0)
		{
			return FIXED_FORM;
		}
		else if (strcmp(option[0], "free") == 0)
		{
			return FREE_FORM;
		}
		else if (strcmp(option[0], "tab") == 0)
		{
			return TAB_FORM;
		}
		else
		{
			fprintf(stderr, "Error: invalid option\n");
			return INVALID;
		}
	}
	/* Parse -tab-width-n */
	else if ((count == 3)
		&& (strcmp(option[0], "tab"  ) == 0)
		&& (strcmp(option[1], "width") == 0))
	{
		int width = strtol(option[2], (char **)NULL, 10);
		if (width >= 0)
		{
			*num = width;
			return TAB_WIDTH;
		}
		else
		{
			fprintf(stderr, "Error: invalid tab width\n");
			return INVALID;
		}
	}
	/* Parse -debug, -d */
	else if ((count == 1) && ((strcmp(option[0], "debug") == 0)
		|| (strcmp(option[0], "d") == 0)))
	{
			return DEBUG;
	}
	/* Parse -columns-n, -c-n */
	else if ((count == 2) && ((strcmp(option[0], "columns") == 0)
		|| (strcmp(option[0], "c") == 0)))
	{
		int col = strtol(option[1], (char **)NULL, 10);
		if (col > 0)
		{
			*num = col;
			return COLUMNS;
		}
		else
		{
			fprintf(stderr, "Error: invalid number of columns\n");
			return INVALID;
		}
	}
	/* Parse -case-sen */
	else if ((count == 2) && (strcmp(option[0], "case") == 0)
		&& (strcmp(option[1], "sen") == 0))
	{
		return CASE_SEN;
	}
	else
	{
		fprintf(stderr, "Error: invalid option\n");
		return INVALID;
	}
}

int main(int argc, const char* argv[])
{
	if (argc < 2)
	{
		fprintf(stderr, "Error: Expected source path\n");
		print_usage(argv[0]);
		return EXIT_FAILURE;
	}

	const char* path = argv[argc - 1];

	const char* source_file_ext = get_file_ext(path);

	ofc_lang_opts_t opts = OFC_LANG_OPTS_F77;

	if (source_file_ext
		&& (strcasecmp(source_file_ext, "F90") == 0))
		opts = OFC_LANG_OPTS_F90;

	int i;
	for (i = 1; i < (argc - 1); i++)
	{
		char* arg = strdup(argv[i]);
		int num = 0;
		args_e name = get_options(arg, &num);
		free(arg);

		switch(name)
		{
			case FIXED_FORM:
				opts.form = OFC_LANG_FORM_FIXED;
				break;
			case FREE_FORM:
				opts.form = OFC_LANG_FORM_FREE;
				break;
			case TAB_FORM:
				opts.form = OFC_LANG_FORM_TAB;
				break;
			case TAB_WIDTH:
				opts.tab_width = num;
				break;
			case DEBUG:
				opts.debug = true;
				break;
			case COLUMNS:
				opts.columns = num;
				break;
			case CASE_SEN:
				opts.case_sensitive = true;
				break;
			default:
				print_usage(argv[0]);
				return EXIT_FAILURE;
		}
	}

	ofc_file_t* file = ofc_file_create(path, opts);
	if (!file)
	{
		fprintf(stderr, "Error: Failed read source file '%s'\n", path);
		return EXIT_FAILURE;
	}

	ofc_sparse_t* condense = ofc_prep(file);
	ofc_file_delete(file);
	if (!condense)
	{
		fprintf(stderr, "Error: Failed preprocess source file '%s'\n", path);
		return EXIT_FAILURE;
	}

	ofc_parse_stmt_list_t* program
		= ofc_parse_file(condense);

	if (!program)
	{
		fprintf(stderr, "Error: Failed to parse program\n");
		ofc_sparse_delete(condense);
		return EXIT_FAILURE;
	}

	#ifdef OFC_PRINT_PARSE
	ofc_colstr_t* cs = ofc_colstr_create(72, 0);
	if (!ofc_parse_stmt_list_print(cs, 0, program))
	{
		fprintf(stderr, "Error: Failed to print parse tree\n");
		ofc_parse_stmt_list_delete(program);
		ofc_sparse_delete(condense);
		return EXIT_FAILURE;
	}
	ofc_colstr_fdprint(cs, STDOUT_FILENO);
	ofc_colstr_delete(cs);
	#endif

	ofc_sema_scope_t* sema = ofc_sema_scope_global(
		&opts, condense, program);
	if (!sema)
	{
		fprintf(stderr, "Error: Program failed semantic analysis\n");
		ofc_parse_stmt_list_delete(program);
		ofc_sparse_delete(condense);
		return EXIT_FAILURE;
	}

	#ifdef OFC_PRINT_SEMA
	ofc_colstr_t* cs = ofc_colstr_create(72, 0);
	if (!ofc_sema_scope_print(cs, 0, sema))
	{
		fprintf(stderr, "Error: Failed to print semantic tree\n");
		ofc_colstr_delete(cs);
		ofc_sema_scope_delete(sema);
		ofc_parse_stmt_list_delete(program);
		ofc_sparse_delete(condense);
		return EXIT_FAILURE;
	}
	ofc_colstr_fdprint(cs, STDOUT_FILENO);
	ofc_colstr_delete(cs);
	#endif

	ofc_sema_scope_delete(sema);
	ofc_parse_stmt_list_delete(program);
	ofc_sparse_delete(condense);
	return EXIT_SUCCESS;
}
