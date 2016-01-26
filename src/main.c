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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "ofc/file.h"
#include "ofc/parse/file.h"
#include "ofc/prep.h"
#include "ofc/sema.h"
#include "ofc/cliarg.h"


int main(int argc, const char* argv[])
{
	ofc_lang_opts_t   lang_opts   = OFC_LANG_OPTS_DEFAULT;
	ofc_global_opts_t global_opts = OFC_GLOBAL_OPTS_DEFAULT;

	ofc_file_t* file = NULL;

	if (!ofc_cliarg_parse(argc, argv,
		&file, &lang_opts, &global_opts))
		return EXIT_FAILURE;

	ofc_sparse_t* condense = ofc_prep(file);
	if (!condense)
	{
		fprintf(stderr, "\nError: Failed preprocess source file '%s'\n",
			ofc_file_get_path(file));
		return EXIT_FAILURE;
	}
	ofc_file_delete(file);

	ofc_parse_stmt_list_t* program
		= ofc_parse_file(condense);

	if (!program)
	{
		fprintf(stderr, "\nError: Failed to parse program\n");
		ofc_sparse_delete(condense);
		return EXIT_FAILURE;
	}

	if (global_opts.parse_print)
	{
		ofc_colstr_t* cs = ofc_colstr_create(72, 0);
		if (!ofc_parse_stmt_list_print(cs, 0, program))
		{
			fprintf(stderr, "\nError: Failed to print parse tree\n");
			ofc_parse_stmt_list_delete(program);
			ofc_sparse_delete(condense);
			return EXIT_FAILURE;
		}
		ofc_colstr_fdprint(cs, STDOUT_FILENO);
		ofc_colstr_delete(cs);
	}

	ofc_sema_scope_t* sema = ofc_sema_scope_global(
		&lang_opts, program);
	if (!sema)
	{
		fprintf(stderr, "\nError: Program failed semantic analysis\n");
		ofc_parse_stmt_list_delete(program);
		ofc_sparse_delete(condense);
		return EXIT_FAILURE;
	}

	if (global_opts.sema_print)
	{
		ofc_colstr_t* cs = ofc_colstr_create(72, 0);
		if (!ofc_sema_scope_print(cs, 0, sema))
		{
			fprintf(stderr, "\nError: Failed to print semantic tree\n");
			ofc_colstr_delete(cs);
			ofc_sema_scope_delete(sema);
			ofc_parse_stmt_list_delete(program);
			ofc_sparse_delete(condense);
			return EXIT_FAILURE;
		}
		ofc_colstr_fdprint(cs, STDOUT_FILENO);
		ofc_colstr_delete(cs);
	}

	ofc_sema_scope_delete(sema);
	ofc_parse_stmt_list_delete(program);
	ofc_sparse_delete(condense);
	return EXIT_SUCCESS;
}
