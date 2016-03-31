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

ofc_global_opts_t global_opts;

int main(int argc, const char* argv[])
{
	global_opts = OFC_GLOBAL_OPTS_DEFAULT;

	ofc_print_opts_t print_opts = OFC_PRINT_OPTS_DEFAULT;

	ofc_file_list_t* file_list = ofc_file_list_create();

	if (!ofc_cliarg_parse(argc, argv,
		&file_list, &print_opts, &global_opts))
		return EXIT_FAILURE;

	unsigned i;
	for (i = 0; i < file_list->count; i++)
	{
		ofc_file_t* file = file_list[i];

		ofc_sparse_t* condense = ofc_prep(file);
		if (!condense)
		{
			if (ofc_file_no_errors())
				ofc_file_error(file, NULL, "Failed to preprocess source file");
			return EXIT_FAILURE;
		}
		ofc_file_delete(file);

		ofc_parse_stmt_list_t* program
			= ofc_parse_file(condense);
		if (!program)
		{
			if (ofc_file_no_errors())
				ofc_file_error(file, NULL, "Failed to parse program");
			ofc_sparse_delete(condense);
			return EXIT_FAILURE;
		}

		if (global_opts.parse_print)
		{
			ofc_colstr_t* cs = ofc_colstr_create(print_opts, 72, 0);
			if (!ofc_parse_stmt_list_print(cs, 0, program))
			{
				ofc_file_error(file, NULL, "Failed to print parse tree");
				ofc_parse_stmt_list_delete(program);
				ofc_sparse_delete(condense);
				return EXIT_FAILURE;
			}
			ofc_colstr_fdprint(cs, STDOUT_FILENO);
			ofc_colstr_delete(cs);
		}

		ofc_sema_scope_t* sema = NULL;
		if (!global_opts.parse_only)
		{
			sema = ofc_sema_scope_global(program);
			if (!sema)
			{
				if (ofc_file_no_errors())
					ofc_file_error(file, NULL, "Program failed semantic analysis");
				ofc_parse_stmt_list_delete(program);
				ofc_sparse_delete(condense);
				return EXIT_FAILURE;
			}
		}

		/* TODO - Add a command line argument for this pass. */
		if (!ofc_sema_pass_struct_type(sema))
		{
			ofc_file_error(file, NULL, "Failed 'STRUCTURE to TYPE' semantic pass");
			ofc_sema_scope_delete(sema);
			ofc_parse_stmt_list_delete(program);
			ofc_sparse_delete(condense);
			return EXIT_FAILURE;
		}

		/* TODO - Add a command line argument for this pass. */
		if (!ofc_sema_pass_char_transfer(sema))
		{
			ofc_file_error(file, NULL, "Failed 'string cast TRANSFER' semantic pass");
			ofc_sema_scope_delete(sema);
			ofc_parse_stmt_list_delete(program);
			ofc_sparse_delete(condense);
			return EXIT_FAILURE;
		}

		/* TODO - Add a command line argument for this pass. */
		if (!ofc_sema_pass_unref_label(sema))
		{
			ofc_file_error(file, NULL, "Failed 'Remove unused labels' semantic pass");
			ofc_sema_scope_delete(sema);
			ofc_parse_stmt_list_delete(program);
			ofc_sparse_delete(condense);
			return EXIT_FAILURE;
		}


		/* TODO - Add a command line argument for this pass. */
		if (!ofc_sema_pass_unlabelled_format(sema))
		{
			ofc_file_error(file, NULL, "Failed 'Remove unlabelled format statements' semantic pass");
			ofc_sema_scope_delete(sema);
			ofc_parse_stmt_list_delete(program);
			ofc_sparse_delete(condense);
			return EXIT_FAILURE;
		}

		/* TODO - Add a command line argument for this pass. */
		if (!ofc_sema_pass_unlabelled_continue(sema))
		{
			ofc_file_error(file, NULL, "Failed 'Remove unlabelled continue statements' semantic pass");
			ofc_sema_scope_delete(sema);
			ofc_parse_stmt_list_delete(program);
			ofc_sparse_delete(condense);
			return EXIT_FAILURE;
		}

		if (global_opts.sema_print)
		{
			ofc_colstr_t* cs = ofc_colstr_create(print_opts, 72, 0);
			if (!ofc_sema_scope_print(cs, 0, sema))
			{
				ofc_file_error(file, NULL, "Failed to print semantic tree");
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
	}

	return EXIT_SUCCESS;
}
