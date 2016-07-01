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

#include "ofc/sema.h"

typedef enum
{
	OFC_SEMA_PASS_STRUCT_TYPE = 0,
	OFC_SEMA_PASS_CHAR_TRANSFER,
	OFC_SEMA_PASS_UNREF_LABEL,
	OFC_SEMA_PASS_UNLABELLED_FORMAT,
	OFC_SEMA_PASS_UNLABELLED_CONTINUE,
	OFC_SEMA_PASS_UNUSED_COMMON,
	OFC_SEMA_PASS_UNUSED_DECL,
	OFC_SEMA_PASS_INTEGER_LOGICAL,

	OFC_SEMA_PASS_COUNT
} ofc_sema_pass_e;

typedef struct
{
	ofc_sema_pass_e type;
	char*           desc;

	bool (*pass_func)(ofc_sema_scope_t* scope);
} ofc_sema_pass_t;

static const ofc_sema_pass_t passes[] =
{
	{ OFC_SEMA_PASS_STRUCT_TYPE,         "STRUCTURE to TYPE",                     ofc_sema_pass_struct_type         },
	{ OFC_SEMA_PASS_CHAR_TRANSFER,       "string cast TRANSFER",                  ofc_sema_pass_char_transfer       },
	{ OFC_SEMA_PASS_UNREF_LABEL,         "remove unreferenced labels",            ofc_sema_pass_unref_label         },
	{ OFC_SEMA_PASS_UNLABELLED_FORMAT,   "remove unlabelled format statements",   ofc_sema_pass_unlabelled_format   },
	{ OFC_SEMA_PASS_UNLABELLED_CONTINUE, "remove unlabelled continue statements", ofc_sema_pass_unlabelled_continue },
	{ OFC_SEMA_PASS_UNUSED_COMMON,       "warn about unused COMMON blocks",       ofc_sema_pass_unused_common       },
	{ OFC_SEMA_PASS_UNUSED_DECL,         "remove unused declarations",            ofc_sema_pass_unused_decl         },
	{ OFC_SEMA_PASS_INTEGER_LOGICAL,     "INTEGER to LOGICAL Expression",         ofc_sema_pass_integer_logical     },
};

bool ofc_sema_run_passes(
	ofc_file_t* file,
	ofc_sema_pass_opts_t* sema_pass_opts,
	ofc_sema_scope_t* scope)
{
	unsigned i;
	for(i = 0; i < OFC_SEMA_PASS_COUNT; i++)
	{
		switch (i)
		{
			case OFC_SEMA_PASS_STRUCT_TYPE:
				if(!sema_pass_opts->struct_type)
					continue;
				break;

			case OFC_SEMA_PASS_UNREF_LABEL:
				if(!sema_pass_opts->unref_label)
					continue;
				break;

			case OFC_SEMA_PASS_UNLABELLED_FORMAT:
				if(!sema_pass_opts->unlabelled_format)
					continue;
				break;

			case OFC_SEMA_PASS_UNLABELLED_CONTINUE:
				if(!sema_pass_opts->unlabelled_continue)
					continue;
				break;

			case OFC_SEMA_PASS_CHAR_TRANSFER:
				if(!sema_pass_opts->char_transfer)
					continue;
				break;

			case OFC_SEMA_PASS_UNUSED_COMMON:
				if(!sema_pass_opts->unused_common)
					continue;
				break;

			case OFC_SEMA_PASS_UNUSED_DECL:
				if(!sema_pass_opts->unused_decl)
					continue;
				break;

			case OFC_SEMA_PASS_INTEGER_LOGICAL:
				if(!sema_pass_opts->integer_logical)
					continue;
				break;

			default:
				return false;
		}

        if (!passes[i].pass_func(scope))
		{
			ofc_file_error(file, NULL,
				"Failed %s semantic pass",
					passes[i].desc);
			return false;
		}
	}
	return true;
}
