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
	STRUCT_TYPE = 0,
	CHAR_TRANSFER,
	UNREF_LABEL,
	UNLABELLED_FORMAT,
	UNLABELLED_CONTINUE,

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
	{ STRUCT_TYPE,         "STRUCTURE to TYPE",                     ofc_sema_pass_struct_type         },
	{ CHAR_TRANSFER,       "string cast TRANSFER",                  ofc_sema_pass_char_transfer       },
	{ UNREF_LABEL,         "remove unreferenced labels",            ofc_sema_pass_unref_label         },
	{ UNLABELLED_FORMAT,   "remove unlabelled format statements",   ofc_sema_pass_unlabelled_format   },
	{ UNLABELLED_CONTINUE, "remove unlabelled continue statements", ofc_sema_pass_unlabelled_continue },
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
			case STRUCT_TYPE:
				if(!sema_pass_opts->struct_type)
					continue;
				break;

			case UNREF_LABEL:
				if(!sema_pass_opts->unref_label)
					continue;
				break;

			case UNLABELLED_FORMAT:
				if(!sema_pass_opts->unlabelled_format)
					continue;
				break;

			case UNLABELLED_CONTINUE:
				if(!sema_pass_opts->unlabelled_continue)
					continue;
				break;

			case CHAR_TRANSFER:
				if(!sema_pass_opts->char_transfer)
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
