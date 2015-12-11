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

#include "ofc/parse/debug.h"

typedef struct
{
	ofc_sparse_ref_t ref;
	char*            message;
} ofc_parse_debug_msg_t;

struct ofc_parse_debug_s
{
	unsigned                count, max;
	ofc_parse_debug_msg_t** message;
};



ofc_parse_debug_t* ofc_parse_debug_create(void)
{
	ofc_parse_debug_t* stack
		= (ofc_parse_debug_t*)malloc(
			sizeof(ofc_parse_debug_t));
	if (!stack) return NULL;

	stack->count   = 0;
	stack->max     = 0;
	stack->message = NULL;

	return stack;
}

void ofc_parse_debug_delete(ofc_parse_debug_t* stack)
{
	if (!stack)
		return;

	unsigned i;
	for (i = 0; i < stack->count; i++)
	{
		if (!stack->message[i])
			continue;
		free(stack->message[i]->message);
		free(stack->message[i]);
	}
	free(stack->message);
	free(stack);
}


unsigned ofc_parse_debug_position(const ofc_parse_debug_t* stack)
{
	return (stack ? stack->count : 0);
}

void ofc_parse_debug_rewind(
	ofc_parse_debug_t* stack, unsigned position)
{
	if (!stack)
		return;

	unsigned i;
	for (i = position; i < stack->count; i++)
	{
		if (!stack->message[i])
			continue;

		free(stack->message[i]->message);
		free(stack->message[i]);
		stack->message[i] = NULL;
	}

	stack->count = position;
}

void ofc_parse_debug_print(const ofc_parse_debug_t* stack)
{
	if (!stack)
		return;

	unsigned i;
	for (i = 0; i < stack->count; i++)
	{
		ofc_parse_debug_msg_t* message
			= stack->message[i];
		if (!message) continue;

		ofc_sparse_ref_warning(message->ref,
			"%s", message->message);
	}
}


#include <stdarg.h>

static void ofc_parse_debug_message(
	ofc_parse_debug_t* stack,
	ofc_sparse_ref_t ref,
	const char* format, va_list args)
{
	/* Error reporting is critical, if it fails we just abort. */

	if (!stack)
		abort();

	ofc_parse_debug_msg_t* message
		= (ofc_parse_debug_msg_t*)malloc(
			sizeof(ofc_parse_debug_msg_t));
	if (!message) abort();

	message->ref = ref;

	va_list largs;
	va_copy(largs, args);
	int len = vsnprintf(NULL, 0, format, largs);
	va_end(largs);

	if (len <= 0) abort();

	message->message = (char*)malloc(len + 1);
	if (!message->message) abort();
	vsprintf(message->message, format, args);

	if (stack->count >= stack->max)
	{
		unsigned nmax = (stack->max << 1);
		if (nmax == 0) nmax = 16;
		ofc_parse_debug_msg_t** nstack
			= (ofc_parse_debug_msg_t**)realloc(stack->message,
				sizeof(ofc_parse_debug_msg_t*) * nmax);
		if (!nstack) abort();
		stack->message = nstack;
		stack->max = nmax;
	}

	stack->message[stack->count++] = message;
}

void ofc_parse_debug_warning(
	ofc_parse_debug_t* stack,
	const ofc_sparse_ref_t ref,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);
	ofc_parse_debug_message(
		stack, ref,
		format, args);
	va_end(args);
}
