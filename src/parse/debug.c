#include "debug.h"
#include <stdlib.h>
#include <stdio.h>

typedef struct
{
	const ofc_sparse_t* src;
	const char*     ptr;

	char* message;
} ofc_parse_debug_msg_t;

struct ofc_parse_debug_s
{
	unsigned            count, max;
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

		ofc_sparse_warning(message->src, message->ptr,
			"%s", message->message);
	}
}


#include <stdarg.h>

static void ofc_parse_debug_message(
	ofc_parse_debug_t* stack,
	const ofc_sparse_t* src, const char* ptr,
	const char* format, va_list args)
{
	/* Error reporting is critical, if it fails we just abort. */

	if (!stack)
		abort();

	ofc_parse_debug_msg_t* message
		= (ofc_parse_debug_msg_t*)malloc(
			sizeof(ofc_parse_debug_msg_t));
	if (!message) abort();

	message->src = src;
	message->ptr = ptr;

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
	const ofc_sparse_t* src, const char* ptr,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);
	ofc_parse_debug_message(
		stack, src, ptr,
		format, args);
	va_end(args);
}
