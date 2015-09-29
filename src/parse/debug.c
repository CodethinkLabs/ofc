#include "debug.h"
#include <stdlib.h>
#include <stdio.h>

typedef struct
{
	const sparse_t* src;
	const char*     ptr;

	bool is_error;
	char* message;
} parse_debug_msg_t;

struct parse_debug_s
{
	unsigned            count, max;
	parse_debug_msg_t** message;
};



parse_debug_t* parse_debug_create(void)
{
	parse_debug_t* stack
		= (parse_debug_t*)malloc(
			sizeof(parse_debug_t));
	if (!stack) return NULL;

	stack->count   = 0;
	stack->max     = 0;
	stack->message = NULL;

	return stack;
}

void parse_debug_delete(parse_debug_t* stack)
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
	free(stack);
}


unsigned parse_debug_position(const parse_debug_t* stack)
{
	return (stack ? stack->count : 0);
}

void parse_debug_rewind(
	parse_debug_t* stack, unsigned position)
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

void parse_debug_print(const parse_debug_t* stack)
{
	if (!stack)
		return;

	unsigned i;
	for (i = 0; i < stack->count; i++)
	{
		parse_debug_msg_t* message
			= stack->message[i];
		if (!message) continue;

		if (message->is_error)
		{
			sparse_error(message->src, message->ptr,
				"%s", message->message);
		}
		else
		{
			sparse_warning(message->src, message->ptr,
				"%s", message->message);
		}
	}
}


#include <stdarg.h>

static void parse_debug_message(
	parse_debug_t* stack,
	const sparse_t* src, const char* ptr,
	bool is_error, const char* format, va_list args)
{
	/* Error reporting is critical, if it fails we just abort. */

	if (!stack)
		abort();

	parse_debug_msg_t* message
		= (parse_debug_msg_t*)malloc(
			sizeof(parse_debug_msg_t));
	if (!message) abort();

	message->src = src;
	message->ptr = ptr;

	message->is_error = is_error;

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
		parse_debug_msg_t** nstack
			= (parse_debug_msg_t**)realloc(stack->message,
				sizeof(parse_debug_msg_t*) * nmax);
		if (!nstack) abort();
		stack->message = nstack;
		stack->max = nmax;
	}

	stack->message[stack->count++] = message;
}

void parse_debug_error(
	parse_debug_t* stack,
	const sparse_t* src, const char* ptr,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);
	parse_debug_message(
		stack, src, ptr,
		true, format, args);
	va_end(args);
}

void parse_debug_warning(
	parse_debug_t* stack,
	const sparse_t* src, const char* ptr,
	const char* format, ...)
{
	va_list args;
	va_start(args, format);
	parse_debug_message(
		stack, src, ptr,
		false, format, args);
	va_end(args);
}
