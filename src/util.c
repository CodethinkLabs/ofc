#include "util.h"
#include <stdio.h>

bool dprintf_bool(
	int fd, const char* format, ...)
{
	va_list args;
	va_start(args, format);
	bool success = (vdprintf(fd, format, args) > 0);
	va_end(args);
	return success;
}
