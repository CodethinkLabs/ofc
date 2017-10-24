/* Copyright 2017 Codethink Ltd.
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

#include "ofc/util/dprintf.h"
#include <stdarg.h>
#include <unistd.h>

#ifndef NATIVE_DPRINTF
int dprintf(int fd, const char *format, ...)
{
	va_list args;
	va_start(args, format);

	va_list largs;
	va_copy(largs, args);
	int len = vsnprintf(NULL, 0, format, largs);
	va_end(largs);

	if (len <= 0)
	{
		va_end(args);
		return len;
	}

	char buff[len + 1];
	int plen = vsnprintf(buff, (len + 1), format, args);
	va_end(args);

	if (len != plen)
		return -1;

	return (int)write(fd, buff, len);
}
#endif
