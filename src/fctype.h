#ifndef __fctype_h__
#define __fctype_h__

#include <stdbool.h>
#include <ctype.h>

bool is_vspace(char c);
bool is_hspace(char c);
bool is_ident(char c);

bool is_end_statement(const char* c, unsigned* len);

#endif
