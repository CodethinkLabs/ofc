#ifndef __ofc_fctype_h__
#define __ofc_fctype_h__

#include <stdbool.h>
#include <ctype.h>

bool ofc_is_vspace(char c);
bool ofc_is_hspace(char c);
bool ofc_is_ident(char c);

bool ofc_is_end_statement(const char* c, unsigned* len);

#endif
