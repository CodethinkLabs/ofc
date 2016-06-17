#ifndef __ofc_global_h__
#define __ofc_global_h__

#include <ofc/sema.h>

bool ofc_global_pass_common(
	ofc_sema_scope_t* scope);

bool ofc_global_pass_args(
	ofc_sema_scope_t* scope);

#endif
