#ifndef __ofc_sema_h__
#define __ofc_sema_h__

#include <ofc/parse.h>
#include <ofc/hashmap.h>

typedef struct ofc_sema_stmt_s      ofc_sema_stmt_t;
typedef struct ofc_sema_stmt_list_s ofc_sema_stmt_list_t;
typedef struct ofc_sema_scope_s     ofc_sema_scope_t;
typedef struct ofc_sema_type_s      ofc_sema_type_t;
typedef struct ofc_sema_expr_s      ofc_sema_expr_t;

#include <ofc/sema/array.h>
#include <ofc/sema/structure.h>
#include <ofc/sema/typeval.h>
#include <ofc/sema/parameter.h>
#include <ofc/sema/implicit.h>
#include <ofc/sema/decl.h>
#include <ofc/sema/label.h>

#include <ofc/sema/stmt.h>
#include <ofc/sema/scope.h>
#include <ofc/sema/type.h>
#include <ofc/sema/expr.h>

#endif
