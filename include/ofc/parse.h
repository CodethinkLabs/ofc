#ifndef __ofc_parse_h__
#define __ofc_parse_h__

#include <ofc/sparse.h>
#include <ofc/str_ref.h>
#include <ofc/fctype.h>

#include <stdio.h>

#include <ofc/parse/debug.h>

typedef struct ofc_parse_lhs_s ofc_parse_lhs_t;
typedef struct ofc_parse_expr_s ofc_parse_expr_t;
typedef struct ofc_parse_stmt_s ofc_parse_stmt_t;
typedef struct ofc_parse_implicit_do_s ofc_parse_implicit_do_t;

#include <ofc/parse/list.h>
#include <ofc/parse/keyword.h>
#include <ofc/parse/literal.h>
#include <ofc/parse/label.h>
#include <ofc/parse/operator.h>
#include <ofc/parse/array.h>
#include <ofc/parse/lhs.h>
#include <ofc/parse/expr.h>
#include <ofc/parse/assign.h>
#include <ofc/parse/call_arg.h>
#include <ofc/parse/implicit_do.h>
#include <ofc/parse/star_len.h>
#include <ofc/parse/type.h>
#include <ofc/parse/data.h>
#include <ofc/parse/decl.h>
#include <ofc/parse/common.h>
#include <ofc/parse/save.h>
#include <ofc/parse/implicit.h>
#include <ofc/parse/iolist.h>
#include <ofc/parse/format.h>
#include <ofc/parse/record.h>
#include <ofc/parse/pointer.h>
#include <ofc/parse/stmt.h>

#endif
