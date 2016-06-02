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
typedef struct ofc_parse_expr_implicit_do_s ofc_parse_expr_implicit_do_t;
typedef struct ofc_parse_lhs_implicit_do_s  ofc_parse_lhs_implicit_do_t;

#include <ofc/parse/list.h>
#include <ofc/parse/keyword.h>
#include <ofc/parse/literal.h>
#include <ofc/parse/operator.h>
#include <ofc/parse/array.h>
#include <ofc/parse/lhs.h>
#include <ofc/parse/expr.h>
#include <ofc/parse/assign.h>
#include <ofc/parse/call_arg.h>
#include <ofc/parse/define_file_arg.h>
#include <ofc/parse/implicit_do.h>
#include <ofc/parse/star_len.h>
#include <ofc/parse/type.h>
#include <ofc/parse/data.h>
#include <ofc/parse/decl.h>
#include <ofc/parse/common.h>
#include <ofc/parse/save.h>
#include <ofc/parse/implicit.h>
#include <ofc/parse/format.h>
#include <ofc/parse/pointer.h>
#include <ofc/parse/stmt.h>
#include <ofc/parse/file.h>

#endif
