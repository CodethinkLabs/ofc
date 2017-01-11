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

#ifndef __ofc_sema_h__
#define __ofc_sema_h__

#include <ofc/parse.h>
#include <ofc/hashmap.h>
#include <ofc/global_opts.h>

typedef struct ofc_sema_stmt_s       ofc_sema_stmt_t;
typedef struct ofc_sema_scope_s      ofc_sema_scope_t;
typedef struct ofc_sema_module_s     ofc_sema_module_t;
typedef struct ofc_sema_type_s       ofc_sema_type_t;
typedef struct ofc_sema_expr_s       ofc_sema_expr_t;
typedef struct ofc_sema_lhs_s        ofc_sema_lhs_t;
typedef struct ofc_sema_decl_s       ofc_sema_decl_t;
typedef struct ofc_sema_decl_alias_s ofc_sema_decl_alias_t;
typedef struct ofc_sema_implicit_s   ofc_sema_implicit_t;

typedef struct ofc_sema_decl_list_s         ofc_sema_decl_list_t;
typedef struct ofc_sema_decl_alias_map_s    ofc_sema_decl_alias_map_t;
typedef struct ofc_sema_expr_list_s         ofc_sema_expr_list_t;
typedef struct ofc_sema_stmt_list_s         ofc_sema_stmt_list_t;
typedef struct ofc_sema_lhs_list_s          ofc_sema_lhs_list_t;
typedef struct ofc_sema_module_list_s       ofc_sema_module_list_t;
typedef struct ofc_sema_format_label_list_s ofc_sema_format_label_list_t;

#include <ofc/sema/kind.h>
#include <ofc/sema/array.h>
#include <ofc/sema/structure.h>
#include <ofc/sema/typeval.h>
#include <ofc/sema/parameter.h>
#include <ofc/sema/equiv.h>
#include <ofc/sema/common.h>
#include <ofc/sema/format.h>
#include <ofc/sema/label.h>
#include <ofc/sema/external.h>
#include <ofc/sema/dummy_arg.h>
#include <ofc/sema/intrinsic.h>
#include <ofc/sema/io.h>
#include <ofc/sema/arg.h>
#include <ofc/sema/range.h>
#include <ofc/sema/accessibility.h>

#include <ofc/sema/stmt.h>
#include <ofc/sema/type.h>
#include <ofc/sema/expr.h>
#include <ofc/sema/lhs.h>
#include <ofc/sema/decl.h>
#include <ofc/sema/implicit.h>
#include <ofc/sema/scope.h>
#include <ofc/sema/module.h>

#include <ofc/sema/pass.h>

#endif
