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

#ifndef __ofc_prep_h__
#define __ofc_prep_h__

#include "file.h"
#include "sparse.h"

ofc_sparse_t* ofc_prep_unformat(ofc_file_t* file);
ofc_sparse_t* ofc_prep_condense(ofc_sparse_t* unformat);
ofc_sparse_t* ofc_prep(ofc_file_t* file);

#endif
