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

#ifndef __ofc_fctype_h__
#define __ofc_fctype_h__

#include <stdbool.h>
#include <ctype.h>

bool ofc_is_vspace(char c);
bool ofc_is_hspace(char c);
bool ofc_is_ident(char c);

bool ofc_is_end_statement(const char* c, unsigned* len);

#endif
