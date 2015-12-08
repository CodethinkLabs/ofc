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

#ifndef __ofc_label_table_h__
#define __ofc_label_table_h__

#include <stdbool.h>

typedef struct ofc_label_table_s ofc_label_table_t;

ofc_label_table_t* ofc_label_table_create(void);
void               ofc_label_table_delete(ofc_label_table_t* table);

bool ofc_label_table_add(
	ofc_label_table_t* table, unsigned offset, unsigned number);
bool ofc_label_table_find(
	const ofc_label_table_t* table, unsigned offset, unsigned* number);

#endif
