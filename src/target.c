/* Copyright 2016 Codethink Ltd.
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

#include "ofc/target.h"

static unsigned ofc_target__logical_size = 4;
static unsigned ofc_target__integer_size = 4;
static unsigned ofc_target__real_size    = 4;
static unsigned ofc_target__pointer_size = sizeof(void*);

bool ofc_target_logical_size_set(unsigned size)
{
	if (size == 0)
		return false;
	ofc_target__logical_size = size;
	return true;
}

unsigned ofc_target_logical_size_get(void)
{
	return ofc_target__logical_size;
}


bool ofc_target_integer_size_set(unsigned size)
{
	if (size == 0)
		return false;
	ofc_target__integer_size = size;
	return true;
}

unsigned ofc_target_integer_size_get(void)
{
	return ofc_target__integer_size;
}


bool ofc_target_real_size_set(unsigned size)
{
	if (size == 0)
		return false;
	ofc_target__real_size = size;
	return true;
}

unsigned ofc_target_real_size_get(void)
{
	return ofc_target__real_size;
}

bool ofc_target_pointer_size_set(unsigned size)
{
	if (size == 0)
		return false;
	ofc_target__pointer_size = size;
	return true;
}

unsigned ofc_target_pointer_size_get(void)
{
	return ofc_target__pointer_size;
}
