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

#ifndef __ofc_sema_module_h__
#define __ofc_sema_module_h__

struct ofc_sema_module_s
{
	ofc_sema_scope_t* scope;

	ofc_sema_decl_alias_map_t* rename;
	ofc_sema_decl_list_t*      only;
} ;

struct ofc_sema_module_list_s
{
	unsigned 			count;
	ofc_sema_module_t** module;
};



#endif
