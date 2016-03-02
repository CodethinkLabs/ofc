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

#ifndef __ofc_sema_kind_h__
#define __ofc_sema_kind_h__

/* We follow the old GNU fortran KIND notation:
	http://gcc.gnu.org/onlinedocs/gcc-3.4.4/g77/Kind-Notation.html */

typedef enum
{
	OFC_SEMA_KIND_NONE    =  0,
	OFC_SEMA_KIND_DEFAULT =  1,
	OFC_SEMA_KIND_DOUBLE  =  2,
	OFC_SEMA_KIND_1_BYTE  =  3,
	OFC_SEMA_KIND_QUAD    =  4,
	OFC_SEMA_KIND_HALF    =  5,
	OFC_SEMA_KIND_2_BYTE  =  6,
	OFC_SEMA_KIND_POINTER =  7,
	OFC_SEMA_KIND_4_BYTE  = 12,
	OFC_SEMA_KIND_8_BYTE  = 24,
	OFC_SEMA_KIND_QUARTER = 25,
	OFC_SEMA_KIND_10_BYTE = 30,
	OFC_SEMA_KIND_16_BYTE = 48,
} ofc_sema_kind_e;

#endif
