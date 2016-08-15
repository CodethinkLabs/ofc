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

#ifndef __ofc_sema_typeval_h__
#define __ofc_sema_typeval_h__

typedef struct
{
    const ofc_sema_type_t* type;

	ofc_sparse_ref_t src;

	struct
	{
		bool        logical;
		int64_t     integer;
		long double real;

		struct
		{
			long double real;
			long double imaginary;
		} complex;

		char* character;
	};
} ofc_sema_typeval_t;


ofc_sema_typeval_t* ofc_sema_typeval_create_logical(
	bool value, ofc_sema_kind_e kind,
	ofc_sparse_ref_t ref);
ofc_sema_typeval_t* ofc_sema_typeval_create_integer(
	int value, ofc_sema_kind_e kind,
	ofc_sparse_ref_t ref);
ofc_sema_typeval_t* ofc_sema_typeval_create_real(
	long double value, ofc_sema_kind_e kind,
	ofc_sparse_ref_t ref);
ofc_sema_typeval_t* ofc_sema_typeval_create_complex(
	long double real, long double imaginary,
	ofc_sema_kind_e kind,
	ofc_sparse_ref_t ref);
ofc_sema_typeval_t* ofc_sema_typeval_create_character(
	const char* data, ofc_sema_kind_e kind, unsigned len,
	ofc_sparse_ref_t ref);

ofc_sema_typeval_t* ofc_sema_typeval_literal(
	const ofc_parse_literal_t* literal,
	const ofc_sema_type_t* type);
void ofc_sema_typeval_delete(
	ofc_sema_typeval_t* typeval);

bool ofc_sema_typeval_compare(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);

bool ofc_sema_typeval_is_one(
	const ofc_sema_typeval_t* typeval);

unsigned ofc_sema_typeval_size(
	const ofc_sema_typeval_t* typeval);

ofc_sema_typeval_t* ofc_sema_typeval_copy(
	const ofc_sema_typeval_t* typeval);
ofc_sema_typeval_t* ofc_sema_typeval_cast(
	const ofc_sema_typeval_t* typeval,
	const ofc_sema_type_t* type);

bool ofc_sema_typeval_get_logical(
	const ofc_sema_typeval_t* typeval,
	bool* logical);
bool ofc_sema_typeval_get_integer(
	const ofc_sema_typeval_t* typeval,
	int64_t* integer);
bool ofc_sema_typeval_get_real(
	const ofc_sema_typeval_t* typeval,
	long double* real);
bool ofc_sema_typeval_get_complex(
	const ofc_sema_typeval_t* typeval,
	long double* real, long double* imaginary);
bool ofc_sema_typeval_get_character(
	const ofc_sema_typeval_t* typeval,
	const char** character);

bool ofc_typeval_character_equal_strz(
const ofc_sema_typeval_t* tv, const char* strz);
bool ofc_typeval_character_equal_strz_ci(
const ofc_sema_typeval_t* tv, const char* strz);


ofc_sema_typeval_t* ofc_sema_typeval_power(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_multiply(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_concat(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_divide(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_add(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_subtract(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_negate(
	const ofc_sema_typeval_t* a);
ofc_sema_typeval_t* ofc_sema_typeval_eq(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_ne(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_lt(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_le(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_gt(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_ge(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_not(
	const ofc_sema_typeval_t* a);
ofc_sema_typeval_t* ofc_sema_typeval_and(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_or(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_xor(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_eqv(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_neqv(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);

bool ofc_sema_typeval_can_print(
	const ofc_sema_typeval_t* typeval);

bool ofc_sema_typeval_print(ofc_colstr_t*cs,
	const ofc_sema_typeval_t* typeval);

#endif
