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

#ifndef __ofc_parse_operator_h__
#define __ofc_parse_operator_h__

typedef enum
{
	OFC_PARSE_OPERATOR_POWER,
	OFC_PARSE_OPERATOR_MULTIPLY,
	OFC_PARSE_OPERATOR_CONCAT,
	OFC_PARSE_OPERATOR_DIVIDE,
	OFC_PARSE_OPERATOR_ADD,
	OFC_PARSE_OPERATOR_SUBTRACT,
	OFC_PARSE_OPERATOR_EQ,
	OFC_PARSE_OPERATOR_NE,
	OFC_PARSE_OPERATOR_LT,
	OFC_PARSE_OPERATOR_LE,
	OFC_PARSE_OPERATOR_GT,
	OFC_PARSE_OPERATOR_GE,
	OFC_PARSE_OPERATOR_NOT,
	OFC_PARSE_OPERATOR_AND,
	OFC_PARSE_OPERATOR_OR,
	OFC_PARSE_OPERATOR_XOR,
	OFC_PARSE_OPERATOR_EQV,
	OFC_PARSE_OPERATOR_NEQV,

	OFC_PARSE_OPERATOR_COUNT
} ofc_parse_operator_e;


static const unsigned OPERATOR_PRECEDENCE_MAX = 11;

const char* ofc_parse_operator_str_rep(
	const ofc_parse_operator_e operator);

unsigned ofc_parse_operator(
	const ofc_sparse_t* sparse, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_operator_e* operator);

bool ofc_parse_operator_unary(
	ofc_parse_operator_e operator);
bool ofc_parse_operator_binary(
	ofc_parse_operator_e operator);

unsigned ofc_parse_operator_precedence_unary(
	ofc_parse_operator_e operator);
unsigned ofc_parse_operator_precedence_binary(
	ofc_parse_operator_e operator);

bool ofc_parse_operator_print(
	ofc_colstr_t* cs, const ofc_parse_operator_e operator);

#endif
