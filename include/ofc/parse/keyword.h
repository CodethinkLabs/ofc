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

#ifndef __ofc_parse_keyword_h__
#define __ofc_parse_keyword_h__

typedef enum
{
	OFC_PARSE_KEYWORD_INCLUDE = 0,
	OFC_PARSE_KEYWORD_USE,
	OFC_PARSE_KEYWORD_ONLY,

	OFC_PARSE_KEYWORD_PROGRAM,
	OFC_PARSE_KEYWORD_SUBROUTINE,
	OFC_PARSE_KEYWORD_FUNCTION,
	OFC_PARSE_KEYWORD_MODULE,
	OFC_PARSE_KEYWORD_BLOCK_DATA,

	OFC_PARSE_KEYWORD_CONTAINS,

	OFC_PARSE_KEYWORD_STRUCTURE,
	OFC_PARSE_KEYWORD_UNION,
	OFC_PARSE_KEYWORD_MAP,
	OFC_PARSE_KEYWORD_RECORD,
	OFC_PARSE_KEYWORD_SEQUENCE,

	OFC_PARSE_KEYWORD_IF,
	OFC_PARSE_KEYWORD_THEN,
	OFC_PARSE_KEYWORD_ELSE,
	OFC_PARSE_KEYWORD_GO_TO,
	OFC_PARSE_KEYWORD_DO,
	OFC_PARSE_KEYWORD_WHILE,
	OFC_PARSE_KEYWORD_SELECT,
	OFC_PARSE_KEYWORD_CASE,
	OFC_PARSE_KEYWORD_DEFAULT,
	OFC_PARSE_KEYWORD_CONTINUE,
	OFC_PARSE_KEYWORD_STOP,
	OFC_PARSE_KEYWORD_PAUSE,
	OFC_PARSE_KEYWORD_CYCLE,
	OFC_PARSE_KEYWORD_EXIT,

	OFC_PARSE_KEYWORD_LOGICAL,
	OFC_PARSE_KEYWORD_CHARACTER,
	OFC_PARSE_KEYWORD_INTEGER,
	OFC_PARSE_KEYWORD_REAL,
	OFC_PARSE_KEYWORD_COMPLEX,
	OFC_PARSE_KEYWORD_BYTE,
	OFC_PARSE_KEYWORD_DOUBLE_PRECISION,
	OFC_PARSE_KEYWORD_DOUBLE_COMPLEX,

	OFC_PARSE_KEYWORD_TRUE,
	OFC_PARSE_KEYWORD_FALSE,

	OFC_PARSE_KEYWORD_IMPLICIT,
	OFC_PARSE_KEYWORD_IMPLICIT_NONE,
	OFC_PARSE_KEYWORD_UNDEFINED,

	OFC_PARSE_KEYWORD_COMMON,
	OFC_PARSE_KEYWORD_NAMELIST,
	OFC_PARSE_KEYWORD_DIMENSION,
	OFC_PARSE_KEYWORD_VIRTUAL,
	OFC_PARSE_KEYWORD_EQUIVALENCE,

	OFC_PARSE_KEYWORD_KIND,

	OFC_PARSE_KEYWORD_ASSIGN,
	OFC_PARSE_KEYWORD_TO,

	OFC_PARSE_KEYWORD_CALL,
	OFC_PARSE_KEYWORD_ENTRY,
	OFC_PARSE_KEYWORD_RETURN,

	OFC_PARSE_KEYWORD_EXTERNAL,
	OFC_PARSE_KEYWORD_INTRINSIC,
	OFC_PARSE_KEYWORD_AUTOMATIC,
	OFC_PARSE_KEYWORD_STATIC,
	OFC_PARSE_KEYWORD_VOLATILE,
	OFC_PARSE_KEYWORD_POINTER,

	OFC_PARSE_KEYWORD_DATA,
	OFC_PARSE_KEYWORD_PARAMETER,
	OFC_PARSE_KEYWORD_SAVE,

	OFC_PARSE_KEYWORD_PUBLIC,
	OFC_PARSE_KEYWORD_PRIVATE,

	OFC_PARSE_KEYWORD_FORMAT,

	OFC_PARSE_KEYWORD_OPEN,
	OFC_PARSE_KEYWORD_INQUIRE,
	OFC_PARSE_KEYWORD_REWIND,
	OFC_PARSE_KEYWORD_BACKSPACE,
	OFC_PARSE_KEYWORD_READ,
	OFC_PARSE_KEYWORD_WRITE,
	OFC_PARSE_KEYWORD_END_FILE,
	OFC_PARSE_KEYWORD_CLOSE,
	OFC_PARSE_KEYWORD_PRINT,
	OFC_PARSE_KEYWORD_TYPE,
	OFC_PARSE_KEYWORD_ENCODE,
	OFC_PARSE_KEYWORD_DECODE,
	OFC_PARSE_KEYWORD_ACCEPT,
	OFC_PARSE_KEYWORD_DEFINE_FILE,

	OFC_PARSE_KEYWORD_RESHAPE,

	OFC_PARSE_KEYWORD_COUNT
} ofc_parse_keyword_e;


bool ofc_sparse_ref_begins_with_keyword(
	ofc_sparse_ref_t ref, bool* space, bool* is);

unsigned ofc_parse_ident(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_sparse_ref_t* ident);

unsigned ofc_parse_name(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_sparse_ref_t* name);
ofc_sparse_ref_t* ofc_parse_name_alloc(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	unsigned* len);


const char* ofc_parse_keyword_name(
	ofc_parse_keyword_e keyword);

unsigned ofc_parse_keyword(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword);

unsigned ofc_parse_keyword_named(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword,
	ofc_sparse_ref_t* name);

unsigned ofc_parse_keyword_end(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword, bool force);

unsigned ofc_parse_keyword_end_named(
	const ofc_sparse_t* src, const char* ptr,
	ofc_parse_debug_t* debug,
	ofc_parse_keyword_e keyword, bool force,
	ofc_sparse_ref_t* name);

#endif
