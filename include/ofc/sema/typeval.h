#ifndef __ofc_sema_typeval_h__
#define __ofc_sema_typeval_h__

typedef struct
{
    const ofc_sema_type_t* type;

	ofc_str_ref_t src;

	union
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

ofc_sema_typeval_t* ofc_sema_typeval_unsigned(
	unsigned value, ofc_str_ref_t ref);
ofc_sema_typeval_t* ofc_sema_typeval_literal(
	const ofc_sema_scope_t* scope,
	const ofc_parse_literal_t* literal,
	const ofc_sema_type_t* type);
void ofc_sema_typeval_delete(
	ofc_sema_typeval_t* typeval);

bool ofc_sema_typeval_compare(
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);

ofc_sema_typeval_t* ofc_sema_typeval_copy(
	const ofc_sema_typeval_t* typeval);
ofc_sema_typeval_t* ofc_sema_typeval_cast(
	const ofc_sema_scope_t* scope,
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
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_multiply(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_concat(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_divide(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_add(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_subtract(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_negate(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a);
ofc_sema_typeval_t* ofc_sema_typeval_eq(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_ne(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_lt(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_le(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_gt(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_ge(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_not(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a);
ofc_sema_typeval_t* ofc_sema_typeval_and(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_or(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_eqv(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);
ofc_sema_typeval_t* ofc_sema_typeval_neqv(
	const ofc_sema_scope_t* scope,
	const ofc_sema_typeval_t* a,
	const ofc_sema_typeval_t* b);

bool ofc_sema_typeval_print(ofc_colstr_t*cs,
	const ofc_sema_typeval_t* typeval);

#endif
