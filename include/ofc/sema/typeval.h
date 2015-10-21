#ifndef __ofc_sema_typeval_h__
#define __ofc_sema_typeval_h__

typedef struct
{
    const ofc_sema_type_t* type;

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

ofc_sema_typeval_t* ofc_sema_typeval_literal(
	const ofc_parse_literal_t* literal,
	const ofc_sema_type_t* type);
void ofc_sema_typeval_delete(
	ofc_sema_typeval_t* typeval);

ofc_sema_typeval_t* ofc_sema_typeval_copy(
	const ofc_sema_typeval_t* typeval);

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

#endif
