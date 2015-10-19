#ifndef __ofc_sema_typeval_h__
#define __ofc_sema_typeval_h__

typedef struct
{
    const ofc_sema_type_t* type;

	union
	__attribute__((__packed__))
	{
		bool        logical;
		int64_t     integer;
		long double real;

		struct
		{
			long double real;
			long double imag;
		} complex;

		char* character;
	};
} ofc_sema_typeval_t;

ofc_sema_typeval_t* ofc_sema_typeval_literal(
	const ofc_parse_literal_t* literal,
	const ofc_sema_type_t* type);
void ofc_sema_typeval_delete(
	ofc_sema_typeval_t* typeval);

bool ofc_sema_typeval_get(
	const ofc_sema_typeval_t* typeval,
	const ofc_sema_type_t* type,
	void* value);

#endif
