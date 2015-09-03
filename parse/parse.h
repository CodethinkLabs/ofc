#ifndef __parse_h__
#define __parse_h__

typedef enum
{
	PARSE_TYPE_NAME,
	PARSE_TYPE_OP,

	PARSE_TYPE_BINARY_CONSTANT,
	PARSE_TYPE_OCTAL_CONSTANT,
	PARSE_TYPE_HEX_CONSTANT,

	PARSE_TYPE_LOGICAL_LITERAL,
	PARSE_TYPE_SIGNED_INT_LITERAL_CONSTANT,
	PARSE_TYPE_INT_LITERAL_CONSTANT,

} parse_type_e;

/* TODO - Diverge from spec and specify operator here. */
typedef enum
{
	PARSE_OP_POWER,
	PARSE_OP_MULT,
	PARSE_OP_ADD,
	PARSE_OP_CONCAT,
	PARSE_OP_REL,
	PARSE_OP_NOT,
	PARSE_OP_AND,
	PARSE_OP_OR,
	PARSE_OP_EQV,
	PARSE_OP_NEQV,
} parse_op_e;

typedef struct parse_s parse_t;

typedef struct
{
	parse_t name;
	parse_t specification_part;
	parse_t execution_part;
	parse_t internal_subprogram_part;
} parse_program_t

struct parse_s
{
	parse_type_e type;

	union
	{
		struct
		{
			const char* base;
			unsigned    size;
		} name;

		parse_op_e op;

		parse_program_t* program;
	};
};

unsigned parse_executable_program(file_t* file, unsigned line, unsigned offset);
unsigned parse_program_unit(file_t* file, unsigned line, unsigned offset);
unsigned parse_main_program(file_t* file, unsigned line, unsigned offset);
unsigned parse_external_subprogram(file_t* file, unsigned line, unsigned offset);
unsigned parse_function_subprogram(file_t* file, unsigned line, unsigned offset);
unsigned parse_subroutine_subprogram(file_t* file, unsigned line, unsigned offset);
unsigned parse_module(file_t* file, unsigned line, unsigned offset);
unsigned parse_block_data(file_t* file, unsigned line, unsigned offset);
unsigned parse_specification_part(file_t* file, unsigned line, unsigned offset);
unsigned parse_implicit_part(file_t* file, unsigned line, unsigned offset);
unsigned parse_implicit_part_stmt(file_t* file, unsigned line, unsigned offset);
unsigned parse_declaration_construct(file_t* file, unsigned line, unsigned offset);
unsigned parse_execution_part(file_t* file, unsigned line, unsigned offset);
unsigned parse_execution_part_construct(file_t* file, unsigned line, unsigned offset);
unsigned parse_internal_subprogram_part(file_t* file, unsigned line, unsigned offset);
unsigned parse_internal_subprogram(file_t* file, unsigned line, unsigned offset);
unsigned parse_module_subprogram_part(file_t* file, unsigned line, unsigned offset);
unsigned parse_module_subprogram(file_t* file, unsigned line, unsigned offset);

/* Returns number of characters read, can't cross line boundaries. */
unsigned parse_program(const char* src, parse_t* entry);


unsigned parse_name(const char* src, parse_t* entry);
unsigned parse_op(const char* src, parse_t* entry);

unsigned parse_binary_constant(const char* src, parse_t* entry);
unsigned parse_octal_constant(const char* src, parse_t* entry);
unsigned parse_hex_constant(const char* src, parse_t* entry);
unsigned parse_boz_literal_constant(const char* src, parse_t* entry);

unsigned parse_constant(file_t* file, unsigned line, unsigned offset);
unsigned parse_literal_constant(file_t* file, unsigned line, unsigned offset);
unsigned parse_named_constant(file_t* file, unsigned line, unsigned offset);
unsigned parse_int_constant(file_t* file, unsigned line, unsigned offset);
unsigned parse_char_constant(file_t* file, unsigned line, unsigned offset);


#endif
