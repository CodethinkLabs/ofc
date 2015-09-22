#ifndef __parse_format_h__
#define __parse_format_h__

typedef enum
{
	PARSE_FORMAT_DESC_LITERAL    = '\'',
	PARSE_FORMAT_DESC_INTEGER    = 'I',
	PARSE_FORMAT_DESC_REAL       = 'F',
	PARSE_FORMAT_DESC_D          = 'D',
	PARSE_FORMAT_DESC_E          = 'E',
	PARSE_FORMAT_DESC_G          = 'G',
	PARSE_FORMAT_DESC_CHARACTER  = 'A',
	PARSE_FORMAT_DESC_LOGICAL    = 'L',
	PARSE_FORMAT_DESC_HOLLERITH  = 'H',
	PARSE_FORMAT_DESC_S          = 'S',
	PARSE_FORMAT_DESC_REAL_SCALE = 'P',
	PARSE_FORMAT_DESC_X          = 'X',
	PARSE_FORMAT_DESC_T          = 'T',
	PARSE_FORMAT_DESC_SLASH      = '/',
	PARSE_FORMAT_DESC_BN,
	PARSE_FORMAT_DESC_BZ,
	PARSE_FORMAT_DESC_SP,
	PARSE_FORMAT_DESC_SS,
	PARSE_FORMAT_DESC_TL,
	PARSE_FORMAT_DESC_TR,
	PARSE_FORMAT_DESC_STRING,
	PARSE_FORMAT_DESC_REPEAT,
} parse_format_desc_e;

typedef struct parse_format_desc_s parse_format_desc_t;

struct parse_format_desc_s
{
	parse_format_desc_e type;

	bool neg;
	unsigned n;
	union
	{
		struct
		{
			unsigned w, d, e;
		};

		string_t string;

		struct
		{
			unsigned             count;
			parse_format_desc_t* list;
		} repeat;
	};
};


unsigned parse_format_desc(
	const sparse_t* src, const char* ptr,
	parse_format_desc_t* desc);

void parse_format_desc_cleanup(
	parse_format_desc_t desc);


unsigned parse_format_desc_list(
	const sparse_t* src, const char* ptr,
	char terminator,
	parse_format_desc_t** list, unsigned* count);

void parse_format_desc_list_delete(
	parse_format_desc_t* list, unsigned count);

#endif

