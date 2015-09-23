#ifndef __parse_record_h__
#define __parse_record_h__

typedef struct
{
	str_ref_t structure;
	str_ref_t name;
} parse_record_t;

typedef struct
{
	unsigned         count;
	parse_record_t** record;
} parse_record_list_t;

parse_record_list_t* parse_record_list(
	const sparse_t* src, const char* ptr,
	unsigned* len);
void parse_record_list_delete(
	parse_record_list_t* list);
bool parse_record_list_print(
	int fd, const parse_record_list_t* list);

#endif
