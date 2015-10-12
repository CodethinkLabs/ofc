#ifndef __ofc_lang_opts_h__
#define __ofc_lang_opts_h__

#include <stdbool.h>

typedef enum
{
	OFC_LANG_FORM_FIXED = 0,
	OFC_LANG_FORM_FREE,
	OFC_LANG_FORM_TAB,
} ofc_lang_form_e;

typedef struct
{
	ofc_lang_form_e form;
	unsigned        tab_width;
	bool            debug;
	unsigned        columns;
	bool            case_sensitive;
} ofc_lang_opts_t;

static const ofc_lang_opts_t OFC_LANG_OPTS_F77 =
{
	.form           = OFC_LANG_FORM_FIXED,
	.tab_width      = 8,
	.debug          = false,
	.columns        = 72,
	.case_sensitive = false,
};

static const ofc_lang_opts_t OFC_LANG_OPTS_F77_TAB =
{
	.form           = OFC_LANG_FORM_TAB,
	.tab_width      = 1,
	.debug          = false,
	.columns        = 72,
	.case_sensitive = false,
};

static const ofc_lang_opts_t OFC_LANG_OPTS_F90 =
{
	.form           = OFC_LANG_FORM_FREE,
	.tab_width      = 8,
	.debug          = false,
	.columns        = 132,
	.case_sensitive = false,
};

#endif
