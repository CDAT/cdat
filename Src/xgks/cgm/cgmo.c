/*
 * This file implements an XGKS output Metafile (MO) workstation using the 
 * Computer Graphics Metafile (CGM) standard.
 *
 * This module will only work on platforms having 8-bit "char"s.
 * This requirement is tested at runtime by this module.
 */

/*LINTLIBRARY*/

/* POSIX includes: */
#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <sys/types.h>		/* for uid_t */
#include <stdio.h>		/* for *printf(), ftell(), & fseek() */
#include <stddef.h>		/* for offsetof() */
#include <stdlib.h>
#include <time.h>		/* for time(), localtime(), gmtime(), & 
				 * strftime() */
#include <sys/utsname.h>	/* for uname() */
#include <unistd.h>		/* for getlogin() */
#include <string.h>
#include <math.h>		/* for sqrt() */
#include <assert.h>
#include <limits.h>		/* for CHAR_BIT */
#include "gks_implem.h"
#include "cgm.h"		/* for public, API details */
#include "cgm_implem.h"		/* for implementation details */

#ifndef lint
    static char rcsid[]	= "$Id$";
#endif

/*
 * CGM output-routines that are simple enough to be implemented as macros:
 */
#define mo_lineindex(cgmo, num)	\
    mo_element(cgmo, num, ATTRIBUTE_CL, LINEINDEX_ID, \
		mo_indexes, offsetof(mf_cgmo, lineindex), 1)
#define mo_linetype(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, LINETYPE_ID, \
		mo_indexes, offsetof(mf_cgmo, linetype), 1)
#define mo_linewidth(cgmo, num)	\
    mo_element(cgmo, num, ATTRIBUTE_CL, LINEWIDTH_ID, \
		mo_reals, offsetof(mf_cgmo, linewidth), 1)
#define mo_linecolr(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, LINECOLR_ID, \
		mo_indexed_colors, offsetof(mf_cgmo, linecolr), 1)
#define mo_markerindex(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, MARKERINDEX_ID, \
		mo_indexes, offsetof(mf_cgmo, markerindex), 1)
#define mo_markertype(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, MARKERTYPE_ID, \
		mo_indexes, offsetof(mf_cgmo, markertype), 1)
#define mo_markersize(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, MARKERSIZE_ID, \
		mo_reals, offsetof(mf_cgmo, markersize), 1)
#define mo_markercolr(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, MARKERCOLR_ID, \
		mo_indexed_colors, offsetof(mf_cgmo, markercolr), 1)
#define mo_textindex(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, TEXTINDEX_ID, \
		mo_indexes, offsetof(mf_cgmo, textindex), 1)
#define mo_textfontindex(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, \
		TEXTFONTINDEX_ID, mo_indexes, offsetof(mf_cgmo, txfp.font), 1)
#define mo_textprec(cgmo, num)	\
    mo_element(cgmo, num, ATTRIBUTE_CL, TEXTPREC_ID, \
		mo_enums, offsetof(mf_cgmo, txfp.prec), 1)
#define mo_charexpan(cgmo, num)	\
    mo_element(cgmo, num, ATTRIBUTE_CL, CHAREXPAN_ID, \
		mo_reals, offsetof(mf_cgmo, charexpan), 1)
#define mo_charspace(cgmo, num)	\
    mo_element(cgmo, num, ATTRIBUTE_CL, CHARSPACE_ID, \
		mo_reals, offsetof(mf_cgmo, charspace), 1)
#define mo_textcolr(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, TEXTCOLR_ID, \
		mo_indexed_colors, offsetof(mf_cgmo, textcolr), 1)
#define mo_charheight(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, CHARHEIGHT_ID, \
		mo_vdcs, offsetof(mf_cgmo, charheight), 1)
#define mo_charori(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, CHARORI_ID, \
		mo_vdcs, offsetof(mf_cgmo, charori[0]), 4)
#define mo_textpath(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, TEXTPATH_ID, \
		mo_enums, offsetof(mf_cgmo, textpath), 1)
#define mo_fillindex(cgmo, num)	\
    mo_element(cgmo, num, ATTRIBUTE_CL, FILLINDEX_ID, \
		mo_indexes, offsetof(mf_cgmo, fillindex), 1)
#define mo_intstyle(cgmo, num)	\
    mo_element(cgmo, num, ATTRIBUTE_CL, INTSTYLE_ID, \
		mo_enums, offsetof(mf_cgmo, intstyle), 1)
#define mo_fillcolr(cgmo, num)	\
    mo_element(cgmo, num, ATTRIBUTE_CL, FILLCOLR_ID, \
		mo_indexed_colors, offsetof(mf_cgmo, fillcolr), 1)
#define mo_hatchindex(cgmo, num) \
    mo_element(cgmo, num, ATTRIBUTE_CL, HATCHINDEX_ID, \
		mo_indexes, offsetof(mf_cgmo, hatchindex), 1)
#define mo_patindex(cgmo, num)	\
    mo_element(cgmo, num, ATTRIBUTE_CL, PATINDEX_ID, \
		mo_indexes, offsetof(mf_cgmo, patindex), 1)

#define MO_BEGMF(cgmo, num)		GKS_STMT( \
	mo_header(DELIMITER_CL, BEGMF_ID); \
	mo_string(cgmo, num, "XGKS CGM"); \
	mo_flush(cgmo, num, 0); \
	mo_mode(cgmo, num, CGMO_IN_METAFILE); \
    )
#define MO_MFVERSION(cgmo, num)	GKS_STMT( \
	mo_header(MF_DESCRIPTOR_CL, MFVERSION_ID); \
	mo_int(cgmo, num, MFVERSION); \
	mo_flush(cgmo, num, 0); \
    )
#define MO_MFELEMLIST(cgmo, num)	GKS_STMT( \
	mo_header(MF_DESCRIPTOR_CL, MFELEMLIST_ID); \
	mo_int(cgmo, num, num_elements); \
	mo_ints(cgmo, num, (char*)mf_elements_list, 2*num_elements); \
	mo_flush(cgmo, num, 0); \
    )
#define MO_BEGPICBODY(cgmo, num)	GKS_STMT( \
	mo_header(DELIMITER_CL, BEGPICBODY_ID); \
	mo_flush(cgmo, num, 0); \
	mo_mode(cgmo, num, CGMO_IN_BODY); \
    )
#define MO_ENDPIC(cgmo, num)		GKS_STMT( \
	mo_header(DELIMITER_CL, ENDPIC_ID); \
	mo_flush(cgmo, num, 0); \
	mo_mode(cgmo, num, CGMO_IN_METAFILE); \
    )
#define MO_ENDMF(cgmo, num)		GKS_STMT( \
	mo_header(DELIMITER_CL, ENDMF_ID); \
	mo_flush(cgmo, num, 0); \
	mo_mode(cgmo, num, CGMO_UNSET); \
    )

/*
 * Metafile element-list (for the METAFILE DESCRIPTION command):
 */
#define DRAWING_SET_CL	-1
#define DRAWING_SET_ID	 0
static int	mf_elements_list[][2]	= {DRAWING_SET_CL, DRAWING_SET_ID};
static int	num_elements		= NUM_ELEMENTS(mf_elements_list);

/*
 * I/O buffer.  Though it's defined here, it's also used for CGM input.
 */
unsigned char		cgm_buf[077776];

/*
 * Output CGM state variables:
 */
static int		header_class;	/* Item class */
static int		header_id;	/* Item identifier */
static int		header_written;	/* Command-header has been written? */
static unsigned char	*mo_ptr	= cgm_buf;
					/* Next available byte in buffer */
static unsigned char	*mo_outp	= cgm_buf + sizeof(cgm_buf);
					/* End-of-buffer pointer sentinel */


/*
 * Save header information.
 */
#define mo_header(class, id)	GKS_STMT( \
	assert(!header_written); \
	header_class	= class; \
	header_id	= id; \
    )


/*
 * Flush the (complete or partial) CGM-element parameter-buffer.
 */
    static void
mo_flush(cgmo, num, more_data)
    mf_cgmo	**cgmo;
    int		num;
    int		more_data;
{
    if (num > 0) {
	int		imf;
	int		pad;
	int		nbytes	= mo_ptr - cgm_buf;
	static int	long_form;

	if (!header_written) {
	    unsigned short	header;
	    unsigned char	bytes[2];

	    long_form	= nbytes > MAX_SHORT_CMD_LENGTH;
	    header	= header_class << 12
			     | header_id << 5 
			     | (long_form
				    ? LONG_CMD_LENGTH
				    : nbytes);
	    HTON16(header, bytes);

	    for (imf = 0; imf < num; ++imf)
		(void) fwrite((voidp)bytes, (size_t)1, (size_t)2, 
			      cgmo[imf]->fp);
	    header_written	= 1;
	}

	if (long_form) {
	    /*
	     * Long-form command.  Write a partition control word.
	     */
	    unsigned short	pcw	= more_data
						? MORE_DATA_BIT | nbytes 
						: nbytes;
	    unsigned char	bytes[2];

	    HTON16(pcw, bytes);

	    for (imf = 0; imf < num; ++imf)
		(void) fwrite((voidp)bytes, (size_t)1, (size_t)2, 
			      cgmo[imf]->fp);
	}

	/* Pad the data if necessary */
	pad	= nbytes % 2;
	if (pad) {
	    assert(!more_data);		/* padding shall only occur at end of
					 * command element and not at the end
					 * of a non-terminal data-partition 
					 * (except for cell arrays, but that's
					 * taken care of before here) */
	    *mo_ptr	= 0;
	}

	/* Write the data. */
	for (imf = 0; imf < num; ++imf)
	    (void) fwrite((voidp)cgm_buf, (size_t)1, (size_t)(nbytes+pad), 
			  cgmo[imf]->fp);

	/* Reset the buffer pointer */
	mo_ptr	= cgm_buf;

	/* Reset the element state if appropriate */
	if (!more_data)
	    header_written	= 0;
    }
}

#define size_header()		(size_t)2


/*
 * Write a parameter octet.
 */
#define mo_octet(cgmo, num, byte)	GKS_STMT( \
	*mo_ptr++	= byte; \
	if (mo_ptr == mo_outp) \
	    mo_flush(cgmo, num, 1); \
    )


/*
 * Write parameter octets.
 */
    static void
mo_octets(cgmo, num, bytes, nbytes)
    mf_cgmo		**cgmo;
    int			num;
    unsigned char	*bytes;
    size_t		nbytes;
{
    while (nbytes--)
	mo_octet(cgmo, num, *bytes++);
}


/*
 * Write a 16-bit, integer parameter.
 */
#define mo_int16(cgmo, num, val)	GKS_STMT( \
	unsigned short 	value	= val; \
	unsigned char	bytes[2]; \
	HTON16(value, bytes); \
	mo_octets(cgmo, num, bytes, 2); \
    )


/*
 * Write 16-bit, integer parameters.
 */
    static void
mo_int16s(cgmo, num, ptr, nval)
    mf_cgmo	**cgmo;
    int		num;
    char	*ptr;
    int		nval;
{
    int		*vals	= (int*)ptr;

    assert(nval >= 0);

    while (nval-- > 0)
	mo_int16(cgmo, num, *vals++);
}


/*
 * Write an integer parameter.
 */
#define mo_int(mo, n, val) 	mo_int16(mo, n, val)

#define size_int()		size_int16()


/*
 * Write integer parameters.
 */
    static void
mo_ints(cgmo, num, ptr, nval)
    mf_cgmo	**cgmo;
    int		num;
    char	*ptr;
    int		nval;
{
    int		*vals	= (int*)ptr;

    assert(nval >= 0);

    while (nval-- > 0)
	mo_int(cgmo, num, *vals++);
}


/*
 * Write a 16-bit, unsigned integer parameter.
 */
#define mo_uint16(cgmo, num, val)	GKS_STMT( \
	unsigned short 	value	= val; \
	unsigned char	bytes[2]; \
	HTON16(value, bytes); \
	mo_octets(cgmo, num, bytes, 2); \
    )


/*
 * Write an index parameter.
 */
#define mo_index(mo, n, val)	mo_int16(mo, n, val)

#define size_index()		size_int()


/*
 * Write a color index parameter.
 */
#define mo_color_index(mo, n, val)	mo_octet(mo, n, val)

#define size_color_index()		(size_t)1


/*
 * Write index parameters.
 */
    static void
mo_indexes(cgmo, num, ptr, nval)
    mf_cgmo	**cgmo;
    int		num;
    char	*ptr;
    int		nval;
{
    int		*vals	= (int*)ptr;

    assert(nval >= 0);

    while (nval-- > 0)
	mo_index(cgmo, num, *vals++);
}


/*
 * Write an ennumeration parameter.
 */
#define mo_enum(mo, n, val)	mo_int16(mo, n, (int)val)

#define mo_enums		mo_int16s

#define size_enum()		size_int16()


/*
 * Write a string parameter.
 */
    static void
mo_string(cgmo, num, string)
    mf_cgmo	**cgmo;
    int		num;
    char	*string;
{
    size_t	nchr	= strlen(string);

    if (nchr <= MAX_SHORT_STR_LENGTH) {
	mo_octet(cgmo, num, nchr);
	mo_octets(cgmo, num, (unsigned char*)string, nchr);
    } else {
	unsigned	length;

	mo_octet(cgmo, num, LONG_STR_LENGTH);

	while (nchr > 0) {
	    length	= MIN(32767, nchr);
	    nchr	-= length;
	    if (nchr > 0)
		length	|= MSB_16;
	    mo_uint16(cgmo, num, length);
	    mo_octets(cgmo, num, (unsigned char*)string, (size_t)length);
	    string	+= length;
	}
    }
}


/*
 * Write a 32-bit, fixed-point, real parameter.
 */
#define mo_real32fx(cgmo, num, val)		GKS_STMT( \
	mf_cgmo		**mo	= cgmo; \
	int		nmo	= num; \
	double		x	= val; \
	short		swhole	= x < 0 ? x - 1 : x; \
	unsigned short	whole	= HTOTS(swhole); \
	unsigned short	frac	= UNORM_TO_INT16(x-swhole); \
	unsigned char	bytes[2]; \
	HTON16(whole, bytes); \
	mo_octets(mo, nmo, bytes, 2); \
	HTON16(frac, bytes); \
	mo_octets(mo, nmo, bytes, 2); \
	assert(x-swhole >= 0); \
	assert(x-swhole <= 1); \
    )

#define size_real32fx()		(size_t)4


/*
 * Write a real parameter.
 */
#define mo_real(mo, n, val)	mo_real32fx(mo, n, val)

#define size_real()		size_real32fx()


/*
 * Write real parameters.
 */
    static void
mo_reals(cgmo, num, ptr, nval)
    mf_cgmo	**cgmo;
    int		num;
    char	*ptr;
    int		nval;
{
    float	*vals	= (float*)ptr;

    assert(nval >= 0);

    while (nval-- > 0)
	mo_real(cgmo, num, *vals++);
}


/*
 * Write a VDC parameter.
 */
#define MO_VDC(cgmo, num, val)	GKS_STMT( \
	int	x	= NORM_TO_INT16(val); \
	assert(val >= -1); \
	assert(val <= 1); \
	assert(size_vdc(*cgmo) == size_int16()); \
	mo_int16(cgmo, num, x); \
    )


/*
 * Write VDC parameters.
 */
    static void
mo_vdcs(cgmo, num, ptr, nval)
    mf_cgmo	**cgmo;
    int		num;
    char	*ptr;
    int		nval;
{
    Gfloat	*vals	= (Gfloat*)ptr;

    while (nval-- > 0) {
	Gfloat	val	= *vals++;

	MO_VDC(cgmo, num, val);
    }
}


/*
 * Write a point.
 */
#define MO_POINT(cgmo, num, ptr)	GKS_STMT( \
	MO_VDC(cgmo, num, (ptr)->x); \
	MO_VDC(cgmo, num, (ptr)->y); \
    )


/*
 * Write point parameters.
 */
    static void
mo_points(cgmo, num, ptr, nval)
    mf_cgmo	**cgmo;
    int		num;
    char	*ptr;
    int		nval;
{
    Gpoint	*vals	= (Gpoint*)ptr;

    while (nval-- > 0) {
	Gpoint	*pt	= vals++;

	MO_POINT(cgmo, num, pt);
    }
}


/*
 * Write a direct color parameter.
 *
 * NB: The default direct color precision is used.
 */
#define mo_direct_color(cgmo, num, ptr)		GKS_STMT( \
	Gcobundl	*modc_colr	= ptr; \
	unsigned char	modc_rep[3]; \
	assert(modc_colr->red >= 0); \
	assert(modc_colr->red <= 1); \
	assert(modc_colr->green >= 0); \
	assert(modc_colr->green <= 1); \
	assert(modc_colr->blue >= 0); \
	assert(modc_colr->blue <= 1); \
	modc_rep[0]	= UNORM_TO_INT8(modc_colr->red); \
	modc_rep[1]	= UNORM_TO_INT8(modc_colr->green); \
	modc_rep[2]	= UNORM_TO_INT8(modc_colr->blue); \
	mo_octets(cgmo, num, modc_rep, 3); \
    )


/*
 * Write direct color parameters.
 *
 * NB: The default direct color precision is used.
 */
    static void
mo_direct_colors(cgmo, num, ptr, ncolor)
    mf_cgmo	**cgmo;
    int		num;
    char	*ptr;
    int		ncolor;
{
    Gcobundl	*colors	= (Gcobundl*)ptr;

    while (ncolor-- > 0)
	mo_direct_color(cgmo, num, colors++);
}


/*
 * Write an indexed-color parameter.
 */
#define mo_indexed_color(cgmo, num, val)	GKS_STMT( \
	mo_octet(cgmo, num, val); \
    )

#define size_indexed_color()	(size_t)1


/*
 * Write indexed-color parameters.
 */
    static void
mo_indexed_colors(cgmo, num, ptr, ncolor)
    mf_cgmo	**cgmo;
    int		num;
    char	*ptr;
    int		ncolor;
{
    int		*colors	= (int*)ptr;

    while (ncolor-- > 0)
	mo_indexed_color(cgmo, num, *colors++);
}


/*
 * Write a row of a cell-array to output CGM elements.
 */
    static void
mo_cellrow(cgmo, num, color, ncolor)
    mf_cgmo	**cgmo;
    int		num;
    Gint	*color;
    int		ncolor;
{
    int			pad	= ncolor % 2;

    assert(DEFAULT_COLRPREC == 8);

    while (ncolor-- > 0) {
	assert(*color <= 255);
	mo_octet(cgmo, num, *color++);
    }

    if (pad)
	mo_octet(cgmo, num, 0);
}


/*
 * Write an element whose parameters are all the same.
 */
    static void
mo_element(cgmo, num, class, id, func, offset, nval)
    mf_cgmo		**cgmo;
    int			num;
    int			class;
    int			id;
    void		(*func)();
    unsigned		offset;
    int			nval;
{
    mo_header(class, id);
    (*func)(cgmo, num, (char*)cgmo[0]+offset, nval);
    mo_flush(cgmo, num, 0);
}


/*
 * Return a string identifying the user, installation, and date.
 */
    static char*
AuthorDate()
{
    int			min_offset;
    char		*username	= getlogin();
    char		timebuf[24];
    static char		buffer[128];
    char		*cp		= buffer;
    char		*endp		= buffer + sizeof(buffer) - 1;
    time_t		clock		= time((time_t *) NULL);
    struct tm		local_tm;
    struct tm		utc_tm;
    struct utsname	name;

#   define ADD_STRING(string)	GKS_STMT( \
	    size_t	nchr	= strlen(string); \
	    nchr	= MIN(endp - cp, nchr); \
	    (void) strncpy(cp, string, nchr); \
	    cp	+= nchr; \
	)

    ADD_STRING("UCAR/Unidata XGKS/CGM $LastChangedRevision$: ");

    if (username == NULL) {
	ADD_STRING("<unknown>");
    } else {
	ADD_STRING(username);
    }

    if (uname(&name) != -1) {
	ADD_STRING("@");
	ADD_STRING(name.nodename);
    }

    ADD_STRING(" ");

    local_tm	= *localtime(&clock);
    utc_tm	= *gmtime(&clock);

    (void) strftime(timebuf, sizeof(timebuf), "%Y-%m-%d %H:%M:%S", 
		    &local_tm);
    ADD_STRING(timebuf);

    min_offset	= (local_tm.tm_hour - utc_tm.tm_hour)*60 +
		  (local_tm.tm_min  - utc_tm.tm_min);

    if (local_tm.tm_year > utc_tm.tm_year || 
	    local_tm.tm_yday > utc_tm.tm_yday) {
	min_offset	+= 24*60;
    } else if (local_tm.tm_year < utc_tm.tm_year || 
	    local_tm.tm_yday < utc_tm.tm_yday) {
	min_offset	-= 24*60;
    }

    if (min_offset != 0) {
	int	hour	= ABS(min_offset/60);
	int	min	= ABS(min_offset%60);

	(void) sprintf(timebuf, " UTC%c%02d%02d", 
		       min_offset < 0 ? '-' : '+',
		       hour,
		       min);

	ADD_STRING(timebuf);
    }

    *cp	= 0;

    return buffer;
}


/*
 * Set the mode of output CGMs.
 */
    static void
mo_mode(cgmo, num, mode)
    mf_cgmo	**cgmo;
    int		num;
    cgmo_mode	mode;
{
    while (num-- > 0)
	(*cgmo++)->mode	= mode;
}


/*
 * Write the Metafile description.
 */
    static void
mo_mfdesc(cgmo, num)
    mf_cgmo	**cgmo;
    int		num;
{
    char	*desc	= AuthorDate();

#if DEBUG
    int		ii;

    for (ii = 0; ii < num; ++ii)
	assert(IS_IN_METAFILE(cgmo[ii]));
#endif

    mo_header(MF_DESCRIPTOR_CL, MFDESC_ID);
    mo_string(cgmo, num, desc);
    mo_flush(cgmo, num, 0);
}


/*
 * Write the VDC extent.
 *
 * This is equivalent to the current workstation window.
 */
    static void
mo_vdcext(cgmo, num)
    mf_cgmo	**cgmo;
    int		num;
{
    mo_header(PIC_DESCRIPTOR_CL, VDCEXT_ID);

    for (; num-- > 0; ++cgmo) {
	/*
	 * Because the default VDC EXTENT is the entire display surface,
	 * we only output a different one.
	 */
        if (cgmo[0]->ws->wsti.current.w.xmin != 0 ||
	    cgmo[0]->ws->wsti.current.w.ymin != 0 ||
	    cgmo[0]->ws->wsti.current.w.xmax != 1 ||
	    cgmo[0]->ws->wsti.current.w.ymax != 1) {

	    Gpoint	corner;

	    corner.x    = cgmo[0]->ws->wsti.current.w.xmin;
	    corner.y    = cgmo[0]->ws->wsti.current.w.ymin;
	    MO_POINT(cgmo, 1, &corner);

	    corner.x   = cgmo[0]->ws->wsti.current.w.xmax;
	    corner.y   = cgmo[0]->ws->wsti.current.w.ymax;
	    MO_POINT(cgmo, 1, &corner);

	    mo_flush(cgmo, 1, 0);
	}
    }
}


/*
 * Write a BEGIN PICTURE delimiter.
 *
 * The identifier is taken from the picture number;
 */
    static void
mo_begpic(cgmo, num)
    mf_cgmo	**cgmo;
    int		num;
{
    int		ii;

    for (ii = 0; ii < num; ++ii) {
	char	identifier[16];			/* Should be large enough */

	assert(IS_IN_METAFILE(cgmo[ii]));

	(void) sprintf(identifier, "XGKS %d", ++cgmo[ii]->picture_number);
	mo_header(DELIMITER_CL, BEGPIC_ID);
	mo_string(cgmo+ii, 1, identifier);
	mo_flush(cgmo+ii, 1, 0);
	mo_mode(cgmo+ii, 1, CGMO_IN_PICTURE);
    }
}


/*
 * Write the current text alignment.
 */
    static void
mo_textalign(cgmo, num)
    mf_cgmo	**cgmo;
    int		num;
{
    while (num-- > 0) {
	float	dummy	= 0;

	assert(IS_IN_BODY(cgmo[num]) || IS_NOT_EMPTY(cgmo[num]));

	/*
	 * Fix a silly bug in the SunGKS 3.0 CGM->PostScript translator 
	 * that causes it to mishandle text when there are changes to the 
	 * Text Alignment in a metacode input file.  This patch forces a 
	 * entry in the metacode file to set the text alignment to normal 
	 * before changing it to something else.  Thanks to Harry Edmond 
	 * <harry@atmos.washington.edu> for the fix.
	 */
	if (cgmo[num]->textalign.hor != GTH_NORMAL 
		|| cgmo[num]->textalign.ver != GTV_NORMAL) {
	    int	normal	= 0;

	    mo_header(ATTRIBUTE_CL, TEXTALIGN_ID);
	    mo_enum(cgmo+num, 1, normal);
	    mo_enum(cgmo+num, 1, normal);
	    mo_real(cgmo+num, 1, dummy);
	    mo_real(cgmo+num, 1, dummy);
	    mo_flush(cgmo+num, 1, 0);
	}

	mo_header(ATTRIBUTE_CL, TEXTALIGN_ID);

	mo_enum(cgmo+num, 1, cgmo[num]->textalign.hor);
	mo_enum(cgmo+num, 1, cgmo[num]->textalign.ver);

	mo_real(cgmo+num, 1, dummy);
	mo_real(cgmo+num, 1, dummy);
	mo_flush(cgmo+num, 1, 0);
    }
}


/*
 * Write the current background color.
 */
    static void
mo_backcolr(cgmo, num)
    mf_cgmo	**cgmo;
    int		num;
{
    int		ii;

    for (ii = 0; ii < num; ++ii) {
	Gcobundl	rep;

	assert(IS_IN_PICTURE(cgmo[ii]));

	if (ginqcolourrep(cgmo[ii]->ws->ws_id, 0, GSET, &rep) == OK) {
	    mo_header(PIC_DESCRIPTOR_CL, BACKCOLR_ID);
	    mo_direct_color(cgmo+ii, 1, &rep);
	    mo_flush(cgmo+ii, 1, 0);
	}
    }
}


/*
 * Write a color-table entry.
 */
    static void
mo_color(cgmo, num, idx, rep)
    mf_cgmo	**cgmo;
    int		num;
    Gint	idx;		/* Origin-0 color index */
    Gcobundl	*rep;
{
    mo_header(ATTRIBUTE_CL, COLRTABLE_ID);
    mo_color_index(cgmo, num, idx);
    mo_direct_color(cgmo, num, rep);
    mo_flush(cgmo, num, 0);
}


/*
 * Write the current color table.
 *
 * NB: Colour-index 0 (the background color) is not written; that should have
 * been handled by an earlier mo_backcolr().
 */
    static void
mo_colrtable(cgmo, num)
    mf_cgmo	**cgmo;
    int		num;
{
    int		ii;

    for (ii = 0; ii < num; ++ii) {
	Gintlist       indices;

	assert(IS_IN_BODY(cgmo[ii]));

	if (ginqcolourindices(cgmo[ii]->ws->ws_id, &indices) == OK) {
	    int	ncolors	= indices.number;
	    int	*idxp	= indices.integers;

	    for (; ncolors-- > 0; ++idxp) {
		Gcobundl	rep;

		if (*idxp != 0 && ginqcolourrep(cgmo[ii]->ws->ws_id, *idxp, 
						GSET, &rep) == OK)
		    mo_color(cgmo+ii, 1, *idxp, &rep);
	    }

	    ufree((voidp)indices.integers);
	}
    }
}


/*
 * Write the current clip rectangle.
 *
 * According to Addendum E of the CGM standard, a CLIP RECTANGLE element is
 * written to the metafile with value (0.,0.,1.,1.) if the `clipping indicator'
 * entry in the GKS state list is `noclip', or with the value of the `clipping
 * rectangle' in the GKS state list if the `clipping indicator' entry in the
 * GKS state list is `clip'.
 */
    static void
mo_cliprec(cgmo, num)
    mf_cgmo	**cgmo;
    int		num;
{
    Gcliprec	clipping;
    struct ws_struct *wst;
    Gpoint		lower_left,
			upper_right;

   wst = (*cgmo)->ws;



/*    if (ginqclip(&clipping) == OK) {			*/
/*	if (clipping.ind == GNOCLIP) {
	    lower_left.x	= 0.;
	    lower_left.y	= 0.;
	    upper_right.x	= 1.;
	    upper_right.y	= 1.;
	} else {
*/
/*		RLM replaced Feb 8, 1995.				*/

/*	    lower_left.x	= clipping.rec.xmin;
	    lower_left.y	= clipping.rec.ymin;
	    upper_right.x	= clipping.rec.xmax;
	    upper_right.y	= clipping.rec.ymax;
*/
	    lower_left.x	= wst->clip.xmin;
	    lower_left.y	= wst->clip.ymin;
	    upper_right.x	= wst->clip.xmax;
	    upper_right.y	= wst->clip.ymax;
/*	}		*/

	mo_header(CONTROL_CL, CLIPRECT_ID);
	MO_POINT(cgmo, num, &lower_left);
	MO_POINT(cgmo, num, &upper_right);
	mo_flush(cgmo, num, 0);
/*    }			*/
}
/*			RLM created this function Dec 15, 1994.		*/
/* Write the given clip rectangle.
 *
 */
    static void
mo_clip_rec(cgmo, num, crect)
    mf_cgmo	**cgmo;
    int		num;
    Gcliprec	*crect;
{

    Gpoint		lower_left,
			upper_right;
    struct ws_struct *wst;

    wst = (*cgmo)->ws;

    lower_left.x	= crect->rec.xmin;
    lower_left.y	= crect->rec.ymin;
    upper_right.x	= crect->rec.xmax;
    upper_right.y	= crect->rec.ymax;
/*			RLM added feb 7, 1995.			*/

    wst->clip.xmax = (crect->rec.xmin > crect->rec.xmax) ?
				crect->rec.xmin : crect->rec.xmax;
    wst->clip.xmin = (crect->rec.xmin < crect->rec.xmax) ?
				crect->rec.xmin : crect->rec.xmax;
    wst->clip.ymax = (crect->rec.ymin > crect->rec.ymax) ?
				crect->rec.ymin : crect->rec.ymax;
    wst->clip.ymin = (crect->rec.ymin < crect->rec.ymax) ?
				crect->rec.ymin : crect->rec.ymax;


    if ( (*cgmo)->mode == CGMO_NOT_EMPTY)	{
	mo_header(CONTROL_CL, CLIPRECT_ID);
	MO_POINT(cgmo, num, &lower_left);
	MO_POINT(cgmo, num, &upper_right);
	mo_flush(cgmo, num, 0);
    }
}


/*
 * Write the current pattern size.
 */
    static void
mo_patsize(cgmo, num)
    mf_cgmo	**cgmo;
    int		num;
{
    mo_header(ATTRIBUTE_CL, PATSIZE_ID);
    MO_VDC(cgmo, num, xgks_state.gks_ptattr.heightvec.x);
    MO_VDC(cgmo, num, xgks_state.gks_ptattr.heightvec.y);
    MO_VDC(cgmo, num, xgks_state.gks_ptattr.widthvec.x);
    MO_VDC(cgmo, num, xgks_state.gks_ptattr.widthvec.y);
    mo_flush(cgmo, num, 0);
}


/*
 * Write the current pattern reference-point.
 */
   static void
mo_patrefpt(cgmo, num)
    mf_cgmo	**cgmo;
    int		num;
{
    mo_header(ATTRIBUTE_CL, FILLREFPT_ID);
    MO_POINT(cgmo, num, &xgks_state.gks_ptattr.ptp);
    mo_flush(cgmo, num, 0);
}


/*
 * Write the current ASFs.
 *
 * Only 15 ASFs are written: the 3 edge ASFs are omitted.  Both the hatch
 * and pattern ASFs are set from the GKS fill-style ASF.
 */
    static void
mo_asf(cgmo, num)
    mf_cgmo	**cgmo;
    int		num;
{
    int		iasf		= 0;
    int		bundled		= 1,
		individual	= 0;

#   undef	PUT_ASF
#   define	PUT_ASF(member)		GKS_STMT( \
	    mo_enum(cgmo, num, iasf); \
	    ++iasf; \
	    mo_enum(cgmo, num, \
		     (int)xgks_state.member == (int)GBUNDLED \
			? bundled  \
			: individual); \
	)

    mo_header(ATTRIBUTE_CL, ASF_ID);

    PUT_ASF(gks_lnattr.type);			/* line type */
    PUT_ASF(gks_lnattr.width);			/* line width */
    PUT_ASF(gks_lnattr.colour);			/* line color */

    PUT_ASF(gks_mkattr.type);			/* marker type */
    PUT_ASF(gks_mkattr.size);			/* marker size */
    PUT_ASF(gks_mkattr.colour);			/* marker color */

    PUT_ASF(gks_txattr.fp);			/* text font index */
    PUT_ASF(gks_txattr.fp);			/* text precision */
    PUT_ASF(gks_txattr.tx_exp);			/* character expansion factor */
    PUT_ASF(gks_txattr.space);			/* character spacing */
    PUT_ASF(gks_txattr.colour);			/* text color */

    PUT_ASF(gks_flattr.inter);			/* interior fill style */
    PUT_ASF(gks_flattr.colour);			/* interior fill color */
    PUT_ASF(gks_flattr.style);			/* hatch index */

    /*
     * The DomainOS compiler warns about an unused value being assigned
     * to variable "iasf" in the following line.  This warning may be
     * safely ignored.
     */
    PUT_ASF(gks_flattr.style);			/* pattern index */

    mo_flush(cgmo, num, 0);
}


/*
 * Write the current attribute elements.
 */
    static void
mo_attributes(cgmo, num)
    mf_cgmo	**cgmo;
    int		num;
{
    mo_lineindex(cgmo, num);
    mo_linetype(cgmo, num);
    mo_linewidth(cgmo, num);
    mo_linecolr(cgmo, num);
    mo_markerindex(cgmo, num);
    mo_markertype(cgmo, num);
    mo_markersize(cgmo, num);
    mo_markercolr(cgmo, num);
    mo_textindex(cgmo, num);
    mo_textfontindex(cgmo, num);
    mo_textprec(cgmo, num);
    mo_charexpan(cgmo, num);
    mo_charspace(cgmo, num);
    mo_textcolr(cgmo, num);
    mo_charheight(cgmo, num);
    mo_charori(cgmo, num);
    mo_textpath(cgmo, num);
    mo_textalign(cgmo, num);
    mo_fillindex(cgmo, num);
    mo_intstyle(cgmo, num);
    mo_fillcolr(cgmo, num);
    mo_hatchindex(cgmo, num);
    mo_patindex(cgmo, num);
    mo_patsize(cgmo, num);
    mo_patrefpt(cgmo, num);
    mo_colrtable(cgmo, num);
    /*
    mo_pattable(cgmo, num);
     */
    mo_asf(cgmo, num);
}


/*
 * Insure that we're in a picture body.
 */
    static void
mo_insure_in_body(cgmo, num)
    mf_cgmo	**cgmo;
    int		num;
{
    while (num-- > 0) {
	assert(IS_IN_PICTURE(cgmo[num]) || IS_IN_BODY(cgmo[num]));

	if (IS_IN_PICTURE(cgmo[num])) {
	    mo_backcolr(cgmo+num, 1);
	    mo_vdcext(cgmo+num, 1);
	    MO_BEGPICBODY(cgmo+num, 1);		/* BEGIN PICTURE BODY */
	    mo_attributes(cgmo+num, 1);		/* attribute settings */
	    mo_cliprec(cgmo+num, 1);		/* CLIP RECTANGLE */
	}
    }
}


/*
 * Write an item to output CGMs (not implemented yet).
 */
    int
CGMwriteItem(mf, num, type, length, data)
    /*ARGSUSED*/
    Metafile       *mf;		/* Metafile structure */
    int		    num;	/* number of output Metfiles */
    Gint	    type;	/* item type */
    Gint	    length;	/* item length */
    Gchar          *data;	/* item data-record */
{
    return OK;
}

/*
 * DNW - 12/16/94
 * Re-position the file pointer to over write the END METAFILE 
 * opcode (i.e., the last two bytes of the file).
 */
int remove_end_metafile(fp)
FILE        *fp;
{
    long int    cur_pos;

    fseek(fp, 0L, SEEK_END); /* Position at end of file */
    cur_pos = ftell(fp);     /* Get the file pointer position */
    cur_pos -= 2;            /* Position at END METAFILE opcode */
    fseek(fp, cur_pos, SEEK_SET);
    cur_pos = ftell(fp);     /* Get the file pointer position */

    if (cur_pos == 0)
       return 0;
    else
       return 1;
}

/*
 * Open an output CGM.
 */
    int
CGMmoOpen(ws)
    WS_STATE_PTR	ws;
{
    int		status	= 1;	/* return status = error */
    int         ffd;
    static char	me[]	= "CGMmoOpen()";

    if (CHAR_BIT != 8) {
	(void) fprintf(stderr, 
	    "%s: I can't work on platforms where CHAR_BIT != 8.  Sorry.\n",
	    me);
	return 1;
    }

    assert(ws != NULL);

    if ((ws->mf.cgmo = (mf_cgmo*)umalloc(sizeof(mf_cgmo))) != NULL) {
	mf_cgmo	*cgmo	= ws->mf.cgmo;

        /* Check to see if the user has access */
        ffd = access(ws->conn, F_OK);

        if (ffd != 0) { /* The file does not exist! */
	  if ((cgmo->fp = fopen(ws->conn, "w")) == NULL) /* DNW - 12/19/94 */
	     (void)CGMmoClose(&ws->mf);
	} else {
	  if ((cgmo->fp = fopen(ws->conn, "r+")) == NULL) /* DNW - 12/19/94 */
	       (void)CGMmoClose(&ws->mf);
        }
	cgmo->ws			= ws;
	cgmo->type			= MF_CGM;


	cgmo->picture_number	= 0;
	cgmo->mode = CGMO_IN_METAFILE; /* DNW - 12/19/94 */

	if (remove_end_metafile(cgmo->fp) == 0) { /* DNW - 12/19/94 */
	   MO_BEGMF(&cgmo, 1);			/* BEGIN METAFILE */
	   MO_MFVERSION(&cgmo, 1);		/* METAFILE VERSION */
	   MO_MFELEMLIST(&cgmo, 1);		/* METAFILE ELEMENT LIST */
	   mo_mfdesc(&cgmo, 1);		/* METAFILE DESCRIPTION */
	   mo_begpic(&cgmo, 1);		/* BEGIN PICTURE */
	} else {
	   mo_begpic(&cgmo, 1);		/* BEGIN PICTURE */
	}
	status	= OK;
    }

    return status;
}


/*
 * Set the clear flag.
 *
 * According to Addendum E of the CGM standard, certain actions need only be
 * performed if the view surface is non-empty and the GKS functions imply a
 * dynamic change to the image.
 */
    int
CGMclear(mf, num, flag)
    Metafile	*mf;
    int		num;
    /* ARGSUSED */
    Gclrflag	flag;
{
    int		ii;

    for (ii = 0; ii < num; ++ii) {
	mf_cgmo	*cgmo	= mf[ii].cgmo;

	if (IS_NOT_EMPTY(cgmo)) {
	    MO_ENDPIC(&cgmo, 1);		/* END PICTURE */
	    mo_begpic(&cgmo, 1);		/* BEGIN PICTURE */
	    mo_backcolr(&cgmo, 1);		/* BACKGROUND COLOR */
	    mo_vdcext(&cgmo, 1);		/* VDC EXTENT */
	}
    }

    return OK;
}


/*
 * Redraw all segments in output CGMs.  
 *
 * Since this is a dynamic action, we don't do anything unless the view 
 * surface is non-empty -- in which case we clear the view surface and then 
 * call the XGKS workstation-redraw function.
 */
    int
CGMredrawAllSeg(mf, num)
    Metafile	*mf;
    int		num;
{
    int		ii;
    extern void	XgksDrawSegToWs PROTO((WS_STATE_PTR));

    for (ii = 0; ii < num; ++ii) {
	mf_cgmo	*cgmo	= mf[ii].cgmo;

	if (IS_NOT_EMPTY(cgmo)) {
	    CGMclear(mf+ii, 1, (Gclrflag)1);
	    XgksDrawSegToWs(mf[ii].cgmo->ws);
	}
    }

    return OK;
}


/*
 * Set the update flag in output CGMs.
 *
 * Since UPDATE has no graphical effect and doesn't affect Metafile contents,
 * we do nothing.
 */
     int
CGMupdate(mf, num, regenflag)
    /*ARGSUSED*/
    Metafile	*mf;
    int		num;
    Gregen	regenflag;
{
    return OK;
}


/*
 * Set the deferal state in output CGMs.
 */
    int
CGMdefer(mf, num, defer_mode, regen_mode)
    /*ARGSUSED*/
    Metafile	*mf;
    int		num;
    Gdefmode	defer_mode;
    Girgmode	regen_mode;
{
    return OK;
}


/*
 * Write a message to output CGMs.
 */
    int
CGMmessage(mf, num, string)
    Metafile	*mf;
    int		num;
    char	*string;
{
    mf_cgmo	**cgmo	= &mf->cgmo;
    int		action	= 0;		/* 0 => no action should be taken as
					 * a result of the message */

    mo_insure_in_body(cgmo, num);
    mo_header(EXTERN_CL, MESSAGE_ID);
    mo_enum(cgmo, num, action);
    mo_string(cgmo, num, string);
    mo_flush(cgmo, num, 0);
    mo_mode(cgmo, num, CGMO_NOT_EMPTY);

    return OK;
}


/*
 * Write a graphic to an output CGM.
 *
 * This routine is suitable for POLYLINEs, POLYMARKERs, and FILLAREAs.
 */
    int
CGMoutputGraphic(mf, num, code, num_pt, pos)
    Metafile		*mf;
    int			num;
    int			code;
    Gint		num_pt;
    Gpoint		*pos;
{
    int			id;
    mf_cgmo		**cgmo	= &mf->cgmo;

    switch ((gksm_item_id)code) {
    case GKSM_POLYLINE:		id = LINE_ID;		break;
    case GKSM_POLYMARKER:	id = MARKER_ID;		break;
    case GKSM_FILL_AREA:	id = POLYGON_ID;	break;
    default:	assert(0);
		return 1;
    }
/*			RLM replaced Dec 19, 1994.			*/
/*    mo_insure_in_body(cgmo, num);
    mo_header(PRIMITIVE_CL, id);
    mo_points(cgmo, num, (char*)pos, num_pt);
    mo_flush(cgmo, num, 0);
    mo_mode(cgmo, num, CGMO_NOT_EMPTY);
*/
    if (id == POLYGON_ID) clip_fill(cgmo,num,id,num_pt,pos);
    else clipper(cgmo,num,id,num_pt,pos);
    return OK;
}


/*
 * Write text.
 */
    int
CGMtext(mf, num, at, string)
    Metafile	*mf;
    int		num;
    Gpoint	*at;
    Gchar	*string;
{
    int		final	= 1;		/* 1 => final text */
    float xmx,ymx,xmn,ymn;

    mf_cgmo	**cgmo	= &mf->cgmo;

    xmn=(*cgmo)->ws->clip.xmin;
    xmx=(*cgmo)->ws->clip.xmax;
    ymn=(*cgmo)->ws->clip.ymin;
    ymx=(*cgmo)->ws->clip.ymax;

/*		RLM added conditionals Dec 19, 1994.			*/
    if (at->x > xmn && at->x < xmx && at->y > ymn && at->y < ymx)
      {
       mo_insure_in_body(cgmo, num);
       mo_header(PRIMITIVE_CL, TEXT_ID);
       MO_POINT(cgmo, num, at);
       mo_enum(cgmo, num, final);
       mo_string(cgmo, num, string);
       mo_flush(cgmo, num, 0);
       mo_mode(cgmo, num, CGMO_NOT_EMPTY);
      }

    return OK;
}


/*
 * Write a cell array.
 */
    int
CGMcellArray(mf, num, ll, ur, lr, foldx, colour, dim)
    Metafile	*mf;
    int		num;
    Gpoint	*ll, *ur, *lr;
    Gint	foldx, *colour;
    Gipoint	*dim;
{
    int		colrprec	= size_cell()*8;
    int		cell_rep_mode	= DEFAULT_CELL_REP_MODE;
    Gint	*stopp;
    mf_cgmo	**cgmo	= &mf->cgmo;

/*		RLM added conditionals  19 Dec 1994.			*/

    if (ll->x >= 0.0 && ll ->x <= 1.0 &&
	ur->x >= 0.0 && ur ->x <= 1.0 &&
	lr->x >= 0.0 && lr ->x <= 1.0)
      {
       mo_insure_in_body(cgmo, num);
       mo_header(PRIMITIVE_CL, CELLARRAY_ID);
       MO_POINT(cgmo, num, ll);
       MO_POINT(cgmo, num, ur);
       MO_POINT(cgmo, num, lr);
       mo_int(cgmo, num, dim->x);
       mo_int(cgmo, num, dim->y);
       mo_int(cgmo, num, colrprec);
       mo_enum(cgmo, num, cell_rep_mode);

       for (stopp = colour + foldx*dim->y; colour < stopp; colour += foldx)
	mo_cellrow(cgmo, num, colour, dim->x);

       mo_mode(cgmo, num, CGMO_NOT_EMPTY);
      }

    return OK;
}


/*
 * Set the size of an individual graphics primitive attribute (i.e one having
 * a single real value).
 *
 * This routine is suitable for POLYLINEs, POLYMARKERs, CHARACTER EXPANSIONs,
 * and CHARACTER SPACEs.
 */
    int
CGMsetGraphSize(mf, num, code, size)
    Metafile		*mf;
    int			num;
    int			code;
    double		size;
{
    mf_cgmo	**cgmo	= &mf->cgmo;

    switch ((gksm_item_id)code) {

#	undef	SET_ATTR
#	define	SET_ATTR(name, func)	GKS_STMT( \
		int	ii; \
		for (ii = 0; ii < num; ++ii) { \
		    cgmo[ii]->name	= size; \
		    if (IS_NOT_EMPTY(cgmo[ii])) \
			func(cgmo+ii, 1); \
		} \
	    )

    case GKSM_LINEWIDTH_SCALE_FACTOR:
	SET_ATTR(linewidth, mo_linewidth);
	break;
    case GKSM_MARKER_SIZE_SCALE_FACTOR:
	SET_ATTR(markersize, mo_markersize);
	break;
    case GKSM_CHARACTER_EXPANSION_FACTOR:
	SET_ATTR(charexpan, mo_charexpan);
	break;
    case GKSM_CHARACTER_SPACING:
	SET_ATTR(charspace, mo_charspace);
	break;
    default:	assert(0);
		return 1;
    }

    return OK;
}


/*
 * Close a segment.
 *
 * Since we're not implementing CGM Adendum 1 functionality yet, this is 
 * ignored.
 */
    int
CGMcloseSeg(mf, num)
    /*ARGSUSED*/
    Metafile	*mf;
    int		num;
{
    return OK;
}


/*
 * Set a graphics primitive index or enumeration attribute (i.e. set an
 * attribute whose value is a single integer).
 */
    int
CGMsetGraphAttr(mf, num, code, attr)
    Metafile		*mf;
    int			num;
    int			code;
    Gint		attr;
{
    mf_cgmo		**cgmo	= &mf->cgmo;

    switch ((gksm_item_id)code) {

#	undef	SET_ATTR
#	define	SET_ATTR(name, func)	GKS_STMT( \
		int	ii; \
		for (ii = 0; ii < num; ++ii) { \
		    cgmo[ii]->name	= attr; \
		    if (IS_NOT_EMPTY(cgmo[ii])) \
			func(cgmo+ii, 1); \
		} \
	    )

    case GKSM_POLYLINE_INDEX:
	SET_ATTR(lineindex, mo_lineindex);
	break;
    case GKSM_LINETYPE:
	SET_ATTR(linetype, mo_linetype);
	break;
    case GKSM_POLYLINE_COLOUR_INDEX:
	SET_ATTR(linecolr.index, mo_linecolr);
	break;
    case GKSM_POLYMARKER_INDEX:
	SET_ATTR(markerindex, mo_markerindex);
	break;
    case GKSM_MARKER_TYPE:
	SET_ATTR(markertype, mo_markertype);
	break;
    case GKSM_POLYMARKER_COLOUR_INDEX:
	SET_ATTR(markercolr.index, mo_markercolr);
	break;
    case GKSM_TEXT_INDEX:
	SET_ATTR(textindex, mo_textindex);
	break;
    case GKSM_TEXT_COLOUR_INDEX:
	SET_ATTR(textcolr.index, mo_textcolr);
	break;
    case GKSM_FILL_AREA_INDEX:
	SET_ATTR(fillindex, mo_fillindex);
	break;
    case GKSM_FILL_AREA_STYLE_INDEX:
	if (attr >= 0)
	    SET_ATTR(patindex, mo_patindex);
	SET_ATTR(hatchindex, mo_hatchindex);
	break;
    case GKSM_FILL_AREA_COLOUR_INDEX:
	SET_ATTR(fillcolr.index, mo_fillcolr);
	break;
    case GKSM_PICK_IDENTIFIER:
    case GKSM_CREATE_SEGMENT:
    case GKSM_DELETE_SEGMENT:
	return OK;
    default:	assert(0);
		return 1;
    }

    return OK;
}


/*
 * Set the text font and precision.
 */
    int
CGMsetTextFP(mf, num, txfp)
    Metafile	*mf;
    int		num;
    Gtxfp	*txfp;
{
    int		i;

    for (i = 0; i < num; ++i) {
	mf[i].cgmo->txfp	= *txfp;

	if (IS_NOT_EMPTY(mf[i].cgmo)) {
	    mo_textfontindex(&mf[i].cgmo, 1);
	    mo_textprec(&mf[i].cgmo, 1);
	}
    }

    return OK;
}


/*
 * Set the character up- and base-vectors.
 *
 * This function sets the CGM element CHARACTER ORIENTATION.
 */
    int
CGMsetCharUp(mf, num, up, base)
    Metafile	*mf;
    int		num;
    Gpoint	*up,
		*base;
{
    int		ii;
    Gpoint	ndc_up,
		ndc_base;

    if (up == NULL) {
	XgksComputeVec(&ndc_up, &ndc_base);
    } else {
	ndc_up		= *up;
	ndc_base	= *base;
    }

    for (ii = 0; ii < num; ++ii) {
	mf[ii].cgmo->charheight	= HYPOT(ndc_up.x, ndc_up.y);
	mf[ii].cgmo->charori[0]	= ndc_up.x;
	mf[ii].cgmo->charori[1]	= ndc_up.y;
	mf[ii].cgmo->charori[2]	= ndc_base.x;
	mf[ii].cgmo->charori[3]	= ndc_base.y;
	
	if (IS_NOT_EMPTY(mf[ii].cgmo)) {
	    mo_charori(&mf[ii].cgmo, 1);
	    mo_charheight(&mf[ii].cgmo, 1);
	}
    }

    return OK;
}


/*
 * Set the text-path.
 */
    int
CGMsetTextPath(mf, num, path)
    Metafile	*mf;
    int		num;
    Gtxpath	path;
{
    int		i;

    for (i = 0; i < num; ++i) {
	mf[i].cgmo->textpath	= path;
	if (IS_NOT_EMPTY(mf[i].cgmo))
	    mo_textpath(&mf[i].cgmo, 1);
    }

    return OK;
}


/*
 * Set the text alignment.
 */
    int
CGMsetTextAlign(mf, num, align)
    Metafile	*mf;
    int		num;
    Gtxalign	*align;
{
    int		ii;

    for (ii = 0; ii < num; ++ii) {
	mf[ii].cgmo->textalign	= *align;
	if (IS_NOT_EMPTY(mf[ii].cgmo))
	    mo_textalign(&mf[ii].cgmo, 1);
    }

    return OK;
}


/*
 * Set the interior fill-style (hollow, solid, etc.).
 */
    int
CGMsetFillStyle(mf, num, style)
    Metafile	*mf;
    int		num;
    Gflinter	style;
{
    int		ii;

    for (ii = 0; ii < num; ++ii) {
	mf[ii].cgmo->intstyle	= style;
	if (IS_NOT_EMPTY(mf[ii].cgmo))
	    mo_intstyle(&mf[ii].cgmo, 1);
    }

    return OK;
}


/*
 * Set the pattern size.
 */
    int
CGMsetPatSize(mf, num)
    Metafile	*mf;
    int		num;
{
    while (num-- > 0) {
	mf_cgmo	*cgmo	= mf[num].cgmo;

	if (IS_NOT_EMPTY(cgmo))
	    mo_patsize(&cgmo, 1);
    }

    return OK;
}


/*
 * Set the pattern reference-point.
 */
    int
CGMsetPatRefpt(mf, num)
    Metafile	*mf;
    int		num;
{
    while (num-- > 0) {
	mf_cgmo	*cgmo	= mf[num].cgmo;

	if (IS_NOT_EMPTY(cgmo))
	    mo_patrefpt(&cgmo, 1);
    }

    return OK;
}


/*
 * Set the ASFs.
 */
    int
CGMsetAsf(mf, num)
    Metafile	*mf;
    int		num;
{
    while (num-- > 0) {
	mf_cgmo	*cgmo	= mf[num].cgmo;

	if (IS_NOT_EMPTY(cgmo))
	    mo_asf(&cgmo, 1);
    }

    return OK;
}


/*
 * Set the line or marker representation.
 *
 * Although CGM Addendum 1 specifies how to modify the bundle table, we haven't
 * implement this capability just yet.
 */
    int
CGMsetLineMarkRep(mf, num, code, idx, type, size, colour)
    /*ARGSUSED*/
    Metafile		*mf;
    int			num;
    int			code;
    Gint		idx, type, colour;
    double		size;
{
    return OK;
}


/*
 * Set the text representation.
 *
 * Not implemented yet.
 */
    int
CGMsetTextRep(mf, num, idx, rep)
    /*ARGSUSED*/
    Metafile	*mf;
    int		num;
    Gint	idx;
    Gtxbundl	*rep;
{
    return OK;
}


/*
 * Set the fill representation.  
 *
 * Not implemented yet.
 */
    int
CGMsetFillRep(mf, num, idx, rep)
    /*ARGSUSED*/
    Metafile	*mf;
    int		num;
    Gint	idx;
    Gflbundl	*rep;
{
    return OK;
}


/*
 * Set a pattern representation.
 *
 * Not implemented yet.
 */
    int
CGMsetPatRep(mf, num, idx, rep)
    /*ARGSUSED*/
    Metafile	*mf;
    int		num;
    Gint	idx;
    Gptbundl	*rep;
{
    return OK;
}


/*
 * Set the representation of a colour.
 */
    int
CGMsetColRep(mf, num, idx, rep)
    Metafile	*mf;
    int		num;
    Gint	idx;		/* Origin-0 color index */
    Gcobundl	*rep;
{
    int		ii;
    mf_cgmo	**cgmo	= &mf->cgmo;


    for (ii = 0; ii < num; ++ii)
	if (IS_NOT_EMPTY(cgmo[ii]))
	    mo_color(cgmo+ii, 1, idx, rep);

    return OK;
}


/*
 * Set the clip rectangle.
 */
    int
CGMsetClip(mf, num, crect)
    Metafile	*mf;
    int		num;
    /* ARGSUSED */
/*		RLM replaced Dec 15, 1994.			*/
/*    Glimit	*rect;						*/
      Gcliprec	*crect;
{
    while (num-- > 0) {
	mf_cgmo		*cgmo		= mf[num].cgmo;
/*	&&&& RLM removed following line Feb 3, 1995.		*/
/*	if (IS_NOT_EMPTY(cgmo))					*/
/*			RLM changed Dec 15 1994.		*/
/*	    mo_cliprec(&cgmo, 1);				*/
/*			RLM changed again Feb 8, 1995.		*/
/*	if (IS_NOT_EMPTY(cgmo))					*/
	    mo_clip_rec(&cgmo,1,crect);
    }

    return OK;
}


/*
 * Set either the Workstation window or the Workstation viewport.
 *
 * According to Addendum E of the CGM standard, certain actions need only be
 * performed if the view surface is non-empty or the GKS functions imply a
 * dynamic change to the image.  Consequently, we check whether the view
 * surface is empty or not, and start a new picture if it's non-empty.
 */
    int
CGMsetLimit(mf, num, code, rect)
    Metafile		*mf;
    int			num;
    /*ARGSUSED*/
    int			code;
    Glimit		*rect;
{
    while (num-- > 0) {
	mf_cgmo	*cgmo	= mf[num].cgmo;

	if (IS_NOT_EMPTY(cgmo)) {
	    MO_ENDPIC(&cgmo, 1);
	    mo_begpic(&cgmo, 1);
	    mo_backcolr(&cgmo, 1);
	    mo_vdcext(&cgmo, 1);
	}
    }

    return OK;
}


/*
 * Rename a segment.
 *
 * Not implemented yet.
 */
    int
CGMrenameSeg(mf, num, old, new)
    /*ARGSUSED*/
    Metafile	*mf;
    int		num;
    Gint	old, new;
{
    return OK;
}


/*
 * Set a segment transformation.
 *
 * Not implemented yet.
 */
    int
CGMsetSegTran(mf, num, name, matrix)
    /*ARGSUSED*/
    Metafile	*mf;
    int		num;
    Gint	name;
    Gfloat	matrix[2][3];
{
    return OK;
}


/*
 * Set a segment (non-transformation) attribute.
 *
 * Not implemented yet.
 */
    int
CGMsetSegAttr(mf, num, name, code, attr)
    /*ARGSUSED*/
    Metafile		*mf;
    int			num;
    Gint		name;
    int			code;
    Gint		attr;
{
    return OK;
}


/*
 * Set the segment visibility in an output Metafile.
 *
 * Not implemented yet.
 */
    int
CGMsetSegVis(mf, num, name, vis)
    /*ARGSUSED*/
    Metafile	*mf;
    int		num;
    Gint	name;
    Gsegvis	vis;
{
    return OK;
}


/*
 * Set segment highlighting.
 *
 * Not implemented yet.
 */
    int
CGMsetSegHilight(mf, num, name, hilight)
    /*ARGSUSED*/
    Metafile	*mf;
    int		num;
    Gint	name;
    Gseghi	hilight;
{
    return OK;
}


/*
 * Set segment priority.
 *
 * Not implemented yet.
 */
    int
CGMsetSegPri(mf, num, name, pri)
    /*ARGSUSED*/
    Metafile	*mf;
    int		num;
    Gint	name;
    double	pri;
{
    return OK;
}


/*
 * Set segment detectability.
 *
 * Not implemented yet.
 */
    int
CGMsetSegDetect(mf, num, name, det)
    /*ARGSUSED*/
    Metafile	*mf;
    int		num;
    Gint	name;
    Gsegdet	det;
{
    return OK;
}


/*
 * Close an output CGM.
 */
    int
CGMmoClose(mf)
    Metafile	*mf;
{
    int		status	= 1;		/* return status = error */

    if (mf != NULL && mf->cgmo != NULL) {
	mf_cgmo *cgmo	= mf->cgmo;

	if (cgmo->fp != NULL) {
	    assert(IS_IN_PICTURE(cgmo) || IS_IN_BODY(cgmo));
	    MO_ENDPIC(&cgmo, 1);
	    MO_ENDMF(&cgmo, 1);

	    if (!ferror(cgmo->fp) & fclose(cgmo->fp) != EOF)
		status	= OK;
	}

	ufree((voidp)mf->cgmo);
	mf->cgmo	= NULL;
    }

    return status;
}
/*		Clip lines, fill area polygons, marker points.		*/
/*		Prior to sending the metafile.				*/

    int markit,fillit;

#ifdef CYGWIN_NT
/*  Cygwin doesn't have enough memory for an array 1,000,000 points */
#define MAXPT 100000
#else
/*  All other platforms have enough memory for an array of 1,000,000 points */
#define MAXPT 1000000
#endif

    int clipper(mf_cgmo **cgmo, /*	Metafile out structure pointer	*/
		int num,	/*	Pointer to metafile out structure*/
		int id,		/*	Id of the primitive.		*/

		int np,		/*	Number of points.		*/
		Gpoint pt[]	/*	Pointer to points.		*/
		)
      {
       int i,j,k,ixm,ix,iym,iy;
       float xmx,ymx,xmn,ymn,x,y,xm,ym;
       float xx,yy,xx2,yy2;
       Gpoint p[MAXPT];

       if (np <= 0 || np > MAXPT -10 )
	 {
	  fprintf (stderr,"Error - clipper, point count wrong (%d).\n",np);
	  return 0;
	 }
       cgm_init();

/*		Loop over the points.					*/

       xmn=(*cgmo)->ws->clip.xmin;
       xmx=(*cgmo)->ws->clip.xmax;
       ymn=(*cgmo)->ws->clip.ymin;
       ymx=(*cgmo)->ws->clip.ymax;

       x=y=1.e20;
       ix=ixm=iy=iym=0;
       for (i=j=0; i < np; i++)
	 {
	  xm=x;
	  ym=y;
	  ixm=ix;
	  iym=iy;

	  x=pt[i].x;
	  y=pt[i].y;

/*			Set ix and iy.  The center space is the
			only drawable space (i.e. ix=iy=0).

				|	|
				| iy=1	|
			------------------------
				|	|
			ix=-1	|ix=iy=0| ix=1
				|	|
			------------------------
				|	|
				| iy=-1	|
*/
	  if (x >= xmn)
	    {
	     if (x <= xmx) ix=0;
	     else ix=1;
	    }
	  else
	    {
	     ix=-1;
	    }
	  if (y >= ymn)
	    {
	     if (y <= ymx) iy=0;
	     else iy=1;
	    }
	  else
	    {
	     iy=-1;
	    }
/*			Take care of markers.				*/

	  if (id == MARKER_ID)
	    {
	     if (ix == 0 && iy == 0)
	       {
	        p[j].x=x;
	        p[j].y=y;
	        j++;
	       }
	     continue;
	    }
/*			If the point is inside.			*/

	  if (iy == 0 && ix == 0)
	    {
	     if (i > 0)
	       {
		if (ixm == 0)
		  {
/*				This is the most frequent case.		*/
/*				The previous point was saved, now
				save this point.			*/
		   if (iym == 0)
		     {
		     }
		   else if (iym == -1)
		     {
		      p[j].x=x-((y-ymn)/(y-ym))*(x-xm);
		      p[j].y=ymn;
		      j++;
		     }
		   else if (iym == 1)
		     {
		      p[j].x=x-((y-ymx)/(y-ym))*(x-xm);
		      p[j].y=ymx;
		      j++;
		     }
		  }	/*  End ixm == 0			*/
		else if (ixm == -1)
		  {
		   if (iym == 0)
		     {
		      p[j].x=xmn;
		      p[j].y=y-((x-xmn)/(x-xm))*(y-ym);
		      j++;
		     }
		   else if (iym == -1)
		     {
		      if ( (xx=x-((y-ymn)/(y-ym))*(x-xm)) >= xmn)
			{
			 p[j].x=xx;
			 p[j].y=ymn;
		         j++;
			}
		      else
			{
			 p[j].x=xmn;
			 p[j].y=y-((x-xmn)/(x-xm))*(y-ym);
		         j++;
			}
		     }
		   else if (iym == 1)
		     {
		      if ( (xx=x-((y-ymx)/(y-ym))*(x-xm)) >= xmn)
			{
			 p[j].x=xx;
			 p[j].y=ymx;
		         j++;
			}
		      else
			{
			 p[j].x=xmn;
			 p[j].y=y-((x-xmn)/(x-xm))*(y-ym);
		         j++;
			}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 1)
		  {
		   if (iym == 0)
		     {
		      p[j].x=xmx;
		      p[j].y=y-((x-xmx)/(x-xm))*(y-ym);
		      j++;
		     }
		   else if (iym == -1)
		     {
		      if ( (xx=x-((y-ymn)/(y-ym))*(x-xm)) < xmx)
			{
			 p[j].x=xx;
			 p[j].y=ymn;
		         j++;
			}
		      else
			{
			 p[j].x=xmx;
			 p[j].y=y-((x-xmx)/(x-xm))*(y-ym);
		         j++;
			}
		     }
		   else if (iym == 1)
		     {
		      if ( (xx=x-((y-ymx)/(y-ym))*(x-xm)) < xmx)
			{
			 p[j].x=xx;
			 p[j].y=ymx;
		         j++;
			}
		      else
			{
			 p[j].x=xmx;
			 p[j].y=y-((x-xmx)/(x-xm))*(y-ym);
		         j++;
			}
		     }
		  }	/*  End ixm == 1			*/
	       }	/*  End i > 0				*/

	     p[j].x = x;
	     p[j].y = y;
	     j++;
	     continue;
	    }	/*  End ix == 0 && iy == 0			*/
 
/*		The point is outside the clip rectangle.	*/

/*			Continue if this is first point.	*/
/*			It should have been saved if it's inside.*/
	
	  if (i == 0) continue;

/*			Continue if current and previous are 
			on the same side of the clip rectangle.
			There is no crossing of the clip area.		*/
	  if (ix == ixm && iy == iym) continue;
	  if ((ix != 0 && ix == ixm) || (iy != 0 && iy == iym)) continue;

/*			Pre-compute xx, xx2, yy, yy2			*/
	  if (y != ym)
	    {
	     xx=x-((y-ymn)/(y-ym))*(x-xm);
	     xx2=x-((y-ymx)/(y-ym))*(x-xm);
	    }
	  else xx=xx2=1.e20;
	  if (x != xm)
	    {
	     yy=y-((x-xmn)/(x-xm))*(y-ym);
	     yy2=y-((x-xmx)/(x-xm))*(y-ym);
	    }
	  else yy=yy2=1.e20;

/*			Current point on left side.			*/

	  if (ix == -1)
	    {
/*			Current point at bottom left.			*/

	     if (iy == -1)
	       {
		if (ixm == 0)
	          {
		   if (iym == 0)
		     {
		      if (xx > xmn) {	p[j].x=xx; p[j].y=ymn; j++;}
		      else {		p[j].x=xmn; p[j].y=yy; j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx2 > xmn)
			{
					p[j].x=xx2; p[j].y=ymx; j++;
		         if (xx > xmn) {p[j].x=xx;  p[j].y=ymn; j++;}
		         else {		p[j].x=xmn; p[j].y=yy;  j++;}
			}
		     }
		  }	/*  End ixm == 0			*/
		else if (ixm == 1)
	          {
		   if (iym == 0)
		     {
		      if (xx < xmx)
			{
					p[j].x=xmx; p[j].y=yy2; j++;
			 if (yy < ymn) {p[j].x=xx;  p[j].y=ymn; j++;}
			 else {		p[j].x=xmn; p[j].y=yy;  j++;}
			}
		     }
		   else if (iym == 1)
		     {
		      if (xx < xmx && yy < ymx)
		        {
			 if (xx2 > xmx) {   p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		    p[j].x=xx2; p[j].y=ymx; j++;}
			 if (xx > xmn)     {p[j].x=xx;  p[j].y=ymn; j++;}
			 else if (yy > ymn){p[j].x=xmn; p[j].y=yy;  j++;}
			}
		     }
		  }	/*  End ixm == 1			*/
	       }	/*  End iy == -1			*/

	     else if (iy == 0)
	       {
		if (ixm == 0)
	          {
		   if (iym == -1)
		     {
		      if (xx > xmn) {	p[j].x=xx;  p[j].y=ymn; j++;
					p[j].x=xmn; p[j].y=yy;  j++;}
		     }
		   else if (iym == 0) { p[j].x=xmn; p[j].y=yy;  j++;}
		   else if (iym == 1)
		     {
		      if (xx2 > xmn) {	p[j].x=xx2; p[j].y=ymx; j++;
					p[j].x=xmn; p[j].y=yy;  j++;}
		     }
		  }	/*  End ixm == 0			*/
		else if (ixm == 1)
	          {
		   if (iym == -1)
		     {
		      if (yy > ymn)
			{
			 if (xx < xmx) {p[j].x=xx;  p[j].y=ymn; j++;}
			 else {		p[j].x=xmx; p[j].y=yy2; j++;}
					p[j].x=xmn; p[j].y=yy;  j++;
			}
		     }
		   else if (iym == 0)  {p[j].x=xmx; p[j].y=yy2; j++;
					p[j].x=xmn; p[j].y=yy;  j++;}
		   else if (iym == 1)
		     {
		      if (yy < ymx)
		        {
			 if (xx2 > xmx) {p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		 p[j].x=xx2; p[j].y=ymx; j++;}
					 p[j].x=xmn; p[j].y=yy;  j++;
			}
		     }
		  }	/*  End ixm == 1			*/
	       }	/*  End iy == 0				*/

	     else if (iy == 1)
	       {
		if (ixm == 0)
	          {
		   if (iym == -1)
		     {
		      if (xx > xmn)
			{
					p[j].x=xx;  p[j].y=ymn; j++;
		         if (yy < ymx) {p[j].x=xmn; p[j].y=yy;  j++;}
		         else {		p[j].x=xx2; p[j].y=ymx; j++;}
			}
		     }
		   else if (iym == 0)
		     {
		      if (yy > ymx) {	p[j].x=xx2; p[j].y=ymx; j++;}
		      else {		p[j].x=xmn; p[j].y=yy;  j++;}
		     }
		  }	/*  End ixm == 0			*/
		else if (ixm == 1)
	          {
		   if (iym == -1)
		     {
		      if (yy > ymn && xx2 < xmx)
			{
			 if (xx > xmx) {p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		p[j].x=xx;  p[j].y=ymn; j++;}
			 if (yy < ymx) {p[j].x=xmn; p[j].y=yy;  j++;}
			 else {		p[j].x=xx2; p[j].y=ymx; j++;}
			}
		     }
		   else if (iym == 0)
		     {
		      if (yy2 < ymx)
			{
			 		p[j].x=xmx; p[j].y=yy2; j++;
			 if (yy < ymx) {p[j].x=xmn; p[j].y=yy;  j++;}
			 else {		p[j].x=xx2; p[j].y=ymx; j++;}
			}
		     }
		  }	/*  End ixm == 1			*/
	       }	/*  End iy == 1				*/

	    }	/*  End ix == -1				*/

/*			Current point in middle (horizontal).		*/

	  else if (ix == 0)
	    {
/*			Current point at bottom middle.			*/

	     if (iy == -1)
	       {
		if (ixm == -1)
	          {
		   if (iym == 0)
		     {
		      if (xx > xmn) {	p[j].x=xmn; p[j].y=yy;  j++;
					p[j].x=xx;  p[j].y=ymn; j++;}
		     }
		   else if (iym == 1)
		     {
		      if (yy > ymn)
			{
		         if (xx2 > xmn) {p[j].x=xx2; p[j].y=ymx; j++;}
		         else {		 p[j].x=xmn; p[j].y=yy;  j++;}
					 p[j].x=xx;  p[j].y=ymn; j++;
			}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
		  {
		   if (iym == 0) {	p[j].x=xx;   p[j].y=ymn; j++;}
		   else if (iym == 1) {	p[j].x=xx2;  p[j].y=ymx; j++;
					p[j].x=xx;   p[j].y=ymn; j++;}
		  }
		else if (ixm == 1)
	          {
		   if (iym == 0)
		     {
		      if (xx < xmx) {	p[j].x=xmx; p[j].y=yy2; j++;
					p[j].x=xx;  p[j].y=ymn; j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx < xmx)
		        {
			 if (yy2 < ymx) {p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		 p[j].x=xx2; p[j].y=ymx; j++;}
					 p[j].x=xx;  p[j].y=ymn; j++;
			}
		     }
		  }	/*  End ixm == 1			*/
	       }	/*  End iy == -1			*/

	     else if (iy == 1)
	       {
		if (ixm == -1)
	          {
		   if (iym == -1)
		     {
		      if (yy < ymx)
			{
		         if (xx < xmn) {p[j].x=xmn; p[j].y=yy;  j++;}
		         else {		p[j].x=xx;  p[j].y=ymn; j++;}
					p[j].x=xx2; p[j].y=ymx; j++;
			}
		     }
		   else if (iym == 0)
		     {
		      if (xx2 > xmn) {	p[j].x=xmn; p[j].y=yy;  j++;
					p[j].x=xx2; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
		  {
		   if (iym == -1) {	p[j].x=xx;  p[j].y=ymn; j++;
					p[j].x=xx2; p[j].y=ymx; j++;}
		   else if (iym == 0) {	p[j].x=xx2; p[j].y=ymx; j++;}
		  }
		else if (ixm == 1)
	          {
		   if (iym == -1)
		     {
		      if (yy2 < ymx)
			{
			 if (xx < xmx) { p[j].x=xx;  p[j].y=ymn; j++;}
			 else {		 p[j].x=xmn; p[j].y=yy2; j++;}
			    		 p[j].x=xx2; p[j].y=ymx; j++;
			}
		     }
		   else if (iym == 0)
		     {
		      if (xx2 < xmx) {	p[j].x=xmx; p[j].y=yy2; j++;
					p[j].x=xx2; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 1			*/
	       }	/*  End iy == 1				*/
	    }	/*  End ix == 0				*/

/*			Current point on right side.			*/

	  else if (ix == 1)
	    {
/*			Current point at bottom right.			*/

	     if (iy == -1)
	       {
		if (ixm == -1)
	          {
		   if (iym == 0)
		     {
		      if (xx > xmn)
		        {
					p[j].x=xmn; p[j].y=yy;  j++;
		         if (xx < xmx) {p[j].x=xx;  p[j].y=ymn; j++;}
		         else {		p[j].x=xmx; p[j].y=yy2; j++;}
		        }
		     }
		   else if (iym == 1)
		     {
		      if (xx > xmn && xx2 < xmx)
			{
			 if (xx2 > xmn){p[j].x=xx2; p[j].y=ymx; j++;}
			 else {		p[j].x=xmn; p[j].y=yy;  j++;}
		         if (xx > xmx) {p[j].x=xmx; p[j].y=yy2; j++;}
		         else {		p[j].x=xx;  p[j].y=ymn; j++;}
			}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
	          {
		   if (iym == 0)
		     {
		      if (xx < xmx)	{p[j].x=xx;  p[j].y=ymn; j++;}
		      else {		 p[j].x=xmx; p[j].y=yy2;  j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx2 < xmx)
		        {
					p[j].x=xx2; p[j].y=ymx; j++;
			 if (xx < xmx) {p[j].x=xx;  p[j].y=ymn; j++;}
			 else	{	p[j].x=xmx; p[j].y=yy2; j++;}
			}
		     }
		  }	/*  End ixm == 0			*/
	       }	/*  End iy == -1			*/

	     else if (iy == 0)
	       {
		if (ixm == -1)
	          {
		   if (iym == -1)
		     {
		      if (xx < xmx)
			{
			 if (xx > xmn)	{p[j].x=xx;  p[j].y=ymn; j++;}
			 else	{	 p[j].x=xmn; p[j].y=yy;  j++;}
					 p[j].x=xmx; p[j].y=yy2; j++;
			}
		     }
		   else if (iym == 0) { p[j].x=xmn; p[j].y=yy;  j++;
					p[j].x=xmx; p[j].y=yy2; j++;}
		   else if (iym == 1)
		     {
		      if (xx2 < xmx)
			{
			 if (xx2 > xmn) {p[j].x=xx2; p[j].y=ymx; j++;}
			 else	{	 p[j].x=xmn; p[j].y=yy;  j++;}
					 p[j].x=xmx; p[j].y=yy2;  j++;
			}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
	          {
		   if (iym == -1)
		     {
		      if (xx < xmx) {	p[j].x=xx;  p[j].y=ymn; j++;
					p[j].x=xmx; p[j].y=yy2; j++;}
		     }
		   else if (iym == 0){	p[j].x=xmx; p[j].y=yy2; j++;}
		   else if (iym == 1)
		     {
		      if (xx2 < xmx) {	p[j].x=xx2; p[j].y=ymx; j++;
					p[j].x=xmx; p[j].y=yy2; j++;}
		     }
		  }	/*  End ixm == 0			*/
	       }	/*  End iy == 0				*/

	     else if (iy == 1)
	       {
		if (ixm == -1)
	          {
		   if (iym == -1)
		     {
		      if (xx < xmx && yy < ymx)
			{
			 if (xx > xmn) {p[j].x=xx;  p[j].y=ymn; j++;}
			 else {		p[j].x=xmn; p[j].y=yy;  j++;}
			 if (yy2 < ymx){p[j].x=xmx; p[j].y=yy2; j++;}
		         else	       {p[j].x=xx2; p[j].y=ymx; j++;}
			}
		     }
		   else if (iym == 0)
		     {
		      if (xx2 > xmn)
			{
					  p[j].x=xmn; p[j].y=yy;  j++;
		         if (yy2 > ymx) { p[j].x=xx2; p[j].y=ymx; j++;}
		         else {		  p[j].x=xmx; p[j].y=yy2;  j++;}
			}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
	          {
		   if (iym == -1)
		     {
		      if (xx < xmx)
			{
			    		 p[j].x=xx; p[j].y=ymn;  j++;
			 if (xx2 > xmx) {p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		 p[j].x=xx2;  p[j].y=ymx; j++;}
			}
		     }
		   else if (iym == 0)
		     {
		      if (yy2 < ymx) {p[j].x=xmx; p[j].y=yy2;  j++;}
		      else {	      p[j].x=xx2; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 0			*/
	       }	/*  End iy == 1				*/

	    }	/*  End ix == 1				*/

	  if (j > 0 && id == MARKER_ID)
	    {
	     cgm_points(cgmo,num,id,p,j);
	     j=0;
	    }
	  else if (j > 0 && (ix != 0 && iy != 0))
	    {
	     cgm_points(cgmo,num,id,p,j);
	     j=0;
	    }
	 }	/*  End for i					*/
       if (j > 0)
             cgm_points(cgmo,num,id,p,j);
       cgm_flushit(cgmo,num,id);
       return 1;
      }
 
    int cgm_init()
      {
       markit=fillit=0;
       return OK;
      }
    int cgm_points(mf_cgmo **cgmo,int num,int id,Gpoint *pos,Gint num_pt)
      {
       if (num_pt <= 0) return 0;
       if (id == LINE_ID)
	 {
	  if (num_pt < 2) return OK;
          mo_insure_in_body(cgmo, num);
          mo_header(PRIMITIVE_CL, id);
          mo_points(cgmo, num, (char*)pos, num_pt);
          mo_flush(cgmo, num, 0);
          mo_mode(cgmo, num, CGMO_NOT_EMPTY);
	 }
       else if (id == MARKER_ID)
	 {
	  if (!markit)
	    {
	     mo_insure_in_body(cgmo, num);
	     mo_header(PRIMITIVE_CL, id);
	     markit=1;
	    }
	  mo_points(cgmo, num, (char*)pos, num_pt);
	 }
       else if (id == POLYGON_ID)
	 {
	  if (!fillit)
	    {
	     mo_insure_in_body(cgmo, num);
	     mo_header(PRIMITIVE_CL, id);
	     fillit=1;
	    }
	  mo_points(cgmo, num, (char*)pos, num_pt);
	 }
       return OK;
      }
    int cgm_flushit(mf_cgmo **cgmo,int num,int id)
      {
       if (markit || fillit)
	 {
          mo_flush(cgmo, num, 0);
          mo_mode(cgmo, num, CGMO_NOT_EMPTY);
	 }
       return OK;
      }
/*		Clip fill area polygons.				*/
/*		Prior to sending the metafile.				*/


    int clip_fill(mf_cgmo **cgmo, /*	Metafile out structure pointer	*/
		int num,	/*	Pointer to metafile out structure*/
		int id,		/*	identify the function.		*/
		int np,		/*	Number of points.		*/
		Gpoint pt[]	/*	Pointer to points.		*/
		)
      {
       int i,j,k,ixm,ix,iym,iy,jm,jj,filled;
       float xmx,ymx,xmn,ymn,x,y,xm,ym;
       float xx,yy,xx2,yy2;
       Gpoint p[MAXPT];

       if (np <= 0 || np > MAXPT -10 )
	 {
	  fprintf (stderr,"Error - clip_fill, point count wrong (%d).\n",np);
	  return 0;
	 }
       cgm_init();
       filled=0;

/*		Loop over the points.					*/

       xmn=(*cgmo)->ws->clip.xmin;
       xmx=(*cgmo)->ws->clip.xmax;
       ymn=(*cgmo)->ws->clip.ymin;
       ymx=(*cgmo)->ws->clip.ymax;

/*fprintf(stdout,"num = %d, id = %d, np = %d, limits = x(%g,%g) y(%g,%g)\n",
						num,id,np,xmn,xmx,ymn,ymx);*/
       x=y=1.e20;
       ix=ixm=iy=iym=0;
	jm=0;
       for (i=j=0; i <= np; i++)
	 {
	  xm=x;
	  ym=y;
	  ixm=ix;
	  iym=iy;

	  /* DNW - 6/16/98, overflowing array buffer */
	  if (i == np) {
	     x=pt[0].x;
	     y=pt[0].y;
	  } else {
	     x=pt[i].x;
	     y=pt[i].y;
	  }

/*			Set ix and iy.  The center space is the
			only drawable space (i.e. ix=iy=0).

				|	|
				| iy=1	|
			------------------------
				|	|
			ix=-1	|ix=iy=0| ix=1
				|	|
			------------------------
				|	|
				| iy=-1	|
*/
	  if (x > xmn)
	    {
	     if (x < xmx) ix=0;
	     else ix=1;
	    }
	  else
	    {
	     ix=-1;
	    }
	  if (y > ymn)
	    {
	     if (y < ymx) iy=0;
	     else iy=1;
	    }
	  else
	    {
	     iy=-1;
	    }
/*			If the point is inside.			*/

	  if (iy == 0 && ix == 0)
	    {
	     filled=1;
	     if (i > 0)
	       {
		if (ixm == 0)
		  {
/*				This is the most frequent case.		*/
/*				The previous point was saved, now
				save this point.			*/
		   if (iym == 0)
		     {
		     }
		   else if (iym == -1)
		     {
		      p[j].x=x-((y-ymn)/(y-ym))*(x-xm);
		      p[j].y=ymn;
		      j++;
		     }
		   else if (iym == 1)
		     {
		      p[j].x=x-((y-ymx)/(y-ym))*(x-xm);
		      p[j].y=ymx;
		      j++;
		     }
		  }	/*  End ixm == 0			*/
		else if (ixm == -1)
		  {
		   if (iym == 0)
		     {
		      p[j].x=xmn;
		      p[j].y=y-((x-xmn)/(x-xm))*(y-ym);
		      j++;
		     }
		   else if (iym == -1)
		     {
		      if ( (xx=x-((y-ymn)/(y-ym))*(x-xm)) >= xmn)
			{
			 p[j].x=xx;
			 p[j].y=ymn;
		         j++;
			}
		      else
			{
			 p[j].x=xmn;
			 p[j].y=y-((x-xmn)/(x-xm))*(y-ym);
		         j++;
			}
		     }
		   else if (iym == 1)
		     {
		      if ( (xx=x-((y-ymx)/(y-ym))*(x-xm)) >= xmn)
			{
			 p[j].x=xx;
			 p[j].y=ymx;
		         j++;
			}
		      else
			{
			 p[j].x=xmn;
			 p[j].y=y-((x-xmn)/(x-xm))*(y-ym);
		         j++;
			}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 1)
		  {
		   if (iym == 0)
		     {
		      p[j].x=xmx;
		      p[j].y=y-((x-xmx)/(x-xm))*(y-ym);
		      j++;
		     }
		   else if (iym == -1)
		     {
		      if ( (xx=x-((y-ymn)/(y-ym))*(x-xm)) < xmx)
			{
			 p[j].x=xx;
			 p[j].y=ymn;
		         j++;
			}
		      else
			{
			 p[j].x=xmx;
			 p[j].y=y-((x-xmx)/(x-xm))*(y-ym);
		         j++;
			}
		     }
		   else if (iym == 1)
		     {
		      if ( (xx=x-((y-ymx)/(y-ym))*(x-xm)) < xmx)
			{
			 p[j].x=xx;
			 p[j].y=ymx;
		         j++;
			}
		      else
			{
			 p[j].x=xmx;
			 p[j].y=y-((x-xmx)/(x-xm))*(y-ym);
		         j++;
			}
		     }
		  }	/*  End ixm == 1			*/
	       }	/*  End i > 0				*/

	     p[j].x = x;
	     p[j].y = y;
	     j++;
	     continue;
	    }	/*  End ix == 0 && iy == 0			*/
 
/*		The point is outside the clip rectangle.	*/

/*			Save a point on the periphery and continue
			if this is first point, or it is in the same
			exterior section as before.		*/
/*			It should have been saved if it's inside.*/
	
	  if (i == 0)
	    {
	     if (x<xmn) p[j].x=xmn; else if (x>xmx) p[j].x=xmx; else p[j].x=x;
	     if (y<ymn) p[j].y=ymn; else if (y>ymx) p[j].y=ymx; else p[j].y=y;
	     j++;
	     continue;
	    }
	  if (ix == ixm && iy == iym) continue;

/*			Pre-compute xx, xx2, yy, yy2			*/
	  if (y != ym)
	    {
	     xx=x-((y-ymn)/(y-ym))*(x-xm);
	     xx2=x-((y-ymx)/(y-ym))*(x-xm);
	    }
	  else xx=xx2=1.e20;
	  if (x != xm)
	    {
	     yy=y-((x-xmn)/(x-xm))*(y-ym);
	     yy2=y-((x-xmx)/(x-xm))*(y-ym);
	    }
	  else yy=yy2=1.e20;

/*			Current point on left side.			*/

	  if (ix == -1)
	    {
/*			Current point at bottom left.			*/

	     if (iy == -1)
	       {
		if (ixm == 0)
	          {
		   if (iym == 0)
		     {
		      filled=1;
		      if (xx > xmn) {	p[j].x=xx;  p[j].y=ymn; j++;}
		      else {		p[j].x=xmn; p[j].y=yy; j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx2 > xmn)
			{
			 filled=1;
					p[j].x=xx2; p[j].y=ymx; j++;
		         if (xx > xmn) {p[j].x=xx;  p[j].y=ymn; j++;}
		         else {		p[j].x=xmn; p[j].y=yy;  j++;}
			}
		      else { p[j].x=xmn; p[j].y=ymx; j++; }
		     }
		  }	/*  End ixm == 0			*/
		else if (ixm == 1)
	          {
		   if (iym == 0)
		     {
		      if (xx < xmx)
			{
			 filled=1;
					p[j].x=xmx; p[j].y=yy2; j++;
			 if (yy < ymn) {p[j].x=xx;  p[j].y=ymn; j++;}
			 else {		p[j].x=xmn; p[j].y=yy;  j++;}
			}
		      else { p[j].x=xmx; p[j].y=ymn; j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx < xmx && yy < ymx)
		        {
			 filled=1;
			 if (xx2 > xmx) {   p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		    p[j].x=xx2; p[j].y=ymx; j++;}
			 if (xx > xmn)     {p[j].x=xx;  p[j].y=ymn; j++;}
			 else if (yy > ymn){p[j].x=xmn; p[j].y=yy;  j++;}
			}
		      else if (xx >= xmx) {p[j].x=xmx; p[j].y=ymn; j++;}
		      else {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 1			*/
		p[j].x=xmn; p[j].y=ymn; j++;
	       }	/*  End iy == -1			*/

	     else if (iy == 0)
	       {
		if (ixm == 0)
	          {
		   if (iym == -1)
		     {
		      filled=1;
		      if (xx > xmn) {	p[j].x=xx;  p[j].y=ymn; j++;
					p[j].x=xmn; p[j].y=yy;  j++;}
		      else {p[j].x=xmn; p[j].y=ymn; j++;}
		     }
		   else if (iym == 0) { filled=1;
					p[j].x=xmn; p[j].y=yy;  j++;}
		   else if (iym == 1)
		     {
		      filled=1;
		      if (xx2 > xmn) {	p[j].x=xx2; p[j].y=ymx; j++;
					p[j].x=xmn; p[j].y=yy;  j++;}
		      else {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 0			*/
		else if (ixm == 1)
	          {
		   if (iym == -1)
		     {
		      if (yy > ymn)
			{
			 filled=1;
			 if (xx < xmx) {p[j].x=xx;  p[j].y=ymn; j++;}
			 else {		p[j].x=xmx; p[j].y=yy2; j++;}
					p[j].x=xmn; p[j].y=yy;  j++;
			}
		      else {p[j].x=xmn; p[j].y=ymn; j++;}
		     }
		   else if (iym == 0)  {filled=1;
					p[j].x=xmx; p[j].y=yy2; j++;
					p[j].x=xmn; p[j].y=yy;  j++;}
		   else if (iym == 1)
		     {
		      if (yy < ymx)
		        {
			 filled=1;
			 if (xx2 > xmx) {p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		 p[j].x=xx2; p[j].y=ymx; j++;}
					 p[j].x=xmn; p[j].y=yy;  j++;
			}
		      else {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 1			*/
		p[j].x=xmn; p[j].y=y; j++;
	       }	/*  End iy == 0				*/

	     else if (iy == 1)
	       {
		if (ixm == 0)
	          {
		   if (iym == -1)
		     {
		      if (xx > xmn)
			{
			 filled=1;
					p[j].x=xx;  p[j].y=ymn; j++;
		         if (yy < ymx) {p[j].x=xmn; p[j].y=yy;  j++;}
		         else {		p[j].x=xx2; p[j].y=ymx; j++;}
			}
		      else {p[j].x=xmn; p[j].y=ymn; j++;}
		     }
		   else if (iym == 0)
		     {
		      filled=1;
		      if (yy > ymx) {	p[j].x=xx2; p[j].y=ymx; j++;}
		      else {		p[j].x=xmn; p[j].y=yy;  j++;}
		     }
		  }	/*  End ixm == 0			*/
		else if (ixm == 1)
	          {
		   if (iym == -1)
		     {
		      if (yy > ymn && xx2 < xmx)
			{
			 filled=1;
			 if (xx > xmx) {p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		p[j].x=xx;  p[j].y=ymn; j++;}
			 if (yy < ymx) {p[j].x=xmn; p[j].y=yy;  j++;}
			 else {		p[j].x=xx2; p[j].y=ymx; j++;}
			}
		      else if (yy <= ymn) {p[j].x=xmn; p[j].y=ymn; j++;}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		   else if (iym == 0)
		     {
		      if (yy2 < ymx)
			{
			 filled=1;
			 		p[j].x=xmx; p[j].y=yy2; j++;
			 if (yy < ymx) {p[j].x=xmn; p[j].y=yy;  j++;}
			 else {		p[j].x=xx2; p[j].y=ymx; j++;}
			}
		     }
		  }	/*  End ixm == 1			*/
		p[j].x=xmn; p[j].y=ymx; j++;
	       }	/*  End iy == 1				*/

	    }	/*  End ix == -1				*/

/*			Current point in middle (horizontal).		*/

	  else if (ix == 0)
	    {
/*			Current point at bottom middle.			*/

	     if (iy == -1)
	       {
		if (ixm == -1)
	          {
		   if (iym == 0)
		     {
		      filled=1;
		      if (xx > xmn) {	p[j].x=xmn; p[j].y=yy;  j++;
					p[j].x=xx;  p[j].y=ymn; j++;}
		      else {p[j].x=xmn; p[j].y=ymn; j++;}
		     }
		   else if (iym == 1)
		     {
		      if (yy > ymn)
			{
			 filled=1;
		         if (xx2 > xmn) {p[j].x=xx2; p[j].y=ymx; j++;}
		         else {		 p[j].x=xmn; p[j].y=yy;  j++;}
					 p[j].x=xx;  p[j].y=ymn; j++;
			}
		      else {p[j].x=xmn; p[j].y=ymn; j++;}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
		  {
		   if (iym == 0) {	filled=1;
					p[j].x=xx;   p[j].y=ymn; j++;}
		   else if (iym == 1) { filled=1;
					p[j].x=xx2;  p[j].y=ymx; j++;
					p[j].x=xx;   p[j].y=ymn; j++;}
		  }
		else if (ixm == 1)
	          {
		   if (iym == 0)
		     {
		      if (xx < xmx) {	filled=1;
					p[j].x=xmx; p[j].y=yy2; j++;
					p[j].x=xx;  p[j].y=ymn; j++;}
		      else {p[j].x=xmx; p[j].y=ymn; j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx < xmx)
		        {
			 filled=1;
			 if (yy2 < ymx) {p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		 p[j].x=xx2; p[j].y=ymx; j++;}
					 p[j].x=xx;  p[j].y=ymn; j++;
			}
		      else {p[j].x=xmx; p[j].y=ymn; j++;}
		     }
		  }	/*  End ixm == 1			*/
		p[j].x=x; p[j].y=ymn; j++;
	       }	/*  End iy == -1			*/

	     else if (iy == 1)
	       {
		if (ixm == -1)
	          {
		   if (iym == -1)
		     {
		      if (yy < ymx)
			{
			 filled=1;
		         if (xx < xmn) {p[j].x=xmn; p[j].y=yy;  j++;}
		         else {		p[j].x=xx;  p[j].y=ymn; j++;}
					p[j].x=xx2; p[j].y=ymx; j++;
			}
		      else {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		   else if (iym == 0)
		     {
		      filled=1;
		      if (xx2 > xmn) {	p[j].x=xmn; p[j].y=yy;  j++;
					p[j].x=xx2; p[j].y=ymx; j++;}
		      else {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
		  {
		   if (iym == -1) {	filled=1;
					p[j].x=xx;  p[j].y=ymn; j++;
					p[j].x=xx2; p[j].y=ymx; j++;}
		   else if (iym == 0) {	filled=1;
					p[j].x=xx2; p[j].y=ymx; j++;}
		  }
		else if (ixm == 1)
	          {
		   if (iym == -1)
		     {
		      if (yy2 < ymx)
			{
			 filled=1;
			 if (xx < xmx) { p[j].x=xx;  p[j].y=ymn; j++;}
			 else {		 p[j].x=xmx; p[j].y=yy2; j++;}
			    		 p[j].x=xx2; p[j].y=ymx; j++;
			}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		   else if (iym == 0)
		     {
		      if (xx2 < xmx) {	filled=1;
					p[j].x=xmx; p[j].y=yy2; j++;
					p[j].x=xx2; p[j].y=ymx; j++;}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 1			*/
		p[j].x=x; p[j].y=ymx; j++;
	       }	/*  End iy == 1				*/
	    }	/*  End ix == 0				*/

/*			Current point on right side.			*/

	  else if (ix == 1)
	    {
/*			Current point at bottom right.			*/

	     if (iy == -1)
	       {
		if (ixm == -1)
	          {
		   if (iym == 0)
		     {
		      if (xx > xmn)
		        {
			 filled=1;
					p[j].x=xmn; p[j].y=yy;  j++;
		         if (xx < xmx) {p[j].x=xx;  p[j].y=ymn; j++;}
		         else {		p[j].x=xmx; p[j].y=yy2; j++;}
		        }
		      else {p[j].x=xmn; p[j].y=ymn; j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx > xmn && yy2 < ymx)
			{
			 filled=1;
			 if (xx2 > xmn){p[j].x=xx2; p[j].y=ymx; j++;}
			 else {		p[j].x=xmn; p[j].y=yy;  j++;}
		         if (xx > xmx) {p[j].x=xmx; p[j].y=yy2; j++;}
		         else {		p[j].x=xx;  p[j].y=ymn; j++;}
			}
		      else if (xx <= xmn) {p[j].x=xmn; p[j].y=ymn; j++;}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
	          {
		   if (iym == 0)
		     {
		      filled=1;
		      if (xx < xmx)	{p[j].x=xx;  p[j].y=ymn; j++;}
		      else {		 p[j].x=xmx; p[j].y=yy2;  j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx2 < xmx)
		        {
			 filled=1;
					p[j].x=xx2; p[j].y=ymx; j++;
			 if (xx < xmx) {p[j].x=xx;  p[j].y=ymn; j++;}
			 else	{	p[j].x=xmx; p[j].y=yy2; j++;}
			}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 0			*/
		p[j].x=xmx; p[j].y=ymn; j++;
	       }	/*  End iy == -1			*/

	     else if (iy == 0)
	       {
		if (ixm == -1)
	          {
		   if (iym == -1)
		     {
		      if (xx < xmx)
			{
			 filled=1;
			 if (xx > xmn)	{p[j].x=xx;  p[j].y=ymn; j++;}
			 else	{	 p[j].x=xmn; p[j].y=yy;  j++;}
					 p[j].x=xmx; p[j].y=yy2; j++;
			}
		      else {p[j].x=xmx; p[j].y=ymn; j++;}
		     }
		   else if (iym == 0) { filled=1;
					p[j].x=xmn; p[j].y=yy;  j++;
					p[j].x=xmx; p[j].y=yy2; j++;}
		   else if (iym == 1)
		     {
		      if (xx2 < xmx)
			{
			 filled=1;
			 if (xx2 > xmn) {p[j].x=xx2; p[j].y=ymx; j++;}
			 else	{	 p[j].x=xmn; p[j].y=yy;  j++;}
					 p[j].x=xmx; p[j].y=yy2;  j++;
			}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
	          {
		   if (iym == -1)
		     {
		      if (xx < xmx) {	filled=1;
					p[j].x=xx;  p[j].y=ymn; j++;
					p[j].x=xmx; p[j].y=yy2; j++;}
		      else {p[j].x=xmx; p[j].y=ymn; j++;}
		     }
		   else if (iym == 0){	filled=1;
					p[j].x=xmx; p[j].y=yy2; j++;}
		   else if (iym == 1)
		     {
		      if (xx2 < xmx) {	filled=1;
					p[j].x=xx2; p[j].y=ymx; j++;
					p[j].x=xmx; p[j].y=yy2; j++;}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 0			*/
		p[j].x=xmx; p[j].y=y; j++;
	       }	/*  End iy == 0				*/

	     else if (iy == 1)
	       {
		if (ixm == -1)
	          {
		   if (iym == -1)
		     {
		      if (xx < xmx && yy < ymx)
			{
			 filled=1;
			 if (xx > xmn) {p[j].x=xx;  p[j].y=ymn; j++;}
			 else {		p[j].x=xmn; p[j].y=yy;  j++;}
			 if (yy2 < ymx){p[j].x=xmx; p[j].y=yy2; j++;}
		         else	       {p[j].x=xx2; p[j].y=ymx; j++;}
			}
		      else if (xx >= xmx) {p[j].x=xmx; p[j].y=ymn; j++;}
		      else if (yy >= ymx) {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		   else if (iym == 0)
		     {
		      if (xx2 > xmn)
			{
			 filled=1;
					  p[j].x=xmn; p[j].y=yy;  j++;
		         if (yy2 > ymx) { p[j].x=xx2; p[j].y=ymx; j++;}
		         else {		  p[j].x=xmx; p[j].y=yy2;  j++;}
			}
		      else {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
	          {
		   if (iym == -1)
		     {
		      if (xx < xmx)
			{
			 filled=1;
			    		 p[j].x=xx; p[j].y=ymn;  j++;
			 if (xx2 > xmx) {p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		 p[j].x=xx2;  p[j].y=ymx; j++;}
			}
		      else {p[j].x=xmx; p[j].y=ymn; j++;}
		     }
		   else if (iym == 0)
		     {
		      filled=1;
		      if (yy2 < ymx) {p[j].x=xmx; p[j].y=yy2;  j++;}
		      else {	      p[j].x=xx2; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 0			*/
	        p[j].x=xmx; p[j].y=ymx; j++;
	       }	/*  End iy == 1				*/
	    }	/*  End ix == 1				*/
	  jm=j;
	  if (j > MAXPT-10)
	    {
	     fprintf (stderr,
			"Error - clip_fill, point count too large (%d).\n",np);
	     return 0;
	    }
	 }	/*  End for i					*/
       if (j > 1 && filled)
	 {
	  cgm_points(cgmo,num,id,p,j);
          cgm_flushit(cgmo,num,id);
	 }
       return 1;
      }
 
