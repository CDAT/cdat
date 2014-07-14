/*
 * This file implements an XGKS input Metafile (MI) workstation using the 
 * Computer Graphics Metafile (CGM) standard.
 *
 * This module will only work on platforms having 8-bit "char"s.
 * This requirement is tested at runtime by this module.
 */

/*LINTLIBRARY*/

/* Non-POSIX includes: */
#include <sys/types.h>		/* for uid_t */

/* POSIX includes: */
#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
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
 * Mapping from CGM ASF indexes to GKSM ASF indexes:
 *
 * NB: Only the first 13 are meaningful.  The last 3 are mapped to an unused
 * ASF slot.
 */
static int	gksm_iasf[18]	= {
   0, 1, 2, 3, 4, 5, 6, 6, 7, 8, 9, 10, 12, 11, 11, 13, 13, 13
};


    static double
int16_to_norm(ival)
    int		ival;
{
    return INT_TO_NORM((unsigned)(1<<15), ival);
}


/*
 * Get a partition control word (PCW).
 */
    static void
mi_pcw(fp, more_data, length)
    FILE	*fp;
    int		*more_data;
    int		*length;
{
    unsigned short	pcw	= 0;
    unsigned char	bytes[2];

    (void) fread((voidp)bytes, (size_t)1, (size_t)2, fp);
    pcw		= NTOH16(bytes);
    *more_data	= pcw &  MORE_DATA_BIT;
    *length	= pcw & ~MORE_DATA_BIT;
}


/*
 * Return the next octet of data.
 *
 * This routine will read the next partition(s) if necessary.
 */
    static unsigned
mi_octet(mi)
    mf_cgmi		*mi;
{
    unsigned char	data;

    --mi->total_left;

    if (mi->partition_left == 0) {
	if (mi->partition_length % 2)			/* If padding */
	    (void) fseek(mi->fp, (long)1, SEEK_CUR);	/* Skip */

	do {
	    int	more_data;

	    mi_pcw(mi->fp, &more_data, &mi->partition_length);

	    assert(mi->partition_length > 0 || more_data);

	} while (mi->partition_length == 0);

	mi->partition_left	= mi->partition_length;
    }

    (void) fread((voidp)&data, (size_t)1, (size_t)1, mi->fp);

    --mi->partition_left;

    return data;
}


/*
 * Get the next octets of data.
 */
#define mi_octets(mi, data, num) 	GKS_STMT( \
	int	n		= num; \
	unsigned char	*ptr	= data; \
	while (n-- > 0) \
	    *ptr++	= mi_octet(mi); \
    )


#if 0
/*
 * Go to the start of the current element.
 */
    static void
mi_reset(mi)
    mf_cgmi	*mi;
{
    (void) fseek(mi->fp, mi->start_this_element, SEEK_SET);
}
#endif

/*
 * Return a 16-bit, integer parameter.
 */
    static int
mi_int16(mi)
    mf_cgmi		*mi;
{
    unsigned char	bytes[2];

    mi_octets(mi, bytes, 2);

    return TTOHS((unsigned short)NTOH16(bytes));
}


/*
 * Return an 16-bit, unsigned integer parameter.
 */
    static unsigned
mi_uint16(mi)
    mf_cgmi	*mi;
{
    unsigned char 	bytes[2];

    mi_octets(mi, bytes, 2);

    return (unsigned short)NTOH16(bytes);
}


/*
 * Return an integer parameter.
 */
#define mi_int(mi)	mi_int16(mi)


/*
 * Return an index parameter.
 */
#define mi_index(mi)		mi_int16(mi)


/*
 * Return a color-index parameter.
 */
#define mi_color_index(mi)	mi_octet(mi)


/*
 * Return an ennumeration parameter.
 */
#define mi_enum(mi)		mi_int16(mi)


/*
 * Get a direct color parameter.
 */
    static void
mi_direct_colors(mi, colors, ncolor)
    mf_cgmi		*mi;
    XGKSMCOLOURREP	*colors;
    int			ncolor;
{
    for (; ncolor-- > 0; ++colors) {
	unsigned char	rep[3];

	mi_octets(mi, rep, 3);

	colors->red	= UINT8_TO_NORM(rep[0]);
	colors->green	= UINT8_TO_NORM(rep[1]);
	colors->blue	= UINT8_TO_NORM(rep[2]);
    }
}


/*
 * Get a string parameter.
 */
    static void
mi_string(mi, string, nchr)
    mf_cgmi	*mi;
    char	*string;
    int		*nchr;
{
    int		length	= mi_octet(mi);

    if (length == LONG_STR_LENGTH) {
	 int	more;

	*nchr	= 0;

	do {
	     unsigned	pattern	= mi_uint16(mi);

	     more	= pattern & MSB_16;
	     length	= pattern & ~MSB_16;
	     mi_octets(mi, (unsigned char*)string, length);
	     *nchr	+= length;
	     string	+= length;
	     *string	= 0;
	} while (more);
    } else {
	mi_octets(mi, (unsigned char*)string, length);
	string[length]	= 0;
	*nchr	= length;
    }
}


/*
 * Return a VDC co-ordinate parameter.
 */
#define mi_vdc(mi)	int16_to_norm(mi_int16(mi));


/*
 * Get point parameters.
 */
mi_point(mi, points, npoint)
    mf_cgmi	*mi;
    Gpoint	*points;
    int		npoint;
{
    for (; npoint-- > 0; ++points) {
	points->x	= mi_vdc(mi);
	points->y	= mi_vdc(mi);
    }
}


/*
 * Get a message parameter.
 */
    static void
mi_mesg(mi, msg)
    mf_cgmi	*mi;
    XGKSMMESG	*msg;
{
    msg->string	= (char*)msg + sizeof(XGKSMMESG);
    mi_string(mi, msg->string, &msg->strlen);
}


/*
 * Get a limit rectangle (e.g. workstation widow or clipping rectangle)
 * parameter.
 */
    static void
mi_rect(mi, limit)
    mf_cgmi	*mi;
    XGKSMLIMIT	*limit;
{
    Gpoint	lower_left, upper_right;

    mi_point(mi, &lower_left, 1);
    mi_point(mi, &upper_right, 1);

    limit->rect.xmin	= lower_left.x;
    limit->rect.ymin	= lower_left.y;
    limit->rect.xmax	= upper_right.x;
    limit->rect.ymax	= upper_right.y;
}


/*
 * Get a graphic (i.e. polyline, polymarker, or fill-area) parameter.
 */
    static void
mi_graph(mi, graph)
    mf_cgmi	*mi;
    XGKSMGRAPH	*graph;
{
    graph->num_pts	= BYTES_LEFT(mi)/size_point(mi);
    graph->pts		= JUST_AFTER(graph, XGKSMGRAPH, Gpoint);
    mi_point(mi, graph->pts, graph->num_pts);
}


/*
 * Get a row of a cell-array parameters.
 */
    static void
mi_cellrow(mi, color, ncolor)
    mf_cgmi	*mi;
    Gint	*color;
    int		ncolor;
{
    while (ncolor-- > 0)
	*color++	= mi_octet(mi);

    if (ncolor % 2)
	(void) mi_octet(mi);
}


/*
 * Return a 32-bit, fixed-point, real parameter.
 */
    static double
mi_real32fx(mi)
    mf_cgmi	*mi;
{
    unsigned char	bytes[2];
    unsigned short	whole;
    unsigned short	frac;

    mi_octets(mi, bytes, 2);
    whole	= NTOH16(bytes);

    mi_octets(mi, bytes, 2);
    frac	= NTOH16(bytes);

    return ((int)TTOHS(whole) + UINT16_TO_NORM(frac));
}


/*
 * Return a real parameter.
 */
#define mi_real(mi)		mi_real32fx(mi)


/*
 * Return an indexed-color parameter.
 */
#define mi_indexed_color(mi)	mi_octet(mi)


/*
 * Ingest character vector information.
 */
    static void
ingest_charvec(mi, vec)
    mf_cgmi		*mi;
    XGKSMCHARVEC	*vec;
{
    double		factor	= mi->char_height /
				      HYPOT(mi->char_up.x, mi->char_up.y);

    mi->char_up.x	*= factor;
    mi->char_up.y	*= factor;
    mi->char_base.x	*= factor;
    mi->char_base.y	*= factor;

    vec->up	= mi->char_up;
    vec->base	= mi->char_base;
}


/*
 * Get the parameters of the current element.
 *
 * This is where the translation from CGM elements to GKSM items occurs.
 */
    static int
mi_parameters(mi, pars, mode)
    mf_cgmi	*mi;
    char	*pars;
    decode_mode	mode;
{
    union cgm_data {
	XGKSMONE       one;
	XGKSMTWO       two;
	XGKSMMESG      msg;
	XGKSMGRAPH     graph;
	XGKSMTEXT      text;
	XGKSMSIZE      size;
	XGKSMCHARVEC   vec;
	XGKSMASF       asf;
	XGKSMLMREP     lmrep;
	XGKSMTEXTREP   txrep;
	XGKSMFILLREP   flrep;
	XGKSMPATREP    patrep;
	XGKSMCOLOURREP corep;
	XGKSMLIMIT     limit;
	XGKSMSEGTRAN   tran;
	XGKSMSEGPRI    pri;
	XGKSMCELLARRAY cell;
	XGKSMPATREF    patref;
	XGKSMPATSIZ    patsiz;
    }		*data	= (union cgm_data*)pars;
    static char	me[]	= "mi_parameters";

    /*
     * The following is a hack.  Since one CGM COLORTABLE element can be 
     * equivalent to more than one GKS COLOR REPRESENTATION items, we
     * special-case this element.
     */
    if (mi->mode == READING_COLOR_TABLE) {
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (Gint)GKSM_COLOUR_REPRESENTATION;
	    mi->CurItem.length	= sizeof(data->corep);
	} else {
	    data->corep.idx	= ++mi->color_index;
	    mi_direct_colors(mi, &data->corep, 1);
	    if (BYTES_LEFT(mi) < size_direct_color())
		mi->mode	= NORMAL_MODE;
	}
	return METAFILE_OK;
    }

    mi->mode	= NORMAL_MODE;

    switch (mi->hash_id) {

    case HASH_ID(DELIMITER_CL, BEGMF_ID):
	if (mode == RETURN_INFO) {
#ifdef DEBUG
	    XGKSMMESG	*msg	= (XGKSMMESG*)cgm_buf;

	    mi_mesg(mi, msg);
	    (void) fprintf(stderr, "%s(): begin metafile \"%s\"\n",
			   me, msg->string);
#endif
	    mi->CurItem.type	= (int)GKSM_CGM_ELEMENT;
	    mi->CurItem.length	= 0;
	}
	break;

    case HASH_ID(DELIMITER_CL, ENDMF_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_END_ITEM;
	    mi->CurItem.length	= 0;
	    mi->GksmEmpty	= 1;
#ifdef DEBUG
	    (void) fprintf(stderr, "%s(): end metafile\n", me);
#endif
	}
	break;

    case HASH_ID(DELIMITER_CL, BEGPIC_ID):
	if (mode == RETURN_INFO) {
#ifdef DEBUG
	    XGKSMMESG	*msg	= (XGKSMMESG*)cgm_buf;

	    mi_mesg(mi, msg);
	    (void) fprintf(stderr, "%s(): begin picture \"%s\"\n",
			   me, msg->string);
#endif
	    mi->CurItem.type	= (int)GKSM_CLEAR_WORKSTATION;
	    mi->CurItem.length	= sizeof(data->one);
	} else {
	    data->one.flag	= (Gint)GCONDITIONALLY;
	}
	break;

    case HASH_ID(DELIMITER_CL, BEGPICBODY_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CGM_ELEMENT;
	    mi->CurItem.length	= 0;
#ifdef DEBUG
	    (void) fprintf(stderr, "%s(): begin picture body\n", me);
#endif
	}
	break;

    case HASH_ID(DELIMITER_CL, ENDPIC_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CGM_ELEMENT;
	    mi->CurItem.length	= 0;
#ifdef DEBUG
	    (void) fprintf(stderr, "%s(): end picture body\n", me);
#endif
	}
	break;

    case HASH_ID(MF_DESCRIPTOR_CL, MFVERSION_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CGM_ELEMENT;
	    mi->CurItem.length	= sizeof(data->one);
	} else {
	    data->one.flag	= mi_int(mi);
	    if (data->one.flag != MFVERSION) {
		(void) fprintf(stderr, 
			       "%s(): metafile version is %d (I'm %d)\n",
			       me, data->one.flag, MFVERSION);
		(void) fputs("This might cause problems\n", stderr);
	    }
	}
	break;

    case HASH_ID(MF_DESCRIPTOR_CL, MFDESC_ID):
	if (mode == RETURN_INFO) {
#ifdef DEBUG
	    XGKSMMESG	*msg	= (XGKSMMESG*)cgm_buf;

	    mi_mesg(mi, msg);
	    (void) fprintf(stderr,
			   "%s(): metafile description: \"%s\"\n",
			   me, msg->string);
#endif
	    mi->CurItem.type	= (int)GKSM_CGM_ELEMENT;
	    mi->CurItem.length	= 0;
	}
	break;

    case HASH_ID(MF_DESCRIPTOR_CL, MFELEMLIST_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CGM_ELEMENT;
	    mi->CurItem.length	= 0;
#ifdef DEBUG
	    (void) fprintf(stderr, "%s(): metafile element-list ignored\n",
			   me);
#endif
	}
	break;

    case HASH_ID(PIC_DESCRIPTOR_CL, COLRMODE_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CGM_ELEMENT;
	    mi->CurItem.length	= sizeof(data->one);
	} else {
	    data->one.flag	= mi_enum(mi);
	    if (data->one.flag != DEFAULT_COLRMODE) {
		(void) fprintf(stderr,
			       "%s(): color selection mode is %d (I'm %d)\n",
			       me, data->one.flag, DEFAULT_COLRMODE);
		return MF_ITEM_ERR;
	    }
	}
	break;

    case HASH_ID(PIC_DESCRIPTOR_CL, VDCEXT_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_WORKSTATION_WINDOW;
	    mi->CurItem.length	= sizeof(data->limit);
	} else {
	    mi_rect(mi, &data->limit);
	}
	break;

    case HASH_ID(CONTROL_CL, CLIPRECT_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CLIPPING_RECTANGLE;
	    mi->CurItem.length	= sizeof(data->limit);
	} else {
	    mi_rect(mi, &data->limit);
	    mi->clip_rect		= data->limit.rect;
	}
	break;

    case HASH_ID(PIC_DESCRIPTOR_CL, BACKCOLR_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_COLOUR_REPRESENTATION;
	    mi->CurItem.length	= sizeof(data->corep);
	} else {
	    data->corep.idx	= 0;
	    mi_direct_colors(mi, &data->corep, 1);
	}
	break;

    case HASH_ID(CONTROL_CL, VDCINTEGERPREC_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CGM_ELEMENT;
	    mi->CurItem.length	= sizeof(data->one);
	} else {
	    data->one.flag	= mi_enum(mi);
	    if (data->one.flag != DEFAULT_VDCINTEGERPREC) {
		(void) fprintf(stderr,
			       "%s(): VDC integer precision is %d (I'm %d)\n",
			       me, data->one.flag, DEFAULT_VDCINTEGERPREC);
		return MF_ITEM_ERR;
	    }
	}
	break;

    case HASH_ID(CONTROL_CL, CLIP_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CLIPPING_RECTANGLE;
	    mi->CurItem.length	= sizeof(data->limit);
	} else {
	    data->one.flag	= mi_enum(mi);
	    if ((Gclip)data->one.flag == GCLIP) {
		data->limit.rect	= mi->clip_rect;
	    } else {
		data->limit.rect.xmin	= 0.;
		data->limit.rect.ymin	= 0.;
		data->limit.rect.xmax	= 1.;
		data->limit.rect.ymax	= 1.;
	    }
	}
	break;

    case HASH_ID(PRIMITIVE_CL, LINE_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_POLYLINE;
	    mi->CurItem.length	= sizeof(XGKSMGRAPH) + sizeof(Gpoint)*
				    (BYTES_LEFT(mi)/size_point(mi));
	} else {
	    mi_graph(mi, &data->graph);
	}
	break;

    case HASH_ID(PRIMITIVE_CL, MARKER_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_POLYMARKER;
	    mi->CurItem.length	= sizeof(XGKSMGRAPH) + sizeof(Gpoint)*
				    (BYTES_LEFT(mi)/size_point(mi));
	} else {
	    mi_graph(mi, &data->graph);
	}
	break;

    case HASH_ID(PRIMITIVE_CL, POLYGON_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_FILL_AREA;
	    mi->CurItem.length	= sizeof(XGKSMGRAPH) + sizeof(Gpoint)*
				    (BYTES_LEFT(mi)/size_point(mi));
	} else {
	    mi_graph(mi, &data->graph);
	}
	break;

    case HASH_ID(PRIMITIVE_CL, TEXT_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_TEXT;
	    mi->CurItem.length	= sizeof(data->text) + BYTES_LEFT(mi) + 1;
	} else {
	    mi_point(mi, &data->text.location, 1);
	    (void) mi_enum(mi);
	    data->text.string	= JUST_AFTER(data, XGKSMTEXT, char);
	    mi_string(mi, data->text.string, &data->text.strlen);
	}
	break;

    case HASH_ID(PRIMITIVE_CL, CELLARRAY_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CELLARRAY;
	    mi->CurItem.length	= sizeof(XGKSMCELLARRAY) + sizeof(Gint)*
				    (BYTES_LEFT(mi)/size_cell());
	} else {
	    int		colrprec;	/* Local color precision */
	    int		cell_rep_mode;	/* Local color representation mode */
	    Gint	*pixel, *stopp;

	    mi_point(mi, &data->cell.ll, 1);
	    mi_point(mi, &data->cell.ur, 1);
	    mi_point(mi, &data->cell.lr, 1);

	    data->cell.dim.x	= mi_int(mi);
	    data->cell.dim.y	= mi_int(mi);

	    colrprec	= mi_int(mi);
	    if (colrprec != 0 && colrprec != DEFAULT_COLRPREC) {
		(void) fprintf(stderr,
			       "%s(): local color precision is %d (I'm %d)\n",
			       me, colrprec, DEFAULT_COLRPREC);
		return MF_DATA_ERR;
	    }

	    cell_rep_mode	= mi_enum(mi);
	    if (cell_rep_mode != DEFAULT_CELL_REP_MODE) {
		(void) fprintf(stderr, 
			    "%s(): local cell representation is %d (I'm %d)\n",
			    me, cell_rep_mode, DEFAULT_CELL_REP_MODE);
		return MF_DATA_ERR;
	    }

	    pixel = data->cell.colour = JUST_AFTER(data, XGKSMCELLARRAY, Gint);

	    for (stopp = pixel + data->cell.dim.x*data->cell.dim.y;
		    pixel < stopp;
		    pixel += data->cell.dim.x)
		mi_cellrow(mi, pixel, data->cell.dim.x);
	    }
	break;

#	define GET_INT_ATTR(func, gksm_id)	GKS_STMT( \
		if (mode == RETURN_INFO) { \
		    mi->CurItem.type	= (int)gksm_id; \
		    mi->CurItem.length	= sizeof(data->one); \
		} else { \
		    data->one.flag	= func(mi); \
		} \
	    )

#	define GET_REAL_ATTR(gksm_id)		GKS_STMT( \
		if (mode == RETURN_INFO) { \
		    mi->CurItem.type	= (int)gksm_id; \
		    mi->CurItem.length	= sizeof(data->size); \
		} else { \
		    data->size.size	= mi_real(mi); \
		} \
	    )

    case HASH_ID(ATTRIBUTE_CL, LINEINDEX_ID):
	GET_INT_ATTR(mi_index, GKSM_POLYLINE_INDEX);
	break;

    case HASH_ID(ATTRIBUTE_CL, LINETYPE_ID):
	GET_INT_ATTR(mi_index, GKSM_LINETYPE);
	break;

    case HASH_ID(ATTRIBUTE_CL, LINEWIDTH_ID):
	GET_REAL_ATTR(GKSM_LINEWIDTH_SCALE_FACTOR);
	break;

    case HASH_ID(ATTRIBUTE_CL, LINECOLR_ID):
	GET_INT_ATTR(mi_indexed_color, GKSM_POLYLINE_COLOUR_INDEX);
	break;

    case HASH_ID(ATTRIBUTE_CL, MARKERINDEX_ID):
	GET_INT_ATTR(mi_index, GKSM_POLYMARKER_INDEX);
	break;

    case HASH_ID(ATTRIBUTE_CL, MARKERTYPE_ID):
	GET_INT_ATTR(mi_index, GKSM_MARKER_TYPE);
	break;

    case HASH_ID(ATTRIBUTE_CL, MARKERSIZE_ID):
	GET_REAL_ATTR(GKSM_MARKER_SIZE_SCALE_FACTOR);
	break;

    case HASH_ID(ATTRIBUTE_CL, MARKERCOLR_ID):
	GET_INT_ATTR(mi_indexed_color, GKSM_POLYMARKER_COLOUR_INDEX);
	break;

    case HASH_ID(ATTRIBUTE_CL, TEXTINDEX_ID):
	GET_INT_ATTR(mi_index, GKSM_TEXT_INDEX);
	break;

    case HASH_ID(ATTRIBUTE_CL, TEXTFONTINDEX_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_TEXT_FONT_AND_PRECISION;
	    mi->CurItem.length	= sizeof(data->two);
	} else {
	    data->two.item1	= mi_index(mi);
	    mi->txfp.font	= data->two.item1;
	    data->two.item2	= (int)mi->txfp.prec;
	}
	break;

    case HASH_ID(ATTRIBUTE_CL, TEXTPREC_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_TEXT_FONT_AND_PRECISION;
	    mi->CurItem.length	= sizeof(data->two);
	} else {
	    data->two.item2	= mi_index(mi);
	    mi->txfp.prec	= (Gtxprec)data->two.item2;
	    data->two.item1	= mi->txfp.font;
	}
	break;

    case HASH_ID(ATTRIBUTE_CL, CHAREXPAN_ID):
	GET_REAL_ATTR(GKSM_CHARACTER_EXPANSION_FACTOR);
	break;

    case HASH_ID(ATTRIBUTE_CL, CHARSPACE_ID):
	GET_REAL_ATTR(GKSM_CHARACTER_SPACING);
	break;

    case HASH_ID(ATTRIBUTE_CL, TEXTCOLR_ID):
	GET_INT_ATTR(mi_indexed_color, GKSM_TEXT_COLOUR_INDEX);
	break;

    case HASH_ID(ATTRIBUTE_CL, CHARHEIGHT_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CHARACTER_VECTORS;
	    mi->CurItem.length	= sizeof(data->vec);
	} else {
	    mi->char_height	= mi_vdc(mi);
	    ingest_charvec(mi, &data->vec);
	}
	break;

    case HASH_ID(ATTRIBUTE_CL, CHARORI_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CHARACTER_VECTORS;
	    mi->CurItem.length	= sizeof(data->vec);
	} else {
	    mi->char_up.x	= mi_vdc(mi);
	    mi->char_up.y	= mi_vdc(mi);
	    mi->char_base.x	= mi_vdc(mi);
	    mi->char_base.y	= mi_vdc(mi);
	    ingest_charvec(mi, &data->vec);
	}
	break;

    case HASH_ID(ATTRIBUTE_CL, TEXTPATH_ID):
	GET_INT_ATTR(mi_enum, GKSM_TEXT_PATH);
	break;

    case HASH_ID(ATTRIBUTE_CL, TEXTALIGN_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_TEXT_ALIGNMENT;
	    mi->CurItem.length	= sizeof(data->two);
	} else {
	    data->two.item1	= mi_enum(mi);
	    data->two.item2	= mi_enum(mi);

	    if (data->two.item1 == CGM_CONTINUOUS_HORIZONTAL_ALIGNMENT ||
		    data->two.item2 == CGM_CONTINUOUS_VERTICAL_ALIGNMENT) {
		(void) fprintf(stderr, 
		       "%s(): can't handle continuous text alignment request\n",
		       me);
		return MF_DATA_ERR;
	    }
	}
	break;

    case HASH_ID(ATTRIBUTE_CL, CHARSETINDEX_ID):
    case HASH_ID(ATTRIBUTE_CL, ALTCHARSETINDEX_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CGM_ELEMENT;
	    mi->CurItem.length	= 0;
#ifdef DEBUG
	    (void) fprintf(stderr,
			   "%s(): (alt)character set index ignored\n", me);
#endif
	}
	break;

    case HASH_ID(ATTRIBUTE_CL, FILLINDEX_ID):
	GET_INT_ATTR(mi_index, GKSM_FILL_AREA_INDEX);
	break;

    case HASH_ID(ATTRIBUTE_CL, INTSTYLE_ID):
	GET_INT_ATTR(mi_enum, GKSM_FILL_AREA_INTERIOR_STYLE);
	break;

    case HASH_ID(ATTRIBUTE_CL, FILLCOLR_ID):
	GET_INT_ATTR(mi_indexed_color, GKSM_FILL_AREA_COLOUR_INDEX);
	break;

    case HASH_ID(ATTRIBUTE_CL, HATCHINDEX_ID):
    case HASH_ID(ATTRIBUTE_CL, PATINDEX_ID):
	GET_INT_ATTR(mi_index, GKSM_FILL_AREA_STYLE_INDEX);
	break;

    case HASH_ID(ATTRIBUTE_CL, FILLREFPT_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_PATTERN_REFERENCE_POINT;
	    mi->CurItem.length	= sizeof(data->patref);
	} else {
	    mi_point(mi, &data->patref.ref, 1);
	}
	break;

    case HASH_ID(ATTRIBUTE_CL, PATTABLE_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CGM_ELEMENT;
	    mi->CurItem.length	= 0;
#ifdef DEBUG
	    (void) fprintf(stderr, "%s(): pattern table ignored\n", me);
#endif
	}
	break;

    case HASH_ID(ATTRIBUTE_CL, PATSIZE_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_PATTERN_SIZE;
	    mi->CurItem.length	= sizeof(data->patsiz);
	} else {
	    data->patsiz.hgt.x	= mi_vdc(mi);
	    data->patsiz.hgt.y	= mi_vdc(mi);
	    data->patsiz.wid.x	= mi_vdc(mi);
	    data->patsiz.wid.y	= mi_vdc(mi);
	}
	break;

    case HASH_ID(ATTRIBUTE_CL, COLRTABLE_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_COLOUR_REPRESENTATION;
	    mi->CurItem.length	= sizeof(data->corep);
	} else {
	    mi->color_index	= mi_color_index(mi);
	    data->corep.idx	= mi->color_index;
	    mi_direct_colors(mi, &data->corep, 1);
	    if (BYTES_LEFT(mi) >= size_direct_color())
		mi->mode	= READING_COLOR_TABLE;
	}
	break;

    case HASH_ID(ATTRIBUTE_CL, ASF_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_ASPECT_SOURCE_FLAGS;
	    mi->CurItem.length	= sizeof(data->asf);
	} else {
	    int	iasf;
	    int	cgm_asf[18];

	    for (iasf = 0; iasf < NUM_ELEMENTS(cgm_asf); ++iasf)
		cgm_asf[iasf]	= 0;

	    while (BYTES_LEFT(mi) > 0) {
		iasf		= mi_enum(mi);
		cgm_asf[iasf]	= mi_enum(mi);
	    }

	    for (iasf = 0; iasf < NUM_ELEMENTS(cgm_asf); ++iasf)
		data->asf.asf[gksm_iasf[iasf]]	= cgm_asf[iasf] == 0
							? (int)GINDIVIDUAL
							: (int)GBUNDLED;
	}
	break;

    case HASH_ID(ESCAPE_CL, ESCAPE_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_CGM_ELEMENT;
	    mi->CurItem.length	= 0;
#ifdef DEBUG
	    (void) fprintf(stderr, "%s(): escape request ignored\n", me);
#endif
	}
	break;

    case HASH_ID(EXTERN_CL, MESSAGE_ID):
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_MESSAGE;
	    mi->CurItem.length	= sizeof(data->msg) + BYTES_LEFT(mi) + 1;
	} else {
	    mi_mesg(mi, &data->msg);
	}
	break;

    default:
	if (mode == RETURN_INFO) {
	    mi->CurItem.type	= (int)GKSM_UNKNOWN_ITEM;
	    mi->CurItem.length	= BYTES_LEFT(mi);
	} else {
	    (void) fprintf(stderr, 
			   "%s(): can't decode class=%d, id=%d element\n",
			   me, mi->class, mi->id);
	}
    }						/* hash_id switch */

    return METAFILE_OK;
}


/*
 * Get a command header.
 */
    static int
mi_header(fp, class, id, length)
    FILE	*fp;
    int		*class;
    int		*id;
    unsigned	*length;
{
    unsigned char	bytes[2];
    unsigned short	header;

    if (fread((voidp)bytes, (size_t)1, (size_t)2, fp) != 2)
	return 0;

    header	= NTOH16(bytes);

    *class	= (header >> 12) & 017;
    *id		= (header >>  5) & 0177;
    *length	= header & 037;

    return 1;
}


/*
 * Get a description of the next GKSM item.
 *
 * Note that this routine returns errors or true GKSM items -- CGM elements
 * are handled internally and are not returned.
 */
    static int
mi_item(mi)
    mf_cgmi	*mi;
{
    unsigned	header_length;
    static char	me[]	= "mi_item";

    if (mi->GksmEmpty)
	return 0;

    if (mi->mode == READING_COLOR_TABLE)
	return 1;

    for (;;) {
	(void) fseek(mi->fp, mi->start_next_element, SEEK_SET);

	if (!mi_header(mi->fp, &mi->class, &mi->id, &header_length)) {
	    mi->filestat	= MF_FILE_ERR;
	    return 0;
	}

#if 0
	mi->start_this_element	= mi->start_next_element;
#endif
	mi->hash_id		= HASH_ID(mi->class, mi->id);

	if (header_length == LONG_CMD_LENGTH) {
	    /*
	     * Long-form CGM element.  Accumulate all partition lengths so that
	     * we can return the total length of the data-record.
	     */
	    int		more_data;	/* More data flag */
	    long 	total	= 0;	/* Total data-record length (bytes) */
	    long	first_part	= ftell(mi->fp);
					/* Start of first partition */

	    do {
		int	length;		/* Current partition length */

		mi_pcw(mi->fp, &more_data, &length);
		total	+= length;
		(void) fseek(mi->fp, (long)(length + length%2), SEEK_CUR);
	    } while (more_data);

	    mi->total_length	= total;
	    mi->start_next_element	= ftell(mi->fp);

	    /* Return to 1st partition */
	    (void) fseek(mi->fp, first_part, SEEK_SET);

	    mi_pcw(mi->fp, &more_data, &mi->partition_length);

	} else {
	    /*
	     * Short-form CGM element.
	     */
	    mi->partition_length	= mi->total_length
					= header_length;
	    mi->start_next_element	= ftell(mi->fp) + header_length + 
					    header_length%2;
	}

	mi->total_left		= mi->total_length;
	mi->partition_left	= mi->partition_length;

	mi_parameters(mi, (char*)NULL, RETURN_INFO);

	switch (mi->CurItem.type) {
	case GKSM_CGM_ELEMENT:
	    mi_parameters(mi, (char*)cgm_buf, DECODE_VALUES);
	    break;
	case GKSM_UNKNOWN_ITEM:
	    (void) fprintf(stderr, "%s(): element ignored (class=%d, id=%d)\n",
			   me, mi->class, mi->id);
	    break;
	default:				/* Should be GKSM item */
	    return 1;
	}
    }						/* Until-something-to-return
						 * loop */
}


/*
 * Open an input CGM.  Read up through the header of the first non-delimiter
 * and non-metafile-descriptor element (which should be just after the first
 * BEGIN PICTURE element).
 */
    int
CGMmiOpen(mf, conn)
    Metafile	*mf;		/* Metafile structure */
    char	*conn;		/* Metafile identifier (filename) */
{
    mf_cgmi	*mi;
    static char	me[]	= "CGMmiOpen()";

    if (CHAR_BIT != 8) {
	(void) fprintf(stderr, 
	    "%s: I can't work on platforms where CHAR_BIT != 8.  Sorry.\n",
	    me);
	return 1;
    }

    if ((mf->cgmi = (mf_cgmi*)umalloc(sizeof(mf_cgmi))) == NULL)
	return 1;

    mi	= mf->cgmi;

    if ((mi->fp = fopen(conn, "r")) == NULL) {
	(void)CGMmiClose(mf);
	return 1;
    }
    
    mi->type			= MF_CGM;
    mi->filestat		= METAFILE_OK;
    mi->mode			= NORMAL_MODE;
    mi->GksmEmpty		= 0;
    mi->CurItem.type		= INVALID;
    mi->CurItem.length		= INVALID;
    mi->txfp.font		= 1;
    mi->txfp.prec		= GSTROKE;
    mi->char_height		= 0.01;
    mi->char_up.x		= 0.;
    mi->char_up.y		= 1.;
    mi->char_base.x		= 1.;
    mi->char_base.y		= 0.;
    mi->start_next_element	= ftell(mi->fp);

    return OK;
}


/*
 * Close an input CGM.
 */
    int
CGMmiClose(mf)
    Metafile	*mf;		/* Metafile structure */
{
    int status	= 1;		/* return status = error */

    if (mf != NULL && mf->cgmi != NULL) {
	if (mf->cgmi->fp != NULL)
	    if (!ferror(mf->cgmi->fp) & fclose(mf->cgmi->fp) != EOF)
		status	= OK;
	ufree((voidp)mf->cgmi);
	mf->cgmi	= NULL;
    }

    return status;
}


/*
 * Return the identity of the next item in a Metafile and the amount of storage
 * necessary to contain its data.
 */
    Gint
CGMnextItem(mf)
    Metafile	*mf;		/* Metafile structure */
{
    mf_cgmi	*mi	= mf->cgmi;

    if (feof(mi->fp)) {
	mi->GksmEmpty		= TRUE;
	mi->CurItem.type	= INVALID;
	mi->CurItem.length	= INVALID;
	return METAFILE_OK;
    }

    if (!mi_item(mi)) {
	mi->GksmEmpty		= TRUE;
	mi->CurItem.type	= INVALID;
	mi->CurItem.length	= INVALID;
	return MF_FILE_ERR;
    }

    mi->GksmEmpty	= FALSE;

    return METAFILE_OK;
}


/*
 * Read the data-portion of a Metafile's current item.
 *
 * The filestat field has been added to the workstation state structure to
 * retain MI error information between calls to ggetgksm and greadgksm.  The
 * field has one of four possible integer values (defined in metafile.h):
 *    METAFILE_OK -- no errors so far reading from metafile
 *    MF_DATA_ERR -- type and length of latest item read (current item) are
 *                   ok, but XgksReadData couldn't read the data (eg. non-
 *                   numeric characters found when trying to read an integer)
 *                   The item may be skipped (via greadgksm w/ length 0) and
 *                   MI processing can continue.
 *    MF_ITEM_ERR -- something more serious than a data error found in latest
 *                   item; eg. type invalid, length invalid, data read ter-
 *                   minated prematurely.  This error condition can be detected
 *                   while going on to the next item, so the current item is
 *                   returned correctly, but subsequent attempts to get/read
 *                   will fail.  Since the exact cause of the error is unknown,
 *                   this is not a recoverable condition.
 *    MF_FILE_ERR -- the system reported an I/O error during a read attempt.
 *                   This error is not recoverable.
 * The first function to detect the error will report it, while attempting to
 * process the item it applies to.  In other words, if greadgksm encounters a
 * file error while trying to go on to the next item after successfully reading
 * the current item, the error will not be reported until the next get/read
 * call.  After a fatal error has been reported (via GKS error 163, item is
 * invalid), subsequent get/read attempts will return error 162, no items left
 * in MI, since the error is unrecoverable and no more reading is allowed.
 */
    int
CGMreadItem(mf, record)
    Metafile	*mf;		/* Metafile structure  */
    char        *record;	/* input data-record */
{
    mf_cgmi	*mi	= mf->cgmi;

    if (record != NULL) {
	mi->filestat	= mi_parameters(mi, record, DECODE_VALUES);

	if (feof(mi->fp)) {
	    mi->GksmEmpty	= TRUE;
	    (void) gerrorhand(162, errgreadgksm, xgks_state.gks_err_file);
	    return 162;
	}
	GKSERROR((mi->filestat == MF_ITEM_ERR) || (mi->filestat == MF_FILE_ERR),
		 163, errgreadgksm);
	GKSERROR((mi->filestat == MF_DATA_ERR), 165, errgreadgksm);
    }

    return OK;
}
