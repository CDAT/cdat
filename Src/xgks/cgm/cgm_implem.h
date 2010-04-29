/*
 * This file contains implementation-specific definitions and declarations
 * for the Computer Graphics Metatafile (CGM) implementaion of an XGKS
 * Metafile.
 *
 * $Id$
 */

#ifndef CGM_IMPLEM_H_SEEN
#define CGM_IMPLEM_H_SEEN


/*
 * A CGM direct color:
 */
typedef struct cgm_direct_color {
    int		red;		/* Less than 0 implies RGB value not set */
    int		green;
    int		blue;
}	cgm_direct_color;


/*
 * A CGM color (either direct or indexed, depending on color selection mode):
 */
typedef union cgm_color {
    int			index;
    cgm_direct_color	direct;
}	cgm_color;


/*
 * Output CGM modes:
 */
typedef enum cgmo_mode {
    CGMO_UNSET	= 0,
    CGMO_IN_METAFILE,		/* BEGIN PICTURE not yet seen */
    CGMO_IN_PICTURE,		/* BEGIN PICTURE seen */
    CGMO_IN_BODY,		/* BEGIN PICTURE BODY seen */
    CGMO_NOT_EMPTY		/* Graphic written to view surface */
}	cgmo_mode;

#define IS_UNSET(cgmo)		(cgmo->mode == CGMO_UNSET)
#define IS_IN_METAFILE(cgmo)	(cgmo->mode == CGMO_IN_METAFILE)
#define IS_IN_PICTURE(cgmo)	(cgmo->mode == CGMO_IN_PICTURE)
#define IS_IN_BODY(cgmo)	(cgmo->mode == CGMO_IN_BODY || \
				 cgmo->mode == CGMO_NOT_EMPTY)
#define IS_NOT_EMPTY(cgmo)	(cgmo->mode == CGMO_NOT_EMPTY)


/*
 * Workstation information specific to output Computer Graphics Metafiles 
 * (CGMOs):
 */
typedef struct mf_cgmo {
    MF_COMMON
    struct ws_struct
		*ws;			/* Associated workstation structure */
    cgmo_mode	mode;			/* Output CGM mode */
    int		picture_number;		/* Origin-1 picture number */
    unsigned long
		setmask;
#	define	CGM_MASK_BACKCOLR	(unsigned long)(1 << 0)
#	define	CGM_MASK_LINEINDEX	(unsigned long)(1 << 1)
#	define	CGM_MASK_LINETYPE	(unsigned long)(1 << 2)
#	define	CGM_MASK_LINEWIDTH	(unsigned long)(1 << 3)
#	define	CGM_MASK_LINECOLR	(unsigned long)(1 << 4)
#	define	CGM_MASK_MARKERINDEX	(unsigned long)(1 << 5)
#	define	CGM_MASK_MARKERTYPE	(unsigned long)(1 << 6)
#	define	CGM_MASK_MARKERSIZE	(unsigned long)(1 << 7)
#	define	CGM_MASK_MARKERCOLR	(unsigned long)(1 << 8)
#	define	CGM_MASK_TEXTINDEX	(unsigned long)(1 << 9)
#	define	CGM_MASK_TEXTFONTINDEX	(unsigned long)(1 << 10)
#	define	CGM_MASK_TEXTPREC	(unsigned long)(1 << 11)
#	define	CGM_MASK_CHAREXPAN	(unsigned long)(1 << 12)
#	define	CGM_MASK_CHARSPACE	(unsigned long)(1 << 13)
#	define	CGM_MASK_TEXTCOLR	(unsigned long)(1 << 14)
#	define	CGM_MASK_CHARHEIGHT	(unsigned long)(1 << 15)
#	define	CGM_MASK_CHARORI	(unsigned long)(1 << 16)
#	define	CGM_MASK_TEXTPATH	(unsigned long)(1 << 17)
#	define	CGM_MASK_TEXTALIGN	(unsigned long)(1 << 18)
#	define	CGM_MASK_FILLINDEX	(unsigned long)(1 << 19)
#	define	CGM_MASK_INTSTYLE	(unsigned long)(1 << 20)
#	define	CGM_MASK_FILLCOLR	(unsigned long)(1 << 21)
#	define	CGM_MASK_HATCHINDEX	(unsigned long)(1 << 22)
#	define	CGM_MASK_PATINDEX	(unsigned long)(1 << 23)
#	define	CGM_MASK_PATSIZE	(unsigned long)(1 << 24)
#	define	CGM_MASK_COLRTABLE	(unsigned long)(1 << 25)
    Gcobundl	backcolr;		/* Background color */
    int		lineindex;		/* Line bundle index */
    int		linetype;		/* Line type */
    float	linewidth;		/* Line width */
    cgm_color	linecolr;		/* Line color */
    int		markerindex;		/* Marker bundle index */
    int		markertype;		/* Marker type */
    float	markersize;		/* Marker size */
    cgm_color	markercolr;		/* Marker color */
    int		textindex;		/* Text bundle index */
    Gtxfp	txfp;			/* Text font index and precision */
    float	charexpan;		/* Character expansion factor */
    float	charspace;		/* Inter-character spacing */
    cgm_color	textcolr;		/* Text color index */
    float	charheight;		/* Character height */
    float	charori[4];		/* Character orientation */
    Gtxpath	textpath;		/* Text path */
    Gtxalign	textalign;		/* Text alignment */
    int		fillindex;		/* Fill bundle index */
    Gflinter	intstyle;		/* Interior fill style */
    cgm_color	fillcolr;		/* Fill color */
    int		hatchindex;		/* Hatch index */
    int		patindex;		/* Pattern index */
} mf_cgmo;


/*
 * Workstation information specific to input Computer Graphics Metafiles 
 * (CGMIs):
 */
typedef struct mf_cgmi {
    MF_COMMON
    long	total_length;		/* Total amount of data (excludes 
					 * padding) */
    long	total_left;		/* Total amount of data left (excludes
					 * padding) */
    int		partition_length;	/* Amount of data in current partition
					 * (excludes padding) */
    int		partition_left;		/* Amount left in current partition
					 * (excludes padding) */
    long	start_this_element;	/* File-offset to current element */
    long	start_next_element;	/* File-offset to next element */
    int		mode;			/* CGMI mode */
#	define	NORMAL_MODE		0
#	define	READING_COLOR_TABLE	1
    int		class;			/* Command class of current element */
    int		id;			/* Element ID of current element */
    int		hash_id;		/* Element class and id combination */
    Glimit	clip_rect;		/* Clipping rectangle */
    Gtxfp	txfp;			/* Text font and precision */
    Gpoint	char_up;		/* Character up vector */
    Gpoint	char_base;		/* Character base vector */
    Gfloat	char_height;		/* Character height */
    int		color_index;		/* Color table index */
} mf_cgmi;


/*
 * CGM version:
 */
#define	MFVERSION	1

#undef MIN
#define MIN(a,b)	((a) < (b) ? (a) : (b))
#define ABS(x)		((x) < 0 ? -(x) : (x))
#define SIGN(a,b)	((a) < 0 ? -ABS(b) : ABS(b))

/*
 * Convert between a floating-point representation in the range from
 * zero through one (or minus one through one) and an integral representation.
 */
#define NORM_TO_INT(mod, fval)	SIGN(fval, ((long)(ABS(fval)*((mod)-1) + .5)))
#define UNORM_TO_INT(mod, fval)	MIN((mod)-1, (unsigned long)((fval)*(mod)))
#define INT_TO_NORM(mod, ival)	((double)(ival)/((mod)-1))
#define UINT_TO_NORM(mod, ival)	(.5/(mod) + (double)(ival)/(mod))
#define UNORM_TO_INT8(fval)	UNORM_TO_INT(256, fval)
#define NORM_TO_INT16(fval)	NORM_TO_INT((unsigned)(1<<15), fval)
#define UNORM_TO_INT16(fval)	UNORM_TO_INT((unsigned long)(1<<16), fval)
#define UINT8_TO_NORM(ival)	UINT_TO_NORM(256, ival)
#define UINT16_TO_NORM(ival)	UINT_TO_NORM((unsigned long)(1<<16), ival)

/*
 * Convert between host and network byte orders for 16-bit quantities.
 */
#define NTOH16(n)	(((n)[0] << 8) | (n)[1])
#define HTON16(h, n)	GKS_STMT((n)[0] = MS_8(h); (n)[1] = LS_8(h);)

/*
 * Miscellaneous utility macros:
 */
#define	ABS(x)		((x) < 0 ? -(x) : (x))
#define HYPOT(x,y)	sqrt((double)((x)*(x) + (y)*(y)))
#define NUM_ELEMENTS(a)	(sizeof(a) / sizeof(a[0]))
#define MSB_16		((unsigned)1 << 15)
					/* 16-bit most significant bit */
#define MASK_16		(unsigned)0177777
					/* 16-bit mask */
#define LS_8(val)	((val) & 0377)	/* least significant 8 bits */
#define MS_8(val)	LS_8((val) >> 8)/* most significant 8 bits */
#define ROUNDUP(x, y)	(((x + y - 1)/y)*y)
#define JUST_AFTER(ptr, type, align) \
			(align *)((char*)ptr + \
			    ROUNDUP(sizeof(type), sizeof(align)))

/*
 * Macros for converting between the host and two's-complement forms of a
 * 16-bit integer:
 */
#define HTOTS(s_shrt)	(MASK_16 & (unsigned short)(s_shrt))

#define TTOHS(u_shrt)	((unsigned)(u_shrt) & MSB_16 \
			    ? (long)0-(unsigned short) \
				(MASK_16 & (~(unsigned)(u_shrt) + 1)) \
			    : (long)(u_shrt))

/*
 * Miscellaneous, CGM-specific macros:
 */
#define MAX_SHORT_CMD_LENGTH	30	/* Max bytes in a short command */
#define LONG_CMD_LENGTH		31	/* Long command "length"-value */
#define MAX_SHORT_STR_LENGTH	254	/* Max bytes in a short string */
#define LONG_STR_LENGTH		255	/* Long string "length"-value */
#define MORE_DATA_BIT		(1 << 15)
#define BYTES_LEFT(mi)		(mi->total_left)
#define HASH_ID(class, id)	(((unsigned)(class) << 7) | (unsigned)(id))
#define	PACKED_LIST		1	/* Cell representation mode. */
#define CGM_CONTINUOUS_HORIZONTAL_ALIGNMENT \
				4
#define CGM_CONTINUOUS_VERTICAL_ALIGNMENT \
				6

/*
 * Defaults:
 */
#define DEFAULT_COLRPREC	8	/* Color precision */
#define DEFAULT_CELL_REP_MODE	PACKED_LIST
#define DEFAULT_VDCINTEGERPREC	16	/* VDC integer precision */
#define DEFAULT_COLRMODE	0	/* Indexed */

/*
 * Command class values:
 */
#define DELIMITER_CL		0
#define MF_DESCRIPTOR_CL	1
#define PIC_DESCRIPTOR_CL	2
#define CONTROL_CL		3
#define PRIMITIVE_CL		4
#define ATTRIBUTE_CL		5
#define ESCAPE_CL		6
#define EXTERN_CL		7

/*
 * Element-ID values.  Together with the command-class, these uniquely 
 * identify an individual command.  The names are taken from the CLEAR
 * TEXT encoding (with "_ID" appended).
 */
/* Delimiter Elements: */
#define BEGMF_ID		1
#define ENDMF_ID		2
#define BEGPIC_ID		3
#define BEGPICBODY_ID		4
#define ENDPIC_ID		5

/* Metafile Descriptor Elements: */
#define MFVERSION_ID		1
#define MFDESC_ID		2
#define MFELEMLIST_ID		11

/* Picture Descriptor Elements: */
#define COLRMODE_ID		2
#define VDCEXT_ID		6
#define BACKCOLR_ID		7

/* Control Elements: */
#define VDCINTEGERPREC_ID	1
#define CLIPRECT_ID		5
#define CLIP_ID			6

/* Graphical Primitive Elements: */
#define LINE_ID			1
#define MARKER_ID		3
#define TEXT_ID			4
#define POLYGON_ID		7
#define CELLARRAY_ID		9

/* Primitive Attribute Elements: */
#define LINEINDEX_ID		1
#define LINETYPE_ID		2
#define LINEWIDTH_ID		3
#define LINECOLR_ID		4
#define MARKERINDEX_ID		5
#define MARKERTYPE_ID		6
#define MARKERSIZE_ID		7
#define MARKERCOLR_ID		8
#define TEXTINDEX_ID		9
#define TEXTFONTINDEX_ID	10
#define TEXTPREC_ID		11
#define CHAREXPAN_ID		12
#define CHARSPACE_ID		13
#define TEXTCOLR_ID		14
#define CHARHEIGHT_ID		15
#define CHARORI_ID		16
#define TEXTPATH_ID		17
#define TEXTALIGN_ID		18
#define CHARSETINDEX_ID		19
#define ALTCHARSETINDEX_ID	20
#define FILLINDEX_ID		21
#define INTSTYLE_ID		22
#define FILLCOLR_ID		23
#define HATCHINDEX_ID		24
#define PATINDEX_ID		25
#define FILLREFPT_ID		31
#define PATTABLE_ID		32
#define PATSIZE_ID		33
#define COLRTABLE_ID		34
#define ASF_ID			35

/* Escape Elements: */
#define ESCAPE_ID		1

/* External Elements: */
#define MESSAGE_ID		1

/*
 * CGM element decoding mode:
 */
typedef enum decode_mode {
    RETURN_INFO,
    DECODE_VALUES
} decode_mode;

/*
 * I/O buffer:
 */
extern unsigned char		cgm_buf[];

/*
 * CGM implementation-functions implemented as macros:
 */
#define size_point(cgm)		(2*size_vdc(cgm))
#define size_vdc(cgm)		size_int16()
#define size_int16()		(size_t)2
#define size_direct_color()	(size_t)3
#define size_cell()		(size_t)(DEFAULT_COLRPREC/8)

#endif	/* CGM_IMPLEM_H_SEEN not defined above */
