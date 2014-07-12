/*
 * $Id$
 *
 * This file contains implementation-specific definitions and declarations
 * for the GKS Metafile (GKSM) implementaion of an XGKS Metafile.
 *
 * This file requires header-file "../gks_implem.h".
 */

#ifndef GKSM_IMPLEM_H_SEEN
#define GKSM_IMPLEM_H_SEEN


/*
 * Workstation information specific to GKS Metafiles (GKSM):
 */
typedef struct mf_gksm {
    MF_COMMON
    Gchar	std[5];		/* the string GKSM + '\0' */
    Gchar	info[41];	/* author installation etc. + '\0' */
    Gchar	date[9];	/* yy/mm/dd + '\0' */
    Gint	ver;		/* version number */
    Gint	h;		/* Number of bytes of "GKSM" at start of each
				 * item record (0-4) */
    Gint	t;		/* length of item-type indicator field */
    Gint	l;		/* length of item data-record length-indicator 
				 * field */
    Gint	i;		/* length of field for each integer in the 
				 * item data-record */
    Gint	r;		/* length of field for each real in the item
				 * data-record */
    Gint	f;		/* Number representation indicator: 
				 *	1:	all numbers formatted according
				 *		to ISO 6093
				 *	2:	all numbers stored in internal 
				 *		binary format. */
    Gint	ri;		/* Representation of real values:
				 *	1:	real
				 *	2:	integer */
    Gchar	d1[12];		/* Integer equivalent to 0.0 iff ri==2 */
    Gchar	d2[12];		/* Integer equivalent to 1.0 iff ri==2 */
}		mf_gksm;


typedef Gchar  *CHARPTR;
typedef Gint   *INTPTR;
typedef Gpoint *POINTPTR;

/*
 * NB: Because of the hard-coded formats in the following, the GKSM
 * implementation can ONLY READ FILES THAT IT CREATED.
 */

#define READHINT(fptr, i)	fscanf((fptr), "%2d", (int*)(&(i)))
#define READCHR(fptr, chr)	fscanf((fptr), "%c", (char*)&(chr))

/* Number of repeated bytes of the string "GKSM" in each item-header */
#define GKSM_LENGTH		0

/* Item-header identification number */
#define ITEM_TYPE_LENGTH	3
#define ITEM_TYPE_FMT		"%3d"

/* Item-header length of data-record */
#define ITEM_DREC_LENGTH	6
#define ITEM_LENGTH_FMT    	"%6d"

/* Data-record integer values */
#define INT_FIELD_LENGTH	6
#define INT_FMT			"%6d"
#define INT_FMT_SCAN		"%6c"
#define READINT(fptr, i)	fscanf((fptr), " %d%*[ ]", (int*)(&(i)))

/* Data-record floating-point values */
#define FLOAT_FIELD_LENGTH	11
#define FLOAT_FMT		"%11.5f"
#define FLOAT_FMT_SCAN		"%11c"
#define READFTP(fptr, ftp)	fscanf((fptr), " %f%*[ ]", (float*)&(ftp))

/* Type of representation of numbers (1=>formatted; 2=>unformatted) */
#define OUTPUT_FMT_TYPE		1

/*
 * Type of representation of floating-point numbers iff OUTPUT_FMT_TYPE == 2
 * (1=>real; 2=>integer)
 */
#define NUMBER_REP		1

#define INT_SIZE	sizeof(Gint)
#define FLOAT_SIZE	sizeof(Gfloat)
#define POINT_SIZE	sizeof(Gpoint)
#define IPOINT_SIZE	sizeof(Gipoint)
#define LIMIT_SIZE	sizeof(Glimit)
#define CHARPTR_SIZE	sizeof(CHARPTR)
#define INTPTR_SIZE	sizeof(INTPTR)
#define POINTPTR_SIZE	sizeof(POINTPTR)


#endif	/* GKSM_IMPLEM_H_SEEN not defined above */
