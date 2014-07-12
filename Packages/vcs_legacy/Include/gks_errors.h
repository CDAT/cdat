/*
 *		Copyright IBM Corporation 1989
 *
 *                      All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of IBM not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 *
 *
 * University of Illinois at Urbana-Champaign
 * Department of Computer Science
 * 1304 W. Springfield Ave.
 * Urbana, IL	61801
 *
 * (C) Copyright 1987, 1988 by The University of Illinois Board of Trustees.
 * All rights reserved.
 *
 * Tool: X 11 Graphical Kernel System
 * Author: Gregory Scott Rogers
 * Author: Sung Hsien Ching Kelvin
 * Author: Yu Pan
 * 
 * States
 * 
 * $Id$
 * $__Header$
 */

#define ENOTGKCL	1	/* GKS not in proper state: GKS shall be in the state GKCL */
#define ENOTGKKOP	2	/* GKS not in proper state: GKS shall be in the state GKOP */
#define ENOTWASC	3	/* GKS not in proper state: GKS shall be in the state WSAC */
#define ENOTSGOP	4	/* GKS not in proper state: GKS shall be in the state SGOP */
#define ENOTACOP	5	/* GKS not in proper state: GKS shall be either in the state WSAC or in the state SGOP */
#define ENOTOPAC	6	/* GKS not in proper state: GKS shall be either in the state WSOP or in the state WSAC */
#define ENOTWSOP	7	/* GKS not in proper state: GKS shall be in one of the states WSOP, WSAC or SGOP */
#define ENOTGWWS	8	/* GKS not in proper state: GKS shall be in one of the states GKOP, WSOP, WSAC or SGOP */

/*
 * Workstations
 */
#define EWSIDINV	20	/* Specified workstation identifier is invalid */
#define ECNIDINV	21	/* Specified connection identifier is invalid */
#define EWSTPINV	22	/* Specified workstation type is invalid */
#define ENOWSTYP	23	/* Specified workstation type does not exist */
#define EWSISOPN	24	/* Specified workstation is open */
#define EWSNOTOP	25	/* Specified workstation is not open */
#define EWSCNTOP	26	/* Specified workstation cannot be opened */
#define EWISSNOP	27	/* Workstation Independent Segment Storage is not open */
#define EWISSOPN	28	/* Workstation Independent Segment Storage is already open */
#define EWSISACT	29	/* Specified workstation is active */
#define EWSNTSCT	30	/* Specified workstation is not active */
#define EWSCATMO	31	/* Specified workstation is of category MO */
#define EWSNOTMO	32	/* Specified workstation is not of category MO */
#define EWSCATMI	33	/* Specified workstation is of category MI */
#define EWSNOTMI	34	/* Specified workstation is not of category MI */
#define EWSCATIN	35	/* Specified workstation is of category INPUT */
#define EWSIWISS	36	/* Specified workstation is Workstation Independent Segment Storage */
#define EWSNOTOI	37	/* Specified workstation is not of category OUTIN */
#define EWSNOTIO	38	/* Specified workstation is neither of category INPUT nor of category OUTIN */
#define EWSNOTOO	39	/* Specified workstation is neither of category OUTPUT nor of category OUTIN */
#define EWSNOPXL	40	/* Specified workstation has no pixel store readback capability */
#define EWSNOGDP	41	/* Specified workstation type is not able to generate the specified generalized drawing primitive */
#define EWSMXOPN	42	/* Maximum number of simultaneously open workstations would be exceeded */
#define EWSMXACT	43	/* Maximum number of simultaneously active workstations would be exceeded */

/*
 * Transformations
 */
#define EBADXFRM	50	/* Transformation number is invalid */
#define EBADRCTD	51	/* Rectangle definition is invalid */
#define EBDVIEWP	52	/* Viewport is not within the Normalized Device Coordinate unit square */
#define EBDWINDW	53	/* Workstation window is not within the Normalized Device Coordinate unit square */
#define EVIEWDSP	54	/* Workstation viewport is not within the display space */

/*
 * Output Attributes
 */
#define EBADLINX	60	/* Polyline index is invalid */
#define ENOLINEX	61	/* A representation for the specified polyline index has not been defined on this workstation */
#define ENOPLINX	62	/* A representation for the specified polyline index has not been predefined on this workstation */
#define ELINEEQZ	63	/* Linetype is equal to zero */
#define ENOLINTP	64	/* Specified linetype is not supported on this workstation */
#define	ELNWDLTZ	65	/* Linewidth scale factor is less than zero */
#define EBADMRKX	66	/* Polymarker index is invalid */
#define ENOMARKX	67	/* A representation for the specified polymarker index has not been defined on this workstation */
#define	ENOPMRKX	68	/* A representation for the specified polymarker index has not been predefined on this workstation */
#define EMAKREQZ	69	/* Marker type is equal to zero */
#define ENOMRKTP	70	/* Specified marker type is not supported on this workstation */
#define	EMKSZLTZ	71	/* Marker size scale factor is less than zero */
#define EBADTXTX	72	/* Text index is invalid */
#define ENOTEXTX	73	/* A representation for the specified text index has not been defined on this workstation */
#define	ENOPTXTX	74	/* A representation for the specified text index has not been predefined on this workstation */
#define ETXTFEQZ	75	/* Text font is equal to zero */
#define ENOTXTFP	76	/* Requested text font is not supported for the specified precision on this workstation */
#define ECEXFLEZ	77	/* Character expansion factor is less than or equal to zero */
#define ECHHTLEZ	78	/* Character height is less than or equal to zero */
#define ECHRUPVZ	79	/* Length of character up vector is zero */
#define EBADFILX	80	/* Fill area index is invalid */
#define ENOFILLX	81	/* A representation for the specified fill area index has not been defined on this workstation */
#define	ENOPFILX	82	/* A representation for the specified fill area index has not been predefined on this workstation */
#define ENOFSTYL	83	/* Specified fill area interior style is not supported on this workstation */
#define ESTYLEQZ	84	/* Style (pattern or hatch) index is equal to zero */
#define EBADPATN	85	/* Specified pattern index is invalid */
#define ENOHATCH	86	/* Specified hatch style is not supported on this workstation */
#define EPATSZLZ	87	/* Pattern size value is not positive */
#define ENOPATNX	88	/* A representation for the specified pattern index has not been defined on this workstation */
#define	ENOPPTNX	89	/* A representation for the specified pattern index has not been predefined on this workstation */
#define ENOPSTYL	90	/* Interior style PATTERN is not supported on this workstation */
#define ECADIMEN	91	/* Dimensions of colour array are invalid */
#define ECINDXLZ	92	/* Colour index is less than zero */
#define EBADCOLX	93	/* Colour index is invalid */
#define ENOCOLRX	94	/* A representation for the specified colour index has not been defined on this workstation */
#define ENOPCLRX	95	/* A representation for the specified colour index has not been predefined on this workstation */
#define ECOLRNGE	96	/* Colour is outside range [0,1] */
#define EBADPICK	97	/* Pick identifier is invalid */

/*
 * Output Primitives
 */
#define ENPOINTS	100	/* Number of points is invalid */
#define ECHRCODE	101	/* Invalid code in string */
#define EBDGDPID	102	/* Generalized drawing primitive identifier is invalid */
#define EGDPDATA	103	/* Content of generalized drawing primitive data record is invalid */
#define	ECANTGDP	104	/* At least one active workstation is not able to generate the specified generalized drawing primitive */
#define	ECNTGDPC	105	/* At least one active workstation is not able to generate the specified generalized drawing primitive under the current transformations and clipping rectangle */

/*
 * Segments
 */
#define EBADNAME	120	/* Specified segment name is invalid */
#define ENAMUSED	121	/* Specified segment name is already in use */
#define EWHATSEG	122	/* Specified segment does not exist */
#define EWORKSEG	123	/* Specified segment does not exist on specified workstation */
#define EWISSSEG	124	/* Specified segment does not exist on Workstation Independent Segment Storage */
#define ESEGOPEN	125	/* Specified segment is open */
#define ESEGPRIR	126	/* Segment priority is outside the range [0,1] */

/*
 * Input
 */
#define ENOINDEV	140	/* Specified input device is not present on workstation */
#define EREQUEST	141	/* Input device is not in REQUEST mode */
#define ENSAMPLE	142	/* Input device is not in SAMPLE mode */
#define ENOEVSMP	143	/* EVENT and SAMPLE input mode are not available at this level of GKS */
#define ENOPETWS	144	/* Specified prompt and echo type is not supported on this workstation */
#define EEBOUNDS	145	/* Echo area is outside display space */
#define EBADDATA	146	/* Contents of input data record are invalid */
#define EINQOVFL	147	/* Input queue has overflowed */
#define ENOQOVFL	148	/* Input queue has not overflowed since GKS was opened or the last invocation of INQUIRE INPUT QUEUE OVERFLOW */
#define EASWSCLO	149	/* Input queue has overflowed, but associated workstation has been closed */
#define ENOCURIV	150	/* No input value of the correct class is in the current event report */
#define EINVTOUT	151	/* Timeout is invalid */
#define EBDINITV	152	/* Initial value is invalid */
#define ESTROKSZ	153	/* Number of points in the initial strokes is greater than the buffer size */
#define ESTRINSZ	154 	/* Length of the initial string is greater than the buffer size */

/*
 * Metafiles
 */
#define ERESERVE	160	/* Item type is not allowed for user items */
#define EBDLNGTH	161	/* Item length is invalid */
#define EMNOITEM	162	/* No item is left in GKS Metafile input */
#define EMITMINV	163	/* Metafile item is invalid */
#define ENOTGKSI	164	/* Item type is not a valid GKS item */
#define EBADCNTS	165	/* Content of item data record is invalid for the specified item type */
#define EEBDMXDR	166	/* Maximum item data record length is invalid */
#define EINTERPT	167	/* User item cannot be interpreted */
#define	ENOFUNCT	168	/* Specified function is not supported in this level of GKS */

/*
 * Escape
 */
#define ENOESCFN	180	/* Specified escape function is not supported */
#define	ESCIDINV	181	/* Specified escape function identification is invalid */
#define EESCDATA	182	/* Contents of escape data record are invalid */

/* 
 * Miscellaneous
 */
#define	EBDERRFL	200	/* Specified error file is invalid */

/* 
 * System
 */
#define EMEMSPAC	300	/* Storage overflow has occurred in GKS */
#define ESEGSPAC	301	/* Storage overflow has occurred in segment storage */
#define EIO_READ	302	/* Input/Output error has occurred while reading */
#define EIOWRITE	303	/* Input/Output error has occurred while writing */
#define EIOSENDD	304	/* Input/Output error has occurred while sending data to a workstation */
#define EIORECDA	305	/* Input/Output error has occurred while receiving data from a workstation */
#define EIOLIBMG	306	/* Input/Output error has occurred during program library management */
#define EIORDWDT	307	/* Input/Output error has occurred while reading workstation description table */
#define EMATHERR	308	/* Arithmetic error has occurred */
#define ESTRGERR	309	/* Character string does not match any known value */

/*
 * Language Binding Specific
 */
#define EBADENUM	2000	/* Enumeration type out of range */
#define EOUTSIZE	2001	/* Output parameter size insufficient */
#define EBADSMEM	2002	/* List element or set member not available */
#define EBADPKDR	2003	/* Invalid data record */
