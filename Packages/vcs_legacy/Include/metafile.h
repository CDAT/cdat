#ifndef XGKS_METAFILE_H
#define XGKS_METAFILE_H

/*
 * GKSM item identification numbers:
 */
typedef enum gksm_item_id {
    GKSM_CGM_ELEMENT			= -2,
    GKSM_UNKNOWN_ITEM			= -1,
    GKSM_END_ITEM			= 0,
    GKSM_CLEAR_WORKSTATION,
    GKSM_REDRAW_ALL_SEGMENTS_ON_WORKSTATION,
    GKSM_UPDATE_WORKSTATION,
    GKSM_DEFERRAL_STATE,
    GKSM_MESSAGE,
    GKSM_ESCAPE,
    GKSM_POLYLINE			= 11,
    GKSM_POLYMARKER,
    GKSM_TEXT,
    GKSM_FILL_AREA,
    GKSM_CELLARRAY,
    GKSM_GENERALIZED_DRAWING_PRIMITIVE,
    GKSM_POLYLINE_INDEX			= 21,
    GKSM_LINETYPE,
    GKSM_LINEWIDTH_SCALE_FACTOR,
    GKSM_POLYLINE_COLOUR_INDEX,
    GKSM_POLYMARKER_INDEX,
    GKSM_MARKER_TYPE,
    GKSM_MARKER_SIZE_SCALE_FACTOR,
    GKSM_POLYMARKER_COLOUR_INDEX,
    GKSM_TEXT_INDEX,
    GKSM_TEXT_FONT_AND_PRECISION,
    GKSM_CHARACTER_EXPANSION_FACTOR,
    GKSM_CHARACTER_SPACING,
    GKSM_TEXT_COLOUR_INDEX,
    GKSM_CHARACTER_VECTORS,
    GKSM_TEXT_PATH,
    GKSM_TEXT_ALIGNMENT,
    GKSM_FILL_AREA_INDEX,
    GKSM_FILL_AREA_INTERIOR_STYLE,
    GKSM_FILL_AREA_STYLE_INDEX,
    GKSM_FILL_AREA_COLOUR_INDEX,
    GKSM_PATTERN_SIZE,
    GKSM_PATTERN_REFERENCE_POINT,
    GKSM_ASPECT_SOURCE_FLAGS,
    GKSM_PICK_IDENTIFIER,
    GKSM_POLYLINE_REPRESENTATION	= 51,
    GKSM_POLYMARKER_REPRESENTATION,
    GKSM_TEXT_REPRESENTATION,
    GKSM_FILL_AREA_REPRESENTATION,
    GKSM_PATTERN_REPRESENTATION,
    GKSM_COLOUR_REPRESENTATION,
    GKSM_CLIPPING_RECTANGLE		= 61,
    GKSM_WORKSTATION_WINDOW		= 71,
    GKSM_WORKSTATION_VIEWPORT,
    GKSM_CREATE_SEGMENT			= 81,
    GKSM_CLOSE_SEGMENT,
    GKSM_RENAME_SEGMENT,
    GKSM_DELETE_SEGMENT,
    GKSM_SET_SEGMENT_TRANSFORMATION	= 91,
    GKSM_SET_VISIBILITY,
    GKSM_SET_HIGHLIGHTING,
    GKSM_SET_SEGMENT_PRIORITY,
    GKSM_SET_DETECTABILITY,
    GKSM_USER_ITEM			= 101	/* NB: just an indicator; not 
						 * the actual value */
}	gksm_item_id;

/*
 * Suitable for Item type :
 *	0  - END ITEM
 *	2  - REDRAW ALL SEGMENTS ON WORKSTATION
 *	82 - CLOSE SEGMENT
 *
 *	XGKSM0
 */

/*
 * Suitable for Item type :
 *	1  - CLEAR WORKSTAION
 *	3  - UPDATE WORKSTAION
 *	21 - POLYLINE INDEX
 *	22 - LINETYPE
 *	24 - POLYLINE COLOUR INDEX
 *	25 - POLYMARKER INDEX
 *	26 - MARKER TYPE
 *	28 - POLYMARKER COLOUR INDEX
 *	29 - TEXT INDEX
 *	33 - TEXT COLOUR INDEX
 *	35 - TEXT PATH
 *	37 - FILL AREA INDEX
 *	38 - FILL AREA INTERIOR STYLE
 *	39 - FILL AREA STYLE INDEX
 *	40 - FILL AREA COLOUR INDEX
 *	44 - PICK IDENTIFIER
 *	81 - CREATE SEGMENT
 *	84 - DELETE SEGMENT
 */
typedef struct {
    Gint            flag;
}               XGKSMONE;


/*
 * Suitable for Item Type :
 *	4  - DEFERRAL STATE
 *	30 - TEXT FONT AND PRECISION
 *	36 - TEXT ALIGNMENT
 *	83 - RENAME SEGMENT
 *	92 - SET SEGMENT VISIBILITY
 *	93 - SET SEGMENT HIGHLIGHT
 *	95 - SET SEGMENT DETECTABLILITY
 */
typedef struct {
    Gint            item1, item2;
}               XGKSMTWO;


/*
 * Suitable for MESSAGE :
 *	5 - XgksMoMessage
 */
typedef struct {
    Gint            strlen;
    Gchar          *string;
}               XGKSMMESG;


/*
 * Suitable for item type :
 *	11 - POLYLINE
 *	12 - POLYMARKER
 *	14 - FILL AREA
 */
typedef struct {
    Gint            num_pts;
    Gpoint         *pts;
}               XGKSMGRAPH;


/*
 * Suitable for TEXT
 *	13 - XgksMoText
 */
typedef struct {
    Gpoint          location;
    Gint            strlen;
    Gchar          *string;
}               XGKSMTEXT;


/*
 * Suitablr for Cell Array
 *	15 - XgksMoCellArray
 */
typedef struct {
    Gpoint          ll, ur, lr;
    Gipoint         dim;
    Gint           *colour;
}               XGKSMCELLARRAY;


/*
 * Suitable for item type :
 *	23 - LINE WIDTH SCALE FACTOR
 *	27 - MARKER SIZE SCALE FACTOR
 *	31 - CHARACTER EXPANSION FACTOR
 *	32 - CHARACTER SPACING
 */
typedef struct {
    Gfloat          size;
}               XGKSMSIZE;


/*
 * Suitable for CHARACTER VECTRO
 *	34 - XgksMoSetCharVec
 */
typedef struct {
    Gpoint          up, base;
}               XGKSMCHARVEC;


/*
 * Suitable for ASPECT SOURCE FALGS
 *	43 - XgksMoSetAsf
 *
 * There's an extra slot at the end to accomodate the way cgm/cgm.c handles
 * ASF's.
 */
typedef struct {
    Gint            asf[13+1];
}               XGKSMASF;


/*
 * Suitable for item type :
 *	51 - POLYLINE REPRESENTATION
 *	52 - POLYMARKER REPRESENTATION
 */
typedef struct {
    Gint            idx, style, colour;
    Gfloat          size;
}               XGKSMLMREP;


/*
 * Suitable for : TEXT REPRESENTATION
 *	53 - XgksMoSetTextRep
 */
typedef struct {
    Gint            idx, font, prec, colour;
    Gfloat          tx_exp, space;
}               XGKSMTEXTREP;


/*
 * Suitable for FILL AREA REPRESENTATION
 *	54 - XgksMoSetFillRep
 */
typedef struct {
    Gint            idx, intstyle, style, colour;
}               XGKSMFILLREP;


/*
 * Suitable for PATTERN REPRESENTATION
 *	55 - XgksMoSegPatRep
 */
typedef struct {
    Gint            idx;
    Gipoint         size;
    Gint           *array;
}               XGKSMPATREP;


/*
 * Suitable For COLOUR REPRESENTATION
 *	56 - XgksMoSetColourRep
 */
typedef struct {
    Gint            idx;
    Gfloat          red, green, blue;
}               XGKSMCOLOURREP;


/*
 * Suitable for item type :
 *	61 - CLIPPING RECTANGLE
 *	71 - WORKSTATION WINDOW
 *	72 - WORKSTATION VIEWPORT
 */
typedef struct {
    Glimit          rect;
}               XGKSMLIMIT;


/*
 * Suitable for SET SEGMENT TRANSFORMATION
 *	91 - XgksMoSegSegTrans
 */
typedef struct {
    Gint            name;
    Gfloat          matrix[2][3];
}               XGKSMSEGTRAN;


/*
 * Suitable for SET SEGMENT PRIORITY
 *	94 - XgksMoSegSegPri
 */
typedef struct {
    Gint            name;
    Gfloat          pri;
}               XGKSMSEGPRI;


/*
 * Suitable for SET PATTERN SIZE
 *       41 - XgksMoSetPatSiz
 */
typedef struct {
    Gpoint          wid;
    Gpoint          hgt;
}               XGKSMPATSIZ;


/*
 * Suitable for SET PATTERN REFERENCE PT
 *        42 - XgksMoSetPatRef
 */
typedef struct {
    Gpoint          ref;
}               XGKSMPATREF;


/*
 * Metafile API:
 */
extern int XgksMiOpenWs		PROTO((WS_STATE_PTR ws));
extern int XgksMoOpenWs		PROTO((WS_STATE_PTR ws));
extern int XgksMiCloseWs	PROTO((WS_STATE_PTR ws));
extern int XgksMoCloseWs	PROTO((WS_STATE_PTR ws));
extern int XgksMoClearWs	PROTO((WS_STATE_PTR ws, Gclrflag flag));
extern int XgksMoReDrawAllSeg	PROTO((WS_STATE_PTR ws));
extern int XgksMoUpdateWs	PROTO((WS_STATE_PTR ws, Gregen regenflag));
extern int XgksMoDeferWs	PROTO((WS_STATE_PTR ws, Gdefmode defer_mode, 
				       Girgmode regen_mode));
extern int XgksMoMessage	PROTO((WS_STATE_PTR ws, Gchar *string));
extern int XgksMoGraphicOutputToWs	PROTO((WS_STATE_PTR ws, Gint code, 
					       Gint num_pt, Gpoint *pos));
extern int XgksMoGraphicOutput	PROTO((Gint code, Gint num_pt, Gpoint *pos));
extern int XgksMoTextToWs	PROTO((WS_STATE_PTR ws, Gpoint *at, 
				       Gchar *string));
extern int XgksMoText		PROTO((Gpoint *at, Gchar *string));
extern int XgksMoCellArrayToWs	PROTO((WS_STATE_PTR ws, Gpoint *ll, Gpoint *ur,
				       Gpoint *lr, 
				       Gint row, Gint *colour, Gipoint *dim));
extern int XgksMoCellArray	PROTO((Gpoint *ll, Gpoint *ur, Gpoint *lr,
				       Gint row, Gint *colour, Gipoint *dim));
extern int XgksMoSetGraphicSizeOnWs	PROTO((WS_STATE_PTR ws, Gint code, 
					       double size));
extern int XgksMoSetGraphicSize	PROTO((Gint code, double size));
extern int XgksMoCloseSegOnWs	PROTO((WS_STATE_PTR ws));
extern int XgksMoCloseSeg	PROTO((void));
extern int XgksMoSetGraphicAttrOnWs	PROTO((WS_STATE_PTR ws, Gint code, 
					      Gint attr));
extern int XgksMoSetGraphicAttr	PROTO((Gint code, Gint attr));
extern int XgksMoSetTextFPOnWs	PROTO((WS_STATE_PTR ws, Gtxfp *txfp));
extern int XgksMoSetTextFP	PROTO((Gtxfp *txfp));
extern int XgksMoSetCharUpOnWs	PROTO((WS_STATE_PTR ws, Gpoint *up,
				       Gpoint *base));
extern int XgksMoSetCharUp	PROTO((void));
extern int XgksMoSetTextPathOnWs	PROTO((WS_STATE_PTR ws, Gtxpath path));
extern int XgksMoSetTextPath	PROTO((Gtxpath path));
extern int XgksMoSetTextAlignOnWs	PROTO((WS_STATE_PTR ws, 
					      Gtxalign *align));
extern int XgksMoSetTextAlign	PROTO((Gtxalign *align));
extern int XgksMoSetFillIntStyleOnWs	PROTO((WS_STATE_PTR ws, 
					      Gflinter style));
extern int XgksMoSetFillIntStyle	PROTO((Gflinter style));
extern int XgksMoSetPatSizeOnWs	PROTO((WS_STATE_PTR ws));
extern int XgksMoSetPatSize	PROTO((void));
extern int XgksMoSetPatRefOnWs	PROTO((WS_STATE_PTR ws));
extern int XgksMoSetPatRef	PROTO((void));
extern int XgksMoSetAsfOnWs	PROTO((WS_STATE_PTR ws));
extern int XgksMoSetAsf		PROTO((void));
extern int XgksMoSetLineMarkRep	PROTO((WS_STATE_PTR ws, Gint code, Gint idx, 
				       Gint type, double size, Gint colour));
extern int XgksMoSetTextRep	PROTO((WS_STATE_PTR ws, Gint idx, Gtxbundl *rep));
extern int XgksMoSetFillRep	PROTO((WS_STATE_PTR ws, Gint idx, Gflbundl *rep));
extern int XgksMoSetPatRep	PROTO((WS_STATE_PTR ws, Gint idx, Gptbundl *rep));
extern int XgksMoSetColourRep	PROTO((WS_STATE_PTR ws, Gint idx, Gcobundl *rep));
/*		RLM  replaced Dec 15, 1994.			*/
/*extern int XgksMoSetClipOnWs	PROTO((WS_STATE_PTR ws, Glimit *rect));  */
extern int XgksMoSetClipOnWs	PROTO((WS_STATE_PTR ws, Gcliprec *rect));
/*		RLM  replaced Dec 15, 1994.			*/
/*extern int XgksMoSetClip	PROTO((Glimit *rect));		*/
extern int XgksMoSetClip	PROTO((Gcliprec *crect));
extern int XgksMoSetLimit	PROTO((WS_STATE_PTR ws, Gint code, 
				       Glimit *rect));
extern int XgksMoRenameSeg	PROTO((Gint old, Gint new));
extern int XgksMoSetSegTransOnWs	PROTO((WS_STATE_PTR ws, Gint name, 
					       Gfloat matrix[2][3]));
extern int XgksMoSetSegTrans	PROTO((Gint name, Gfloat matrix[2][3]));
extern int XgksMoSetSegAttrOnWs	PROTO((WS_STATE_PTR ws, Gint name, Gint code,
				       Gint attr));
extern int XgksMoSetSegVis	PROTO((Gint name, Gsegvis vis));
extern int XgksMoSetSegHiLight	PROTO((Gint name, Gseghi hilight));
extern int XgksMoSetSegPriOnWs	PROTO((WS_STATE_PTR ws, Gint name, double pri));
extern int XgksMoSetSegPri	PROTO((Gint name, double pri));
extern int XgksMoSetSegDet	PROTO((Gint name, Gsegdet det));
extern int XgksMoActivateWs	PROTO((WS_STATE_PTR ws));
extern int XgksInitGksM		PROTO((void));

#endif	/* XGKS_METAFILE_H not defined above */
