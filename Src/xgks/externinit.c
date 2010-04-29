/*
 *	This file initializes all data objects having external linkage.
 *	They are segregated this way so that an XGKS shared-library may
 *	be built under SunOS.  See the SunOS "Programming Utilities
 *	and Libraries" for further information.
 */

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include "gks_implem.h"

/*	The data for predefined patterns */
static
Gint pttn0[] = {1, 0, 0, 0,
		1, 1, 0, 0,
		1, 1, 1, 0,
		1, 1, 1, 1};
static
Gint pttn1[] = {1, 1, 1, 1,
		1, 0, 0, 1,
		1, 0, 0, 1,
		1, 1, 1, 1};
static
Gint pttn2[] = {0, 0, 0, 0,
		0, 1, 1, 0,
		0, 1, 1, 0,
		0, 0, 0, 0};
static
Gint pttn3[] = {0, 0, 1, 1,
		0, 0, 1, 1,
		1, 1, 0, 0,
		1, 1, 0, 0};
static
Gint pttn4[] = {1, 0, 0, 0,
		1, 0, 0, 0,
		1, 0, 0, 0,
		1, 0, 0, 0};
static
Gint pttn5[] = {0, 0, 0, 0,
		0, 0, 0, 0,
		0, 0, 0, 0,
		1, 1, 1, 1};
static
Gint pttn6[] = {1, 1, 1, 0,
		1, 1, 1, 0,
		1, 1, 1, 0,
		1, 1, 1, 0};
static
Gint pttn7[] = {1, 1, 1, 1,
		1, 1, 1, 1,
		1, 1, 1, 1,
		0, 0, 0, 0};
static
Gint pttn8[] = {1, 1, 1, 0,
		1, 1, 0, 1,
		1, 0, 1, 1,
		0, 1, 1, 1};
static
Gint pttn9[] = {0, 0, 0, 1,
		0, 0, 1, 0,
		0, 1, 0, 0,
		1, 0, 0, 0};
static
Gint pttn10[] = {0, 1, 1, 1,
		1, 0, 1, 1,
		1, 1, 0, 1,
		1, 1, 1, 0};
static
Gint pttn11[] = {1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1};
static
Gint pttn12[] = {1, 0, 0, 0, 0, 0, 0, 0,
		1, 0, 0, 0, 0, 0, 0, 0,
		1, 0, 0, 0, 0, 0, 0, 0,
		1, 0, 0, 0, 0, 0, 0, 0, 
	        1, 0, 0, 0, 0, 0, 0, 0,
		1, 0, 0, 0, 0, 0, 0, 0,
		1, 0, 0, 0, 0, 0, 0, 0,
		1, 1, 1, 1, 1, 1, 1, 1};
static
Gint pttn13[] = {1, 0, 0, 0, 0, 0, 0, 1,
		0, 1, 0, 0, 0, 0, 1, 0,
		0, 0, 1, 0, 0, 1, 0, 0,
		0, 0, 0, 1, 1, 0, 0, 0, 
	        0, 0, 0, 1, 1, 0, 0, 0,
		0, 0, 1, 0, 0, 1, 0, 0,
		0, 1, 0, 0, 0, 0, 1, 0,
		1, 0, 0, 0, 0, 0, 0, 1};
static
Gint pttn14[] = {0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 
	        1, 1, 1, 1, 1, 1, 1, 1,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		1, 1, 1, 1, 1, 1, 1, 1};
static
Gint pttn15[] = {1, 0, 0, 1, 0, 0, 0, 0,
		1, 0, 0, 1, 0, 0, 0, 0,
		1, 0, 0, 1, 0, 0, 0, 0,
		1, 0, 0, 1, 0, 0, 0, 0, 
	        1, 0, 0, 1, 0, 0, 0, 0,
		1, 0, 0, 1, 0, 0, 0, 0,
		1, 0, 0, 1, 0, 0, 0, 0,
		1, 0, 0, 1, 0, 0, 0, 0};
static
Gint pttn16[] = {
	1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1,
	1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1,
	1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1,
	0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1,
	1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0,
	1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0,
	1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0,
	0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1,
	1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1,
	1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1,
	1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1,
	0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1,
	1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0,
	1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0,
	1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0,
	0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1
};
static
Gint pttn17[] = {
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1,
	1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1,
	1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0,
	0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0,
	1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1,
	1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1,
	1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0,
	0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0,
	1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0
};
static
Gint pttn18[] = {
	1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0,
	1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
	0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1,
	0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0,
	0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1,
	1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
	1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0,
	1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0,
	1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0,
	1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
	0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1,
	0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0,
	0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1,
	1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
	1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0,
	1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0
};
static
Gint pttn19[] = {
	1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
	0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1
};

Gptbundl xgks_xpttn[] = {
    {{4, 4}, pttn0},
    {{4, 4}, pttn1},
    {{4, 4}, pttn2},
    {{4, 4}, pttn3},
    {{4, 4}, pttn4},
    {{4, 4}, pttn5},
    {{4, 4}, pttn6},
    {{4, 4}, pttn7},
    {{4, 4}, pttn8},
    {{4, 4}, pttn9},
    {{4, 4}, pttn10},
    {{4, 4}, pttn11},
    {{8, 8}, pttn12},
    {{8, 8}, pttn13},
    {{8, 8}, pttn14},
    {{8, 8}, pttn15},
    {{16, 16}, pttn16},
    {{16, 16}, pttn17},
    {{16, 16}, pttn18},
    {{16, 16}, pttn19},
    {{4, 4}, pttn0}
};


/*
 *	The following tables of gks function names are added for c1130.
 *	when adding or deleting entries from this table, be sure to correct
 *	the range checking of statements located in the errorlog() function.
 */
char	*procname[] = {
    "",
    "gopengks()",
    "gclosegks()",
    "gopenws()",
    "gclosews()",
    "gactivatews()",
    "gdeactivatews()",
    "gclearws()",
    "gredrawsegws()",
    "gupdatews()",
    "gsetdeferst()",
    "gmessage()",
    "gescredrawnotify()",
    "gescsetcolourmask()",
    "gescsetdcsize()",
    "gescstoreprimi()",
    "gescinqxattr()",
    "gpolyline()",
    "gpolymarker()",
    "gtext()",
    "gfillarea()",
    "gcellarray()",
    "ggdp()",
    "gsetlineind()",
    "gsetlinetype()",
    "gsetlinewidth()",
    "gsetlinecolourind()",
    "gsetmarkerind()",
    "gsetmarkertype()",
    "gsetmarkersize()",
    "gsetmarkercolourind()",
    "gsettextind()",
    "gsettextfontprec()",
    "gsetcharexpan()",
    "gsetcharspace()",
    "gsettextcolourind()",
    "gsetcharheight()",
    "gsetcharup()",
    "gsettextpath()",
    "gsettextalign()",
    "gsetfillind()",
    "gsetfillintstyle()",
    "gsetfillstyleind()",
    "gsetfillcolourind()",
    "gsetpatsize()",
    "gsetpatrefpt()",
    "gsetasf()",
    "gsetpickid()",
    "gsetlinerep()",
    "gsetmarkerrep()",
    "gsetpatrep()",
    "gsetfillrep()",
    "gsettextrep()",
    "gsetcolourrep()",
    "gsetwindow()",
    "gsetviewport()",
    "gsetviewportinputpri()",
    "gselntran()",
    "gsetclip()",
    "gsetwswindow()",
    "gsetwsviewport()",
    "gcreateseg()",
    "gcloseseg()",
    "grenameseg()",
    "gdelseg()",
    "gdelsegws()",
    "gassocsegws()",
    "gcopysegws()",
    "ginsertseg()",
    "gsetsegtran()",
    "gsetvis()",
    "gsethighlight()",
    "gsetsegpri()",
    "gsetdet()",
    "gsetsegattr()",
    "ginitloc()",
    "ginitstroke()",
    "ginitval()",
    "ginitchoice()",
    "ginitpick()",
    "ginitstring()",
    "gsetlocmode()",
    "gsetstrokemode()",
    "gsetvalmode()",
    "gsetchoicemode()",
    "gsetpickmode()",
    "gsetstringmode()",
    "greqloc()",
    "greqstroke()",
    "greqval()",
    "greqchoice()",
    "greqpick()",
    "greqstring()",
    "gsampleloc()",
    "gsamplestroke()",
    "gsampleval()",
    "gsamplechoice()",
    "gsamplepick()",
    "gsamplestring()",
    "gawaitevent()",
    "gflushevents()",
    "ggetloc()",
    "ggetstroke()",
    "ggetval()",
    "ggetchoice()",
    "ggetpick()",
    "ggetstring()",
    "gwritegksm()",
    "ggettypegksm()",
    "greadgksm()",
    "ggetgksm()",
    "ginterpret()",
    "gevaltran()",
    "gaccumtran()",
    "gemergencyclosegks()",
    "gerrorhand()",
    "gerrorlog()",
    "ginqactivews()",
    "ginqasf()",
    "ginqassocws()",
    "ginqavailgdp()",
    "ginqavailwstypes()",
    "ginqchoicest()",
    "ginqclip()",
    "ginqcolourfacil()",
    "ginqcolourindices()",
    "ginqcolourrep()",
    "ginqcurntrannum()",
    "ginqdefchoice()",
    "ginqdefdeferst()",
    "ginqdefloc()",
    "ginqdefpick()",
    "ginqdefstring()",
    "ginqdefstroke()",
    "ginqdefval()",
    "ginqdisplayspacesize()",
    "ginqdisplaysize()",
    "ginqfillfacil()",
    "ginqfillindices()",
    "ginqfillrep()",
    "ginqgdp()",
    "ginqindivattr()",
    "ginqinputoverflow()",
    "ginqlevelgks()",
    "ginqlinefacil()",
    "ginqlineindices()",
    "ginqlinerep()",
    "ginqlocst()",
    "ginqmarkerfacil()",
    "ginqmarkerindices()",
    "ginqmarkerrep()",
    "ginqmaxntrannum()",
    "ginqmaxwssttables()",
    "ginqmodsegattr()",
    "ginqmodwsattr()",
    "ginqmoreevents()",
    "ginqnameopenseg()",
    "ginqntrannum()",
    "ginqntran()",
    "ginqnumavailinput()",
    "ginqnumsegpri()",
    "ginqopenws()",
    "ginqopst()",
    "ginqpatfacil()",
    "ginqpatindices()",
    "ginqpatrep()",
    "ginqcurpickid()",
    "ginqpickst()",
    "ginqpixel()",
    "ginqpixelarray()",
    "ginqpixelarraydim()",
    "ginqpredcolourrep()",
    "ginqpredfillrep()",
    "ginqpredlinerep()",
    "ginqpredmarkerrep()",
    "ginqpredpatrep()",
    "ginqpredtextrep()",
    "ginqprimattr()",
    "ginqsegattr()",
    "ginqsegnames()",
    "ginqsegnamesws()",
    "ginqstringst()",
    "ginqstrokest()",
    "ginqtextextent()",
    "ginqtextfacil()",
    "ginqtextindices()",
    "ginqtextrep()",
    "ginqvalst()",
    "ginqwscategory()",
    "ginqwsclass()",
    "ginqwsconntype()",
    "ginqwsdeferupdatest()",
    "ginqwsmaxnum()",
    "ginqwsst()",
    "ginqwstran()",
    "gurec()",
    "gprec()"
};


/*
 *	When adding or deleting entries from this table, be sure to correct
 *	the range checking of statements located in the errorlog() function.
 */
char	*procname1000[] = {
    "XgksChoUpdatePrompt()",
    "XgksEnqueueEvent()",
    "XgksLocUpdatePrompt()",
    "XgksStrUpdatePrompt()",
    "XgksPicUpdatePrompt()",
    "XgksInsertMesgPrimi()",
    "XgksDuplicatePrimi()",
    "XgksSegPrimiTran()",
    "XgksStkUpdatePrompt()",
    "XgksValUpdatePrompt()",
    "XgksDistFillarea()",
    "XgksInitWssFillArea()",
    "XgksDistCellarray()",
    "XgksExecData()",
    "xPolyMarker()",
    "xFillArea()",
    "xInqPixelarray()"
};


Gflbundl def_flbundl[PDF_FILL_BNDLS] = {
    /* predefined fill rep 1 */
    {
      GHOLLOW,		/* interior style */
      1,		/* style index */
      1			/* colour index */
    },
    /* predefined fill rep 2 */
    {
      GSOLID,		/* interior style */
      1,		/* style index */
      1			/* colour index */
    },
    /* predefined fill rep 3 */
    {
      GPATTERN,		/* interior style */
      1,		/* style index */
      1			/* colour index */
    },
    /* predefined fill rep 4 */
    {
      GHATCH,		/* interior style */
      -1,		/* style index */		/* c1143 c1174 */
      1			/* colour index */
    },
    /* predefined fill rep 5 */
    {
      GHATCH,		/* interior style */
      -10,		/* style index */		/* c1143 c1174 */
      1			/* colour index */
    }
};


Glnbundl def_lnbundl[PDF_LINE_BNDLS] = {
    /* predefined line rep 1 */
    {
      GLN_SOLID,	/* type */
      1.0,		/* width */
      1			/* colour */
    },
    /* predefined line rep 2 */
    {
      GLN_DASH,		/* type */
      1.0,		/* width */
      1			/* colour */
    },
    /* predefined line rep 3 */
    {
      GLN_DOT,		/* type */
      1.0,		/* width */
      1			/* colour */
    },
    /* predefined line rep 4 */
    {
      GLN_DOTDASH,	/* type */
      1.0,		/* width */
      1			/* colour */
    },
    /* predefined line rep 5 */
    {
      GLN_SOLID,	/* type */
      5.0,		/* width */
      1			/* colour */
    }
};


Gmkbundl def_mkbundl[PDF_MARK_BNDLS] = {
    /* predefined marker rep 1 */
    {
      GMK_STAR,		/* type */
      6.0,		/* size */
      1			/* colour */
    },
    /* predefined marker rep 2 */
    {
      GMK_POINT,	/* type */
      6.0,		/* size */
      1			/* colour */
    },
    /* predefined marker rep 3 */
    {
      GMK_PLUS,		/* type */
      6.0,		/* size */
      1			/* colour */
    },
    /* predefined marker rep 4 */
    {
      GMK_O,		/* type */
      6.0,		/* size */
      1			/* colour */
    },
    /* predefined marker rep 5 */
    {
      GMK_X,		/* type */
      6.0,		/* size */
      1			/* colour */
    }
};


/*	Predefined pattern attributes: */
PTATTR  def_ptattr = {
    { 1.0, 0.0 },   /* pattern width vector */       
    { 0.0, 1.0 },   /* pattern height vector */      
    { 0.0, 0.0 }    /* pattern reference point */    
};


Gtxbundl def_txbundl[PDF_TEXT_BNDLS] = {
    /* predefined text rep 1 */
    {
    {
      1,		/* font */
      GSTROKE		/* precision */
    },
      1.0,		/* expansion */
      0.15,		/* spacing */
      1			/* colour */
    },
    /* predefined text rep 2 */
    {
    {
      2,		/* font */
      GSTROKE		/* precision */
    },
      1.0,		/* expansion */
      0.15,		/* spacing */
      1			/* colour */
    },
    /* predefined text rep 3 */
    {
    {
      3,		/* font */
      GSTROKE		/* precision */
    },
      1.0,		/* expansion */
      0.2,		/* spacing */
      1			/* colour */
    },
    /* predefined text rep 4 */
    {
    {
      4,		/* font */
      GSTROKE		/* precision */
    },
      1.0,		/* expansion */
      0.15,		/* spacing */
      1			/* colour */
    },
    /* predefined text rep 5 */
    {
    {
      5,		/* font */
      GSTROKE		/* precision */
    },							/* d1 */
      1.0,		/* expansion */
      0.1,		/* spacing */
      1			/* colour */
    },
    /* predefined text rep 6 */
    {
    {
      6,		/* font */
      GSTROKE		/* precision */
    },
      1.0,		/* expansion */
      0.1,		/* spacing */
      1			/* colour */
    }
};


char	*progname	= NULL;


/*	Not static because the input devices need this information */
DashList xgksDASHES[10] = {
    { 2, {16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
    {12, {5, 5, 5, 5, 15, 5, 5, 5, 5, 5, 15, 5, 0, 0, 0, 0, 0} }, /*  |  */
    { 8, {20, 20, 20, 20, 20, 20, 20, 20, 0, 0, 0, 0, 0, 0, 0, 0, 0} },	/*  |  */
    { 4, {10, 8, 10, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} }, /*  \ /  */
    {16, {4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0} },	/*  v  */
    { 8, {8, 4, 4, 4, 8, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
    { 4, {40, 40, 40, 40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
    { 4, {40, 40, 40, 40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
    { 4, {40, 40, 40, 40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
    { 4, {40, 40, 40, 40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
    /*
	{2, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0},
	{6, 10, 6, 1, 4, 1, 6, 0, 0, 0, 0},
	{2, 3, 8, 0, 0, 0, 0, 0, 0, 0, 0},
	{2, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0},
	{2, 1, 6, 0, 0, 0, 0, 0, 0, 0, 0},
	{4, 8, 4, 1, 4, 0, 0, 0, 0, 0, 0},
	{2, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0},
	{2, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0},
	{2, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0},
	{2, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0},
    */
};
