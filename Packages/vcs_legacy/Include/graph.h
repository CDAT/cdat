#include "vcs_legacy_names_length.h"
/* Defines he default time untis */
#define VCS_DEFAULT_TIME_UNITS "days since 2000\0"
#define VCS_DEFAULT_CALENDAR 135441
/*  The following are graphic display attributes.			*/

	struct iso
	  {
	   int id;		/* Identification, used for replace.	*/
	   int p;		/* Internal priority for isolines.	*/
	   float lev;		/* Level value.				*/
	   float incr;		/* Increment between other levels using
				   lev as a reference.  If zero no other
				   levels will be displayed.		*/
	   int hici;		/* Label highlight color index.		*/
	   char lab[13];	/* Label string, if "*", labels will be
				   created from levels.  When incr is
				   given (non-zero) this label will be
				   displayed for all unless it's "*".	*/
	   char lb[17];		/* Line bundle table name.		*/
	   char tb[17];		/* Text bundle table name.		*/
	   char to[17];		/* Text orientation table name.		*/
	   int cw;               /* draw arrow clockwise/no arrow/counter clockwise (1/0/-1) */
	    float ls;            /* scale factor from default length */
	    float angle;         /* angle for barbs */
	    float spc;          /* spacing factor relative to default spacing between arrows */
	   struct iso *next;	/* Pointer to next set of isolines.	*/
	  };

/*		Graphics isolines.					*/

/*		The tics and labels are defined by lists.  To cause the
		values to be computed at display time as a function of
		the display space in World Coordinates use "*" instead
		of a list name.  NULL indicates no display.  Either an
		"*" or a non-existent list name will cause the values
		to be computed, however, non-existent list names will
		be indicated with a warning message.  			*/

	struct gi_attr
	  {
	   char proj[VCS_MX_NM_LEN];	/* Projection name.		*/
	   char xtl1[256];	/* First set of x tics labels =L_name.	*/
	   char xtl2[256];	/* Second set of x tics labels =L_name.	*/
	   char xmt1[256];	/* Minor x tics first set =L_name.	*/
	   char xmt2[256];	/* Minor x tics second set =L_name.	*/
	   char ytl1[256];	/* First set of y tics labels =L_name.	*/
	   char ytl2[256];	/* Second set of y tics labels=L_name.	*/
	   char ymt1[256];	/* Minor y tics first set =L_name.	*/
	   char ymt2[256];	/* Minor y tics second set =L_name.	*/
	   float dsp[4]	;	/* Display space (x1,y1,x2,y2) U.C..	*/
	   char xat[17] ;	/* X-axis transform to (log,ln,exp,cos) */
	   char yat[17] ;	/* Y-axis transform to (log,ln,exp,cos) */
	   int labels;		/* Indicate if labels displayed (y/n)	*/
	   char timeunits[256];    /* units for time components based vlues */
	   int calendar;       /* calendar type for time based values */
	   int idsp[4]	;	/* Display space (x1,y1,x2,y2) time coord ? */
	   struct iso *line;	/* Isoline definitions.			*/
	  };

	struct gi_tab
	  {
	   char name[17];	/* Graphic isoline attributes name.	*/
	   struct gi_attr *pGi_attr;/* pointer to graphic isoline attribute
							structure.	*/
	   struct gi_tab *next;	/* Pointer to next table entry.		*/
	  };


/*		Graphics Outlines.					*/

/*		The tics and labels are defined by lists.  To cause the
		values to be computed at display time as a function of
		the display space in World Coordinates use "*" instead
		of a list name.  NULL indicates no display.  Either an
		"*" or a non-existent list name will cause the values
		to be computed, however, non-existent list names will
		be indicated with a warning message.  			*/

	struct go_attr
	  {
	   char proj[VCS_MX_NM_LEN];	/* Projection name.		*/
	   char xtl1[256];	/* First set of x tics labels =L_name.	*/
	   char xtl2[256];	/* Second set of x tics labels =L_name.	*/
	   char xmt1[256];	/* Minor x tics first set =L_name.	*/
	   char xmt2[256];	/* Minor x tics second set =L_name.	*/
	   char ytl1[256];	/* First set of y tics labels =L_name.	*/
	   char ytl2[256];	/* Second set of y tics labels=L_name.	*/
	   char ymt1[256];	/* Minor y tics first set =L_name.	*/
	   char ymt2[256];	/* Minor y tics second set =L_name.	*/
	   float dsp[4]	;	/* Display space (x1,y1,x2,y2) U.C..	*/
	   char xat[17] ;	/* X-axis transform to (log,ln,exp,cos) */
	   char yat[17] ;	/* Y-axis transform to (log,ln,exp,cos) */
	   char lb[17];		/* Line bundle name.			*/
	   int out[10];		/* List of values to be outlined.	*/
	   int n;		/* Number of values to be outlined.	*/
	   char timeunits[256];    /* units for time components based vlues */
	   int calendar;       /* calendar type for time based values */
	   int idsp[4]	;	/* Display space (x1,y1,x2,y2) time coord ? */
	  };

	struct go_tab
	  {
	   char name[17];	/* Graphic outline attributes name.	*/
	   struct go_attr *pGo_attr;/* pointer to graphic outline attribute
							structure.	*/
	   struct go_tab *next;	/* Pointer to next table entry.		*/
	  };

	struct fill_range
	  {
	   int id;		/* Identification, used for replace.	*/
	   float lev1;		/* First level value.			*/
	   float lev2;		/* Second level value.			*/
	   char fill_name[17];	/* Fill attributes name.		*/
	   struct fill_range *next;/* Pointer to next set of isolines.	*/
	  };

/*		Graphics fill between isolines.				*/

/*		The tics and labels are defined by lists.  To cause the
		values to be computed at display time as a function of
		the display space in World Coordinates use "*" instead
		of a list name.  NULL indicates no display.  Either an
		"*" or a non-existent list name will cause the values
		to be computed, however, non-existent list names will
		be indicated with a warning message.  			*/

	struct gfi_attr
	  {
	   char proj[VCS_MX_NM_LEN];	/* Projection name.		*/
	   char xtl1[256];	/* First set of x tics labels =L_name.	*/
	   char xtl2[256];	/* Second set of x tics labels =L_name.	*/
	   char xmt1[256];	/* Minor x tics first set =L_name.	*/
	   char xmt2[256];	/* Minor x tics second set =L_name.	*/
	   char ytl1[256];	/* First set of y tics labels =L_name.	*/
	   char ytl2[256];	/* Second set of y tics labels=L_name.	*/
	   char ymt1[256];	/* Minor y tics first set =L_name.	*/
	   char ymt2[256];	/* Minor y tics second set =L_name.	*/
	   float dsp[4]	;	/* Display space (x1,y1,x2,y2) U.C..	*/
	   char xat[17] ;	/* X-axis transform to (log,ln,exp,cos) */
	   char yat[17] ;	/* Y-axis transform to (log,ln,exp,cos) */
	   float missing;	/* Color to use for filling missing data.*/
	   char timeunits[256];    /* units for time components based vlues */
	   int calendar;       /* calendar type for time based values */
	   int idsp[4]	;	/* Display space (x1,y1,x2,y2) time coord ? */
	   char *legend;	/* Holds legend list values.		*/
	   struct fill_range *line;/* Range definitions.		*/
	  };

	struct gfi_tab
	  {
	   char name[17];  /* Graphic fill isoline attributes name.	*/
	   struct gfi_attr *pGfi_attr;/* pointer to graphic fill between
					isolines attribute structure.	*/
	   struct gfi_tab *next;/* Pointer to next table entry.		*/
	  };

/*		Fill continental outlines.				*/

	struct gfo_attr
	  {
	   char proj[VCS_MX_NM_LEN];	/* Projection name.		*/
	   char xtl1[256];	/* First set of x tics labels =L_name.	*/
	   char xtl2[256];	/* Second set of x tics labels =L_name.	*/
	   char xmt1[256];	/* Minor x tics first set =L_name.	*/
	   char xmt2[256];	/* Minor x tics second set =L_name.	*/
	   char ytl1[256];	/* First set of y tics labels =L_name.	*/
	   char ytl2[256];	/* Second set of y tics labels=L_name.	*/
	   char ymt1[256];	/* Minor y tics first set =L_name.	*/
	   char ymt2[256];	/* Minor y tics second set =L_name.	*/
	   float dsp[4]	;	/* Display space (x1,y1,x2,y2) U.C..	*/
	   char xat[17] ;	/* X-axis transform to (log,ln,exp,cos) */
	   char yat[17] ;	/* Y-axis transform to (log,ln,exp,cos) */
	   char f[17];		/* Fill bundle name.			*/
	   int out[10];		/* List of values to be outlined.	*/
	   int n;		/* Number of values to be outlined.	*/
	   char timeunits[256];    /* units for time components based vlues */
	   int calendar;       /* calendar type for time based values */
	   int idsp[4]	;	/* Display space (x1,y1,x2,y2) time coord ? */
	  };

	struct gfo_tab
	  {
	   char name[17];	/* Graphic fill outline attributes name.*/
	   struct gfo_attr *pGfo_attr;/* pointer to graphic fill outline
				 		attribute structure.	*/
	   struct gfo_tab *next;/* Pointer to next table entry.		*/
	  };
	struct gcon_attr
	  {
	   char proj[VCS_MX_NM_LEN];	/* Projection name.		*/
	   char xtl1[256];	/* First set of x tics labels =L_name.	*/
	   char xtl2[256];	/* Second set of x tics labels =L_name.	*/
	   char xmt1[256];	/* Minor x tics first set =L_name.	*/
	   char xmt2[256];	/* Minor x tics second set =L_name.	*/
	   char ytl1[256];	/* First set of y tics labels =L_name.	*/
	   char ytl2[256];	/* Second set of y tics labels=L_name.	*/
	   char ymt1[256];	/* Minor y tics first set =L_name.	*/
	   char ymt2[256];	/* Minor y tics second set =L_name.	*/
	   float dsp[4]	;	/* Display space (x1,y1,x2,y2) U.C..	*/
	   char lb[17];		/* Line bundle name.			*/
	   int  cont_type;	/* Type of continent: None, Fine, etc.  */
	   char timeunits[256];    /* units for time components based vlues */
	   int calendar;       /* calendar type for time based values */
	   int idsp[4]	;	/* Display space (x1,y1,x2,y2) time coord ? */
	  };

	struct gcon_tab
	  {
	   char name[17];	/* Continents attributes name.	*/
	   struct gcon_attr *pGcon_attr;/* pointer to continents attribute
							structure.	*/
	   struct gcon_tab *next;/* Pointer to next table entry.	*/
	  };

	struct default_continents
	  {
	   int selected;		/* index of selected default	*/
	   char lb[17];			/* line attributes name		*/
	   char options[6][17];		/* options for the default	*/
	  };

/*		Graphics box fill.					*/

/*		The tics and labels are defined by lists.  To cause the
		values to be computed at display time as a function of
		the display space in World Coordinates use "*" instead
		of a list name.  NULL indicates no display.  Either an
		"*" or a non-existent list name will cause the values
		to be computed, however, non-existent list names will
		be indicated with a warning message.  			*/

	struct gfb_attr
	  {
	   char proj[VCS_MX_NM_LEN];	/* Projection name.		*/
	   char xtl1[256];	/* First set of x tics labels =L_name.	*/
	   char xtl2[256];	/* Second set of x tics labels =L_name.	*/
	   char xmt1[256];	/* Minor x tics first set =L_name.	*/
	   char xmt2[256];	/* Minor x tics second set =L_name.	*/
	   char ytl1[256];	/* First set of y tics labels =L_name.	*/
	   char ytl2[256];	/* Second set of y tics labels=L_name.	*/
	   char ymt1[256];	/* Minor y tics first set =L_name.	*/
	   char ymt2[256];	/* Minor y tics second set =L_name.	*/
	   float dsp[4]	;	/* Display space (x1,y1,x2,y2) U.C..	*/
	   char xat[17] ;	/* X-axis transform to (log,ln,exp,cos) */
	   char yat[17] ;	/* Y-axis transform to (log,ln,exp,cos) */
	   float lev1,lev2;	/* Data range that matches color range.	*/
	   int color_1,color_2;	/* Color index range for box fill.	*/
	   int boxfill_type;	/* Produce legend from which method (0,1,2)   *
	            * 0 - linear - VCS will use compute or list legend values *
	            * 2 - log10  - VCS will plot the data using log10         *
	            * 3 - custom - VCS will compute legend using custom       *
				*     values and display legend values evenly */
	   char *legend;	/* Holds legend list values.		*/
	   int ext_1,ext_2;	/* Indicate extend beyond range (y/n).	*/
				/* Use color_n index for the extension.	*/
	   int missing;		/* Color to use for filling missing data.*/
	   char timeunits[256];    /* units for time components based vlues */
	   int calendar;       /* calendar type for time based values */
	   int idsp[4]	;	/* Display space (x1,y1,x2,y2) time coord ? */
	   struct fill_range *line;/* Range definitions.		*/
	  };

	struct gfb_tab
	  {
	   char name[17];	/* Graphic box fill attributes name.	*/
	   struct gfb_attr *pGfb_attr;/* pointer to graphic box fill
					 attribute structure.		*/
	   struct gfb_tab *next;/* Pointer to next table entry.		*/
	  };



/*			Graphics vectors.				*/

	struct gv_attr
	  {
	   char proj[VCS_MX_NM_LEN];	/* Projection name.		*/
	   char xtl1[256];	/* First set of x tics labels =L_name.	*/
	   char xtl2[256];	/* Second set of x tics labels =L_name.	*/
	   char xmt1[256];	/* Minor x tics first set =L_name.	*/
	   char xmt2[256];	/* Minor x tics second set =L_name.	*/
	   char ytl1[256];	/* First set of y tics labels =L_name.	*/
	   char ytl2[256];	/* Second set of y tics labels=L_name.	*/
	   char ymt1[256];	/* Minor y tics first set =L_name.	*/
	   char ymt2[256];	/* Minor y tics second set =L_name.	*/
	   float dsp[4]	;	/* Display space (x1,y1,x2,y2) U.C..	*/
	   char xat[17] ;	/* X-axis transform to (log,ln,exp,cos) */
	   char yat[17] ;	/* Y-axis transform to (log,ln,exp,cos) */
	   char lb[17];		/* Line attribute set name.		*/
	   float vsf;		/* Vector length scale factor.		*/
	   int vpos;		/* Vector position: 'h' head at point.
						    'c' center at point.
						    't' tail at point.	*/
	   int vtype;		/* Vector type:

				Show magnitude (vsf*0.02NDC*v/10=length)

						 1 = arrow head.
						 2 = arrow tail.
						 3 = arrow head and tail.

						(vsfx0.02NDC = fixed length)

						10 = wind barbs (10 full)
								( 5 half)
				Direction only  (vsfx0.02NDC = fixed length)

						11 = arrow head.
						12 = arrow tail.
						13 = arrow head and tail.
									*/
	   float vlen;		/* Vector length for the legend.	*/
	   char timeunits[256];    /* units for time components based vlues */
	   int calendar;       /* calendar type for time based values */
	   int idsp[4]	;	/* Display space (x1,y1,x2,y2) time coord ? */

	  };


	struct gv_tab
	  {
	   char name[17];	/* Graphic vector attributes name.	*/
	   struct gv_attr *pGv_attr;/* pointer to graphic vector
					 attribute structure.		*/
	   struct gv_tab *next;/* Pointer to next table entry.		*/
	  };
/*			Graphics X(y) vs y				*/

	struct gXy_attr
	  {
	   char proj[VCS_MX_NM_LEN];	/* Projection name.		*/
	   char xtl1[256];	/* First set of x tics labels =L_name.	*/
	   char xtl2[256];	/* Second set of x tics labels =L_name.	*/
	   char xmt1[256];	/* Minor x tics first set =L_name.	*/
	   char xmt2[256];	/* Minor x tics second set =L_name.	*/
	   char ytl1[256];	/* First set of y tics labels =L_name.	*/
	   char ytl2[256];	/* Second set of y tics labels=L_name.	*/
	   char ymt1[256];	/* Minor y tics first set =L_name.	*/
	   char ymt2[256];	/* Minor y tics second set =L_name.	*/
	   float dsp[4]	;	/* Display space (x1,y1,x2,y2) U.C..	*/
	   char xat[17] ;	/* X-axis transform to (log,ln,exp,cos) */
	   char yat[17] ;	/* Y-axis transform to (log,ln,exp,cos) */
	   char lb[17];		/* Line attribute set name.		*/
	   char mb[17];		/* Marker attribute set name.		*/
	   char timeunits[256];    /* units for time components based vlues */
	   int calendar;       /* calendar type for time based values */
	   int idsp[4]	;	/* Display space (x1,y1,x2,y2) time coord ? */
	  };


	struct gXy_tab
	  {
	   char name[17];	/* Graphic X(y) vs y attributes name.	*/
	   struct gXy_attr *pGXy_attr;/* pointer to graphic vector
					 attribute structure.		*/
	   struct gXy_tab *next;/* Pointer to next table entry.		*/
	  };

/*			Graphics Y(x) vs x				*/

	struct gYx_attr
	  {
	   char proj[VCS_MX_NM_LEN];	/* Projection name.		*/
	   char xtl1[256];	/* First set of x tics labels =L_name.	*/
	   char xtl2[256];	/* Second set of x tics labels =L_name.	*/
	   char xmt1[256];	/* Minor x tics first set =L_name.	*/
	   char xmt2[256];	/* Minor x tics second set =L_name.	*/
	   char ytl1[256];	/* First set of y tics labels =L_name.	*/
	   char ytl2[256];	/* Second set of y tics labels=L_name.	*/
	   char ymt1[256];	/* Minor y tics first set =L_name.	*/
	   char ymt2[256];	/* Minor y tics second set =L_name.	*/
	   float dsp[4]	;	/* Display space (x1,y1,x2,y2) U.C..	*/
	   char xat[17] ;	/* X-axis transform to (log,ln,exp,cos) */
	   char yat[17] ;	/* Y-axis transform to (log,ln,exp,cos) */
	   char lb[17];		/* Line attribute set name.		*/
	   char mb[17];		/* Marker attribute set name.		*/
	   char timeunits[256];    /* units for time components based vlues */
	   int calendar;       /* calendar type for time based values */
	   int idsp[4]	;	/* Display space (x1,y1,x2,y2) time coord ? */
	  };


	struct gYx_tab
	  {
	   char name[17];	/* Graphic Y(x) vs x attributes name.	*/
	   struct gYx_attr *pGYx_attr;/* pointer to graphic vector
					 attribute structure.		*/
	   struct gYx_tab *next;/* Pointer to next table entry.		*/
	  };

/*			Graphics X(t) vs Y(t)				*/

	struct gXY_attr
	  {
	   char proj[VCS_MX_NM_LEN];	/* Projection name.		*/
	   char xtl1[256];	/* First set of x tics labels =L_name.	*/
	   char xtl2[256];	/* Second set of x tics labels =L_name.	*/
	   char xmt1[256];	/* Minor x tics first set =L_name.	*/
	   char xmt2[256];	/* Minor x tics second set =L_name.	*/
	   char ytl1[256];	/* First set of y tics labels =L_name.	*/
	   char ytl2[256];	/* Second set of y tics labels=L_name.	*/
	   char ymt1[256];	/* Minor y tics first set =L_name.	*/
	   char ymt2[256];	/* Minor y tics second set =L_name.	*/
	   float dsp[4]	;	/* Display space (x1,y1,x2,y2) U.C..	*/
	   char xat[17] ;	/* X-axis transform to (log,ln,exp,cos) */
	   char yat[17] ;	/* Y-axis transform to (log,ln,exp,cos) */
	   char lb[17];		/* Line attribute set name.		*/
	   char mb[17];		/* Marker attribute set name.		*/
	   char timeunits[256];    /* units for time components based vlues */
	   int calendar;       /* calendar type for time based values */
	   int idsp[4]	;	/* Display space (x1,y1,x2,y2) time coord ? */
	  };


	struct gXY_tab
	  {
	   char name[17];	/* Graphic X(t) vs Y(t) attributes name.*/
	   struct gXY_attr *pGXY_attr;/* pointer to graphic X(t) vs Y(t)
					 attribute structure.		*/
	   struct gXY_tab *next;/* Pointer to next table entry.	*/
	  };

/*	Graphics X([x][,y][,z][,t]) vs Y([x][,y][,z][,t]) ScatterPlot	*/

	struct gSp_attr
	  {
	   char proj[VCS_MX_NM_LEN];	/* Projection name.		*/
	   char xtl1[256];	/* First set of x tics labels =L_name.	*/
	   char xtl2[256];	/* Second set of x tics labels =L_name.	*/
	   char xmt1[256];	/* Minor x tics first set =L_name.	*/
	   char xmt2[256];	/* Minor x tics second set =L_name.	*/
	   char ytl1[256];	/* First set of y tics labels =L_name.	*/
	   char ytl2[256];	/* Second set of y tics labels=L_name.	*/
	   char ymt1[256];	/* Minor y tics first set =L_name.	*/
	   char ymt2[256];	/* Minor y tics second set =L_name.	*/
	   float dsp[4]	;	/* Display space (x1,y1,x2,y2) U.C..	*/
	   char xat[17] ;	/* X-axis transform to (log,ln,exp,cos) */
	   char yat[17] ;	/* Y-axis transform to (log,ln,exp,cos) */
	   char mb[17];		/* Marker attribute set name.		*/
	   char timeunits[256];    /* units for time components based vlues */
	   int calendar;       /* calendar type for time based values */
	   int idsp[4]	;	/* Display space (x1,y1,x2,y2) time coord ? */
	  };


	struct gSp_tab
	  {
	   char name[17];	/* Graphic ScatterPlot attributes name.*/
	   struct gSp_attr *pGSp_attr;/* pointer to graphic ScatterPlot
					 attribute structure.		*/
	   struct gSp_tab *next;/* Pointer to next table entry.	*/
	  };

/*		Graphics meshfill.					*/

/*		The tics and labels are defined by lists.  To cause the
		values to be computed at display time as a function of
		the display space in World Coordinates use "*" instead
		of a list name.  NULL indicates no display.  Either an
		"*" or a non-existent list name will cause the values
		to be computed, however, non-existent list names will
		be indicated with a warning message.  			*/


	struct gfm_attr
	  {
	   char proj[VCS_MX_NM_LEN];	/* Projection name.		 */
	   char xtl1[256];	/* First set of x tics labels =L_name.	 */
	   char xtl2[256];	/* Second set of x tics labels =L_name.	 */
	   char xmt1[256];	/* Minor x tics first set =L_name.	 */
	   char xmt2[256];	/* Minor x tics second set =L_name.	 */
	   char ytl1[256];	/* First set of y tics labels =L_name.	 */
	   char ytl2[256];	/* Second set of y tics labels=L_name.	 */
	   char ymt1[256];	/* Minor y tics first set =L_name.	 */
	   char ymt2[256];	/* Minor y tics second set =L_name.	 */
	   float dsp[4]	;	/* Display space (x1,y1,x2,y2) U.C..	 */
	   char xat[17] ;	/* X-axis transform to (log,ln,exp,cos)  */
	   char yat[17] ;	/* Y-axis transform to (log,ln,exp,cos)  */
	   int missing  ;       /* Color to use for filling missing data.*/
	   float xwrap  ;       /* x wrapping                            */
	   float ywrap  ;       /* y wrapping                            */
	   int mesh     ;       /* drawing the mesh ?                    */
	   char timeunits[256];    /* units for time components based vlues */
	   int calendar;       /* calendar type for time based values */
	   int idsp[4]	;	/* Display space (x1,y1,x2,y2) time coord ? */
	   char *legend ;	/* Holds legend list values.		*/
	   struct fill_range *line; /* Range definitions                 */
	  };

	struct gfm_tab
	  {
	   char name[17];	/* Graphic meshfill attributes name.	*/
	   struct gfm_attr *pGfm_attr;/* pointer to graphic meshfill
					 attribute structure.		*/
	   struct gfm_tab *next;/* Pointer to next table entry.		*/
	  };
