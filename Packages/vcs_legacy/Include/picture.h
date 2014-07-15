#include "vcs_legacy_names_length.h"
/*  The following is a table of format sets.				*/

    struct form
      {
       char s_name[VCS_MX_NM_LEN];	 /* Name - must appear in the format request.	*/
       char s_units[41]; /* Units - must appear in the format request.	*/
       char format[121]; /* Format - describes formatting for value.	*/
       struct form *next;
      };
    struct table_form
      {
       char name[VCS_MX_NM_LEN];		/* Name of the assemblage of formats.	*/
       struct form *variety;	/* Points to the assemblage of formats.	*/
       struct table_form *next;
      };

    struct array_segments       /* Contains x,y segment values for lines, markers, fillareas.  */
      {
       int npts;        	/* Number of points.                            */
       float *pts;      	/* Point values.         			*/
       struct array_segments *next;
      } ;
    struct points_struct     	/* Contains pointers to array_segment struct.  	*/
      {
       int nsegs;        	/* Number of segments.                   	*/
       struct array_segments *ps;      /* Pointer to list of array segments.   	*/
      };

    struct char_segments       /* Contains char segment values for string.  */
      {
       int npts;        	/* Number of points.                            */
       char *cpts;      	/* character values.         			*/
       struct char_segments *next;
      } ;
    struct strings_struct     	/* Contains pointers to char_segment struct.  	*/
      {
       int nsegs;        	/* Number of segments.                   	*/
       struct char_segments *ss;/* Pointer to list of character segments.   	*/
      };

/*		The format should contain:

		%n - to display the name of the value being formatted.
		%u - to display the units of the value being formatted.
		%g - to format the value, or
		%t - to format time as defined in a following set of
		     bracketed units definitions, as [h:m:s] or [d/M/y]
		     or perhaps [d/M/c+1979].

		Where:	1. s - second
			2. m - minute
			3. h - hour
			4. day - day
			5. M - month number
			   mon - three character month designator (no capital)
			   Mon - three character month designator (1st capital)
			   MON - three character month designator (all capital)
			   month - full month name (no capital)
			   Month - full month name (1st capital)
			   MONTH - full month name (all capital)
			6. S - three character season designator
			       (i.e. 1 - DJF, 2 - MAM, 3 - JJA, 4 - SON)
			   Sea - three character/month season designator
			         (i.e. Dec-Jan-Feb)
			   SEA - three character season designator in caps
			         (i.e. DEC-JAN-FEB)
			   season - three month of the season in full
				    (i.e. december-january-february)
			   Season - three months of the season capitalized in
				    full (i.e. December-January-February)
			   SEASON - three month of the season in full caps
				    (i.e. DECEMBER-JANUARY-FEBRUARY)
			7. y - year last two digits (i.e. no century)
			   c - year all four digits

		The %t and bracketed format designators should only
		be used when the variable's name includes "time" and
		units are "month" or "hour" etc..
									*/
/*  The following is a table of text bundles.			*/

	struct table_text
	  {
	   char name[VCS_MX_NM_LEN];	/* name of the text bundle.	*/
           char proj[VCS_MX_NM_LEN];       /* Projection name.            */
	   int txfont;	/* font index.				*/
	   int txpr;	/* precision.				*/
	   float txexp;	/* character expansion factor.		*/
	   float txsp;	/* character spacing.			*/
	   int   txci;	/* color index.				*/
           int	 priority; 	/* placement of the primitive	*/
	   int   txfci;	/* fill-in color index.				*/
           float tvp[4];/* text view port.			*/
           float twc[4];/* text world coordinates.		*/
           struct points_struct *tx;	/* text x values.	*/
           struct points_struct *ty;	/* text y values.	*/
           struct strings_struct *ts;	/* text string values.	*/
	   struct table_text *next;/* pointer - next entry.	*/
	  };

/*  The following is a table of character orientation bundles.	*/

	struct table_chorn
	  {
	   char name[VCS_MX_NM_LEN];	/* name of the text orientation bundle.*/
	   float chh;	/* character height in NDC.		*/
	   float chua;	/* character clockwise up angle (zero is vertical)*/
	   int chpath;	/* text path
			 'r' - right,
			 'l' - left,
			 'u' - up,
			 'd' - down.			*/
	   int chalh;	/* text horizontal alignment 
			 'l' - left,
			 'c' - center,
			 'r' - right.		*/
	   int chalv;	/* text vertical alignment 
			 't' - top,
			 'c' - cap,
			 'h' - half,
			 'b' - base,
			 's' - bottom surface.		*/
	   struct table_chorn *next;	/* pointer - next entry.*/
	  };

/*  The following is a table of polyline bundles.		*/

	struct table_line
	  {
	   char name[VCS_MX_NM_LEN];/* name of the line bundle table.	*/
           char proj[VCS_MX_NM_LEN];       /* Projection name.            */
	   int *ltyp;	/* line type.				*/
           int ltyp_size; /* size of ltyp                       */
	   float *lwsf;	/* line width scale factor.		*/
           int lwsf_size; /* size of lwsf                       */
	   int *lci;	/* line color index.			*/
           int lci_size; /* size of lci                       */
           int	 priority; 	/* placement of the primitive	*/
           float lvp[4];/* line view port.			*/
           float lwc[4];/* line world coordinates.		*/
           struct points_struct *lx;	/* line x values.	*/
           struct points_struct *ly;	/* line y values.	*/
	   struct table_line *next;	/* pointer - next entry.*/
	  };

/* The following is a table of fill area bundles.		*/

	struct table_fill
	  {
	   char name[VCS_MX_NM_LEN];/* name of the fill area bundle table.*/
           char proj[VCS_MX_NM_LEN];       /* Projection name.            */
	   int *fais;	/* fill area interior style.		*/
           int fais_size; /* size of fais                       */
	   int *fasi;	/* fill area style index.   		*/
           int fasi_size; /* size of fasi                       */
	   int *faci;	/* fill area color index.   		*/
           int faci_size; /* size of faci                       */
	   float x;	/* pattern reference x ndc.		*/
	   float y;	/* pattern reference y ndc.		*/
	   float w;	/* pattern width in ndc.		*/
	   float h;	/* pattern height in ndc.		*/
           int	 priority; 	/* placement of the primitive	*/
           float fvp[4];/* fill view port.			*/
           float fwc[4];/* fill world coordinates.		*/
           struct points_struct *fx;	/* fill x values.	*/
           struct points_struct *fy;	/* fill y values.	*/
	   struct table_fill *next;	/* pointer - next entry.*/
	  };

/* The following is a table of marker bundles.			*/

	struct table_mark
	  {
	   char name[VCS_MX_NM_LEN];/* name of the marker bundle table.	*/
           char proj[VCS_MX_NM_LEN];       /* Projection name.            */
	   int *mtyp;	/* marker type.				*/
           int mtyp_size; /* size of mtyp                       */
	   float *msize;	/* marker size			*/
           int msize_size; /* size of msize                     */
	   int *mci;	/* marker color index.			*/
           int mci_size; /* size of mci                         */
           int	 priority; 	/* placement of the primitive	*/
           float mvp[4];/* marker view port.			*/
           float mwc[4];/* marker world coordinates.		*/
           struct points_struct *mx;	/* marker x values.	*/
           struct points_struct *my;	/* marker y values.	*/
	   struct table_mark *next;	/* pointer - next entry.*/
	  };

/* The following is a table of pattern bundles.			*/

	struct table_pattern
	  {
	   int index;	/* pattern index number.		*/
	   int ix;	/* pattern array, first dimension.	*/
	   int iy;	/* pattern array, second deimension.	*/
	   int *array;	/* pattern array.			*/
	   struct table_pattern *next;	/* pointer - next entry.*/
	  };

/* The following is the structure for text display descriptors in
   picture elements.						*/

	struct pe_text
	  {
	   int p;	/* priority.				*/
	   float x;	/* x NDC position of text.		*/
	   float y;	/* y NDC position of text.		*/
	   
	   char tb[VCS_MX_NM_LEN];	/* text bundle name.			*/
	   char to[VCS_MX_NM_LEN];	/* text orientation name.		*/
	  };

/* The following defines the format for display of dimension values.	*/

	struct pe_form
	  {
	   int p;	/* priority.				*/
	   float x;	/* x NDC position of text.		*/
	   float y;	/* y NDC position of text.		*/
	   char fmt[31];/* format.				*/
	   char tb[VCS_MX_NM_LEN];	/* text bundle name.			*/
	   char to[VCS_MX_NM_LEN];	/* text orientation name.		*/
	  };

/* The following is the structure for the data display space.	*/

	struct pe_dsp
	  {
	   int p;	/* priority.				*/
	   float x1;	/* x NDC position of lower left corner.	*/
	   float y1;	/* y NDC position of lower left corner.	*/
	   float x2;	/* x NDC position of upper right corner.*/
	   float y2;	/* y NDC position of upper right corner.*/
	   float ratio; /* user desired aspect ratio */
	  };

/* The following is the structure for the legend display space.	*/

	struct pe_leg
	  {
	   int p;	/* priority.				*/
	   float x1;	/* x NDC position of lower left corner.	*/
	   float y1;	/* y NDC position of lower left corner.	*/
	   float x2;	/* x NDC position of upper right corner.*/
	   float y2;	/* y NDC position of upper right corner.*/
	   char tb[VCS_MX_NM_LEN];	/* text bundle name.			*/
	   char to[VCS_MX_NM_LEN];	/* text orientation name.		*/
	   char ln[VCS_MX_NM_LEN]; /* line bundle name.			*/
	  };

/* The following is the structure for the x_tic mark display.	*/

	struct pe_x_tic
	  {
	   int p;	/* priority.				*/
	   float y1;	/* y NDC position of lower left corner.	*/
	   float y2;	/* y NDC position of upper right corner.*/
	   char ln[VCS_MX_NM_LEN];	/* line bundle name.			*/
	  };

/* The following is the structure for the y_tic mark display.	*/

	struct pe_y_tic
	  {
	   int p;	/* priority.				*/
	   float x1;	/* x NDC position of lower left corner.	*/
	   float x2;	/* x NDC position of upper right corner.*/
	   char ln[VCS_MX_NM_LEN];	/* line bundle name.			*/
	  };

/* The following is the structure for the x_label display.	*/

	struct pe_x_lab
	  {
	   int p;	/* priority.				*/
	   float y;	/* y NDC position of lower left corner.	*/
	   char tb[VCS_MX_NM_LEN];	/* text bundle name.			*/
	   char to[VCS_MX_NM_LEN];	/* text orientation name.		*/
	  };

/* The following is the structure for the y_label display.	*/

	struct pe_y_lab
	  {
	   int p;	/* priority.				*/
	   float x;	/* y NDC position of lower left corner.	*/
	   char tb[VCS_MX_NM_LEN];	/* text bundle name.			*/
	   char to[VCS_MX_NM_LEN];	/* text orientation name.		*/
	  };

/* The following is the structure for the box display.		*/

	struct pe_box
	  {
	   int p;	/* priority.				*/
	   float x1;	/* x NDC position of lower left corner.	*/
	   float y1;	/* y NDC position of lower left corner.	*/
	   float x2;	/* x NDC position of upper right corner.*/
	   float y2;	/* y NDC position of upper right corner.*/
	   char ln[VCS_MX_NM_LEN];	/* line bundle name.			*/
	  };

/* The following is the structure for the line display.		*/

	struct pe_line
	  {
	   int p;	/* priority.				*/
	   float x1;	/* x NDC position of first point.	*/
	   float y1;	/* y NDC position of first point.	*/
	   float x2;	/* x NDC position of last point.	*/
	   float y2;	/* y NDC position of last point.	*/
	   char ln[VCS_MX_NM_LEN];	/* line bundle name.			*/
	  };

/* The following is the structure for the table of picture
   elements (or templates).					*/

	struct p_tab
	  {
           int    normalized_flg;/* for CDAT to determine whether or not to normalize. */
	   int	  orientation_flg; /* 0 = landscape and 1 = portrait, landscape is the default orientation */
	   char   name[VCS_MX_NM_LEN];	/* name of the table entry.		*/
	   struct pe_text F;	/* file name text element.		*/
	   struct pe_text f;	/* function text element.		*/
	   struct pe_text lmask;/* logical mask text element.		*/
	   struct pe_text trnf;	/* transformation text element.		*/
	   struct pe_text s;	/* source text element.			*/
	   struct pe_text n;	/* name text element.			*/
	   struct pe_text ti;	/* title text element.			*/
	   struct pe_text u;	/* units text element.			*/
	   struct pe_text crd;	/* creation date text element.		*/
	   struct pe_text crt;	/* creation time text element.		*/
	   struct pe_text com1;	/* first comment text element.		*/
	   struct pe_text com2;	/* second comment text element.		*/
	   struct pe_text com3;	/* third comment text element.		*/
	   struct pe_text com4;	/* fourth comment text element.		*/
	   struct pe_text xn;	/* x dimension name text element.	*/
	   struct pe_text yn;	/* y dimension name text element.	*/
	   struct pe_text zn;	/* z dimension name text element.	*/
	   struct pe_text tn;	/* t dimension name text element.	*/
	   struct pe_text xu;	/* x dimension units text element.	*/
	   struct pe_text yu;	/* y dimension units text element.	*/
	   struct pe_text zu;	/* z dimension units text element.	*/
	   struct pe_text tu;	/* t dimension units text element.	*/

	   struct pe_form xv;	/* x value format and text description.	*/
	   struct pe_form yv;	/* y value format and text description.	*/
	   struct pe_form zv;	/* z value format and text description.	*/
	   struct pe_form tv;	/* t value format and text description.	*/
	   struct pe_form mean;	/* data mean text element.		*/
	   struct pe_form max;	/* data maximum text element.		*/
	   struct pe_form min;	/* data minimum text element.		*/
	   struct pe_x_tic xt1;	/* x axis tic mark element for label 1.	*/
	   struct pe_x_tic xt2;	/* x axis tic mark element for label 2.	*/
	   struct pe_x_tic xmta;/* minor x axis tic mark element.	*/
	   struct pe_x_tic xmtb;/* minor x axis tic mark element.	*/
	   struct pe_y_tic yt1;	/* y axis tic mark element for label 1.	*/
	   struct pe_y_tic yt2;	/* y axis tic mark element for label 2.	*/
	   struct pe_y_tic ymta;/* minor y axis tic mark element.	*/
	   struct pe_y_tic ymtb;/* minor y axis tic mark element.	*/
	   struct pe_x_lab xl1;	/* x label display element.		*/
	   struct pe_x_lab xl2;	/* x label display element.		*/ 
	   struct pe_y_lab yl1;	/* y label display element.		*/ 
	   struct pe_y_lab yl2;	/* y label display element.		*/
	   struct pe_box b1;	/* box display element.			*/
	   struct pe_box b2;	/* box display element.			*/
	   struct pe_box b3;	/* box display element.			*/
	   struct pe_box b4;	/* box display element.			*/
	   struct pe_line l1;	/* line display element.		*/
	   struct pe_line l2;	/* line display element.		*/
	   struct pe_line l3;	/* line display element.		*/
	   struct pe_line l4;	/* line display element.		*/
	   struct pe_leg leg;	/* legend display space element.	*/
	   struct pe_dsp dsp;	/* display space.			*/
	   struct p_tab *next;	/* pointer - next entry.		*/
	  };
