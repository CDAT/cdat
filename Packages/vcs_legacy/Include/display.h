/*  The display table structure contains names for a picture template structure,
    a graphics description structure, and data array structures.  It contains
    status of each display segment.						*/
#define NTYPES 16
#include "vcs_legacy_names_length.h"

     struct display_tab
       {
        int wkst_id;	        /* Associate display with a canvas.     */
	char name[VCS_MX_NM_LEN];/* Name of the display.	       	*/

	int off;		/* Display status indicator (true=off).	*/
	int pri;		/* Priority increment for all segments.	*/
	int continents;		/* Continents type for this pix.	*/

	char type[17];		/* Type of display (command).		*/
	char proj_name[256];	/* Name of projection.		        */

	char g_name[1024];	/* Name of the graph description.	*/
	char p_name[17];	/* Name of the picture template.	*/
	char p_orig_name[17];/* Name of the original template. 	*/

	int na;			/* Number of arrays required.		*/

	char a[6][17];		/* Names of data arrays.		*/

/*	element [0] - id - is the segment number or zero		*/
/*	element [1] - valid(t/f) - indicates whether the display
				   descriptors are valid.		*/
/*	element [2] - exist(t/f) - indicates whether the data exists.	*/
/*	element [3] - reg(t/f) - indicates whether regeneration is
				 necessary.				*/

	int F_seg[4];	/* filename[id,valid(t/f),exist(t/f),reg(t/f)]	*/
	int f_seg[4];	/* function			""		*/
	int lmask_seg[4];/* logical mask		""		*/
	int trnf_seg[4];/* transformation functions	""		*/
	int s_seg[4];	/* source			""		*/
	int n_seg[4];	/* name				""		*/
	int ti_seg[4];	/* title			""		*/
	int u_seg[4];	/* units			""		*/
	int crd_seg[4];	/* creation date		""		*/
	int crt_seg[4];	/* creation time		""		*/
	int com1_seg[4];/* comment #1			""		*/
	int com2_seg[4];/* comment #2			""		*/
	int com3_seg[4];/* comment #3			""		*/
	int com4_seg[4];/* comment #4			""		*/
	int xn_seg[4];	/* x dimension name		""		*/
	int yn_seg[4];	/* y dimension name		""		*/
	int zn_seg[4];	/* z dimension name		""		*/
	int tn_seg[4];	/* t dimension name		""		*/
	int xu_seg[4];	/* x units			""		*/
	int yu_seg[4];	/* y units			""		*/
	int zu_seg[4];	/* z units			""		*/
	int tu_seg[4];	/* t units			""		*/
	int xv_seg[4];	/* x value			""		*/
	int yv_seg[4];	/* y value			""		*/
	int zv_seg[4];	/* z value			""		*/
	int tv_seg[4];	/* t value			""		*/
	int mean_seg[4];/* mean				""		*/
	int max_seg[4];	/* max				""		*/
	int min_seg[4];	/* min				""		*/
	int xt1_seg[4];	/* x axis tic mark #1		""		*/
	int xt2_seg[4];	/* x axis tic mark #2		""		*/
	int xmta_seg[4];/* minor x axis tic mark #1	""		*/
	int xmtb_seg[4];/* minor x axis tic mark #2	""		*/
	int yt1_seg[4];	/* y axis tic mark #1		""		*/
	int yt2_seg[4];	/* y axis tic mark #2		""		*/
	int ymta_seg[4];/* minor y axis tic mark #1	""		*/
	int ymtb_seg[4];/* minor y axis tic mark #2	""		*/
	int xl1_seg[4];	/* x axis label #1		""		*/
	int xl2_seg[4];	/* x axis label #2		""		*/
	int yl1_seg[4];	/* y axis label #1		""		*/
	int yl2_seg[4];	/* y axis label #2		""		*/
	int b1_seg[4];	/* box 1			""		*/
	int b2_seg[4];	/* box 2			""		*/
	int b3_seg[4];	/* box 3			""		*/
	int b4_seg[4];	/* box 4			""		*/
	int l1_seg[4];	/* line 1			""		*/
	int l2_seg[4];	/* line 2			""		*/
	int l3_seg[4];	/* line 3			""		*/
	int l4_seg[4];	/* line 4			""		*/
	int leg_seg[4];	/* legend			""		*/
	int dsp_seg[4];	/* graphics display		""		*/
	float dsp_used[4];/* world coordinates used   	""		*/

	struct display_tab *next;/* Pointer to next display info.	*/
       };

/*		Display types available.				*/

      struct displays
	{
	 char type[17];		/* Display type.			*/
	 int na;		/* Number of arrays required (<=6).	*/
	 int ndim[6];		/* Number of dimensions for each array. */
	 char adim[6][17];	/* Array description, i.e. A(x,y).	*/
	};
