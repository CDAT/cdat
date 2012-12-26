#include "gks.h"
#include "gksshort.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "array.h"
#include "list.h"
#include "picture.h"
#include "graph.h"
#include "project.h"
#include "display.h"

    extern FILE *fpin,*fpout,*fperr;

    extern struct table_text Tt_tab;
    extern struct table_chorn To_tab;

    extern struct table_text *plot_pTt; 
    extern struct table_chorn *plot_pTo;

    extern char text_hpath,text_vpath,text_path;
    extern float text_height,text_angle;
    extern int text_color,text_font;

/*		Set text attributes in preparation for writing with GKS.*/

    int set_text_attr(struct table_text *pTt,struct table_chorn *pTo)
      {

/*			Set the text attributes.			*/
       Gtxfp pfp;
       Gtxalign pta;
       Gtxpath ptp;
       Gpoint   vxy;
       float angle;
	plot_pTt = (struct table_text *)pTt;
	plot_pTo = (struct table_chorn *)pTo;

/* 	gsplci(242); /\* for now red only ! *\/ */

/* /\*       pfp.font=pTt->txfont; */
/*        pfp.prec=pTt->txpr;				*\/ */
       pfp.prec=2;
       if (pTt->txfont > 0) pfp.font=pTt->txfont;
       else pfp.font=1;
       gstxfp(&pfp);
       gstxci(pTt->txci);
       gschxp(pTt->txexp);
       gschsp(pTt->txsp);
       gschh(pTo->chh);
         vxy.x=sin((3.1415926536/180.0)*pTo->chua);
         vxy.y=cos((3.1415926536/180.0)*pTo->chua);

       gschup(&vxy);
         if      (pTo->chalh == 'l') pta.hor=GTH_LEFT;
	 else if (pTo->chalh == 'c') pta.hor=GTH_CENTRE;
	 else if (pTo->chalh == 'r') pta.hor=GTH_RIGHT;
	 else 			     pta.hor=GTH_CENTRE;
	 if      (pTo->chalv == 't') pta.ver=GTV_TOP;
	 else if (pTo->chalv == 'c') pta.ver=GTV_CAP;
	 else if (pTo->chalv == 'h') pta.ver=GTV_HALF;
	 else if (pTo->chalv == 'b') pta.ver=GTV_BASE;
	 else if (pTo->chalv == 's') pta.ver=GTV_BOTTOM;
	 else 			     pta.ver=GTV_HALF;
       gstxal(&pta);
	 if      (pTo->chpath == 'r') ptp=GTP_RIGHT;
	 else if (pTo->chpath == 'l') ptp=GTP_LEFT;
	 else if (pTo->chpath == 'u') ptp=GTP_UP;
	 else if (pTo->chpath == 'd') ptp=GTP_DOWN;
	 else 			      ptp=GTP_RIGHT;
       gstxp(ptp);
       text_path = plot_pTo->chpath;
       text_hpath = plot_pTo->chalh;
       text_color = plot_pTt->txci;
       text_vpath = plot_pTo->chalv;
       text_font = plot_pTt->txfont;
       text_height = plot_pTo->chh;
       text_angle = (float) plot_pTo->chua;
       if ((vxy.y<0.0000001)&&(-.0000001<vxy.y)) angle = 90.*sign(vxy.x)*sign(vxy.y);
       else angle = atan(vxy.x/vxy.y)/3.1415926536*180.;
       if (sign(vxy.y)==1) {
	 text_angle=-angle;
       }
       else {
	 text_angle=180-angle;
       }
       return 1;
      }
