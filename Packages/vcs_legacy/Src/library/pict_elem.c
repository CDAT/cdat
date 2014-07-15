#include "gks.h"
#include "gksshort.h"
#include <stdio.h>
#include <stdlib.h>
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
#include "cdms.h"

    extern FILE *fpin,*fpout,*fperr;

    extern struct table_text Tt_tab;
    extern struct table_chorn To_tab;
    extern struct table_line Tl_tab;
    extern struct l_tab L_tab[2];

    extern struct display_tab D_tab;

    extern struct project_attr p_PRJ;

    extern char PRJ_names[PRJ_TYPES][17];

    extern int segment_num;

    int nicedf(float a,float b,float *dr,int *pw10,float *center);

/*		Display picture elements except for graphics.		*/

    int pict_elem(pA,pP,pD,proj,xtl1,xtl2,xmt1,xmt2,ytl1,ytl2,ymt1,ymt2,dsp)
      struct a_attr *pA;
      struct p_tab *pP;
      struct display_tab *pD;
      char *proj,*xtl1,*xtl2,*xmt1,*xmt2,*ytl1,*ytl2,*ymt1,*ymt2;
      float dsp[4];
      {
	/* DNW - used as dummy place holders */
	struct pe_x_lab 	xl_null   = {0,0.0,"",""};
        struct pe_y_lab 	yl_null   = {0,0.0,"",""};
        int            		intA_null[] = {0,0,0,0};

       if (pA != NULL)
	 {

	  if (pA->notok) return 0;
	  show_text(pA->F,&pP->F,pD->F_seg,pD->pri);
	  show_text(pA->f,&pP->f,pD->f_seg,pD->pri);
	  show_text(pA->lmask,&pP->lmask,pD->lmask_seg,pD->pri);
	  show_text(pA->trnf,&pP->trnf,pD->trnf_seg,pD->pri);
	  show_text(pA->s,&pP->s,pD->s_seg,pD->pri);
	  show_text(pA->n,&pP->n,pD->n_seg,pD->pri);
	  show_text(pA->ti,&pP->ti,pD->ti_seg,pD->pri);
	  show_text(pA->u,&pP->u,pD->u_seg,pD->pri);
	  show_text(pA->crd,&pP->crd,pD->crd_seg,pD->pri);
	  show_text(pA->crt,&pP->crt,pD->crt_seg,pD->pri);
	  show_text(pA->com1,&pP->com1,pD->com1_seg,pD->pri);
	  show_text(pA->com2,&pP->com2,pD->com2_seg,pD->pri);
	  show_text(pA->com3,&pP->com3,pD->com3_seg,pD->pri);
	  show_text(pA->com4,&pP->com4,pD->com4_seg,pD->pri);

	  if (pA->xs[0] != NULL && *pA->xs[0] > 1)
	    {
	     show_text(pA->xn[0],&pP->xn,pD->xn_seg,pD->pri);
	     show_text(pA->xu[0],&pP->xu,pD->xu_seg,pD->pri);
	    }
	  if (pA->xs[1] != NULL && *pA->xs[1] > 1)
	    {
	     show_text(pA->xn[1],&pP->yn,pD->yn_seg,pD->pri);
	     show_text(pA->xu[1],&pP->yu,pD->yu_seg,pD->pri);
	    }
	  if (pA->xs[2] != NULL && *pA->xs[2] > 1)
	    {
	     show_text(pA->xn[2],&pP->zn,pD->zn_seg,pD->pri);
	     show_text(pA->xu[2],&pP->zu,pD->zu_seg,pD->pri);
	    }
	  if (pA->xs[3] != NULL && *pA->xs[3] > 1)
	    {
	     show_text(pA->xn[3],&pP->tn,pD->tn_seg,pD->pri);
	     show_text(pA->xu[3],&pP->tu,pD->tu_seg,pD->pri);
	    }

	  show_form("Mean",pA->u,&pA->mean,&pP->mean,pD->mean_seg,pD->pri);
	  show_form("Max",pA->u,&pA->max,&pP->max,pD->max_seg,pD->pri);
	  show_form("Min",pA->u,&pA->min,&pP->min,pD->min_seg,pD->pri);

	  if (pA->xs[0] != NULL && *pA->xs[0] == 1)
	    show_form(pA->xn[0],pA->xu[0],pA->xv[0],&pP->xv,pD->xv_seg,pD->pri);
	  if (pA->xs[1] != NULL && *pA->xs[1] == 1)
	    show_form(pA->xn[1],pA->xu[1],pA->xv[1],&pP->yv,pD->yv_seg,pD->pri);
	  if (pA->xs[2] != NULL && *pA->xs[2] == 1)
	    show_form(pA->xn[2],pA->xu[2],pA->xv[2],&pP->zv,pD->zv_seg,pD->pri);
	  if (pA->xs[3] != NULL && *pA->xs[3] == 1)
	    show_form(pA->xn[3],pA->xu[3],pA->xv[3],&pP->tv,pD->tv_seg,pD->pri);
	 }
       draw_box (pP->b1,pD->b1_seg,pD->pri);
       draw_box (pP->b2,pD->b2_seg,pD->pri);
       draw_box (pP->b3,pD->b3_seg,pD->pri);
       draw_box (pP->b4,pD->b4_seg,pD->pri);

       draw_line (pP->l1,pD->l1_seg,pD->pri);
       draw_line (pP->l2,pD->l2_seg,pD->pri);
       draw_line (pP->l3,pD->l3_seg,pD->pri);
       draw_line (pP->l4,pD->l4_seg,pD->pri);

/*       if (pA != NULL && pA->un.data == NULL)				*/
       if (pD->dsp_seg[0] == 0) 
	 {
	  pD->xt1_seg[3]=0;
	  if (pD->xt1_seg[0] > 0) {gdsg(pD->xt1_seg[0]); pD->xt1_seg[0]=0;}
	  pD->xl1_seg[3]=0;
	  if (pD->xl1_seg[0] > 0) {gdsg(pD->xl1_seg[0]); pD->xl1_seg[0]=0;}
	  pD->yt1_seg[3]=0;
	  if (pD->yt1_seg[0] > 0) {gdsg(pD->yt1_seg[0]); pD->yt1_seg[0]=0;}
	  pD->yl1_seg[3]=0;
	  if (pD->yl1_seg[0] > 0) {gdsg(pD->yl1_seg[0]); pD->yl1_seg[0]=0;}

	  pD->xt2_seg[3]=0;
	  if (pD->xt2_seg[0] > 0) {gdsg(pD->xt2_seg[0]); pD->xt2_seg[0]=0;}
	  pD->xl2_seg[3]=0;
	  if (pD->xl2_seg[0] > 0) {gdsg(pD->xl2_seg[0]); pD->xl2_seg[0]=0;}
	  pD->yt2_seg[3]=0;
	  if (pD->yt2_seg[0] > 0) {gdsg(pD->yt2_seg[0]); pD->yt2_seg[0]=0;}
	  pD->yl2_seg[3]=0;
	  if (pD->yl2_seg[0] > 0) {gdsg(pD->yl2_seg[0]); pD->yl2_seg[0]=0;}

	  pD->xmta_seg[3]=0;
	  if (pD->xmta_seg[0] > 0) {gdsg(pD->xmta_seg[0]); pD->xmta_seg[0]=0;}
	  pD->xmtb_seg[3]=0;
	  if (pD->xmtb_seg[0] > 0) {gdsg(pD->xmtb_seg[0]); pD->xmtb_seg[0]=0;}
	  pD->ymta_seg[3]=0;
	  if (pD->ymta_seg[0] > 0) {gdsg(pD->ymta_seg[0]); pD->ymta_seg[0]=0;}
	  pD->ymtb_seg[3]=0;
	  if (pD->ymtb_seg[0] > 0) {gdsg(pD->ymtb_seg[0]); pD->ymtb_seg[0]=0;}
	 }
       else if ( pP->dsp.x1 < 1.0 && pP->dsp.y1 < 1.0 && 
	         pP->dsp.x1 > 0.0 && pP->dsp.y1 > 0.0 &&
	         pP->dsp.x2 < 1.0 && pP->dsp.y2 < 1.0 &&
	         pP->dsp.x2 > 0.0 && pP->dsp.y2 > 0.0 && pP->dsp.p != 0)
	 {
          show_xtic_labels
			 (pP->xt1,pD->xt1_seg,pP->xl1,pD->xl1_seg,xtl1,pD->pri);
          show_xtic_labels
			 (pP->xt2,pD->xt2_seg,pP->xl2,pD->xl2_seg,xtl2,pD->pri);
/* DNW - uses the dummy place holders */
          show_xtic_labels
			 (pP->xmta,pD->xmta_seg,xl_null,intA_null,xmt1,pD->pri);
          show_xtic_labels
			 (pP->xmtb,pD->xmtb_seg,xl_null,intA_null,xmt2,pD->pri);

          show_ytic_labels
			 (pP->yt1,pD->yt1_seg,pP->yl1,pD->yl1_seg,ytl1,pD->pri);
          show_ytic_labels
			 (pP->yt2,pD->yt2_seg,pP->yl2,pD->yl2_seg,ytl2,pD->pri);
/* DNW - uses the dummy place holders */
          show_ytic_labels
			 (pP->ymta,pD->ymta_seg,yl_null,intA_null,ymt1,pD->pri);
          show_ytic_labels
			 (pP->ymtb,pD->ymtb_seg,yl_null,intA_null,ymt2,pD->pri);
	 }
       return 1;
      }

    int draw_box (box,seg,pri)

      struct pe_box box;
      int seg[4];
      int pri;

      {

       Gpoint point[5];
       struct table_line *pl;

       if (seg[3] == 0) return 1;

       if (box.x1<=0.0 || box.x2<=0.0 || box.y1<=0.0 || box.y2<=0.0 ||
	   box.x1>=1.0 || box.x2>=1.0 || box.y1>=1.0 || box.y2>=1.0 ||
	   box.p<=0)
	 {
	  seg[3]=0;
	  if (seg[0] > 0) {gdsg(seg[0]); seg[0]=0;}
	  return 1;
	 }
/*			Set line attributes.				*/
       pl=&Tl_tab;
       for (;pl!=NULL && strcmp(box.ln,pl->name)!=0;pl=pl->next);
       if (pl == NULL) pl=&Tl_tab;

       if (pl->ltyp == NULL)
          gsln(1);
       else
          gsln(pl->ltyp[0]);
       if (pl->lwsf == NULL)
          gslwsc(1.0);
       else
          gslwsc(pl->lwsf[0]);
       if (pl->lci == NULL)
          gsplci(241);
       else
          gsplci(pl->lci[0]);

/*			Set up the segment.				*/

       seg[1]=1;
       seg[2]=1;
       seg[3]=0;

       if (seg[0] > 0) gdsg(seg[0]);
       else seg[0]=++segment_num;
       gcrsg(seg[0]);
       gssgp(seg[0], (pri+box.p)/1000.0);

       point[0].x=box.x1;
       point[0].y=box.y1;
       point[1].x=box.x2;
       point[1].y=box.y1;
       point[2].x=box.x2;
       point[2].y=box.y2;
       point[3].x=box.x1;
       point[3].y=box.y2;
       point[4].x=box.x1;
       point[4].y=box.y1;
       gpl(5,point);

       gclsg();
       return 1;
      }

    int draw_line (lin,seg,pri)

      struct pe_line lin;
      int seg[4];
      int pri;

      {

       Gpoint point[2];
       struct table_line *pl;

       if (seg[3] == 0) return 1;

       if (lin.x1<=0.0 || lin.x2<=0.0 || lin.y1<=0.0 || lin.y2<=0.0 ||
	   lin.x1<=0.0 || lin.x2<=0.0 || lin.y1<=0.0 || lin.y2<=0.0 ||
	   lin.p<=0)
	 {
	  seg[3]=0;
	  if (seg[0] > 0) {gdsg(seg[0]); seg[0]=0;}
	  return 1;
	 }
/*			Set line attributes.				*/
       pl=&Tl_tab;
       for (;pl!=NULL && strcmp(lin.ln,pl->name)!=0;pl=pl->next);
       if (pl == NULL) pl=&Tl_tab;

       if (pl->ltyp == NULL)
          gsln(1);
       else
          gsln(pl->ltyp[0]);
       if (pl->lwsf == NULL)
          gslwsc(1.0);
       else
          gslwsc(pl->lwsf[0]);
       if (pl->lci == NULL)
          gsplci(241);
       else
          gsplci(pl->lci[0]);

/*			Set up the segment.				*/

       seg[1]=1;
       seg[2]=1;
       seg[3]=0;

       if (seg[0] > 0) gdsg(seg[0]);
       else seg[0]=++segment_num;
       gcrsg(seg[0]);
       gssgp(seg[0], (pri+lin.p)/1000. );

       point[0].x=lin.x1;
       point[0].y=lin.y1;
       point[1].x=lin.x2;
       point[1].y=lin.y2;
       gpl(2,point);

       gclsg();
       return 1;
      }

/*	Show a text string from picture elements and array attributes.	*/

    int show_text(str,pt,seg,pri)

      char *str;
      struct pe_text *pt;
      int seg[4],pri;
      {
       struct table_text *pTt;
       struct table_chorn *pTo;
       Gpoint pxy[4];

       if (seg[3]==0) return 1;

       if (str==NULL || strprt(str)<=0 || pt->x<=0.0 || pt->y<=0.0 ||
	   pt->x>=1.0 || pt->y>=1.0 || pt->p<=0)
	  {
	   if (str == NULL || strprt(str)<=0) seg[2]=0;
	   if (pt->x <= 0.0 ||pt->y <= 0.0 || pt->x >= 1.0 || pt->y >= 1.0)
		seg[1]=0;
	   seg[3]=0;
	   if (seg[0] > 0) {gdsg(seg[0]); seg[0]=0;}
	   return 1;
	  }

       if (seg[0] != 0) gdsg(seg[0]);
       else seg[0]=++segment_num;

       gcrsg(seg[0]);
       gssgp(seg[0],(pri+pt->p)/1000.0);

/*			Find the text bundle.				*/
       pTt=&Tt_tab;
       while (pTt != NULL)
	 {
	  if (strcmp(pt->tb,pTt->name) == 0) break;
	  pTt=pTt->next;
	 }
       if (pTt == NULL) pTt=&Tt_tab;

/*			Find the text orientation bundle.		*/
       pTo=&To_tab;
       while (pTo != NULL)
	 {
	  if (strcmp(pt->to,pTo->name) == 0) break;
	  pTo=pTo->next;
	 }
       if (pTo == NULL) pTo=&To_tab;
		 
       set_text_attr(pTt,pTo);

/*       un.extent[0]=cairogqtxx(opwk,pxy[0],label,&err);		*/

	 pxy[0].x=pt->x;
	 pxy[0].y=pt->y;
	 trimbl(str,strlen(str));
       gtx (&pxy[0],(unsigned char *)str);

       seg[1]=1;
       seg[2]=1;
       seg[3]=0;

       gclsg();
       return 1;
      }

/*	Show a formatted text string from picture elements and
	array attributes.						*/

    int show_form(name,units,val,pt,seg,pri)

      char *name,*units;
      float *val;
      struct pe_form *pt;
      int seg[4],pri;
      {
       struct table_text *pTt;
       struct table_chorn *pTo;
       Gpoint pxy[4];
       char str[257];
       char month[13][10];
       cdCompTime comptime;

       if (seg[3]==0) return 1;

       if (val == NULL || pt->x<=0.0 || pt->y<=0.0 || pt->x>=1.0 || pt->y>=1.0
		|| pt->p<=0)
	  {
	   if (val == NULL) seg[2]=0;
	   if (pt->x <= 0.0 ||pt->y <= 0.0 || pt->x >= 1.0 || pt->y >= 1.0)
		seg[1]=0;
	   seg[3]=0;

	   if (seg[0] > 0) {gdsg(seg[0]); seg[0]=0;}
	   return 1;
	  }

       if (seg[0] != 0) gdsg(seg[0]);
       else seg[0]=++segment_num;

       gcrsg(seg[0]);
       gssgp(seg[0],(pri+pt->p)/1000.0);

/*			Find the text bundle.				*/
       pTt=&Tt_tab;
       while (pTt != NULL)
	 {
	  if (strcmp(pt->tb,pTt->name) == 0) break;
	  pTt=pTt->next;
	 }
       if (pTt == NULL) pTt=&Tt_tab;

/*			Find the text orientation bundle.		*/
       pTo=&To_tab;
       while (pTo != NULL)
	 {
	  if (strcmp(pt->to,pTo->name) == 0) break;
	  pTo=pTo->next;
	 }
       if (pTo == NULL) pTo=&To_tab;
		 
       set_text_attr(pTt,pTo);

/*       un.extent[0]=cairogqtxx(opwk,pxy[0],label,&err);		*/

	 pxy[0].x=pt->x;
	 pxy[0].y=pt->y;

       if (cmpncs("time", name)!= 0) 
          format (name,units,(double)*val,pt->fmt,str);
       else { /* use cdtime to compute the time from time units */
    	  strcpy(month[1],"January");
    	  strcpy(month[2],"February");
    	  strcpy(month[3],"March");
    	  strcpy(month[4],"April");
    	  strcpy(month[5],"May");
    	  strcpy(month[6],"June");
    	  strcpy(month[7],"July");
    	  strcpy(month[8],"August");
    	  strcpy(month[9],"September");
    	  strcpy(month[10],"October");
    	  strcpy(month[11],"November");
    	  strcpy(month[12],"December");
	  comptime.month = 0; comptime.year=0; comptime.day=0; comptime.hour=0;
          cdRel2Comp(cd360, units, (double)*val , &comptime);
	  if (comptime.month !=0)
	     sprintf(str, "%s/%d", month[comptime.month], comptime.year);
          else
             format (name,units,(double)*val,pt->fmt,str);
       }

       gtx (&pxy[0],(unsigned char *)str);

       seg[1]=1;
       seg[2]=1;
       seg[3]=0;

       gclsg();
       return 1;
      }

/*		Display x axis tics and the labels for them.		*/

    int show_xtic_labels (xt,xt_seg,xl,xl_seg,list,pri)

      struct pe_x_tic xt;
      int xt_seg[4];
      struct pe_x_lab xl;
      int xl_seg[4];
      char *list;
      int pri;

      {
       int pw10,k,k1,k2,i,np=50;
       float dr,del,center;
       Gpoint pxy[2];
       Gpoint *pxy2;
       struct project_attr *pj;
       struct l_tab *ptab;
       struct l_val *pv,**ptl;
       struct table_line *pl;
       float x,y,y1,y2,sy;
       struct table_text *pTt;
       struct table_chorn *pTo;

       pj=&p_PRJ;
       if (xt_seg[3] == 0 && xl_seg != NULL && xl_seg[3] == 0) return 1;

       if (list==NULL || *list=='\0') return 1;

/*			Find the list of values and strings.
			If "*" is given, then compute reasonable values.
			Return if it doesn't exist or is empty.		*/

       for (ptab=&L_tab[0];
	     ptab != NULL && strcmp(list,ptab->name) != 0;
     		ptab=ptab->next);

       if (strcmp(list,"*") == 0)
	 {
	  if ( nicedf(pj->X1,pj->X2,&dr,&pw10,&center) )
	    {
	     del=dr*pow(10.0,(double)pw10);
	     k1=(pj->X1-center<pj->X2-center)?(pj->X1-center)/del:
							(pj->X2-center)/del;
	     k2=(pj->X1-center<pj->X2-center)?(pj->X2-center)/del:
							(pj->X1-center)/del;
	     k1--;
	     k2++;
	     if( (ptab=(struct l_tab *)
			malloc(sizeof(struct l_tab)) )==NULL)
	       {
	        err_warn(1,fperr,"Error - no memory for x tick list.\n");
		xt_seg[3]=0;
		if (xt_seg[0] > 0) {gdsg(xt_seg[0]); xt_seg[0]=0;}
		xl_seg[3]=0;
		if (xl_seg[0] > 0) {gdsg(xl_seg[0]); xl_seg[0]=0;}
		return 0;
	       }
	     ptab->next=NULL;
	     ptab->val=NULL;
	     ptab->count=k2-k1+1;
	     ptab->name[0]='\0';
	     ptl=&ptab->val;

	     for (k=k1;k<k2;k++)
	       {
		if((*ptl=pv=(struct l_val *)malloc(sizeof(struct l_val)))==NULL)
		  {
		   err_warn(1,fperr,"Error - no memory for x tick list.\n");
		   xt_seg[3]=0;
		   if (xt_seg[0] > 0) {gdsg(xt_seg[0]); xt_seg[0]=0;}
		   xl_seg[3]=0;
		   if (xl_seg[0] > 0) {gdsg(xl_seg[0]); xl_seg[0]=0;}
		   killL(ptab);
		   return 0;
		  }
		pv->next=NULL;
		ptl=&pv->next;
		pv->data=k*del+center;
		if ((pv->str=(char *)malloc(20)) == NULL)
		  {
		   err_warn(1,fperr,"Error - no memory for x tick list.\n");
		   xt_seg[3]=0;
		   if (xt_seg[0] > 0) {gdsg(xt_seg[0]); xt_seg[0]=0;}
		   xl_seg[3]=0;
		   if (xl_seg[0] > 0) {gdsg(xl_seg[0]); xl_seg[0]=0;}
		   killL(ptab);
		   return 0;
		  }
		sprintf(pv->str,"%g",pv->data);
	       }
	    }
	  else
	    {
	     if( (ptab=(struct l_tab *)
			malloc(sizeof(struct l_tab)) )==NULL)
	       {
	        err_warn(1,fperr,"Error - no memory for x tick list.\n");
		xt_seg[3]=0;
		if (xt_seg[0] > 0) {gdsg(xt_seg[0]); xt_seg[0]=0;}
		xl_seg[3]=0;
		if (xl_seg[0] > 0) {gdsg(xl_seg[0]); xl_seg[0]=0;}
		return 0;
	       }
	     ptab->next=NULL;
	     ptab->count=2;
	     ptab->name[0]='\0';
	     if((pv=(struct l_val *)malloc(sizeof(struct l_val)))==NULL)
	       {
		err_warn(1,fperr,"Error - no memory for x tick list.\n");
		xt_seg[3]=0;
		if (xt_seg[0] > 0) {gdsg(xt_seg[0]); xt_seg[0]=0;}
		xl_seg[3]=0;
		if (xl_seg[0] > 0) {gdsg(xl_seg[0]); xl_seg[0]=0;}
		killL(ptab);
		return 0;
	       }
	     ptab->val=pv;
	     pv->next=NULL;
	     pv->data=pj->X1;
	     if ((pv->str=(char *)malloc(20)) == NULL)
	       {
	        err_warn(1,fperr,"Error - no memory for x tick list.\n");
		xt_seg[3]=0;
		if (xt_seg[0] > 0) {gdsg(xt_seg[0]); xt_seg[0]=0;}
	        xl_seg[3]=0;
	        if (xl_seg[0] > 0) {gdsg(xl_seg[0]); xl_seg[0]=0;}
	        killL(ptab);
	        return 0;
	       }
	     sprintf(pv->str,"%g",pj->X1);

	     if ((pv->next=(struct l_val *)malloc(sizeof(struct l_val)))==NULL)
	       {
		err_warn(1,fperr,"Error - no memory for x tick list.\n");
		xt_seg[3]=0;
		if (xt_seg[0] > 0) {gdsg(xt_seg[0]); xt_seg[0]=0;}
		xl_seg[3]=0;
		if (xl_seg[0] > 0) {gdsg(xl_seg[0]); xl_seg[0]=0;}
		killL(ptab);
		return 0;
	       }
	     pv=pv->next;
	     pv->next=NULL;
	     pv->data=pj->X2;
	     if ((pv->str=(char *)malloc(20)) == NULL)
	       {
	        err_warn(1,fperr,"Error - no memory for x tick list.\n");
		xt_seg[3]=0;
		if (xt_seg[0] > 0) {gdsg(xt_seg[0]); xt_seg[0]=0;}
	        xl_seg[3]=0;
	        if (xl_seg[0] > 0) {gdsg(xl_seg[0]); xl_seg[0]=0;}
	        killL(ptab);
	        return 0;
	       }
	     sprintf(pv->str,"%g",pj->X2);
	    }	     
	 }
       else if (ptab==NULL || ptab->val==NULL || ptab->count<=0)
	 {
	  err_warn(1,fperr,
	    "Error - list (L_%s) for X tick and label is missing or invalid.\n",
								list);
	  xt_seg[3]=0;
	  if (xt_seg[0] > 0) {gdsg(xt_seg[0]); xt_seg[0]=0;}
	  xl_seg[3]=0;
	  if (xl_seg[0] > 0) {gdsg(xl_seg[0]); xl_seg[0]=0;}
	  return 0;
	 }

       if (xt.y1>0.0 && xt.y1<1.0 && xt.y2>0.0 && xt.y2<1.0 && xt.p>0)
	 {

	  if (xt_seg[3] != 0)
	    {


/*			Set the tics segment.				*/

             if (xt_seg[0] > 0) gdsg(xt_seg[0]);
             else xt_seg[0] = ++segment_num;
             gcrsg(xt_seg[0]);
             gssgp(xt_seg[0],(pri+xt.p)/1000.0);

/*			Set line attributes and draw tics.		*/

             pl=&Tl_tab;
             for (;pl!=NULL && strcmp(xt.ln,pl->name)!=0;pl=pl->next);
             if (pl == NULL) pl=&Tl_tab;
             if (pl->ltyp == NULL)
                gsln(1);
             else
                gsln(pl->ltyp[0]);
             if (pl->lwsf == NULL)
                gslwsc(1.0);
             else
                gslwsc(pl->lwsf[0]);
             if (pl->lci == NULL)
                gsplci(241);
             else
                gsplci(pl->lci[0]);

/*			Convert y to user coordinates.			*/

	     sy=(pj->Y2-pj->Y1)/(pj->y2_NDC-pj->y1_NDC);
             y1=pj->Y1+(xt.y1-pj->y1_NDC)*sy;
             y2=pj->Y1+(xt.y2-pj->y1_NDC)*sy;
             pv=ptab->val;

/* 	     if ((pj->proj_type>0)&&(strcmp(list,"*")==0)) pv=pv->next; */

             for (;pv != NULL;pv=pv->next)
               {
                x=pv->data;
/* 		if (pj->proj_type>0) */
/* 		  { */
/* /\* 		    y1=(pj->Y2-pj->Y1)/(np-1.); *\/ */
/* 		    if ((pxy2=(Gpoint *)malloc(np*sizeof(Gpoint)))==NULL) */
/* 		      { */
/* 			err_warn(1,fperr,"Error: Memory overflow in pict_elem.c pxy2 malloc.\n"); */
/* 			return 0; */
/* 		      } */
		    
/* 		    for  (i=0;i<np;i++) */
/* 		      { */
/* 			pxy2[i].x=x; */
/* /\* 			pxy2[i].y=pj->Y1+((float)i)*y1; *\/ */
/* 			pxy2[i].y=y1+(y2-y1)/((float)np)*i; */
/* 		      } */
/* 		    proj_pl(np,pxy2); */
/* 		    free(pxy2); */
/* 		  } */
/* 		else */
/* 		  { */
		    if ( (pj->X1-x)*(pj->X2-x)<=0.0 && fabs(pj->X2-pj->X1)>1e-10)
		      {
			pxy[0].x=x;
			pxy[0].y=y1;
			pxy[1].x=x;
			pxy[1].y=y2;
			proj_pl(2,pxy);
		      }
/* 		  } */
               }
             xt_seg[1]=1;
             xt_seg[2]=1;
             xt_seg[3]=0;

             gclsg();
	    }
	 }
       else
	 {
	  xt_seg[3]=0;
	  if (xt_seg[0] > 0) {gdsg(xt_seg[0]); xt_seg[0]=0;}
	  /*return 1; DNW - 5/25/200 Don't return yet, we must check to
			    see if we need to process the x-labels */
	 }

       if (&xl == NULL || &xl_seg == NULL) return 1;

       if (xl_seg[3] == 0) return 1;

       if (xl.y > 0.0 && xl.y < 1.0 && xl.p>0)
	 {
/*			Set the labels segment.				*/

          if (xl_seg[0] > 0) gdsg(xl_seg[0]);
          else xl_seg[0] = ++segment_num;
          gcrsg(xl_seg[0]);
          gssgp(xl_seg[0],(pri+xl.p)/1000.0);

/*			Find the text bundle.				*/
          pTt=&Tt_tab;
          while (pTt != NULL)
	    {
	     if (strcmp(xl.tb,pTt->name) == 0) break;
	     pTt=pTt->next;
	    }
          if (pTt == NULL) pTt=&Tt_tab;

/*			Find the text orientation bundle.		*/
          pTo=&To_tab;
          while (pTo != NULL)
	    {
	     if (strcmp(xl.to,pTo->name) == 0) break;
	     pTo=pTo->next;
	    }
          if (pTo == NULL) pTo=&To_tab;

	 set_text_attr(pTt,pTo);		 

/*       un.extent[0]=cairogqtxx(opwk,pxy[0],label,&err);		*/

          sy=(pj->Y2-pj->Y1)/(pj->y2_NDC-pj->y1_NDC);
          y1=pj->Y1+(xl.y-pj->y1_NDC)*sy;
          pv=ptab->val;

          for (;pv != NULL;pv=pv->next)
            {
	     y=y1;
             x=pv->data;
             if ( (pj->X1-x)*(pj->X2-x) <= 0.0 && fabs(pj->X2-pj->X1)>1e-10)
               {
	        project(1,&x,&y);
	        pxy[0].x=x;
	        pxy[0].y=y;
	        gtx (&pxy[0],(unsigned char *)pv->str);

               }
            }

          xl_seg[1]=1;
          xl_seg[2]=1;
          xl_seg[3]=0;

          gclsg();
	 }
       else
	 {
	  xl_seg[3]=0;
	  if (xl_seg[0] > 0) {gdsg(xl_seg[0]); xl_seg[0]=0;}
	 }
/*			If the list had to be created, kill it.		*/

       if (ptab->name[0] == '\0') killL(ptab);
       return 1;
      }

/*		Display y axis tics and the labels for them.		*/

    int show_ytic_labels (yt,yt_seg,yl,yl_seg,list,pri)

      struct pe_y_tic yt;
      int yt_seg[4];
      struct pe_y_lab yl;
      int yl_seg[4];
      char *list;
      int pri;
      {
       int pw10,k,k1,k2,i,np=50;
       float dr,del,center;
       struct project_attr *pj;
       Gpoint pxy[2];
       Gpoint *pxy2;
       struct l_tab *ptab;
       struct l_val *pv,**ptl;
       struct table_line *pl;
       float x,y,x1,x2,sx;
       struct table_text *pTt;
       struct table_chorn *pTo;

       pj=&p_PRJ;
       if (yt_seg[3] == 0 && yl_seg != NULL && yl_seg[3] == 0) return 1;

       if (list==NULL || *list=='\0') return 1;

/*			Find the list of values and strings.
			If "*" is given, then compute reasonable values.
			Return it if doesn't exist or is empty.		*/

       for (ptab=&L_tab[0];
	         ptab != NULL && strcmp(list,ptab->name) != 0;
			ptab=ptab->next);

       if (strcmp(list,"*") == 0)
	 {
	  if ( nicedf(pj->Y1,pj->Y2,&dr,&pw10, &center) )
	    {
	     del=dr*pow(10.0,(double)pw10);
	     k1=(pj->Y1-center<pj->Y2-center)?(pj->Y1-center)/del:
							(pj->Y2-center)/del;
	     k2=(pj->Y1-center<pj->Y2-center)?(pj->Y2-center)/del:
							(pj->Y1-center)/del;
	     k1--;
	     k2++;
	     if( (ptab=(struct l_tab *)
			malloc(sizeof(struct l_tab)) )==NULL)
	       {
	        err_warn(1,fperr,"Error - no memory for y tick list.\n");
		yt_seg[3]=0;
		if (yt_seg[0] > 0) {gdsg(yt_seg[0]); yt_seg[0]=0;}
		yl_seg[3]=0;
		if (yl_seg[0] > 0) {gdsg(yl_seg[0]); yl_seg[0]=0;}
		return 0;
	       }
	     ptab->next=NULL;
	     ptab->val=NULL;
	     ptab->count=k2-k1+1;
	     ptab->name[0]='\0';
	     ptl=&ptab->val;

	     for (k=k1;k<k2;k++)
	       {
		if((*ptl=pv=(struct l_val *)malloc(sizeof(struct l_val)))==NULL)
		  {
		   err_warn(1,fperr,"Error - no memory for y tick list.\n");
		   yt_seg[3]=0;
		   if (yt_seg[0] > 0) {gdsg(yt_seg[0]); yt_seg[0]=0;}
		   yl_seg[3]=0;
		   if (yl_seg[0] > 0) {gdsg(yl_seg[0]); yl_seg[0]=0;}
		   killL(ptab);
		   return 0;
		  }
		pv->next=NULL;
		ptl=&pv->next;
		pv->data=k*del+center;
		if ((pv->str=(char *)malloc(20)) == NULL)
		  {
		   err_warn(1,fperr,"Error - no memory for y tick list.\n");
		   yt_seg[3]=0;
		   if (yt_seg[0] > 0) {gdsg(yt_seg[0]); yt_seg[0]=0;}
		   yl_seg[3]=0;
		   if (yl_seg[0] > 0) {gdsg(yl_seg[0]); yl_seg[0]=0;}
		   killL(ptab);
		   return 0;
		  }
		sprintf(pv->str,"%g",pv->data);
	       }
	    }
	  else
	    {
	     if( (ptab=(struct l_tab *)
			malloc(sizeof(struct l_tab)) )==NULL)
	       {
	        err_warn(1,fperr,"Error - no memory for x tick list.\n");
		yt_seg[3]=0;
		if (yt_seg[0] > 0) {gdsg(yt_seg[0]); yt_seg[0]=0;}
		yl_seg[3]=0;
		if (yl_seg[0] > 0) {gdsg(yl_seg[0]); yl_seg[0]=0;}
		return 0;
	       }
	     ptab->next=NULL;
	     ptab->count=2;
	     ptab->name[0]='\0';
	     if((pv=(struct l_val *)malloc(sizeof(struct l_val)))==NULL)
	       {
		err_warn(1,fperr,"Error - no memory for y tick list.\n");
		yt_seg[3]=0;
		if (yt_seg[0] > 0) {gdsg(yt_seg[0]); yt_seg[0]=0;}
		yl_seg[3]=0;
		if (yl_seg[0] > 0) {gdsg(yl_seg[0]); yl_seg[0]=0;}
		killL(ptab);
		return 0;
	       }
	     ptab->val=pv;
	     pv->next=NULL;
	     pv->data=pj->Y1;
	     if ((pv->str=(char *)malloc(20)) == NULL)
	       {
	        err_warn(1,fperr,"Error - no memory for y tick list.\n");
		yt_seg[3]=0;
		if (yt_seg[0] > 0) {gdsg(yt_seg[0]); yt_seg[0]=0;}
	        yl_seg[3]=0;
	        if (yl_seg[0] > 0) {gdsg(yl_seg[0]); yl_seg[0]=0;}
	        killL(ptab);
	        return 0;
	       }
	     sprintf(pv->str,"%g",pj->Y1);

	     if ((pv->next=(struct l_val *)malloc(sizeof(struct l_val)))==NULL)
	       {
		err_warn(1,fperr,"Error - no memory for y tick list.\n");
		yt_seg[3]=0;
		if (yt_seg[0] > 0) {gdsg(yt_seg[0]); yt_seg[0]=0;}
		yl_seg[3]=0;
		if (yl_seg[0] > 0) {gdsg(yl_seg[0]); yl_seg[0]=0;}
		killL(ptab);
		return 0;
	       }
	     pv=pv->next;
	     pv->next=NULL;
	     pv->data=pj->Y2;
	     if ((pv->str=(char *)malloc(20)) == NULL)
	       {
	        err_warn(1,fperr,"Error - no memory for y tick list.\n");
		yt_seg[3]=0;
		if (yt_seg[0] > 0) {gdsg(yt_seg[0]); yt_seg[0]=0;}
	        yl_seg[3]=0;
	        if (yl_seg[0] > 0) {gdsg(yl_seg[0]); yl_seg[0]=0;}
	        killL(ptab);
	        return 0;
	       }
	     sprintf(pv->str,"%g",pj->Y2);
	    }	     
	 }
       else if (ptab==NULL || ptab->val==NULL || ptab->count<=0)
	 {
	  err_warn(1,fperr,
	    "Error - list (L_%s) for X tick and label is missing or invalid.\n",
								list);
	  yt_seg[3]=0;
	  if (yt_seg[0] > 0) {gdsg(yt_seg[0]); yt_seg[0]=0;}
	  yt_seg[3]=0;
	  if (yt_seg[0] > 0) {gdsg(yt_seg[0]); yt_seg[0]=0;}
	  return 0;
	 }

       if (yt.x1 > 0.0 && yt.x1 < 1.0 && yt.x2 > 0.0 && yt.x2 < 1.0 && yt.p>0)
	 {
	  if (yt_seg[3] != 0)
	    {

/*			Set the tics segment.				*/

	     if (yt_seg[0] > 0) gdsg(yt_seg[0]);
	     else yt_seg[0] = ++segment_num;
	     gcrsg(yt_seg[0]);
	     gssgp(yt_seg[0],(pri+yt.p)/1000.0);

/*			Set line attributes and draw tics.		*/

	     pl=&Tl_tab;
	     for (;pl!=NULL && strcmp(yt.ln,pl->name)!=0;pl=pl->next);
	     if (pl == NULL) pl=&Tl_tab;
             if (pl->ltyp == NULL)
                gsln(1);
             else
                gsln(pl->ltyp[0]);
             if (pl->lwsf == NULL)
                gslwsc(1.0);
             else
                gslwsc(pl->lwsf[0]);
             if (pl->lci == NULL)
                gsplci(241);
             else
                gsplci(pl->lci[0]);

	     sx=(pj->X2-pj->X1)/(pj->x2_NDC-pj->x1_NDC);
	     x1=pj->X1+(yt.x1-pj->x1_NDC)*sx;
	     x2=pj->X1+(yt.x2-pj->x1_NDC)*sx;
	     pv=ptab->val;

/* 	     if ((pj->proj_type>0)&&(strcmp(list,"*")==0)) pv=pv->next; */

	     for (;pv != NULL;pv=pv->next)
	       {
		 y=pv->data;
/* 		 if (pj->proj_type>0) */
/* 		   { */
/* /\* 		     x1=(pj->X2-pj->X1)/(np-1.); *\/ */
/* 		     if ((pxy2=(Gpoint *)malloc(np*sizeof(Gpoint)))==NULL) */
/* 		       { */
/* 			 err_warn(1,fperr,"Error: Memory overflow in pict_elem.c pxy2 malloc.\n"); */
/* 			 return 0; */
/* 		       } */
/* 		     for  (i=0;i<np;i++) */
/* 		       { */
/* 			 pxy2[i].y=y; */
/* /\* 			 pxy2[i].x=pj->X1+(float)i*x1; *\/ */
/* 			 pxy2[i].x=x1+(x2-x1)/((float)np)*i; */
/* 		       } */
/* 		     proj_pl(np,pxy2); */
/* 		     free(pxy2); */
/* 		   } */
/* 		 else */
/* 		   { */
		     if ( (pj->Y1-y)*(pj->Y2-y)<=0.0 && fabs(pj->Y2-pj->Y1)>1e-10)
		       {
			 pxy[0].x=x1;
			 pxy[0].y=y;
			 pxy[1].x=x2;
			 pxy[1].y=y;
			 proj_pl(2,pxy);
		       }
/* 		   } */
	       }
	     yt_seg[1]=1;
	     yt_seg[2]=1;
	     yt_seg[3]=0;

	     gclsg();
	    }
	 }
       else
	 {
	  yt_seg[3]=0;
	  if (yt_seg[0] > 0) {gdsg(yt_seg[0]); yt_seg[0]=0;}
	  /*return 1; DNW - 5/25/200 Don't return yet, we must check to
			    see if we need to process the y-labels */
	 }

       if (&yl == NULL || &yl_seg == NULL) return 1;

       if (yl_seg[3] == 0) return 1;

       if (yl.x > 0.0 && yl.x < 1.0 && yl.p > 0)
	 {

/*			Set the labels segment.				*/

          if (yl_seg[0] > 0) gdsg(yl_seg[0]);
          else yl_seg[0] = ++segment_num;
          gcrsg(yl_seg[0]);
          gssgp(yl_seg[0],(pri+yl.p)/1000.0);

/*			Find the text bundle.				*/
          pTt=&Tt_tab;
          while (pTt != NULL)
	    {
	     if (strcmp(yl.tb,pTt->name) == 0) break;
	     pTt=pTt->next;
	    }
          if (pTt == NULL) pTt=&Tt_tab;

/*			Find the text orientation bundle.		*/
          pTo=&To_tab;
          while (pTo != NULL)
	    {
	     if (strcmp(yl.to,pTo->name) == 0) break;
	     pTo=pTo->next;
	    }
          if (pTo == NULL) pTo=&To_tab;

	  set_text_attr(pTt,pTo);
		 
/*       un.extent[0]=cairogqtxx(opwk,pxy[0],label,&err);		*/

          sx=(pj->X2-pj->X1)/(pj->x2_NDC-pj->x1_NDC);
          x1=pj->X1+(yl.x-pj->x1_NDC)*sx;	
          pv=ptab->val;
   
          for (;pv != NULL;pv=pv->next)
            {
	     x=x1;
             y=pv->data;
	     
             if ( (pj->Y1-y)*(pj->Y2-y) <= 0.0)
               {
	        project(1,&x,&y);
	        pxy[0].x=x;
	        pxy[0].y=y;
	        gtx (&pxy[0],(unsigned char *)pv->str);

               }
            }

          yl_seg[1]=1;
          yl_seg[2]=1;
          yl_seg[3]=0;

          gclsg();
	 }
       else
	 {
	  yl_seg[3]=0;
	  if (yl_seg[0] > 0) {gdsg(yl_seg[0]); yl_seg[0]=0;}
	 }

       if (ptab->name[0] == '\0') killL(ptab);
       return 1;
      }
