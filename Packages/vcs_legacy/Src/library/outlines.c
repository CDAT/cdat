#include "gks.h"
#include "gksshort.h"
#ifdef USEX11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "array.h"
#include "list.h"
#include "picture.h"
#include "graph.h"
#include "project.h"
#include "display.h"
#include "workstations.h"

    extern FILE *fpin,*fpout,*fperr;

    extern struct workstations Wkst[];
    extern struct table_line Tl_tab;

    extern struct display_tab D_tab;

    extern struct project_attr p_PRJ;

    extern char PRJ_names[PRJ_TYPES][17];

    extern int segment_num;

    extern int IN[14][2][2],JN[14][2][2];

/*		Table of increments in i and j for outline following.

    int IN[14][2][2] = {
			0,0,0,1, 0,1,1,1, 0,0,1,1, 1,1,0,1, 0,0,0,1,
			0,1,0,1, 0,0,0,1, 0,1,0,0, 0,1,0,1, 0,1,0,0,
			0,1,1,1, 1,1,0,0, 1,1,0,1, 0,1,0,0
		       };
    int JN[14][2][2] = {
			0,1,0,0, 0,0,0,1, 0,1,0,1, 0,1,1,1, 0,1,1,1,
			0,0,1,1, 0,1,1,1, 1,1,0,1, 1,1,0,0, 0,0,0,1,
			1,1,0,1, 0,1,0,1, 0,1,0,0, 0,0,0,1
		       };

        COMPUTE THE INDEX FOR TYPE OF LINE THROUGH THE BOX
        "o" marks the start of the line. Higher values are always
        to the right of the line.

        1        2        3        4        5        6        7
        ......   ......   ......   ....\.   ./....   ..|...   ./....
        o    .   .    /   o-----   .    o   o    o   . |  .   o    .
        .\....   ....o.   ......   ......   ..../.   ..o...   ......

        8        9        10       11       12       13       14
        .o....   ..o...   ....o.   ....o.   ......   ......   ......
        /    .   . |  .   \    \   .    \   -----o   .    o   \    .
        ......   ..|...   .o....   ......   ......   ..../.   .o....


        15 is all greater than C.
        0 is all less than C.
									*/

    int outlines (pD,pGo,pP,ptab)

      struct display_tab *pD;
      struct go_attr *pGo;
      struct p_tab *pP;
      struct a_tab *ptab;

      {
	int i,j,k,erret;

	struct a_attr *pA;
	struct project_attr *pj;
	float dsp[NDS];

	Glimit pvp;

        int l;
        float *save_xv_0, *save_xv_1, *save_xb_0, *save_xb_1;
        int did_save_xv=0, did_save_xb=0, did_save_yv=0, did_save_yb=0;
        extern int trans_axis();

	int ND;
	int save_xs[NDS];
	float save_xl[NDS],save_xf[NDS];
	char proj[17];

	pj=&p_PRJ;
	pA=ptab->pA_attr;
	erret=0;

	if (pD->dsp_seg[0] > 0 && pD->dsp_seg[3] > 0)
	  {
	   gdsg(pD->dsp_seg[0]);
	   pD->dsp_seg[0]=0;
	  }
	if (pD->leg_seg[0] > 0 && pD->leg_seg[3] > 0)
	  {
	   gdsg(pD->leg_seg[0]);
	   pD->leg_seg[0]=0;
	  }
	if (pGo->n <= 0 || pP->dsp.p <= 0) return 0;

/*		Make sure the list of values doesn't contain
		duplicates, which will invalidate the algorithm.	*/

	for (i=0;i < pGo->n-1;i++)
	  {
	   for (j=i+1;j<pGo->n;j++)
	     if (pGo->out[i] == pGo->out[j])
	       {
		for (k=j;k<pGo->n-1;pGo->out[k]=pGo->out[k+1],k++);
		(pGo->n)--;
	       }
	  }

	for (ND=i=0;i<pA->ND;i++)
	  {
	   save_xs[i]=*pA->xs[i];
	   save_xf[i]=*pA->xf[i];
	   save_xl[i]=*pA->xl[i];
	   if (*pA->xs[i] > 1) ND++;
	  }

	if (pD->dsp_seg[3] > 0 && (pP->dsp.p > 0 && pP->dsp.x1 < 1.0 &&
	    pP->dsp.y1 < 1.0 && pP->dsp.x1 > 0.0 && pP->dsp.y1 > 0.0 &&
	    pP->dsp.x2 < 1.0 && pP->dsp.y2 < 1.0 && pP->dsp.x2 > 0.0 &&
	    pP->dsp.y2 > 0.0 ) )
	  {

/*			Check NDC space is greater than zero.		*/

	   if (pP->dsp.x2-pP->dsp.x1 < .001 || pP->dsp.y2-pP->dsp.y1 < .001)
	     {
	      err_warn(1,fperr,"Error - display space is nil for OUTLINE.\n");
	      pD->dsp_seg[1]=0;
	      pD->off=2;
	      return 0;
	     }
/*			Check the number of dimensions and number of
			values of the variable's dimensions.		*/

	   if (erret == 0 && (pA->xs[0] == NULL || pA->xs[1] == NULL ||
			      *(pA->xs[0]) <= 1 || *(pA->xs[1]) <= 1) )
	     {
	      err_warn(1,fperr,
		"Error - OUTLINE requires 2 multi-valued dimensions.\n");
	      pD->off=2;
	      return 0;
	     }
	   if (erret == 0 &&  ( (pA->xs[2] != NULL && *(pA->xs[2]) > 1) ||
				(pA->xs[3] != NULL && *(pA->xs[3]) > 1)    )  )
	     {
	      err_warn(0,fperr,
			"Warning - OUTLINE is 2D; (%s) is %dD.\n"
		"          Used only first values of excess dimensions.\n",
				ptab->name,ND);
	      for (i=2;i<pA->ND;i++)
		{
		 *pA->xs[i]=1;
		 *pA->xf[i]=*pA->XF[i];
		 *pA->xl[i]=*pA->XF[i];
		}
	     }

/*                              Transform the x coordinate axis.         */
           if ( ((strcmp("linear",pGo->xat) != 0) &&
                 (strcmp("",pGo->xat) != 0)) &&
                 (strcmp("linear",pGo->proj) == 0) ) {
              if ((save_xv_0=(float *)malloc(pA->xs[0][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");
                   goto cleanup;
                 } else {
                   if (erret=trans_axis(save_xv_0,pA->xv[0],pA->xs[0][0],
                            NULL, NULL, pGo->xat, &did_save_xv, "x") == 0)
                      goto cleanup;
                 }

              if ((save_xb_0=(float *)malloc((pA->xs[0][0]+1)*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");
                   goto cleanup;
                 } else {
                   if (erret=trans_axis(save_xb_0,pA->xb[0],pA->xs[0][0]+1,
                            &pA->xf[0][0], &pA->xl[0][0], pGo->xat,
                            &did_save_xb, "x") == 0)
                      goto cleanup;
                 }
           }

/*                              Transform the y coordinate axis.         */
           if ( ((strcmp("linear",pGo->yat) != 0) &&
                 (strcmp("",pGo->yat) != 0)) &&
                 (strcmp("linear",pGo->proj) == 0) ) {
              if ((save_xv_1=(float *)malloc(pA->xs[1][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");
                   goto cleanup;
                 } else {
                   if (erret=trans_axis(save_xv_1,pA->xv[1],pA->xs[1][0],
                            NULL, NULL, pGo->yat, &did_save_yv, "y") == 0)
                      goto cleanup;
                 }
              if ((save_xb_1=(float *)malloc((pA->xs[1][0]+1)*sizeof(float)))==
                             NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");
                   goto cleanup;
                 } else {
                   if (erret=trans_axis(save_xb_1,pA->xb[1],pA->xs[1][0]+1,
                            &pA->xf[1][0], &pA->xl[1][0], pGo->yat,
                            &did_save_yb, "y") == 0)
                      goto cleanup;
                 }
           }

/*			First and last of first two dimension determines
			direction of the plot.				*/
	   dsp[0]=*pA->xf[0];
	   dsp[1]=*pA->xf[1];
	   dsp[2]=*pA->xl[0];
	   dsp[3]=*pA->xl[1];

/*			Set up the projection for this picture.		*/
	   strcpy(proj,pGo->proj);
	   if (cmpncs(proj,"Mollweide")==0 || cmpncs(proj,"Robinson")==0 ||
	       cmpncs(proj,"Polar"    )==0 )
	     {
              if (cmpncs(pA->xn[0],"longitude") != 0 ||
                  cmpncs(pA->xn[1],"latitude" ) != 0 )
		{
		 err_warn(1,fperr,
		      "Error - (OUTLINES) spherical projections work"
		      " only with data on a\n"
		      "        longitude x latitude grid.  Linear"
		      " projection is used.\n");
		 strcpy(proj,"Linear");
		}
	     }


	   /* Remember value used for click capabilities */
	   pD->dsp_used[0]=dsp[0];
	   pD->dsp_used[1]=dsp[1];
	   pD->dsp_used[2]=dsp[2];
	   pD->dsp_used[3]=dsp[3];

	   if (set_projection(proj,pP->dsp,pGo->dsp,dsp) == 0)
	     {
	      err_warn(1,fperr,"Error - in projection for OUTLINES.\n");
	      erret++;
	     }

/*		If data range is outside (or on) the display (user coordinate)
		range limits and both are on the same side,
		there is nothing that will display.			*/

	   if ( (pj->X1-*pA->xf[0])*(pj->X2-*pA->xf[0]) > 0.0 &&
		(pj->X1-*pA->xl[0])*(pj->X2-*pA->xl[0]) > 0.0 &&
		(pj->X1-*pA->xl[0])*(pj->X1-*pA->xf[0]) > 0.0 &&
		(pj->Y1-*pA->xf[1])*(pj->Y2-*pA->xf[1]) > 0.0 &&
		(pj->Y1-*pA->xl[1])*(pj->Y2-*pA->xl[1]) > 0.0 &&
		(pj->Y1-*pA->xl[1])*(pj->Y1-*pA->xf[1]) > 0.0 )
	     {
	      err_warn(1,fperr,
		   "Error - OUTLINE data is outside the display range.\n");
	      erret++;
	     }

/*              If the variable isn't computed then restore excess dimensions.*/
           if (did_save_xv)
           {
              for (l = 0; l < pA->xs[0][0]; ++l)
                 pA->xv[0][l] = save_xv_0[l];
              *pA->xf[0] = save_xf[0];
              *pA->xl[0] = save_xl[0];
              free((char *) save_xv_0);
              did_save_xv = 0;
           }
           if (did_save_xb)
           {
              for (l = 0; l < pA->xs[0][0]+1; ++l)
                 pA->xb[0][l] = save_xb_0[l];
              *pA->xf[0] = save_xf[0];
              *pA->xl[0] = save_xl[0];
              free((char *) save_xb_0);
              did_save_xb = 0;
           }

           if (did_save_yv)
           {
              for (l = 0; l < pA->xs[1][0]; ++l)
                 pA->xv[1][l] = save_xv_1[l];
              *pA->xf[1] = save_xf[1];
              *pA->xl[1] = save_xl[1];
              free((char *) save_xv_1);
              did_save_yv = 0;
           }
           if (did_save_yb)
           {
              for (l = 0; l < pA->xs[1][0]+1; ++l)
                 pA->xb[1][l] = save_xb_1[l];
              *pA->xf[1] = save_xf[1];
              *pA->xl[1] = save_xl[1];
              free((char *) save_xb_1);
              did_save_yb = 0;
           }

#ifdef cray
	   if (erret == 0 && acquire_A(ptab,"I*8") > 0)
#else
	   if (erret == 0 && acquire_A(ptab,"I*4") > 0)
#endif
	     {
/*                      Compute the mean if from CDAT or Computed */
           if (pA->f != NULL)
                 immmm(pA->un.data,pA->mask,&pA->xs[0],&pA->xw[0],2,
                        &pA->XC[0],&pA->xv[0],&pA->min,&pA->max,&pA->mean);

/*                              Transform the x coordinate axis.         */
            if ( ((strcmp("linear",pGo->xat) != 0) &&
                 (strcmp("",pGo->xat) != 0)) &&
                 (strcmp("linear",pGo->proj) == 0) ) {
              if ((save_xv_0=(float *)malloc(pA->xs[0][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");
                   goto cleanup;
                 } else {
                   if (erret=trans_axis(save_xv_0,pA->xv[0],pA->xs[0][0],
                            NULL, NULL, pGo->xat,
                            &did_save_xv, "x") == 0)
                      goto cleanup;
                 }
              if ((save_xb_0=(float *)malloc((pA->xs[0][0]+1)*sizeof(float)))==
                   NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");
                   goto cleanup;
                 } else {
                   if (erret=trans_axis(save_xb_0,pA->xb[0],pA->xs[0][0]+1,
                            &pA->xf[0][0], &pA->xl[0][0], pGo->xat,
                            &did_save_xb, "x") == 0)
                      goto cleanup;
                 }
           }

/*                              Transform the y coordinate axis.         */
           if ( ((strcmp("linear",pGo->yat) != 0) &&
                 (strcmp("",pGo->yat) != 0)) &&
                 (strcmp("linear",pGo->proj) == 0) ) {
              if ((save_xv_1=(float *)malloc(pA->xs[1][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");
                   goto cleanup;
                 } else {
                   if (erret=trans_axis(save_xv_1,pA->xv[1],pA->xs[1][0],
                            NULL, NULL, pGo->yat, &did_save_yv, "y") == 0)
                      goto cleanup;
                 }

              if ((save_xb_1=(float *)malloc((pA->xs[1][0]+1)*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");
                   goto cleanup;
                 } else {
                   if (erret=trans_axis(save_xb_1,pA->xb[1],pA->xs[1][0]+1,
                            &pA->xf[1][0], &pA->xl[1][0], pGo->yat,
                            &did_save_yb, "y") == 0)
                       goto cleanup;
                 }
           }


	      pvp.xmin=pP->dsp.x1;
	      pvp.xmax=pP->dsp.x2;
	      pvp.ymin=pP->dsp.y1;
	      pvp.ymax=pP->dsp.y2;

	      gswn(2,&pvp);
	      gsvp(2,&pvp);
	      gselnt(2);

	      gsclip(GCLIP);

	      gcrsg(pD->dsp_seg[0]=++segment_num);
	      gssgp(pD->dsp_seg[0],(pP->dsp.p+pD->pri)/1000.0);

	      if (!folout(*pA->xs[0],*pA->xs[1],pA->un.idata,pA->mask,
	         pA->xv[0],pA->xb[0],pA->xv[1],pA->xb[1],pGo) )
		{
		 erret++;
		}

	      pD->dsp_seg[1]=1;
	      pD->dsp_seg[2]=1;
	      pD->dsp_seg[3]=0;
	      gclsg();
	      gselnt(0);
	      gsclip(GNOCLIP);
	     }
	   else if (erret == 0)
	     {
	      err_warn(1,fperr,"Error - data (A_%s) from file (%s) cannot be"
		" acquired.\n",ptab->name,pA->F);
	      if (pD->dsp_seg[0] > 0) gdsg(pD->dsp_seg[0]);
	      pD->dsp_seg[0]=0;
	      pD->dsp_seg[1]=0;
	      pD->dsp_seg[2]=1;
	      pD->dsp_seg[3]=0;
	      erret++;
	     }
	   killA_tmp();
	  }
	pict_elem(pA,pP,pD,pGo->proj,pGo->xtl1,pGo->xtl2,pGo->xmt1,pGo->xmt2,
				     pGo->ytl1,pGo->ytl2,pGo->ymt1,pGo->ymt2,
				     pGo->dsp);

/*		Restore excess dimensions.				*/

/*              If the variable isn't computed then restore excess dimensions.*/
cleanup:   if (did_save_xv)
           {
              for (l = 0; l < pA->xs[0][0]; ++l)
                 pA->xv[0][l] = save_xv_0[l];
              *pA->xf[0] = save_xf[0];
              *pA->xl[0] = save_xl[0];
              free((char *) save_xv_0);
           }
           if (did_save_xb)
           {
              for (l = 0; l < pA->xs[0][0]+1; ++l)
                 pA->xb[0][l] = save_xb_0[l];
              *pA->xf[0] = save_xf[0];
              *pA->xl[0] = save_xl[0];
              free((char *) save_xb_0);
           }

           if (did_save_yv)
           {
              for (l = 0; l < pA->xs[1][0]; ++l)
                 pA->xv[1][l] = save_xv_1[l];
              *pA->xf[1] = save_xf[1];
              *pA->xl[1] = save_xl[1];
              free((char *) save_xv_1);
           }
           if (did_save_yb)
           {
              for (l = 0; l < pA->xs[1][0]+1; ++l)
                 pA->xb[1][l] = save_xb_1[l];
              *pA->xf[1] = save_xf[1];
              *pA->xl[1] = save_xl[1];
              free((char *) save_xb_1);
           }

	   for (i=2;i<pA->ND;i++)
	     {
	      *pA->xs[i]=save_xs[i];
	      *pA->xf[i]=save_xf[i];
	      *pA->xl[i]=save_xl[i];
	     }

	if (!ptab->FROM_CDAT) { /* DNW - do not remove the data form CDAT */
	  if (pA->un.idata != NULL){free((char *)pA->un.idata);pA->un.idata=NULL;}
	  if (pA->mask != NULL) {free((char *)pA->mask); pA->mask=NULL;}
	}
	if (erret == 0) return 1;
	else
	  {
	   pD->off=2;
	   return 0;
	  }
      }

/*		Follow and draw outlines.		*/

    int folout (IM,JM,X,MMask,xv,xb,yv,yb,po)

 	int IM,JM;
	int X[];
	short MMask[];
	float xb[];
	float xv[];
	float yb[];
	float yv[];
	struct go_attr *po;

/*      int IM,JM;	Limits of the array dimension sizes.		*/
/*      short X[JM,IM];	Array to be contoured.			*/
/*      short MMask[JM,IM];Mask array (0 - invalid, 1 - valid).		*/
/*      float xb[IM+1];		Values of x node bounds.		*/
/*      float yb[JM+1];		Values of y node bounds.		*/

      {
	int i,j,m;
	int ixj;
	struct table_line *pl;
	int nbox,n,i1,i2,j1,j2;
	int closed;
	float xl,yl,xu,yu;
	Gpoint pxy[201];
	int nxy;
	int err;
	int ix,jy,i0,j0,I,J;
	char *mask;
	float x0,x1,y0,y1;

	if ( (mask=(char *)malloc(IM*JM)) == NULL)
	  {
	   err_warn(1,fperr,"Error - memory for OUTLINE mask not found.\n");
	   return 0;
	  }

/*		Mask out missing data.					*/

	for (i=0,j=0;j < JM-1;)
	  {
	   ixj=j*IM+i;
	   if ( MMask[ixj]     == 0 ||
		MMask[ixj+1]   == 0 ||
		MMask[ixj+IM]  == 0 ||
		MMask[ixj+IM+1] == 0)
			mask[ixj] = -1;
	   else mask[ixj] = 0;
	   if (++i == IM-1) { i=0; j++;}
	  } /* end for i & j loop  */


/*			Set line attributes.				*/

	pl=&Tl_tab;
	for (;pl!=NULL && strcmp(po->lb,pl->name)!=0;pl=pl->next);
        if (pl == NULL)
	  {
	   free((char *)mask);
	   err_warn(1,fperr,"Error - line attributes problem.\n");
	   return 0;
	  }
        if (pl->ltyp == NULL)
	   gsln(1);
        else
	   gsln(pl->ltyp[0]);
        if (pl->ltyp == NULL)
	   gslwsc(1.0);
        else
	   gslwsc(pl->lwsf[0]);
        if (pl->ltyp == NULL)
	   gsplci(241);
        else
	   gsplci(pl->lci[0]);

/*	Set nodes to be outlined.
	The validity of these areas is determined prior to entry and
	indicated in the array, mask (-ve = invalid, >=0 = valid).	*/

	set_outline(po->n,po->out,X,mask,IM,JM,&nbox,&i1,&i2,&j1,&j2);

	if (nbox > 0)
	  {
/*			Find the limits of the grid.			*/
	   if (xv[0] < xv[IM-1]) {xl=xv[0]; xu=xv[IM-1];}
	   else			 {xu=xv[0]; xl=xv[IM-1];}
	   if (yv[0] < yv[JM-1]) {yl=yv[0]; yu=yv[JM-1];}
	   else			 {yu=yv[0]; yl=yv[JM-1];}

	   n=nbox;
	   for (i=i1,j=j1; j <= j2;)
	     {
/*			Break out for each level when all boxes plotted.*/

	      if (n <= 0) break; 

	      ixj=j*IM+i;
	      if (mask[ixj] > 0 && mask[ixj] < 16)
		{
/*			Find the beginning of the line.			*/

		 trace_begin(mask,IM,JM,i,j,&i0,&j0,&ix,&jy,&closed);


/*			Compute points and draw the line.		*/

		 nxy=0;
		 I=i0; J=j0;
		 ixj=J*IM+I;
		 m=mask[ixj];
/*		 if (m > 16 || m <= 0) break;			*/
		 if (m == 10)
		   {
		    if (jy < 0) {m=11; mask[ixj]=14;}
		    else	   {m=14; mask[ixj]=11;}
		   }
		 else if (m == 5)
		   {
		    if (ix < 0) {m=13; mask[ixj]=7;}
		    else 	   {m=7;  mask[ixj]=13;}
		   }
		 else
		   {
		    mask[ixj]+=16;
		    n--;
		   }
		 m--;

		 x0=xb[I+IN[m][0][0]+IN[m][0][1]];
		 x0=(x0>xl)?x0:xl;
		 x0=(x0<xu)?x0:xu;
		 y0=yb[J+JN[m][0][0]+JN[m][0][1]];
		 y0=(y0>yl)?y0:yl;
		 y0=(y0<yu)?y0:yu;
		 pxy[nxy].x=x0;
		 pxy[nxy].y=y0;

		 while (I >= i1 && I <= i2 && J >= j1 && J <= j2)
		   {
		    nxy++;
		    if (nxy == 200)
		      {
		       x1=pxy[nxy].x;
		       x1=(x1>xl)?x1:xl;
		       x1=(x1<xu)?x1:xu;
		       pxy[nxy].x=x1;
		       y1=pxy[nxy].y;
		       y1=(y1>yl)?y1:yl;
		       y1=(y1<yu)?y1:yu;
		       pxy[nxy].y=y1;

		       if ( (err=proj_pl(nxy+1,pxy)) != 0)
			 {
			  err_warn(1,fperr,"Error - folow (%d) polyline\n",err);
			  free((char *)mask);
			  return 0;
			 }
		       pxy[0].x=pxy[nxy].x;
		       pxy[0].y=pxy[nxy].y;
		       nxy=1;
		      }
		    pxy[nxy].x=xb[I+1];
		    pxy[nxy].y=yb[J+1];
		    pxy[nxy+1].x=xb[I+IN[m][1][0]+IN[m][1][1]];
		    pxy[nxy+1].y=yb[J+JN[m][1][0]+JN[m][1][1]];

		    ix=IN[m][1][0]+IN[m][1][1]-1;
		    jy=JN[m][1][0]+JN[m][1][1]-1;
		    I+=ix;
		    J+=jy;
		    if (I >= 0 && I < IM && J >= 0 && J < JM)
		      {
		       ixj=J*IM+I;
		       m=mask[ixj];
		       if (m > 16 || m <= 0) break;
		       if (m == 10)
		         {
		          if (jy > 0) {m=14; mask[ixj]=11;}
		          else	      {m=11; mask[ixj]=14;}
		         }
		       else if (m == 5)
		         {
		          if (ix > 0) {m=7; mask[ixj]=13;}
		          else	      {m=13; mask[ixj]=7;}
		         }
		       else
		         {
		          n--;  /* Reduce the count of nodes to plot.	*/
			  mask[ixj]+=16;
		         }
		       m--;
		      }
		   }
		 nxy++;
		 x1=pxy[nxy].x;
		 x1=(x1>xl)?x1:xl;
		 x1=(x1<xu)?x1:xu;
		 pxy[nxy].x=x1;
		 y1=pxy[nxy].y;
		 y1=(y1>yl)?y1:yl;
		 y1=(y1<yu)?y1:yu;
		 pxy[nxy].y=y1;
		 if ((err=proj_pl(nxy+1,pxy)) != 0)	    
		   {
		    err_warn(1,fperr,"Error - folow (%d) OUTLINE\n",err);
		    free((char *)mask);
		    return 0;
		   }

/*			Increment i and j, iff the initial node wasn't
			a double crossing (i.e.mask = 10 or 5) that
			didn't get completed.				*/

		}
	      if (mask[i+j*IM] <= 0 || mask[i+j*IM] > 15)
		{
		 i=(i<i2)?i+1:0;
		 if (i == 0) j++;
		}
	     }  /*		End for i and j <= j2 loop.		*/
	  } /* end for k loop  */

	free((char *)mask);
	return 1;
      }


/*	Test for existence of a crossing in the grid box.		*/

    int set_outline (n,L,X,mask,IM,JM,nbox,i1,i2,j1,j2)

	int n;			/* Number of values to outline.		*/
	int L[];		/* Values to be outlined.		*/
	int X[];		/* Test four values of the grid box.	*/
				/*  a4------a3				*/
				/*   |      |				*/
				/*  a1------a2				*/
	char mask[];		/* Indicate crossings.			*/
	int IM,JM;		/* Size of 2D array.			*/
	int *nbox;		/* Number of boxes active.		*/
	int *i1,*i2,*j1,*j2;	/* Bound of lines in the array.		*/

      {
       int i,j,k,ixj,nx;


	*i1=IM-1;	/* Limits (i1 & i2) of indices involved in "i".	*/
	*i2=0;
	*j1=JM-1;	/* Limits (j1 & j2) of indices involved in "j".	*/
	*j2=0;
	*nbox=0;

       for (i=0,j=0; j < JM-1; )
	 {
	  ixj=j*IM+i;

	  if (mask[ixj] >= 0)
	    {
	     nx=0;
	     for (k=0;k<n;k++)
	       {

		if (X[ixj] 	== L[k]) nx+=1;
		if (X[ixj+1] 	== L[k]) nx+=2;
		if (X[ixj+1+IM]	== L[k]) nx+=4;
		if (X[ixj+IM] 	== L[k]) nx+=8;
		if (nx == 15) nx=0;
	       }
	     mask[ixj]=nx;
	     if (nx > 0)
	       {
	        (*nbox)++;
	        if (nx%5 == 0) (*nbox)++;
	        if (*i1 > i) *i1=i;
	        if (*i2 < i) *i2=i;
	        if (*j1 > j) *j1=j;
	        if (*j2 < j) *j2=j;
	       }
	    }
	  if (++i == IM-1)
	    { i=0; j++;}
	 }
       if (*nbox == 0) *i1=*i2=*j1=*j2=0;
       return 1;
      }
