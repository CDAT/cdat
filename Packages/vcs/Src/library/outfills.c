#include "gks.h"
#include "gksshort.h"
#ifdef USEX11
#include <X11/Xlib.h>
#endif
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
#include "workstations.h"

    extern FILE *fpin,*fpout,*fperr;

    extern struct table_fill Tf_tab;
    extern struct table_pattern Pat_tab;

    extern struct project_attr p_PRJ;

    extern char PRJ_names[PRJ_TYPES][17];

    extern int segment_num;

    extern int IN[14][2][2],JN[14][2][2];


    int outfills (pD,pGfo,pP,ptab)

      struct display_tab *pD;
      struct gfo_attr *pGfo;
      struct p_tab *pP;
      struct a_tab *ptab;

      {
	int i,j,k,erret;
	float dsp[NDS];

	int ND;
	int save_xs[NDS];
	float save_xl[NDS],save_xf[NDS];
	char proj[17];

	Glimit pvp;

        int l;
        float *save_xb_0, *save_xb_1;
        int did_save_xb=0, did_save_yb=0;
        extern int trans_axis();

	struct a_attr *pA;
	struct project_attr *pj;
	erret=0;

	if (pGfo->n <= 0) erret++;

/*		Make sure the list of values doesn't contain
		duplicates, which will invalidate the algorithm.	*/

	for (i=0;i<pGfo->n-1;i++)
	  {
	   for (j=i+1;j<pGfo->n;j++)
	     if (pGfo->out[i] == pGfo->out[j])
	       {
		for (k=j;k<pGfo->n-1;pGfo->out[k]=pGfo->out[k+1],k++);
		(pGfo->n)--;
	       }
	  }

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

	for (ND=i=0;i<pA->ND;i++)
	  {
	   save_xs[i]=*pA->xs[i];
	   save_xf[i]=*pA->xf[i];
	   save_xl[i]=*pA->xl[i];
	   if (*pA->xs[i] > 1) ND++;
	  }






/*			Check NDC space is greater than zero.		*/

	if (pP->dsp.x2-pP->dsp.x1 < .001 || pP->dsp.y2-pP->dsp.y1 < .001)
	  {
	   err_warn(1,fperr,
			"Error - NDC display space is nil for OUTFILL.\n");
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
	if (erret == 0 && (pA->xs[2] != NULL && *(pA->xs[2]) > 1) ||
	        (pA->xs[3] != NULL && *(pA->xs[3]) > 1)    )
	  {
	   err_warn(0,fperr,
			"Warning - OUTFILL is 2D; (%s) is %dD.\n"
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
           if ( ((strcmp("linear",pGfo->xat) != 0) &&
                 (strcmp("",pGfo->xat) != 0)) &&
                 (strcmp("linear",pGfo->proj) == 0) ) {
              if ((save_xb_0=(float *)malloc((pA->xs[0][0]+1)*sizeof(float)))==
                   NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");                   goto cleanup;
              } else {
                   if (erret=trans_axis(save_xb_0,pA->xb[0],pA->xs[0][0]+1,                            &pA->xf[0][0], &pA->xl[0][0], pGfo->xat,
                            &did_save_xb, "x") == 0)
                      goto cleanup;
              }
           }

/*                              Transform the y coordinate axis.         */
           if ( ((strcmp("linear",pGfo->yat) != 0) &&
                 (strcmp("",pGfo->yat) != 0)) &&
                 (strcmp("linear",pGfo->proj) == 0) ) {
              if ((save_xb_1=(float *)malloc((pA->xs[1][0]+1)*sizeof(float)))==
                             NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");                   goto cleanup;
              } else {
                   if (erret=trans_axis(save_xb_1,pA->xb[1],pA->xs[1][0]+1,                            &pA->xf[1][0], &pA->xl[1][0], pGfo->yat,
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
	strcpy(proj,pGfo->proj);
	if (cmpncs(proj,"Mollweide")==0 || cmpncs(proj,"Robinson")==0 ||
	       cmpncs(proj,"Polar"    )==0 )
	  {
           if (cmpncs(pA->xn[0],"longitude") != 0 ||
                  cmpncs(pA->xn[1],"latitude" ) != 0 )
	     {	
	      err_warn(1,fperr,
		      "Error - (OUTFILLS) spherical projections work"
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

	if (set_projection(proj,pP->dsp,pGfo->dsp,dsp) == 0)
	  {
	   err_warn(1,fperr,"Error - in projection for OUTFILLS.\n");
	   erret++;
	  }

/*		If data range is outside (or on) the display (user coordinate)
		range limits and both are on the same side,
		there is nothing that will display.			*/

	if (    (pj->X1-*pA->xf[0])*(pj->X2-*pA->xf[0]) > 0.0 &&
		(pj->X1-*pA->xl[0])*(pj->X2-*pA->xl[0]) > 0.0 &&
		(pj->X1-*pA->xl[0])*(pj->X1-*pA->xf[0]) > 0.0 &&
		(pj->Y1-*pA->xf[1])*(pj->Y2-*pA->xf[1]) > 0.0 &&
		(pj->Y1-*pA->xl[1])*(pj->Y2-*pA->xl[1]) > 0.0 &&
		(pj->Y1-*pA->xl[1])*(pj->Y1-*pA->xf[1]) > 0.0 )
	  {
	   err_warn(1,fperr,
		   "Error - OUTFILL data is outside the display range.\n");
	   erret++;
	  }

/*              If the variable isn't computed then restore excess dimensions.*/
           if (did_save_xb)
           {
              for (l = 0; l < pA->xs[0][0]+1; ++l)
                 pA->xb[0][l] = save_xb_0[l];
              *pA->xf[0] = save_xf[0];
              *pA->xl[0] = save_xl[0];
              free((char *) save_xb_0);
              did_save_xb = 0;
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

	if (pD->dsp_seg[3] > 0 && (pP->dsp.p > 0 && pP->dsp.x1 < 1.0 &&
	    pP->dsp.y1 < 1.0 && pP->dsp.x1 > 0.0 && pP->dsp.y1 > 0.0 &&
	    pP->dsp.x2 < 1.0 && pP->dsp.y2 < 1.0 && pP->dsp.x2 > 0.0 &&
	    pP->dsp.y2 > 0.0 ) )
	  {
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
           if ( ((strcmp("linear",pGfo->xat) != 0) &&
                 (strcmp("",pGfo->xat) != 0)) &&
                 (strcmp("linear",pGfo->proj) == 0) ) {
              if ((save_xb_0=(float *)malloc((pA->xs[0][0]+1)*sizeof(float)))==
                   NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");                   goto cleanup;
              } else {
                   if (erret=trans_axis(save_xb_0,pA->xb[0],pA->xs[0][0]+1,                            &pA->xf[0][0], &pA->xl[0][0], pGfo->xat,
                            &did_save_xb, "x") == 0)
                      goto cleanup;
              }
           }

/*                              Transform the y coordinate axis.         */
           if ( ((strcmp("linear",pGfo->yat) != 0) &&
                 (strcmp("",pGfo->yat) != 0)) &&
                 (strcmp("linear",pGfo->proj) == 0) ) {
              if ((save_xb_1=(float *)malloc((pA->xs[1][0]+1)*sizeof(float)))==
                             NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");                   goto cleanup;
              } else {
                   if (erret=trans_axis(save_xb_1,pA->xb[1],pA->xs[1][0]+1,                            &pA->xf[1][0], &pA->xl[1][0], pGfo->yat,
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

	      filbox(*pA->xs[0],*pA->xs[1],pA->un.idata,pA->mask,
	         pA->xb[0],pA->xb[1],pGfo);

	      pD->dsp_seg[1]=1;
	      pD->dsp_seg[2]=1;
	      pD->dsp_seg[3]=0;

	      gclsg();

	      gselnt(0);

	      gsclip(GNOCLIP);

	     }
	   else if (erret == 0)
	     {
	      if (pA->F != NULL)
		err_warn(1,fperr,
			"Error - outfill data (A_%s) from file (%s) not"
			  " acquired.\n",ptab->name,pA->F);
	      else if (pA->f != NULL)
		err_warn(1,fperr,
			"Error - outfill data (A_%s) from (%s) cannot be"
			  " computed.\n",ptab->name,pA->f);
	      else
		err_warn(1,fperr,"Error - outfilling (A_%s).\n",ptab->name);
	      if (pD->dsp_seg[0] > 0) gdsg(pD->dsp_seg[0]);
	      pD->dsp_seg[0]=0;
	      pD->dsp_seg[1]=0;
	      pD->dsp_seg[2]=1;
	      pD->dsp_seg[3]=0;
	      erret++;
	     }
	   killA_tmp();
	  }
	pict_elem(pA,pP,pD,pGfo->proj,pGfo->xtl1,pGfo->xtl2,pGfo->xmt1,
		pGfo->xmt2,pGfo->ytl1,pGfo->ytl2,pGfo->ymt1,pGfo->ymt2,
				     pGfo->dsp);

/*		If the variable isn't computed then restore excess dimensions.*/
cleanup:   if (did_save_xb)
           {
              for (l = 0; l < pA->xs[0][0]+1; ++l)
                 pA->xb[0][l] = save_xb_0[l];
              *pA->xf[0] = save_xf[0];
              *pA->xl[0] = save_xl[0];
              free((char *) save_xb_0);
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
	  if (pA->un.data != NULL) {free((char *)pA->un.data);pA->un.data=NULL;}
	  if (pA->mask != NULL) {free((char *)pA->mask); pA->mask=NULL;}
	}
	if (erret == 0) return 1;
	else
	  {
	   pD->off=2;
	   return 0;
	  }
      }

/*		Follow and fill outlines.				*/

    int filbox(IM,JM,X,MMask,xb,yb,po)

 	int IM,JM;
	int X[];
	short MMask[];
	float xb[];
	float yb[];
	struct gfo_attr *po;

/*      int IM,JM;	Limits of the array dimension sizes.		*/
/*      short X[JM,IM];	Array to be contoured.			*/
/*      short MMask[JM,IM];Mask array (0 - invalid, 1 - valid).		*/
/*      float xb[IM+1];		Values of x node bounds.		*/
/*      float yb[JM+1];		Values of y node bounds.		*/

      {
	int i,j,k,m;
	int ixj,gotit;
	struct table_fill *pf;
	Gpoint pxy[201];
	Gpoint fxy;


/*			Set fill attributes.				*/

	for (pf=&Tf_tab;pf!=NULL && strcmp(po->f,pf->name)!=0;pf=pf->next);
        if (pf == NULL)
	  {
	   err_warn(1,fperr,
	     "Warning - fill attribute set name not found - use default.\n");
	   pf=&Tf_tab;
	  }
	if (pf->faci==NULL) gsfaci(241);
	else gsfaci(pf->faci[0]);
	if (pf->fais==NULL) gsfais(0);
	  else gsfais(pf->fais[0]);
	if (pf->fasi==NULL) gsfasi(1);
	  else gsfasi(pf->fasi[0]);

	fxy.x=pf->w;
	fxy.y=pf->h;
	gspa(&fxy);
	pxy[0].x=pf->x;
	pxy[0].y=pf->y;
	gsparf(&pxy[0]);

	m=0;
	for (j=0; j < JM; j++)
	  for (i=0; i < IM; i++)
	  {
	   ixj=j*IM+i;
	   if (MMask[ixj] >= 0)
	     {
	      for (gotit=0,k=0;k < po->n;k++)
		 if (X[ixj] == po->out[k]) {gotit=1; break;}
	     }
	   if (gotit)
	     {
	      if (m == 0)
	        {
	         pxy[m].x=xb[i];
	         pxy[m].y=yb[j+1];
	         m++;
	         pxy[m].x=xb[i];
	         pxy[m].y=yb[j];
	         m++;
	        }
	      pxy[m].x=xb[i+1];
	      pxy[m].y=yb[j];
	      pxy[m+1].x=xb[i+1];
	      pxy[m+1].y=yb[j+1];
	     }
	   else if (m > 0) {proj_gfa(4,pxy); m=0;}

	   if (m > 0 && i == IM-1) {proj_gfa(4,pxy); m=0;}

	  }  /*		End for i and for j loop.		*/

	return 1;
      }
