#include "gks.h"
#include "gksshort.h"
#ifdef USEX11
#include <X11/Xlib.h>
#endif
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
#include "workstations.h"
#include "vcs_legacy_marker.h"

#define NMAX 1000

    extern FILE *fpin,*fpout,*fperr;

    extern struct workstations Wkst[];
    extern struct table_text Tt_tab;
    extern struct table_chorn To_tab;
    extern struct table_line Tl_tab;
    extern struct table_mark Tm_tab;

    extern struct display_tab D_tab;

    extern struct project_attr p_PRJ;

    extern char PRJ_names[PRJ_TYPES][17];

    extern int segment_num;

/*			Proto-types					*/

    int nicedf(float a,float b,float *dr,int *pw10,float *center);
    int plot_line (int im,float *x,short *mx,float *y,short *my,
							char *lb,char *mb);

/*		Draw ScatterPlot as specified.				*/

    int scatter_plot(
	      struct display_tab *pD,
	      struct gSp_attr *pGSp,
	      struct p_tab *pP,
	      struct a_tab *ptabx,
	      struct a_tab *ptaby
	     )

      {
	int i,j,k,erret,pw10,ex,ey,sx,sy;
	float dsp[NDS],dr,dx,x1,x2,dy,y1,y2,center,eps;
	struct a_attr *pX,*pY;
	struct project_attr *pj;
        extern int trans_axis();
        extern int trans_coord();

	Glimit pvp;

	int NDx,NDy;
	int l,ierr;
        float saveX_min,saveX_max,saveY_min,saveY_max;

	pj=&p_PRJ;
	pX=ptabx->pA_attr;
	pY=ptaby->pA_attr;
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
	sx=sy=1;

	for (NDx=i=0;i<pX->ND;i++)
	   if (*pX->xs[i] > 1) {NDx++; sx=(*pX->xs[i])*sx;}
	for (NDy=i=0;i<pY->ND;i++)
	   if (*pY->xs[i] > 1) {NDy++; sy=(*pY->xs[i])*sy;}

/*			Check NDC space is greater than zero.		*/

	if (pP->dsp.x2-pP->dsp.x1 < .001 || pP->dsp.y2-pP->dsp.y1 < .001)
	  {
	   err_warn(1,fperr,
			"Error - NDC display space is nil for ScatterPlot.\n");
	   pD->dsp_seg[1]=0;
	   pD->off=2;
	   return 0;
	  }

/*			Check the number of dimensions and number of
			values of the variable's dimensions.		*/

	if (NDx != NDy || sx != sy)
	  {
	   err_warn(1,fperr,
		"Error - ScatterPlot requires equal size for X and Y.\n");
	   pD->off=2;
	   return 0;
	  }
	if (erret == 0)
	  {
	   for (i=0; i < pX->ND; i++)
	     {
	      if (pX->xs[i] != NULL && *pX->xs[i] > 1)
		{
		 j=*pX->xs[i]-1;
		 eps=1.e-6*fabs(*pX->xv[i]-*(pX->xv[i]+j));
		 if (pY->xs[i]==NULL || *pY->xs[i]!=*pX->xs[i])
		   /* ok do not check dim names are the same, just values */
/*  || */
/* 			cmpncs(pY->xn[i],pX->xn[i])!=0)	 { */
		   erret++;
		 else
		   {
		    for (j=0; j < *pY->xs[i]; j++)
		      {
		       if (fabs(*(pX->xv[i]+j)-*(pY->xv[i]+j)) > eps)
			 {
			  erret++;
			  break;
			 }
		      }
		   }
		 if (erret!=0 )
		   {
		    err_warn(1,fperr,
			"Error - ScatterPlot requires X and Y defined in the"
				" same place, in multi-valued dimensions.\n"
		"        Check multi-valued dimension names and values.\n");
		    pD->off=2;
		    return 0;
		   }
		}
	     }
	  }

#ifdef cray
	if (erret==0 && (ex=acquire_A(ptabx,"R*8")) > 0 &&
			   (ey=acquire_A(ptaby,"R*8")) > 0)
#else
	if (erret==0 && (ex=acquire_A(ptabx,"R*4")) > 0 &&
			   (ey=acquire_A(ptaby,"R*4")) > 0)
#endif
	  {

  /* 				Store the min and max values 		 */
           saveX_min = pX->min;
           saveX_max = pX->max;
           saveY_min = pY->min;
           saveY_max = pY->max;

   /*                     	Transform the x coordinate axis.    	*/
           if ( ((strcmp("linear",pGSp->xat) != 0) &&
                 (strcmp("",pGSp->xat) != 0)) &&
                 (strcmp("linear",pGSp->proj) == 0) ) {
               if (ierr=trans_coord(&pX->min, &pX->max, pGSp->xat,
                            NULL, "x") == 0)
                   goto cleanup;
               /* Transform the X data. */
               if (ierr=trans_axis(pX->un.data, pX->un.data, sx,
                            NULL, NULL, pGSp->xat, NULL, "x") == 0)
                   goto cleanup;
           }

/*			Get a default for the X axis.			*/
	   if (!nicedf(pX->min,pX->max,&dr,&pw10,&center) )
	     {
	      dr=1.0;
	      pw10=0.0;
	      center=pX->min;
	     }
	   dx=dr*pow( (double) 10.0, (double) pw10);
	   k=(pX->min-center)/dx;
	   if (pX->min < center) k--;
	   x1=center+k*dx;
	   k=(pX->max-center)/dx;
	   if (pX->max > center) k++;
	   x2=center+k*dx;

   /*                     	Transform the y coordinate axis.    	*/
           if ( ((strcmp("linear",pGSp->yat) != 0) &&
                 (strcmp("",pGSp->yat) != 0)) &&
                 (strcmp("linear",pGSp->proj) == 0) ) {
               if (ierr=trans_coord(&pY->min, &pY->max, pGSp->yat,
                            NULL, "y") == 0)
                  goto cleanup;
               /* Transform the Y data. */
               if (ierr=trans_axis(pY->un.data, pY->un.data, sy,
                            NULL, NULL, pGSp->yat, NULL, "y") == 0)
                  goto cleanup;
           }

/*			Get a default for the Y axis.			*/
	   if (!nicedf(pY->min,pY->max,&dr,&pw10,&center) )
	     {
	      dr=1.0;
	      pw10=0.0;
	      center=pY->min;
	     }
	   dy=dr*pow( (double) 10.0, (double) pw10);
	   k=(pY->min-center)/dy;
	   if (pY->min < center) k--;
	   y1=center+k*dy;
	   k=(pY->max-center)/dy;
	   if (pY->max > center) k++;
	   y2=center+k*dy;
		 
/*			First and last of first dimension determines
			direction of the plot.				*/

	   dsp[0]=x1;
	   dsp[1]=y1;
	   dsp[2]=x2;
	   dsp[3]=y2;

/*			Set up the projection for this picture.		*/


	   /* Remember value used for click capabilities */
	   pD->dsp_used[0]=dsp[0];
	   pD->dsp_used[1]=dsp[1];
	   pD->dsp_used[2]=dsp[2];
	   pD->dsp_used[3]=dsp[3];

	   if (set_projection(pGSp->proj,pP->dsp,pGSp->dsp,dsp) == 0)
	     {
	      err_warn(1,fperr,"Error - in projection for ScatterPlot.\n");
	      erret++;
	     }
/*		If data range is outside (or on) the display (user coordinate)
		range limits and both are on the same side,
		there is nothing that will display.			*/

	   if (((pj->X1-pX->min)*(pj->X2-pX->min) > 0.0 &&
		   (pj->X1-pX->max)*(pj->X2-pX->max) > 0.0 &&
		   (pj->X1-pX->max)*(pj->X1-pX->min) > 0.0 ) ||
	 	  ((pj->Y1-pY->min)*(pj->Y2-pY->min) > 0.0 &&
		   (pj->Y1-pY->max)*(pj->Y2-pY->max) > 0.0 &&
		   (pj->Y1-pY->max)*(pj->Y1-pY->min) > 0.0) )
	     {
	      err_warn(1,fperr,
		   "Error - X(xyzt) /and/or Y(xyzt) data is outside the display"
								" range.\n");
	      erret++;
	     }
	     
	   if (erret == 0 && 
		    pD->dsp_seg[3] > 0 && (pP->dsp.p > 0 && pP->dsp.x1 < 1.0 &&
		    pP->dsp.y1 < 1.0 && pP->dsp.x1 > 0.0 && pP->dsp.y1 > 0.0 &&
		    pP->dsp.x2 < 1.0 && pP->dsp.y2 < 1.0 && pP->dsp.x2 > 0.0 &&
		    pP->dsp.y2 > 0.0 ))
	     {
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

	      plot_line(sx,pX->un.data,pX->mask,pY->un.data,pY->mask,
						NULL,pGSp->mb);

	      gclsg();
	      gselnt(0);

	      gsclip(GNOCLIP);

	      pD->dsp_seg[1]=1;
	      pD->dsp_seg[2]=1;
	      pD->dsp_seg[3]=0;
	     }

	  }

	else if (erret == 0)
	  {
	   if (pX->F != NULL && ex==0)
	     {
	      err_warn(1,fperr,
			"Error - X(...) data (A_%s) from file (%s) not"
			  " acquired.\n",ptabx->name,pX->F);
	      pX->notok=1;
	     }
	   else if (pX->f != NULL && ex==0)
	     {
	      err_warn(1,fperr,
			"Error - X(...) data (A_%s) from (%s) cannot be"
			  " computed.\n",ptabx->name,pX->f);
	      pX->notok=1;
	     }
	   if (pY->F != NULL && ey==0)
	     {
	      err_warn(1,fperr,
			"Error - Y(...) data (A_%s) from file (%s) not"
			  " acquired.\n",ptaby->name,pY->F);
	      pY->notok=1;
	     }
	   else if (pY->f != NULL && ey==0)
	     {
	      err_warn(1,fperr,
			"Error - Y(...) data (A_%s) from (%s) cannot be"
			  " computed.\n",ptaby->name,pY->f);
	      pX->notok=1;
	     }
	   else
	      err_warn(1,fperr,
		   "Error - with X(...) or Y(...) component (A_%s, A_%s).\n",
					ptabx->name,ptaby->name);
	   if (pD->dsp_seg[0] > 0) gdsg(pD->dsp_seg[0]);
	   pD->dsp_seg[0]=0;
	   pD->dsp_seg[1]=0;
	   pD->dsp_seg[2]=1;
	   pD->dsp_seg[3]=0;
	   erret++;
	  }
	killA_tmp();

	if (erret == 0 && pD->leg_seg[3] > 0 && pD->dsp_seg[3] > 0)
	  {
	   gcrsg(pD->leg_seg[0]=++segment_num);
	   gssgp(pD->leg_seg[0],(pP->leg.p+pD->pri)/1000.0);

	   if (legendGSp(pGSp->mb,pX->n,pY->n,pP,pD) > 0)
	     {
	      gclsg();
	      pD->leg_seg[1]=1;
	      pD->leg_seg[2]=1;
	      pD->leg_seg[3]=0;
	     }
	   else
	     {
	      gclsg();
	      gdsg(pD->leg_seg[0]);
	      pD->leg_seg[0]=0;
	      pD->leg_seg[1]=0;
	      pD->leg_seg[2]=0;
	      pD->leg_seg[3]=0;
	      erret++;
	     }
	  }

	if (erret == 0)
	   pict_elem(pX,pP,pD,pGSp->proj,
		pGSp->xtl1,pGSp->xtl2,pGSp->xmt1,pGSp->xmt2,
		pGSp->ytl1,pGSp->ytl2,pGSp->ymt1,pGSp->ymt2,
				     pGSp->dsp);

cleanup:pX->min = saveX_min;
        pX->max = saveX_max;
	pY->min = saveY_min;
        pY->max = saveY_max;

	if (!ptabx->FROM_CDAT) { /* DNW - do not remove the data form CDAT */
	  if (pX->un.data != NULL) {free((char *)pX->un.data);pX->un.data=NULL;}
	  if (pX->mask != NULL) {free((char *)pX->mask); pX->mask=NULL;}
	}
	if (!ptaby->FROM_CDAT) { /* DNW - do not remove the data form CDAT */
	  if (pY->un.data != NULL) {free((char *)pY->un.data);pY->un.data=NULL;}
	  if (pY->mask != NULL) {free((char *)pY->mask); pY->mask=NULL;}
	}
	if (erret == 0) return 1;
	else
	  {
	   pD->off=2;
	   return 0;
	  }
       }


/*			Display the legend for ScatterPlot.		*/


    int legendGSp(
		    char *mb,
		    char *xname,
		    char *yname,
		    struct p_tab *pP,
		    struct display_tab *pD
		   )

      {
       extern struct vcs_legacy_marker Vma_tab;
       Gpoint pxy[2];
	
       struct pe_leg pe;
       struct table_text *pTt;
       struct table_chorn *pTo;
       struct table_mark *pTm;
       Gtxalign pta;
       Gtxpath ptp;
       char xyname[256];

       if (pP == NULL || pD == NULL)
	 {
	  err_warn(1,fperr,
			"Error - no template or no display, so no legend.\n");
	  return 0;
	 }

       pe=pP->leg;
       if (pe.x1 <= 0.0 || pe.x1 >= 1.0 || pe.x2 <= 0.0 || pe.x2 >= 1.0 ||
	   pe.y1 <= 0.0 || pe.y1 >= 1.0 || pe.y2 <= 0.0 || pe.y2 >= 1.0 ||
	   pe.p <= 0)
	    return 1;

       for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
	    if (strcmp(pTt->name,pe.tb) == 0) break;
       if (pTt == NULL) pTt=&Tt_tab;
       for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
	    if (strcmp(pTo->name,pe.to) == 0) break;
       if (pTo == NULL) pTo=&To_tab;

       set_text_attr(pTt,pTo);
	   pta.hor=GTH_LEFT;
	   pta.ver=GTV_HALF;
       gstxal(&pta);
	   ptp=GTP_RIGHT;
       gstxp(ptp);

       pTm=NULL;
       if (mb != NULL && mb[0] != '\0')
	  for (pTm=&Tm_tab; pTm != NULL; pTm=pTm->next)
	     if (strcmp(pTm->name,mb) == 0) break;

       if (pTm != NULL)
	 {
/*	  gsmk(pTm->mtyp);
	  gsmksc(pTm->msize);
	  gspmci(pTm->mci);*/
          Vma_tab.type = pTm->mtyp[0];
          Vma_tab.size = pTm->msize[0];
          Vma_tab.colour = pTm->mci[0];
	  pxy[0].x=0.5*(pe.x1+pe.x2);
	  pxy[0].y=pe.y1;
	  vgpm(1,pxy);
	 }
       if ((int)strlen(xname)+(int)strlen(yname) < 250)
		sprintf(xyname,"%s vs %s",xname,yname);
       else xyname[0]='\0';

       if (xyname[0] != '\0')
	 {
	  pxy[0].x=pe.x2+.01;
	  pxy[0].y=pe.y2;
	  gtx (pxy,(unsigned char *)xyname);
	 }
       return 1;
      }
