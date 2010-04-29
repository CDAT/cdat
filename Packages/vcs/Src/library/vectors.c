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

#define NMAX 1000

    extern FILE *fpin,*fpout,*fperr;

    extern struct workstations Wkst[];
    extern struct table_text Tt_tab;
    extern struct table_chorn To_tab;
    extern struct table_line Tl_tab;

    extern struct display_tab D_tab;

    extern struct project_attr p_PRJ;

    extern struct default_continents Dc;

    extern char PRJ_names[PRJ_TYPES][17];

    extern int segment_num;

    int legendGv(int type,int pos,float vsf,char *lb,float vlen,
				struct p_tab *pP,struct display_tab *pD);

    extern int generic_world (float x1,float y1,float x2,float y2, char *lb,
                               char *trans_xaxis, char *trans_yaxis);
    extern int generic_world2 (float x1,float y1,float x2,float y2, char *lb,
                               char *trans_xaxis, char *trans_yaxis);

/*		Compute a neat, mean velocity.	*/

    float mean_veloc(float x,float y)
      {
       float vi;
       double xx,vv;
       int k;

       xx=x*x+y*y;
       vv=sqrt(xx);
       if (vv < 1.e-20) return 0.0;
       if ( vv > 1.0) {k=(int)vv; vi=k; return vi;}
       k=log10(vv);
       k--;
       vi=(int) (vv*pow( (double) 10.0, (double) -k));
       return vi*pow( (double) 10.0, (double) k);
      }

/*		Draw vectors as specified.				*/

    int vectors(pD,pGv,pP,ptabx,ptaby)

      struct display_tab *pD;
      struct gv_attr *pGv;
      struct p_tab *pP;
      struct a_tab *ptabx,*ptaby;

      {
	int i,erret;
	float dsp[NDS],vlen,vxm,vym;
	struct a_attr *pAx,*pAy;
	struct project_attr *pj;

	int l;
        int did_savex_x=0, did_savex_y=0;
        float *savex_xv_0, *savex_xv_1;
        float X1,Y1,X2,Y2; /*   generic_world coordinate values */
        extern int trans_axis();

	Glimit pvp;

	int NDx,NDy;
	int savex_xs[NDS],savey_xs[NDS];
	float savex_xl[NDS],savex_xf[NDS],savey_xl[NDS],savey_xf[NDS];
	char proj[17];

	float *save_data,save_min,save_max,save_mean;
	short *save_mask;
	int size;
	int savecontinents;

	pj=&p_PRJ;
	pAx=ptabx->pA_attr;
	pAy=ptaby->pA_attr;
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

	vlen=fabs(pGv->vlen);

	for (NDx=i=0;i<pAx->ND;i++)
	  {
	   savex_xs[i]=*pAx->xs[i];
	   savex_xf[i]=*pAx->xf[i];
	   savex_xl[i]=*pAx->xl[i];
	   if (*pAx->xs[i] > 1) NDx++;
	  }

	for (NDy=i=0;i<pAy->ND;i++)
	  {
	   savey_xs[i]=*pAy->xs[i];
	   savey_xf[i]=*pAy->xf[i];
	   savey_xl[i]=*pAy->xl[i];
	   if (*pAy->xs[i] > 1) NDy++;
	  }


/*			Check the number of dimensions and number of
			values of the variable's dimensions.		*/

	if (erret == 0 && (
	       pAx->xs[0] == NULL || pAx->xs[1] == NULL ||
	       pAy->xs[0] == NULL || pAy->xs[1] == NULL ||
			*(pAx->xs[0]) <= 1 || *(pAx->xs[1]) <= 1 ||
			*(pAy->xs[0]) <= 1 || *(pAy->xs[1]) <= 1)  )
	  {
	   err_warn(1,fperr,
		"Error - VECTOR requires 2 multi-valued dimensions.\n");
	   pD->off=2;
	   return 0;
	  }

 /*     generic_world coordinate values */
        X1= *pAx->xf[0];
        Y1= *pAx->xf[1];
        X2= *pAx->xl[0];
        Y2= *pAx->xl[1];








/*			Check NDC space is greater than zero.		*/

	if (pP->dsp.x2-pP->dsp.x1 < .001 || pP->dsp.y2-pP->dsp.y1 < .001)
	  {
	   err_warn(1,fperr,
			"Error - NDC display space is nil for VECTOR.\n");
	   pD->dsp_seg[1]=0;
	   pD->off=2;
	   return 0;
	  }

	if (erret == 0 && (
		(pAx->xs[2] != NULL && *(pAx->xs[2]) > 1) ||
		(pAy->xs[2] != NULL && *(pAy->xs[2]) > 1) ||
	        (pAx->xs[3] != NULL && *(pAx->xs[3]) > 1) ||
	        (pAy->xs[3] != NULL && *(pAy->xs[3]) > 1) )  )
	  {
	   if (pAx->f != NULL || pAy->f != NULL)
	     {
	      if (pAx->f != NULL)
		   err_warn(0,fperr,
	            "Warning - VECTOR is 2D. Taking a subset of (%dD) computed "
		    "variable.\n"
		    "        Modify dimensions of rhs: %s=%s\n",
				NDx,ptabx->name,pAx->f);
	      if (pAy->f != NULL)
		   err_warn(0,fperr,
	            "Warning - VECTOR is 2D. Taking subset of (%dD) computed "
		    "variable.\n"
		    "        Modify dimensions of rhs: %s=%s\n",
				NDy,ptaby->name,pAy->f);

/*	      erret++;*/
	     }
	   else
	     {
	      err_warn(0,fperr,
			"Warning - VECTOR is 2D; (%s) is %dD.\n"
		"          Used only first values of excess dimensions.\n",
				ptabx->name,NDx);
	      err_warn(0,fperr,
			"Warning - VECTOR is 2D; (%s) is %dD.\n"
		"          Used only first values of excess dimensions.\n",
				ptaby->name,NDy);
	      for (i=2;i<pAx->ND;i++)
		{
		 *pAx->xs[i]=1;
		 *pAx->xf[i]=*pAx->XF[i];
		 *pAx->xl[i]=*pAx->XF[i];
		}
	      for (i=2;i<pAy->ND;i++)
		{
		 *pAy->xs[i]=1;
		 *pAy->xf[i]=*pAy->XF[i];
		 *pAy->xl[i]=*pAy->XF[i];
		}
	     }
	  }

	if ( *pAy->xs[0] != *pAx->xs[0] || *pAy->xs[1] != *pAx->xs[1] ||
		*pAy->xf[0] != *pAx->xf[0] || *pAy->xf[1] != *pAx->xf[1] ||
		*pAy->xl[0] != *pAx->xl[0] || *pAy->xl[1] != *pAx->xl[1] )
	  {
	   err_warn(1,fperr,
		"Error - VECTOR components must be on the same grid.\n");
	   return erret++;
	  }

/*                              Transform the x_x coordinate axis.         */
           if ( ((strcmp("linear",pGv->xat) != 0) &&
                 (strcmp("",pGv->xat) != 0)) &&
                 (strcmp("linear",pGv->proj) == 0) ) {
              if ((savex_xv_0=(float *)malloc(pAx->xs[0][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(savex_xv_0,pAx->xv[0],pAx->xs[0][0],
                            &pAx->xf[0][0], &pAx->xl[0][0], pGv->xat,
                            &did_savex_x, "x") == 0) {
                       if (did_savex_x) {
                          for (l = 0; l < pAx->xs[0][0]; ++l)
                             pAx->xv[0][l] = savex_xv_0[l];
                          *pAx->xf[0] = savex_xf[0];
                          *pAx->xl[0] = savex_xl[0];
                          free((char *) savex_xv_0);
                       }
                       return 0;
                    } else {
                      for (l = 0; l < pAx->xs[0][0]; ++l)
                          pAy->xv[0][l] = pAx->xv[0][l];
                      *pAy->xf[0] = *pAx->xf[0];
                      *pAy->xl[0] = *pAx->xl[0];
	            }
                 }
           }

/*                              Transform the x_y coordinate axis.         */
           if ( ((strcmp("linear",pGv->yat) != 0) &&
                 (strcmp("",pGv->yat) != 0)) &&
                 (strcmp("linear",pGv->proj) == 0) ) {
              if ((savex_xv_1=(float *)malloc(pAx->xs[1][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(savex_xv_1,pAx->xv[1],pAx->xs[1][0],
                            &pAx->xf[1][0], &pAx->xl[1][0], pGv->yat,
                            &did_savex_y, "y") == 0) {
                       if (did_savex_x) {
                          for (l = 0; l < pAx->xs[0][0]; ++l)
                             pAx->xv[0][l] = pAy->xv[0][l] = savex_xv_0[l];
                          *pAx->xf[0] = *pAy->xf[0] = savex_xf[0];
                          *pAx->xl[0] = *pAy->xl[0] = savex_xl[0];
                          free((char *) savex_xv_0);
                       }
   
                       if (did_savex_y) {
                          for (l = 0; l < pAx->xs[1][0]; ++l)
                             pAx->xv[1][l] = savex_xv_1[l];
                          *pAx->xf[1] = savex_xf[1];
                          *pAx->xl[1] = savex_xl[1];
                          free((char *) savex_xv_1);
                       }
                       return 0;
                    } else {
                      for (l = 0; l < pAx->xs[1][0]; ++l)
                             pAy->xv[1][l] = pAx->xv[1][l];
                      *pAy->xf[1] = *pAx->xf[1];
                      *pAy->xl[1] = *pAx->xl[1];
		    }
                 }
           }

/*			First and last of first two dimensions determines
			direction of the plot.				*/

	dsp[0]=*pAx->xf[0];
	dsp[1]=*pAx->xf[1];
	dsp[2]=*pAx->xl[0];
	dsp[3]=*pAx->xl[1];

/*			Set up the projection for this picture.		*/
	strcpy(proj,pGv->proj);
	if (cmpncs(proj,"Mollweide")==0 || cmpncs(proj,"Robinson")==0 ||
	       cmpncs(proj,"Polar"    )==0 )
	  {
	   if (cmpncs(pAx->XN[0],"longitude") != 0 ||
	          cmpncs(pAx->XN[1],"latitude" ) != 0 )
	     {
	      err_warn(1,fperr,
		      "Error - (VECTORS) spherical projections work"
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

	if (set_projection(proj,pP->dsp,pGv->dsp,dsp) == 0)
	  {
	   err_warn(1,fperr,"Error - in projection for VECTORS.\n");
	   erret++;
	  }
	     
/*		If data range is outside (or on) the display (user coordinate)
		range limits and both are on the same side,
		there is nothing that will display.			*/

	if (   ((pj->X1-*pAx->xf[0])*(pj->X2-*pAx->xf[0]) > 0.0 &&
		(pj->X1-*pAx->xl[0])*(pj->X2-*pAx->xl[0]) > 0.0 &&
		(pj->X1-*pAx->xl[0])*(pj->X1-*pAx->xf[0]) > 0.0 ) ||
	       ((pj->Y1-*pAx->xf[1])*(pj->Y2-*pAx->xf[1]) > 0.0 &&
		(pj->Y1-*pAx->xl[1])*(pj->Y2-*pAx->xl[1]) > 0.0 &&
		(pj->Y1-*pAx->xl[1])*(pj->Y1-*pAx->xf[1]) > 0.0 ))
	  {
	   err_warn(1,fperr,
		   "Error - VECTOR data is outside the display range.\n");
	   erret++;
	  }







	if (pD->dsp_seg[3] > 0 && (pP->dsp.p > 0 && pP->dsp.x1 < 1.0 &&
	    pP->dsp.y1 < 1.0 && pP->dsp.x1 > 0.0 && pP->dsp.y1 > 0.0 &&
	    pP->dsp.x2 < 1.0 && pP->dsp.y2 < 1.0 && pP->dsp.x2 > 0.0 &&
	    pP->dsp.y2 > 0.0 ) )
	  {

/*              If the variable isn't computed then restore excess dimensions.*/
           if (did_savex_x) {
              for (l = 0; l < pAx->xs[0][0]; ++l)
                 pAx->xv[0][l] = pAy->xv[0][l] = savex_xv_0[l];
              *pAx->xf[0] = *pAy->xf[0] = savex_xf[0];
              *pAx->xl[0] = *pAy->xl[0] = savex_xl[0];
              free((char *) savex_xv_0);
           }
           if (did_savex_y) {
              for (l = 0; l < pAx->xs[1][0]; ++l)
                 pAx->xv[1][l] = pAy->xv[1][l] = savex_xv_1[l];
              *pAx->xf[1] = *pAy->xf[1] = savex_xf[1];
              *pAx->xl[1] = *pAy->xl[1] = savex_xl[1];
              free((char *) savex_xv_1);
           }

#ifdef cray
	   if (erret==0 && acquire_A(ptabx,"R*8")>0 && acquire_A(ptaby,"R*8")>0)
#else
	   if (erret==0 && acquire_A(ptabx,"R*4")>0 && acquire_A(ptaby,"R*4")>0)
#endif
	     {
/*                      Compute the mean if from CDAT or Computed */
           if (pAx->f != NULL)
                 mmmm(pAx->un.data,pAx->mask,&pAx->xs[0],&pAx->xw[0],2,
                        &pAx->XC[0],&pAx->xv[0],&pAx->min,&pAx->max,&pAx->mean);
	   save_min=pAx->min;
	   save_mean=pAx->mean;
	   save_max=pAx->max;
	   
	   /* Save data, replace with vector value and computes mean again.... */
	   
	   size=1;
	   for (l = 0; l < pAx->ND; l++) size *= *pAx->xs[l];
	   if ((save_data=(float *)malloc(size*sizeof(float)))==NULL) {
	     err_warn(1,fperr, "Not enough memory to store plot data.\n");
	     return 0;
	   }
	   if ((save_mask=(short *)malloc(size*sizeof(short)))==NULL) {
	     err_warn(1,fperr, "Not enough memory to store plot data.\n");
	     return 0;
	   }
	   for (l=0; l<size; ++l) 
	     {
	       save_data[l] = pAx->un.data[l];
	       save_mask[l] = pAx->mask[l];
	       if ((pAx->un.data[l]<9.9E19) && pAx->mask[l] && pAy->mask[l])
		 {
		   pAx->un.data[l]=sqrt(pAx->un.data[l]*pAx->un.data[l]+pAy->un.data[l]*pAy->un.data[l]);
		 }
	       else
		 {
		   pAx->mask[l]=0;
		 }
	     }
	   
	   mmmm(pAx->un.data,pAx->mask,&pAx->xs[0],&pAx->xw[0],2,
		&pAx->XC[0],&pAx->xv[0],&pAx->min,&pAx->max,&pAx->mean);
	   /* Put the data back */

	   for (l=0; l<size; ++l) 
	     {
	       pAx->un.data[l] = save_data[l];
	       pAx->mask[l] = save_mask[l];
	     }
	   free((char *) pAx->un.data);
	   free((char *) pAx->mask);
	   pAx->un.data = save_data;
	   pAx->mask = save_mask;
	   
           if (pAy->f != NULL)
                 mmmm(pAy->un.data,pAy->mask,&pAy->xs[0],&pAy->xw[0],2,
                        &pAy->XC[0],&pAy->xv[0],&pAy->min,&pAy->max,&pAy->mean);

/*                              Transform the x_x coordinate axis.         */
           if ( ((strcmp("linear",pGv->xat) != 0) &&
                 (strcmp("",pGv->xat) != 0)) &&
                 (strcmp("linear",pGv->proj) == 0) ) {
              if ((savex_xv_0=(float *)malloc(pAx->xs[0][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");                   return 0;
                 } else {
                   if (erret=trans_axis(savex_xv_0,pAx->xv[0],pAx->xs[0][0],
                            &pAx->xf[0][0], &pAx->xl[0][0], pGv->xat,
                            &did_savex_x, "x") == 0) {
                       if (did_savex_x)
                       {
                          for (l = 0; l < pAx->xs[0][0]; ++l)
                             pAx->xv[0][l] = savex_xv_0[l];
                          *pAx->xf[0] = savex_xf[0];
                          *pAx->xl[0] = savex_xl[0];
                          free((char *) savex_xv_0);
                       }
                       return 0;
                    }  else {
                      for (l = 0; l < pAx->xs[0][0]; ++l)
                          pAy->xv[0][l] = pAx->xv[0][l];
                      *pAy->xf[0] = *pAx->xf[0];
                      *pAy->xl[0] = *pAx->xl[0];
                    }
                 }
           }

/*                              Transform the x_y coordinate axis.         */
           if ( ((strcmp("linear",pGv->yat) != 0) &&
                 (strcmp("",pGv->yat) != 0)) &&
                 (strcmp("linear",pGv->proj) == 0) ) {
              if ((savex_xv_1=(float *)malloc(pAx->xs[1][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");                   return 0;
                 } else { 
                    if (erret=trans_axis(savex_xv_1,pAx->xv[1],pAx->xs[1][0],
                            &pAx->xf[1][0], &pAx->xl[1][0], pGv->yat,
                            &did_savex_y, "y") == 0) {
                       if (did_savex_x) {
                          for (l = 0; l < pAx->xs[0][0]; ++l)
                             pAx->xv[0][l] = pAy->xv[0][l] = savex_xv_0[l];
                          *pAx->xf[0] = *pAy->xf[0] = savex_xf[0];
                          *pAx->xl[0] = *pAy->xl[0] = savex_xl[0];
                          free((char *) savex_xv_0);
                       }
   
                       if (did_savex_y) {
                          for (l = 0; l < pAx->xs[1][0]; ++l)
                             pAx->xv[1][l] = savex_xv_1[l];
                          *pAx->xf[1] = savex_xf[1];
                          *pAx->xl[1] = savex_xl[1];
                          free((char *) savex_xv_1);
                       }
                       return 0;
                    } else {
                      for (l = 0; l < pAx->xs[1][0]; ++l)
                             pAy->xv[1][l] = pAx->xv[1][l];
                      *pAy->xf[1] = *pAx->xf[1];
                      *pAy->xl[1] = *pAx->xl[1];
                    }
                 }
           }

	      if (erret == 0)
		{
		 if (vlen > 0.999e20)
		   {
		    vxm=(fabs(pAx->min)>fabs(pAx->max))?
						fabs(pAx->min):fabs(pAx->max);
		    vym=(fabs(pAy->min)>fabs(pAy->max))?
						fabs(pAy->min):fabs(pAy->max);
		    vlen=mean_veloc(vxm,vym);	      
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
		 savecontinents = Dc.selected;
		 if (pD->continents==-1) pD->continents = Dc.selected;
		 else Dc.selected = pD->continents;
/* DNW - 05/03/01	         if (doexist("longitude",pAx->XN[0]) &&
			doexist("latitude",pAx->XN[1])  && Dc.selected > 0)*/
/* DNW - 08/20/07                if (Dc.selected > 0) I put the above line back to avoid 
                                                      continents drawing when dimensions are not (long,lat)*/
                 if (doexist("longitude",pAx->XN[0]) && doexist("latitude",pAx->XN[1])  && pD->continents > 0)
		   {
                     /*    generic_world coordinate values */
                     if (pD->continents < 3)
                        generic_world(X1,Y1,X2,Y2,Dc.lb,pGv->xat,pGv->yat);
                     else
                        generic_world2(X1,Y1,X2,Y2,Dc.lb,pGv->xat,pGv->yat);
		   }
		 Dc.selected = savecontinents;

	         arrows(*pAx->xs[0],*pAx->xs[1],pAx->un.data,pAy->un.data,
			pAx->mask,pAy->mask,
			pAx->xv[0],pAx->xv[1],pGv->vtype,pGv->vpos,pGv->vsf,
			vlen,pGv->lb);

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
	      if (pAx->F != NULL)
		{
		 err_warn(1,fperr,
			"Error - vector data (A_%s) from file (%s) not"
			  " acquired.\n",ptabx->name,pAx->F);
		 pAx->notok=1;
		}
	      else if (pAx->f != NULL)
		{
		 err_warn(1,fperr,
			"Error - vector data (A_%s) from (%s) cannot be"
			  " computed.\n",ptabx->name,pAx->f);
		 pAx->notok=1;
		}
	      else if (pAy->F != NULL)
		{
		 err_warn(1,fperr,
			"Error - vector data (A_%s) from file (%s) not"
			  " acquired.\n",ptaby->name,pAy->F);
		 pAy->notok=1;
		}
	      else if (pAy->f != NULL)
		{
		 err_warn(1,fperr,
			"Error - vector data (A_%s) from (%s) cannot be"
			  " computed.\n",ptaby->name,pAy->f);
		 pAy->notok=1;
		}
	      else
		err_warn(1,fperr,
		   "Error - with vector component (A_%s) or A_%s.\n",
				ptabx->name,ptaby->name);
	      if (pD->dsp_seg[0] > 0) gdsg(pD->dsp_seg[0]);
	      pD->dsp_seg[0]=0;
	      pD->dsp_seg[1]=0;
	      pD->dsp_seg[2]=1;
	      pD->dsp_seg[3]=0;
	      erret++;
	     }
	   killA_tmp();
          }
	if (erret == 0 && pD->leg_seg[3] > 0)
	  {
	   if (vlen > 0.999e20)
	     {
	      vxm=(fabs(pAx->min)>fabs(pAx->max))?
						fabs(pAx->min):fabs(pAx->max);
	      vym=(fabs(pAy->min)>fabs(pAy->max))?
						fabs(pAy->min):fabs(pAy->max);
	      vlen=mean_veloc(vxm,vym);
	     }
	   gcrsg(pD->leg_seg[0]=++segment_num);
	   gssgp(pD->leg_seg[0],(pP->leg.p+pD->pri)/1000.0);

	   if (legendGv(pGv->vtype,pGv->vpos,pGv->vsf,pGv->lb,vlen,pP,pD) > 0)
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

	pict_elem(pAx,pP,pD,pGv->proj,
		pGv->xtl1,pGv->xtl2,pGv->xmt1,pGv->xmt2,
		pGv->ytl1,pGv->ytl2,pGv->ymt1,pGv->ymt2,
				     pGv->dsp);

	pAx->min=save_min;
	pAx->max=save_max;
	pAx->mean=save_mean;
	/*		If the variable isn't computed then restore excess dimensions.*/
	if (did_savex_x) {
	  for (l = 0; l < pAx->xs[0][0]; ++l)
	    pAx->xv[0][l] = pAy->xv[0][l] = savex_xv_0[l];
	  *pAx->xf[0] = *pAy->xf[0] = savex_xf[0];
	  *pAx->xl[0] = *pAy->xl[0] = savex_xl[0];
	  free((char *) savex_xv_0);
	}
	if (did_savex_y) {
	  for (l = 0; l < pAx->xs[1][0]; ++l)
	    pAx->xv[1][l] = pAy->xv[1][l] = savex_xv_1[l];
	  *pAx->xf[1] = *pAy->xf[1] = savex_xf[1];
	  *pAx->xl[1] = *pAy->xl[1] = savex_xl[1];
	  free((char *) savex_xv_1);
	}
	
	if (!ptabx->FROM_CDAT) { /* DNW - do not remove the data form CDAT */
	  if (pAx->un.data != NULL){free((char *)pAx->un.data);pAx->un.data=NULL;}
	  if (pAx->mask != NULL) {free((char *)pAx->mask); pAx->mask=NULL;}
	}
	if (!ptaby->FROM_CDAT) { /* DNW - do not remove the data form CDAT */
	  if (pAy->un.data != NULL){free((char *)pAy->un.data);pAy->un.data=NULL;}
	  if (pAy->mask != NULL) {free((char *)pAy->mask); pAy->mask=NULL;}
	}
	if (erret == 0) return 1;
	else
	  {
	    pD->off=2;
	    return 0;
	  }
      }

/*		Draw vector arrows.					*/

    int arrows (im,jm,u,v,mx,my,xv,yv,type,vpos,vsf,vlen,lb)

 	int im,jm;
	float *u,*v;
	short *mx,*my;
	float *xv;
	float *yv;
	int type;
	int vpos;
	float vsf;
	float vlen;
	char *lb;

/*      int im,jm;	Limits of the array dimension sizes.		*/
/*      float u[im,jm];	X component of vectors.				*/
/*      float v[im,jm];	Y component of vectors.				*/
/*	short mx;	mask for x velocity component.			*/
/*	short my;	mask for y velocity component.			*/
/*      float xv;	Values of x nodes.				*/
/*      float yv;	Values of y nodes.				*/
/*      int type;	Type of vector.					*/
/*	int vpos;	Vector attached at: 'h' head, 'c' center, 't' tail.*/
/*	float vsf;	Vector length scale factor.			*/
/*	float vlen;	Reference vector length for a 0.02NDC vector.	*/
/*	float lwsf;	Line width scale factor.			*/
/*	int color;	Color index for the vector.			*/

      {
	int i,j,k,n,m,ij,ji;
	int i5,i10,i50;
	int do_solid=0;
	float dx,rx,ry;
	char label[20];
	Gpoint pxy[100];
	Gpoint prel[100];
	float sv,cv,squv;
	struct table_line *pl;
	struct project_attr *pj;

	pj=&p_PRJ;


	pl=&Tl_tab;
	strncpy(label,lb,17); label[16] = '\0';
	if (strlen(label) <= (size_t) 0) {
           strncpy(label,"default",17); label[16] = '\0';
	}
	for (;pl!=NULL && strcmp(label,pl->name)!=0;pl=pl->next);
	if (pl == NULL)
	  {
	   err_warn(1,fperr,"Error - line attributes problem.\n");
	   return 0;
	  }

        if (pl->ltyp == NULL)
	   gsln(1);
        else
	   gsln(pl->ltyp[0]);
        if (pl->lwsf == NULL)
	   gslwsc(1.0);
        else
	   gslwsc(pl->lwsf[0]);
        if (pl->lci == NULL) {
	  gsplci(241);
 	  gsfaci(241);
	}
       else {
	  gsplci(pl->lci[0]);
	  gsfaci(pl->lci[0]);
	}

        gsfais(GSOLID);

	if (type > 4 || type < 1) type=2;

	switch (type)
	  {
	   case 1:			/* Type = 1	wind barbs.	*/
		prel[0].x=0.01;
		prel[0].y=0.0;
		prel[1].x=-0.01;
		prel[1].y=0.0;
		n=2;
		break;
	   case 2:			/* Type = 2 	hollow arrow head. */
		prel[0].x=0.005;
		prel[0].y=-0.0035;
		prel[1].x=0.01;
		prel[1].y=0.0;
		prel[2].x=0.005;
		prel[2].y=0.0035;
		prel[3].x=0.01;
		prel[3].y=0.0;
		prel[4].x=-0.01;
		prel[4].y=0.0;
		n=5;
                do_solid=0;
		break;
	   case 3:			/* Type=3	solid arrow tail.   */
		prel[0].x=0.005;
		prel[0].y=-0.0035;
		prel[1].x=0.01;
		prel[1].y=0.0;
		prel[2].x=0.005;
		prel[2].y=0.0035;
		prel[3].x=0.01;
		prel[3].y=0.0;
		prel[4].x=-0.01;
		prel[4].y=0.0;
		n=5;
                do_solid=1;
		break;
	   case 4:			/* Type=4	head and tail.	*/
		prel[0].x=0.01;
		prel[0].y=0.0;
		prel[1].x=0.005;
		prel[1].y=-0.0035;
		prel[2].x=0.005;
		prel[2].y=0.0035;
		prel[3].x=0.01;
		prel[3].y=0.0;
		prel[4].x=-0.005;
		prel[4].y=0.0;
		prel[5].x=-0.01;
		prel[5].y=-0.0035;
		prel[6].x=-0.01;
		prel[6].y=0.0;
		prel[7].x=-0.01;
		prel[7].y=0.0035;
		prel[8].x=-0.005;
		prel[8].y=0.0;
		n=9;
		break;
	   default:
		break;
	  }
/*		Position the arrow.  Assume centered at the point
		unless head or tail are requested.			*/

	dx=0.0;
	if (vpos == 't') dx = 0.01;
	else if (vpos == 'h') dx = -0.01;
	   for (m=0;m<n;m++)
	     {
	      prel[m].x=(prel[m].x+dx)*vsf;
	      prel[m].y=prel[m].y*vsf;
	     }
	rx=(pj->X2-pj->X1)/(pj->x2_NDC-pj->x1_NDC);
	ry=(pj->Y2-pj->Y1)/(pj->y2_NDC-pj->y1_NDC);

/*			Draw each vector.				*/

	for (j=0;j<jm;j++)
	  {
	   ji=j*im;
	   for (i=0;i<im;i++)
	     {
	      ij=i+ji;
	      if (mx[ij] & my[ij])
		{
	         sv=v[ij];
	         cv=u[ij];
/*			Barbs are drawn if type > 4 or <= 1.		*/

		 if (type > 4 || type <= 1)
		   {
		    prel[0].x=(0.01+dx)*vsf;
		    prel[0].y=0.0;
		    prel[1].x=(-0.01+dx)*vsf;
		    prel[1].y=0.0;
		    n=2;
		    squv=sqrt(u[ij]*u[ij]+v[ij]*v[ij]);
		    sv=sv/squv;
		    cv=cv/squv;
		    i50=(squv+2.5)/50.0;
		    i10=(squv-i50*50.0+2.5)/10.0;
		    i5=(squv-i50*50.0-i10*10.0+2.5)/5.0;
		    if (i50 > 10)
		      {
		       prel[n].x=-0.01*vsf;
		       prel[n++].y=0.008*vsf;
		       prel[n].x=0.01*vsf;
		       prel[n++].y=0.008*vsf;
		       prel[n].x=0.01*vsf;
		       prel[n++].y=0.0;
		       for (m=0; m<n; m++)
			 {
			  pxy[m].x=xv[i]+rx*(cv*prel[m].x-sv*prel[m].y);
			  pxy[m].y=yv[j]+ry*(sv*prel[m].x+cv*prel[m].y);	
			 }
		       proj_pl(n,pxy);
		       continue;
		      }
		    if (squv < 0.5)
		      {
		        n=0;
			prel[n].x=-0.002*vsf;
			prel[n++].y=0.002*vsf;
			prel[n].x=0.002*vsf;
			prel[n++].y=0.002*vsf;
			prel[n].x=0.002*vsf;
			prel[n++].y=-0.002*vsf;
			prel[n].x=-0.002*vsf;
			prel[n++].y=-0.002*vsf;
			prel[n].x=-0.002*vsf;
			prel[n++].y=0.002*vsf;
		       for (m=0; m<n; m++)
			 {
			  pxy[m].x=xv[i]+rx*prel[m].x;
			  pxy[m].y=yv[j]+ry*prel[m].y;	
			 }
			proj_pl(n,pxy);
			continue;
		      }
		    
		    for (m=0;m<i50;m++)
		      {
		       prel[n].x=(-0.01+m*0.0015+dx)*vsf;
		       prel[n++].y=0.0;
		       prel[n].x=(-0.01+m*0.0015+0.00075+dx)*vsf;
		       prel[n++].y=0.008;
		       prel[n].x=(-0.01+(m+1)*0.0015+dx)*vsf;
		       prel[n++].y=0.0;
		      }
		    for (k=0;k<i10;k++)
		      {
		       prel[n].x=(-0.01+(k+m)*0.0015+dx)*vsf;
		       prel[n++].y=0.008;
		       prel[n].x=(-0.01+(k+m)*0.0015+dx)*vsf;
		       prel[n++].y=0.0;
		       prel[n].x=(-0.01+(k+m+1)*0.0015+dx)*vsf;
		       prel[n++].y=0.0;
		      }
		    if (i5 > 0)
		      {
		       prel[n].x=(-0.01+(k+m)*0.0015-0.00075+dx)*vsf;
		       prel[n++].y=0.004;
		      }
		   }
		 else
		   {
		    sv=sv/vlen;
		    cv=cv/vlen;
		   }
	         for (m=0; m<n; m++)
		   {
		    pxy[m].x=xv[i]+rx*(cv*prel[m].x-sv*prel[m].y);
		    pxy[m].y=yv[j]+ry*(sv*prel[m].x+cv*prel[m].y);	
		   }
	         proj_pl(n,pxy);
                 if (do_solid == 1)
                    proj_gfa(3,pxy);
		}
	     }
	  }
	return 1;
      }


/*			Display the legend for vectors.			*/


      int legendGv(int type,int pos,float vsf,char *lb,float vlen,
				struct p_tab *pP,struct display_tab *pD)

	{
	 int n,k,m;
	 int do_solid=0;
	 float dr,del,v;
	 char str[256],label[17];
	 struct pe_leg pe;
	 Gpoint pxy[10];
	 Gpoint prel[10];
	 int ity;
	 Gpoint vxy;
	
	 struct table_text *pTt;
	 struct table_chorn *pTo;
	 struct table_line *pTl;
	 Gtxalign pta;
	 Gtxpath ptp;

	 if (pP == NULL || pD == NULL)
	   {
	    err_warn(1,fperr,
			"Error - no template or no display, so no legend.\n");
	    return 0;
	   }

	 if (fabs(vlen) > 0.99e20)
	   {
	    err_warn(1,fperr,"Error - reference vector length too large."
		"  Probably no data.\n");
	    return 0;
	   }

	 pe=pP->leg;
	 if (pe.x1 <= 0.0 || pe.x1 >= 1.0 || pe.x2 <= 0.0 || pe.x2 >= 1.0 ||
	     pe.y1 <= 0.0 || pe.y1 >= 1.0 || pe.y2 <= 0.0 || pe.y2 >= 1.0 ||
	     pe.p <= 0)
	    return 1;
/*			No need for a legend if barbs indicate magnitude.*/

	 /*if (type <= 1 || type > 4) return 1;*/
	 if (type == 1) return 1;

	 for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
	    if (strcmp(pTt->name,pe.tb) == 0) break;
	 if (pTt == NULL) pTt=&Tt_tab;
	 for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
	    if (strcmp(pTo->name,pe.to) == 0) break;
	 if (pTo == NULL) pTo=&To_tab;

	 for (pTl=&Tl_tab; pTl != NULL; pTl=pTl->next)
	    if (strcmp(pTl->name,lb) == 0) break;

	 set_text_attr(pTt,pTo);

/*			Now take care of special case for legends:
			up-vector, alignment, path.			*/

	 v=pTo->chua;
	 while (v > 360.0) v=v-360.0;
	 while (v < 0.0) v=v+360.0;
	 k=(v+45.0)/90.0;
	 v=k*90.0;
	   vxy.x=sin((3.1415926536/180.0)*v);
	   vxy.y=cos((3.1415926536/180.0)*v);
	 gschup(&vxy);
	   pta.hor=GTH_CENTRE;
	   pta.ver=GTV_HALF;
	 gstxal(&pta);
	   ptp=GTP_RIGHT;
	 gstxp(ptp);



	if (pTl == NULL) pTl=&Tl_tab;
        if (pTl->ltyp == NULL)
	   gsln(1);
        else
	   gsln(pTl->ltyp[0]);
        if (pTl->lwsf == NULL)
	   gslwsc(1.0);
        else
	   gslwsc(pTl->lwsf[0]);
        if (pTl->lci == NULL)
	   gsplci(241);
        else
	   gsplci(pTl->lci[0]);

	del=vlen;
	dr=vsf;

	ity=type;
	if (ity > 4 || ity <= 0) ity=2;

	switch (ity)
	  {

	   case 1:			/* Type = 1	wind barbs.	*/
		prel[0].x=0.01;
		prel[0].y=0.0;
		prel[1].x=-0.01;
		prel[1].y=0.0;
		n=2;
		break;
	   case 2:			/* Type = 2 	hollow arrow head.   */
		prel[0].x=0.005;
		prel[0].y=-0.0035;
		prel[1].x=0.01;
		prel[1].y=0.0;
		prel[2].x=0.005;
		prel[2].y=0.0035;
		prel[3].x=0.01;
		prel[3].y=0.0;
		prel[4].x=-0.01;
		prel[4].y=0.0;
		n=5;
                do_solid=0;
		break;
	   case 3:			/* Type=3	solid arrow tail.   */
		prel[0].x=0.005;
		prel[0].y=-0.0035;
		prel[1].x=0.01;
		prel[1].y=0.0;
		prel[2].x=0.005;
		prel[2].y=0.0035;
		prel[3].x=0.01;
		prel[3].y=0.0;
		prel[4].x=-0.01;
		prel[4].y=0.0;
		n=5;
                do_solid=1;
		break;
	   case 4:			/* Type=4	head and tail.	*/
		prel[0].x=0.01;
		prel[0].y=0.0;
		prel[1].x=0.005;
		prel[1].y=-0.0035;
		prel[2].x=0.005;
		prel[2].y=0.0035;
		prel[3].x=0.01;
		prel[3].y=0.0;
		prel[4].x=-0.005;
		prel[4].y=0.0;
		prel[5].x=-0.01;
		prel[5].y=-0.0035;
		prel[6].x=-0.01;
		prel[6].y=0.0;
		prel[7].x=-0.01;
		prel[7].y=0.0035;
		prel[8].x=-0.005;
		prel[8].y=0.0;
		n=9;
		break;
	   default:
		break;
	  }

/*			Adjust the arrow to the point.			*/

	 for (m=0;m<n;m++)
	   {
	    prel[m].x=prel[m].x;
	    pxy[m].x=((pe.x1>pe.x2)?pe.x2:pe.x1)+dr*prel[m].x;
	    pxy[m].y=((pe.y1>pe.y2)?pe.y1:pe.y2)+dr*prel[m].y;
	   }
	 gpl(n,pxy);
         if (do_solid == 1)
             gfa(3,pxy);

	 pxy[0].x=((pe.x1>pe.x2)?pe.x2:pe.x1);
	 pxy[0].y=((pe.y1>pe.y2)?pe.y2:pe.y1);
	 sprintf(str,"%g",del);
	 gtx (pxy,(unsigned char *)str);
	 return 1;
	}
