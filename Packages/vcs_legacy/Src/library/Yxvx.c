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

/*		Draw Y(x) vs x as specified.				*/

    int Yxvx(
	      struct display_tab *pD,
	      struct gYx_attr *pGYxvx,
	      struct p_tab *pP,
	      struct a_tab *ptab
	     )

      {
	int i,k,erret,pw10;
	float dsp[NDS],dr,dy,y1,y2,center;
	struct a_attr *pA;
	struct project_attr *pj;
	Glimit pvp;

	int ND;
	int save_xs[NDS];
	float save_xl[NDS],save_xf[NDS];

	int l;
 	float *save_xv_0;
  	int did_save_x=0;
	extern int trans_axis();

	char *hold_y_name=NULL;

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
			"Error - NDC display space is nil for Y(x) vx x.\n");
	   pD->dsp_seg[1]=0;
	   pD->off=2;
	   return 0;
	  }

/*			Check the number of dimensions and number of
			values of the variable's dimensions.		*/

	if (erret == 0 && (pA->xs[0] == NULL || *(pA->xs[0]) <= 1 ))
	  {
	   err_warn(1,fperr,
		"Error - Y(x) vx x requires 1 multi-valued dimension.\n");
	   pD->off=2;
	   return 0;
	  }

	if (erret == 0 && ((pA->xs[1] != NULL && *(pA->xs[1]) > 1) ||
			      (pA->xs[2] != NULL && *(pA->xs[2]) > 1) ||
			      (pA->xs[3] != NULL && *(pA->xs[3]) > 1)  )  )

/* DNW - 10/8/02 No need to print out this error message
	  {
	   if (pA->f != NULL)
	     {
	      err_warn(0,fperr,
	            "Warning - Yxvsx is 1D. Taking a subset of (%dD) computed "
		    "variable.\n"
		    "        Modify dimensions of rhs: %s=%s\n",
				ND,ptab->name,pA->f);
*	      erret++;*
	     }
	   else
*/
	     {
	      err_warn(0,fperr,
			"Warning - Yxvsx is 1D; (%s) is %dD.\n"
		"          Used only first values of excess dimensions.\n",
				ptab->name,ND);
	      for (i=1;i<pA->ND;i++)
	        {
		 *pA->xs[i]=1;
		 *pA->xf[i]=*pA->XF[i];
		 *pA->xl[i]=*pA->XF[i];
		}
	     }
/*	  }*/


#ifdef cray
	if (erret==0 && acquire_A(ptab,"R*8") > 0 )
#else
	if (erret==0 && acquire_A(ptab,"R*4") > 0 )
#endif
	  {
/*                      Compute the mean if from CDAT or Computed */
           if (pA->f != NULL)
                 mmmm(pA->un.data,pA->mask,&pA->xs[0],&pA->xw[0],1,
                        &pA->XC[0],&pA->xv[0],&pA->min,&pA->max,&pA->mean);

/*                              Transform the x coordinate axis.         */

           if ( ((strcmp("linear",pGYxvx->xat) != 0) &&
                 (strcmp("",pGYxvx->xat) != 0)) &&
                 (strcmp("linear",pGYxvx->proj) == 0) ) {
              if ((save_xv_0=(float *)malloc(pA->xs[0][0]*sizeof(float)))==NULL)
                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");
                   return 0;
                 } else {
                    if (erret=trans_axis(save_xv_0,pA->xv[0],pA->xs[0][0],
                            &pA->xf[0][0], &pA->xl[0][0], pGYxvx->xat,
                            &did_save_x, "x") == 0) {
                      if (did_save_x) {
                         for (l = 0; l < pA->xs[0][0]; ++l)
                            pA->xv[0][l] = save_xv_0[l];
                         *pA->xf[0] = save_xf[0];
                         *pA->xl[0] = save_xl[0];
                         free((char *) save_xv_0);
                      }
                      return 0;
                   }
                 }
           } else
             strcpy(pGYxvx->xat, "linear");

/*			Get a default for the X axis.			*/

	   if (!nicedf(pA->min,pA->max,&dr,&pw10,&center) )
	     {
	      dr=1.0;
	      pw10=0.0;
	      center=pA->min;
	     }
	   dy=dr*pow( (double) 10.0, (double) pw10);
	   k=(pA->min-center)/dy;
	   if (pA->min < center) k--;
	   y1=center+k*dy;
	   k=(pA->max-center)/dy;
	   if (pA->max > center) k++;
	   y2=center+k*dy;
		 
/* 			First and last of first dimension determines */
/* 			direction of the plot.				* */

/*  DNW - 10/8/02 - This allows only the Y-axis to change. Now with this removed the user */
/* 		 can modify the X-axis as well. */
/* C.D. 7/18/03 Further modify to allow setting both x and y and autoscale */
	   if (pGYxvx->dsp[0]<9.9e19) 
	     {
	       dsp[0]=pGYxvx->dsp[0];
	     } 
	   else
	     {
/* 	       dsp[0]=pGYxvx->dsp[0]=*pA->xf[0]; */
	       dsp[0]=*pA->xf[0];
	     }
	   dsp[1]=y1;
	   if (pGYxvx->dsp[2]<9.9e19) 
	     {
	       dsp[2]=pGYxvx->dsp[2];
	     }
	   else
	     {
/* 	       dsp[2]=pGYxvx->dsp[2]=*pA->xl[0]; */
	       dsp[2]=*pA->xl[0];
	     }
	   dsp[3]=y2;

	   /* Remember value used for click capabilities */
	   pD->dsp_used[0]=dsp[0];
	   pD->dsp_used[1]=dsp[1];
	   pD->dsp_used[2]=dsp[2];
	   pD->dsp_used[3]=dsp[3];

/*			Set up the projection for this picture.		*/

	   if (set_projection(pGYxvx->proj,pP->dsp,pGYxvx->dsp,dsp) == 0)
	     {
	      err_warn(1,fperr,"Error - in projection for Y(x) vs x.\n");
	      erret++;
	     }
	     
/*		If data range is outside (or on) the display (user coordinate)
		range limits and both are on the same side,
		there is nothing that will display.			*/

	   if (   ((pj->Y1-pA->min)*(pj->Y2-pA->min) > 0.0 &&
		   (pj->Y1-pA->max)*(pj->Y2-pA->max) > 0.0 &&
		   (pj->Y1-pA->max)*(pj->Y1-pA->min) > 0.0 ) 
		  ||
	 	  ((pj->X1-*pA->xf[0])*(pj->X2-*pA->xf[0]) > 0.0 &&
		   (pj->X1-*pA->xl[0])*(pj->X2-*pA->xl[0]) > 0.0 &&
		   (pj->X1-*pA->xl[0])*(pj->X1-*pA->xf[0]) > 0.0)
		  )
	     {
/* 	       printf("pj-X1 %f\n",pj->X1); */
/* 	       printf("pj-Y1 %f\n",pj->Y1); */
/* 	       printf("pj-X2 %f\n",pj->X2); */
/* 	       printf("pj-Y2 %f\n",pj->Y2); */
/* 	       printf("pA->min %f\n",pA->min); */
/* 	       printf("pA->max %f\n",pA->max); */
/* 	       printf("pA->xf[0] %f\n",pA->xf[0]); */
/* 	       printf("pA->xl[0] %f\n",pA->xl[0]); */
	       err_warn(1,fperr,
			"Error - Y(x) data is outside the display range.\n");
	       erret++;
	     }

	   if (erret == 0 && pD->dsp_seg[3] > 0 && (pP->dsp.p > 0 &&
		 pP->dsp.x1 < 1.0 && pP->dsp.y1 < 1.0 && pP->dsp.x1 > 0.0 &&
		 pP->dsp.y1 > 0.0 && pP->dsp.x2 < 1.0 && pP->dsp.y2 < 1.0 &&
		 pP->dsp.x2 > 0.0 && pP->dsp.y2 > 0.0 ) )
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

	      plot_line(*pA->xs[0],pA->xv[0],NULL,pA->un.data,pA->mask,
						pGYxvx->lb,pGYxvx->mb);

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
	   if (pA->F != NULL)
	     {	
	      err_warn(1,fperr,
			"Error - Y(x) data (A_%s) from file (%s) not"
			  " acquired.\n",ptab->name,pA->F);
	      pA->notok=1;
	     }
	   else if (pA->f != NULL)
	     {
	      err_warn(1,fperr,
			"Error - Y(x) data (A_%s) from (%s) cannot be"
			  " computed.\n",ptab->name,pA->f);
	      pA->notok=1;
	     }
	   else
		err_warn(1,fperr,"Error - with Y(x) component (A_%s).\n",
								ptab->name);
	   if (pD->dsp_seg[0] > 0) gdsg(pD->dsp_seg[0]);
	   pD->dsp_seg[0]=0;
	   pD->dsp_seg[1]=0;
	   pD->dsp_seg[2]=1;
	   pD->dsp_seg[3]=0;
	   erret++;
	  }
	killA_tmp();

	if (erret == 0 && pD->leg_seg[3] > 0)
	  {
	   gcrsg(pD->leg_seg[0]=++segment_num);
	   gssgp(pD->leg_seg[0],(pP->leg.p+pD->pri)/1000.0);

	   if (legendGYxvx(pGYxvx->lb,pGYxvx->mb,pA->n,pP,pD) > 0)
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

	if (erret == 0) {
          if (pA->xn[1] != NULL) {
           if ((hold_y_name = (char *) malloc(strlen(pA->xn[1])+1)) == NULL)
                err_warn(1,fperr, "Error - no memory for labelling plot.\n");
           strncpy(hold_y_name,pA->xn[1],strlen(pA->xn[1])); hold_y_name[strlen(pA->xn[1])] = '\0';
           free((char *) pA->xn[1]);
	   free((int *) pA->xs[1]);
	   pA->xn[1]=NULL;
	  }
	  else 
	    {
           if ((hold_y_name = (char *) malloc(1)) == NULL)
                err_warn(1,fperr, "Error - no memory for labelling plot.\n");
	      strcpy(hold_y_name,"\0");
	    }
           if ((pA->xn[1] = (char *) malloc(strlen(pA->n)+1)) == NULL)
                err_warn(1,fperr, "Error - no memory for labelling plot.\n");
           strncpy(pA->xn[1],pA->n,strlen(pA->n)); pA->xn[1][strlen(pA->n)] = '\0';
	   if ((pA->xs[1] = (int *) malloc(sizeof(int))) == NULL)
                err_warn(1,fperr, "Error - no memory for labelling plot (int).\n");
           *pA->xs[1]=2;
          

	   pict_elem(pA,pP,pD,pGYxvx->proj,
	   pGYxvx->xtl1,pGYxvx->xtl2,pGYxvx->xmt1,pGYxvx->xmt2,
	   pGYxvx->ytl1,pGYxvx->ytl2,pGYxvx->ymt1,pGYxvx->ymt2,
				     pGYxvx->dsp);

          if (pA->xn[1] != NULL) {
           free((char *) pA->xn[1]);
           if ((pA->xn[1] = (char *) malloc(strlen(hold_y_name)+1)) == NULL)
                err_warn(1,fperr, "Error - no memory for labelling plot.\n");
           strncpy(pA->xn[1],hold_y_name,strlen(hold_y_name));pA->xn[1][strlen(hold_y_name)]='\0';
          }
        }

/*		If the variable isn't computed then restore excess dimensions.*/

	if (did_save_x)
        {
           for (l = 0; l < pA->xs[0][0]; ++l)
              pA->xv[0][l] = save_xv_0[l];
	   /* Commented out by C. Doutriaux on Apr 4th, 2005 made the canvas disapear */
	   /* when going back and forth between log10 and linear axis */
/* 	   *pA->xf[0] = pGYxvx->dsp[0] = save_xf[0]; */
/*            *pA->xl[0] = pGYxvx->dsp[2] = save_xl[0]; */
	   *pA->xf[0]  = save_xf[0];
           *pA->xl[0]  = save_xl[0];
	   free((char *) save_xv_0);
	}

	if (pA->f == NULL)
{

	   for (i=1;i<pA->ND;i++)
	     {
	      *pA->xs[i]=save_xs[i];
	      *pA->xf[i]=save_xf[i];
	      *pA->xl[i]=save_xl[i];
	     }
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


/*			Display the legend for Yxvx.			*/


    int legendGYxvx(
		    char *lb,
		    char *mb,
		    char *name,
		    struct p_tab *pP,
		    struct display_tab *pD
		   )

      {
       extern struct vcs_legacy_marker Vma_tab;
       Gpoint pxy[2];
	
       struct pe_leg pe;
       struct table_text *pTt;
       struct table_chorn *pTo;
       struct table_line *pTl;
       struct table_mark *pTm;
       Gtxalign pta;
       Gtxpath ptp;

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

       pTl=NULL;
       if (lb != NULL && lb[0] != '\0')
	  for (pTl=&Tl_tab; pTl != NULL; pTl=pTl->next)
	     if (strcmp(pTl->name,lb) == 0) break;
       pTm=NULL;
       if (mb != NULL && mb[0] != '\0')
	  for (pTm=&Tm_tab; pTm != NULL; pTm=pTm->next)
	     if (strcmp(pTm->name,mb) == 0) break;

       if (pTl != NULL)
	 {
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
	  pxy[0].x=pe.x1;
	  pxy[0].y=pe.y1;
	  pxy[1].x=pe.x2;
	  pxy[1].y=pe.y1;
	  gpl(2,pxy);
	 }
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

       if (name != NULL)
	 {
	  pxy[0].x=pe.x2+.01;
	  pxy[0].y=pe.y2;
	  gtx (pxy,(unsigned char *)name);
	 }
       return 1;
      }
