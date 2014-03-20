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

    extern struct vcs_legacy_marker Vma_tab;

    extern struct project_attr p_PRJ;

    extern char PRJ_names[PRJ_TYPES][17];

    extern int segment_num;

/*			Proto-types					*/

    int nicedf(float a,float b,float *dr,int *pw10,float *center);

/*		Draw X(y) vs y as specified.				*/

    int Xyvy(
	      struct display_tab *pD,
	      struct gXy_attr *pGXyvy,
	      struct p_tab *pP,
	      struct a_tab *ptab
	     )

      {
	int i,k,erret,pw10;
	float dsp[NDS],dr,dx,x1,x2,center;
	struct a_attr *pA;
	struct project_attr *pj;
	Glimit pvp;

	int ND;
	int save_xs[NDS];
	int did_xs=0;
	float save_xl[NDS],save_xf[NDS];

	int l;
	float *save_xv_0;
	int did_save_y=0;
        extern int trans_axis();

	char *hold_x_name=NULL, *hold_y_name=NULL;

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
			"Error - NDC display space is nil for X(y) vs y.\n");
	   pD->dsp_seg[1]=0;
	   pD->off=2;
	   return 0;
	  }

/*			Check the number of dimensions and number of
			values of the variable's dimensions.		*/

	if (erret == 0 && (pA->xs[0] == NULL || *(pA->xs[0]) <= 1 ))
	  {
	   err_warn(1,fperr,
		"Error - X(y) vs y requires 1 multi-valued dimension.\n");
	   pD->off=2;
	   return 0;
	  }

	if (erret == 0 && ((pA->xs[1] != NULL && *(pA->xs[1]) > 1) ||
			      (pA->xs[2] != NULL && *(pA->xs[2]) > 1) ||
			      (pA->xs[3] != NULL && *(pA->xs[3]) > 1) ) )

/* DNW - 10/8/02 No need to print out this error message
	  {
	   if (pA->f != NULL)
	     {	
	      err_warn(0,fperr,
	            "Warning - Xyvsy is 1D. Taking a subset of (%dD) computed "
		    "variable.\n"
		    "        Modify dimensions of rhs: %s=%s\n",
				ND,ptab->name,pA->f);
*	      erret++;*
	     }
	   else */
	     {
	      err_warn(0,fperr,
			"Warning - Xyvsy is 1D; (%s) is %dD.\n"
		"          Used only first values of excess dimensions.\n",
				ptab->name,ND);
	      for (i=1;i<pA->ND;i++)
		{
		 *pA->xs[i]=1;
		 *pA->xf[i]=*pA->XF[i];
		 *pA->xl[i]=*pA->XF[i];
		}
	     }
/*	  } */


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

/*                              Transform the y coordinate axis.         */

           if ( ((strcmp("linear",pGXyvy->yat) != 0) &&
                 (strcmp("",pGXyvy->yat) != 0)) &&
                 (strcmp("linear",pGXyvy->proj) == 0) ) {
              if ((save_xv_0=(float *)malloc(pA->xs[0][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(save_xv_0,pA->xv[0],pA->xs[0][0],
                            &pA->xf[0][0], &pA->xl[0][0], pGXyvy->yat,
                            &did_save_y, "y") == 0) {
                       if (did_save_y) {
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
             strcpy(pGXyvy->yat, "linear");

/*			Get a default for the X axis.			*/

	   if (!nicedf(pA->min,pA->max,&dr,&pw10,&center) )
	     {	
	      dr=1.0;
	      pw10=0.0;
	      center=pA->min;
	     }
	   dx=dr*pow( (double) 10.0, (double) pw10);
	   k=(pA->min-center)/dx;
	   if (pA->min < center) k--;
	   x1=center+k*dx;
	   k=(pA->max-center)/dx;
	   if (pA->max > center) k++;
	   x2=center+k*dx;
		 
/* 			First and last of first dimension determines */
/* 			direction of the plot.				* */

/*  DNW - 10/8/02 - This allows only the X-axis to change. Now with this removed the user */
/*                  can modify the Y-axis as well. */
/* C.D. 7/18/03 Further modify to allow setting both x and y and autoscale */
	   if (pGXyvy->dsp[1]<9.9e19) 
	     {
	       dsp[1]=pGXyvy->dsp[1];
	     } 
	   else
	     {
/* 	       dsp[1]=pGXyvy->dsp[1]=*pA->xf[0]; */
	       dsp[1]=*pA->xf[0];
	     }
	   dsp[0]=x1;
	   if (pGXyvy->dsp[3]<9.9e19) 
	     {
	       dsp[3]=pGXyvy->dsp[3];
	     }
	   else
	     {
/* 	       dsp[3]=pGXyvy->dsp[3]=*pA->xl[0]; */
	       dsp[3]=*pA->xl[0];
	     }
	   dsp[2]=x2;

	   /* Remembers these values for later use in click part */
	   pD->dsp_used[0]=dsp[0];
	   pD->dsp_used[1]=dsp[1];
	   pD->dsp_used[2]=dsp[2];
	   pD->dsp_used[3]=dsp[3];
/*			Set up the projection for this picture.		*/

	   
	   if (set_projection(pGXyvy->proj,pP->dsp,pGXyvy->dsp,dsp) == 0)
	     {
	      err_warn(1,fperr,"Error - in projection for X(y) vs y.\n");
	      erret++;
	     }
	     
/*		If data range is outside (or on) the display (user coordinate)
		range limits and both are on the same side,
		there is no	   printf("hold_y_name: %s\n",hold_y_name);
thing that will display.			*/

	   if (   ((pj->X1-pA->min)*(pj->X2-pA->min) > 0.0 &&
		   (pj->X1-pA->max)*(pj->X2-pA->max) > 0.0 &&
		   (pj->X1-pA->max)*(pj->X1-pA->min) > 0.0 ) ||
	 	  ((pj->Y1-*pA->xf[0])*(pj->Y2-*pA->xf[0]) > 0.0 &&
		   (pj->Y1-*pA->xl[0])*(pj->Y2-*pA->xl[0]) > 0.0 &&
		   (pj->Y1-*pA->xl[0])*(pj->Y1-*pA->xf[0]) > 0.0) )
	     {
	      err_warn(1,fperr,
			   "Error - X(y) data is outside the display range.\n");
	      erret++;
	     }
	   if (   erret == 0 && pD->dsp_seg[3] > 0 && (pP->dsp.p > 0 &&
		  pP->dsp.x1 < 1.0 && pP->dsp.y1 < 1.0 && pP->dsp.x1 > 0.0 &&
		  pP->dsp.y1 > 0.0 && pP->dsp.x2 < 1.0 && pP->dsp.y2 < 1.0 &&
		  pP->dsp.x2 > 0.0 && pP->dsp.y2 > 0.0 ))
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

	      plot_line(*pA->xs[0],pA->un.data,pA->mask,pA->xv[0],NULL,
						pGXyvy->lb,pGXyvy->mb);

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
			"Error - X(y) data (A_%s) from file (%s) not"
			  " acquired.\n",ptab->name,pA->F);
	      pA->notok=1;
	     }
	   else if (pA->f != NULL)
	     {
	      err_warn(1,fperr,
			"Error - X(y) data (A_%s) from (%s) cannot be"
			  " computed.\n",ptab->name,pA->f);
	      pA->notok=1;
	     }
	   else
		err_warn(1,fperr,"Error - with X(y) component (A_%s).\n",
								ptab->name);
	   if (pD->dsp_seg[0] > 0) gdsg(pD->dsp_seg[0]);
	   pD->dsp_seg[0]=0;
	   pD->dsp_seg[1]=0;
	   pD->dsp_seg[2]=1;
	   pD->dsp_seg[3]=0;
	   erret++;
	  }
	killA_tmp();

	/* DNW - 10/17/00 if (erret == 0 && pD->leg_seg[3] > 0 & pD->dsp_seg[3] > 0)*/
	if (erret == 0 && pD->leg_seg[3] > 0)
	  {
	   gcrsg(pD->leg_seg[0]=++segment_num);
	   gssgp(pD->leg_seg[0],(pP->leg.p+pD->pri)/1000.0);

	   if (legendGXyvy(pGXyvy->lb,pGXyvy->mb,pA->n,pP,pD) > 0)
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
          if ((hold_x_name = (char *) malloc(strlen(pA->xn[0])+1)) == NULL)
               err_warn(1,fperr, "Error - no memory for labelling plot.\n");
          strncpy(hold_x_name, pA->xn[0], strlen(pA->xn[0])); hold_x_name[strlen(pA->xn[0])] = '\0';
          if (pA->xn[1] != NULL) {
           if ((hold_y_name = (char *) malloc(strlen(pA->xn[1])+1)) == NULL)
	     err_warn(1,fperr, "Error - no memory for labelling plot.\n");
           strncpy(hold_y_name, pA->xn[1], strlen(pA->xn[1])); hold_y_name[strlen(pA->xn[1])] = '\0';
	   free((char *) pA->xn[1]);
          }
          if (pA->xn[0]!=NULL) free((char *) pA->xn[0]);
          if ((pA->xn[0] = (char *) malloc(strlen(pA->n)+1)) == NULL)
               err_warn(1,fperr, "Error - no memory for labelling plot.\n");
          strncpy(pA->xn[0],pA->n,strlen(pA->n)); pA->xn[0][strlen(pA->n)] = '\0';

/*           if (pA->xn[1] != NULL) { */
/*            free((char *) pA->xn[1]); */
/* 	  } */
           if ((pA->xn[1] = (char *) malloc(strlen(hold_x_name)+1)) == NULL)
                err_warn(1,fperr, "Error - no memory for labelling plot.\n");
           strncpy(pA->xn[1],hold_x_name,strlen(hold_x_name)); pA->xn[1][strlen(hold_x_name)] = '\0';

	   /* Oct 5th, 2005: C.D there used to be an if test pa->xs[1] NULL but it made everytying cored ump */
	   did_xs=(int)pA->xs[1];
	   free(pA->xs[1]);

           if ((pA->xs[1] = (int *) malloc(sizeof(int))) == NULL)
                err_warn(1,fperr, "Error - no memory for labelling plot.\n");
	   *pA->xs[1]=2;
	

	   pict_elem(pA,pP,pD,pGXyvy->proj,
		pGXyvy->xtl1,pGXyvy->xtl2,pGXyvy->xmt1,pGXyvy->xmt2,
		pGXyvy->ytl1,pGXyvy->ytl2,pGXyvy->ymt1,pGXyvy->ymt2,
				     pGXyvy->dsp);

	   free((int *)pA->xs[1]);
	   /* No need for test on did_xs */
/* 	   if (did_xs!=-1) */
/* 	     { */
	       if ((pA->xs[1] = (int *) malloc(sizeof(int))) == NULL)
		 err_warn(1,fperr, "Error - no memory for labelling plot.\n");
	       *pA->xs[1]=did_xs;
/* 	     } */

          free((char *) pA->xn[0]);
          if ((pA->xn[0] = (char *) malloc(strlen(hold_x_name)+1)) == NULL)
               err_warn(1,fperr, "Error - no memory for labelling plot.\n");
          strncpy(pA->xn[0],hold_x_name,strlen(hold_x_name));pA->xn[0][strlen(hold_x_name)] = '\0';
          if (pA->xn[1] != NULL) {
           free((char *) pA->xn[1]);
	   pA->xn[1]=NULL;
	   if (hold_y_name!=NULL) {
/* 	     printf("hold_y_name: %s\n",hold_y_name); */
	     if ((pA->xn[1] = (char *) malloc(strlen(hold_y_name)+1)) == NULL)
	       err_warn(1,fperr, "Error - no memory for labelling plot.\n");
	     strncpy(pA->xn[1],hold_y_name,strlen(hold_y_name));pA->xn[1][strlen(hold_y_name)] = '\0';
	   }
          }
        }

/*		If the variable isn't computed then restore excess dimensions.*/

        if (did_save_y)
        {
           for (l = 0; l < pA->xs[0][0]; ++l)
	   /* Commented out by C. Doutriaux on Apr 4th, 2005 made the canvas disapear */
	   /* when going back and forth between log10 and linear axis */
/*            *pA->xf[0] = pGXyvy->dsp[1] = save_xf[0]; */
/*            *pA->xl[0] = pGXyvy->dsp[3] = save_xl[0]; */
              pA->xv[0][l] = save_xv_0[l];
           *pA->xf[0] =  save_xf[0];
           *pA->xl[0] =  save_xl[0];
           free((char *) save_xv_0);
        }

	if (pA->f == NULL)
	   for (i=1;i<pA->ND;i++)
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

/*		Draw lines and plot markers.				*/

    int plot_line (int im,float *x,short *mx,float *y,short *my,
							char *lb,char *mb)


/*      int im;		Size of the array dimension.			*/
/*      float x[im];	X						*/
/*      float y[im];	Y						*/
/*	short mx[im];	mask for x, NULL indicates no mask.		*/
/*	short my[im];	mask for y, NULL indicates no mask.		*/
/*      float lb;	Attributes of line.				*/
/*      float mb;	Attributes of marker.				*/

      {
	int i,j,k;
	float dx,rx,ry;
	char label[20];
	Gpoint pxy[100];
	struct table_line *pl;
	struct table_mark *pm;

	if (lb != NULL && lb[0] != '\0')
	  {
	   for (pl=&Tl_tab;pl!=NULL && strcmp(lb,pl->name)!=0;pl=pl->next);
	   if (pl == NULL)
	      err_warn(1,fperr,"Error - line attributes (%s) not found.\n",lb);
	   else
	     {
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

	      for (j=i=0;i<im;i++)
	        {
	         if ( (mx == NULL || mx[i]) && (my == NULL || my[i]) )
		   {
		    pxy[j].x=x[i];
		    pxy[j].y=y[i];
		    j++;
		   }
	         else
		   {
		    if (j > 1) proj_pl(j,pxy);
		    j=0;
		   }
		 if (j > 99)
		   {
		    proj_pl(j,pxy);
		    pxy[0].x=pxy[j-1].x;
		    pxy[0].y=pxy[j-1].y;
		    j=1;
		   }
	        }

	      if (j > 1) proj_pl(j,pxy);
	     }
	  }
	if (mb != NULL && mb[0] != '\0')
	  {
	   for (pm=&Tm_tab;pm!=NULL && strcmp(mb,pm->name)!=0;pm=pm->next);
	   if (pm == NULL)
	    /*err_warn(1,fperr,"Error - marker attributes (%s) not found.\n",mb);*/
            ;
	   else
	     {
/*	      gsmk(pm->mtyp);
	      gsmksc(pm->msize);
	      gspmci(pm->mci);*/
              Vma_tab.type = pm->mtyp[0];
              Vma_tab.size = pm->msize[0];
              Vma_tab.colour = pm->mci[0];

	      for (j=i=0;i<im;i++)
	        {
	         if ( (mx == NULL || mx[i]) && (my == NULL || my[i]) )
		   {
		    pxy[j].x=x[i];
		    pxy[j].y=y[i];
		    j++;
		   }
	         else
		   {
		    if (j > 0) proj_pm(j,pxy);
		    j=0;
		   }
		 if (j > 99)
		   {
		    proj_pm(j,pxy);
		    j=0;
		   }
	        }

	      if (j > 0) proj_pm(j,pxy);
	     }
	  }

	return 1;
      }


/*			Display the legend for Xyvy.			*/


    int legendGXyvy(
		    char *lb,
		    char *mb,
		    char *name,
		    struct p_tab *pP,
		    struct display_tab *pD
		   )

      {
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
