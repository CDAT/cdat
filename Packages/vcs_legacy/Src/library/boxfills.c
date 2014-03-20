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
#include "workstations.h"

	typedef struct level_fill {int color; float l1,l2;} S_boxfill;

#define NMAX 1000

    extern FILE *fpin,*fpout,*fperr;

    extern struct workstations Wkst[];
    extern struct table_text Tt_tab;
    extern struct table_chorn To_tab;
    extern struct table_line Tl_tab;
    extern struct table_fill Tf_tab;
    extern struct table_pattern Pat_tab;

    extern struct display_tab D_tab;

    extern struct project_attr p_PRJ;

    extern struct default_continents Dc;

    extern char PRJ_names[PRJ_TYPES][17];

    extern int segment_num;

    int nicedf(float a,float b,float *dr,int *pw10,float *center);

    int vcs_legacy_compute_legend_values(int horiz, S_boxfill *regis,
                                  struct pe_leg pe, float vch,
                                  struct table_line *pTl, 
                                  int num, int l1, int l2, int boxfill_type);
    int pts_compute_legend_values(int horiz, S_boxfill *regis, 
                                  struct pe_leg pe, float vch,
                                  struct table_line *pTl,
                                  int num, int l1, int l2, char * legend);
    int list_compute_legend_values(int horizs, S_boxfill *regis, 
                                  struct pe_leg pe, float vch,
                                  struct table_line *pTl, int num, int l1, int l2,
				  char * legend, int boxfill_type, int ext_1, int ext_2,
                                  struct fill_range *values);
    void show_legend_numbers(int horizs, S_boxfill *regis,
                                  struct pe_leg pe, float vch,
                                  struct table_line *pTl, int num,
                                  int l1, int l2, int number,
                                  float * legend_nums, struct l_val *vtab);


    extern int generic_world (float x1,float y1,float x2,float y2, char *lb,
                               char *trans_xaxis, char *trans_yaxis);
    extern int generic_world2 (float x1,float y1,float x2,float y2, char *lb,
                               char *trans_xaxis, char *trans_yaxis);


    int boxfills(pD,pGfb,pP,ptab)

      struct display_tab *pD;
      struct gfb_attr *pGfb;
      struct p_tab *pP;
      struct a_tab *ptab;

      {
	int n,erret,level_ret;
	int i,k,k1,k2,ki,c1,c2;
	int e1,e2;
	float lev1,lev2,dr,del,center;
	float dsp[NDS];
	struct a_attr *pA;
	struct project_attr *pj;
	Glimit pvp;
	S_boxfill regis[256], legend_regis[256];
	int num_regis, data_regis, legend_num_regis;
        int custom_start, custom_end, custom_stride;

	int ND;
	int save_xs[NDS];
	float save_xl[NDS],save_xf[NDS];
	float X1,Y1,X2,Y2; /*   generic_world coordinate values */
	char proj[17];

        int l, size=1, ierr;
        float *save_xv_0, *save_xv_1;
        int did_save_x=0, did_save_y=0;
        extern int trans_axis();

	float save_min, save_max, save_mean;
	S_boxfill save_regis[256];
	int save_num_regis=0, save_color_1, save_color_2;
	short *save_mask;
	float *fp, *save_data, *log10_data, save_lev_1, save_lev_2;
	void replace_boxfill_data_f( );
	int savecontinents;

	struct fill_range       	*pfiso;
        struct table_fill               *pf, *ptb, *ftab;


	pj=&p_PRJ;
	pA=ptab->pA_attr;
	erret=0;
	num_regis=0; data_regis=0; legend_num_regis=0;

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

/*     generic_world coordinate values */
        X1= *pA->xf[0];
        Y1= *pA->xf[1];
        X2= *pA->xl[0];
        Y2= *pA->xl[1];

/*			Check NDC space is greater than zero.		*/

	if (pP->dsp.x2-pP->dsp.x1 < .001 || pP->dsp.y2-pP->dsp.y1 < .001)
	  {
	   err_warn(1,fperr,
			"Error - NDC display space is nil for BOXFILL.\n");
	   pD->dsp_seg[1]=0;
	   pD->off=2;
	   erret++;
	  }


/*			Check the number of dimensions and number of
			values of the variable's dimensions.		*/

	if (erret == 0 && (pA->xs[0] == NULL || pA->xs[1] == NULL ||
			      *(pA->xs[0]) <= 1 || *(pA->xs[1]) <= 1)  )
	  {
	   err_warn(1,fperr,
		"Error - BOXFILL requires 2 multi-valued dimensions.\n");
	   pD->off=2;
	   return 0;
	  }
	if (erret == 0 && ((pA->xs[2] != NULL && *(pA->xs[2]) > 1) ||
			      (pA->xs[3] != NULL && *(pA->xs[3]) > 1)  )  )
	  {
	   if (pA->f != NULL)
	     {
	      err_warn(0,fperr,
	          "Warning - BOXFILL is 2D. Taking a subset of (%dD) computed "
		  "variable.\n"
		  "        Modify dimensions of rhs: %s=%s\n",
				ND,ptab->name,pA->f);
	   /*   erret++;*/
	     }
	   else
	     {
	      err_warn(0,fperr,
			"Warning - BOXFILL is 2D; (%s) is %dD.\n"
		"          Used only first values of excess dimensions.\n",
				ptab->name,ND);
	      for (i=2;i<pA->ND;i++)
		{
		 *pA->xs[i]=1;
		 *pA->xf[i]=*pA->XF[i];
		 *pA->xl[i]=*pA->XF[i];
		}
	     }
	  }


/*                              Transform the x coordinate axis.         */
           if ( ((strcmp("linear",pGfb->xat) != 0) &&
                 (strcmp("",pGfb->xat) != 0)) &&
                 (strcmp("linear",pGfb->proj) == 0) ) {
              if ((save_xv_0=(float *)malloc((pA->xs[0][0]+1)*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");                   return 0;
                 } else {
                    if (ierr=trans_axis(save_xv_0,pA->xb[0],pA->xs[0][0]+1,
                            &pA->xf[0][0], &pA->xl[0][0], pGfb->xat,
                            &did_save_x, "x") == 0) {
                       if (did_save_x) {
                          for (l = 0; l < pA->xs[0][0]+1; ++l)
                             pA->xb[0][l] = save_xv_0[l];
                          *pA->xf[0] = save_xf[0];
                          *pA->xl[0] = save_xl[0];
                          free((char *) save_xv_0);
                       }
                       return 0;
                    }
                 }
           } else
	     strcpy(pGfb->xat, "linear");

/*                              Transform the y coordinate axis.         */
           if ( ((strcmp("linear",pGfb->yat) != 0) &&
                 (strcmp("",pGfb->yat) != 0)) &&
                 (strcmp("linear",pGfb->proj) == 0) ) {
              if ((save_xv_1=(float *)malloc((pA->xs[1][0]+1)*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");                   return 0;
                 } else {
                    if (ierr=trans_axis(save_xv_1,pA->xb[1],pA->xs[1][0]+1,
                            &pA->xf[1][0], &pA->xl[1][0], pGfb->yat,
                            &did_save_y, "y") == 0) {
                       if (did_save_x) {
                          for (l = 0; l < pA->xs[0][0]+1; ++l)
                             pA->xb[0][l] = save_xv_0[l];
                          *pA->xf[0] = save_xf[0];
                          *pA->xl[0] = save_xl[0];
                          free((char *) save_xv_0);
                       }
   
                       if (did_save_y) {
                          for (l = 0; l < pA->xs[1][0]+1; ++l)
                             pA->xb[1][l] = save_xv_1[l];
                          *pA->xf[1] = save_xf[1];
                          *pA->xl[1] = save_xl[1];
                          free((char *) save_xv_1);
                       }
                       return 0;
                    }
                 }
           } else
             strcpy(pGfb->yat, "linear");

/*			First and last of first two dimension determines
			direction of the plot.				*/

	dsp[0]=*pA->xf[0];
	dsp[1]=*pA->xf[1];
	dsp[2]=*pA->xl[0];
	dsp[3]=*pA->xl[1];

/*			Set up the projection for this picture.		*/
	strcpy(proj,pGfb->proj);
	if (cmpncs(proj,"Mollweide")==0 || cmpncs(proj,"Robinson")==0 ||
	    cmpncs(proj,"Polar"    )==0 )
	  {
	   if (cmpncs(pA->XN[0],"longitude") != 0 ||
	          cmpncs(pA->XN[1],"latitude" ) != 0 )
	     {
	      err_warn(1,fperr,
		      "Error - (BOXFILLS) spherical projections work"
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

	if (set_projection(proj,pP->dsp,pGfb->dsp,dsp) == 0)
	  {
	   err_warn(1,fperr,"Error - in projection for BOXFILLS.\n");
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
		   "Error - BOXFILL data is outside the display range.\n");
	   erret++;
	  }

	if (pD->dsp_seg[3] > 0 && (pP->dsp.p > 0 && pP->dsp.x1 < 1.0 &&
	    pP->dsp.y1 < 1.0 && pP->dsp.x1 > 0.0 && pP->dsp.y1 > 0.0 &&
	    pP->dsp.x2 < 1.0 && pP->dsp.y2 < 1.0 && pP->dsp.x2 > 0.0 &&
	    pP->dsp.y2 > 0.0 ) )
	  {

/*              If the variable isn't computed then restore excess dimensions.*/
           if (did_save_x)
           {
              for (l = 0; l < pA->xs[0][0]+1; ++l)
                 pA->xb[0][l] = save_xv_0[l];
              *pA->xf[0] = save_xf[0];
              *pA->xl[0] = save_xl[0];
              free((char *) save_xv_0);
	      did_save_x = 0;
           }

           if (did_save_y)
           {
              for (l = 0; l < pA->xs[1][0]+1; ++l)
                 pA->xb[1][l] = save_xv_1[l];
              *pA->xf[1] = save_xf[1];
              *pA->xl[1] = save_xl[1];
              free((char *) save_xv_1);
	      did_save_y = 0;
           }

#ifdef cray
	   if (erret == 0 && acquire_A(ptab,"R*8") > 0)
#else
	   if (erret == 0 && acquire_A(ptab,"R*4") > 0)
#endif
	     {
/* 			Compute the mean if from CDAT or Computed */
	   if ((pGfb->boxfill_type == 2) || (pGfb->boxfill_type == 3)) { /* Compute the log10 of the data */
		save_min = pA->min; save_max = pA->max, save_mean = pA->mean;

	        for (l = 0; l < pA->ND; l++) size *= *pA->xs[l];
		if ((save_data=(float *)malloc(size*sizeof(float)))==NULL) {
                    err_warn(1,fperr, "Not enough memory to store plot data.\n");
                    return 0;
                }
		if ((save_mask=(short *)malloc(size*sizeof(short)))==NULL) {
                    err_warn(1,fperr, "Not enough memory to store plot data.\n");
                    return 0;
                }
	        for (l=0; l<size; ++l) {
	   	    save_data[l] = pA->un.data[l];
	   	    save_mask[l] = pA->mask[l];
		}

		replace_boxfill_data_f(pA->un.data,pA->mask,&pA->xs[0],pA->ND,pA->min,pA->max,
                                       pGfb->boxfill_type,pGfb->legend, pGfb->line,&num_regis,
                                       &data_regis);

		save_lev_1 = pGfb->lev1;
		save_lev_2 = pGfb->lev2;
		pGfb->lev1 = pGfb->lev2 = 1.e+20;
	        if (pGfb->boxfill_type == 3) { /* Store originial color legend values for custom */
		   save_color_1 = pGfb->color_1;
		   save_color_2 = pGfb->color_2;
	           for (legend_num_regis=0, pfiso=pGfb->line; pfiso!=NULL; legend_num_regis++, pfiso=pfiso->next);
		   pGfb->color_1 = 16; /* Set the min color for the regis */
		   pGfb->color_2 = 16+num_regis-1; /* Set the max color for the regis */
		}
	        set_bfills(pGfb,pA,save_regis,&save_num_regis);/* save the original legend color range */
		if (save_num_regis == 1)
	           set_bfills(pGfb,pA,regis,&num_regis);/* save the original legend color range */

	   }
	   /*DNW - 8/12/04 - if (pA->f != NULL) - Always do this since f is now for the most part always NULL. */
           mmmm(pA->un.data,pA->mask,&pA->xs[0],&pA->xw[0],2,
                        &pA->XC[0],&pA->xv[0],&pA->min,&pA->max,&pA->mean);
/*                              Transform the x coordinate axis.         */
           if ( ((strcmp("linear",pGfb->xat) != 0) &&
                 (strcmp("",pGfb->xat) != 0)) &&
                 (strcmp("linear",pGfb->proj) == 0) ) {
              if ((save_xv_0=(float *)malloc((pA->xs[0][0]+1)*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(save_xv_0,pA->xb[0],pA->xs[0][0]+1,
                            &pA->xf[0][0], &pA->xl[0][0], pGfb->xat,
                            &did_save_x, "x") == 0) {
                       if (did_save_x) {
                          for (l = 0; l < pA->xs[0][0]+1; ++l)
                             pA->xb[0][l] = save_xv_0[l];
                          *pA->xf[0] = save_xf[0];
                          *pA->xl[0] = save_xl[0];
                          free((char *) save_xv_0);
                       }
                       return 0;
                    }
                 }
           } else
             strcpy(pGfb->xat, "linear");

/*                              Transform the y coordinate axis.         */
           if ( ((strcmp("linear",pGfb->yat) != 0) &&
                 (strcmp("",pGfb->yat) != 0)) &&
                 (strcmp("linear",pGfb->proj) == 0) ) {
              if ((save_xv_1=(float *)malloc((pA->xs[1][0]+1)*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(save_xv_1,pA->xb[1],pA->xs[1][0]+1,
                            &pA->xf[1][0], &pA->xl[1][0], pGfb->yat,
                            &did_save_y, "y") == 0) {
                       if (did_save_x) {
                          for (l = 0; l < pA->xs[0][0]+1; ++l)
                             pA->xb[0][l] = save_xv_0[l];
                          *pA->xf[0] = save_xf[0];
                          *pA->xl[0] = save_xl[0];
                          free((char *) save_xv_0);
                       }
   
                       if (did_save_y) {
                          for (l = 0; l < pA->xs[1][0]+1; ++l)
                             pA->xb[1][l] = save_xv_1[l];
                          *pA->xf[1] = save_xf[1];
                          *pA->xl[1] = save_xl[1];
                          free((char *) save_xv_1);
                       }
                       return 0;
                    }
                 }
           } else
             strcpy(pGfb->yat, "linear");

              if (save_num_regis != 1) {
                 if (pGfb->boxfill_type == 3) { /* For custom boxfill type only.*/
                  pGfb->color_2 = 16+data_regis-1; /* Set the data max color for the regis */
                  set_bfills(pGfb,pA,regis,&data_regis);
                  pGfb->color_2 = 16+num_regis-1; /* Set the legend max color for the regis */
                  set_bfills(pGfb,pA,legend_regis,&legend_num_regis);
                 } else {
                  set_bfills(pGfb,pA,regis,&num_regis);
                  set_bfills(pGfb,pA,legend_regis,&legend_num_regis);
                 }
              }

              /* Search for the fillarea object in the link list and set the color 
               * index values for the legend only.
               */
	      if (save_num_regis != 0) { /* Don't change the colors for 'linear' */
	         for (i=0, pfiso=pGfb->line; pfiso!=NULL; i++, pfiso=pfiso->next) {
                    for (ptb=ftab=&Tf_tab;
                       ftab!=NULL && cmpncs(pfiso->fill_name,ftab->name)!=0;
                           ptb=ftab,ftab=ftab->next);
                        if ((pGfb->boxfill_type != 3) && (i < num_regis)) regis[i].color = *ftab->faci;
                        if (i < legend_num_regis) legend_regis[i].colour = *ftab->faci;
	         }
	      }

              /* Do this for custom boxfill type only. From the legend color range find
               * the corresponding data color range.
               */
              if (pGfb->boxfill_type == 3) {
                 custom_start = 0;
                 custom_end = num_regis;
                 for (i=0, pfiso=pGfb->line; pfiso!=NULL; i++, pfiso=pfiso->next) {
                    if ((pfiso->lev1 <= save_min) && (pfiso->lev2 > save_min)) {
                       custom_start = i;
                    }
                    if ((pfiso->lev1 < save_max) && (pfiso->lev2 >= save_max)) {
                       custom_end = i;
                    }
                 }
                 for (i=0; i < data_regis; i++) {
                     regis[i].colour = legend_regis[custom_start+i].colour;
                 }
              }


	      if (erret == 0)
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

                 if (pGfb->boxfill_type != 3)
	            fillbox(*pA->xs[0],*pA->xs[1],pA->un.data,pA->mask,
			 pA->xb[0],pA->xb[1],regis,num_regis,pGfb->proj);
                 else
	            fillbox(*pA->xs[0],*pA->xs[1],pA->un.data,pA->mask,
			 pA->xb[0],pA->xb[1],regis,data_regis,pGfb->proj);

/* DNW - 05/03/01	         if (doexist("longitude",pA->XN[0]) &&
			doexist("latitude",pA->XN[1])  && Dc.selected > 0) */
/* DNW - 08/20/07	         if (Dc.selected > 0) I put the above line back to avoid 
                                                      continents drawing when dimensions are not (long,lat)*/
		 savecontinents = Dc.selected;
		 if (pD->continents==-1) pD->continents = Dc.selected;
		 else Dc.selected = pD->continents;
		 

	         if (doexist("longitude",pA->XN[0]) && doexist("latitude",pA->XN[1])  && pD->continents > 0)
		   {
                    /*    generic_world coordinate values */
                    if (pD->continents < 3)
		       generic_world(X1,Y1,X2,Y2,Dc.lb,pGfb->xat,pGfb->yat);
	            else
		       generic_world2(X1,Y1,X2,Y2,Dc.lb,pGfb->xat,pGfb->yat);
		   }
		 Dc.selected = savecontinents;

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
		err_warn(1,fperr,
			"Error - boxfill data (A_%s) from file (%s) not"
			  " acquired.\n",ptab->name,pA->F);
	      else if (pA->f != NULL)
		err_warn(1,fperr,
			"Error - boxfill data (A_%s) from (%s) cannot be"
			  " computed.\n",ptab->name,pA->f);
	      else
		err_warn(1,fperr,"Error - boxfilling (A_%s).\n",ptab->name);
	      if (pD->dsp_seg[0] > 0) gdsg(pD->dsp_seg[0]);
	      pA->notok=1;
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
	   gcrsg(pD->leg_seg[0]=++segment_num);
	   gssgp(pD->leg_seg[0],(pP->leg.p+pD->pri)/1000.0);

	  if (pGfb->boxfill_type == 3) { /* Restore the originial color legend values for custom */
	      num_regis=save_num_regis;
	      pGfb->color_1 = save_color_1;
	      pGfb->color_2 = save_color_2;
	      for (k=0; k < save_num_regis; k++ ) {
	          regis[k].l1    = save_regis[k].l1;
	          regis[k].l2    = save_regis[k].l2;
	      }
	   }
          /* Restore the originial levels values for log10 or custom */
	  if ((pGfb->boxfill_type == 2) || (pGfb->boxfill_type == 3)) { 
		  pGfb->lev1 = save_lev_1;
		  pGfb->lev2 = save_lev_2;
	  }

	   if (num_regis == 0) set_bfills(pGfb,pA,regis,&num_regis);

           if (pGfb->boxfill_type == 3) level_ret = legendGfb(legend_regis,legend_num_regis,pP,pD);
           else level_ret = legendGfb(regis,num_regis,pP,pD);
	   if (level_ret > 0)
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

	if ((pGfb->boxfill_type == 2) || (pGfb->boxfill_type == 3)) /* Restore the min, max, & mean for log10 custom computed data */
	   { pA->min = save_min; pA->max = save_max; pA->mean = save_mean; 
	     for (k=0; k < save_num_regis; k++ ) regis[k].color = save_regis[k].color;
	     free((char *) pA->un.data);
	     free((char *) pA->mask);
	     pA->un.data = save_data;
	     pA->mask = save_mask;
	   }

	pict_elem(pA,pP,pD,pGfb->proj,
		pGfb->xtl1,pGfb->xtl2,pGfb->xmt1,pGfb->xmt2,
		pGfb->ytl1,pGfb->ytl2,pGfb->ymt1,pGfb->ymt2,
				     pGfb->dsp);

/*		If the variable isn't computed then restore excess dimensions.*/
        if (did_save_x)
        {
           for (l = 0; l < pA->xs[0][0]+1; ++l)
              pA->xb[0][l] = save_xv_0[l];
           *pA->xf[0] = save_xf[0];
           *pA->xl[0] = save_xl[0];
           free((char *) save_xv_0);
        }

        if (did_save_y)
        {
           for (l = 0; l < pA->xs[1][0]+1; ++l)
              pA->xb[1][l] = save_xv_1[l];
           *pA->xf[1] = save_xf[1];
           *pA->xl[1] = save_xl[1];
           free((char *) save_xv_1);
        }

	if (pA->f == NULL)
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

/*		log10 Boxfill data (note: this is for  float data type).		*/

    void replace_boxfill_data_f (pdat,pmask,pdim,nd,dmin,dmax,type,legend,values,color_num,data_num)
	float *pdat;      /*  Data array (float).                         */
	short *pmask;     /*  Data mask.                                  */
	int *pdim[];      /*  Dimensions.                                 */
	int nd;           /*  Number of dimensions.                       */
        float dmin;       /*  Minimum data value.                         */
        float dmax;       /*  Maximum data value.                         */
	int type;         /*  Legend type (i.e., log10 or custom)         */
	char * legend;    /*  Custom legend                               */
	struct fill_range *values; /* level values                        */
        int *color_num;   /*  Return the number of color indices to use.  */
        int *data_num;    /*  Return the number of data indices to use.   */

      {
	int		i, j, k = 0, size=1;
	float		*fp;
	short		*sp;

	int cmax=0, dcmax=0, found;
        int lstart=0,lend=0;
        float *level,*level2,temp;
        struct l_tab *ptab;
	struct l_val *vtab;
        extern struct l_tab L_tab[2];
	struct fill_range       *pfiso;

	if (nd < 1) return ;

	for (i = 0; i < nd; i++) size *= *pdim[i];

	if (type == 2) {
	   for (fp=pdat,sp=pmask,i=0; i<size; fp++,sp++,i++) {
	      if (*sp) {
	         if  (*fp > 0)
		    *fp = log10(*fp);
		 else {       /* if value is <= 0, then mask out */
		   *fp = 1e20;
		   *sp = 1;
		 }
	      }
	   }
	} else {
	   for (cmax = 0, pfiso = values; pfiso != NULL; cmax++, pfiso = pfiso->next);
	   cmax += 1;

           if ((level=(float *)malloc(cmax*sizeof(float)))==NULL) {
                err_warn(1,fperr, "Error - memory overflow in legend axis.\n");
                return ;
           }

           /* Count the number of usable color levels and get the levels */
	   pfiso = values;
           level[0] = pfiso->lev1;
           *color_num = 0;
	   for (i = 1, pfiso = values; pfiso != NULL; pfiso = pfiso->next, i++) {
              level[i] = pfiso->lev2;
              *color_num += 1;
           }

	   /* Sort the levels from smallest to largest */
	   for (i = 1; i < cmax; i++) {
	      temp = level[i];
	      for (j=i-1; j >= 0 && level[j] > temp; j --)
		  level[j+1] = level[j];
	      level[j+1] = temp;
	   }

           /* Find the min and max indices from the sorted level list */
           *data_num = 0;
	   for (i = 0; i < cmax; i++) {
             if ((dmin >= level[i]) && (dmin < level[i+1])) lstart = i;
             if ((dmax > level[i]) && (dmax <= level[i+1])) {lend = i+1; break;}
             lend = i;
           }

           /* Get the number of usable data levels */
           *data_num = (lend - lstart); 
           dcmax=*data_num+1;

           /* Get the actual data levels from the legend level */
           if ((level2=(float *)malloc(dcmax*sizeof(float)))==NULL) {
                err_warn(1,fperr, "Error - memory overflow in legend axis.\n");
                return ;
           }
	   for (i = 0; i < dcmax; i++) {
               level2[i] = level[lstart+i];
           }
              
           /* Use the acutal usable data levels to determine recalibrate the data into bins */
	   for (fp=pdat,sp=pmask,i=0; i<size; fp++,sp++,i++) {
	      found = 0;
	      for (j=0; j < dcmax-1; j++) {
	         if (*sp) {
		    if ((*fp >= level2[j]) && (*fp <= level2[j+1])) {
		       *fp = (j + 0.5) / dcmax;
		       found = 1;
		       break;
		    }
		 }
	      }

	      if (found == 0) {
	         *fp = 1.0e20;
	         *sp = 0;
	      }

	   }

	   free((char *) level);
	}

      }


/*		Fill boxfill.						*/

    int fillbox (im,jm,a,mask,xb,yb,regis,ni,proj)

 	int im,jm;
	float *a;
	short *mask;
	float *xb;
	float *yb;
	S_boxfill regis[];
	int ni;
	char *proj;

/*      int im,jm;	Limits of the array dimension sizes.		*/
/*      float a[im,jm];	Array to be contoured.				*/
/*      float xb;	Boundary values of x nodes.			*/
/*      float yb;	Boundary values of y nodes.			*/
/*      S_boxfill *regis;	Pointer to color and ranges.		*/
/*	int ni;		number of different colors and ranges.		*/
/*			An extra color has been added "regis[ni]" to	*/
/*			declare a color for missing data, and so there	*/
/*			are actually ni+1 instances of regis.		*/
/*	char *proj	A string defining the type of projection.	*/

/*	Other values are taken from global variables defined above.	*/

      {
	int i,j,m,mm;
	Gpoint pxy[4];

	int pole;

	pole=cmpncs(proj,"polar") == 0;

	gsfais(GSOLID);

	for (j=0;j<jm;j++)
	  {
	   pxy[0].x=xb[0];
	   pxy[0].y=yb[j];
	   pxy[3].x=xb[0];
	   pxy[3].y=yb[j+1];
	   mm=-1;
	
	   for (i=0;i<im;i++)
	     {
	      if (mask[i+j*im])
		{

/*			Loop over ranges.				*/

	         for (m=0; m<ni; m++)
		   {
		    if ((regis[m].l1-a[i+j*im])*(regis[m].l2-a[i+j*im]) <= 0.0)
		      {
		       if (m == mm)
		         {
		          pxy[1].x=xb[i+1];
		          pxy[1].y=yb[j];
		          pxy[2].x=xb[i+1];
		          pxy[2].y=yb[j+1];
		         }
		       else if (mm == -1)
		         {
		          pxy[1].x=xb[i+1];
		          pxy[1].y=yb[j];
		          pxy[2].x=xb[i+1];
		          pxy[2].y=yb[j+1];
		         }
		       else
		         {
	   	          gsfaci(regis[mm].color);
		          proj_gfa(4,pxy);
		          pxy[0].x=pxy[1].x;
		          pxy[0].y=pxy[1].y;
		          pxy[3].x=pxy[2].x;
		          pxy[3].y=pxy[2].y;
		          pxy[1].x=xb[i+1];
		          pxy[1].y=yb[j];
		          pxy[2].x=xb[i+1];
		          pxy[2].y=yb[j+1];
		         }
		       mm =m;
		       break;
		      }
		   }
	         if (m == ni)
		   {
		    if (mm != -1)
		      {
	   	       gsfaci(regis[mm].color);
		       proj_gfa(4,pxy);
		      }
		    pxy[0].x=xb[i+1];
		    pxy[0].y=yb[j];
		    pxy[3].x=xb[i+1];
		    pxy[3].y=yb[j+1];
		    pxy[1].x=xb[i+1];
		    pxy[1].y=yb[j];
		    pxy[2].x=xb[i+1];
		    pxy[2].y=yb[j+1];
		    mm=-1;
		   }
		}
	      else
		{
		 if (mm != -1 && mm != ni)
		   {
		    gsfaci(regis[mm].color);
		    proj_gfa(4,pxy);
		    pxy[0].x=xb[i];
		    pxy[0].y=yb[j];
		    pxy[3].x=xb[i];
		    pxy[3].y=yb[j+1];
		   }
		 pxy[1].x=xb[i+1];
		 pxy[1].y=yb[j];
		 pxy[2].x=xb[i+1];
		 pxy[2].y=yb[j+1];
		 mm=ni;
		}
/*								*/
	      if (pole && mm != -1)
		{
	         gsfaci(regis[mm].color);
	         proj_gfa(4,pxy);
		    pxy[0].x=xb[i+1];
		    pxy[0].y=yb[j];
		    pxy[3].x=xb[i+1];
		    pxy[3].y=yb[j+1];
		    pxy[1].x=xb[i+1];
		    pxy[1].y=yb[j];
		    pxy[2].x=xb[i+1];
		    pxy[2].y=yb[j+1];
		 mm=-1;
		}
	     }
	   if (mm != -1)
	     {
	      gsfaci(regis[mm].color);
	      proj_gfa(4,pxy);
	     }

	  }
	return 1;
      }


/*			Display the legend for boxfill.			*/

      int legendGfb(S_boxfill regis[],int num,struct p_tab *pP,
					struct display_tab *pD)

	{
	 int i,k,n,ni;
	 float v,vch,fi,dr,dx,dy,center,lev2,regis_store_1,regis_store_2;
	 struct pe_leg pe;
	 Gpoint pxy[4],sxy[9];
	 Gpoint vxy;
/*	 Gint *pws;
	 struct workstations *w;
*/
	
	 struct table_text *pTt;
	 struct table_chorn *pTo;
	 struct table_line *pTl;
         struct fill_range *pifr;
	 Gtxalign pta;
	 Gtxpath ptp;


         struct gfb_tab                  *ptgfb=NULL;
	 extern struct gfb_tab           *getGfb();

	 int horiz,l1,l2, ier;

	 ni=num;

	 if (ni <= 0)
	   {
	    err_warn(1,fperr,
			"Error - no fill ranges specified, so no legend.\n");
	    return 0;
	   }
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

/*	 if (strprt(pe.tb) == 0) return 0;				*/

	 for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
	    if (strcmp(pTt->name,pe.tb) == 0) break;
	 if (pTt == NULL) pTt=&Tt_tab;
	 for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
	    if (strcmp(pTo->name,pe.to) == 0) break;
	 if (pTo == NULL) pTo=&To_tab;
	 for (pTl=&Tl_tab; pTl != NULL; pTl=pTl->next)
	    if (strcmp(pTl->name,pe.ln) == 0) break;

	 horiz=0;
	 if (fabs(pe.x2-pe.x1) >= fabs(pe.y2-pe.y1)) horiz=1;

	 set_text_attr(pTt,pTo);

/*			Now take care of special case for legends:
			up-vector, alignment, path.			*/

	 v=pTo->chua;
	 while (v > 360.0) v=v-360.0;
	 while (v < 0.0) v=v+360.0;
	 /*DNW - 9/28/00 Allow rotation of legend test k=(v+45.0)/90.0;
	 v=k*90.0;*/
	   vxy.x=sin((3.1415926536/180.0)*v);
	   vxy.y=cos((3.1415926536/180.0)*v);
	 gschup(&vxy);
	   pta.hor=GTH_CENTRE;
	   pta.ver=GTV_HALF;
	 gstxal(&pta);
	   ptp=GTP_RIGHT;
	 gstxp(ptp);

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
	   }

/*		Find which legend labelling method.			*/
         ptgfb = getGfb(pD->g_name);

         /* For boxfill type 3--custom legends--, check for arrow extension. */
         if (ptgfb->pGfb_attr->boxfill_type == 3) {
            if (ptgfb->pGfb_attr->line->lev1 <= -0.9999e20) {ptgfb->pGfb_attr->ext_1 = 'y';} /* set ext_1 value */
            else {ptgfb->pGfb_attr->ext_1 = 'n';}

            pifr=ptgfb->pGfb_attr->line;
            while (pifr != NULL){ lev2 = pifr->lev2; pifr=pifr->next; }
            if (lev2 >= 0.9999e20) {ptgfb->pGfb_attr->ext_2 = 'y';} /* set ext_2 value */
            else {ptgfb->pGfb_attr->ext_2 = 'n';}

            if (ptgfb->pGfb_attr->ext_1 == 'y') {regis_store_1 = regis[0].l1; regis[0].l1 = -1e20;}
            if (ptgfb->pGfb_attr->ext_2 == 'y') {regis_store_2 = regis[num-1].l2; regis[num-1].l2 = 1e20;}
         }

	 if ( (l1=(fabs(regis[0].l1)) > 0.99E20)) ni--;
	 if ( (l2=(fabs(regis[num-1].l2)) > 0.99E20)) ni--;

	 if (ni <= 0)
	   {
	    err_warn(1,fperr,
		"Error - only overflow fill ranges specified, so no legend.\n");
	    return 0;
	   }

/*		Compute skip interval for labelling the legend.		*/

/*	 for (sk=1,k=25;k > 1;k--)
	   {
	    if (ni == (ni/k)*k && (ni/k) < 15 && (ni/k) > 5) break;
	    else if (ni/k > 5 && ni/k < 15)
	      {
	       if (sk%5 != 0 && sk%2 != 0) sk=k;
	      }
	   }
	 if (k > 1) sk=k;
*/


	 gsfais(GSOLID);

/*			Draw the legend.				*/

	 if (horiz)
	   {	    
	    sxy[0].x=pe.x2;
	    sxy[0].y=pe.y2;
	    sxy[1].x=pe.x2;
	    sxy[1].y=pe.y2;
	    sxy[2].x=pe.x1;
	    sxy[2].y=pe.y2;
	    sxy[3].x=pe.x1;
	    sxy[3].y=pe.y2;
	    sxy[4].x=pe.x1;
	    sxy[4].y=pe.y1;
	    sxy[5].x=pe.x1;
	    sxy[5].y=pe.y1;
	    sxy[6].x=pe.x2;
	    sxy[6].y=pe.y1;
	    sxy[7].x=pe.x2;
	    sxy[7].y=pe.y1;
	    sxy[8].x=pe.x2;
	    sxy[8].y=pe.y2;

	    pta.hor=GTH_CENTRE;
	    pta.ver=GTV_TOP;
	    vch=pe.y1-0.5*pTo->chh;
	    if (pe.y2 < pe.y1)
	      {
	       pta.ver=GTV_BOTTOM;
	       vch=pe.y1+0.5*pTo->chh;
	      }
	    gstxal(&pta);
	    if (l1)
	      {
	       pxy[0].x=pe.x1;
	       pxy[0].y=pe.y1;
               if ((pe.x2-pe.x1) >= 0)
	          pxy[1].x=pe.x1-0.02;
	       else
	          pxy[1].x=pe.x1+0.02;
	       pxy[1].y=0.5*(pe.y1+pe.y2);
	       pxy[2].x=pxy[1].x;
	       pxy[2].y=pxy[1].y;
	       pxy[3].x=pxy[0].x;
	       pxy[3].y=pe.y2;

	       sxy[3].y=pxy[1].y;
	       sxy[3].x=pxy[1].x;
	       sxy[4].y=pxy[1].y;
	       sxy[4].x=pxy[1].x;

	       gsfaci(regis[0].color);
	       gfa(4,pxy);

	      }
	    for (i=0,fi=0.0,n=l1;n < num-l2;i++,n++)
	      {
	       pxy[0].x=pe.x1+((fi+1.0)/ni)*(pe.x2-pe.x1);
	       pxy[0].y=pe.y1;
	       pxy[1].x=pe.x1+(fi/ni)*(pe.x2-pe.x1);
	       pxy[1].y=pe.y1;  
	       pxy[2].x=pxy[1].x;
	       pxy[2].y=pe.y2;
	       pxy[3].x=pxy[0].x;
	       pxy[3].y=pe.y2;

	       gsfaci(regis[n].color);
	       gfa(4,pxy);

	       fi=fi+1.0;
	      }
	    if (l2)
	      {
               if ((pe.x2-pe.x1) >= 0)
	          pxy[0].x=pe.x2+0.02;
               else
	          pxy[0].x=pe.x2-0.02;
	       pxy[0].y=0.5*(pe.y1+pe.y2);
	       pxy[1].x=pe.x2;
	       pxy[1].y=pe.y1;  
	       pxy[2].x=pe.x2;
	       pxy[2].y=pe.y2;
	       pxy[3].x=pxy[0].x;
	       pxy[3].y=pxy[0].y;

	       gsfaci(regis[num-1].color);
	       gfa(4,pxy);

	       sxy[0].y=pxy[0].y;
	       sxy[0].x=pxy[0].x;
	       sxy[7].y=pxy[0].y;
	       sxy[7].x=pxy[0].x;
	       sxy[8].y=pxy[0].y;
	       sxy[8].x=pxy[0].x;
	      }

	    if (pTl != NULL)
	      {
	       gpl(9,sxy);
	      }
           }

	 else			/*		Not horizontal.		*/

	   {
	    sxy[0].x=pe.x1;
	    sxy[0].y=pe.y2;
	    sxy[1].x=pe.x1;
	    sxy[1].y=pe.y2;
	    sxy[2].x=pe.x1;
	    sxy[2].y=pe.y1;
	    sxy[3].x=pe.x1;
	    sxy[3].y=pe.y1;
	    sxy[4].x=pe.x2;
	    sxy[4].y=pe.y1;
	    sxy[5].x=pe.x2;
	    sxy[5].y=pe.y1;
	    sxy[6].x=pe.x2;
	    sxy[6].y=pe.y2;
	    sxy[7].x=pe.x2;
	    sxy[7].y=pe.y2;
	    sxy[8].x=pe.x1;
	    sxy[8].y=pe.y2;

	    pta.hor=GTH_LEFT;
	    pta.ver=GTV_HALF;
	    vch=pe.x2+pTo->chh;
	    if (pe.x2 < pe.x1)
	      {
	       pta.hor=GTH_RIGHT;
	       vch=pe.x2-pTo->chh;
	      }
	    gstxal(&pta);

	    if (l1)
	      {
	       pxy[0].y=pe.y1;
	       pxy[0].x=pe.x1;
	       if ((pe.y2-pe.y1) >= 0)
	          pxy[1].y=pe.y1-0.02;
               else
	          pxy[1].y=pe.y1+0.02;
	       pxy[1].x=0.5*(pe.x1+pe.x2);
	       pxy[2].y=pxy[1].y;
	       pxy[2].x=pxy[1].x;
	       pxy[3].y=pxy[0].y;
	       pxy[3].x=pe.x2;

	       sxy[3].x=pxy[1].x;
	       sxy[3].y=pxy[1].y;
	       sxy[4].x=pxy[1].x;
	       sxy[4].y=pxy[1].y;

	       gsfaci(regis[0].color);
	       gfa(4,pxy);
	      }

	    for (i=0,fi=0.0,n=l1;n < num-l2;i++,n++)
	      {
	       pxy[0].y=pe.y1+((fi+1.0)/ni)*(pe.y2-pe.y1);
	       pxy[0].x=pe.x1;
	       pxy[1].y=pe.y1+(fi/ni)*(pe.y2-pe.y1);
	       pxy[1].x=pe.x1;  
	       pxy[2].y=pxy[1].y;
	       pxy[2].x=pe.x2;
	       pxy[3].y=pxy[0].y;
	       pxy[3].x=pe.x2;

	       gsfaci(regis[n].color);
	       gfa(4,pxy);
	       fi=fi+1.0;
	      }
	    if (l2)
	      {
	       if ((pe.y2-pe.y1) >= 0)
	          pxy[0].y=pe.y2+0.02;
               else
	          pxy[0].y=pe.y2-0.02;
	       pxy[0].x=0.5*(pe.x1+pe.x2);
	       pxy[1].y=pe.y2;
	       pxy[1].x=pe.x1;  
	       pxy[2].y=pe.y2;
	       pxy[2].x=pe.x2;
	       pxy[3].y=pxy[0].y;
	       pxy[3].x=pxy[0].x;

	       gsfaci(regis[num-1].color);
	       gfa(4,pxy);

	       sxy[0].y=pxy[0].y;
	       sxy[0].x=pxy[0].x;
	       sxy[7].y=pxy[0].y;
	       sxy[7].x=pxy[0].x;
	       sxy[8].y=pxy[0].y;
	       sxy[8].x=pxy[0].x;

	      }

	    if (pTl != NULL)
	      {
	       gpl(9,sxy);
	      }
	   }

/*      Use the array list of values to compute the legend values.	*/
	     if ( (ptgfb->pGfb_attr->boxfill_type == 0) && 
		  ((ptgfb->pGfb_attr->legend != NULL) && ptgfb->pGfb_attr->legend[0] != '\0') )
	       ier = list_compute_legend_values(horiz, regis, pe, vch, pTl,
                              num, l1, l2, ptgfb->pGfb_attr->legend, ptgfb->pGfb_attr->boxfill_type,
			      ptgfb->pGfb_attr->ext_1,ptgfb->pGfb_attr->ext_2, ptgfb->pGfb_attr->line);
/* 		VCS will compute the legend values.			*/
	     else if ((ptgfb->pGfb_attr->boxfill_type == 0) ||
	         (ptgfb->pGfb_attr->boxfill_type == 2))
	       ier = vcs_legacy_compute_legend_values(horiz, regis, pe, vch, pTl,
			     num, l1, l2, ptgfb->pGfb_attr->boxfill_type);
	     else if    (ptgfb->pGfb_attr->boxfill_type == 3)
               if (ptgfb->pGfb_attr->ext_1 == 'y') regis[0].l1 = regis_store_1;   /* restore the regis values */
               if (ptgfb->pGfb_attr->ext_2 == 'y') regis[num-1].l2 = regis_store_2;/* restore the regis values */
	       ier = list_compute_legend_values(horiz, regis, pe, vch, pTl,
                              num, l1, l2, ptgfb->pGfb_attr->legend, ptgfb->pGfb_attr->boxfill_type,
			      ptgfb->pGfb_attr->ext_1,ptgfb->pGfb_attr->ext_2, ptgfb->pGfb_attr->line);

	     /*
	     else if (ptgfb->pGfb_attr->boxfill_type == 1)
*	Use number of points and start and end to compute the legend values. *
	       ier = pts_compute_legend_values(horiz, regis, pe, vch, pTl,
                             num, l1, l2, ptgfb->pGfb_attr->legend);
	      */
	 return ier;
	}

/*		Compute boxfill ranges and colors.			*/

    int set_bfills(struct gfb_attr *pGfb,struct a_attr *pA,
						S_boxfill *regis,int *num)
      {
	int k,k1,k2,ki,c1,c2,n,np;
	int e1,e2;
	float lev1,lev2,dr,del;
	float center;


	n=0;

	c1=(pGfb->color_1<0)?0:( (pGfb->color_1>255)?255:pGfb->color_1);
	c2=(pGfb->color_2<0)?0:( (pGfb->color_2>255)?255:pGfb->color_2);
	ki=1;
	if (c2 < c1) ki=-1;
	e1=pGfb->ext_1;
	e2=pGfb->ext_2;
	lev1=pGfb->lev1;
	lev2=pGfb->lev2;
	k1=0;
	k2=(c2-c1);
	if (e1 == 'y') k1+=ki;
	else e1='n';
	if (e2 == 'y') k2-=ki;
	else e2='n';
	dr=(lev2-lev1)/(k2-k1+ki);

/*		If no boxfill ranges specified then compute an interval
		and assume reference at 0.0.				*/

	if (pGfb->lev1>=.99e20 || pGfb->lev2>=.99e20)
	  {
	   dr=(pA->max - pA->min)/224.0;
	   if (dr <= 1.e-20)
	     {
	      del=1.e10;
	      lev1=-del;
	      lev2=+del;
	      e1=e2='n';
	      k1=0;
	      k2=1;
	     }
	   else
	     {
	      nicedf(pA->min,pA->max,&del,&np,&center);

/*		    np=log10(fabs((double)dr));
		    del=(int) (dr*pow(10.0,(double) -np)+0.5);
		    dr=del*pow(10.0,(double) np);
*/
	      dr=del*pow(10.0,(double) np);
	      k1=pA->min/dr;
	      if (k1*dr > pA->min) k1--;
	      k2=pA->max/dr;
	      if (k2*dr < pA->max) k2++;
	      lev1=k1*dr;
	      lev2=k2*dr;
/*	         c1=16;
	         c2=239;
*/
	      e1=e2='n';
	      k2=c2-c1;
	      k1=0;
	     }
	  }
	ki=1;
	if (k2 < k1) ki=-1;
	dr=(lev2-lev1)/(k2-k1+ki);
	n=0;
	if (e1 == 'y')
	  {
	   if (lev1 <= lev2) regis[n].l1=-1.e20;
	   else regis[n].l1=1.e20;
	   regis[n].l2=lev1;
	   regis[n].color=c1;
	   n=1;
	  }
	for (k=k1;k != k2+ki;k+=ki)
	  {
	   regis[n].color=c1+k;
	   regis[n].l1=lev1+(k-k1)*dr;
	   regis[n].l2=lev1+(k+1-k1)*dr;
	   n++;
	  }
	if (e2 == 'y')
	  {
	   if (lev2 < lev1) regis[n].l2=-1.e20;
	   else regis[n].l2=1.e20;
	   regis[n].color=c2;
	   regis[n].l1=lev2;
	   n++;
	  }
	regis[n].color=pGfb->missing;

	*num=n;
	return 1;
      }

/* 		VCS will compute the legend values			*/
int pts_compute_legend_values(int horiz, S_boxfill *regis, struct pe_leg pe, 
                              float vch, struct table_line *pTl, int num,
			      int l1, int l2, char * legend)
{
	char str[256], *leg_str;
	float dr, dx, dy, center;
	int pw10, k, k1, k2, ki;

	int i=0,j=0,b=0,ct=0,number,is_a_num;
	float start, end, diff, *legend_nums;
        struct l_tab *ptab;
	struct l_val *vtab;
	extern struct l_tab L_tab[2];

	float temp, max;

	if (legend == NULL) {
           err_warn(1,fperr, "Error - empty string for legend points.\n");
           return 1;
        }
/*		Check to see if it is a List or a string of numbers.	*/
        ptab=&L_tab[0];
        while ((ptab != NULL) && (strcmp(ptab->name,legend) != 0))
           ptab=ptab->next;

	if (ptab == NULL) {
	   if ((leg_str=(char *)malloc((strlen(legend)+1)*sizeof(char) +1))==
                NULL)
	   {
                err_warn(1,fperr, "Error - memory overflow in legend axis.\n");
                return 1;
           }
	   strcpy(leg_str, legend);
	   strcat(leg_str, " ");
	   while (leg_str[i] != '\0') {
              if ( (isspace(leg_str[i]) == 0) && (leg_str[i] != ',') ) {
                 str[j] = leg_str[i];
                 ++j;
                 b=1;
              } else {
                if (b) {
                   ++ct;
                   str[j] = '\0';
                   is_a_num = isnum(str);
                   if (is_a_num) {
                      if (ct==1) number = atoi(str);
		      else if (ct==2) start = (float)atof(str);
		      else if (ct==3) {end = (float)atof(str); break;}
                   } else {
                     err_warn(1,fperr,
                         "Error - Legend value is not a number.\n");
		     return 1;
                   }
                   j=0;
	        }
                b=0;
	      }
              ++i;
	   }
	   free((char *) leg_str);
        } else {
		/*
	   vtab = ptab->val;
	   while (vtab != NULL) {
              ++ct;
              if (ct==1) number = vtab->data;
	      else if (ct==2) start = vtab->data;
	      else if (ct==3) {end = vtab->data; break;}
              vtab = vtab->next;
	   }

	   */
           number = ptab->count;
	}
/*		Create the legend list numbers.			*/
	if ((legend_nums=(float *)malloc((number+1)*sizeof(float)))==NULL) {
             err_warn(1,fperr, "Error - memory overflow in legend axis.\n");
             return 1;
        }
        number = ptab->count;
	vtab = ptab->val;
	for (i = 0; vtab != NULL; i++, vtab = vtab->next)
              legend_nums[i] = vtab->data;
	for (i = 1; i < number; i++) {
	   temp = legend_nums[i];
	   for (j=i-1; j >= 0 && legend_nums[j] > temp; j --)
	      legend_nums[j+1] = legend_nums[j];
	   legend_nums[j+1] = temp;
	   }
	max = legend_nums[(number-1)] - legend_nums[0];
	for (i = 0; i < number; i++) {
	   vtab = ptab->val;
	   ct = 0;
	   while (vtab != NULL) {
	      temp = (float) atof( vtab->str );
	      if (temp == legend_nums[i]) {
		  vtab->data = max*ct/(number-1);
		  sprintf(vtab->str,"%g",vtab->data);
		  printf("ct = %d and data = %g and str = %s\n",ct, vtab->data, vtab->str);
		  break;
	      }
	      ct+=1;
	      vtab = vtab->next;
	   }
	}
	/*
	if (ct==1) {
             err_warn(1,fperr, "Error - missing start and end points needed to produce legend.\n");
             return 1;
	} else if (ct==2) {
           legend_nums[0] = start;
	} else {
           legend_nums[0] = start;
	   diff = (end - start)/number;
	   for (i=1; i<number; ++i)
               legend_nums[i] = start + i*diff;
           legend_nums[number] = end;
        }

                            (number+1), legend_nums, NULL);
	*/

	show_legend_numbers(horiz, regis, pe, vch, pTl, num, l1, l2,
			    number, legend_nums, ptab->val);

	free((char *) legend_nums);

	return 1;
}

/* 		VCS will compute the legend values			*/
int list_compute_legend_values(int horiz, S_boxfill *regis, struct pe_leg pe, 
                              float vch, struct table_line *pTl, int num,
			      int l1, int l2, char * legend, int boxfill_type,
			      int ext_1, int ext_2, struct fill_range *values)
{
	 char str[256], *leg_str;
	 float dr, dx, dy, center;
	 int pw10, k, k1, k2, ki;

        int i=0,j=0,b=0,ct=0,number,is_a_num;
        float *legend_nums,  *str_values;
        struct l_tab *ptab=NULL;
	struct l_val *vtab,*hvtab,*pvtab;
        extern struct l_tab L_tab[2];

	char value_str[1024];
	float temp, omax, max;
	struct fill_range *pfiso;

	if (boxfill_type == 0) {
	   if (legend == NULL) {
              err_warn(1,fperr, "Error - empty string for legend list.\n");
              return 1;
           }
   /*              Check to see if it is a List or a string of numbers.    */
           ptab=&L_tab[0];
           while ((ptab != NULL) && (strcmp(ptab->name,legend) != 0))
              ptab=ptab->next;
	   if (ptab != NULL) hvtab = ptab->val;
	} else {     /* Do this for custom */
           if ((vtab=(struct l_val *) malloc(sizeof(*vtab))) == NULL) {
                err_warn(1,fperr, "Error - can't allocate memory for new VCS list!\n");
                return 1;
           }
	   vtab->data = (float) values->lev1;
	   sprintf(value_str,"%f", vtab->data);
	   vtab->str = (char *) malloc(strlen(value_str) * sizeof(char) + 1);
	   strcpy(vtab->str, value_str);
	   vtab->next = NULL;
	   hvtab = pvtab = vtab;
	   for (number = 1, pfiso = values; pfiso != NULL; number++, pfiso = pfiso->next) {
              if ((vtab=(struct l_val *) malloc(sizeof(*vtab))) == NULL) {
                   err_warn(1,fperr, "Error - can't allocate memory for new VCS list!\n");
                   return 1;
              }
	      vtab->data = (float) pfiso->lev2;
	      sprintf(value_str,"%g", vtab->data);
	      vtab->str = (char *) malloc(strlen(value_str) * sizeof(char) + 1);
	      strcpy(vtab->str, value_str);
	      vtab->next = NULL;
	      pvtab->next = vtab;
	      pvtab = vtab;
	   }
	}

        if ( (boxfill_type == 0) && (ptab == NULL) ) {
           if ((leg_str=(char *)malloc((strlen(legend)+1)*sizeof(char) +1))==
                NULL)
           {
                err_warn(1,fperr, "Error - memory overflow in legend axis.\n");
                return 1;
           }
           strcpy(leg_str, legend);
           strcat(leg_str, " ");

	   number = get_number_of_values(leg_str);

           if ((legend_nums=(float *)malloc((number+1)*sizeof(float)))==NULL) {
             err_warn(1,fperr, "Error - memory overflow in legend axis.\n");
             return 1;
           }
           while (leg_str[i] != '\0') {
              if ( (isspace(leg_str[i]) == 0) && (leg_str[i] != ',') ) {
                   str[j] = leg_str[i];
                   ++j;
                   b=1;
              } else {
                   if (b) {
                      str[j] = '\0';
                      is_a_num = isnum(str);
                      if (is_a_num) {
                         legend_nums[ct] = (float)atof(str); 
                         ++ct;
                      }
                      j=0;
                   }
                   b=0;
              }
              ++i;
           }
	   free((char *) leg_str);
        } else {
	  if (boxfill_type == 0) number = ptab->count;

          if ((legend_nums=(float *)malloc((number+1)*sizeof(float)))==NULL) {
             err_warn(1,fperr, "Error - memory overflow in legend axis.\n");
             return 1;
          }
          if ((str_values=(float *)malloc((number)*sizeof(float)))==NULL) {
             err_warn(1,fperr, "Error - memory overflow in legend axis.\n");
             return 1;
          }
           
	  for (i = 0,vtab=hvtab; vtab != NULL; i++, vtab = vtab->next)
              legend_nums[i] = vtab->data;

	  if (boxfill_type == 3) { /* Do this for custom legends */
	     for (i = 1; i < number; i++) { /* Sort the legend values */
	        temp = legend_nums[i];
	        for (j=i-1; j >= 0 && legend_nums[j] > temp; j --)
	           legend_nums[j+1] = legend_nums[j];
	        legend_nums[j+1] = temp;
	     }

             /* Remove the extension values. */
             if ((ext_1 == 'y') && (ext_2 == 'y')) {
                for (i = 0; i < number-1; i++) { /* Remove -1e20 and 1e20 */
                    legend_nums[i] = legend_nums[i+1];
                }
             }

	     omax = (regis[num-1].l2 - regis[0].l1);
	     max = legend_nums[(number-1)] - legend_nums[0];
	     for (i = 0; i < number; i++) {
	        vtab = hvtab;
	        ct = 0;
	        while (vtab != NULL) {
	           temp = (float) atof( vtab->str );
	           if (temp == legend_nums[i]) {
	              vtab->data = regis[0].l1 + (max*ct/(number-1)*omax/max);
                      if ((ext_1 == 'y') && (ext_2 == 'n')) str_values[i] = legend_nums[i+1]; /* offset legend numbers */
                      else str_values[i] = legend_nums[i];
/* DEBUG       printf("ct = %d and data = %g and str = %s and gstr = %g\n",ct, vtab->data, vtab->str, str_values[i]);*/
	              break;
	           }
	           ct+=1;
	           vtab = vtab->next;
	        }
	     }
   
	     for (i = 0, vtab=hvtab; vtab != NULL; i++, vtab = vtab->next)
                sprintf(vtab->str, "%g", str_values[i]);
   
	  }

	  for (i = 0, vtab=hvtab; vtab != NULL; i++, vtab = vtab->next)
              legend_nums[i] = vtab->data;

	  free((char *) str_values);
	}

	show_legend_numbers(horiz, regis, pe, vch, pTl, num, l1, l2,
			    number, legend_nums, hvtab);

	free((char *) legend_nums);

	return 1;
}

void show_legend_numbers(int horiz, S_boxfill *regis, struct pe_leg pe,
                              float vch, struct table_line *pTl, int num,
                              int l1, int l2, int number, float *legend_nums,
                              struct l_val *vtab)
{
	Gpoint pxy[4], gxy;
 	int k;
	char str[256];
        double a,b,c;

	if (horiz)
   	   {	    
	       for (k = 0; k < number; ++k)
		 {
		  if ((legend_nums[k] >= regis[0+l1].l1) &&
                     (legend_nums[k] <= regis[num-1-l2].l2))
		    {
                     if (vtab != NULL) {
		        sprintf(str,"%s",vtab->str);
                        vtab = vtab->next;
                     } else 
                        sprintf(str,"%g",legend_nums[k]);
		     gxy.y=vch;
                     a = (legend_nums[k]-regis[0+l1].l1);
                     b = (regis[num-1-l2].l2-regis[0+l1].l1);
                     c = (pe.x2-pe.x1);
                     if (b != 0)
		        gxy.x=pe.x1+( a/b*c );
                     else
                        gxy.x=(pe.x2+pe.x1)/2;
		     gtx(&gxy,(unsigned char *)str);
		     if (pTl != NULL)
		       {
			pxy[0].x=gxy.x;
		        pxy[0].y=pe.y2;
			pxy[1].x=pxy[0].x;
			pxy[1].y=pe.y1;
		        gpl(2,pxy);
		       }
		    } else {
                      vtab = vtab->next;
                    }
		 }
	   }

	 else			/*		Not horizontal.		*/

	   {
	       for (k = 0; k < number; ++k)
		 {
		  if ((legend_nums[k] >= regis[0+l1].l1) &&
                     (legend_nums[k] <= regis[num-1-l2].l2))
		    {
                     if (vtab != NULL) {
		        sprintf(str,"%s",vtab->str);
                        vtab = vtab->next;
                     } else 
                        sprintf(str,"%g",legend_nums[k]);
	              gxy.x=vch;
                      a = (legend_nums[k]-regis[0+l1].l1);
                      b = (regis[num-1-l2].l2-regis[0+l1].l1);
                      c = (pe.y2-pe.y1);
                      if (b != 0)
                         gxy.y=pe.y1+( a/b*c );
                      else
                         gxy.y=(pe.y2+pe.y1)/2;
	              gtx(&gxy,(unsigned char *)str);
	              if (pTl != NULL)
	                {
			 pxy[0].y=gxy.y;
		         pxy[0].x=pe.x2;
			 pxy[1].y=pxy[0].y;
			 pxy[1].x=pe.x1;
	                 gpl(2,pxy);
	                }
                    }
		 }
	   }
}

/* 		VCS will compute the legend values			*/
int vcs_legacy_compute_legend_values(int horiz, S_boxfill *regis, struct pe_leg pe, 
                              float vch, struct table_line *pTl, int num,
			      int l1, int l2, int boxfill_type)
{
	 char str1[256];
	 Gpoint pxy[4], gxy, sxy[9];
	 float dr, dx, dy, center;
	 int pw10, k, k1, k2, ki;

	 if (horiz)
	   {	    
	    sprintf(str1,"%g",regis[0+l1].l1);
            if (boxfill_type == 2) sprintf(str1, "%g", pow(10,regis[0+l1].l1));
	    gxy.x=pe.x1;
	    gxy.y=vch;
	    gtx(&gxy,(unsigned char *)str1);
	    if (pTl != NULL)
	      {
	       pxy[0].y=pe.y2;
	       pxy[0].x=pe.x1;
	       pxy[1].y=pe.y1;
	       pxy[1].x=pe.x1;
	       gpl(2,pxy);
	      }

	    if (nicedf(regis[0+l1].l1,regis[num-1-l2].l2,&dr,&pw10,&center))
	      {
	       dx=dr*pow(10.0,(double)pw10);
	       k1=regis[0+l1].l1/dx;
	       k2=regis[num-1-l2].l2/dx;
	       if (k2 < k1)
		 {
		  ki=-1;
		  if (k1*dx >= regis[0+l1].l1) k1--;
		  if (k2*dx <= regis[num-1-l2].l2) k2++;
		  if (k2 > k1) k2=k1=ki=0;
		 }
	       else
		 {
		  ki=1;
		  if (k1*dx <= regis[0+l1].l1) k1++;
		  if (k2*dx >= regis[num-1-l2].l2) k2--;
		  if (k2 < k1) k2=k1=ki=0;
		 }
	       
	       for (k=k1;k != k2+ki ;k+=ki)
		 {
		  if ((k*dx-regis[0+l1].l1)*(k*dx-regis[num-1-l2].l2) < 0)
		    {
		     sprintf(str1,"%g",k*dx);
		     if (boxfill_type == 2) sprintf(str1, "%g", pow(10,k*dx));
		     gxy.y=vch;
		     gxy.x=pe.x1+
			(k*dx-regis[0+l1].l1)/
			(regis[num-1-l2].l2-regis[0+l1].l1)*
			(pe.x2-pe.x1);
		     gtx(&gxy,(unsigned char *)str1);
		     if (pTl != NULL)
		       {
			pxy[0].x=gxy.x;
		        pxy[0].y=pe.y2;
			pxy[1].x=pxy[0].x;
			pxy[1].y=pe.y1;
		        gpl(2,pxy);
		       }
		    }
		 }
	      }
	    sprintf(str1,"%g",regis[num-1-l2].l2);
            if (boxfill_type == 2) sprintf(str1, "%g", pow(10,regis[num-1-l2].l2));
	    gxy.x=pe.x2;
	    gxy.y=vch;
	    gtx(&gxy,(unsigned char *)str1);
	    if (pTl != NULL)
	      {
	       pxy[0].y=pe.y2;
	       pxy[0].x=pe.x2;
	       pxy[1].y=pe.y1;
	       pxy[1].x=pe.x2;
	       gpl(2,pxy);
	      }

	   }

	 else			/*		Not horizontal.		*/

	   {
	    sprintf(str1,"%g",regis[0+l1].l1);
            if (boxfill_type == 2) sprintf(str1, "%g", pow(10,regis[0+l1].l1));
	    gxy.x=vch;
	    gxy.y=pe.y1;
	    gtx(&gxy,(unsigned char *)str1);
	    if (pTl != NULL)
	      {
	       pxy[0].y=pe.y1;
	       pxy[0].x=pe.x2;
	       pxy[1].y=pe.y1;
	       pxy[1].x=pe.x1;
	       gpl(2,pxy);
	      }

	    if (nicedf(regis[0+l1].l1,regis[num-1-l2].l2,&dr,&pw10,&center))
	      {
	       dy=dr*pow(10.0,(double)pw10);
	       k1=regis[0+l1].l1/dy;
	       k2=regis[num-1-l2].l2/dy;
	       if (k2 < k1)
		 {
		  ki=-1;
		  if (k1*dy >= regis[0+l1].l1) k1--;
		  if (k2*dy <= regis[num-1-l2].l2) k2++;
		  if (k2 > k1) k2=k1=ki=0;
		 }
	       else
		 {
		  ki=1;
		  if (k1*dy <= regis[0+l1].l1) k1++;
		  if (k2*dy >= regis[num-1-l2].l2) k2--;
		  if (k2 < k1) k2=k1=ki=0;
		 }
	       
	       for (k=k1;k != k2+ki ;k+=ki)
		 {
		  if ((k*dy-regis[0+l1].l1)*(k*dy-regis[num-1-l2].l2) < 0)
		    {
		     sprintf(str1,"%g",k*dy);
                     if (boxfill_type == 2) sprintf(str1, "%g", pow(10,k*dy));
		     gxy.x=vch;
		     gxy.y=pe.y1+
			(k*dy-regis[0+l1].l1)/
			(regis[num-1-l2].l2-regis[0+l1].l1)*
			(pe.y2-pe.y1);
		     gtx(&gxy,(unsigned char *)str1);
		     if (pTl != NULL)
		       {
			pxy[0].y=gxy.y;
		        pxy[0].x=pe.x2;
			pxy[1].y=pxy[0].y;
			pxy[1].x=pe.x1;
		        gpl(2,pxy);
		       }
		    }
		 }
	      }
	    sprintf(str1,"%g",regis[num-1-l2].l2);
            if (boxfill_type == 2) sprintf(str1, "%g", pow(10,regis[num-1-l2].l2));
	    gxy.y=pe.y2;
	    gxy.x=vch;
	    gtx(&gxy,(unsigned char *)str1);
	    if (pTl != NULL)
	      {
	       pxy[0].y=pe.y2;
	       pxy[0].x=pe.x2;
	       pxy[1].y=pe.y2;
	       pxy[1].x=pe.x1;
	       gpl(2,pxy);
	      }
	   }

	return 1;
}

int get_number_of_values(char * legend)
{
	int i=0,j=0,b=0,ct=0,is_a_num;
	char str[256];

        while (legend[i] != '\0') {
           if ( (isspace(legend[i]) == 0) && (legend[i] != ',') ) {
                str[j] = legend[i];
                ++j;
                b=1;
           } else {
                if (b) {
                   str[j] = '\0';
                   is_a_num = isnum(str);
                   if (is_a_num)
                      ++ct;
                   j=0;
                }
                b=0;
           }
           ++i;
        }

	return (ct);
}
