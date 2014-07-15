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
extern struct table_fill Tf_tab;
extern struct table_pattern Pat_tab;

extern struct display_tab D_tab;

extern struct projection_attr p_PRJ_list;
extern struct project_attr p_PRJ;

extern struct default_continents Dc;

extern char PRJ_names[PRJ_TYPES][17];

extern int segment_num;

int nice15(float a,float b,float *dr,int *pw10,float *center);

extern int IN[14][2][2],JN[14][2][2];

/*		Table of increments in i and j for isoline following.	*/

/*    int IN[14][2][2] = {
      0,0,0,1, 0,1,1,1, 0,0,1,1, 1,1,0,1, 0,0,0,1,
      0,1,0,1, 0,0,0,1, 0,1,0,0, 0,1,0,1, 0,1,0,0,
      0,1,1,1, 1,1,0,0, 1,1,0,1, 0,1,0,0
      };
      int JN[14][2][2] = {
      0,1,0,0, 0,0,0,1, 0,1,0,1, 0,1,1,1, 0,1,1,1,
      0,0,1,1, 0,1,1,1, 1,1,0,1, 1,1,0,0, 0,0,0,1,
      1,1,0,1, 0,1,0,1, 0,1,0,0, 0,0,0,1
      };						*/

    extern int generic_world (float x1,float y1,float x2,float y2, char *lb,
                               char *trans_xaxis, char *trans_yaxis);
    extern int generic_world2 (float x1,float y1,float x2,float y2, char *lb,
                               char *trans_xaxis, char *trans_yaxis);

/*
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
/*		Globals for isofill.					*/

float *A;	/* Array of values.				*/
short *mask;	/* Dual valued mask for (min, max) values.	*/
int IM;		/* First dimension size for A and mask.		*/
int JM;		/* Second dimension size for A and mask.	*/
float *xv;	/* Vector of values for first dimension.	*/
float *yv;	/* Vector of values for second dimension.	*/
Gpoint xyu[NMAX+12];/* Vector of upper line values.		*/
Gpoint xyl[NMAX+12];/* Vector of lower line values.		*/
int nxyu;	/* Number of values in xyu.			*/
int nxyl;	/* Number of values in xyl.			*/

void missing_background(im,jm,x,y,pGfi)
 	int im,jm;
	float *x;
	float *y;
        struct gfi_attr *pGfi;
{
        float x1, x2, y1, y2;
	Gpoint pxy[5];
/*			Set up the fill area definitions.		*/
	gsfaci( (int) pGfi->missing );
	gsfais( GSOLID );

        x1 = x[0]; if (pGfi->dsp[0] < 1.e20) x1 = pGfi->dsp[0];
        y1 = y[0]; if (pGfi->dsp[1] < 1.e20) y1 = pGfi->dsp[1];
        x2 = x[im-1]; if (pGfi->dsp[2] < 1.e20) x2 = pGfi->dsp[2];
        y2 = y[jm-1]; if (pGfi->dsp[3] < 1.e20) y2 = pGfi->dsp[3];

        pxy[0].x=x1;
        pxy[0].y=y1;
        pxy[1].x=x2;
        pxy[1].y=y1;
        pxy[2].x=x2;
        pxy[2].y=y2;
        pxy[3].x=x1;
        pxy[3].y=y2;
        pxy[4].x=x1;
        pxy[4].y=y1;

/*			Plot missing background				*/
	proj_gfa(5,pxy);
}

int isofills (pD,pGfi,pP,ptab)
	
	struct display_tab *pD;
	struct gfi_attr *pGfi;
	struct p_tab *pP;
	struct a_tab *ptab;
	
{
	int i,j,n,erret;
	int k1,k2;
	float dr,del,center;
	float dsp[NDS];
	struct a_attr *pA;
	struct table_fill *pt,*ptt;
	struct fill_range *pl,*p1,*pl1,*plg;
	struct project_attr *pj;
	struct projection_attr *prj;
	Glimit pvp;
	
	int ND;
	int save_xs[NDS];
	float save_xl[NDS],save_xf[NDS];
        float X1,Y1,X2,Y2; /*   generic_world coordinate values */
	char proj[17],tmpchar[256];

        int l,tmp;
        float *save_xv_0, *save_xv_1;
        int did_save_x=0, did_save_y=0;
        extern int trans_axis();
	int savecontinents;
	
        struct l_tab *pltab=NULL;
	extern struct l_tab L_tab[2];
	extern struct fill_range *generate_auto_fill_range(float min,float max);

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
	pl=plg=pGfi->line;
	
	for (ND=i=0;i<pA->ND;i++)
	{
		save_xs[i]=*pA->xs[i];
		save_xl[i]=*pA->xl[i];
		save_xf[i]=*pA->xf[i];
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
			 "Error - NDC display space is nil for ISOFILL.\n");
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
			 "Error - ISOFILL requires 2 multi-valued dimensions.\n");
		pD->off=2;
		return 0;
	}
	
	if (erret == 0 && ((pA->xs[2] != NULL && *(pA->xs[2]) > 1) ||
			   (pA->xs[3] != NULL && *(pA->xs[3]) > 1)  )  )
	{
		if (pA->f != NULL)
		{
			err_warn(0,fperr,
				 "Warning - ISOFILL is 2D. Taking a subset of (%dD) computed "
				 "variable.\n"
				 "        Modify dimensions of rhs: %s=%s\n",
				 ND,ptab->name,pA->f);
		/*	erret++;*/
		}
		else
		{
			err_warn(0,fperr,
				 "Warning - ISOFILL is 2D; (%s) is %dD.\n"
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
           if ( ((strcmp("linear",pGfi->xat) != 0) &&
                 (strcmp("",pGfi->xat) != 0)) &&
                 (strcmp("linear",pGfi->proj) == 0) ) {
              if ((save_xv_0=(float *)malloc(pA->xs[0][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(save_xv_0,pA->xv[0],pA->xs[0][0],
                            &pA->xf[0][0], &pA->xl[0][0], pGfi->xat,
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
           }

/*                              Transform the y coordinate axis.         */
           if ( ((strcmp("linear",pGfi->yat) != 0) &&
                 (strcmp("",pGfi->yat) != 0)) &&
                 (strcmp("linear",pGfi->proj) == 0) ) {
              if ((save_xv_1=(float *)malloc(pA->xs[1][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(save_xv_1,pA->xv[1],pA->xs[1][0],
                            &pA->xf[1][0], &pA->xl[1][0], pGfi->yat,
                            &did_save_y, "y") == 0) {
                       if (did_save_x) {
                          for (l = 0; l < pA->xs[0][0]; ++l)
                             pA->xv[0][l] = save_xv_0[l];
                          *pA->xf[0] = save_xf[0];
                          *pA->xl[0] = save_xl[0];
                          free((char *) save_xv_0);
                       }
   
                       if (did_save_y) {
                          for (l = 0; l < pA->xs[1][0]; ++l)
                             pA->xv[1][l] = save_xv_1[l];
                          *pA->xf[1] = save_xf[1];
                          *pA->xl[1] = save_xl[1];
                          free((char *) save_xv_1);
                       }
                       return 0;
                    }
                 }
           }

/*			First and last of first two dimension determines
			direction of the plot.				*/
	
	dsp[0]=*pA->xf[0];
	dsp[1]=*pA->xf[1];
	dsp[2]=*pA->xl[0];
	dsp[3]=*pA->xl[1];
	strcpy(proj,pGfi->proj);
	if (cmpncs(proj,"Mollweide")==0 || cmpncs(proj,"Robinson")==0 ||
	    cmpncs(proj,"Polar"    )==0 )
	{
		if (cmpncs(pA->XN[0],"longitude") != 0 ||
		    cmpncs(pA->XN[1],"latitude" ) != 0 )
		{
			err_warn(1,fperr,
				 "Error - (ISOFILLS) spherical projections work"
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

	if (set_projection(proj,pP->dsp,pGfi->dsp,dsp) == 0)
	{
		err_warn(1,fperr,"Error - in projection for ISOFILLS.\n");
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
			 "Error - ISOFILL data is outside the display range.\n");
		erret++;
	}

    /* HVO (commented the print)*/
/*     fprintf(stderr, "ERROR RETURN 3 %d!!\n", pD->dsp_seg[3]); */
	if (pD->dsp_seg[3] > 0 &&
        (pP->dsp.p > 0 && pP->dsp.x1 < 1.0 &&
	   pP->dsp.y1 < 1.0 && pP->dsp.x1 > 0.0 && pP->dsp.y1 > 0.0 &&
	   pP->dsp.x2 < 1.0 && pP->dsp.y2 < 1.0 && pP->dsp.x2 > 0.0 &&
	   pP->dsp.y2 > 0.0 ) )
	{
		
/*              If the variable isn't computed then restore excess dimensions.*/           if (did_save_x)
           {
              for (l = 0; l < pA->xs[0][0]; ++l)
                 pA->xv[0][l] = save_xv_0[l];
              *pA->xf[0] = save_xf[0];
              *pA->xl[0] = save_xl[0];
              free((char *) save_xv_0);
              did_save_x = 0;
           }

           if (did_save_y)
           {
              for (l = 0; l < pA->xs[1][0]; ++l)
                 pA->xv[1][l] = save_xv_1[l];
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
/*                      Compute the mean if from CDAT or Computed */
/*DNW - 8/12/04 - if (pA->f != NULL) - Always do this since f is now for the most part always NULL. */
           mmmm(pA->un.data,pA->mask,&pA->xs[0],&pA->xw[0],2,
                        &pA->XC[0],&pA->xv[0],&pA->min,&pA->max,&pA->mean);
	   if (pA->min==pA->max) {
		err_warn(1,fperr,
			 "Error - ISOFILL data min and max are identical.\n");
		erret++;
		return 1;
	}

	   //fprintf(stderr,"ok mean,mx,mn: %f, %f, %f\n",pA->mean,pA->min,pA->max);
           if ( ((strcmp("linear",pGfi->xat) != 0) &&
                 (strcmp("",pGfi->xat) != 0)) &&
                 (strcmp("linear",pGfi->proj) == 0) ) {
              if ((save_xv_0=(float *)malloc(pA->xs[0][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(save_xv_0,pA->xv[0],pA->xs[0][0],
                            &pA->xf[0][0], &pA->xl[0][0], pGfi->xat,
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
           }

/*                              Transform the y coordinate axis.         */
	   if ( ((strcmp("linear",pGfi->yat) != 0) &&
                 (strcmp("",pGfi->yat) != 0)) &&
                 (strcmp("linear",pGfi->proj) == 0) ) {
              if ((save_xv_1=(float *)malloc(pA->xs[1][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(save_xv_1,pA->xv[1],pA->xs[1][0],
                            &pA->xf[1][0], &pA->xl[1][0], pGfi->yat,
                            &did_save_y, "y") == 0) {
                       if (did_save_x) {
                          for (l = 0; l < pA->xs[0][0]; ++l)
                             pA->xv[0][l] = save_xv_0[l];
                          *pA->xf[0] = save_xf[0];
                          *pA->xl[0] = save_xl[0];
                          free((char *) save_xv_0);
                       }
   
                       if (did_save_y) {
                          for (l = 0; l < pA->xs[1][0]; ++l)
                             pA->xv[1][l] = save_xv_1[l];
                          *pA->xf[1] = save_xf[1];
                          *pA->xl[1] = save_xl[1];
                          free((char *) save_xv_1);
                       }
                       return 0;
                    }
                 }
           }
			
	   	IM=*pA->xs[0];
	  	JM=*pA->xs[1];
			
		if ((mask=(short *)malloc(2*IM*JM*sizeof(short))) == NULL)
		{
			err_warn(1,fperr,
				 "Error - memory for contour mask not found.\n");
			return 0;
		}
			
			/*		Set up the contouring mask from the data mask.		*/
			
		for (i=0,j=0;j < JM-1;)
		{
			if (*(pA->mask+j*IM+i) == 0 ||
			    *(pA->mask+j*IM+i+1) == 0 ||
			    *(pA->mask+(j+1)*IM+i) == 0 ||
			    *(pA->mask+(j+1)*IM+i+1) == 0 )
				mask[2*(j*IM+i)+1]=mask[2*(j*IM+i)] = -1;
			else
				mask[2*(j*IM+i)+1]=mask[2*(j*IM+i)] = 0;
			if (++i == IM-1) { i=0; j++;}
		} /* end for i & j loop  */
		
/*		If no isofill ranges specified then compute an interval
		and assume reference at 0.0.				*/
			
		if (pl == NULL || (pl->lev1 >= 0.99e20 && pl->lev2 >= 0.99e20 &&
				   pl->next == NULL) )
		  {
		    plg=generate_auto_fill_range(pA->min,pA->max);
		    if (plg==NULL)
		      {
			err_warn(1,fperr,
				 "Error - no memory to compute isofill ranges (%s)\n",
				 pD->name);
			pD->off=2;
			pD->dsp_seg[1]=0;
			pD->dsp_seg[2]=0;
			pD->dsp_seg[3]=0;
			erret++;
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
			
			gcrsg(pD->dsp_seg[0]=++segment_num);
			gssgp(pD->dsp_seg[0],(pP->dsp.p+pD->pri)/1000.0);
			
			if ((pGfi->missing >= 0) && (pGfi->missing <= 255))
                           missing_background(*pA->xs[0],*pA->xs[1],
                                            pA->xv[0],pA->xv[1],pGfi);

			gsclip(GCLIP);

/* 	/\* First of all copy the selected projection so we can overwrite stuff *\/ */
/* 	for (prj=&p_PRJ_list;prj->next!=NULL;prj=prj->next) */
/* 	  { */
/* 	    printf("Examining: %s vs %s\n",prj->name,pGfi->proj); */
/* 	    if (strcmp(prj->name,pGfi->proj)==0) set_projection(&prj,&p_PRJ); */
/* 	  } */

			filliso(*pA->xs[0],*pA->xs[1],pA->un.data,
				pA->xv[0],pA->xv[1],plg);
			
/* DNW - 05/03/01			if (doexist("longitude",pA->XN[0]) &&
			    doexist("latitude",pA->XN[1])  && Dc.selected > 0) */
/* DNW - 08/20/07                if (Dc.selected > 0) I put the above line back to avoid 
                                                      continents drawing when dimensions are not (long,lat)*/
		      savecontinents = Dc.selected;
		      if (pD->continents==-1) pD->continents = Dc.selected;
		      else Dc.selected = pD->continents;
                      if (doexist("longitude",pA->XN[0]) && doexist("latitude",pA->XN[1])  && pD->continents > 0)
			{
                         /*    generic_world coordinate values */
                         if (pD->continents < 3)
                          generic_world(X1,Y1,X2,Y2,Dc.lb,pGfi->xat,pGfi->yat);
                         else
                          generic_world2(X1,Y1,X2,Y2,Dc.lb,pGfi->xat,pGfi->yat);
			}
		      Dc.selected = savecontinents;
			gclsg();
			gselnt(0);
			
			gsclip(GNOCLIP);
			
			free((char *)mask);
			mask=NULL;
			
			pD->dsp_seg[1]=1;
			pD->dsp_seg[2]=1;
			pD->dsp_seg[3]=0;
		}
	}
	else if (erret == 0)
	{
		if (pA->F != NULL)
			err_warn(1,fperr,
				 "Error - isofill data (A_%s) from file (%s) not"
				 " acquired.\n",ptab->name,pA->F);
		else if (pA->f != NULL)
          err_warn(1,fperr,
				 "Error - isofill data (A_%s) from (%s) cannot be"
				 " computed.\n",ptab->name,pA->f);
		else
			err_warn(1,fperr,"Error - isofilling (A_%s).\n",ptab->name);
		if (pD->dsp_seg[0] > 0) gdsg(pD->dsp_seg[0]);
		pA->notok=1;
		pD->dsp_seg[0]=0;
		pD->dsp_seg[1]=0;
		pD->dsp_seg[2]=1;
		pD->dsp_seg[3]=0;
		erret++;
	}
	killA_tmp();	/* Remove temporary space for acquired arrays.	*/
	
	}
	if (erret == 0 && pD->leg_seg[3] > 0)
	{
		gcrsg(pD->leg_seg[0]=++segment_num);
		gssgp(pD->leg_seg[0],(pP->leg.p+pD->pri)/1000.0);

                /* Check to see if it is a List or a string of numbers.	*/
	        if (pGfi->legend != NULL) {
                   pltab=&L_tab[0];
                   while ((pltab != NULL) && (strcmp(pltab->name,pGfi->legend) != 0))
                      pltab=pltab->next;
		}
	
		if (legendGfi(plg,pP,pD, pltab) > 0)
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
	if (plg != NULL && plg != pGfi->line)
		for (pl1=plg; pl1!=NULL;)
		{
			plg=plg->next;
			free((char *)pl1);
			pl1=plg;
		}
	
	
	pict_elem(pA,pP,pD,pGfi->proj,
		  pGfi->xtl1,pGfi->xtl2,pGfi->xmt1,pGfi->xmt2,
		  pGfi->ytl1,pGfi->ytl2,pGfi->ymt1,pGfi->ymt2,
		  pGfi->dsp);
	
/*		If the variable isn't computed then restore excess dimensions.*/

        if (did_save_x)
        {
           for (l = 0; l < pA->xs[0][0]; ++l)
              pA->xv[0][l] = save_xv_0[l];
           *pA->xf[0] = save_xf[0];
           *pA->xl[0] = save_xl[0];
           free((char *) save_xv_0);
        }

        if (did_save_y)
        {
           for (l = 0; l < pA->xs[1][0]; ++l)
              pA->xv[1][l] = save_xv_1[l];
           *pA->xf[1] = save_xf[1];
           *pA->xl[1] = save_xl[1];
           free((char *) save_xv_1);
        }

	if (pA->f == NULL)
		for (i=2;i<pA->ND;i++)
		{
			*pA->xs[i]=save_xs[i];
			*pA->xl[i]=save_xl[i];
			*pA->xf[i]=save_xf[i];
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

/*		Fill between isolines.					*/

int filliso (im,jm,a,x,y,prng)
	
 	int im,jm;
	float *a;
	float *x;
	float *y;
	struct fill_range *prng;
	
	/*      int im,jm;	Limits of the array dimension sizes.		*/
	/*      float a[im,jm];	Array to be contoured.				*/
	/*      float x;		Values of x nodes.			*/
	/*      float y;		Values of y nodes.			*/
	/*      struct lin *prng;	Pointer to ranges.			*/
	
	/*	Other values are taken from global variables defined above.	*/
	
{
	int i,j,m,c;
	struct table_fill *pf;
	int nbox,n,i1,i2,j1,j2;
	Gpoint pxy[2*NMAX+24];
	Gpoint fxy;
	
	int nxy;
	struct fill_range *po;
	float r1,r2;
	
	A=a;
	IM=im;
	JM=jm;
	xv=x;
	yv=y;
	
/*			Loop for each set of ranges.			*/
	
	for (po=prng; po!=NULL; po=po->next)
	{
		r1=((po->lev1<=po->lev2)?po->lev1:po->lev2);
		r2=((po->lev1> po->lev2)?po->lev1:po->lev2);
		
/*			Set up the fill area definitions.		*/
		
		for (pf=&Tf_tab;pf!=NULL;pf=pf->next)
			if (strcmp(pf->name,po->fill_name) == 0)
			{
				gsfaci(pf->faci[0]);
				gsfais(pf->fais[0]);
				gsfasi(pf->fasi[0]);
				
				fxy.x=pf->w;
				fxy.y=pf->h;
				gspa(&fxy);
				pxy[0].x=pf->x;
				pxy[0].y=pf->y;
				gsparf(&pxy[0]);
				
				break;
			}
		
/*			If the fill is undefined proceed to the next.	*/
		
		if (pf == NULL)
		{
			err_warn(1,fperr,
				 "Error - Fill name (%s) not found for (%g -> %g).\n",
				 po->fill_name,po->lev1,po->lev2);
			continue;
		}
		
/*			If the range is for missing data then fill it.	*/
	
	/*			Set up the projection for this picture.		*/
		
		if (r1 >= 0.99e20 && r2 >= 0.99e20)
		{
			n=0;
			for (j=0;j<jm-1;j++)
			{
				for (i=0;i<im-1;i++)
				{
					if (mask[2*(j*im+i)] < 0)
					{
						if (n == 0)
						{
							pxy[n].x=xv[i];
							pxy[n].y=yv[j+1];
							n++;
							pxy[n].x=xv[i];
							pxy[n].y=yv[j];
							n++;
							pxy[n].x=xv[i+1];
							pxy[n].y=yv[j];
							n++;
							pxy[n].x=xv[i+1];
							pxy[n].y=yv[j+1];
						}
						else
						{
							pxy[n-1].x=xv[i+1];
							pxy[n-1].y=yv[j];
							pxy[n].x=xv[i+1];
							pxy[n].y=yv[j+1];
						}
					}
					else if (n > 0)
					{
						proj_gfa(n+1,pxy);
						n=0;
					}
					if (n > NMAX)
					{
						proj_gfa(n+1,pxy);
						n=0;
					}
				}
				
				if (n > 0)
				{
					proj_gfa(n+1,pxy);
					n=0;
				}
			}
			continue;
		}
		
/*			If the area is nil, proceed to the next.	*/
		
		if (r1 == r2) continue;
		
/*fprintf (stdout," Range %s %g %g\n",po->fill_name,po->lev1,po->lev2); */
		
		nbox=set_fill_mask(r1,r2,A,IM,JM,&i1,&i2,&j1,&j2);
		
		/*  fprintf(stdout,"     ");
		    for (i=i1;i<=i2;i++)
		    fprintf(stdout,"%d",i%10);
		    fprintf(stdout,"\n");
		    for (j=j2;j>=j1;j--)
		    {
		    fprintf(stdout,"(%d)  ",j);
		    for (i=i1;i<=i2;i++)
		    if (mask[2*(i+j*IM)] >= 0) fprintf(stdout,"%x",mask[2*(i+j*IM)]);
		    else fprintf(stdout,"+");
		    fprintf(stdout,"\n");
		    }
		    
		    fprintf(stdout,"\n");
		    for (j=j2;j>=j1;j--)
		    {
		    fprintf(stdout,"(%d)  ",j);
		    for (i=i1;i<=i2;i++)
		    if (mask[2*(i+j*IM)+1] >= 0) fprintf(stdout,"%x",mask[2*(i+j*IM)+1]);
		    else fprintf(stdout,"+");
		    fprintf(stdout,"\n");
		    }							*/
		for (i=i1,j=j1; j <= j2 && nbox > 0;)
		{
			while ((m=mask[2*(j*IM+i)]) > 0 && m < 16)
			{
				nxyu=nxyl=0;
				switch(m)
				{
				  case 1:
					c=fill01(r1,r2,i,j);
					break;
				  case 2:
					c=fill02(r1,r2,i,j);
					break;
				  case 3:
					c=fill03(r1,r2,i,j);
					break;
				  case 4:
					c=fill04(r1,r2,i,j);
					break;
				  case 5:
					c=fill05(r1,r2,i,j);
					break;
				  case 6:
					c=fill06(r1,r2,i,j);
					break;
				  case 7:
					c=fill07(r1,r2,i,j);
					break;
				  case 8:
					c=fill08(r1,r2,i,j);
					break;
				  case 9:
					c=fill09(r1,r2,i,j);
					break;
				  case 10:
					c=fill10(r1,r2,i,j);
					break;
				  case 11:
					c=fill11(r1,r2,i,j);
					break;
				  case 12:
					c=fill12(r1,r2,i,j);
					break;
				  case 13:
					c=fill13(r1,r2,i,j);
					break;
				  case 14:
					c=fill14(r1,r2,i,j);
					break;
				  case 15:
					c=fill15(r1,r2,i,j);
					break;
				}
				if (c < 0)
				{
					err_warn(1,fperr,"Error - tracing fill area (%d).\n",c);
					return 0;
				}
				for (n=0;n<nxyu+1;n++)
				{
					pxy[n].x=xyu[n].x;
					pxy[n].y=xyu[n].y;
				}
				for (nxy=nxyu+1,n=nxyl;n >= 0;n--)
				{
					pxy[nxy+nxyl-n].x=xyl[n].x;
					pxy[nxy+nxyl-n].y=xyl[n].y;
				}
				/*	fprintf(stdout,"Crossing Number (%d).\n",m);
					for (n=0;n  <nxyu+1;n++)
					{
					fprintf (stdout," %d(%g,%g) ",n,xyu[n].x,xyu[n].y);
					if (n%6 == 5) fprintf(stdout,"\n");
					}
					fprintf(stdout,"\n");
					for (n=0;n  <nxyl+1;n++)
					{
					fprintf (stdout," %d(%g,%g) ",n,xyl[n].x,xyl[n].y);
					if (n%6 == 5) fprintf(stdout,"\n");
					}
					fprintf(stdout,"\n");						*/
				proj_gfa(nxyu+nxyl+2,pxy);
			}
			
			i=(i<i2)?i+1:0;
			if (i == 0) j++;
		}  /*		End for i and j <= j2 loop.		*/
	} /* end for k loop  */
	
	/*	free((char *)mask);			*/
	return 1;
}

/*	Test for existence of a crossing in the grid box.
	Set the mask (defined global) values for the crossings.
	Return number of boxes within the range.			*/

int set_fill_mask (r1,r2,a,im,jm,i1,i2,j1,j2)
	
	float r1,r2;		/* Level for isoline.			*/
	float a[];		/* Test four values of the grid box.	*/
	/*  a4------a3				*/
	/*   |      |				*/
	/*  a1------a2				*/
	
	/*	short mask[][2];	   Indicate crossings.			*/
	
	int im,jm;		/* Size of 2D array.			*/
	int *i1,*i2,*j1,*j2;	/* Bound of range in the array.		*/
	
{
	int i,j,ixj,nx,mx;
	int nbox;
	
	
	*i1=im-1;	/* Limits (i1 & i2) of indices involved in "i".	*/
	*i2=0;
	*j1=jm-1;	/* Limits (j1 & j2) of indices involved in "j".	*/
	*j2=0;
	nbox=0;
	
	for (j=0;j<jm-1;j++)
		for (i=0;i<im-1;i++)
		{
			ixj=j*im+i;
			
			if (mask[ixj*2] >= 0)
			{
				nx=0;
				if (a[ixj] 	> r1) nx+=1;
				if (a[ixj+1] 	> r1) nx+=2;
				if (a[ixj+1+im]	> r1) nx+=4;
				if (a[ixj+im] 	> r1) nx+=8;
				mx=0;
				if (a[ixj] 	> r2) mx+=1;
				if (a[ixj+1] 	> r2) mx+=2;
				if (a[ixj+1+im]	> r2) mx+=4;
				if (a[ixj+im] 	> r2) mx+=8;
				if (mx==15 && nx==15) mx=nx=0;
				if (mx==0 && nx > 0) mx=15;
				mask[ixj*2]=nx;
				mask[ixj*2+1]=mx;
				if (nx > 0)
				{
					if (*i1 > i) *i1=i;
					if (*i2 < i) *i2=i;
					if (*j1 > j) *j1=j;
					if (*j2 < j) *j2=j;
					nbox++;
				}
			}
		}
	if (*i1 > *i2 || *j1 > *j2) *i1=*i2=*j1=*j2=0;
	return nbox;
}

/*			Display the legend for isofill.			*/

int legendGfi(struct fill_range *pifr,struct p_tab *pP,
	      struct display_tab *pD, struct l_tab *pltab)
	
{
	int i,k;
	int ni,contig;
	int rhs;
	float v,fi,xmx,xmn,ymx,ymn;
	char str1[256],str2[256];
	struct pe_leg pe;
	struct fill_range *po;
	Gpoint pxy[20],txy[2],fstxy[2],gxy;
	Gpoint fxy;
	
	Gpoint vxy;
	
	struct table_text *pTt;
	struct table_chorn *pTo;
	struct table_line *pTl;
	struct table_fill *pTf;
	Gtxalign pta;
	Gtxpath ptp;
	
	int horiz,l1,l2;

	struct l_val *vtab;
	
	if (pifr == NULL)
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
	/*			Count isofill descriptors used.			*/
	
	for (contig=ni=0,fi=0.0,po=pifr;po != NULL;po=po->next)
	{
		if (po->lev1 > 0.99e20 && po->lev2 > 0.99e20) continue;
		if (ni > 0 && po->lev1 == fi) contig++;
		fi=po->lev2;
		ni++;
	}
	if (contig != ni-1) contig=0;
	/* Commented OUt by C. Doutriaux 10/24/2005 */
	/* Replaced with actual values, allows for vertical display of values on either side */
/* 	ymx=(pe.y1 > pe.y2)?pe.y1:pe.y2; */
/* 	ymn=(pe.y1 < pe.y2)?pe.y1:pe.y2; */
/* 	xmx=(pe.x1 > pe.x2)?pe.x1:pe.x2; */
/* 	xmn=(pe.x1 < pe.x2)?pe.x1:pe.x2; */

	ymx=pe.y2;
	ymn=pe.y1;
	xmx=pe.x2;
	xmn=pe.x1;
	
	/*			Draw the legend.				*/
	
	if (horiz) 
	  {
	    ymx=(pe.y1 > pe.y2)?pe.y1:pe.y2;
	    ymn=(pe.y1 < pe.y2)?pe.y1:pe.y2;
	    for (i=0,fi=0.0,po=pifr;po != NULL;po=po->next,i++)
	      {
		if (po->lev1 > 0.99e20 && po->lev2 > 0.99e20) continue;
		pxy[0].x=pe.x1+(fi/ni)*(pe.x2-pe.x1);
		pxy[0].y=ymn;
		pxy[1].x=pe.x1+((fi+1.0)/ni)*(pe.x2-pe.x1);
		pxy[1].y=ymn;
		pxy[2].x=pxy[1].x;
		pxy[2].y=ymx;
		pxy[3].x=pxy[0].x;
		pxy[3].y=ymx;
		
		l1=po->lev1 > 0.99E20 || po->lev1 < -0.99E20; /* flag to draw the left end point */
		l2=po->lev2 > 0.99E20 || po->lev2 < -0.99E20; /* flag to draw the right end point */
		if (l1)
		  {
		    pxy[0].y=0.5*(pe.y1+pe.y2); /* Draw the left point */
		    pxy[3].y=pxy[0].y;
		  }
		if (l2)
		  {
		    pxy[1].y=0.5*(pe.y1+pe.y2); /* Draw the right point */
		    pxy[2].y=pxy[1].y;
		  }
		pxy[4].x=pxy[0].x;
		pxy[4].y=pxy[0].y;
		
		for (pTf=&Tf_tab; pTf != NULL; pTf=pTf->next)
		  if (strcmp(pTf->name,po->fill_name) == 0)
		    {
		      gsfaci(pTf->faci[0]);
		      gsfais(pTf->fais[0]);
		      gsfasi(pTf->fasi[0]);
		      
		      fxy.x=pTf->w;
		      fxy.y=pTf->h;
		      gspa(&fxy);
		      gxy.x=pTf->x;
		      gxy.y=pTf->y;
		      gsparf(&gxy);
		      
		      gfa(4,pxy);
		      
		      break;
		    }
		
		if (i == 0) {
		  fstxy[0].x = pxy[0].x;
		  fstxy[0].y = pxy[0].y; /* Get the first line for the legend */
		  fstxy[1].x = pxy[0].x;
		  fstxy[1].y = pxy[3].y;
		}
		if (pltab == NULL) {
		  if (pTl != NULL) gpl(5,pxy);
		  sprintf(str1,"%g",po->lev1); /* Get the level strings */
		  sprintf(str2,"%g",po->lev2);
		} else {
		  if ( (!l1) && (!l2) && (pTl != NULL)) {
		    txy[0].x = pxy[0].x;
		    txy[0].y = pxy[0].y;   /* Draw the bottom line of the legend box */
		    txy[1].x = pxy[1].x;
		    txy[1].y = pxy[0].y;
		    gpl(2,txy);
		    txy[0].x = pxy[0].x;
		    txy[0].y = pxy[3].y;   /* Draw the top line of the legend box */
		    txy[1].x = pxy[1].x;
		    txy[1].y = pxy[3].y;
		    gpl(2,txy);
		  } else if (pTl != NULL) {gpl(5,pxy);}
		  str1[0] = '\0'; str2[0] = '\0';
		  vtab = pltab->val;
		  while (vtab != NULL) {
		    if (vtab->data == po->lev1) {
		      txy[0].x = pxy[0].x;
		      txy[0].y = pxy[0].y; /* Draw the vertical line at the location */
		      txy[1].x = pxy[0].x;
		      txy[1].y = pxy[3].y;
		      gpl(2,txy);
		      strcpy(str1, vtab->str); /* Get the 1st given string */
		    }
		    if (vtab->data == po->lev2)
		      strcpy(str2, vtab->str); /* Get the 2nd given string */
		    vtab = vtab->next;
		  }
		}
		
		if (contig)
		  {
		    if (i%2 == 0)
		      {
			if (!l1)
			  {
			    gxy.x=pxy[0].x;
			    /*		       gxy.y=pxy[3].y+0.75*pTo->chh;		*/
			    gxy.y=pxy[3].y+pTo->chh;
			    gtx(&gxy,(unsigned char *)str1);
			  }
			if (po->next==NULL && !l2)
			  {
			    gxy.x=pxy[1].x;
			    gxy.y=pxy[1].y-0.75*pTo->chh;
			    gtx(&gxy,(unsigned char *)str2);
			  }
		      }
		    else
		      {
			if (!l1)
			  {
			    gxy.x=pxy[0].x;
			    gxy.y=pxy[0].y-0.75*pTo->chh;
			    gtx(&gxy,(unsigned char *)str1);
			  }
			if (po->next==NULL && !l2)
			  {
			    gxy.x=pxy[1].x;
			    gxy.y=pxy[2].y+0.75*pTo->chh;
			    gtx(&gxy,(unsigned char *)str2);
			  }
		      }
		  }
		else
		  {
		    v=0.5*(pxy[0].x+pxy[1].x);
		    gxy.x=v;
		    gxy.y=pxy[2].y+0.75*pTo->chh;
		    if (!l1) gtx(&gxy,(unsigned char *)str1);
		    gxy.y=pxy[0].y-0.75*pTo->chh;
		    if (!l2) gtx(&gxy,(unsigned char *)str2);
		  }
		fi=fi+1.0;
	      }
	    /* Draw the end lines on the legend */
	    if (!l2) {
	      gpl(2,fstxy);
	      txy[0].x = pxy[1].x;
	      txy[0].y = pxy[0].y;
	      txy[1].x = pxy[1].x;
	      txy[1].y = pxy[3].y;
	      gpl(2,txy);
	    }
	  } else			/*		Not horizontal.		*/
	    {
	      pta.hor=(pe.x1 > pe.x2)?GTH_RIGHT:GTH_LEFT;
	      pta.ver=GTV_HALF;
	      gstxal(&pta);
	      if (pe.x2 >= pe.x1) rhs=1;
	      else rhs=0;
	      for (i=0,fi=0.0,po=pifr;po != NULL;po=po->next,i++)
		{
		  if (po->lev1 > 0.99e20 && po->lev2 > 0.99e20) continue;
		  pxy[0].y=pe.y1+(fi/ni)*(pe.y2-pe.y1);
		  pxy[0].x=xmn;
		  pxy[1].y=pe.y1+((fi+1.0)/ni)*(pe.y2-pe.y1);
		  pxy[1].x=xmn;
		  pxy[2].y=pxy[1].y;
		  pxy[2].x=xmx;
		  pxy[3].y=pxy[0].y;
		  pxy[3].x=xmx;
		  
		  if (po->lev1 > 0.99E20) l1=1;
		  else if (po->lev1 < -0.99E20) l1=-1;
		  else l1=0;
		  if (po->lev2 > 0.99E20) l2=1;
		  else if (po->lev2 < -0.99E20) l2=-1;
		  else l2=0;
		  
		  if (l1 != 0)
		    {
		      pxy[0].x=0.5*(pe.x1+pe.x2);
		      pxy[3].x=pxy[0].x;
		    }
		  if (l2 != 0)
		    {
		      pxy[1].x=0.5*(pe.x1+pe.x2);
		      pxy[2].x=pxy[1].x;
		    }
		  pxy[4].x=pxy[0].x;
		  pxy[4].y=pxy[0].y;
		  
		  for (pTf=&Tf_tab; pTf != NULL; pTf=pTf->next)
		    if (strcmp(pTf->name,po->fill_name) == 0)
		      {
			gsfaci(pTf->faci[0]);
			gsfais(pTf->fais[0]);
			gsfasi(pTf->fasi[0]);
			gfa(4,pxy);
			break;
		      }
		  if ( (pTl != NULL) && (pltab == NULL) ) {gpl(5,pxy);}
		  
		  if (i == 0) {
		    fstxy[0].x = pxy[0].x;
		    fstxy[0].y = pxy[0].y; /* Get the first line for the legend */
		    fstxy[1].x = pxy[2].x;
		    fstxy[1].y = pxy[0].y;
		  }
		  
		  if (contig)
		    {
		      if (pltab == NULL) {
			sprintf(str1,"%g",po->lev1); /* Get the level strings */
			sprintf(str2,"%g",po->lev2);
		      } else {
			if ( (!l1) && (!l2) && (pTl != NULL)) {
			  txy[0].x = pxy[0].x;
			  txy[0].y = pxy[0].y;   /* Draw left of the legend box */
			  txy[1].x = pxy[0].x;
			  txy[1].y = pxy[1].y;
			  gpl(2,txy);
			  txy[0].x = pxy[2].x;
			  txy[0].y = pxy[0].y;   /* Draw right of the legend box */
			  txy[1].x = pxy[2].x;
			  txy[1].y = pxy[1].y;
			  gpl(2,txy);
			} else if (pTl != NULL) {gpl(5,pxy);}
			str1[0] = '\0'; str2[0] = '\0';
			vtab = pltab->val;
			while (vtab != NULL) {
			  if (vtab->data == po->lev1) {
			    txy[0].x = pxy[0].x;
			    txy[0].y = pxy[0].y; /* Draw line at the location */
			    txy[1].x = pxy[2].x;
			    txy[1].y = pxy[0].y;
			    gpl(2,txy);
			    strcpy(str1, vtab->str); /* Get the 1st given string */
			  }
			  if (vtab->data == po->lev2) {
			    strcpy(str2, vtab->str); /* Get the 2nd given string */
			  }
			  vtab = vtab->next;
			}
		      }
		      if (l1== 0)
			{
			  gxy.y=pxy[0].y;
			  if (l2 == 0) gxy.x=pxy[2].x+(-1.+2*rhs)*1.25*pTo->chh;
			  else 	  gxy.x=pxy[3].x+(-1.+2*rhs)*1.25*pTo->chh;
			  gtx(&gxy,(unsigned char *)str1);
			}
		      if (po->next==NULL && l2==0)
			{
			  gxy.y=pxy[1].y;
			  gxy.x=pxy[2].x+(-1.+2*rhs)*1.25*pTo->chh;
			  gtx(&gxy,(unsigned char *)str2);
			}
			}
		  else
		    {
		      if (l1==0 && l2==0) sprintf(str1,"%g - %g",po->lev1,po->lev2);
		      else if (l1<0)
			{
			  if      (l2==0) sprintf(str1,"-oo - %g",po->lev2);
			  else if (l2<0) sprintf(str1,"-oo - -oo");
			  else if (l2>0) sprintf(str1,"-oo - oo");
			}
		      else if (l1>0)
			{
			  if      (l2==0) sprintf(str1,"oo - %g",po->lev2);
			  else if (l2<0) sprintf(str1,"oo - -oo");
			  else if (l2>0) sprintf(str1,"oo - oo");
			}
		      else if (l2<0) sprintf(str1,"%g - -oo",po->lev1);
		      else if (l2>0) sprintf(str1,"%g - oo",po->lev1);
		      v=0.5*(pxy[0].y+pxy[1].y);
		      gxy.y=v;
		      if (rhs) { gxy.x=pe.x2+1.25*pTo->chh; pta.hor=GTH_LEFT; }
		      else     { gxy.x=pe.x2-1.25*pTo->chh; pta.hor=GTH_RIGHT;}
		      pta.ver=GTV_HALF;
		      gstxal(&pta);
		      gtx(&gxy,(unsigned char *)str1);
		    }
		  fi=fi+1.0;
		}
	      /* Draw the end lines on the legend */
	      if (!l2) {
		gpl(2,fstxy);
		txy[0].x = pxy[0].x;
		txy[0].y = pxy[1].y;
		txy[1].x = pxy[2].x;
		txy[1].y = pxy[1].y;
		gpl(2,txy);
	      }
	    }
	return 1;
}
