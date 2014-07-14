#include "gks.h"
#include "gksshort.h"
#ifdef USEX11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
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

/*    struct iso ISO={0,0,0.0,0.0,0,"*","default","default","default",NULL};*/

    extern struct workstations Wkst[];
    extern struct table_text Tt_tab;
    extern struct table_chorn To_tab;
    extern struct table_line Tl_tab;
    extern struct table_fill Tf_tab;
    extern struct table_mark Tm_tab;

    extern struct display_tab D_tab;

    extern struct project_attr p_PRJ;

    extern struct default_continents Dc;

    extern char PRJ_names[PRJ_TYPES][17];

    extern int segment_num;

    extern IN[14][2][2],JN[14][2][2];

    int nicedf(float a,float b,float *dr,int *pw10,float *center);

/*		Table of increments in i and j for isoline following.	*/

/*  int IN[14][2][2] = {
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

    int isolines (pD,pGi,pP,ptab)

      struct display_tab *pD;
      struct gi_attr *pGi;
      struct p_tab *pP;
      struct a_tab *ptab;

      {
	int i,j,nc,erret;

	struct a_attr *pA;
	struct project_attr *pj;
	struct iso *pGi_line;
	char *mask,*maska,*maskb,*maskc;
	float C[51];
	int Cid[51];
	int IM,JM;
	float dsp[NDS];

        int l;
        int did_save_x=0, did_save_y=0;
        float *save_xv_0, *save_xv_1;
        float X1,Y1,X2,Y2; /*	generic_world coordinate values */
        extern int trans_axis();
	int savecontinents;

	Glimit pvp;

	int ND;
	int save_xs[NDS];
	float save_xl[NDS],save_xf[NDS];
	char proj[17];
	float diagonal,diagonal2,ratio,tmp;

	pj=&p_PRJ;
	pA=ptab->pA_attr;
	erret=0;
	pGi_line=pGi->line;

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
/*			Save values to reset in case of too many dimensions.*/

	for (ND=i=0;i<pA->ND;i++)
	  {
	   save_xs[i]=*pA->xs[i];
	   save_xl[i]=*pA->xl[i];
	   save_xf[i]=*pA->xf[i];
	   if (*pA->xs[i] > 1) ND++;
	  }

 /*	generic_world coordinate values */
        X1= *pA->xf[0];
        Y1= *pA->xf[1];
        X2= *pA->xl[0];
        Y2= *pA->xl[1];

/*			Check NDC space is greater than zero.		*/

	if (pP->dsp.x2-pP->dsp.x1 < .001 || pP->dsp.y2-pP->dsp.y1 < .001)
	  {
	   err_warn(1,fperr,
			"Error - NDC display space is nil for ISOLINE.\n");
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
		"Error - ISOLINE requires 2 multi-valued dimensions.\n");
	   pD->off=2;
	   return 0;
	  }
	if (erret == 0 && ((pA->xs[2] != NULL && *(pA->xs[2]) > 1) ||
			      (pA->xs[3] != NULL && *(pA->xs[3]) > 1)  )  )
	  {
	   if (pA->f != NULL)
	     {
	      err_warn(0,fperr,
	            "Warning - ISOLINE is 2D. Taking a subset of (%dD) computed "
		    "variable.\n"
		    "        Modify dimensions of rhs: %s=%s\n",
				ND,ptab->name,pA->f);
	     /* erret++;*/
	     }
	   else
	     {
	      err_warn(0,fperr,
			"Warning - ISOLINE is 2D; (%s) is %dD.\n"
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
           if ( ((strcmp("linear",pGi->xat) != 0) &&
                 (strcmp("",pGi->xat) != 0)) &&
                 (strcmp("linear",pGi->proj) == 0) ) {
              if ((save_xv_0=(float *)malloc(pA->xs[0][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(save_xv_0,pA->xv[0],pA->xs[0][0],
                            &pA->xf[0][0], &pA->xl[0][0], pGi->xat,
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
           if ( ((strcmp("linear",pGi->yat) != 0) &&
                 (strcmp("",pGi->yat) != 0)) &&
                 (strcmp("linear",pGi->proj) == 0) ) {
              if ((save_xv_1=(float *)malloc(pA->xs[1][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(save_xv_1,pA->xv[1],pA->xs[1][0],
                            &pA->xf[1][0], &pA->xl[1][0], pGi->yat,
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
	diagonal = sqrt((dsp[2]-dsp[0])*(dsp[2]-dsp[0]) + (dsp[3]-dsp[1])*(dsp[3]-dsp[1]));
	if ((pGi->dsp[0]>1.E19) || (pGi->dsp[2]>1.E19)) diagonal2 = (dsp[2]-dsp[0])*(dsp[2]-dsp[0]);
	else diagonal2 = (pGi->dsp[2]-pGi->dsp[0])*(pGi->dsp[2]-pGi->dsp[0]);
	tmp = diagonal2;
	if ((pGi->dsp[1]>1.E19) || (pGi->dsp[3]>1.E19)) diagonal2 = (dsp[3]-dsp[1])*(dsp[3]-dsp[1]);
	else diagonal2 = (pGi->dsp[3]-pGi->dsp[1])*(pGi->dsp[3]-pGi->dsp[1]);
	ratio = sqrt(tmp/diagonal2)/(pP->dsp.x2-pP->dsp.x1)*(pP->dsp.y2-pP->dsp.y1);
	diagonal2 = sqrt(tmp+diagonal2);
	diagonal = diagonal/(diagonal/diagonal2);
/*			Set up the projection for this picture.		*/
	strcpy(proj,pGi->proj);
	if (cmpncs(proj,"Mollweide")==0 || cmpncs(proj,"Robinson")==0 ||
	       cmpncs(proj,"Polar"    )==0 )
	  {
	   if (cmpncs(pA->XN[0],"longitude") != 0 ||
	          cmpncs(pA->XN[1],"latitude" ) != 0 )
	     {
	      err_warn(1,fperr,
		      "Error - (ISOLINES) spherical projections work"
		      " only with data on a\n"
		      "        longitude x latitude y grid.  Linear"
		      " projection is used.\n");
	      strcpy(proj,"Linear");
	     }
	  }


	   /* Remember value used for click capabilities */
	   pD->dsp_used[0]=dsp[0];
	   pD->dsp_used[1]=dsp[1];
	   pD->dsp_used[2]=dsp[2];
	   pD->dsp_used[3]=dsp[3];

	if (set_projection(proj,pP->dsp,pGi->dsp,dsp) == 0)
	  {
	   err_warn(1,fperr,"Error - in projection for ISOLINES.\n");
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
		   "Error - ISOLINE data is outside the display range.\n");
	   erret++;
	  }

	if (pD->dsp_seg[3] > 0 && (pP->dsp.p > 0 && pP->dsp.x1 < 1.0 &&
	    pP->dsp.y1 < 1.0 && pP->dsp.x1 > 0.0 && pP->dsp.y1 > 0.0 &&
	    pP->dsp.x2 < 1.0 && pP->dsp.y2 < 1.0 && pP->dsp.x2 > 0.0 &&
	    pP->dsp.y2 > 0.0 ) )
	  {

/*		If the variable isn't computed then restore excess dimensions.*/
           if (did_save_x)
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
/*                              Transform the x coordinate axis.         */
           if ( ((strcmp("linear",pGi->xat) != 0) &&
                 (strcmp("",pGi->xat) != 0)) &&
                 (strcmp("linear",pGi->proj) == 0) ) {
              if ((save_xv_0=(float *)malloc(pA->xs[0][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming x-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(save_xv_0,pA->xv[0],pA->xs[0][0],
                            &pA->xf[0][0], &pA->xl[0][0], pGi->xat,
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
           if ( ((strcmp("linear",pGi->yat) != 0) &&
                 (strcmp("",pGi->yat) != 0)) &&
                 (strcmp("linear",pGi->proj) == 0) ) {
              if ((save_xv_1=(float *)malloc(pA->xs[1][0]*sizeof(float)))==NULL)                 {
                   err_warn(1,fperr,
                            "Error - memory overflow in tranforming y-axis.\n");                   return 0;
                 } else {
                    if (erret=trans_axis(save_xv_1,pA->xv[1],pA->xs[1][0],
                            &pA->xf[1][0], &pA->xl[1][0], pGi->yat,
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

/*			Allocate masks needed in lab_iso.		*/

	      if (pGi->labels != 'n')
	        {

	         if ( (maska=(char *)malloc(IM*JM)) == NULL)
	           {
	            err_warn(1,fperr,
			"Error - memory for isoline labels mask not found.\n");
		    erret++;
	           }

	         if ( erret == 0 && (maskb=(char *)malloc(IM*JM)) == NULL)
	           {
	            err_warn(1,fperr,
			"Error - memory for isoline labels mask not found.\n");
		    free((char *)maska);
		    erret++;
	           }

	         if ( erret == 0 && (maskc=(char *)malloc(IM*JM)) == NULL)
	           {
		    err_warn(1,fperr,
			"Error - memory for isoline labels mask not found.\n");
		    free((char *)maska);
		    free((char *)maskb);
		    erret++;
	           }
/*		Set up the contouring mask from the data mask.		*/

		 if (erret == 0)
		   for (i=0,j=0;j < JM-1;)
		     {
		      if (*(pA->mask+j*IM+i) == 0 ||
			*(pA->mask+j*IM+i+1) == 0 ||
			*(pA->mask+(j+1)*IM+i) == 0 ||
			*(pA->mask+(j+1)*IM+i+1) == 0 )
			*(maska+j*IM+i)=*(maskb+j*IM+i)=*(maskc+j*IM+i) = -1;
		      else
			*(maska+j*IM+i)=*(maskb+j*IM+i)=*(maskc+j*IM+i)=0;

		      if (++i == IM-1)  { i=0; j++;}
	             } /* end for i & j loop  */

		}
	      if ( erret == 0 && (mask=(char *)malloc(IM*JM)) == NULL)
		{
		 err_warn(1,fperr,
			"Error - memory for isoline mask not found.\n");
		 erret++;
		}

/*		Set up the contouring mask from the data mask.		*/

	      if (erret == 0)
		for (j=0;j<JM-1;j++)
		  for (i=0;i<IM-1;i++)
		    {
		     if (*(pA->mask+j*IM+i) == 0 ||
			*(pA->mask+j*IM+i+1) == 0 ||
			*(pA->mask+(j+1)*IM+i) == 0 ||
			*(pA->mask+(j+1)*IM+i+1) == 0 )
			*(mask+j*IM+i) = -1;
		     else
			*(mask+j*IM+i) = 0;
	            } /* end for i & j loop  */

	      if (erret == 0 && 
		 (nc=set_isoline(&pGi_line,500,pA->min,pA->max,C,Cid)) <= 0)
		{
		 free((char *)mask);
	         if (pGi->labels != 'n')
		   {
		    free((char *)maska);
		    free((char *)maskb);
		    free((char *)maskc);
		   }
		 erret++;
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
	         if(folow(*pA->xs[0],*pA->xs[1],pA->un.data,mask,C,Cid,nc,
		  pA->xv[0],pA->xv[1],pGi_line,diagonal,ratio) )
	           {
	            if (pGi->labels != 'n')
	            lab_iso(*pA->xs[0],*pA->xs[1],pA->un.data,C,Cid,nc,
			pA->xv[0],pA->xv[1],pGi_line,maska,maskb,
			maskc);
	           }
		 else erret++;

/* DNW - 05/03/01	         if (doexist("longitude",pA->XN[0]) &&
		  doexist("latitude",pA->XN[1])  && Dc.selected > 0)   */
/* DNW - 08/20/07                if (Dc.selected > 0) I put the above line back to avoid 
                                                      continents drawing when dimensions are not (long,lat)*/
		 savecontinents = Dc.selected;
		 if (pD->continents==-1) pD->continents = Dc.selected;
		 else Dc.selected = pD->continents;
                 if (doexist("longitude",pA->XN[0]) && doexist("latitude",pA->XN[1])  && pD->continents > 0)
		   {
                     /*    generic_world coordinate values */
	             if (pD->continents < 3)
			generic_world(X1,Y1,X2,Y2,Dc.lb,pGi->xat,pGi->yat);
                     else
			generic_world2(X1,Y1,X2,Y2,Dc.lb,pGi->xat,pGi->yat);
		   }
		 Dc.selected = savecontinents;
	         pD->dsp_seg[1]=1;
	         pD->dsp_seg[2]=1;
	         pD->dsp_seg[3]=0;

	         gclsg();
		 gselnt(0);

	         gsclip(GNOCLIP);

	         free(mask);
	         if (pGi->labels != 'n')
	           {
		    free(maska);
	            free(maskb);
	            free(maskc);
		   }
		}
	     }
	   else if (erret == 0)
	     {
	      if (pA->F != NULL)
		err_warn(1,fperr,
			"Error - isoline data (A_%s) from file (%s) not"
			  " acquired.\n",ptab->name,pA->F);
	      else if (pA->f != NULL)
		err_warn(1,fperr,
			"Error - isoline data (A_%s) from (%s) cannot be"
			  " computed.\n",ptab->name,pA->f);
	      else
		err_warn(1,fperr,"Error - isolining (A_%s).\n",ptab->name);
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
	   if ((nc=set_isoline(&pGi_line,500,pA->min,pA->max,C,Cid)) <= 0)
			 erret++;
	   gcrsg(pD->leg_seg[0]=++segment_num);
	   gssgp(pD->leg_seg[0],(pP->leg.p+pD->pri)/1000.0);

	   if (legendGi(nc,C,Cid,pGi_line,pP) > 0)
	     {

	      pD->leg_seg[1]=1;
	      pD->leg_seg[2]=1;
	      pD->leg_seg[3]=0;
	      gclsg();
	     }
	   else
	     {
	      gclsg();
	      gdsg(pD->leg_seg[0]);
	      pD->leg_seg[0]=0;
	      pD->leg_seg[1]=0;
	      pD->leg_seg[2]=0;
	      pD->leg_seg[3]=0;
/*	      erret++; 		 DNW - 5/2/01 - don't turn the off flag off. */
	     }

	  }

	pict_elem(pA,pP,pD,pGi->proj,pGi->xtl1,pGi->xtl2,pGi->xmt1,pGi->xmt2,
				     pGi->ytl1,pGi->ytl2,pGi->ymt1,pGi->ymt2,
				     pGi->dsp);

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

/* draws an arrow at p1, pointing toward p2 */
int forward_draw(p1,p2,length,arrow_angle,ratio)
  Gpoint p1,p2;
  float length,arrow_angle,ratio;
  {
    Gpoint arrow[3];
    arrow[1]=p1;
    float angle,Alpha,angle2;
	struct project_attr *pj;
	float rx,ry;

	pj=&p_PRJ;
 	rx=(pj->X2-pj->X1)/(pj->x2_NDC-pj->x1_NDC);
	ry=(pj->Y2-pj->Y1)/(pj->y2_NDC-pj->y1_NDC);
   
    angle = atan(ratio*(p2.y-p1.y)/(p2.x-p1.x));
    if ((p2.x-p1.x)<0) angle += 3.1415926535897931;
/*     printf("angle, dy, dx: %f, %f, %f\n",angle/3.14*180,(p2.y-p1.y),(p2.x-p1.x)); */
    angle2 = arrow_angle/180*3.1415926535897931;
    Alpha = angle+angle2;
    arrow[0].x=p1.x-rx*length*cos(Alpha);
    arrow[0].y=p1.y-ry*length*sin(Alpha);
    Alpha = angle-angle2;
    arrow[2].x=p1.x-rx*length*cos(Alpha);
    arrow[2].y=p1.y-ry*length*sin(Alpha);
    proj_pl(3,arrow);
  }

/* part that draws the direction arrows */
int draw_arrows(n,points,clockwise,length,angle,spacing,ratio)
     int n,clockwise;
     float length,angle,spacing,ratio;
     Gpoint points[200];
{
  extern int isClockwise();
  int clockwiseness,i,clockwise2;
  float distance;
  float norm,pct,total;

  Gpoint arrow_loc;
  clockwiseness=2*isClockwise(points,n)-1;
  total=0.;
  for (i=0;i<n-1;i++) total += sqrt((points[i+1].x-points[i].x)*(points[i+1].x-points[i].x)+ratio*(points[i+1].y-points[i].y)*(points[i+1].y-points[i].y));
  if ((total/spacing)<.75) return 0;
  i = (int)(total/spacing)-1;
  if (total<spacing) distance = total/2.-spacing;
    else distance = -(total-i*spacing)/2.;
  for (i=0;i<n-1;i++){
    norm = sqrt((points[i+1].x-points[i].x)*(points[i+1].x-points[i].x)+ratio*(points[i+1].y-points[i].y)*(points[i+1].y-points[i].y));
    while ((distance+spacing)<norm) {
      pct = (distance + spacing)/norm;
      /* set the point for  arrow */
      arrow_loc.x=points[i].x+(points[i+1].x-points[i].x)*pct;
      arrow_loc.y=points[i].y+(points[i+1].y-points[i].y)*pct;

      clockwise2=clockwise;
      if (abs(clockwise)==2) clockwise2 = clockwise2/abs(clockwise2)*sign(arrow_loc.y);
      if (abs(clockwise)==3) clockwise2 = clockwise2/abs(clockwise2)*sign(arrow_loc.x);
      if (clockwiseness*clockwise2==1) {
	forward_draw(arrow_loc,points[i+1],length,angle,ratio);
      }
      else {
	forward_draw(arrow_loc,points[i],length,angle,ratio);
      }
      distance +=spacing;
    }
    distance-=norm;
  }
  return 0;
}
/*		Follow and draw isolines.		*/

    int folow (IM,JM,A,mask,C,Cid,nc,xv,yv,piso,diagonal,ratio)

 	int IM,JM;
	float A[];
	char mask[];
	float C[];
	int Cid[];
	int nc;
	float xv[];
	float yv[];
	struct iso *piso;
	float diagonal;
	float ratio;

/*      int IM,JM;	Limits of the array dimension sizes.		*/
/*      float A[JM,IM];	Array to be isolined.				*/
/*      short mask[JM,IM];Mask array (0 - invalid, 1 - valid).		*/
/*      float xv[xs];		Values of x nodes.			*/
/*      float yv[ys];		Values of y nodes.			*/
/*      struct iso *piso;	Pointer to isoline definitions.		*/

      {
	int i,j,k,m;
	int ixj;
	struct table_line *pl;
	int nbox,n,i1,i2,j1,j2;
	int closed;
	Gpoint pxy[200];
	int nxy;
	int err;
	char label[17];
	struct iso *po;
	int ix,jy,i0,j0,I,J;
	float a0,a1,coef,x0,x1,y0,y1;
/*			Set line attributes.				*/

	for (k=0; k < nc; k++)
	  {
	   po=piso;
	   if (po != NULL)
	     {
	      while (po != NULL)
	        {
	         if (Cid[k] == po->id)
		   {
		    pl=&Tl_tab;
		    strncpy(label,po->lb,17); label[16] = '\0';
		    if (strlen(label)<=(size_t) 0) {
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
                    if (pl->lci == NULL)
		       gsplci(241);
                    else
		       gsplci(pl->lci[0]);
		    break;
		   }
	         po=po->next;
	        }
	      if (po == NULL)
	        {
	         err_warn(1,fperr,"Error - no isolines defined.\n");
	         return 0;
	        }
	     }
	   else
	     {
	      pl=&Tl_tab;
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
	     }

/*   fprintf (stdout," Level %g \n",C[k]);			*/


/*	Set line crossings for areas bounded by 4 valid points.
	The validity of these areas is determined prior to entry and
	indicated in the array, mask (-ve = invalid, >=0 = valid).	*/

	   set_mask(C[k],A,mask,IM,JM,&nbox,&i1,&i2,&j1,&j2);

	   if (nbox > 0)
	     {
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
/*		    if (m > 16 || m <= 0) break;			*/
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
			a0=A[ixj+IN[m][0][0]+JN[m][0][0]*IM];
			a1=A[ixj+IN[m][0][1]+JN[m][0][1]*IM];
		    coef=(C[k]-a0)/(a1-a0);
			x0=xv[I+IN[m][0][0]];
			x1=xv[I+IN[m][0][1]];
		    pxy[nxy].x=(x1-x0)*coef+x0;
			y0=yv[J+JN[m][0][0]];
			y1=yv[J+JN[m][0][1]];
		    pxy[nxy].y=(y1-y0)*coef+y0;

		    while (I >= i1 && I <= i2 && J >= j1 && J <= j2)
		      {
		       nxy++;
		       if (nxy == 200)
			 {
			  if ( (err=proj_pl(nxy,pxy)) != 0)
			    {
			     err_warn(1,fperr,
				"Error - folow (%d) polyline\n",err);
			     return 0;
			    }
			  pxy[0].x=pxy[nxy-1].x;
			  pxy[0].y=pxy[nxy-1].y;
			  nxy=1;
			 }
			a0=A[ixj+IN[m][1][0]+JN[m][1][0]*IM];
			a1=A[ixj+IN[m][1][1]+JN[m][1][1]*IM];
		       coef=(C[k]-a0)/(a1-a0);
			x0=xv[I+IN[m][1][0]];
			x1=xv[I+IN[m][1][1]];
		       pxy[nxy].x=(x1-x0)*coef+x0;
			y0=yv[J+JN[m][1][0]];
			y1=yv[J+JN[m][1][1]];
		       pxy[nxy].y=(y1-y0)*coef+y0;

		       ix=IN[m][1][0]+IN[m][1][1]-1;
		       jy=JN[m][1][0]+JN[m][1][1]-1;
		       I+=ix;
		       J+=jy;
		       if (I < i1 || I > i2 || J < j1 || J > j2) break;
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
		    if (nxy > 0 && (err=proj_pl(nxy+1,pxy)) != 0)
		      {
		       err_warn(1,fperr,"Error - folow (%d) polyline\n",err);
		       return 0;
		      }
		    if ((nxy>0) && (po->cw!=0)) {
		      draw_arrows(nxy+1,pxy,po->cw,90.*po->ls/12000.,po->angle,po->spc*diagonal/50.,ratio);/* draw clockwise arrows */
		    }
		   }

/*			Increment i and j, iff the initial node wasn't
			a double crossing (i.e.mask = 10 or 5) that
			didn't get completed.				*/

		 if (mask[i+j*IM] <= 0 || mask[i+j*IM] > 15)
		   {
		    i=(i<i2)?i+1:0;
		    if (i == 0) j++;
		   }

		}  /*		End for i and j <= j2 loop.		*/
	     }
	  } /* end for k loop  */

	return 1;
      }


/*	Encode numbers and reduce to least length.			*/

    int codeg(r,ch,n)
      float r;
      char ch[];
      int n;
      {
       int i,k;
       char str[64];
       double dr;

       str[63]='\0';
       dr=r;
       sprintf (str,"%G",dr);
/*		Count number of characters.				*/

       for (k=0;k<64 && str[k] != '\0';k++);
       if (k <= 0)
	 {
	  ch[0]='#';
	  ch[1]='\0';
	  return 0;
	 }
/*		Search for an exponent.					*/
       for (i=0; i <= k && str[i] != 'E'; i++);
/*		Minimize characters in the exponent.			*/
       if (i <= k && str[i] == 'E')
	 {
	  i++;
	  if (str[i] == '+')
	    {
	     str[i]=str[i+1];
	     str[i+1]=str[i+2];
	     str[i+2]='\0';
	     k-=1;
	    }
	  else i++;		/* it must be minus, so skip it */

	  if (str[i]=='0')
	    {
	     str[i]=str[i+1];
	     str[i+1]='\0';
	     k-=1;
	    }
	 }
/*		Move characters to the output string.			*/
       for (i=0; (ch[i]=str[i]) != '\0' && i < n; i++);
       ch[n-1]='\0';
       return k;
      }

/*	Set up the isoline levels in increasing order.
	The number of isoline levels is returned.			*/

    int set_isoline(po,cmx,Amn,Amx,C,Cid)

      struct iso **po;	/* Pointer to isoline definitions.		*/
      int cmx;		/* Max number of isoline levels allowed.	*/
      float Amn,Amx;	/* Max and min isoline levels.			*/
      float C[];	/* Return isoline levels to plot.		*/
      int Cid[];	/* Return iso.id values for isolines.		*/
      {
	int i,j,k,k1,k2,c;
	struct iso *piso;
	float Cmin;
	float dC;
	float dr,del,center;
	int n;

/*		If no isoline levels specified then compute an interval
		and assume reference at 0.0.				*/

	if (Amx == Amn)
	  {
	   err_warn (1,fperr,"Error - isoline data is flat.\n");
	   return 0;
	  }
/*	if (*po == NULL) return 0;			*/
	if (*po == NULL || fabs((*po)->incr) >= 0.999e20 )
	  {
	   if (nicedf(Amx,Amn,&dr,&n,&center) == 0) return 0;
	   del=dr*pow(10.0,(double) n);
	   k1=Amn/del;
	   if (k1*del < Amn) k1++;
	   k2=Amx/del;
	   if (k2*del > Amx) k2--;
	   if (k2-k1+1 > cmx) return 0;
	   for (i=0;i < cmx && (i+k1)*del < Amx;i++)
	     {
	      C[i]=(i+k1)*del;
	      if (*po != NULL) Cid[i]=(*po)->id;
	      else Cid[i]=0;
	     }

/*	   ISO.incr=del;
	   if (fabs((*po)->lev) >= .999e20) ISO.lev=0.0;
	   else ISO.lev=(*po)->lev;
	   *po=&ISO;
*/
	   return i;
	  }

	dC=1.e-4*fabs(Amx-Amn);

/* 	for (i=0,piso=*po; piso != NULL; piso=piso->next) */
/* 	  { */
/* 	    printf("center: %f\n",piso->lev); */
/* 	  } */
	for (i=0,piso=*po; piso != NULL; piso=piso->next)
	  {
	   center=piso->lev;
	   del=piso->incr;
	   if (fabs(center) > .999e20) center=0.0;
	   if (del != 0.0 && center+del == center)
	     {
	      err_warn (1,fperr,"Error - isoline increment is too small.\n");
	      return 0;
	     }

/*		Find the number of increments from the base level to
		the min and max of the data.				*/
	   if (del > dC)
	     {
	      k1=(Amn-center)/del;
	      k2=(Amx-center)/del;
	      if (center+k1*del < Amn) k1++;
	      if (center+k2*del >= Amx) k2--;
	     }
	   else if (del < -dC)
	     {
	      k1=(Amx-center)/del;
	      k2=(Amn-center)/del;
	      if (center+k1*del >= Amx) k1++;
	      if (center+k2*del <  Amn) k2--;
	     }
	   else
	     {
	      if (center >= Amx || center < Amn) continue; 
	      k1=0;
	      k2=0;
	     }
	   if ( (k1 < 0 && k2 < 0) || (k1 > k2) ) continue;
	   k1=(k1 < 0)? 0 : k1;
	   k2=(k2 < 0)? 0 : k2;
	   for (k=k1; k <= k2; k++)
	     {
	      C[i]=center+k*del;
	      Cid[i]=piso->id;
	      /* The following seems to cause problem when data have HUGE spread */
	      /* comenting out seems to fix it w/o affecting anything else... */
	      /* May be it was old code, not needed anymore */
/* 	      for (j=0;j<i;j++) */
/* 		{ */
/* 		 if (fabs(C[j]-C[i]) < dC) */
/* 		   { */
/* 		    i--; */
/* 		    break; */
/* 		   } */
/* 		} */
	      i++;
	      if (i >= cmx)
	        { 
	         err_warn(1,fperr,"Error - too many isoline levels.\n");
	         return 0;
	        }

	     }
	  }

/* 		Sort the levels and move the ids with them.		*/

	for (k=0; k < i-1; k++)
	  {
	   Cmin=C[k];
	   for (j=k+1; j < i; j++)
	     {
	      if (C[j] < Cmin)
		{
	         Cmin=C[j];
	         C[j]=C[k];
	         C[k]=Cmin;
	         c=Cid[j];
	         Cid[j]=Cid[k];
	         Cid[k]=c;
		 j=k;
		}
	      else if (C[j] == C[k])
		{
		 for (n=j; n < i-1; n++)
		   {
		    C[n]=C[n+1];
		    Cid[n]=Cid[n+1];
		   }
		 j--;
		 i--;
		}
	     }
	  }
	return i;
      }

/*		Label isolines.						*/

    int lab_iso ( IM,JM,A,C,Cid,nc,xv,yv,piso,maska,maskb,maskc)

 	int IM,JM;
	float A[];
	float C[];
	int Cid[];
	int nc;
	float xv[];
	float yv[];
	struct iso *piso;
	char *maska,*maskb,*maskc;

/*      int IM,JM;	Limits of the array dimension sizes.		*/
/*      float A[JM,IM];	Array to be isolined.				*/
/*	float C;		Isolines to be labelled.		*/
/*	int Cid;		Attribute id for isolines.		*/
/*	int nc;			Number of isolines.			*/
/*      float xv[xs];		Values of x nodes.			*/
/*      float yv[ys];		Values of y nodes.			*/
/*      struct iso *piso;	Pointer to isoline definitions.		*/
/*	char *maska,maskb,maskc;Masks for prior, imm, and next isoline. */

      {
	int i,j,k;
	int ixj;
	int K;
	int nbox,nboxm,nboxp,nboxmp,n;
	int i1,i2,j1,j2;
	int i1p,i2p,j1p,j2p;
	int i1m,i2m,j1m,j2m;
	int i1mp,i2mp,j1mp,j2mp;
	char label[13];
	Gpoint pxy[20];
	Gintlist wsid;
	int opwk;
	int *pwk;
	
	struct table_text *pTt;
	struct table_chorn *pTo;
	Gextent extent[6];
	Gpoint points[6][4];

	Gpoint lab_ext[2][100][4];
	int closed;
	struct iso *po;
	int i0,j0;
	char *mask,*maskm,*maskp,*mtmp;
	int mk[3];
	float dx,dy;
	int ix0,jy0;
	float rext;
	int N;
	int iff[6],nf;
	int nl0,nl1;

/*		Initialize the beginning parameters - DNW 		*/
	mk[0]=mk[1]=mk[2]=0;
	po=piso;
	nbox=nboxm=nboxp=0;
	i1=i2=j1=j2=i1p=i2p=j1p=j2p=i1m=i2m=j1m=j2m=0;
	for (i = 0; i < 19; ++i) {
          pxy[i].x = 0.0;
          pxy[i].y = 0.0;
	}

	if (nc <= 0) return 0;

/*			Check for existence of a valid label.		*/

	for (k=0; k < nc; k++)
	  {
	   po=piso;
	   while (po != NULL)
	     {
	      if (Cid[k] == po->id) break;
	      po=po->next;
	     }
	   if (po == NULL) return 0;
	   if (strlen(po->lab) > (size_t) 0) break;
	   if (k == nc-1) return 0;
	  }

	mask=maska;
	maskm=maskb;
	maskp=maskc;

	nl0=0;
/*			Set the label					*/

	for (k=0; k < nc; k++)
	  {
	   nl1=0;
	   po=piso;
	   while (po != NULL)
	     {
	      if (Cid[k] == po->id)
		{
		 strncpy(label,po->lab,13); label[12] = '\0';
		 break;
		}
	      po=po->next;
	     }
	   if (label[0] == '*')
		codeg(C[k],label,13);
	   else if (strlen(label) == 0 || label[0] == ' ') continue;

	   opwk=7; /* Set opened workstation to the WISS. This needed for text background mode. */
           wsid.number = 0;
           wsid.integers = NULL;
           gqopwk(&wsid);
	   for (i=0,pwk=wsid.integers;i<wsid.number;i++,pwk++)
	     {
	      if (*pwk != 7) {opwk=*pwk; break;}
	     }
/*
	   if (opwk == 0)
	     {
	      err_warn(0,fperr,"Warning - (lab_iso) no workstations open.\n");
	     }
*/
           if (wsid.number > 0 && wsid.integers != NULL)
	       free((char *)wsid.integers);

/*			Find the text bundle.				*/
	   pTt=&Tt_tab;
	   while (pTt != NULL)
	     {
	      if (strcmp(po->tb,pTt->name) == 0) break;
	      pTt=pTt->next;
	     }
	   if (pTt == NULL) pTt=&Tt_tab;

/*			Find the text orientation bundle.		*/
	   pTo=&To_tab;
	   while (pTo != NULL)
	     {
	      if (strcmp(po->to,pTo->name) == 0) break;
	      pTo=pTo->next;
	     }
	   if (pTo == NULL) pTo=&To_tab;

	   set_text_attr(pTt,pTo);
/*		Find text extents.					*/

	   if (opwk != 0)
#ifdef CAIRODRAW
		cairogqtxx(opwk,pxy[0],(unsigned char *)label,&extent[0]);
#else
		gqtxx(opwk,pxy[0],(unsigned char *)label,&extent[0]);
#endif
	   else
		getextent(&(pxy[0]),label,pTt,pTo,&(extent[0]));

	   points[0][0].x=extent[0].ll.x;
	   points[0][0].y=extent[0].ll.y;
	   points[0][1].x=extent[0].lr.x;
	   points[0][1].y=extent[0].lr.y;
	   points[0][2].x=extent[0].ur.x;
	   points[0][2].y=extent[0].ur.y;
	   points[0][3].x=extent[0].ul.x;
	   points[0][3].y=extent[0].ul.y;


/*			Shift masks.					*/

	   mtmp=maskm;
	   maskm=mask;
	   mask=maskp;
	   maskp=mtmp;

/*			Shift limit indices and active box counts.	*/

	   i1m=i1;  i2m=i2;  j1m=j1;  j2m=j2;
	   i1=i1p;  i2=i2p;  j1=j1p;  j2=j2p;
	   nboxm=nbox; nbox=nboxp;

/*			Set k mask (when k = 0) and limits.		*/

	   if (k == 0)
	     set_mask(C[k],A,mask,IM,JM,&nbox,&i1,&i2,&j1,&j2);

/*			Set the k+1 mask (maskp) or zero it if k+1 = nc.*/

	   if (k+1 < nc)
	     set_mask(C[k+1],A,maskp,IM,JM,&nboxp,&i1p,&i2p,&j1p,&j2p);
	   else
	     {
	      for (i=0,j=0; j < JM-1; )
	        {
	         ixj=j*IM+i;
		 maskp[ixj]=((maskp[ixj] > 0)?0:maskp[ixj]);
	         if (++i == IM-1) { i=0; j++;}
	        }
	      i1p=i2p=j1p=j2p=nboxp=0;
	     }

	   if (nbox > 0 && pTo->chh > 0.001)
	     {
	      for (i=i1,j=j1; j <= j2;)
		{

		 ixj=j*IM+i;
		 if (mask[ixj] > 0 && mask[ixj] < 15)
		   {
/*			Find the beginning of the line.			*/

		    trace_begin(mask,IM,JM,i,j,&i0,&j0,
						&ix0,&jy0,&closed);


/*			Find possible label points.			*/

		    dx=points[0][2].x-points[0][1].x;
		    dy=points[0][2].y-points[0][1].y;
		    rext=sqrt(dx*dx+dy*dy);
		    N=find_labels(A,C[k],xv,yv,mask,IM,JM,rext,i0,j0,ix0,jy0,
					closed,pxy);
		    if (N > 0)
		      {
/*				Find text extents.		*/

		       for (nf=0,n=0;n<N;n++)
		         {
			  iff[n]=1;
			  if (opwk != 0)
#ifdef CAIRODRAW
			    cairogqtxx(opwk,pxy[n],
				(unsigned char *)label,&(extent[n]));
#else
			    gqtxx(opwk,pxy[n],
				(unsigned char *)label,&(extent[n]));
#endif
			  else
			    getextent (&(pxy[n]),label,pTt,pTo,&(extent[n]));

			  points[n][0].x=extent[n].ll.x;
			  points[n][0].y=extent[n].ll.y;
			  points[n][1].x=extent[n].lr.x;
			  points[n][1].y=extent[n].lr.y;
			  points[n][2].x=extent[n].ur.x;
			  points[n][2].y=extent[n].ur.y;
			  points[n][3].x=extent[n].ul.x;
			  points[n][3].y=extent[n].ul.y;

			  check_label(lab_ext,nl0,nl1,&points[n][0],&iff[n]);
			  if (iff[n] > 0) nf++;
		         }
		       for (K=((k==0)?1:k-1); K <= k+1 && K < nc && nf > 0;K+=2)
			 {
			  if (K == k-1)
			    {
			     mtmp=maskm;
			     nboxmp=nboxm;
			     i1mp=i1m;
			     i2mp=i2m;
			     j1mp=j1m;
			     j2mp=j2m;
			    }
			  else
			    {
			     mtmp=maskp;
			     nboxmp=nboxp;
			     i1mp=i1p;
			     i2mp=i2p;
			     j1mp=j1p;
			     j2mp=j2p;
			    }
			  if (nboxmp <= 0) break;
			  test_labels(IM,JM,A,mtmp,nboxmp,i1mp,i2mp,
				j1mp,j2mp,C[K],xv,yv,N,points,iff);
			  set_mask(C[K],A,mtmp,IM,JM,&nboxmp,
				&i1mp,&i2mp,&j1mp,&j2mp);
			  for (n=0,nf=0;n<N;n++) if (iff[n] > 0) nf++;
			 }
/*			Print isoline label, if possible.		*/

		       for (n=0;nf>0 && n<N;n++)
			 {
			  if (iff[n] > 0)
			    {
			     if (nl1 >= 99) break;
			     for (j=0;j<4;j++)
			       {
			        lab_ext[1][nl1][j].x=points[n][j].x;
			        lab_ext[1][nl1][j].y=points[n][j].y;
			       }
			     nl1++;
			     gsfaci(po->hici);
			     gsfais(GSOLID);
                             gfa(4,points[n]);
			     gtx(&pxy[n],(unsigned char *)label);
			     break;
			    }
			 }

		      }
		    erase_line(mask,IM,JM,i0,j0,ix0,jy0);
		   }

/*			Increment i and j.				*/

		 i=(i<i2)?i+1:i1;
		 if (i == i1) j++;

		}  /*		End for i and j <= j2 loop.		*/
	      set_mask(C[k],A,mask,IM,JM,&nbox,&i1,&i2,&j1,&j2);
	     }
	   nl0=nl1;
	   for (n=0;n<nl0;n++)
	     {
	      for (j=0;j<4;j++)
		{
		 lab_ext[0][n][j].x=lab_ext[1][n][j].x;
		 lab_ext[0][n][j].y=lab_ext[1][n][j].y;
		}
	     }
	  } /* end for k loop  */

/*	free((char *)mask);
	free((char *)maskm);
	free((char *)maskp);*/
	return 1;
      }

/*			Compute length of the line, extremes in x and y,
			indices and position (x,y) of end of line, and
			number of points.				*/

      int find_labels(A,C,xv,yv,mask,IM,JM,rlab,i0,j0,ix0,jy0,closed,pxy)

	float A[];	/* Data array A[IM,JM].				*/
	float C;	/* Isoline level.				*/
	float xv[];	/* X dimension values.				*/
	float yv[];	/* Y dimension values.				*/
	char mask[];	/* Isoline mask[JM,IM], JM-1 and IM-1 values.	*/
	int IM,JM;	/* Size of array A.				*/
	float rlab;	/* Label length in NDC.				*/
	int i0,j0;	/* Starting point of the isoline.		*/
	int ix0,jy0;	/* Vector increment for entrance to [j0,i0].	*/
	int closed;	/* Logical indicator, true if isoline closes.	*/
	Gpoint pxy[];	/* Output points for labels in NDC, prioritized.*/

	{
	 int N;		/* Output count of points found for labels.	*/
	 int i,j,k,ix,jy;
	 int ixj;
	 int m,ms;
	 float xe[4],ye[4];
	 float X0,Y0;
	 float a0,a1,x0,x1,y0,y1;

	 float dx,dy,dr;
	 float xp,yp,xn,yn;
	 int II,JJ;
	 int ih,jh,is,js,ish,jsh,ixs,jys;
	 float xs,ys,xh,yh,xsh,ysh;
	 float xsp,ysp,xsn,ysn;
	 float r,rs,rh,rl,rlm;
	 float coef,ratio;
	
	 N=0;

/*			Compute the first point (x,y).			*/

	 i=i0; j=j0;
	 ix=ix0;
	 jy=jy0;
	 ixj=i+j*IM;
	 m=mask[ixj];
	 if (m > 16 || m <= 0) return N;
	 if 	 (m == 10) m=((jy<0)?11:14);
	 else if (m == 5 ) m=((ix<0)?13: 7);
	 m--;
	  a0=A[ixj+IN[m][0][0]+JN[m][0][0]*IM];
	  a1=A[ixj+IN[m][0][1]+JN[m][0][1]*IM];
	 coef=(C-a0)/(a1-a0);
	  x0=xv[i+IN[m][0][0]];
	  x1=xv[i+IN[m][0][1]];
	 xp=(x1-x0)*coef+x0;
	  y0=yv[j+JN[m][0][0]];
	  y1=yv[j+JN[m][0][1]];
	 yp=(y1-y0)*coef+y0;

/*			Project into NDC space.				*/

	 project(1,&xp,&yp);

/*			Save current point.				*/

	 X0=xp;
	 Y0=yp;
	 r=0.0;
	 rl=0.0;

/* 			Set all limits to be the current point.		*/

	 for ( k=0; k < 4; k++)
	   {
	    xe[k]=xp;
	    ye[k]=yp;
	   }

/*			Find limit points and length of line.		*/

	 while (i >= 0 &&  i < IM-1 && j >= 0 && j < JM-1)
	   {
	    xn=xp; yn=yp;

/*			Find next point (x,y).				*/

	     a0=A[ixj+IN[m][1][0]+JN[m][1][0]*IM];
	     a1=A[ixj+IN[m][1][1]+JN[m][1][1]*IM];
	    coef=(C-a0)/(a1-a0);
	     x0=xv[i+IN[m][1][0]];
	     x1=xv[i+IN[m][1][1]];
	    xp=(x1-x0)*coef+x0;
	     y0=yv[j+JN[m][1][0]];
	     y1=yv[j+JN[m][1][1]];
	    yp=(y1-y0)*coef+y0;

/*			Project into NDC space.				*/

	    project(1,&xp,&yp);

/*			Add length of segment to length.		*/

	    dx=xp-xn;
	    dy=yp-yn;
	    r=r+sqrt(dx*dx+dy*dy);

	    dx=xp-X0;
	    dy=yp-Y0;
	    rlm=sqrt(dx*dx+dy*dy);
	    rl=((rl<rlm)?rlm:rl);

/*			Adjust limits.					*/

	    if (ye[0] > yp) {xe[0]=xp; ye[0]=yp;}
	    if (xe[1] < xp) {xe[1]=xp; ye[1]=yp;}
	    if (ye[2] < yp) {xe[2]=xp; ye[2]=yp;}
	    if (xe[3] > xp) {xe[3]=xp; ye[3]=yp;}

/*			Jump to the next point of the line.		*/

	    ix=IN[m][1][0]+IN[m][1][1]-1;
	    jy=JN[m][1][0]+JN[m][1][1]-1;
	    i+=ix;
	    j+=jy;

/*			If the line closed, get out.			*/

	    if ( i == i0 && j == j0) break;

/*			Get the mask indicator.				*/

	    if (i >= 0 && i < IM && j >=0 && j < JM)
	      {
	       ixj=j*IM+ i;
	       m=mask[ixj];
	       if (m > 16 || m <= 0) break;
	       if (m == 10) m=((jy<0)?11:14);
	       else if (m == 5) m=((ix<0)?13:7);
	       m--;
	      }
	   }

/*			If the length of the line is less than twice
			the label length, no labels will be drawn.	*/

	 if (rl < rlab) return N;

/*			Find the smoothest segment
			and the mid-point of the line.			*/

	 rh=0.0;
	 xh=0.0; yh=0.0;
	 ih=0; jh=0;
	 ix=ix0; jy=jy0;
	 xs=ys=0.0;
	 is=js=0.0;

	 i=i0; j=j0;
	 xp=X0; yp=Y0;

	 ratio=0.0;

	 ixj=j*IM+ i;
	 m=mask[ixj];
	 if	 (m == 10) m=((jy>0)?14:11);
	 else if (m == 5 ) m=((ix>0)? 7:13);
	 m--;

	 while (i >= 0 && i < IM-1 && j >= 0 && j < JM-1 && m >= 0 && m < 16)
	   {
	    xn=xp; yn=yp;

	     a0=A[ixj+IN[m][1][0]+JN[m][1][0]*IM];
	     a1=A[ixj+IN[m][1][1]+JN[m][1][1]*IM];
	    coef=(C-a0)/(a1-a0);
	     x0=xv[i+IN[m][1][0]];
	     x1=xv[i+IN[m][1][1]];
	    xp=(x1-x0)*coef+x0;
	     y0=yv[j+JN[m][1][0]];
	     y1=yv[j+JN[m][1][1]];
	    yp=(y1-y0)*coef+y0;

	    project(1,&xp,&yp);

	    dx=xp-xn;
	    dy=yp-yn;
	    dr=sqrt(dx*dx+dy*dy);
	    rh=rh+dr;

/*			Check for and set the mid point.		*/

	    if ( (rh-0.5*r)*(rh-dr-0.5*r) <= 0.0 )
	      {
	       ih=i; jh=j;
	       xh=xp; yh=yp;
	      }

/*			Look ahead the length of the label for
			smoothness.  Set if it is smoothest.		*/

	    II=i;
	    JJ=j;
	    xsp=xn;
	    ysp=yn;
	    rs=0.0;
	    rlm=0.0;
	    ixs=ix;
	    jys=jy;
	    ms=m;
/*%%%%%%    while (II>=0 && II<=IM-1 && JJ>=0 && JJ<=JM-1 && ms>=0 && ms<16)*/

	    while (II>=0 && II<IM-1 && JJ>=0 && JJ<JM-1 && ms>=0 && ms<16)
	      {
	       xsn=xsp; ysn=ysp;
	       ixj=(II+JJ*IM);

		a0=A[ixj+IN[ms][1][0]+JN[ms][1][0]*IM];
		a1=A[ixj+IN[ms][1][1]+JN[ms][1][1]*IM];
	       coef=(C-a0)/(a1-a0);
		x0=xv[II+IN[ms][1][0]];
		x1=xv[II+IN[ms][1][1]];
	       xsp=(x1-x0)*coef+x0;
		y0=yv[JJ+JN[ms][1][0]];
		y1=yv[JJ+JN[ms][1][1]];
	       ysp=(y1-y0)*coef+y0;

	       project(1,&xsp,&ysp);

	       dx=xsp-xsn;
	       dy=ysp-ysn;
	       dr=sqrt(dx*dx+dy*dy);
	       rl=rl+dr;
	       dx=xsp-xn;
	       dy=ysp-yn;
	       rs=sqrt(dx*dx+dy*dy);
		  
	       if ((rl-0.5*rlab)*(rlm-0.5*rlab) <= 0.0)
		 {
		  ish=II;
		  jsh=JJ;
		  xsh=xsp;
		  ysh=ysp;
		 }
	       rlm=rl;

	       if (rl > rlab)
		 {
		  if (ratio < rs/rl)
		    {
		     is=ish; js=jsh;
		     xs=xsh; ys=ysh;
		     ratio=rs/rl;
		    }
		  break;
		 }
	       ixs=IN[ms][1][0]+IN[ms][1][1]-1;
	       jys=JN[ms][1][0]+JN[ms][1][1]-1;
	       II=II+ixs;
	       JJ=JJ+jys;
	       ms=mask[II+JJ*IM];
	       if      (ms == 10) ms=((jys>0)?14:11);
	       else if (ms == 5 ) ms=((ixs>0)?7 :13);
	       ms--;
	      }
			  
	    ix=IN[m][1][0]+IN[m][1][1]-1;
	    jy=JN[m][1][0]+JN[m][1][1]-1;
	    i+=ix;
	    j+=jy;
	    if (i == i0 && j == j0) break;
	    if (i >= 0 && i < IM && j >=0 && j < JM)
	      {
	       ixj=j*IM+i;
	       m=mask[ixj];
	       if      (m == 10) m=((jy>0)?14:11);
	       else if (m == 5 ) m=((ix>0)?7 :13);
	       m--;
	      }
	   }

	 if (closed)
	   {
	    pxy[0].x=xe[0];
	    pxy[0].y=ye[0];
	    pxy[1].x=xe[2];
	    pxy[1].y=ye[2];
	    pxy[2].x=xe[1];
	    pxy[2].y=ye[1];
	    pxy[3].x=xe[3];
	    pxy[3].y=ye[3];
	    N=4;
	   }
	 else if (ih != 0 && jh != 0)
	   {
	    pxy[0].x=xh;
	    pxy[0].y=yh;
	    N=1;
	   }
	 if (is != 0 && js != 0 && ratio > 0.8)
	   {
	    pxy[N].x=xs;
	    pxy[N].y=ys;
	    N++;
	   }
	 if (!closed)
	   {
	    pxy[N].x=xe[0];
	    pxy[N].y=ye[0];
	    pxy[N+1].x=xe[2];
	    pxy[N+1].y=ye[2];
	    pxy[N+2].x=xe[1];
	    pxy[N+2].y=ye[1];
	    pxy[N+3].x=xe[3];
	    pxy[N+3].y=ye[3];
	    N+=4;
	   }
	 return N;
	}


/*		Follow neighbor isolines and check for intersection
		with label extent rectangles.				*/

    int test_labels ( IM,JM,A,mask,nbox,i1,i2,j1,j2,C,xv,yv,N,points,iff)

 	int IM,JM;		/* Limits of the array dimension sizes.	*/
	float A[];		/* Array to be isolined.		*/
	char mask[];		/* Mask array with lines marked.	*/
	int nbox;		/* Number of boxes marked.		*/
	int i1,i2;		/* Limits in I.				*/
	int j1,j2;		/* Limits in J.				*/
	float C;		/* Isoline level.			*/
	float xv[];		/* Values of x nodes.			*/
	float yv[];		/* Values of y nodes.			*/
	int N;			/* Number of possible labels to check.	*/
	Gpoint points[6][4];	/* Extent of label text.		*/
	int iff[6];		/* Indicate label is valid.		*/

      {
	int i,j,k,m,n;
	int ixj;
	float x,y,xp[6][2],yp[6][2];
	int ix,jy,i0,j0,I,J;
	int ix0,jy0;
	int closed;
	float a0,a1,coef,x0,x1,y0,y1;
	double xrp[6][4],yrp[6][4];
	double Tr[6][2],dx,dy,r;
	float xi,yi;
	float xc,yc;

	if (nbox <= 0 || N <= 0 || N > 6) return 0;

	n=N;

/*			Set up transformations.				*/

	for (i=0; i < N; i++)
	  {
	   dx=points[i][1].x-points[i][0].x;
	   dy=points[i][1].y-points[i][0].y;
	   r=sqrt(dx*dx+dy*dy);
	   if (r < 0.0001) 
	    {
	     iff[i]=0;
	     Tr[i][0]=1.0;
	     Tr[i][1]=0.0;
	    }
	   else
	    {
	     Tr[i][0]=dx/r;	/* Cos(phi)	*/
	     Tr[i][1]=dy/r;	/* Sin(phi)	*/
	    }

/*			Transform text extents.				*/

/*			x'= x*Cos(phi)+y*Sin(phi)			*/
/*			y'=-x*Sin(phi)+y*Cos(phi)			*/
	   for (j=0; j < 4; j++)
	     {
	      xrp[i][j]= Tr[i][0]*points[i][j].x+Tr[i][1]*points[i][j].y;
	      yrp[i][j]=-Tr[i][1]*points[i][j].x+Tr[i][0]*points[i][j].y;
	     }
	  }

	for (i=i1,j=j1; j <= j2;)
	  {
/*			Break out for each level when all boxes checked.*/

	   ixj=j*IM+i;
	   if (mask[ixj] > 0 && mask[ixj] < 16)
	     {
/*			Find the beginning of the line.			*/

	      trace_begin(mask,IM,JM,i,j,&i0,&j0,&ix0,&jy0,&closed);

	      ix=ix0;
	      jy=jy0;

/*			Compute points.					*/

	      I=i0; J=j0;
	      ixj=J*IM+I;
	      m=mask[ixj];
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
	      else mask[ixj]=mask[ixj]+16;
	      m--;
		a0=A[ixj+IN[m][0][0]+JN[m][0][0]*IM];
		a1=A[ixj+IN[m][0][1]+JN[m][0][1]*IM];
	      coef=(C-a0)/(a1-a0);
		x0=xv[I+IN[m][0][0]];
		x1=xv[I+IN[m][0][1]];
	      x=(x1-x0)*coef+x0;
		y0=yv[J+JN[m][0][0]];
		y1=yv[J+JN[m][0][1]];
	      y=(y1-y0)*coef+y0;

/*			Project into NDC space.				*/

	      project(1,&x,&y);

	      for (k=0; k<N; k++)
		{
		 if (iff[k])
		   {
/*		        x'(k+1)= x cos(phi) + y sin(phi)
			y'(k+1)=-x sin(phi) + y cos(phi)		*/
		    xp[k][1]= Tr[k][0]*x+Tr[k][1]*y;
		    yp[k][1]=-Tr[k][1]*x+Tr[k][0]*y;
		   }
		}


	      while (I >= i1 && I <= i2 && J >= j1 && J <= j2)
	        {
		 for (k=0; k<N; k++)
		   {
	            xp[k][0]=xp[k][1];
	            yp[k][0]=yp[k][1];
		   }
		   a0=A[ixj+IN[m][1][0]+JN[m][1][0]*IM];
		   a1=A[ixj+IN[m][1][1]+JN[m][1][1]*IM];
	         coef=(C-a0)/(a1-a0);
		   x0=xv[I+IN[m][1][0]];
		   x1=xv[I+IN[m][1][1]];
		 x=(x1-x0)*coef+x0;
		   y0=yv[J+JN[m][1][0]];
		   y1=yv[J+JN[m][1][1]];
		 y=(y1-y0)*coef+y0;

/*			Project into NDC space.				*/

		 project(1,&x,&y);

/*			Check whether the line crosses or enters the
			label position.					*/
		 n=0;

	         for (k=0; k<N; k++)
		   {
		    if (iff[k] > 0)
		      {
/*			x'(k+1)= x cos(phi) + y sin(phi)
			y'(k+1)=-x sin(phi) + y cos(phi)		*/
		       xp[k][1]= Tr[k][0]*x+Tr[k][1]*y;
		       yp[k][1]=-Tr[k][1]*x+Tr[k][0]*y;

		       yc=(yp[k][0]-yrp[k][0])*(yp[k][0]-yrp[k][3]);
		       xc=(xp[k][0]-xrp[k][0])*(xp[k][0]-xrp[k][1]);

/*			(y'(k)-y1')(y'(k)-y4') > 0			*/
		       if (yc > 0.0)
		         {
/*			(y1'-y'(k))(y1'-y'(k+1)) <= 0			*/
		          if ((yrp[k][0]-yp[k][0])*(yrp[k][0]-yp[k][1]) <= 0.0)
			    {
			     xi=xp[k][0]+(yrp[k][0]-yp[k][0])*
				(xp[k][1]-xp[k][0])/(yp[k][1]-yp[k][0]);
/*			(xi(p1p2)-x1')(xi(p1p2)-x2') <= 0		*/
			     if ((xi-xrp[k][0])*(xi-xrp[k][1]) <= 0.0) iff[k]=0;
			    }
/*			(y4'-y'(k))(y4'-y'(k+1)) <= 0			*/
		          else
			  if ((yrp[k][3]-yp[k][0])*(yrp[k][3]-yp[k][1]) <= 0.0)
			    {
			     xi=xp[k][0]+(yrp[k][3]-yp[k][0])*
				(xp[k][1]-xp[k][0])/(yp[k][1]-yp[k][0]);
/*			(xi(p4p3)-x4')(xi(p4p3)-x3') <= 0		*/
			     if ((xi-xrp[k][3])*(xi-xrp[k][2]) <= 0.0) iff[k]=0;
			    }
		         }

/*			(x'(k)-x1')(x'(k)-x2') > 0			*/
		       if (xc > 0.0 && iff[k] > 0)
			 {
/*			(x1'-x'(k))(x1'-x'(k+1)) <= 0			*/
			  if ((xrp[k][0]-xp[k][0])*(xrp[k][0]-xp[k][1]) <= 0.0)
			    {
			     yi=yp[k][0]+(xrp[k][0]-xp[k][0])*
				(yp[k][1]-yp[k][0])/(xp[k][1]-xp[k][0]);
/*			(yi(p1p4)-y1')(yi(p1p4)-y4') <= 0.0		*/
			     if ((yi-yrp[k][0])*(yi-yrp[k][3]) <= 0.0) iff[k]=0;
			    }
/*			(x2'-x'(k))(x2'-x'(k+1)) <= 0			*/
			  else
			  if ((xrp[k][1]-xp[k][0])*(xrp[k][1]-xp[k][1]) <= 0.0)
			    {
			     yi=yp[k][0]+(xrp[k][1]-xp[k][0])*
				(yp[k][1]-yp[k][0])/(xp[k][1]-xp[k][0]);
/*			(yi(p2p3)-y2')(yi(p2p3)-y3') <= 0		*/
			     if ((yi-yrp[k][1])*(yi-yrp[k][2]) <= 0.0) iff[k]=0;
			    }
			 }
			
		       if (xc <= 0.0 && yc <= 0.0) iff[k]=0;
		      }
		    n+=iff[k];
		   }
		 if (n <= 0) return 0;

		 nbox--;  /* Reduce the count of boxes.	*/

		 ix=IN[m][1][0]+IN[m][1][1]-1;
		 jy=JN[m][1][0]+JN[m][1][1]-1;
		 I+=ix;
		 J+=jy;
		 if (I >= 0 && I < IM && J >=0 && J < JM)
		   {
		    ixj=J*IM+I;
		    m=mask[ixj];
		    if (m > 16 || m <= 0) break;
		    if (m == 10) m=((jy>0)?14:11);
		    else if (m == 5) m=((ix>0)?7:13);
		    else mask[ixj]=mask[ixj]+16;
		    m--;
		   }
		}
		
	     }

/*			Increment i and j.				*/

	   i=((i<i2)?i+1:i1);
	   if (i == i1) j++;

	  }  /*		End for i and j <= j2 loop.		*/

/*			Check the boundaries.				*/

	for (j=0;j<JM;j+=JM-1)
	  {
	      i=0;
	      x=xv[i];
	      y=yv[j];
	      project(1,&x,&y);/* Project into NDC space.*/
	      for (k=0; k<N; k++)
		{
		 if (iff[k])
		   {
		    xp[k][1]= Tr[k][0]*x+Tr[k][1]*y;
		    yp[k][1]=-Tr[k][1]*x+Tr[k][0]*y;
		   }
		}
	      for (i=1;i<IM;i++)
		{
		 for (k=0; k<N; k++)
		   {
	            xp[k][0]=xp[k][1];
	            yp[k][0]=yp[k][1];
		   }
		 x=xv[i];
		 y=yv[j];
		 project(1,&x,&y);/* Project into NDC space.*/

/*			Check whether the line crosses or enters the
			label position.					*/
		 n=0;

	         for (k=0; k<N; k++)
		   {
		    if (iff[k] > 0)
		      {
		       xp[k][1]= Tr[k][0]*x+Tr[k][1]*y;
		       yp[k][1]=-Tr[k][1]*x+Tr[k][0]*y;
		       yc=(yp[k][0]-yrp[k][0])*(yp[k][0]-yrp[k][3]);
		       xc=(xp[k][0]-xrp[k][0])*(xp[k][0]-xrp[k][1]);
		       if (yc > 0.0)
		         {
		          if ((yrp[k][0]-yp[k][0])*(yrp[k][0]-yp[k][1]) <= 0.0)
			    {
			     xi=xp[k][0]+(yrp[k][0]-yp[k][0])*
				(xp[k][1]-xp[k][0])/(yp[k][1]-yp[k][0]);
			     if ((xi-xrp[k][0])*(xi-xrp[k][1]) <= 0.0) iff[k]=0;
			    }
		          else
			  if ((yrp[k][3]-yp[k][0])*(yrp[k][3]-yp[k][1]) <= 0.0)
			    {
			     xi=xp[k][0]+(yrp[k][3]-yp[k][0])*
				(xp[k][1]-xp[k][0])/(yp[k][1]-yp[k][0]);
			     if ((xi-xrp[k][3])*(xi-xrp[k][2]) <= 0.0) iff[k]=0;
			    }
		         }
		       if (xc > 0.0 && iff[k] > 0)
			 {
			  if ((xrp[k][0]-xp[k][0])*(xrp[k][0]-xp[k][1]) <= 0.0)
			    {
			     yi=yp[k][0]+(xrp[k][0]-xp[k][0])*
				(yp[k][1]-yp[k][0])/(xp[k][1]-xp[k][0]);
			     if ((yi-yrp[k][0])*(yi-yrp[k][3]) <= 0.0) iff[k]=0;
			    }
			  else
			  if ((xrp[k][1]-xp[k][0])*(xrp[k][1]-xp[k][1]) <= 0.0)
			    {
			     yi=yp[k][0]+(xrp[k][1]-xp[k][0])*
				(yp[k][1]-yp[k][0])/(xp[k][1]-xp[k][0]);
			     if ((yi-yrp[k][1])*(yi-yrp[k][2]) <= 0.0) iff[k]=0;
			    }
			 }
		       if (xc <= 0.0 && yc <= 0.0) iff[k]=0;
		      }
		    n+=iff[k];
		   }
		 if (n <= 0) return 0;
		}
	  }

/*		Now check vertical boundaries.				*/

	for (i=0;i<IM;i+=IM-1)
	  {
	      j=0;
	      x=xv[i];
	      y=yv[j];
	      project(1,&x,&y);/* Project into NDC space.*/
	      for (k=0; k<N; k++)
		{
		 if (iff[k])
		   {
		    xp[k][1]= Tr[k][0]*x+Tr[k][1]*y;
		    yp[k][1]=-Tr[k][1]*x+Tr[k][0]*y;
		   }
		}
	      for (j=1;j<JM;j++)
		{
		 for (k=0; k<N; k++)
		   {
	            xp[k][0]=xp[k][1];
	            yp[k][0]=yp[k][1];
		   }
		 x=xv[i];
		 y=yv[j];
		 project(1,&x,&y);/* Project into NDC space.*/

/*			Check whether the line crosses or enters the
			label position.					*/
		 n=0;

	         for (k=0; k<N; k++)
		   {
		    if (iff[k] > 0)
		      {
		       xp[k][1]= Tr[k][0]*x+Tr[k][1]*y;
		       yp[k][1]=-Tr[k][1]*x+Tr[k][0]*y;
		       yc=(yp[k][0]-yrp[k][0])*(yp[k][0]-yrp[k][3]);
		       xc=(xp[k][0]-xrp[k][0])*(xp[k][0]-xrp[k][1]);
		       if (yc > 0.0)
		         {
		          if ((yrp[k][0]-yp[k][0])*(yrp[k][0]-yp[k][1]) <= 0.0)
			    {
			     xi=xp[k][0]+(yrp[k][0]-yp[k][0])*
				(xp[k][1]-xp[k][0])/(yp[k][1]-yp[k][0]);
			     if ((xi-xrp[k][0])*(xi-xrp[k][1]) <= 0.0) iff[k]=0;
			    }
		          else
			  if ((yrp[k][3]-yp[k][0])*(yrp[k][3]-yp[k][1]) <= 0.0)
			    {
			     xi=xp[k][0]+(yrp[k][3]-yp[k][0])*
				(xp[k][1]-xp[k][0])/(yp[k][1]-yp[k][0]);
			     if ((xi-xrp[k][3])*(xi-xrp[k][2]) <= 0.0) iff[k]=0;
			    }
		         }
		       if (xc > 0.0 && iff[k] > 0)
			 {
			  if ((xrp[k][0]-xp[k][0])*(xrp[k][0]-xp[k][1]) <= 0.0)
			    {
			     yi=yp[k][0]+(xrp[k][0]-xp[k][0])*
				(yp[k][1]-yp[k][0])/(xp[k][1]-xp[k][0]);
			     if ((yi-yrp[k][0])*(yi-yrp[k][3]) <= 0.0) iff[k]=0;
			    }
			  else
			  if ((xrp[k][1]-xp[k][0])*(xrp[k][1]-xp[k][1]) <= 0.0)
			    {
			     yi=yp[k][0]+(xrp[k][1]-xp[k][0])*
				(yp[k][1]-yp[k][0])/(xp[k][1]-xp[k][0]);
			     if ((yi-yrp[k][1])*(yi-yrp[k][2]) <= 0.0) iff[k]=0;
			    }
			 }
		       if (xc <= 0.0 && yc <= 0.0) iff[k]=0;
		      }
		    n+=iff[k];
		   }
		 if (n <= 0) return 0;
		}
	  }
	return 0;
      }

/*	Test for existence of a crossing in the grid box.		*/

    int set_mask (c,A,mask,IM,JM,nbox,i1,i2,j1,j2)

	float c;		/* Level for isoline.			*/
	float A[];		/* Test four values of the grid box.	*/
				/*  a4------a3				*/
				/*   |      |				*/
				/*  a1------a2				*/
	char mask[];		/* Indicate crossings.			*/
	int IM,JM;		/* Size of 2D array.			*/
	int *nbox;		/* Number of boxes active.		*/
	int *i1,*i2,*j1,*j2;	/* Bound of lines in the array.		*/

      {
       int i,j,ixj,nx;


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
	     if (A[ixj] 	> c) nx+=1;
	     if (A[ixj+1] 	> c) nx+=2;
	     if (A[ixj+1+IM]	> c) nx+=4;
	     if (A[ixj+IM] 	> c) nx+=8;
	     if (nx == 15) nx=0;
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

/*	Find the beginning of a countour line starting at a point on
	the line.  Assume the mask has been set.			*/

      int trace_begin(mask,IM,JM,i,j,I0,J0,IX,JY,closed)

	char mask[];	/*  Mask with line indications.	 Four data	*/
			/*  values defined at the corners of the mask	*/
			/*  grid determine the line crossings the each	*/
			/*  isoline mask value.				*/
	int IM;		/*  Number of values of the first row.		*/
	int JM;		/*  Number of rows.				*/
	int i,j;	/*  First point that was found on the line.	*/
	int *I0,*J0;	/*  Beginning point of the line.		*/
	int *IX,*JY;	/*  Vector increment for entering the box.	*/
	int *closed;	/*  Indicate whether the line is closed.	*/
	{

	 int ix,jy;
	 int m;
	 int n=0;
	 int I,J;

	 ix=-1; jy=-1;
	 *I0=I=i; *J0=J=j;
	 *closed=0;

	 while (I>=0&&I<IM-1&&J>=0&&J<JM-1&&(m=mask[I+J*IM])>0&&m<16)
	   {
	    if (m > 16 || m <= 0) break;
	    if      (m == 5 ) m=((jy>0)?13:7);
	    else if (m == 10) m=((ix<0)?11:14);
	    *I0=I;  *J0=J;
	    m--;
	    ix=IN[m][0][0]+IN[m][0][1]-1;
	    jy=JN[m][0][0]+JN[m][0][1]-1;
	    I+=ix;
	    J+=jy;
	    if (*I0 == i && *J0 == j && n++ > 0)
	      {
	       *closed=1;
	       break;
	      }
	   }
         *IX=-ix;
	 *JY=-jy;

	 return 1;
	}


/*			Erase the line.					*/

      int erase_line(mask,IM,JM,i0,j0,ix0,jy0)

	char mask[];	/* Isoline mask[JM,IM], JM-1 and IM-1 values.	*/
	int IM,JM;	/* Size of mask array.				*/
	int i0,j0;	/* Starting point of the isoline.		*/
	int ix0,jy0;	/* Vector increment for entrance to [j0,i0].	*/
	{
	 int i,j,ix,jy;
	 int ixj;
	 int m;

/*			Set to the first point.				*/

	 i=i0; j=j0;
	 ix=ix0;
	 jy=jy0;

	 while (i>=0&&i<IM-1&&j>=0&&j<JM-1&&(m=mask[(ixj=i+j*IM)])<16&&m>0)
	   {
	    if 	    (m == 10) {m=((jy<0)?11:14); mask[ixj]=((jy<0)?14:11);}
	    else if (m == 5 ) {m=((ix<0)?13: 7); mask[ixj]=((ix<0)? 7:13);}
	    else mask[ixj]=mask[ixj]+16;
	    m--;
/*			Jump to the next point of the line.		*/

	    ix=IN[m][1][0]+IN[m][1][1]-1;
	    jy=JN[m][1][0]+JN[m][1][1]-1;
	    i+=ix;
	    j+=jy;
	   }

	 return 0;
	}

/*		Check that labels don't overlay each other.		*/

      int check_label(lab,n0,n1,point,iff)
	Gpoint lab[2][100][4];
	int n0,n1;
	Gpoint point[4];
	int *iff;
	{
	 int j,k,n;
	 float xl,xu,yl,yu;
	 float XL,XU,YL,YU;
	 float xmin,xmax,ymin,ymax;
	 int ne;

	 xl=point[0].x;
	 xu=xl;
	 yl=point[0].y;
	 yu=yl;
	 for (j=1;j<4;j++)
	   {
	    xl=(xl>point[j].x)?point[j].x:xl;
	    xu=(xu<point[j].x)?point[j].x:xu;
	    yl=(yl>point[j].y)?point[j].y:yl;
	    yu=(yu<point[j].y)?point[j].y:yu;
	   }
	 for (ne=n0,k=0;k<2;ne=n1,k++)
	   {
	    for (n=0;n<ne;n++)
	      {
	       XL=lab[k][n][0].x;
	       XU=XL;
	       YL=lab[k][n][0].y;
	       YU=YL;
	       for (j=1;j<4;j++)
	         {
	          XL=(XL>lab[k][n][j].x)?lab[k][n][j].x:XL;
	          XU=(XU<lab[k][n][j].x)?lab[k][n][j].x:XU;
	          YL=(YL>lab[k][n][j].y)?lab[k][n][j].y:YL;
	          YU=(YU<lab[k][n][j].y)?lab[k][n][j].y:YU;
	         }
	       xmin=(xl>XL)?xl:XL;
	       xmax=(xu<XU)?xu:XU;
	       ymin=(yl>YL)?yl:YL;
	       ymax=(yu<YU)?yu:YU;
	       if (xmin-xmax < 0.01 && ymin-ymax < 0.01) {*iff=0; return 0;}
	      }
	   }
	 return 0;
	}

/*			Display the legend for isolines.		*/

      int legendGi(int nc,float C[],int Cid[],struct iso *piso,struct p_tab *pP)

	{
	 int i,j,k;
	 int ni,nid[75];
	 int err;
	 float dx,dy,x,y,v;
	 char str[256];
	 struct pe_leg pe;
	 struct iso *po;
	 Gpoint pxy[20];
	 Gpoint vxy;
         Gintlist wsid;
	 Gint opwk;
	 int *pwk;
	
	 struct table_text *pTt;
	 struct table_chorn *pTo;
	 struct table_line *pl;
	 Gtxalign pta;
	 Gtxpath ptp;
	 Gextent extent;

	 int horiz;

	 float sizemn=0.005;

	 if (nc <= 0 || nc > 50)
	   {
	    err_warn(1,fperr,
		"Error - isoline legend has no lines or too many lines.\n");
	    return 0;
	   }

	 pe=pP->leg;
	 if (pe.x1 <= 0.0 || pe.x1 >= 1.0 || pe.x2 <= 0.0 || pe.x2 >= 1.0 ||
	     pe.y1 <= 0.0 || pe.y1 >= 1.0 || pe.y2 <= 0.0 || pe.y2 >= 1.0 ||
	     pe.p <= 0)
	    return 1;

	 for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
	    if (strcmp(pTt->name,pe.tb) == 0) break;
	 if (pTt == NULL)
	   {
	    pTt=&Tt_tab;
	    err_warn(0,fperr,
			"Warning - no valid text attributes for legend.\n");
	   }
	 for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
	    if (strcmp(pTo->name,pe.to) == 0) break;
	 if (pTo == NULL)
	   {
	    pTo=&To_tab;
	    err_warn(0,fperr,
		"Warning - no valid text orientation attributes for legend.\n");
	   }
	 horiz=0;
	 if ((dx=fabs(pe.x2-pe.x1)) > (dy=fabs(pe.y2-pe.y1))) horiz=1;
	 if ((horiz && dx<6.0*sizemn) || (!horiz && dy<6.0*sizemn))
	   {
	    /*err_warn(1,fperr,
		"Error - ISOLINE legend space is less than min.\n");*/
	    return 0;
	   }

/*			Count the number of different isoline
			descriptors used.				*/

	 for (ni=i=0;i<nc;i++)
	   {
	    for (j=0;j<ni;j++) if (Cid[i] == nid[j]) break;
	    if (j == ni) nid[ni++]=Cid[i];
	   }
/*			Check if there is enough space for 'ni'
			descriptors.					*/

	 if ((horiz && dy < sizemn*ni) || (!horiz && dx < sizemn*ni) )
	   {
	    /*err_warn(1,fperr,"Error - ISOLINE legend space is too small.\n");*/
	    return 0;
	   }

/*			Set text attributes.				*/

	 set_text_attr(pTt,pTo);

/*			Take care of special case for legends:
			up-vector, alignment, and path.			*/

	 v=pTo->chua;
	 while (v > 360.0) v=v-360.0;
	 while (v < 0.0) v=v+360.0;
	 k=(v+45.0)/90.0;
	 v=k*90.0;
	   x=vxy.x=sin((3.1415926536/180.0)*v);
	   y=vxy.y=cos((3.1415926536/180.0)*v);
	 gschup(&vxy);
	   pta.hor=GTH_LEFT;
	   pta.ver=GTV_HALF;
	 gstxal(&pta);
	   ptp=GTP_RIGHT;
	 gstxp(ptp);

/*			Check whether a workstation is open.		*/
	 opwk=0;
         wsid.number = 0;
         wsid.integers = NULL;
	 gqopwk(&wsid);
	 for (i=0,pwk=wsid.integers; i < wsid.number;pwk++,i++)
	   {
	    if (*pwk != 7) {opwk=*pwk; break;}
	   }
	 if (wsid.number > 0 && wsid.integers != NULL)
	     free((char *)wsid.integers);
	 if (opwk == 0)
	   {
	    err_warn(0,fperr,"Warning - (legendGi) no workstations open.\n");
	   }
/*			Set up a text string of levels.			*/

	 for (k=0;k<ni;k++)
	   {
	    po=piso;
	    if (po != NULL)
	      {
	       while (po != NULL)
	         {
	          if (nid[k] == po->id)
		    {
		     pl=&Tl_tab;
		     strncpy(str,po->lb,17); str[16] = '\0';
		     if (strlen(str) <= (size_t) 0) {
                        strncpy(str,"default",17); str[16] = '\0';
	             }
		     for (;pl!=NULL && strcmp(str,pl->name)!=0;pl=pl->next);
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
                     if (pl->lci == NULL)
                        gsplci(241);
                     else
                        gsplci(pl->lci[0]);
		     break;
		    }
	          po=po->next;
	         }
	      }
	    else
	      {
	       pl=&Tl_tab;
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
	      }
	    if (y > 0.5)  /*		Upright.			*/
	      {
	       pxy[0].x=pe.x1;
	       pxy[0].y=pe.y2-(k+0.5)*pTo->chh;
	       pxy[1].x=pxy[0].x+0.03;
	       pxy[1].y=pxy[0].y;
	       gpl(2,pxy);

	       pxy[2].x=pxy[1].x+pTo->chh;
	       pxy[2].y=pxy[1].y;
	      }
	    if (y < -0.5)  /*		Upside-down.			*/
	      {
	       pxy[0].x=pe.x2;
	       pxy[0].y=pe.y1+(k+0.5)*pTo->chh;
	       pxy[1].x=pxy[0].x-0.03;
	       pxy[1].y=pxy[0].y;
	       gpl(2,pxy);

	       pxy[2].x=pxy[1].x-pTo->chh;
	       pxy[2].y=pxy[1].y;
	      }
	    else if (x > 0.5)  /*	Rotate clockwise 90 deg.	*/
	      {
	       pxy[0].x=pe.x2-(k+0.5)*pTo->chh;
	       pxy[0].y=pe.y2;
	       pxy[1].x=pxy[0].x;
	       pxy[1].y=pxy[0].y-0.03;
	       gpl(2,pxy);

	       pxy[2].x=pxy[1].x;
	       pxy[2].y=pxy[1].y-pTo->chh;
	      }
	    else if (x < -0.5)  /* Rotate counter-clockwise 90 deg.	*/
	      {
	       pxy[0].x=pe.x1+(k+0.5)*pTo->chh;
	       pxy[0].y=pe.y1;
	       pxy[1].x=pxy[0].x;
	       pxy[1].y=pxy[0].y+0.03;
	       gpl(2,pxy);

	       pxy[2].x=pxy[1].x;
	       pxy[2].y=pxy[1].y+pTo->chh;
	      }
	    for (j=i=0;i<nc;i++)
	     if (Cid[i] == nid[k])
	       {
	        sprintf(str,"%g ",C[i]);
/*		Find text extents.					*/

		if (opwk != 0)
#ifdef CAIRODRAW
			cairogqtxx(opwk,pxy[2],(unsigned char *)str,&extent);
#else
			gqtxx(opwk,pxy[2],(unsigned char *)str,&extent);
#endif		else
			getextent(&(pxy[2]),str,pTt,pTo,&extent);

		if (pe.x1 > extent.concat.x ||
				 pe.x2 < extent.concat.x ||
		    pe.y1 > extent.concat.y ||
				 pe.y2 < extent.concat.y)
		  {
		   gtx(&pxy[2],(unsigned char *)"...");
		   break;
		  }
		gtx(&pxy[2],(unsigned char *)str);
		pxy[2].x=extent.concat.x;
		pxy[2].y=extent.concat.y;
		
	       }
	   }
	 return 1;
	}
