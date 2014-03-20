#include "gks.h"
#include "gksshort.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "list.h"
#include "project.h"
#include "picture.h"
#include "graph.h"
#include "display.h"
#include "workstations.h"
#include "vcs_legacy_marker.h"

#define STRMAX 256

extern Gpoint VCS2PSDEVICE();
extern struct a_tab A_tab;
extern struct p_tab Pic_tab;

extern struct display_tab D_tab;
extern struct displays d_type[NTYPES];

extern struct gi_tab Gi_tab;
extern struct go_tab Go_tab;
extern struct gcon_tab Gcon_tab;
extern struct gfi_tab Gfi_tab;
extern struct gfm_tab Gfm_tab;
extern struct gfo_tab Gfo_tab;
extern struct gfb_tab Gfb_tab;
extern struct gv_tab Gv_tab;
extern struct gXy_tab GXy_tab;
extern struct gYx_tab GYx_tab;
extern struct gXY_tab GXY_tab;
extern struct gSp_tab GSp_tab;
extern cairo_surface_t *logo;
extern cairo_pattern_t *logo_p;
extern int logo_width,logo_height;
extern int Inactive;
extern int user_defer_update;

extern int update_ind;

extern Gconid_X_drawable connect_id;

extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */
extern int convertP_to_landscape_portrait(struct p_tab * pp );
/* int updating = 0 ; */
#ifdef QTWM
extern int QtWorking;
#endif
extern int cairoIsSetup;
extern     int err_warn (int beep,FILE *fp,char *fmt,...);
extern int XW,YW;
#ifdef USEX11
int updating =0;
void vcs_legacy_acquire_update(){
  //fprintf(stderr,"acquiring : %i\n",updating);
  updating++;
  while (updating!=1)
    {
/*       if (updating>1) printf("wow!!! updating is : %i\n",updating); */
      /* 	    printf("updating none zero:%i\n",updating); */
    }
}
void vcs_legacy_release_update() {
  //fprintf(stderr,"releasing : %i\n",updating);
  updating--;
}
#else
void vcs_legacy_acquire_update();
void vcs_legacy_release_update();
#endif



int templateRatio(struct p_tab *pp, int wkst_id) {
  struct p_tab *pp2;
  Grectangle xwa,swa;
  double Rout,Rt,Ra,Rwished;
  double delta,dX1,dX2,dY1,dY2;

  if (pp->dsp.ratio==-999) {
    return 0;
  }

  Rwished = fabs(pp->dsp.ratio);

#if defined (QTWM)
  extern void vcs_legacy_Qt_get_window_dimensions_by_id(int id,int *x, int *y,int *w,int *h);
#endif
  
  vcs_legacy_Qt_get_window_dimensions_by_id(wkst_id,&xwa.x,&xwa.y,&xwa.width,&xwa.height);
  Rout = (float)xwa.width/(float)xwa.height;
  Rt = (pp->dsp.y2-pp->dsp.y1)/(pp->dsp.x2-pp->dsp.x1);
  Ra = Rt/Rout;
  if (Rwished>Ra) {
    Ra = Ra/Rwished;
    delta = .5*(pp->dsp.x2-pp->dsp.x1)*(1-Ra);
    pp->dsp.x1 = pp->dsp.x1+delta;
    pp->dsp.x2 = pp->dsp.x2-delta;
  }
  else {
    Ra = Rwished/Ra;
    delta = .5*(pp->dsp.y2-pp->dsp.y1)*(1-Ra);
    pp->dsp.y1 = pp->dsp.y1+delta;
    pp->dsp.y2 = pp->dsp.y2-delta;
  }
  if (pp->dsp.ratio<0) { /* ok negative means move ticks as well */
    pp->b1.x1 = pp->dsp.x1;
    pp->b1.x2 = pp->dsp.x2;
    pp->b1.y1 = pp->dsp.y1;
    pp->b1.y2 = pp->dsp.y2;
    dY1 = pp->xl1.y-pp->xt1.y1;
    dY2 = pp->xl2.y-pp->xt2.y1;
    dX1 = pp->yl1.x-pp->yt1.x1;
    dX2 = pp->yl2.x-pp->yt2.x1;
    delta = pp->xt1.y2-pp->xt1.y1;
    pp->xt1.y1 = pp->dsp.y1;
    pp->xt1.y2 = pp->xt1.y1+delta*Ra;
    delta = pp->xt2.y2-pp->xt2.y1;
    pp->xt2.y1 = pp->dsp.y1;
    pp->xt2.y2 = pp->xt2.y1+delta*Ra;
    delta = pp->xmta.y2-pp->xmta.y1;
    pp->xmta.y1=pp->dsp.y1;
    pp->xmta.y2=pp->xmta.y1+delta*Ra;
    delta = pp->xmtb.y2-pp->xmtb.y1;
    pp->xmtb.y1=pp->dsp.y1;
    pp->xmtb.y2=pp->xmtb.y1+delta*Ra;
    delta = pp->yt1.x2-pp->yt1.x1;
    pp->yt1.x1 = pp->dsp.x1;
    pp->yt1.x2 = pp->yt1.x1+delta*Ra;
    delta = pp->yt2.x2-pp->yt2.x1;
    pp->yt2.x1 = pp->dsp.x1;
    pp->yt2.x2 = pp->yt2.x1+delta*Ra;
    delta = pp->ymta.x2-pp->ymta.x1;
    pp->ymta.x1=pp->dsp.x1;
    pp->ymta.x2=pp->ymta.x1+delta*Ra;
    delta = pp->ymtb.x2-pp->ymtb.x1;
    pp->ymtb.x1=pp->dsp.x1;
    pp->ymtb.x2=pp->ymtb.x1+delta*Ra;
    pp->xl1.y = pp->xt1.y1 + dY1*Ra;
    pp->xl2.y = pp->xt2.y1 + dY2*Ra;
    pp->yl1.x = pp->yt1.x1 + dX1*Ra;
    pp->yl2.x = pp->yt2.x1 + dX2*Ra;
    
  }
  return 0;
};


int vcs_legacy_canvas_update ( short use_defer_flg )
{

  int i,j,k;
  int change;
  int erret;
  int ierr;
  struct a_tab *pa,*pb;
  struct p_tab *pp, *ppf;
  struct display_tab *pd;
  struct gi_tab *pgi;
  struct go_tab *pgo;
  struct gcon_tab *pgcon;
  struct gfi_tab *pgfi;
  struct gfm_tab *pgfm;
  struct gfo_tab *pgfo;
  struct gfb_tab *pgfb;
  struct gv_tab *pgv;
  struct gXy_tab *pgXy;
  struct gYx_tab *pgYx;
  struct gXY_tab *pgXY;
  struct gSp_tab *pgSp;
  Gint wks;

  int *pi;

  char tmpnm[256];
  int store_Dc;
  extern struct default_continents Dc;
  extern float plnorm(int x_or_y, float value);
  extern int killP(struct p_tab *p);
  /*         static int updating = 0; */
  //fprintf(stderr,"in canvas_update, acquiring\n");
   vcs_legacy_acquire_update();

  erret=0;
  //printf("updating is: %i\n",update_ind);
/*   if (!update_ind) {vcs_legacy_release_update(); return 1;} */
  //	printf("actually doing something\n");
  update_ind=0;

  wks=check_canvas_defer();

  if (D_tab.name[0] == '\0') 
    {
      vcs_legacy_release_update();
      return 1;
    }


  /*			Search the displays to determine whether
                they need updating.				*/
  for (pd=&D_tab;pd != NULL;pd=pd->next) {
    if ( (use_defer_flg == 1) || ((wks < 8) || (wks == pd->wkst_id)) ) {
      if (pd->type[0] == '\0') continue;
      ierr=0;

      if (pd->type[0] == '\0' || pd->g_name[0] == '\0' ||
          pd->p_name[0] == '\0' || pd->off == 1)
        {
	      if (pd->off == 0) pd->off=2;
	      erret++;
	      continue;
        }
      /*	   for (j=0;j<NTYPES;j++)
               {
               if (cmpncs(d_type[j].type,pd->type) == 0)
               {
               for (k=0;k<d_type[j].na;k++)
               {
               if (pd->a[k][0] == '\0')
		       {
               if (pd->off == 0) pd->off=2;
               ierr++;
               continue;
		       }
               }
               }
               }
      */
      if (pd->off == 2) pd->off=0;

      for (i=0;i<pd->na;i++)
		if (pd->a[i][0] == '\0')
          {
            pd->off=2;
            ierr++;
            erret++;
            continue;
          }
      if (ierr) continue;

      /*			Find the picture template being used.		*/

      ppf=&Pic_tab;
      while (ppf != NULL )
        {
	      if (strcmp(ppf->name,pd->p_name) == 0) break;
	      ppf=ppf->next;
        }
      if (ppf == NULL)
        {
	      err_warn(1,fperr,"Error - display template (%s) not found.\n",
                   pd->p_name);
	      pd->off=2;
	      erret++;
	      continue;
        }
      /*              Create a new table structure and copy to it its new locations base on the 
                      VCS Canvases page orientation (i.e., landscape or portrait).                */

      if((pp=(struct p_tab *)malloc(sizeof(struct p_tab)))==NULL) {
        err_warn(1,fperr,
                 "Error - memory for getting picture template( gm_template_hold ) not found.\n");
      	vcs_legacy_release_update();
        return 1;
      }

      strcpy( pp->name,"gm_template_hd" );
      copyP_attr(ppf,pp);
      templateRatio(pp,pd->wkst_id-7);

      convertP_to_landscape_portrait( pp );

      /*			Find the type of graphics display.		*/
      for (i=0;pp != NULL && i<NTYPES ;i++)
        {
/*           printf("In update, i: %d, pd->type is: %s, %s\n",i,pd->type,d_type[i].type); */
          if (cmpncs(pd->type,d_type[i].type) == 0)
	        {

              /*			Handle a primitive.			*/
              if (i >= 12) {
                change=0;
/*                 printf("i for primitive is: %d , %s\n",i,pd->type); */
		    
                if (pd->dsp_seg[3]!=0 ) 
                  {
                    change=1;
                    gdsg(pd->dsp_seg[0]);
                    pd->dsp_seg[3]=0;
                  }
/*                 change=1; */
                if (change==1)
                  {
                    vcs_legacy_draw_primatives(pd->name);
                  }
              }
              /*			Handle an ISOLINE display.			*/
              else if (i == 0)
                {

                  /*				Find the graphics attributes.		*/

                  pgi=&Gi_tab;
                  while (pgi != NULL)
                    {
                      if (strcmp(pd->g_name,pgi->name) == 0) break;
                      pgi=pgi->next;
                    }

                  /*				Find the data array.			*/

                  pa=&A_tab;
                  while (pgi != NULL && pa != NULL)
                    {
                      if (strlen(pa->name)>(size_t)0 && pa->pA_attr->notok==0
                          && strcmp(pa->name,pd->a[0])==0) break;
                      pa=pa->next;
                    }

                  /*				Delete affected segments and redraw.	*/

                  if (pgi != NULL && pp != NULL && pa != NULL)
                    {

                      /*				Delete segments that are in use.	*/

                      change=0;

                      for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                        {
                          if (*pi > 0 && *(pi+3) > 0)
                            {
                              gdsg(*pi);
                              *pi=0;
                              change=1;
                            }
                          else if (*(pi+3) > 0) change=1;
                        }
                      if (wks > 0) {
                        if ((Inactive==1) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                        else if ((Inactive==0) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                      }
                      if (change > 0)
                        {
                          if (isolines(pd,pgi->pGi_attr,pp,pa) == 0)
                            {
                              pd->off=2;
                              erret++;
                            }
                          for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                            *(pi+3) = 0;
                        }
                    }
                  else
                    {
                      if ( (pa != NULL) && (!pa->FROM_CDAT) ) {
                        if (pgi == NULL) err_warn(1,fperr,
                                                  "Error - isoline method (%s) not found.\n",pd->g_name);
                      } else if (pa == NULL) {
                        err_warn(1,fperr,"Error - isoline data"
                                 " (%s) not found or not ok.\n",pd->a[0]);
                      }
                      erret++;
                      if (pd->dsp_seg[0] > 0)
                        {
                          gdsg(pd->dsp_seg[0]);
                          if (wks > 0) {
                            if ((Inactive==1) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                            else if ((Inactive==0) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                          }
                        }
                      pd->dsp_seg[1]=0;
                      pd->dsp_seg[3]=0;
                      pd->off=2;
                      continue;
                    }
                }

              /*			Handle an OUTLINE display.			*/

              else if (i == 1)
                {

                  /*				Find the graphics attributes.		*/

                  pgo=&Go_tab;
                  while (pgo != NULL)
                    {
                      if (strcmp(pd->g_name,pgo->name) == 0) break;
                      pgo=pgo->next;
                    }

                  /*				Find the data array.			*/

                  pa=&A_tab;
                  while (pgo != NULL && pa != NULL)
                    {
                      if (strlen(pa->name)>(size_t)0 && pa->pA_attr->notok==0 
                          && strcmp(pa->name,pd->a[0]) == 0) break;
                      pa=pa->next;
                    }

                  /*				Delete affected segments and redraw.	*/

                  if (pgo != NULL && pp != NULL && pa != NULL)
                    {

                      /*				Delete segments that are in use.	*/

                      change=0;

                      for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                        {
                          if (*pi > 0 && *(pi+3) > 0)
                            {
                              gdsg(*pi);
                              *pi=0;
                              change=1;
                            }
                          else if (*(pi+3) > 0) change=1;
                        }
                      if (wks > 0) {
                        if ((Inactive==1) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                        else if ((Inactive==0) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                      }

                      if (change > 0)
                        {
                          outlines (pd,pgo->pGo_attr,pp,pa);
                          for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                            *(pi+3) = 0;
                        }
                    }
                  else
                    {
                      if ( (pa != NULL) && (!pa->FROM_CDAT) ) {
                        if (pgo == NULL) err_warn(1,fperr,
                                                  "Error - outline method (%s) not found.\n",pd->g_name);
                      } else if (pa == NULL) {
                        err_warn(1,fperr,"Error - outline data"
                                 " (%s) not found or not ok.\n",pd->a[0]);
                      }
                      if (pd->dsp_seg[0] > 0)
                        {
                          gdsg(pd->dsp_seg[0]);
                          if (wks > 0) {
                            if ((Inactive==1) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                            else if ((Inactive==0) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                          }
                        }
                      pd->dsp_seg[1]=0;
                      pd->dsp_seg[3]=0;
                      pd->off=2;
                      erret++;
                      continue;
                    }
                }

              /*			Handle a CONTINENTS display.			*/

              else if (i == 2)
                {

                  /*				Find the graphics attributes.		*/

                  pgcon=&Gcon_tab;
                  while (pgcon != NULL)
                    {
                      if (strcmp(pd->g_name,pgcon->name) == 0) break;
                      pgcon=pgcon->next;
                    }

                  /*				Delete affected segments and redraw.	*/

                  if (pgcon != NULL && pp != NULL)
                    {

                      /*				Delete segments that are in use.	*/

                      change=0;

                      for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                        {
                          if (*pi > 0 && *(pi+3) > 0)
                            {
                              gdsg(*pi);
                              *pi=0;
                              change=1;
                            }
                          else if (*(pi+3) > 0) change=1;
                        }
                      if (wks > 0) {
                        if ((Inactive==1) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                        else if ((Inactive==0) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                      }
                      if (change > 0)
                        {
                          store_Dc = Dc.selected;
                          Dc.selected = pgcon->pGcon_attr->cont_type-1;
                          if (Dc.selected < 2)
                            Dc.selected = 1;
                          continents (pp,pd,pgcon);
                          Dc.selected = store_Dc;
                          for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                            *(pi+3) = 0;
                        }
                    }
                  else
                    {
                      if ( (pa != NULL) && (!pa->FROM_CDAT) ) {
                        if (pgcon == NULL) err_warn(1,fperr,
                                                    "Error - continents method (%s) not found.\n",
                                                    pd->g_name);
                      }
                      if (pd->dsp_seg[0] > 0)
                        {
                          gdsg(pd->dsp_seg[0]);
                          if (wks > 0) {
                            if ((Inactive==1) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                            else if ((Inactive==0) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                          }
                        }
                      pd->dsp_seg[1]=0;
                      pd->dsp_seg[3]=0;
                      pd->off=2;
                      erret++;
                      continue;
                    }
                }

              /*			Handle an ISOFILL display.			*/

              else if (i == 3)
                {

                  /*				Find the graphics attributes.		*/

                  pgfi=&Gfi_tab;
                  while (pgfi != NULL)
                    {
                      if (strcmp(pd->g_name,pgfi->name) == 0) break;
                      pgfi=pgfi->next;
                    }

                  /*				Find the data array.			*/

                  pa=&A_tab;
                  while (pgfi != NULL && pa != NULL)
                    {
                      if (strlen(pa->name)>(size_t)0 && pa->pA_attr->notok==0 
                          && strcmp(pa->name,pd->a[0]) == 0) break;
                      pa=pa->next;
                    }

                  /*				Delete affected segments and redraw.	*/

                  if (pgfi != NULL && pp != NULL && pa != NULL)
                    {

                      /*				Delete segments that are in use.	*/

                      change=0;

                      for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                        {
                          if (*pi > 0 && *(pi+3) > 0)
                            {
                              gdsg(*pi);
                              *pi=0;
                              change=1;
                            }
                          else if (*(pi+3) > 0) change=1;
                        }
                      if (wks > 0) {
                        if ((Inactive==1) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                        else if ((Inactive==0) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                      }

                      if (change > 0)
                        {
                          if (isofills(pd,pgfi->pGfi_attr,pp,pa)==0)
                            {
                              if (pd->dsp_seg[0] > 0)
                                gdsg(pd->dsp_seg[0]);
                              if (wks > 0) {
                                if ((Inactive==1) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                                else if ((Inactive==0) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                              }
                              pd->dsp_seg[1]=0;
                              pd->dsp_seg[3]=0;
                              pd->off=2;
                              erret++;
                            }
                          for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                            *(pi+3) = 0;
                        }
                    }
                  else
                    {
                      if ( (pa != NULL) && (!pa->FROM_CDAT) ) {
                        if (pgfi == NULL) err_warn(1,fperr,
                                                   "Error  -isofill method (%s) not found.\n",pd->g_name);
                      } else if (pa == NULL) {
                        err_warn(1,fperr,"Error - isofill data"
                                 " (%s) not found or not ok.\n",pd->a[0]);
                      }
                      if (pd->dsp_seg[0] > 0)
                        {
                          gdsg(pd->dsp_seg[0]);
                          if (wks > 0) {
                            if ((Inactive==1) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                            else if ((Inactive==0) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                          }
                        }
                      pd->dsp_seg[1]=0;
                      pd->dsp_seg[3]=0;
                      pd->off=2;
                      erret++;
                      continue;
                    }
                }
              /*			Handle an OUTLINE FILL display.			*/

              else if (i == 4)
                {

                  /*				Find the graphics attributes.		*/

                  pgfo=&Gfo_tab;
                  while (pgfo != NULL)
                    {
                      if (strcmp(pd->g_name,pgfo->name) == 0) break;
                      pgfo=pgfo->next;
                    }

                  /*				Find the data array.			*/

                  pa=&A_tab;
                  while (pgfo != NULL && pa != NULL)
                    {
                      if (strlen(pa->name)>(size_t)0 && pa->pA_attr->notok==0 
                          && strcmp(pa->name,pd->a[0]) == 0) break;
                      pa=pa->next;
                    }

                  /*				Delete affected segments and redraw.	*/

                  if (pgfo != NULL && pp != NULL && pa != NULL)
                    {

                      /*				Delete segments that are in use.	*/

                      change=0;

                      for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                        {
                          if (*pi > 0 && *(pi+3) > 0)
                            {
                              gdsg(*pi);
                              *pi=0;
                              change=1;
                            }
                          else if (*(pi+3) > 0) change=1;
                        }
                      if (wks > 0) {
                        if ((Inactive==1) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                        else if ((Inactive==0) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                      }
                      if (change > 0)
                        {
                          outfills (pd,pgfo->pGfo_attr,pp,pa);
                          for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                            *(pi+3) = 0;
                        }
                    }
                  else
                    {
                      if ( (pa != NULL) && (!pa->FROM_CDAT) ) {
                        if (pgfo == NULL) err_warn(1,fperr,
                                                   "Error - outfill method (%s) not found.\n",pd->g_name);
                      } else if (pa == NULL) {
                        err_warn(1,fperr,"Error - outfill data"
                                 " (%s) not found or not ok.\n",pd->a[0]);
                      }
                      if (pd->dsp_seg[0] > 0)
                        {
                          gdsg(pd->dsp_seg[0]);
                          if (wks > 0) {
                            if ((Inactive==1) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                            else if ((Inactive==0) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                          }
                        }
                      pd->dsp_seg[1]=0;
                      pd->dsp_seg[3]=0;
                      pd->off=2;
                      erret++;
                      continue;
                    }
                }

              /*			Handle a boxfill display.			*/

              else if (i == 5)
                {

                  /*				Find the graphics attributes.		*/

                  pgfb=&Gfb_tab;
                  while (pgfb != NULL)
                    {
                      if (strcmp(pd->g_name,pgfb->name) == 0) break;
                      pgfb=pgfb->next;
                    }

                  /*				Find the data array.			*/

                  pa=&A_tab;
                  while (pgfb != NULL && pa != NULL)
                    {
                      if (strlen(pa->name)>(size_t)0 && pa->pA_attr->notok==0 
                          && strcmp(pa->name,pd->a[0]) == 0) break;
                      pa=pa->next;
                    }

                  /*				Delete affected segments and redraw.	*/

                  if (pgfb != NULL && pp != NULL && pa != NULL)
                    {

                      /*				Delete segments that are in use.	*/

                      change=0;

                      for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                        {
                          if (*pi > 0 && *(pi+3) > 0)
                            {
                              gdsg(*pi);
                              *pi=0;
                              change=1;
                            }
                          else if (*(pi+3) > 0) change=1;
                        }
                      if (wks > 0) {
                        if ((Inactive==1) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                        else if ((Inactive==0) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                      }
                      if (change > 0)
                        {
                          if (boxfills(pd,pgfb->pGfb_attr,pp,pa)==0)
                            {
                              if (pd->dsp_seg[0] > 0)
                                gdsg(pd->dsp_seg[0]);
                              if (wks > 0) {
                                if ((Inactive==1) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                                else if ((Inactive==0) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                              }
                              pd->dsp_seg[1]=0;
                              pd->dsp_seg[3]=0;
                              pd->off=2;
                              erret++;
                            }
                          for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                            *(pi+3) = 0;
                        }
                    }
                  else
                    {
                      if ( (pa != NULL) && (!pa->FROM_CDAT) ) {
                        if (pgfb == NULL) err_warn(1,fperr,
                                                   "Error - box fill method (%s) not found.\n",
                                                   pd->g_name);
                      } else if (pa == NULL) {
                        err_warn(1,fperr,
                                 "Error - box fill data"
                                 " (%s) not found or not ok.\n",pd->a[0]);
                      }
                      if (pd->dsp_seg[0] > 0)
                        {
                          gdsg(pd->dsp_seg[0]);
                          if (wks > 0) {
                            if ((Inactive==1) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                            else if ((Inactive==0) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                          }
                        }
                      pd->dsp_seg[1]=0;
                      pd->dsp_seg[3]=0;
                      pd->off=2;
                      erret++;
                      continue;
                    }
                }
              /*			Handle a vector display.			*/

              else if (i == 6)
                {

                  /*				Find the graphics attributes.		*/

                  pgv=&Gv_tab;
                  while (pgv != NULL)
                    {
                      if (strcmp(pd->g_name,pgv->name) == 0) break;
                      pgv=pgv->next;
                    }

                  /*				Find the data arrays.			*/

                  pa=&A_tab;
                  while (pgv != NULL && pa != NULL)
                    {
                      if (strlen(pa->name)>(size_t)0 && pa->pA_attr->notok==0 
                          && strcmp(pa->name,pd->a[0]) == 0) break;
                      pa=pa->next;
                    }

                  pb=&A_tab;
                  while (pgv != NULL && pb != NULL)
                    {
                      if (strlen(pb->name)>(size_t)0 && pb->pA_attr->notok==0 
                          && strcmp(pb->name,pd->a[1]) == 0) break;
                      pb=pb->next;
                    }
                  /*				Delete affected segments and redraw.	*/

                  if (pgv != NULL && pp != NULL && pa != NULL && pb != NULL)
                    {

                      /*				Delete segments that are in use.	*/

                      change=0;

                      for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                        {
                          if (*pi > 0 && *(pi+3) > 0)
                            {
                              gdsg(*pi);
                              *pi=0;
                              change=1;
                            }
                          else if (*(pi+3) > 0) change=1;
                        }
                      if (wks > 0) {
                        if ((Inactive==1) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                        else if ((Inactive==0) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                      }
                      if (change > 0)
                        {
                          if (vectors(pd,pgv->pGv_attr,pp,pa,pb)==0)
                            {
                              if (pd->dsp_seg[0] > 0)
                                gdsg(pd->dsp_seg[0]);
                              if (wks > 0) {
                                if ((Inactive==1) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                                else if ((Inactive==0) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                              }
                              pd->dsp_seg[1]=0;
                              pd->dsp_seg[3]=0;
                              pd->off=2;
                              erret++;
                            }
                          for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                            *(pi+3) = 0;
                        }
                    }
                  else
                    {
                      if ( ((pa != NULL) && (!pa->FROM_CDAT)) &&
                           ((pb != NULL) && (!pb->FROM_CDAT)) ) {
                        if (pgv == NULL) 
                          err_warn(1,fperr, "Error - vector method (%s) not found.\n", pd->g_name);
                      } else if (pa == NULL) {
                        err_warn(1,fperr, "Error - vector data (%s) not found or not ok.\n",pd->a[0]);
                      } else if (pb == NULL) {
                        err_warn(1,fperr, "Error - vector data (%s) not found or not ok.\n",pd->a[1]);
                      }
                      if (pd->dsp_seg[0] > 0)
                        {
                          gdsg(pd->dsp_seg[0]);
                          if (wks > 0) {
                            if ((Inactive==1) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                            else if ((Inactive==0) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                          }
                        }
                      pd->dsp_seg[1]=0;
                      pd->dsp_seg[3]=0;
                      pd->off=2;
                      erret++;
                      continue;
                    }
                }

              /*			Handle an X(y) vs y display.			*/

              else if (i == 7)
                {

                  /*				Find the graphics attributes.		*/

                  pgXy=&GXy_tab;
                  while (pgXy != NULL)
                    {
                      if (strcmp(pd->g_name,pgXy->name) == 0) break;
                      pgXy=pgXy->next;
                    }

                  /*				Find the data array.			*/

                  pa=&A_tab;
                  while (pgXy != NULL && pa != NULL)
                    {
                      if (strlen(pa->name)>(size_t)0 && pa->pA_attr->notok==0 
                          && strcmp(pa->name,pd->a[0]) == 0) break;
                      pa=pa->next;
                    }

                  /*				Delete affected segments and redraw.	*/

                  if (pgXy != NULL && pp != NULL && pa != NULL)
                    {

                      /*				Delete segments that are in use.	*/

                      change=0;

                      for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                        {
                          if (*pi > 0 && *(pi+3) > 0)
                            {
                              gdsg(*pi);
                              *pi=0;
                              change=1;
                            }
                          else if (*(pi+3) > 0) change=1;
                        }
                      if (wks > 0) {
                        if ((Inactive==1) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                        else if ((Inactive==0) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                      }
                      if (change > 0)
                        {
                          if (Xyvy(pd,pgXy->pGXy_attr,pp,pa)==0)
                            {
                              if (pd->dsp_seg[0] > 0)
                                gdsg(pd->dsp_seg[0]);
                              if (wks > 0) {
                                if ((Inactive==1) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                                else if ((Inactive==0) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                              }
                              pd->dsp_seg[1]=0;
                              pd->dsp_seg[3]=0;
                              pd->off=2;
                              erret++;
                            }
                          for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                            *(pi+3) = 0;
                        }
                    }
                  else
                    {
                      if ( (pa != NULL) && (!pa->FROM_CDAT) ) {
                        if (pgXy == NULL) err_warn(1,fperr,
                                                   "Error - X(y) vs y method (%s) not found.\n",
                                                   pd->g_name);
                      } else if (pa == NULL) {
                        err_warn(1,fperr, "Error - X(y) data"
                                 " (%s) not found or not ok.\n",pd->a[0]);
                      }
                      if (pd->dsp_seg[0] > 0)
                        {
                          gdsg(pd->dsp_seg[0]);
                          if (wks > 0) {
                            if ((Inactive==1) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                            else if ((Inactive==0) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                          }
                        }
                      pd->dsp_seg[1]=0;
                      pd->dsp_seg[3]=0;
                      pd->off=2;
                      erret++;
                      continue;
                    }
                }
              /*			Handle a Y(x) vs x display.			*/

              else if (i == 8)
                {

                  /*				Find the graphics attributes.		*/

                  pgYx=&GYx_tab;
                  while (pgYx != NULL)
                    {
                      if (strcmp(pd->g_name,pgYx->name) == 0) break;
                      pgYx=pgYx->next;
                    }

                  /*				Find the data array.			*/

                  pa=&A_tab;
                  while (pgYx != NULL && pa != NULL)
                    {
                      if (strlen(pa->name)>(size_t)0 && pa->pA_attr->notok==0 
                          && strcmp(pa->name,pd->a[0]) == 0) break;
                      pa=pa->next;
                    }

                  /*				Tranform the y coordinate axis.         *

                                    for (l = 0; l < pa->pA_attr->xs[0][0]; ++l)
                                    {
                                    pa->pA_attr->xv[0][l] = flog10((float) pa->pA_attr->xv[0][l]);
                                    }
                                    pa->pA_attr->xf[0][0] = flog10((float) pa->pA_attr->xf[0][0]);
                                    pa->pA_attr->xl[0][0] = flog10((float) pa->pA_attr->xl[0][0]);
                  */


                  /*				Delete affected segments and redraw.	*/

                  if (pgYx != NULL && pp != NULL && pa != NULL)
                    {

                      /*				Delete segments that are in use.	*/

                      change=0;

                      for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                        {
                          if (*pi > 0 && *(pi+3) > 0)
                            {
                              gdsg(*pi);
                              *pi=0;
                              change=1;
                            }
                          else if (*(pi+3) > 0) change=1;
                        }
                      if (wks > 0) {
                        if ((Inactive==1) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                        else if ((Inactive==0) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                      }
                      if (change > 0)
                        {
                          if (Yxvx(pd,pgYx->pGYx_attr,pp,pa)==0)
                            {
                              if (pd->dsp_seg[0] > 0)
                                gdsg(pd->dsp_seg[0]);
                              if (wks > 0) {
                                if ((Inactive==1) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                                else if ((Inactive==0) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                              }
                              pd->dsp_seg[1]=0;
                              pd->dsp_seg[3]=0;
                              pd->off=2;
                              erret++;
                            }
                          for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                            *(pi+3) = 0;
                        }
                    }
                  else
                    {
                      if ( (pa != NULL) && (!pa->FROM_CDAT) ) {
                        if (pgYx == NULL) err_warn(1,fperr,
                                                   "Error - Y(x) vs x method (%s) not found.\n",
                                                   pd->g_name);
                      } else if (pa == NULL) {
                        err_warn(1,fperr, "Error - Y(x) data"
                                 " (%s) not found or not ok.\n",pd->a[0]);
                      }
                      if (pd->dsp_seg[0] > 0)
                        {
                          gdsg(pd->dsp_seg[0]);
                          if (wks > 0) {
                            if ((Inactive==1) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                            else if ((Inactive==0) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                          }
                        }
                      pd->dsp_seg[1]=0;
                      pd->dsp_seg[3]=0;
                      pd->off=2;
                      erret++;
                      continue;
                    }
                }
              /*			Handle a X(t) vs Y(t) display.			*/

              else if (i == 9)
                {

                  /*				Find the graphics attributes.		*/

                  pgXY=&GXY_tab;
                  while (pgXY != NULL)
                    {
                      if (strcmp(pd->g_name,pgXY->name) == 0) break;
                      pgXY=pgXY->next;
                    }

                  /*				Find the X(t) data array.		*/

                  pa=&A_tab;
                  while (pgXY != NULL && pa != NULL)
                    {
                      if (strlen(pa->name)>(size_t)0 && pa->pA_attr->notok==0 
                          && strcmp(pa->name,pd->a[0]) == 0) break;
                      pa=pa->next;
                    }

                  /*				Find the Y(t) data array.		*/

                  pb=&A_tab;
                  while (pgXY != NULL && pb != NULL)
                    {
                      if (strlen(pb->name)>(size_t)0 && pb->pA_attr->notok==0 
                          && strcmp(pb->name,pd->a[1]) == 0) break;
                      pb=pb->next;
                    }
                  /*				Delete affected segments and redraw.	*/

                  if (pgXY != NULL && pp != NULL && pa != NULL && pb != NULL)
                    {

                      /*				Delete segments that are in use.	*/

                      change=0;

                      for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                        {
                          if (*pi > 0 && *(pi+3) > 0)
                            {
                              gdsg(*pi);
                              *pi=0;
                              change=1;
                            }
                          else if (*(pi+3) > 0) change=1;
                        }
                      if (wks > 0) {
                        if ((Inactive==1) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                        else if ((Inactive==0) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                      }
                      if (change > 0)
                        {
                          if (XvY(pd,pgXY->pGXY_attr,pp,pa,pb)==0)
                            {
                              if (pd->dsp_seg[0] > 0)
                                gdsg(pd->dsp_seg[0]);
                              if (wks > 0) {
                                if ((Inactive==1) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                                else if ((Inactive==0) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                              }
                              pd->dsp_seg[1]=0;
                              pd->dsp_seg[3]=0;
                              pd->off=2;
                              erret++;
                            }
                          for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                            *(pi+3) = 0;
                        }
                    }
                  else
                    {
                      if ( ((pa != NULL) && (!pa->FROM_CDAT)) &&
                           ((pb != NULL) && (!pb->FROM_CDAT)) ) {
                        if (pgXY == NULL) err_warn(1,fperr,
                                                   "Error - X(t) vs Y(t) method (%s) not found.\n",
                                                   pd->g_name);
                      } else if (pa == NULL) {
                        err_warn(1,fperr, "Error - X(t) data"
                                 " (%s) not found or not ok.\n",pd->a[0]);
                      } else if (pb == NULL) {
                        err_warn(1,fperr, "Error - Y(t) data"
                                 " (%s) not found or not ok.\n",pd->a[1]);
                      }
                      if (pd->dsp_seg[0] > 0)
                        {
                          gdsg(pd->dsp_seg[0]);
                          if (wks > 0) {
                            if ((Inactive==1) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                            else if ((Inactive==0) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                          }
                        }
                      pd->dsp_seg[1]=0;
                      pd->dsp_seg[3]=0;
                      pd->off=2;
                      erret++;
                      continue;
                    }
                }
              /*			Handle a X(...) vs Y(...) ScatterPlot display.	*/

              else if (i == 10)
                {

                  /*				Find the graphics attributes.		*/

                  pgSp=&GSp_tab;
                  while (pgSp != NULL)
                    {
                      if (strcmp(pd->g_name,pgSp->name) == 0) break;
                      pgSp=pgSp->next;
                    }

                  /*				Find the X(...) data array.		*/

                  pa=&A_tab;
                  while (pgSp != NULL && pa != NULL)
                    {
                      if (strlen(pa->name)>(size_t)0 && pa->pA_attr->notok==0 
                          && strcmp(pa->name,pd->a[0]) == 0) break;
                      pa=pa->next;
                    }

                  /*				Find the Y(...) data array.		*/

                  pb=&A_tab;
                  while (pgSp != NULL && pb != NULL)
                    {
                      if (strlen(pb->name)>(size_t)0 && pb->pA_attr->notok==0 
                          && strcmp(pb->name,pd->a[1]) == 0) break;
                      pb=pb->next;
                    }
                  /*				Delete affected segments and redraw.	*/

                  if (pgSp != NULL && pp != NULL && pa != NULL && pb != NULL)
                    {

                      /*				Delete segments that are in use.	*/

                      change=0;

                      for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                        {
                          if (*pi > 0 && *(pi+3) > 0)
                            {
                              gdsg(*pi);
                              *pi=0;
                              change=1;
                            }
                          else if (*(pi+3) > 0) change=1;
                        }
                      if (wks > 0) {
                        if ((Inactive==1) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                        else if ((Inactive==0) && (user_defer_update==0))
                          guwk(wks,GPERFORM);
                      }
                      if (change > 0)
                        {
                          if (scatter_plot(pd,pgSp->pGSp_attr,pp,pa,pb)==0)
                            {
                              if (pd->dsp_seg[0] > 0)
                                gdsg(pd->dsp_seg[0]);
                              if (wks > 0) {
                                if ((Inactive==1) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                                else if ((Inactive==0) && (user_defer_update==0))
                                  guwk(wks,GPERFORM);
                              }
                              pd->dsp_seg[1]=0;
                              pd->dsp_seg[3]=0;
                              pd->off=2;
                              erret++;
                            }
                          for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                            *(pi+3) = 0;
                        }
                    }
                  else
                    {
                      if ( ((pa != NULL) && (!pa->FROM_CDAT)) &&
                           ((pb != NULL) && (!pb->FROM_CDAT)) ) {
                        if (pgSp == NULL) err_warn(1,fperr,
                                                   "Error - ScatterPlot method (%s) not found.\n",
                                                   pd->g_name);
                      } else if (pa == NULL) {
                        err_warn(1,fperr, "Error - X(...) data"
                                 " (%s) not found or not ok.\n",pd->a[0]);
                      } else if (pb == NULL) {
                        err_warn(1,fperr, "Error - Y(...) data"
                                 " (%s) not found or not ok.\n",pd->a[1]);
                      }
                      if (pd->dsp_seg[0] > 0)
                        {
                          gdsg(pd->dsp_seg[0]);
                          if (wks > 0) {
                            if ((Inactive==1) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                            else if ((Inactive==0) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                          }
                        }
                      pd->dsp_seg[1]=0;
                      pd->dsp_seg[3]=0;
                      pd->off=2;
                      erret++;
                      continue;
                    }
                }
              else if (i==11)
                { 
		  
                  /*				Find the graphics attributes.		*/

                  pgfm=&Gfm_tab;
                  while (pgfm != NULL)
                    {
                      if (strcmp(pd->g_name,pgfm->name) == 0) break;
                      pgfm=pgfm->next;
                    }

                  /*				Find the data array.			*/

                  pa=&A_tab;
                  while (pgfm != NULL && pa != NULL)
                    {
                      if (strlen(pa->name)>(size_t)0 && pa->pA_attr->notok==0 
                          && strcmp(pa->name,pd->a[0]) == 0) break;
                      pa=pa->next;
                    }

                  pb=&A_tab;
                  while (pgfm != NULL && pb != NULL)
                    {
                      if (strlen(pb->name)>(size_t)0 && pb->pA_attr->notok==0 
                          && strcmp(pb->name,pd->a[1]) == 0) break;
                      pb=pb->next;
                    }

                  /*				Delete affected segments and redraw.	*/

                  if (pgfm != NULL && pp != NULL && pa != NULL && pb !=NULL)
                    {

                      /*				Delete segments that are in use.	*/
			
                      change=0;
			
                      for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                        {
                          if (*pi > 0 && *(pi+3) > 0)
                            {
                              gdsg(*pi);
                              *pi=0;
                              change=1;
                            }
                          else if (*(pi+3) > 0) change=1;
                        }
                      if (wks > 0) 
                        {
                          if ((Inactive==1) && (user_defer_update==0))
                            guwk(wks,GPERFORM);
                          else if ((Inactive==0) && (user_defer_update==0))
                            guwk(wks,GPERFORM);
                        }
                      if (change > 0)
                        {
                          if (meshfill(pd,pgfm->pGfm_attr,pp,pa->pA_attr,pb->pA_attr)==0)
                            {
                              if (pd->dsp_seg[0] > 0)
                                gdsg(pd->dsp_seg[0]);
                              if (wks > 0) 
                                {
                                  if ((Inactive==1) && (user_defer_update==0))
                                    guwk(wks,GPERFORM);
                                  else if ((Inactive==0) && (user_defer_update==0))
                                    guwk(wks,GPERFORM);
                                }
                              pd->dsp_seg[1]=0;
                              pd->dsp_seg[3]=0;
                              pd->off=2;
                              erret++;
                            }
                          for(pi=&pd->F_seg[0];pi <= &pd->dsp_seg[3];pi+=4)
                            *(pi+3) = 0;
                        }
                    }
                  else
                    {
                      if ( (pa != NULL) && (!pa->FROM_CDAT) ) {
                        if (pgfm == NULL) err_warn(1,fperr,
                                                   "Error - meshfill method (%s) not found.\n",pd->g_name);
                      } else if (pa == NULL) {
                        err_warn(1,fperr,"Error - meshfill data"
                                 " (%s) not found or not ok.\n",pd->a[0]);
                      }
                      if (pd->dsp_seg[0] > 0)
                        {
                          gdsg(pd->dsp_seg[0]);
                          if (wks > 0) {
                            if ((Inactive==1) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                            else if ((Inactive==0) && (user_defer_update==0))
                              guwk(wks,GPERFORM);
                          }
                        }
                      pd->dsp_seg[1]=0;
                      pd->dsp_seg[3]=0;
                      pd->off=2;
                      erret++;
                      continue;
                    }
                }
            }
        }
      killP( pp ); /* Remove the newly created picture template. */
    }
  }
  if (change==1) {
    /* Put code to do the logo here */
    extern void draw_logo(cairo_t *cr);
    draw_logo(connect_id.cr);
  }
  
  /* 	if (wks > 0) { */
  /*            if ((Inactive==1) && (user_defer_update==0)) */
  /*               guwk(wks,GPERFORM); */
  /*            else if ((Inactive==0) && (user_defer_update==0)) */
  /*               guwk(wks,GPERFORM); */
  /*         } */
  vcs_legacy_release_update();

  if (erret == 0) return 1;
  else
    return 0;
}

void draw_logo(cairo_t *cr) {
  int w,h;
  float ratio = .025;
  float logo_ratio;
  float hr,x,y,dw,dh;
  int xtmp;
  cairo_surface_t *surface;
  cairo_surface_type_t stype;
  if (cr == NULL) return;
  //return;
  surface = cairo_get_target(cr);
  stype = cairo_surface_get_type(surface);
  if (stype==CAIRO_SURFACE_TYPE_IMAGE) {
    w=cairo_image_surface_get_width(surface);
    h=cairo_image_surface_get_height(surface);
  } 
  else {
    w = XW;
    h = YW;
  }

  hr = (float)(int)((float)h*ratio)/(float)logo_height;
  logo_ratio = (float)logo_width/(float)logo_height;
  dh = (float)h*ratio;
  y= (float)h-dh;
  dw = dh*logo_ratio;
  x=(float)w-dw;
  /* if (stype==CAIRO_SURFACE_TYPE_PS) { */
  /* 	xtmp = (int) ((float)(YW)/15.); */
  /* 	cairo_translate(cr,xtmp,0); */
  /* } */
  cairo_rectangle(cr,x,y,dw,dh);
  cairo_clip(cr);
  cairo_scale(cr,hr,hr);
  cairo_set_source_surface(cr,logo,x/hr,y/hr);
  //cairo_set_source(cr,logo_p);
  //cairo_set_source_rgb(cr,1.,0.,0.);
  cairo_paint_with_alpha(cr,.5);
  cairo_reset_clip(cr);
  cairo_scale(cr,1./hr,1./hr);
}

void set_viewport_and_worldcoordinate ( 
                                       float vp[],/* view port.                    */
                                       float wc_in[], /* world coordinates.            */
                                       char proj[256]
                                        )
{
#ifdef X11WM
  XWindowAttributes 		xwa;
#elif defined (QTWM)
  extern void vcs_legacy_Qt_get_window_dimensions_by_id(int id,int *x, int *y,int *w,int *h);
#endif
  int w,h,xoff,yoff;
  float 				wc[4];
  float            		canvas_ratio_l=0., canvas_ratio_p=0.;
  Glimit           		window_shape;
  Glimit 				pvp, pwp;
  extern struct workstations 	Wkst[];
  extern struct orientation 	Page;
  struct pe_dsp 			tmp_vp;
  extern int XW ;
  extern int YW ;

  /* Ok wc is not in the same order than template wc need to reorder*/
  wc[0]=(float)wc_in[0];
  wc[1]=(float)wc_in[2];
  wc[2]=(float)wc_in[1];
  wc[3]=(float)wc_in[3];
#ifdef X11WM
  /*			Get the width and height of the VCS Canvas.	*/
  if ((connect_id.display == NULL) || (connect_id.drawable == 0)) {
#else
    /*			Get the width and height of the VCS Canvas.	*/
    if ((connect_id.cr == NULL)) {
#endif
#ifdef X11WM
      /* Must be generating the images in background mode */
      /* if (strcmp(Page.page_orient,"landscape") == 0) { */
      /*   xwa.width = XW; */
      /*   xwa.height = YW; */
      /* } else { */
        xwa.width = XW;
        xwa.height = YW;
      /* } */
#elif defined (QTWM) 
      /* Must be generating the images in background mode */
      /* if (strcmp(Page.page_orient,"landscape") == 0) { */
	w=XW;
	h=YW;
      /* } else { */
      /*   w = XW; */
      /*   h = YW; */
      /* } */
#else
      fprintf(stderr,"insert here your WM setting of  default w/h \n");
#endif
    } else {
#ifdef X11WM
      XGetWindowAttributes(connect_id.display, connect_id.drawable, &xwa);
      w=xwa.width;
      h=xwa.height;
#elif defined(QTWM)
      vcs_legacy_Qt_get_window_dimensions_by_id(connect_id.wkst_id,&xoff,&yoff,&w,&h);
#else
      fprintf(stderr,"insert here your WM getgeometry function\n");
#endif
    }
    /* 
     * Set the workstation window size, just in case the
     * user resized the VCS Canvas.
     */
    canvas_ratio_l = (float) h / (float) w;
    canvas_ratio_p = (float) w / (float) h;
    window_shape.xmin = 0.0;
    window_shape.xmax=(w >= h) ? 1.0:canvas_ratio_p;
    window_shape.ymin = 0.0;
    window_shape.ymax=(w >= h) ? canvas_ratio_l:1.0;

    gswkwn(Wkst[0].id, &window_shape);

    /* Set the world coordinates constants. */
    pwp.xmin=wc[0];
    pwp.xmax=wc[1];
    pwp.ymin=wc[2];
    pwp.ymax=wc[3];
    gswn(2,&pwp);

    /* Calculate and set the viewport  constants. */
    if (w >= h) {
      pvp.xmin = vp[0];
      pvp.xmax = vp[1];
      pvp.ymin = vp[2]*canvas_ratio_l;
      pvp.ymax = vp[3]*canvas_ratio_l;
    } else {
      pvp.xmin = vp[0]*canvas_ratio_p;
      pvp.xmax = vp[1]*canvas_ratio_p;
      pvp.ymin = vp[2];
      pvp.ymax = vp[3];
    }
    /* 	     printf("OK in primimtive viewport is:%f,%f,%f,%f\n",vp[0],vp[1],vp[2],vp[3]); */
    /* 	     printf("OK in primimtive wc is:%f,%f,%f,%f\n",wc[0],wc[1],wc[2],wc[3]); */
    /* 	     printf("OK in primimtive wc is:%f,%f,%f,%f\n",wc_in[0],wc_in[1],wc_in[2],wc_in[3]); */
    /* 	     pvp.xmin = stnorm(0,vp[0]); */
    /* 	     pvp.xmax = stnorm(0,vp[1]); */
    /* 	     pvp.ymin = stnorm(1,vp[2]); */
    /* 	     pvp.ymax = stnorm(1,vp[3]); */
    /* 	     pvp.ymin=0.200000; */
    /* 	     pvp.ymax=0.605000; */
    /* 	     printf("OK in primimtive gswn,gsvp is:%f,%f,%f,%f\n",pvp.xmin,pvp.xmax,pvp.ymin,pvp.ymax); */
    gswn(2,&pvp);
    gsvp(2,&pvp);
    tmp_vp.x1=pvp.xmin;
    tmp_vp.x2=pvp.xmax;
    tmp_vp.y1=pvp.ymin;
    tmp_vp.y2=pvp.ymax;
	     
    set_projection(proj,tmp_vp,wc,wc);

    /* Set the normalized device coordinate number id = 2 */
    gselnt(2);
    gsclip(GCLIP);
  }

  int vcs_legacy_draw_primatives( char * d_name)
  {
    /*C.Doutriaux 2009-08-26 changed from one because default dispalys prio is 0 and 1 would cause primi to be on top always! */
    int				i,j,temp_priority=0; 
    char				*tpt, *tpo, ttname[1000], toname[1000];
	Gpoint                          *pxy;
    Gtxfp 				pfp;
	Gint 				wks;
    struct display_tab 		*pd;
    struct table_line               *tltab;
    struct table_mark               *tmtab;
    struct table_fill               *tftab;
    struct table_text               *tttab;
    struct table_chorn              *totab;
    struct points_struct		*xptr=NULL, *yptr=NULL;
    struct array_segments   	*xpts=NULL, *ypts=NULL;
    struct char_segments		*tx=NULL;
    extern int 			segment_num;
    extern struct table_line        Tl_tab;
    extern struct table_mark        Tm_tab;
    extern struct table_fill        Tf_tab;
    extern struct table_text        Tt_tab;
    extern struct table_chorn       To_tab;
    extern struct project_attr 	p_PRJ;
    extern struct vcs_legacy_marker 	Vma_tab;
	char                            proj[256];
	Glimit                          pvp;
    struct project_attr *pj;

    pj=&p_PRJ;


    /*			Search the displays to find the correct line
    			to be drawn on the VCS Canvas                          	*/
#ifdef QTWM
    //fprintf(stderr,"in draw_prima, acquiring\n");
    vcs_legacy_acquire_update();
#endif
	wks=check_canvas_defer();

    pd=&D_tab;
    while (pd != NULL)
      {
        if (cmpncs(pd->name, d_name) == 0) break;
        pd=pd->next;
      }

    if (pd == NULL) {
#ifdef QTWM
      vcs_legacy_release_update(); 
#endif
      return 0;
    };		/* nothing to plot */

    if (cmpncs(pd->type, "line") == 0) {
      tltab = &Tl_tab;
      while (tltab != NULL) {
        if (cmpncs(pd->g_name, tltab->name) == 0) break;
        tltab = tltab->next;
      }
      if (tltab->priority == 0) {
#ifdef QTWM
	vcs_legacy_release_update() ;
#endif
	return 1;
      } /* do nothing */
      if ((tltab->lx == NULL) || (tltab->ly == NULL)) {
        err_warn(1,fperr, "Error - invalid X/Y points.");
        vcs_legacy_release_update();
        return 0;
      } else if ( ((tltab->lvp[0] > 1) || (tltab->lvp[0] < 0)) ||
                  ((tltab->lvp[1] > 1) || (tltab->lvp[1] < 0)) ||
                  ((tltab->lvp[2] > 1) || (tltab->lvp[2] < 0)) ||
                  ((tltab->lvp[3] > 1) || (tltab->lvp[3] < 0)) ) {
        err_warn(1,fperr, "Error - invalid viewport values.");
#ifdef QTWM
        vcs_legacy_release_update();
#endif
        return 0;
      } else {
	    strcpy(proj,tltab->proj);
	    set_viewport_and_worldcoordinate ( tltab->lvp, tltab->lwc,proj );

        /*			Draw lines.					*/ 

        /* Set the segment number priority */
        gcrsg(pd->dsp_seg[0]=++segment_num);

        /* DEAN 9-7-07: I am not sure why pd->pri is used below. I replaced it with the 
           priority value tltab->priority. Without tltab->priority, the user specified 
           primative priority setting was not being recognized.
           gssgp(pd->dsp_seg[0],(temp_priority+pd->pri)/1000.0);*/
        gssgp(pd->dsp_seg[0],(temp_priority+tltab->priority)/1000.0);

        /* Draw line segments */
        xptr = tltab->lx; yptr = tltab->ly;
        xpts = xptr->ps;  ypts = yptr->ps;
        for (i=0; i<xptr->nsegs; i++) {
          /* Set the line attributes */
          if (i < tltab->ltyp_size) gsln(tltab->ltyp[i]);
          if (i < tltab->lwsf_size) gslwsc(tltab->lwsf[i]);
          if (i < tltab->lci_size)  gsplci(tltab->lci[i]);

          if ((pxy=(Gpoint *)malloc(xpts->npts*sizeof(Gpoint)))==NULL) {
            err_warn(1,fperr,"Error: Memory overflow in line segment.\n");
#ifdef QTWM
            vcs_legacy_release_update();
#endif
            return 0;
          }
          for (j=0;j<xpts->npts;j++) {
            pxy[j].x=xpts->pts[j];
            pxy[j].y=ypts->pts[j];
          }
          proj_pl(xpts->npts,pxy);

          xpts = xpts->next;
          ypts = ypts->next;
          free((char *) pxy);
        }

        /* Close the segment and reset the normalized id back to 0. */
        gclsg();
        gselnt(0);

        gsclip(GNOCLIP);

        /* Set display segment for automatic update */
        pd->dsp_seg[1]=1;
        pd->dsp_seg[2]=1;
        pd->dsp_seg[3]=0;
      }
    } else if (cmpncs(pd->type, "marker") == 0) {
      tmtab = &Tm_tab;
      while (tmtab != NULL) {
        if (cmpncs(pd->g_name, tmtab->name) == 0) break;
        tmtab = tmtab->next;
      }
      if (tmtab->priority == 0) {
#ifdef QTWM
	vcs_legacy_release_update();
#endif
	return 1;
      } /* do nothing */
      if ((tmtab->mx == NULL) || (tmtab->my == NULL)) {
        err_warn(1,fperr, "Error - invalid X/Y points.");
#ifdef QTWM
        vcs_legacy_release_update();
#endif
        return 0;
      } else if ( ((tmtab->mvp[0] > 1) || (tmtab->mvp[0] < 0)) ||
                  ((tmtab->mvp[1] > 1) || (tmtab->mvp[1] < 0)) ||
                  ((tmtab->mvp[2] > 1) || (tmtab->mvp[2] < 0)) ||
                  ((tmtab->mvp[3] > 1) || (tmtab->mvp[3] < 0)) ) {
        err_warn(1,fperr, "Error - invalid viewport values.");
#ifdef QTWM
        vcs_legacy_release_update();
#endif
        return 0;
      } else {
	    strcpy(proj,tmtab->proj);
	    set_viewport_and_worldcoordinate ( tmtab->mvp, tmtab->mwc,proj );
	    
	    /*                      Draw markers.                                   */
	    
	    /* Set the segment number priority */
	    gcrsg(pd->dsp_seg[0]=++segment_num);
        /* DEAN 9-7-07: I am not sure why pd->pri is used below. I replaced it with the 
           priority value tltab->priority. Without tltab->priority, the user specified 
           primative priority setting was not being recognized.
           gssgp(pd->dsp_seg[0],(temp_priority+pd->pri)/1000.0);*/
	    gssgp(pd->dsp_seg[0],(temp_priority+tmtab->priority)/1000.0);
	    
	    /* Draw marker segments */
	    xptr = tmtab->mx; yptr = tmtab->my;
	    xpts = xptr->ps;  ypts = yptr->ps;
	    for (i=0; i<xptr->nsegs; i++) {
	      /* Set the line attributes */
	      if (i < tmtab->mtyp_size) Vma_tab.type = tmtab->mtyp[i];
	      if (i < tmtab->msize_size)
            /* 		 Commented out by C.Doutriaux, when changing to accept projection  */
            /*                     Vma_tab.size = (tmtab->msize[i]*fabs(tmtab->mwc[1]-tmtab->mwc[0])); */
            Vma_tab.size = tmtab->msize[i];
	      if (i < tmtab->mci_size) Vma_tab.colour = tmtab->mci[i];

	      if ((pxy=(Gpoint *)malloc(xpts->npts*sizeof(Gpoint)))==NULL) {
            err_warn(1,fperr,"Error: Memory overflow in marker segment.\n");
#ifdef QTWM
            vcs_legacy_release_update();
#endif
            return 0;
	      }
	      for (j=0;j<xpts->npts;j++) {
            pxy[j].x=xpts->pts[j];
            pxy[j].y=ypts->pts[j];
	      }
	      proj_pm(xpts->npts,pxy);
	      
	      xpts = xpts->next;
	      ypts = ypts->next;
	      free((char *) pxy);
	    }
	    
	    /* Close the segment and reset the normalized id back to 0. */
	    gclsg();
	    gselnt(0);
	    
	    gsclip(GNOCLIP);
	    
	    /* Set display segment for automatic update */
	    pd->dsp_seg[1]=1;
	    pd->dsp_seg[2]=1;
	    pd->dsp_seg[3]=0;
      }
    } else if (cmpncs(pd->type, "fillarea") == 0) {
      tftab = &Tf_tab;
      while (tftab != NULL) {
        if (cmpncs(pd->g_name, tftab->name) == 0) break;
        tftab = tftab->next;
      }
      if (tftab->priority == 0) {
#ifdef QTWM
	vcs_legacy_release_update();
#endif
	return 1;
    } /* do nothing */
      if ((tftab->fx == NULL) || (tftab->fy == NULL)) {
        err_warn(1,fperr, "Error - invalid X/Y points.");
#ifdef QTWM
        vcs_legacy_release_update();
#endif
        return 0;
      } else if ( ((tftab->fvp[0] > 1) || (tftab->fvp[0] < 0)) ||
                  ((tftab->fvp[1] > 1) || (tftab->fvp[1] < 0)) ||
                  ((tftab->fvp[2] > 1) || (tftab->fvp[2] < 0)) ||
                  ((tftab->fvp[3] > 1) || (tftab->fvp[3] < 0)) ) {
        err_warn(1,fperr, "Error - invalid viewport values.");
#ifdef QTWM
        vcs_legacy_release_update();
#endif
        return 0;
      }
   else {
	    /* set_viewport_and_worldcoordinate commented out when putting projection in */
	    strcpy(proj,tftab->proj);
	    set_viewport_and_worldcoordinate ( tftab->fvp, tftab->fwc,proj );

	    

	    /* Set the segment number priority */
	    gcrsg(pd->dsp_seg[0]=++segment_num);
        /* DEAN 9-7-07: I am not sure why pd->pri is used below. I replaced it with the 
           priority value tltab->priority. Without tltab->priority, the user specified 
           primative priority setting was not being recognized.
           gssgp(pd->dsp_seg[0],(temp_priority+pd->pri)/1000.0);*/
	    gssgp(pd->dsp_seg[0],(temp_priority+tftab->priority)/1000.0);
	    
	    /* Draw fill area segments */
	    xptr = tftab->fx; yptr = tftab->fy;
	    xpts = xptr->ps;  ypts = yptr->ps;
	    for (i=0; i<xptr->nsegs; i++) {
	      /* Set the fill area attributes */
	      if (i < tftab->fais_size) gsfais(tftab->fais[i]);
	      if (i < tftab->fasi_size) gsfasi(tftab->fasi[i]);
	      if (i < tftab->faci_size) gsfaci(tftab->faci[i]);
	      /*gspa(tftab->w, tftab->h);
            gsparf(tftab->x, tftab->y);*/
	      
	      if ((pxy=(Gpoint *)malloc(xpts->npts*sizeof(Gpoint)))==NULL) {
            err_warn(1,fperr,"Error: Memory overflow in fill area segment.\n");
#ifdef QTWM
            vcs_legacy_release_update();
#endif
            return 0;
	      }
	      for (j=0;j<xpts->npts;j++) {
            pxy[j].x=xpts->pts[j];
            pxy[j].y=ypts->pts[j];
	      }
	      proj_gfa(xpts->npts,pxy);
	      
	      xpts = xpts->next;
	      ypts = ypts->next;
	      free((char *) pxy);
	    }

	    /* Close the segment and reset the normalized id back to 0. */
	    gclsg();
	    gselnt(0);
	    
	    gsclip(GNOCLIP);
	    
	    /* Set display segment for automatic update */
	    pd->dsp_seg[1]=1;
	    pd->dsp_seg[2]=1;
	    pd->dsp_seg[3]=0;
      }
  } else if (cmpncs(pd->type, "text") == 0) {
      strcpy(ttname, pd->g_name);
      tpt = strtok(ttname, ":::");
      strcpy(toname, pd->g_name);
      tpo = strstr(toname, ":::")+3;
          
      tttab = &Tt_tab;
      while (tttab != NULL) {
        if (cmpncs(tpt, tttab->name) == 0) break;
        tttab = tttab->next;
      }
      if (tttab->priority == 0) {
#ifdef QTWM
	vcs_legacy_release_update();
#endif
	return 1;} /* do nothing */
      if (tttab->ts == NULL) {
#ifdef QTWM
	vcs_legacy_release_update();
#endif
	return 1;}    /* do nothing */

      totab = &To_tab;
      while (totab != NULL) { 
        if (cmpncs(tpo, totab->name) == 0) break;
        totab = totab->next;
      }

      if ((tttab->tx == NULL) || (tttab->ty == NULL)) {
        err_warn(1,fperr, "Error - invalid X/Y points.");
#ifdef QTWM
        vcs_legacy_release_update();
#endif
        return 0;
      } else if ( ((tttab->tvp[0] > 1) || (tttab->tvp[0] < 0)) ||
                  ((tttab->tvp[1] > 1) || (tttab->tvp[1] < 0)) ||
                  ((tttab->tvp[2] > 1) || (tttab->tvp[2] < 0)) ||
                  ((tttab->tvp[3] > 1) || (tttab->tvp[3] < 0)) ) {
        err_warn(1,fperr, "Error - invalid viewport values.");
#ifdef QTWM
        vcs_legacy_release_update();
#endif
        return 0;
      } else {
	    strcpy(proj,tttab->proj);
        /* 	    printf("Ok the projection name is: %s\n",proj); */
	    set_viewport_and_worldcoordinate ( tttab->tvp, tttab->twc,proj );
	    /*                      Draw text.                                     */
	    
	    /* Set the segment number priority */
	    gcrsg(pd->dsp_seg[0]=++segment_num);
        /* DEAN 9-7-07: I am not sure why pd->pri is used below. I replaced it with the 
           priority value tltab->priority. Without tltab->priority, the user specified 
           primative priority setting was not being recognized.
           gssgp(pd->dsp_seg[0],(temp_priority+pd->pri)/1000.0);*/
	    gssgp(pd->dsp_seg[0],(temp_priority+tttab->priority)/1000.0);
	    
	    /* Set the line attributes */
	    /*             pfp.font=tttab->txfont;
                       pfp.prec=2;
                       gstxfp(&pfp);
                       gschxp(tttab->txexp);
                       gschsp(tttab->txsp);
                       gstxci(tttab->txci);*/
	    set_text_attr( tttab, totab );
	    
	    /* Draw text segments */
	    xptr = tttab->tx; yptr = tttab->ty;
	    xpts = xptr->ps;  ypts = yptr->ps;
	    tx = tttab->ts->ss;
	    for (i=0; i<xptr->nsegs; i++) {
	      if ((pxy=(Gpoint *)malloc(xpts->npts*sizeof(Gpoint)))==NULL) {
            err_warn(1,fperr,"Error: Memory overflow in text segment.\n");
#ifdef QTWM
            vcs_legacy_release_update();
#endif
            return 0;
	      }
	      for (j=0;j<xpts->npts;j++) {
            if (tx == NULL) break;
            pxy[j].x=xpts->pts[j];
            pxy[j].y=ypts->pts[j];
            proj_gtx(pxy[j],(unsigned char *)tx->cpts);
            tx = tx->next;
	      }
	      
	      xpts = xpts->next;
	      ypts = ypts->next;
	      free((char *) pxy);
	    }
	    
	    /* Close the segment and reset the normalized id back to 0. */
	    gclsg();
	    gselnt(0);
	    
	    gsclip(GNOCLIP);
	    
	    /* Set display segment for automatic update */
	    pd->dsp_seg[1]=1;
	    pd->dsp_seg[2]=1;
	    pd->dsp_seg[3]=0;
      }
    }

    if (wks > 0) {
      if ((Inactive==1) && (user_defer_update==0))
        guwk(wks,GPERFORM);
      else if ((Inactive==0) && (user_defer_update==0))
        guwk(wks,GPERFORM);
    }

#ifdef QTWM
    vcs_legacy_release_update();
#endif
	return 1;
  }
