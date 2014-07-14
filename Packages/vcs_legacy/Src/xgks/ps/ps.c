#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include <time.h>		/* for time(), localtime(), and strftime() */
#include <sys/types.h>		/* for uid_t */
#include <unistd.h>		/* for getuid() & getlogin() */
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <assert.h>
#include "gks_implem.h"
#include "gksm.h"

#include "workstations.h"
#include <cairo/cairo.h>
#include <cairo/cairo-ps.h>
#include "ps.h"
#include "cgm.h"
#include "cgm_implem.h"


extern struct orientation       Page;
extern int XW ;
extern int YW ;

/* local cairo stuff */

extern int MARGINL;
extern int MARGINT;
extern int MARGINR;
extern int MARGINB;

Gpoint VCS2PSDEVICE(x,y)
     double x;
     double y;
{
  Gpoint to;
  double xr,yr,x2,y2;

  to.x=x;
  to.y=y;
  return to;
  yr=1.;
  if (strcmp(Page.page_orient,"landscape")==0) {
    xr = (double)(YW-MARGINT-MARGINB)/(double)(XW-MARGINR-MARGINL);
    xr = (double)(YW)/(double)(XW);
   // xr = xr*(double)XW/(double)(XW-MARGINL-MARGINR);
    //printf("T<B<L<R: %i, %i, %i, %i, %f, %i, %i,%f, %f\n",MARGINT,MARGINB,MARGINL,MARGINR,xr,XW,YW,x,y);
/*     xr = xr * (double)(XW)/(double)(XW-MARGINT -MARGINB); */
/*     xr = xr / (double)(YW)*(double)(YW-MARGINL-MARGINR); */
    yr = (double)(YW-MARGINL-MARGINR)/(double)(YW);
    to.x = x*xr*(XW-MARGINT-MARGINB)+MARGINB;
    to.y = (YW-MARGINR)-y*yr*(YW-MARGINL-MARGINR); //bigger Y means move to the right
    to.y = (YW-MARGINR)/yr-y*yr*(YW-MARGINL-MARGINR);//-MARGINR); //bigger Y means move to the right
  }
  else {
    xr = (double)(YW)/(double)(XW);
    // xr= (double)(XW-MARGINR-MARGINL)/(double)(YW-MARGINT-MARGINB);
    //xr= (double)(XW-MARGINR-MARGINL)/(double)(YW-MARGINT-MARGINB);
    //   xr=1;
    //xr = xr / (double)XW*(double)(XW-MARGINL-MARGINR);
    to.x = x*xr*(XW-MARGINL-MARGINR)+MARGINL;
    to.y = (YW-MARGINB)-y*yr*(YW-MARGINT-MARGINB);
  }

  to.x=(int)to.x;
  to.y=(int)to.y;
  return to;
}


extern cairo_status_t stream_cairo_write();
/*
 * Open an output PostScript file.
 */
    int
PSmoOpen(WS_STATE_PTR ws)
{
  Gint status = 1;
  struct color_table *ptab;
  double tmp;
  int xtmp;
  cairo_surface_t *surface;
  initLogFlag();
  assert(ws != NULL);
  if ((ws->mf.cgmo = (mf_cgmo*)umalloc(sizeof(mf_cgmo))) != NULL) {
    mf_cgmo	*cgmo	= ws->mf.cgmo;
    
    if ((cgmo->fp = fopen(ws->conn, "w")) == NULL) {
      (void)CAIROmoClose(&ws->mf);
    } else {
      /* C. Doutriaux addition link to cairo lib */
/*       if (strcmp(Page.page_orient,"landscape") == 0) { */
/* 	tmp = YW; */
/*       	YW = XW; */
/* 	XW=tmp; */
/*       } */

#ifdef GENCAIRO
      printf("#include <cairo.h>\n#include <cairo-ps.h>\n");
      printf("int main(){\ncairo_surface_t *surface;\ncairo_t *cr;\ncairo_pattern_t *pattern=NULL;\ncairo_surface_t *image=NULL;\ncairo_t *cr2=NULL;\n");
      printf("double dashes[4];\nint ndashes;\ncairo_matrix_t matrix;\nchar achar[2];\n");
      printf("surface = cairo_ps_surface_create(\"test.ps\", %i, %i);\n",XW,YW);
      printf("cairo_ps_surface_dsc_begin_setup (surface);\n");
      printf("cairo_ps_surface_dsc_begin_page_setup (surface);\n");
      printf("cr = cairo_create (surface);\n");
      printf("cairo_save (cr);\n");
#endif
      if (strcmp(Page.page_orient,"landscape")==0) {
	tmp=YW;
	YW=XW;
	XW=tmp;
	surface = cairo_ps_surface_create_for_stream(stream_cairo_write,
						     ws->mf.any->fp,
						     XW,YW);
      } else {
	surface = cairo_ps_surface_create_for_stream(stream_cairo_write,
						     ws->mf.any->fp,
						     XW,YW);
      };
      cairo_ps_surface_set_eps (surface, TRUE);
      //cairo_ps_surface_dsc_begin_setup (surface);
      //cairo_ps_surface_dsc_begin_page_setup (surface);
      
#ifdef CAIRODRAW
      ws->cr = cairo_create (surface);
      cairo_set_antialias(ws->cr,CAIRO_ANTIALIAS_NONE);
      cairo_save(ws->cr);
      //cairo_set_line_cap(ws->cr,CAIRO_LINE_CAP_SQUARE);
#endif
      cgmo->ws			= ws;
      cgmo->type			= MF_CAIRO;
#ifdef CAIRODRAW
      if (strcmp(Page.page_orient,"landscape")==0) {
	//xtmp = -(int) ((float)(XW)/4.);
	//cairo_translate(ws->cr,xtmp,YW);
	//cairo_rotate(ws->cr,-1.5707963267948966);
#ifdef GENCAIRO
	printf("cairo_translate(cr,%i,%i);\n",xtmp,YW);
	printf("cairo_rotate(cr,-1.5707963267948966);\n");
#endif
      }
#endif
      status	= OK;
    }
  }
  
   return status;
}



/*
 * Write a message to an output PostScript file.
 */
    int
PSmessage(Metafile **mf, int num, Gchar *string)
{
  char tmp[256];
  int length;
  length = strlen(string);
  strcpy(tmp,"%");
  strcat(tmp,string);
  if (length>254) tmp[256]='\0';
  else tmp[length+1]='\0';
/*   cairo_ps_surface_dsc_comment(surface,tmp); */
#ifdef GENCAIRO
  printf("cairo_ps_surface_dsc_comment(surface,\"%s\");\n",tmp);
#endif
  return OK;
}

