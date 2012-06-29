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
#include "cairo/cairo.h"
#include "cairo/cairo-svg.h"
#include "ps.h"
#include "cgm.h"
#include "cgm_implem.h"


extern struct orientation       Page;
extern int XW ;
extern int YW ;

extern cairo_status_t stream_cairo_write();

/*
 * Open an output SVG file.
 */
    int
SVGmoOpen(WS_STATE_PTR ws)
{
  Gint status = 1;
  struct color_table *ptab;
  double tmp;
  double x,y;
  initLogFlag();
  cairo_surface_t *surface;
  assert(ws != NULL);
  if ((ws->mf.cgmo = (mf_cgmo*)umalloc(sizeof(mf_cgmo))) != NULL) {
    mf_cgmo	*cgmo	= ws->mf.cgmo;
    
    if ((cgmo->fp = fopen(ws->conn, "w")) == NULL) {
      (void)CAIROmoClose(&ws->mf);
    } else {
      /* C. Doutriaux addition link to cairo lib */
      x=(double)XW;
      y=(double)YW;
      surface = cairo_svg_surface_create_for_stream(stream_cairo_write,
						   ws->mf.any->fp,
						   x,y);
          
      
#ifdef CAIRODRAW
      ws->cr = cairo_create (surface);
#endif
      
      cgmo->ws			= ws;
      cgmo->type			= MF_CAIRO;
      status	= OK;
    }
  }
  
   return status;
}
