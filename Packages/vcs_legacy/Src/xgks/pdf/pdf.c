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
#include <cairo/cairo-pdf.h>
#include "ps.h"
#include "cgm.h"
#include "cgm_implem.h"


extern struct orientation       Page;
extern int XW ;
extern int YW ;

/* local cairo stuff */


extern cairo_status_t stream_cairo_write();

/*
 * Open an output PDF file.
 */
    int
PDFmoOpen(WS_STATE_PTR ws)
{
  Gint status = 1;
  struct color_table *ptab;
  double tmp;
  cairo_surface_t *surface;
  initLogFlag();
  assert(ws != NULL);
  if ((ws->mf.cgmo = (mf_cgmo*)umalloc(sizeof(mf_cgmo))) != NULL) {
    mf_cgmo	*cgmo	= ws->mf.cgmo;
    
    if ((cgmo->fp = fopen(ws->conn, "w")) == NULL) {
      (void)CAIROmoClose(&ws->mf);
    } else {
      /* C. Doutriaux addition link to cairo lib */
#ifdef GENCAIRO
      printf("#include <cairo.h>\n#include <cairo-ps.h>\n#include <stdio.h>\n");
      printf(" cairo_status_t\nstream_cairo_write (void		*closure,\n	    const unsigned char	*data,\n	    unsigned int	 length)\n{\n  FILE	*fp = closure;\n  int i;\n  for(i=0;i<length;i++){\n    fputc( data[i],fp);\n  }\n  return CAIRO_STATUS_SUCCESS;\n};\n");
      printf("int main(){\ncairo_surface_t *surface;\ncairo_t *cr;\ncairo_pattern_t *pattern=NULL;\ncairo_surface_t *image=NULL;\ncairo_t *cr2=NULL;\n");
      printf("double dashes[4];\nint ndashes;\ncairo_matrix_t matrix;\nchar achar[2];\n");
      printf("FILE *fp;\n");
      printf("fp = fopen(\"test.pdf\",\"w\");\n");
      printf("surface = cairo_pdf_surface_create_for_stream(stream_cairo_write,fp,%i, %i);\n",XW,YW);
      printf("cr = cairo_create (surface);\n");
#endif
      surface = cairo_pdf_surface_create_for_stream(stream_cairo_write,
						   ws->mf.any->fp,
						   XW,YW);
          
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
