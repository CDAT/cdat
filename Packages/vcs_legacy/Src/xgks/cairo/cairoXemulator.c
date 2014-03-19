#include <cairo/cairo.h>
#include "xgks.h"
#ifdef X11WM
#include <X.h>
#elif defined QTWM
#include "Qt/Qt_X_emul.h"
#endif

void CAIROFillPolygon(cairo_t *cr, Gpoint *ype, int npnt, int shape, int mode) 
{
  int i;
  // printf("polyg: cr: %p\n",cr);
  if (mode != CoordModeOrigin ){
    printf("fill polygon in wrong coordmode, not implemented yet! your mode: %i vs %i\n",mode,CoordModeOrigin);
  }

  if (shape != Complex ) {
    printf("fill polygon in wrong shape mode, not implemented yet!\nResults may be different from expected ones");
  }
  cairo_move_to(cr,ype[0].x,ype[0].y);
  if (cairo_status(cr) != CAIRO_STATUS_SUCCESS ) {
    fprintf(stderr,"NOOOOOOOOOOOO BAD PMOVE TO: %s\n",cairo_status_to_string(cairo_status(cr)));
    exit(1);
  }
  for(i=1;i<npnt;i++) {
    cairo_line_to(cr,ype[i].x,ype[i].y);
    if (cairo_status(cr) != CAIRO_STATUS_SUCCESS ) {
      fprintf(stderr,"NOOOOOOOOOOOO BAD PLINE TO: %s\n",cairo_status_to_string(cairo_status(cr)));
      exit(1);
    }
  }
  cairo_fill(cr);
  if (cairo_status(cr) != CAIRO_STATUS_SUCCESS ) {
    fprintf(stderr,"NOOOOOOOOOOOO BAD PFILL%s\n",cairo_status_to_string(cairo_status(cr)));
    exit(1);
  }
}
void CAIRODrawLines(cairo_t *cr,Gpoint *ype,int npnt,int mode)
{
  int i;

/*    printf("line: cr: %p\n",cr); */
  if (mode != CoordModeOrigin ) {
    printf("drawing lines in wrong coordmode! your mode: %i vs %i\n",mode,CoordModeOrigin);
    exit(1);
  }
  cairo_move_to(cr,ype[0].x,ype[0].y);
  if (cairo_status(cr) != CAIRO_STATUS_SUCCESS ) {
    fprintf(stderr,"NOOOOOOOOOOOO BAD MOVETO%s\n",cairo_status_to_string(cairo_status(cr)));
    exit(1);
  }
  for(i=1;i<npnt;i++) {
    cairo_line_to(cr,ype[i].x,ype[i].y);
  if (cairo_status(cr) != CAIRO_STATUS_SUCCESS ) {
    fprintf(stderr,"NOOOOOOOOOOOO BAD LINETO%s\n",cairo_status_to_string(cairo_status(cr)));
    exit(1);
  }
  }
  cairo_stroke(cr);
  if (cairo_status(cr) != CAIRO_STATUS_SUCCESS ) {
    fprintf(stderr,"NOOOOOOOOOOOO BAD STROKE%s\n",cairo_status_to_string(cairo_status(cr)));
    exit(1);
  }
}

CAIRODrawMarkers(cr,pe,n,type,s)
    cairo_t *cr;
    Gpoint *pe;
    int             n, type;
    float           s;
{
    Gpoint        plus[2][2], star[3][2], cros[2][2];
    Gpoint        xseg[2];
    int i, j;
    double x0, y0, s1;
    double           x, y;

    
    switch (type) {

    case GMK_POINT:
      s1 = (double) (2. * (double)s);
      for (i = 0; i < n; i++) {
	x = (double) pe->x;
	y = (double) pe->y;
	x0 = (double) (pe->x - s);
	y0 = (double) (pe->y - s);
	cairo_arc(cr, x, y, s1, 0, 6.3);
	cairo_fill(cr);
	pe++;
      }
      break;
      
    case GMK_PLUS:
	plus[0][0].x = -s;
	plus[0][0].y = 0;
	plus[0][1].x = s;
	plus[0][1].y = 0;
	plus[1][0].x = 0;
	plus[1][0].y = s;
	plus[1][1].x = 0;
	plus[1][1].y = -s;
	for (i = 0; i < n; i++) {
	    x = pe->x;
	    y = pe->y;
	    for (j = 0; j < 2; j++) {
		xseg[0].x = x + plus[j][0].x;
		xseg[0].y = y + plus[j][0].y;
		xseg[1].x = x + plus[j][1].x;
		xseg[1].y = y + plus[j][1].y;
		CAIRODrawLines(cr, &xseg[0], 2, CoordModeOrigin);
	    }
	    pe++;
	}
	break;
	
    case GMK_STAR:
	star[0][0].x = -s;
	star[0][0].y = 0;
	star[0][1].x = s;
	star[0][1].y = 0;
	star[1][0].x = s * 0.5;
	star[1][0].y = s * 0.866;
	star[1][1].x = -s * 0.5;
	star[1][1].y = -s * 0.866;
	star[2][0].x = s * 0.5;
	star[2][0].y = -s * 0.866;
	star[2][1].x = -s * 0.5;
	star[2][1].y = s * 0.866;
	for (i = 0; i < n; i++) {
	    x = pe->x;
	    y = pe->y;
	    for (j = 0; j < 3; j++) {
		xseg[0].x = x + star[j][0].x;
		xseg[0].y = y + star[j][0].y;
		xseg[1].x = x + star[j][1].x;
		xseg[1].y = y + star[j][1].y;
		CAIRODrawLines(cr, &xseg[0], 2, CoordModeOrigin);
	    }
	    pe++;
	}
	break;

    case GMK_O:
      s1 = (double) (2 * (double)s);
      for (i = 0; i < n; i++) {
	x = (double) pe->x;
	y = (double) pe->y;
	x0 = (double) (pe->x - s);
	y0 = (double) (pe->y - s);
	cairo_arc(cr, x, y, s1, 0, 6.3);
	cairo_stroke(cr);
	pe++;
      }
      break;
      
    case GMK_X:
	cros[0][0].x = 0.5 * s;
	cros[0][0].y = 0.866 * s;
	cros[0][1].x = -0.5 * s;
	cros[0][1].y = -0.866 * s;
	cros[1][0].x = 0.5 * s;
	cros[1][0].y = -0.866 * s;
	cros[1][1].x = -0.5 * s;
	cros[1][1].y = 0.866 * s;
	for (i = 0; i < n; i++) {
	    x = pe->x;
	    y = pe->y;
	    for (j = 0; j < 2; j++) {
		xseg[0].x = x + cros[j][0].x;
		xseg[0].y = y + cros[j][0].y;
		xseg[1].x = x + cros[j][1].x;
		xseg[1].y = y + cros[j][1].y;
		CAIRODrawLines(cr, &xseg[0], 2, CoordModeOrigin);
	    }
	    pe++;
	}
	break;

    default:
	break;
    }

}
