/* Charles functions for truetype fonts and vcs_legacy */
/* #include <stdio.h> */

#include <math.h>
#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include "gks_implem.h"
#include "workstations.h"
#include "gksshort.h"
#include "picture.h"
#include <ft2build.h>
#include FT_FREETYPE_H 
#include <cairo/cairo.h>
#include <cairo/cairo-ps.h>
#include <cairo/cairo-ft.h>
#include <cairo/cairo-features.h>
#include <cairo/cairo-pdf.h>
#include <cairo/cairo-ps.h>
#include <cairo/cairo-svg.h>
//#include <cairo/cairo-xlib.h>
/*#include <cairo/cairo-xlib-xrender.h>*/
#include "ttffonts.h"

#ifdef VCSQT
extern cairo_t *CAIRO_FOR_VCS;
#endif

extern FT_Face FT_FACE_FONTS[MAX_FONTS];
extern cairo_font_face_t *CAIRO_FONT_FACES[MAX_FONTS];

extern struct table_FT_VCS_FONTS TTFFONTS;
extern struct orientation Page;

extern Gconid_X_drawable connect_id;/*VCS canvas display and drawable id */

#include "vcs_legacy_names_length.h"
extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */
#include "color.h"
extern struct color_table C_tab;
extern char active_colors[17];
extern struct c_val std_color[16];
float    mylifter = .4;
float    myscaler = .58;

  extern char text_hpath,text_vpath,text_path;
  extern float text_height,text_angle;
  extern int text_color,text_font;

typedef  struct cairo_string_def_
{
  Gpoint deltas[nmax_char];
  int num_char;
  int error,errors[nmax_char];
  char text[nmax_char];
  int char_level[nmax_char];
  Gpoint total; /* total height and width (y/x) */
  Gpoint delta; /* total height and width (y/x) */
  cairo_font_face_t *font_face;
  FT_Face face;
 } cairo_string_def;


/* /\* sample write to ps func *\/ */
/* cairo_write_func_t mywrite(void *closure, unsigned char *data, unsigned int length); */
/* { */
/*   int i=4; */
/*   printf("hi\n"); */
/*   return CAIRO_STATUS_SUCCESS; */
/* } */

double xoffset(cr,preped,factor,alpha,delta,pass)
  cairo_t        *cr;
  cairo_string_def *preped;
  Gpoint *factor;
  double alpha;
  Gpoint delta;
  int pass;
{
  char cntrl2[2];
  cairo_text_extents_t extents;
  double dx,dx2;
  int i;
  dx=0;
  if (text_hpath=='r') /*right justified */
    {
      if (text_path!='l')
	{
	  dx = preped->total.x;
	  factor->x = -1;
	}
    }
  else if (text_hpath=='c') /* centered */
    {
      factor->x = -.5;
      dx = preped->total.x;
      
      if ((pass==2) && ((text_path=='d') || (text_path=='u'))) {
	for (i=0;i<preped->num_char;i++) {
	  if (preped->char_level[i]==0) {
	    cntrl2[0] = preped->text[i];
	    cntrl2[1] = '\0';
	    cairo_text_extents (cr, cntrl2, &extents);
	    dx2 = extents.x_advance - preped->total.x;
	    if (dx2<0.) {
	      preped->deltas[i].x-=dx2/2.;
	      preped->deltas[i+1].x+=dx2/2.;
	    }
	  }
	}
      }
    }
  else if (text_path=='l') /* left justified */
    {
       if (text_path=='l')
	{
	  factor->x = -1.;
	  dx = preped->total.x;
	}
       dx = dx;
    }
  if (pass==1) {
       dx = dx*cos(alpha)+delta.y*sin(alpha)*factor->y;
    }
  else {
       dx = factor->x*dx*cos(alpha)+delta.y*sin(alpha)*factor->y;
  }
  return dx;
}


double yoffset(factor,preped,font_extents,alpha,delta,pass)
  Gpoint *factor;
  cairo_string_def preped;
  cairo_font_extents_t font_extents;
  double alpha;
  Gpoint delta;
  int pass;
{
  double dy;
  dy=0.;
  if (text_vpath=='t')  /* aligned top */
    {
      factor->y=-1;
      if (text_path=='d') {
	dy= font_extents.height;
      }
      else if (text_path=='u') {
	factor->y=1;
	dy= font_extents.descent;
      }
      else {
	dy= preped.total.y;
      }
    }
  else if (text_vpath=='c' )/* cap aligned */
    {
      factor->y=-1;
      if (text_path=='u') {
      dy=  0;
      }
      else {
	dy= font_extents.ascent; 
      }
    }
  else if (text_vpath=='h') /* half aligned */
    { 
      if (text_path=='d') {
	factor->y=.5;
	dy= (preped.total.y-(2*font_extents.ascent+font_extents.descent));
      }
      else if  (text_path=='u') {
	factor->y=-.5;
	dy= (preped.total.y-(2*font_extents.ascent+font_extents.descent));
      }
      else {
	factor->y=-.5;
	dy= (preped.total.y-font_extents.descent);
      }
    }
  else if (text_vpath=='b') /* base aligned  */ 
    { 
      if (text_path=='d')
	{
	  factor->y=1.;
	  dy= preped.total.y-(font_extents.ascent+font_extents.descent); /* needs to be fine tuned for V layout */
	}
      else if (text_path=='u') {
	  factor->y=1.;
	dy= font_extents.ascent;
      }
    }
  else if (text_vpath=='s') /*bottom bottom */
    {
      if (text_path=='d') {
	  factor->y=1.;
	  dy= preped.total.y-(font_extents.ascent);
      }
      else {
	  factor->y=1.;
	dy = font_extents.descent; /* needs to be fine tuned for V layout */
      }
    }
  if (pass==1){
    dy = dy*cos(alpha)-delta.x*sin(alpha)*factor->x;
  }
  else {
    dy = factor->y*dy*cos(alpha)-delta.x*sin(alpha)*factor->x;
  }
  return dy;
}



 cairo_string_def CAIRO_prep_string(cr,string)
     cairo_t        *cr;
    char *          string;
{
  cairo_string_def preped;
  int i,n,j,nerrors,error;
  char tmp_text[1024];
  char cntrl;
  char cntrl2[2];
  int previous,index;

  cairo_text_extents_t extents;
  cairo_font_extents_t font_extents;
  cairo_font_options_t *font_options;
  Gpoint delta,factor,mydelta; 
  double dx,dy,dx2,dy2,dxp,dyp,alpha;
  long dheight,height,width;
  double font_size;
  double rx,x,y;
  double multiplier;
#ifdef X11WM
  XWindowAttributes 	xwa;
  int screen = DefaultScreen (connect_id.display);
#endif
#ifdef QTWM
  int qth,qtw,qtx,qty;
  extern int YW ;
#endif
  cairo_font_face_t *font_face;

/*   strcpy(tmp_text,string); */
  preped.num_char = strlen(string);
/*   printf("figuring out %s with orientation: %s\n",string,plot_pTo->name); */

  /* And now actual code */
  /* First figures out super/subscript */
  /* and determine how many actual char we have */

  previous = 0;
  preped.error = 0;
  n=0;
  for (i=0;i<preped.num_char;i++)
    {
      cntrl=(char)string[i];
      if (cntrl=='!') 
	{
	  i++;
	  if ((char )(string[i])=='U') 
	    {
	      /* moving everything upsters */
	      previous+=1;
	    }
	  else if ((char )(string[i])=='D')
	    {
	      /* moving everything upsters */
	      previous-=1;
	    }
	  else /* ok we want to display a BANG */
	    {
	      preped.text[n]=(char )cntrl;
	      preped.char_level[n]=previous;
	      n++;
	      i--;
	    }
	}
      else /* ok we want to display this */
	{
	  preped.text[n]=(char)cntrl;
	  preped.char_level[n]=previous;
	  n++;
	}
      if (n > nmax_char)
	{
	  preped.error=1;
	  printf("Your string is over the maximum number of characters (%i)",nmax_char);
	  return preped;
	}
    }
  preped.text[n]='\0'; /* safety blank char */
  preped.char_level[n]=0;
  preped.num_char=n; /* saves number of characters drawn */

  rx = 1.; /* postscript has fixed res, no need to adjust */
#ifdef X11WM
  if (cairo_surface_get_type(cairo_get_target(cr))==CAIRO_FORMAT_ARGB32) {

    height = cairo_image_surface_get_height(cairo_get_target(cr));

    multiplier = (float)height;

  }
  else if (cairo_surface_get_type(cairo_get_target(cr))==CAIRO_SURFACE_TYPE_XLIB) {

    dheight = DisplayHeight(connect_id.display,screen);
    XGetWindowAttributes(connect_id.display, connect_id.drawable, &xwa);
    multiplier = (float)xwa.height;

  }
#elif defined (QTWM)
  if (cairo_surface_get_type(cairo_get_target(cr))==CAIRO_FORMAT_ARGB32) {
    vcs_legacy_Qt_get_window_visibility_by_id(connect_id.wkst_id,&qtx);
    if (qtx==1) {
      vcs_legacy_Qt_get_window_dimensions_by_id(connect_id.wkst_id,&qtx,&qty,&qtw,&qth);
      multiplier = (float)qth;
    }
    else {
      multiplier = YW;
    }
  }
#else
  printf("insert here your WM func for rx comp\n");
  if (cairo_surface_get_type(cairo_get_target(cr))==CAIRO_FORMAT_ARGB32) {

    height = cairo_image_surface_get_height(cairo_get_target(cr));

    multiplier = (float)height;

  }
#endif
  else {
    extern int YW;
    multiplier = (float)YW;
  }

  /* printf("multiplier: %f, %f\n",multiplier,text_height); */
  
  /* printf("rx is: %f\n",rx); */
  font_size = (text_height+0.006)*multiplier*.6;
  /* printf("Font size ends up being: %f\n",font_size); */
  
  /* load the face for the font we need */

  preped.error = LoadFontNumber(text_font,&(preped.face));
  if (preped.error==-1) {
    fprintf(stderr,"error loading font\n");
    return preped;
  }
  font_face = CAIRO_FONT_FACES[preped.error];


  cairo_set_font_face (cr, font_face);
  cairo_set_font_size (cr, font_size);
  font_options = cairo_font_options_create ();
  cairo_get_font_options (cr, font_options);
  cairo_font_options_set_antialias (font_options, CAIRO_ANTIALIAS_GRAY);
  cairo_set_font_options (cr, font_options);
  
  cairo_font_options_destroy (font_options);
  cairo_font_extents (cr, &font_extents);
  
  /* init previous glyph index */
  previous=0;

  /* init pen position */
  delta.x=0;
  delta.y=0.;

  /* init total width/height of drawn text */
  preped.total.x=0;
  preped.total.y=0;

  /* init delta for first char, deltas are pen start pos for drawing a glyph*/
  preped.deltas[0].x=0;
  preped.deltas[0].y=0;
  preped.delta.x=0;
  preped.delta.y=0;

  /* first loop to load the glyphs and compute the total width/height */
  nerrors=0;
  for ( n = 0; n < preped.num_char; n++ ) 
    { 
      preped.errors[n]=0;
      cntrl2[0] = preped.text[n];
      cntrl2[1] = '\0';
      cairo_text_extents (cr, cntrl2, &extents);


      if ((text_path=='r') || (text_path=='l'))
	{
	  preped.deltas[n].y = 0.;
	  preped.deltas[n].x = 0.;
	  preped.total.x+=extents.x_advance*pow(myscaler,abs(preped.char_level[n]));
	  preped.total.y=font_extents.ascent+font_extents.descent;
	}
/*       else if (plot_pTo->chpath=='l') */
/* 	{ */
/* 	  printf("%c, extent: %f\n",cntrl,extents.x_advance); */
/* 	  preped.deltas[n].x -= extents.x_advance*pow(myscaler,abs(preped.char_level[n])); /\*we need to go back as much as it will advance *\/ */
/* 	  preped.deltas[n+1].x = -extents.x_advance*pow(myscaler,abs(preped.char_level[n])); /\*will need to go back to before this one next time *\/ */
/* 	  preped.total.x+=extents.x_advance*pow(myscaler,abs(preped.char_level[n]));  */
/* 	  preped.total.y=font_extents.ascent+font_extents.descent; */
/* 	  preped.deltas[n].y = 0.; */
/* 	} */
      else if ((text_path=='d') || (text_path=='u') )
	{
	  dx = extents.x_advance - preped.total.x;
	  if (dx>0) {
	    preped.total.x = extents.x_advance;
	  }
	  preped.total.y+=(font_extents.ascent+font_extents.descent);
	  dx =-extents.x_advance*pow(myscaler,abs(preped.char_level[n])); /* we need to go back before next drawing */
	  
	  while (preped.char_level[n+1]!=0)
	    {
	      preped.deltas[n+1].y = 0.;
	      preped.deltas[n+1].x = 0.;
	      cntrl2[0] = preped.text[n+1];
	      cntrl2[1] = '\0';
	      cairo_text_extents (cr, cntrl2, &extents);
	      dx -=extents.x_advance*pow(myscaler,abs(preped.char_level[n+1])); /* we need to go back before next drawing */
	      n+=1;
	    }
	  preped.deltas[n+1].x = dx;
	  preped.deltas[n+1].y = (font_extents.ascent+font_extents.descent);
	  if (text_path=='u') {
	    preped.deltas[n+1].y = -(font_extents.ascent+font_extents.descent);
	  }
	}

    }
  if (nerrors==preped.num_char)
    {
      preped.error=1;
      return preped;
    }

  /* Now we have the dimensions of the box for the text drawn */
  /* we can figure out its alignment */

  /* first w/o any angle */
  delta.x=0.;
  delta.y=0.;
  factor.x=1.;
  factor.y=1.;
  mydelta.x = xoffset(cr,&preped,&factor,0., delta,1);
  mydelta.y = yoffset(&factor,preped,font_extents,0.,delta,1);

/*   printf("no angle would be: %f,%f, f: %f,%f\n",mydelta.x,mydelta.y,factor.x,factor.y); */
  /* Now with any angle */

/*   if (cairo_surface_get_type(cairo_get_target(cr))==CAIRO_SURFACE_TYPE_XLIB) { */
/*     alpha = text_angle/180.*3.14159; */
/*   } */
/*   else { */
/*     alpha = -text_angle/180.*3.14159; */
/*   } */
    alpha = -text_angle/180.*3.14159;

  delta.x = xoffset(cr,&preped,&factor,alpha,mydelta,2);
  delta.y = yoffset(&factor,preped,font_extents,alpha,mydelta,2);

/*   printf("in prep setting deltas to: %f,%f,%f\n",delta.x,delta.y,text_angle); */
  preped.delta.x = delta.x;
  preped.delta.y = -delta.y;
  return preped;
}


void VCS2CAIRO_setrgb(cr,color)
     cairo_t *cr;
     int color;
{
  double r,g,b;
  int ic;
  struct color_table *ctab;
  for (ctab=&C_tab; strcmp(active_colors,ctab->name)!=0;ctab=ctab->next);
  if (color<240) {
    ic = color;
    r = ctab->cval[ic].red/100.;
    g = ctab->cval[ic].green/100.;
    b = ctab->cval[ic].blue/100.;
  }
  else {
    ic = color - 240;
    r = std_color[ic].red/100.;
    g = std_color[ic].green/100.;
    b = std_color[ic].blue/100.;
  }

  cairo_set_source_rgb (cr, r,g,b );
#ifdef GENCAIRO
  printf("cairo_set_source_rgb (cr, %f, %f, %f );\n",r,g,b);
#endif
}


void VCS2CAIRO_drawMarker(cr,type,size)
     cairo_t        *cr;
     int type;
     double size;
{
  switch(type) {
  case 0:
    break;
  case 1:
    cairo_scale(cr,size,size);
    break;
  }
  return;
}
int VCS2CAIRO_drawString(at,string,cr)
    Gpoint         *at;
    Gchar          *string;
    cairo_t        *cr;
{
    cairo_text_extents_t extents;
    cairo_status_t status;
    int r,ic,i;
    Gpoint          p1;
    cairo_string_def preped;
    char achar[2];
    double dx,dy,dx2,dy2,dxp,dyp,alpha;
    cairo_font_extents_t font_extents;
    cairo_font_face_t *font_face;

    cairo_matrix_t matrix;
    WS_STATE_PTR    ws;

    /* set color rgb according to colormap rgbs */
    VCS2CAIRO_setrgb(cr,text_color);
    preped = CAIRO_prep_string(cr,string);

    //printf("ok string name, angle, path: %s, %f, %c\n",string,text_angle,text_path);
    alpha = text_angle/180.*3.14159;

    cairo_font_extents (cr, &font_extents);

    if (text_path=='l') {
#ifdef GENCAIRO
      printf("cairo_matrix_init_scale( &matrix, -1.,1.);\n");
      printf("cairo_transform(cr,&matrix);\n");
#endif
      cairo_matrix_init_scale( &matrix,
			       -1.,1.);
      cairo_transform(cr,&matrix);
    }
    else if (text_path=='u') {
      cairo_matrix_init_scale( &matrix,
			       1.,-1.);
      cairo_transform(cr,&matrix);
#ifdef GENCAIRO
      printf("cairo_matrix_init_scale( &matrix, 1.,-1.);\n");
      printf("cairo_transform(cr,&matrix);\n");
#endif
   }
    dx = preped.delta.x;
    dy = preped.delta.y;
/*     printf("deltaS: %f, %f\n",dx,dy); */
#ifdef GENCAIRO
      printf("cairo_rel_move_to(cr,%f,%f);\n",dx,dy);
      printf("cairo_rotate(cr,%f);\n",-alpha);
#endif
      //printf("relative move to: %i, %i\n",dx,dy);
      cairo_rel_move_to(cr,dx,dy);
/*     printf("ok alhpa is: %f, and textangle is: %f\n",alpha,text_angle); */
/*     if (cairo_surface_get_type(cairo_get_target(cr))==CAIRO_SURFACE_TYPE_XLIB) { */
/*       cairo_rotate(cr,alpha); */
/*     } */
/*     else { */
/*       cairo_rotate(cr,-alpha); */
/*     } */
       cairo_rotate(cr,-alpha);
   for (i=0;i<preped.num_char;i++) {
      achar[0] = preped.text[i];
      achar[1] = '\0';
      dx2 = preped.deltas[i].x;
      dy2 = preped.deltas[i].y;

#ifdef GENCAIRO
      printf("achar[0] = '%c';\n",preped.text[i]);
      printf("achar[1] = '\\0';\n");
      printf("cairo_rel_move_to(cr,%f,%f);\n",dx2,dy2);
#endif
      cairo_rel_move_to(cr,dx2,dy2);
      ic = preped.char_level[i];
      if (ic!=0) {
	dx=(float)font_extents.height;
	if (ic<0) dx*=mylifter;
	dy=0.;
	
	for (r=0;r<abs(ic);r++) {
	  if (ic>0) {
	    dy += mylifter*dx;
	  }
	  else {
	    dy -= (1.-mylifter)*dx;
	  }
	  dx *= myscaler;
	}
	
	dy=-dy;
	
	dx=0;
/* 	cairo_user_to_device (cr, */
/* 			      &dx, */
/* 			      &dy); */
#ifdef GENCAIRO
	printf("cairo_rel_move_to(cr,%f,%f);\n",0.,dy);
	printf("cairo_scale(cr,%f,%f);\n",pow(myscaler,abs(ic)),pow(myscaler,abs(ic)));
#endif
	cairo_rel_move_to(cr,0.,dy);
	cairo_scale(cr,pow(myscaler,abs(ic)),pow(myscaler,abs(ic)));
	
      }
#ifdef GENCAIRO
	printf("cairo_show_text (cr, achar);\n");
#endif
      cairo_show_text (cr, achar);
      if (ic!=0) {
	cairo_scale(cr,pow(1./myscaler,abs(ic)),pow(1./myscaler,abs(ic)));
	cairo_rel_move_to(cr,0,-dy);
#ifdef GENCAIRO
	printf("cairo_scale(cr,%f,%f);\n",pow(1./myscaler,abs(ic)),pow(1./myscaler,abs(ic)));
	printf("cairo_rel_move_to(cr,%f,%f);\n",0.,-dy);
#endif
      }
      
   }
   cairo_rotate(cr,alpha);
#ifdef GENCAIRO
      printf("cairo_rotate(cr,%f);\n",alpha);
#endif

/*     font_face = cairo_get_font_face(cr); */
/*     cairo_font_face_destroy(font_face); */
    //FT_Done_Face(preped.face);

    return 0;
}


void cairogqtxx(int wkid,Gpoint pxy,char *str,Gextent *extent)
{
  cairo_string_def prep;
#ifdef X11WM
  int screen = DefaultScreen (connect_id.display);
  XWindowAttributes 	xwa;
#endif
  extern int XW,YW;
  cairo_t        *cr;
  cairo_surface_t *surface;
  float ratio ,angle; 
  float tx,ty;
  int width,height,xoff,yoff;
  Gextent extent2,extent3;
  extern float text_angle;
	extern struct orientation       Page;
	extern void printextent();
/*   if (connect_id.display == NULL) printf("display is NULL\n"); */
/*   if (connect_id.drawable == 0) printf("drawable is 0\n"); */
/* 	printf("qd wkid: %i, %i\n",wkid,connect_id.wkst_id); */
  if (wkid!=7) { /* ok we have an opened canvas */
#ifdef X11WM
    XGetWindowAttributes(connect_id.display, connect_id.drawable, &xwa);
    width = xwa.width;
    height = xwa.height;
/*     surface = (cairo_surface_t *)cairo_xlib_surface_create (connect_id.display, */
/* 							    connect_id.drawable, */
/* 							    DefaultVisual (connect_id.display, screen), */
/* 							    width, height);  */
#elif defined (QTWM)
/*     printf("getting dims\n"); */
    vcs_legacy_Qt_get_window_dimensions_by_id(connect_id.wkst_id,&xoff,&yoff,&width,&height);
/*     printf("back %i, %i\n",width,height); */
#else
    printf("insert here your WM getgeom\n");
#endif
  }
  else { /* ok it is bg mode, setting up fake stuff */
    if (strcmp(Page.page_orient,"landscape") == 0) {
      width = 827;
      height= 639;
      width=806;
      height=614;
      width=XW;
      height=YW;
    }
    else {
      width = 639;
      height= 827;
      width=614;
      height=806;
      width=XW;
      height=YW;
    }
/*     surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, */
/* 					 width,height); */
  }
    
/*     cr = cairo_create (surface); */
    
  ratio = (float)(width)/(float)(height);
  /* initialize */
  extent->ll.x = 0;
  extent->ll.y = 0;
  extent->ul.x = 0;
  extent->ul.y = 0;
  extent->ur.x = 0;
  extent->ur.y = 0;
  extent->lr.x = 0;
  extent->lr.y = 0;

  if (ratio>1) pxy.y*=ratio;
  else pxy.x/=ratio;

/*   printf("preping : %s\n",str); */
  prep = CAIRO_prep_string(connect_id.cr,str);

/*   printf("we have : %i, %i,%i\n",wkid,width, height); */
/*   printf("ratio: %f, location unnorm is: %f, %f\n",ratio,pxy.x,pxy.y); */
/*   printf("total: %f,%f\n",prep.total.x,prep.total.y); */
/*   printf("delta: %f,%f\n",prep.delta.x,prep.delta.y); */
/*   printf("angle: %f\n",text_angle); */
/*   if (angle<=90.) angle = (360-text_angle)/180.*3.1415926535897931; */
/*   else  angle = (text_angle)/180.*3.1415926535897931; */
  angle = (text_angle)/180.*3.1415926535897931;

  extent->ll.x = 0. ;
  extent->ur.x = prep.total.x;
  extent->lr.x = prep.total.x;
  extent->ul.x = 0;
  extent->ll.y = 0.;
  extent->lr.y = 0.;
  extent->ur.y = prep.total.y;
  extent->ul.y = prep.total.y;

  extent2.ll.x =   extent->ll.x * cos(angle) - extent->ll.y * sin(angle);
  extent2.ur.x =   extent->ur.x * cos(angle) - extent->ur.y * sin(angle);
  extent2.lr.x =   extent->lr.x * cos(angle) - extent->lr.y * sin(angle);
  extent2.ul.x =   extent->ul.x * cos(angle) - extent->ul.y * sin(angle);
  extent2.ll.y =   extent->ll.x * sin(angle) + extent->ll.y * cos(angle);
  extent2.ul.y =   extent->ul.x * sin(angle) + extent->ul.y * cos(angle);
  extent2.ur.y =   extent->ur.x * sin(angle) + extent->ur.y * cos(angle);
  extent2.lr.y =   extent->lr.x * sin(angle) + extent->lr.y * cos(angle);

  extent3.ll.x = prep.delta.x ;
  extent3.ll.y = -prep.delta.y ;
  extent3.lr.x = prep.delta.x ;
  extent3.lr.y = -prep.delta.y ;
  extent3.ur.x = prep.delta.x ;
  extent3.ur.y = -prep.delta.y ;
  extent3.ul.x = prep.delta.x ;
  extent3.ul.y = -prep.delta.y ;


  extent2.ll.x = extent3.ll.x+extent2.ll.x;
  extent2.ur.x = extent3.ur.x+extent2.ur.x;
  extent2.lr.x = extent3.lr.x+extent2.lr.x;
  extent2.ul.x = extent3.ul.x+extent2.ul.x;
  extent2.ul.y = extent3.ul.y+extent2.ul.y;
  extent2.ur.y = extent3.ur.y+extent2.ur.y;
  extent2.lr.y = extent3.lr.y+extent2.lr.y;
  extent2.ll.y = extent3.ll.y+extent2.ll.y;


  extent2.ll.x = pxy.x + extent2.ll.x/(float)(width);
  extent2.ur.x = pxy.x + extent2.ur.x/(float)(width);
  extent2.lr.x = pxy.x + extent2.lr.x/(float)(width);
  extent2.ul.x = pxy.x + extent2.ul.x/(float)(width);
  extent2.ul.y = pxy.y + extent2.ul.y/(float)(height);
  extent2.ur.y = pxy.y + extent2.ur.y/(float)(height);
  extent2.lr.y = pxy.y + extent2.lr.y/(float)(height);
  extent2.ll.y = pxy.y + extent2.ll.y/(float)(height);

  extent3.ll.x=extent2.ll.x;
  extent3.lr.x=extent2.lr.x;
  extent3.ur.x=extent2.ur.x;
  extent3.ul.x=extent2.ul.x;
  extent3.ll.y=extent2.ll.y;
  extent3.lr.y=extent2.lr.y;
  extent3.ur.y=extent2.ur.y;
  extent3.ul.y=extent2.ul.y;


  extent->ur.x = extent3.ur.x;
  extent->ul.x = extent3.ul.x;
  extent->ul.y = extent3.ul.y;
  extent->ur.y = extent3.ur.y;
  extent->lr.y = extent3.lr.y;
  extent->ll.y = extent3.ll.y;
  extent->ll.x = extent3.ll.x;
  extent->lr.x = extent3.lr.x;
  

  if (ratio>1) {
    extent->ll.y/=ratio;
    extent->lr.y/=ratio;
    extent->ul.y/=ratio;
    extent->ur.y/=ratio;
  }
  else
    {
    extent->ll.x*=ratio;
    extent->lr.x*=ratio;
    extent->ul.x*=ratio;
    extent->ur.x*=ratio;
    }


/*   cairo_destroy (cr); */
/*   printf("ll,ur: %f, %f, %f, %f\n",extent->ll.x,extent->ll.y,extent->ur.x,extent->ur.y); */
/*   cairo_surface_destroy (surface); */

}

