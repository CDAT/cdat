/* Charles functions for truetype fonts and vcs_legacy */
/* #include <stdio.h> */

#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
/* #include <stdlib.h> */
/* #include <time.h>		/\* for time(), localtime(), and strftime() *\/ */
/* #include <sys/types.h>		/\* for uid_t *\/ */
/* #include <unistd.h>		/\* for getuid() & getlogin() *\/ */
/* #include <string.h> */
/* #include <math.h> */
/* #include <ctype.h> */
/* #include <assert.h> */
#include "gks_implem.h"
#include "workstations.h"
#include "gksshort.h"
#include "ttffonts.h"
#include "picture.h"
#include FT_STROKER_H
#include <cairo/cairo.h>
#include <cairo/cairo-ft.h>


extern FT_Face FT_FACE_FONTS[MAX_FONTS];
extern cairo_font_face_t *CAIRO_FONT_FACES[MAX_FONTS];
extern struct table_text *plot_pTt; 
extern struct table_chorn *plot_pTo;
extern struct table_FT_VCS_FONTS TTFFONTS;


int points_counter=0;
int npoints = 10;

extern Gconid_X_drawable connect_id;/*VCS canvas display and drawable id */
/* typedef for outline points (drawn later) */
/* typedef struct outline_points_ */
/* { */
/*   int x; */
/*   int y; */
/*   struct outline_points *next; */

/* } outline_points; */


/* float scalerx,scalery; */
/* float subsupscriptfactor = .45; */
int isClockwise( v,n )
    Gpoint *v;                 /* The vertex list */
    int n;                      /* Number of vertices */
{
    float area;
    int i;

    /* Do the wrap-around first */
    area = v[n-1].x * v[0].y - v[0].x * v[n-1].y;

    /* Compute the area (times 2) of the polygon */
    for (i = 0; i < n-1; i++)
        area += v[i].x * v[i+1].y - v[i+1].x * v[i].y;

    if (area >= 0.0)
      return 0; /* counter clock */
    else
      return 1; /* clockwise */
} /* End of orientation */


/* Gpoint xy2Rtheta(x2,y2,angle) */
/*      float x2,y2,angle; */
/* { */
/*   float R,theta; */
/*   Gpoint out; */

/*   if (x2!=0) */
/*     { */
/*       theta = atan(y2/x2); */
/*       R = x2/cos(theta); */
/*     } */
/*   else if (y2>0) */
/*     { */
/*       theta = -3.14159/2.; */
/*       R = y2/sin(theta); */
/*     } */
/*   else */
/*     {  */
/*       theta = 3.14159/2.; */
/*       R = y2/sin(theta); */
/*     } */
/*   out.x = R * cos(theta - angle/180.*3.14159); */
/*   out.y = R * sin(theta - angle/180.*3.14159); */
/*   return out; */
/* } */

/* Gpoint change_coord_system(head_point,index) */
/*      int index; */
/*      outline_head *head_point; */
/* { */
/*   float x,y; */
/*   float myscaler; */
/*   float x2,y2; */
/*   Gpoint converted; */

/*   x = (float)head_point->outline_points[index].x; */
/*   y = (float)head_point->outline_points[index].y; */
/*   myscaler = pow(subsupscriptfactor,abs(head_point->sscript)); */

/*   x2 = x*myscaler+(float)head_point->delta.x; */
/*   y2 = y*myscaler+(float)head_point->delta.y; */
/*   converted = xy2Rtheta(x2,y2,plot_pTo->chua); */
/*   converted.x=(converted.x)/scalerx*plot_pTo->chh+head_point->start.x; */
/*   converted.y=(converted.y)/scalery*plot_pTo->chh+head_point->start.y; */
/*   return converted; */
/* } */
/* /\* Little draw function ! *\/ */
/* /\* for now a python script is written *\/ */
/* void ttf2GKS(outline_head *head_point) */
/* { */
/* /\*   outline_points *current,*item; *\/ */
/*   int counter=1; */
/*   int clockwise,color,i; */
/*   FT_GlyphSlot glyph; */
/*   struct table_text *Tt; */
/*   Gpoint baryc; */

/*   Tt = plot_pTt; */
  

/* /\*   printf("sscript is %i\n",head_point->sscript); *\/ */
/* /\*   printf("delta x is %i\n",head_point->delta.x); *\/ */

/* /\*   myscaler = plot_pTo->chh; //\\*pow(subsupscriptfactor,abs(head_point->sscript)); *\/ */
/* /\*   printf("ok we are supposed to draw with points_cunter set to %i\n",points_counter); *\/ */
/*   for (counter=0;counter<points_counter;counter++) */
/*     { */
/*       head_point->outline_points[counter] = change_coord_system(head_point,counter); */
/*     } */

/* /\*   printf("this line min max: %f,%f,%i\n",mn,mx,head_point->delta.y); *\/ */
/*   clockwise = isClockwise(&head_point->outline_points[0],points_counter); */
/*   if  (head_point->orientation == FT_ORIENTATION_POSTSCRIPT) clockwise=1-clockwise ; /\* ps is opposite of truetype *\/ */
/*   // i=0 */

/*   gsfais(1); */
/*   if (clockwise) color=Tt->txci; */
/*   else color=Tt->txfci; */
/*   gsfaci(color); */
/* /\*   printf("ok we are supposed to draw with points_cunter set to %i\n",points_counter); *\/ */
/*   if (points_counter>nmax_outline_pts)  */
/*     { */
/*       printf("wwoooo wooo wooo we need more than %i points for this outline, will be truncated inform developpers to increase limit\n",nmax_outline_pts); */
/*       points_counter=nmax_outline_pts; */
/*     } */

/* /\*   baryc.x=0.;baryc.y=0.; *\/ */
/* /\*   for (i=0;i<points_counter;i++) { *\/ */
/* /\*     baryc.x+=head_point->outline_points[i].x; *\/ */
/* /\*     baryc.y+=head_point->outline_points[i].y; *\/ */
/* /\*   } *\/ */
/* /\*   baryc.x/=(float)(points_counter); *\/ */
/* /\*   baryc.y/=(float)(points_counter); *\/ */

/* /\*   item = select_item(self, point, NULL, NULL, pe_none); *\/ */
/* /\*   get_data_coords(self,point,item,&info) *\/ */
/* /\*   printf("barycnter of fill: %f, %f\n",xx,yy); *\/ */
/*   gfa(points_counter,&head_point->outline_points); */
/* /\*   if (color!=240) *\/ */
/* /\*     { *\/ */
/* /\*       gsln(1); *\/ */
/* /\*       gsplci(Tt->txci); *\/ */
/* /\*       gslwsc(.0000000000000000005); *\/ */
/* /\*       gpl(counter,xy); *\/ */
/* /\*     } *\/ */
/*   points_counter=0; */
/* } */


/* /\* Decomposition Functions *\/ */
/* FT_Outline_MoveToFunc mymove( const  FT_Vector *to, */
/* 			      outline_head *head_point) */
/* { */
/*   FT_Stroker mystroke; */
/*   /\* First see if we'd be drawing anytihng *\/ */
/* /\*   printf("in move to points_counter is: %i\n",points_counter); *\/ */
/*   if (points_counter!=0) */
/*     { */
/* /\*       printf("#Ok we are first drawing the previous outline %i,%i\n",current->x,current->y); *\/ */
/*       ttf2GKS(head_point); */
/*       /\* now cleans up the head_point pointer *\/ */
/*     } */
/* /\*   printf("****************************************\n"); *\/ */
/* /\*   printf("\tin move to: %i,%i\n",to->x,to->y); *\/ */
/*   head_point->outline_points[points_counter].x=(float)to->x; */
/*   head_point->outline_points[points_counter].y=(float)to->y; */
/* /\*   printf("VERY First point was: -----------------------   f%i,%i\n",current->x,current->y); *\/ */
/*   return 0; */
/* } */

/* FT_Outline_LineToFunc myline( const  FT_Vector *to, */
/* 			      outline_head *head_point) */
/* { */
/*   points_counter++; */
/*   head_point->outline_points[points_counter].x=(float)to->x; */
/*   head_point->outline_points[points_counter].y=(float)to->y; */
/*   return 0; */
/* } */
/* FT_Outline_ConicToFunc myconic( const FT_Vector *c1, */
/* 				const FT_Vector *to, */
/* 				outline_head *head_point) */
/* { */
/*   int i; /\* number of points on beziers curve *\/ */
/*   float b[npoints];  /\* bezier polygon 3D, so we will set z values to 0 *\/ */
/*   float p[(npoints+1)*3]; /\* points to add to plot max of 50 here*\/ */
/* /\*   return myline( to,head_point); *\/ */
/* /\*   char ch; *\/ */

/* /\*   printf("#we had %i points before\n",counter); *\/ */
/*   b[1]=(float) head_point->outline_points[points_counter].x; */
/*   b[2]=(float) head_point->outline_points[points_counter].y; */
/*   b[3]=(float) 0.; */
/*   b[4]=(float) c1->x; */
/*   b[5]=(float) c1->y; */
/*   b[6]=(float) 0.; */
/*   b[7]=(float) to->x; */
/*   b[8]=(float) to->y; */
/*   b[9]=(float) 0.; */

/* /\*   printf("\t#in conic z0: %i,%i\n",current->x,current->y); *\/ */
/* /\*   printf("\t#in conic c1: %i,%i\n",c1->x,c1->y); *\/ */
/* /\*   printf("\t#in conic to: %i,%i\n",to->x,to->y); *\/ */
/*   bezier(3,b,npoints,p); */

/*   /\* and now creates the added points to the polyline *\/ */
/*   for (i=1;i<npoints;i++) */
/*     { */
/*       if ((p[i*3+1]==p[(i-1)*3+1]) && (p[i*3+2]==p[(i-1)*3+2])) */
/* 	{ */
/* /\* 	  printf ("duplicate point skipped!\n"); *\/ */
/* 	} */
/*       else */
/* 	{ */
/* 	  points_counter++; */
/* 	  head_point->outline_points[points_counter].x=(float)p[i*3+1]; */
/* 	  head_point->outline_points[points_counter].y=(float)p[i*3+2]; */
/* 	} */
/*     } */
/*   return 0; */
/* } */

/* FT_Outline_CubicToFunc mycubic( const FT_Vector *c1, */
/* 				const FT_Vector *c2, */
/* 				const FT_Vector *to, */
/* 				outline_head *head_point) */
/* { */
/*   int i; /\* number of points on beziers curve *\/ */
/*   float b[13];  /\* bezier polygon 3D, so we will set z values to 0 *\/ */
/*   float p[(npoints+1)*3]; /\* points to add to plot max of 50 here*\/ */
/* /\*   return myline( to,head_point); *\/ */
/*   /\* go to the last point *\/ */

/*   b[1]=(float) head_point->outline_points[points_counter].x; */
/*   b[2]=(float) head_point->outline_points[points_counter].y; */
/*   b[3]=(float) 0.; */
/*   b[4]=(float) c1->x; */
/*   b[5]=(float) c1->y; */
/*   b[6]=(float) 0.; */
/*   b[7]=(float) c2->x; */
/*   b[8]=(float) c2->y; */
/*   b[9]=(float) 0.; */
/*   b[10]=(float) to->x; */
/*   b[11]=(float) to->y; */
/*   b[12]=(float) 0.; */

/* /\*   printf("\t#in cubic z0: %i,%i\n",current->x,current->y); *\/ */
/* /\*   printf("\t#in cubic c1: %i,%i\n",c1->x,c1->y); *\/ */
/* /\*   printf("\t#in cubic c2: %i,%i\n",c2->x,c2->y); *\/ */
/* /\*   printf("\t#in cubic to: %i,%i\n",to->x,to->y); *\/ */
/*   bezier(4,b,npoints,p); */

/*   /\* and now creates the added points to the polyline *\/ */
/*   for (i=1;i<npoints;i++) */
/*     { */
/*       if ((p[i*3+1]==p[(i-1)*3+1]) && (p[i*3+2]==p[(i-1)*3+2])) */
/* 	{ */
/* /\* 	  printf ("duplicate point skipped!\n"); *\/ */
/* 	} */
/*       else */
/* 	{ */
/* 	  points_counter++; */
/* 	  head_point->outline_points[points_counter].x=(float)p[i*3+1]; */
/* 	  head_point->outline_points[points_counter].y=(float)p[i*3+2]; */
/* 	} */
/*     } */
/*   return 0; */
/* } */

#include "vcs_legacy_names_length.h"
    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */
extern FT_Library ft_library;

int LoadFontNumber(n)
     int n;
{
  struct table_FT_VCS_FONTS *current;
  char name[VCS_MX_NM_LEN];
  int font_number;

  current = &TTFFONTS;
  while(current!=NULL)
    {
      if (current->index==n) break;
      current=current->next;
    }
  if (current==NULL)
    {
      /* printf("londfontnumber bug\n"); */
      err_warn(1,fperr,"Error - font number (%i) not found.\n",
	       n);
      return -1;
    }
  strcpy(name,current->name);
  font_number = LoadFont(name);
  return font_number;
}

int LoadFont(name)
     char name[VCS_MX_NM_LEN];
{
  struct table_FT_VCS_FONTS *current;
  int font_number,error;

  font_number = 0;
  current =&TTFFONTS; 
  while(current!=NULL)
    {
      if (strcmp(current->name,name)==0) break;
      current=current->next;
    }
  if (current==NULL)
    {
       printf("londfont bug\n");
     err_warn(1,fperr,"Error - font (%s) not found.\n",
	       name);
      return -1;
    }
  /* Ok we now have the font we are looking for ! */
  if (current->loaded == 0)
    {
      error = FT_New_Face( ft_library, current->path, 0, &FT_FACE_FONTS[current->index] );
      if ( error == FT_Err_Unknown_File_Format ) 
	{
	  printf("londfont read bug\n");
	  err_warn(1,fperr,"Error - The font file could be opened and read, but it appears ... that its font format is unsupported\n");
	  return -1;
	}
      else if ( error )
	{ 
	  printf("londfont no file bug\n");
	  err_warn(1,fperr,"Error - The font file could not ... be opened or read, or simply it is broken...\n");
	  return -1;
	}
      CAIRO_FONT_FACES[current->index] = (cairo_font_face_t *)cairo_ft_font_face_create_for_ft_face(FT_FACE_FONTS[current->index],0);
    }
  current->loaded=1;
  return current->index;
 
}

int AddFont(path,name)
     char path[2000];
     char name[VCS_MX_NM_LEN];
{
  FT_Face face;
  struct table_FT_VCS_FONTS *current,*item;
  int error;
  char local_name[VCS_MX_NM_LEN];
  int counter;

  /* first try to load the font */
  error = FT_New_Face( ft_library, path, 0, &face );
  if ( error == FT_Err_Unknown_File_Format ) 
    {
      err_warn(1,fperr,"Error - The font file could be opened and read, but it appears ... that its font format is unsupported\n");
      /* printf("first error font not supported: %s\n",path); */
      return error;
    }
  else if ( error )
    {
      err_warn(1,fperr,"Error - The font file could not ... be opened or read, or simply it is broken...\n");
      /* printf("wrong file %s\n",path); */
      return error;
    }
 
  if (strcmp("\0",name)==0) /* user did not pass a name */
    {
      strcpy(local_name,face->family_name);
    }
  else
    {
      strcpy(local_name,name);
    }

  current =&TTFFONTS;
  /* go to last font */ 
  counter=1;
  while((current->next!=NULL) && ( strcmp(local_name,current->name)!=0))
    {
      current=current->next;
      counter+=1;
    }

  if (strcmp(local_name,current->name)==0)
    {
      err_warn(1,fperr,"Error - Font name already exist!\n");
      FT_Done_Face(face);
      return 1;
    }

  if ((item = (struct table_FT_VCS_FONTS *) malloc (sizeof(struct table_FT_VCS_FONTS)))==NULL)
    {
      err_warn(1,fperr,"Error - Not enough memory to add a font\n");
      FT_Done_Face(face);
      return 1;
    }

  strcpy(item->name,local_name);
  strcpy(item->path,path);
  item->index = counter+1;
  item->loaded = 0;
  FT_FACE_FONTS[item->index]=face;
  item->next=NULL;
  current->next=item;


  return 0;
}

/*  string_def TTF_prep_string(string) */
/*     Gchar          *string; */
/* { */
/*   string_def preped; */
/*   int i,n,j,nerrors,error; */
/*   int dpix,dpiy; */
/*   int sc,char_size; */

/*   float funitsconvx; */
/*   float funitsconvy; */

/*   char* tmp_text; */
/*   char cntrl; */

/*   FT_UInt previous,index; */
/*   FT_Bool use_kerning; */
/*   FT_Vector delta;  */

/*   tmp_text=(char *)string; */
/*   preped.num_char = strlen(tmp_text); */
/* /\*   printf("figuring out %s with orientation: %s\n",string,plot_pTo->name); *\/ */

/*   /\* And now actual code *\/ */
/*   /\* First figures out super/subscript *\/ */
/*   /\* and determine how many actual char we have *\/ */

/*   previous = 0; */
/*   n=0; */
/*   for (i=0;i<preped.num_char;i++) */
/*     { */
/*       cntrl = (char)string[i]; */
/*       if (cntrl=='!')  */
/* 	{ */
/* 	  i++; */
/* 	  if ((char)string[i]=='U')  */
/* 	    { */
/* 	      /\* moving everything upsters *\/ */
/* 	      previous+=1; */
/* 	    } */
/* 	  else if ((char)string[i]=='D') */
/* 	    { */
/* 	      /\* moving everything upsters *\/ */
/* 	      previous-=1; */
/* 	    } */
/* 	  else /\* ok we want to display a BANG *\/ */
/* 	    { */
/* 	      preped.text[n]=(char)cntrl; */
/* 	      preped.char_level[n]=previous; */
/* 	      n++; */
/* 	      i--; */
/* 	    } */
/* 	} */
/*       else /\* ok we want to display this *\/ */
/* 	{ */
/* 	  preped.text[n]=(char)cntrl; */
/* 	  preped.char_level[n]=previous; */
/* 	  n++; */
/* 	} */
/*       if (n > nmax_char) */
/* 	{ */
/* 	  preped.error=1; */
/* 	  printf("Your string is over the maximum number of characters (%i)",nmax_char); */
/* 	  return preped; */
/* 	} */
/*     } */
/*   preped.text[n]='\0'; /\* safety blank char *\/ */

/*   preped.num_char=n; /\* saves number of characters drawn *\/ */

/*   /\* load the face for the font we need *\/ */
/*   preped.error = LoadFontNumber(plot_pTt->txfont,&preped.face); */
/*   if (preped.error!=0) return preped; */

/*   /\* face specifics *\/ */
/*   use_kerning = FT_HAS_KERNING( preped.face ); */

/*   /\* Now we figures out the dpi and scaling values *\/ */
/*   /\* dpis are arbitrary need to be big (i guess) for better printer output *\/ */
/*   dpix=1000; */
/*   dpiy=1000; */

/*   /\* played with the char_size for better screen rendition, not great though *\/ */
/*   char_size=150; /\* size of font for internal computation *\/ */

/*   /\* scaling in x/y dirs to go from font units to outline units *\/ */
/*   funitsconvx = 64.*char_size/preped.face->units_per_EM*dpix/72; */
/*   funitsconvy = 64.*char_size/preped.face->units_per_EM*dpiy/72; */

/*   /\* scaler is for us to go from outline units to good size on screen with default font size *\/ */
/*   scalerx= dpix*.8*char_size/plot_pTt->txexp; */
/*   scalery= dpiy*.8*char_size; */

/*   preped.error = FT_Set_Char_Size( preped.face, /\* handle to face object *\/ */
/* 			    0, /\* char_width in 1/64th of points *\/ */
/* 			    64*char_size, /\* char_height in 1/64th of points *\/ */
/* 			    dpix, /\* horizontal device resolution *\/ */
/* 			    dpiy ); /\* vertical device resolution *\/ */
/*   if (preped.error!=0) return preped; */
/*   /\* init previous glyph index *\/ */
/*   previous=0; */

/*   /\* init pen position *\/ */
/*   delta.x=0; */
/*   delta.y=0.; */

/*   /\* init total width/height of drawn text *\/ */
/*   preped.total.x=0; */
/*   preped.total.y=0; */

/*   /\* init delta for first char, deltas are pen start pos for drawing a glyph*\/ */
/*   preped.deltas[0].x=0; */
/*   preped.deltas[0].y=0; */

/*   /\* first loop to load the glyphs and compute the total width/height *\/ */
/*   nerrors=0; */
/*   for ( n = 0; n < preped.num_char; n++ )  */
/*     {  */
/*       preped.errors[n]=0; */
/*       index = FT_Get_Char_Index( preped.face, preped.text[n] );  */

/*       /\* load glyph image into the slot (erase previous one) *\/   */
/*       error = FT_Load_Char( preped.face, preped.text[n], FT_LOAD_DEFAULT ); */

/*       if (error)  */
/* 	{ */
/* 	  printf("error %i loading glyph %c for font %s\n",error,preped.text[n],preped.face->family_name); */
/* 	  preped.errors[n]=error; */
/* 	  nerrors++; */
/* 	  continue; */
/* 	} */

/*       /\* save glyph outlines for later when we will actually display it *\/ */
/*       i = preped.face->glyph->outline.n_points; */
/*       j = preped.face->glyph->outline.n_contours; */
/*       /\* we need to create the outline first *\/ */
/*       error = FT_Outline_New(ft_library, */
/* 			     i, */
/* 			     j, */
/* 			     &preped.ftglyph[n]); */
/*       if (error)  */
/* 	{ */
/* 	  printf("error %i creating outline for glyph %c for font %s\n",error,preped.text[n],preped.face->family_name); */
/* 	  preped.errors[n]=error; */
/* 	  nerrors++; */
/* 	  continue; */
/* 	} */
/*       /\* and now store it *\/ */
/*       error = FT_Outline_Copy(&preped.face->glyph->outline,&preped.ftglyph[n]); */
/*       if (error)  */
/* 	{ */
/* 	  printf("error %i copying outline for glyph %c for font %s\n",error,preped.text[n],preped.face->family_name); */
/* 	  preped.errors[n]=error; */
/* 	  nerrors++; */
/* 	  continue; */
/* 	} */

/*       /\* retrieve kerning distance and move pen position accordingly *\/  */
/*       /\* but only for right path align *\/ */
/*       if ( use_kerning && previous && index && plot_pTo->chpath=='r')  */
/* 	{  */
/* 	  FT_Get_Kerning( preped.face, previous, index, FT_KERNING_DEFAULT, &delta ); */
/* /\* 	  printf("kerned by                 ----------------------------------->%i\n",delta.x); *\/ */
/* 	  preped.deltas[n].x -= (int)delta.x; */
/* 	  preped.deltas[n].y -= (int)delta.y; */
/* 	} */
     
/*       if (preped.char_level[n]>0) /\* dealing with superscript *\/ */
/* 	{ */
/* 	  i = preped.face->ascender; */
/* 	} */
/*       else if (preped.char_level[n]<0) /\* dealing with subscript *\/ */
/* 	{ */
/* 	  i= preped.face->descender-preped.face->ascender; */
/* 	} */

/*       /\* Now scale i *\/ */
/*       i*=funitsconvy; */
/*       sc=0; */
/*       for (j=0;j<abs(preped.char_level[n]);j++) */
/* 	{ */
/* 	  	  sc+=pow(subsupscriptfactor,j)*i*.8; */
/* 	} */
/*       /\* moves pen up/down *\/ */
/*       preped.deltas[n].y += sc; */

/*       /\* now intialize the next delta values *\/ */
/*       /\* depends of path! *\/ */
/*       if (plot_pTo->chpath=='r') */
/* 	{ */
/* 	  preped.deltas[n+1].y = preped.deltas[n].y + preped.face->glyph->advance.y*pow(subsupscriptfactor,abs(preped.char_level[n])) - sc; */
/* 	  preped.deltas[n+1].x = preped.deltas[n].x + (5+plot_pTt->txsp)/5.*preped.face->glyph->advance.x*pow(subsupscriptfactor,abs(preped.char_level[n]));  */
/* 	} */
/*       else if (plot_pTo->chpath=='l') */
/* 	{ */
/* 	  preped.deltas[n].x = preped.deltas[n].x - (5+plot_pTt->txsp)/5.*preped.face->glyph->advance.x*pow(subsupscriptfactor,abs(preped.char_level[n]));  */
/* 	  preped.deltas[n+1].y = preped.deltas[n].y + preped.face->glyph->advance.y*pow(subsupscriptfactor,abs(preped.char_level[n])) - sc; */
/* 	  preped.deltas[n+1].x = preped.deltas[n].x ; */
/* 	} */
/*       else if (plot_pTo->chpath=='u') */
/* 	{ */
/* 	  preped.deltas[n+1].y = preped.deltas[n].y + (5+plot_pTt->txsp)/5.*(preped.face->height)*funitsconvy*pow(subsupscriptfactor,abs(preped.char_level[n])) - sc; */
/* 	  preped.deltas[n+1].x = preped.deltas[n].x ; */
/* 	} */
/*       else if (plot_pTo->chpath=='d') */
/* 	{ */
/* 	  preped.deltas[n].y = preped.deltas[n].y - (5+plot_pTt->txsp)/5.*(preped.face->height)*funitsconvy*pow(subsupscriptfactor,abs(preped.char_level[n])) - sc; */
/* 	  preped.deltas[n+1].y = preped.deltas[n].y ; */
/* 	  preped.deltas[n+1].x = preped.deltas[n].x ; */
/* 	} */


/*       /\* record current glyph index used for kerning*\/  */
/*       previous = index; */
/*     } */
/*   if (nerrors==preped.num_char) */
/*     { */
/*       preped.error=1; */
/*       return preped; */
/*     } */

/*   /\* code to figure out horizontal/vertical align *\/ */
/*   /\* computes total width/height from last drawn character + its width/height *\/ */
/*   /\* First figures out if we have horizontal or vertical lineup *\/ */
/*   if (preped.face->glyph->advance.y == NULL) /\* no y so it's horizontal layout *\/ */
/*     { */
/*       if (plot_pTo->chpath=='l' || plot_pTo->chpath=='r') */
/* 	{ */
/* 	  preped.total.x = abs(preped.deltas[n].x); */
/* 	  preped.total.y = (preped.face->ascender - preped.face->descender)*funitsconvy; /\*max height for face *\/ */
/* 	  preped.total.y = (preped.face->height)*funitsconvy; */
/* 	} */
/*       else */
/* 	{ */
/* 	  preped.total.x = (preped.face->bbox.xMax+preped.face->bbox.xMin)*funitsconvx; */
/* 	  preped.total.y = abs(preped.deltas[n].y); */
/* 	} */
/*     } */
/*   else /\*vertical layout *\/ */
/*     { */
/*       preped.total.y = abs(preped.deltas[n].y-preped.deltas[0].y); */
/*       preped.total.x = (preped.face->bbox.xMax-preped.face->bbox.xMin)*funitsconvx; */
/*       preped.total.x = (preped.face->bbox.xMax)*funitsconvx; */
/*     } */
/*   /\* Now we have the dimensions of the box for the text drawn *\/ */
/*   /\* we can figure out its alignment *\/ */
/*   delta.x=0; */
/*   delta.y=0; */
/*   if (plot_pTo->chalh=='r') /\*right justified *\/ */
/*     { */
/*       if (plot_pTo->chpath!='l') */
/* 	{ */
/* 	  delta.x = 0-preped.total.x; */
/* 	} */
/*     } */
/*   else if (plot_pTo->chalh=='c') /\* centered *\/ */
/*     { */
/*       if (plot_pTo->chpath!='l') */
/* 	{ */
/* 	  delta.x = 0-preped.total.x/2.; */
/* 	} */
/*       else */
/* 	{ */
/* 	  delta.x=preped.total.x/2.; */
/* 	} */
/*     } */
/*   else if (plot_pTo->chalh=='l') /\* centered *\/ */
/*     { */
/*        if (plot_pTo->chpath=='l') */
/* 	{ */
/* 	  delta.x = preped.total.x; */
/* 	} */
/*     } */
/*   if (plot_pTo->chalv=='t')  /\* aligned top *\/ */
/*     { */
/*       if (plot_pTo->chpath!='d') */
/* 	{ */
/* 	  delta.y= -preped.total.y; /\* needs to be fine tuned for V layout *\/ */
/* 	} */
/*     } */
/*   else if (plot_pTo->chalv=='c' )/\* cap aligned *\/ */
/*     { */
/*       if ((plot_pTo->chpath=='r')|| (plot_pTo->chpath=='l')) */
/* 	{ */
/* 	  delta.y= -preped.face->ascender*funitsconvy; /\* needs to be fine tuned for V layout *\/ */
/* 	} */
/*       else if (plot_pTo->chpath=='d') */
/* 	{ */
/* 	  delta.y = (preped.face->glyph->metrics.vertAdvance-preped.face->glyph->metrics.height);	 */
/* 	} */
/*       else if (plot_pTo->chpath=='u') */
/* 	{ */
/* 	  delta.y= -preped.total.y+(preped.face->glyph->metrics.vertAdvance-preped.face->glyph->metrics.height); */
/* 	} */
/*     } */
/*   else if (plot_pTo->chalv=='h') /\* half aligned *\/ */
/*     {  */
/*        if (plot_pTo->chpath!='d') */
/* 	{ */
/* 	  delta.y= -(preped.total.y/2.+preped.face->descender*funitsconvy); /\* needs to be fine tuned for V layout *\/ */
/* 	} */
/*        else */
/* 	{ */
/* 	  delta.y= (preped.total.y/2.+preped.face->descender*funitsconvy); /\* needs to be fine tuned for V layout *\/ */
/* 	} */
/*     } */
/*   else if (plot_pTo->chalv=='b') /\* bottom aligned  *\/  */
/*     {  */
/*       if (plot_pTo->chpath=='d') */
/* 	{ */
/* 	  delta.y= preped.total.y; /\* needs to be fine tuned for V layout *\/ */
/* 	} */

/*     } */
/*   else if (plot_pTo->chalv=='s') /\*surface bottom *\/ */
/*     { */
/*       if (plot_pTo->chpath!='d')  */
/* 	{ */
/* 	  delta.y = -preped.face->descender*funitsconvy; /\* needs to be fine tuned for V layout *\/ */
/* 	} */
/*       else  */
/* 	{ */
/* 	  delta.y= preped.total.y -preped.face->descender*funitsconvy;  */
/* 	} */
/*     } */
/*   for (n=0;n<preped.num_char;n++)  */
/*     { */
/*       preped.deltas[n].x+=delta.x; */
/*       preped.deltas[n].y+=delta.y; */
/*     } */
/*   return preped; */
/* } */

/* int TTF2VCS_drawString(at,string) */
/*     Gpoint         *at; */
/*     Gchar          *string; */
/* { */
/*   string_def prep; */
/*   FT_Outline_Funcs myfuncs; */
/*   int n; */
/*   int error; */

/*   outline_head head_point; */
/*   points_counter=0; */

/*   /\* prep the string for plotting *\/ */
/*   prep = TTF_prep_string(string); */

/*   if (prep.error!=0)  */
/*     { */
/*       FT_Done_Face(prep.face); */
/*       return prep.error; */
/*     } */
/*   myfuncs.move_to = (FT_Outline_MoveToFunc) mymove; */
/*   myfuncs.line_to = (FT_Outline_LineToFunc) myline; */
/*   myfuncs.conic_to = (FT_Outline_ConicToFunc)myconic; */
/*   myfuncs.cubic_to = (FT_Outline_CubicToFunc) mycubic; */
/*   myfuncs.delta=0; */
/*   myfuncs.shift=0; */

/*   head_point.start.x = at->x; */
/*   head_point.start.y = at->y; */


/*   /\* Now actually draws the glyphs *\/ */
/*   for ( n = 0; n < prep.num_char; n++ )  */
/*     { */
/*       if (prep.errors[n]!=0) continue; */
/*       /\* super/subscript ? *\/ */
/*       head_point.sscript=prep.char_level[n]; */
/*       /\* initialize the outlines pointer to make sure we don't redraw the last outline of the previous glyph  *\/ */
/*       points_counter=0; */
/*       /\* sets the delta *\/ */
/*       head_point.delta = prep.deltas[n]; */

/*       /\* Need to know orientation type to know if filling or not *\/ */
/*       head_point.orientation = FT_Outline_Get_Orientation(&prep.ftglyph[n]); */

/*       /\* decompose the outlines and fill them *\/ */
/*       error = FT_Outline_Decompose(&(prep.ftglyph[n]), */
/* 				   &myfuncs, */
/* 				   &head_point); */
/*       /\* draw the last outfill *\/ */
/*       ttf2GKS(&head_point); */
/*       /\* don't need the outline anymore *\/ */
/*       if (error==0) FT_Outline_Done(ft_library,&prep.ftglyph[n]); */
/*     } */
/*   /\* clean up part *\/ */
/*   FT_Done_Face(prep.face); */
/*   return 0; */
/* } */


/* void ttfgqtxx(wkid,pxy,str,extent) */
/*      int wkid; */
/*      Gpoint pxy; */
/*      char *str; */
/*      Gextent *extent; */
/* { */
/*   string_def prep; */
/*   outline_head hp; */
/*   Gpoint mypoint[2]; */
/*   float dx,dy,x0,x1,y0,y1; */

/*   /\* initialize *\/ */
/*   extent->ll.x = 0; */
/*   extent->ll.y = 0; */
/*   extent->ul.x = 0; */
/*   extent->ul.y = 0; */
/*   extent->ur.x = 0; */
/*   extent->ur.y = 0; */
/*   extent->lr.x = 0; */
/*   extent->lr.y = 0; */

/*   prep = TTF_prep_string(str); */
/*   if (prep.error!=0) */
/*     { */
/*       FT_Done_Face(prep.face); */
/*      /\* printf("error in gq!\n");*\/ */
/*       return ; */
/*     }       */
/*   hp.start=pxy; */
/*   hp.outline_points[0].x=0.; */
/*   hp.outline_points[0].y=0.; */
/*   hp.sscript=prep.char_level[0]; */
/*   hp.delta = prep.deltas[0]; */
/*   mypoint[0] = change_coord_system(&hp,0); */
/*   hp.outline_points[0].x=(float)(prep.total.x); */
/*   hp.outline_points[0].y=prep.total.y; */
/*   mypoint[1] = change_coord_system(&hp,0); */

/*   if (mypoint[1].x>mypoint[0].x) /\* left to right *\/ */
/*     { */
/*       x0 = mypoint[0].x; */
/*       x1 = mypoint[1].x; */
/*     } */
/*   else */
/*     {  dy = mypoint[1].y-mypoint[0].y; */

/*       x0 = mypoint[1].x; */
/*       x1 = mypoint[0].x; */
/*     } */
/*   if (mypoint[1].y>mypoint[0].y) /\* left to right *\/ */
/*     { */
/*       y0 = mypoint[0].y; */
/*       y1 = mypoint[1].y; */
/*     } */
/*   else */
/*     { */
/*       y0 = mypoint[1].y; */
/*       y1 = mypoint[0].y; */
/*     } */

/*   mypoint[0].x=x0; */
/*   mypoint[0].y=y0; */
/*   mypoint[1].x+=x0; */
/*   mypoint[1].y+=y0; */
/*   mypoint[1].x=x1; */
/*   mypoint[1].y=y1; */

/*   /\* ok just a comment to see if it shows up in eclipse!*\/ */
/*   /\* Now sets the extent *\/ */
/*   extent->ll.x = mypoint[0].x; */
/*   extent->ll.y = mypoint[0].y; */
/*   extent->ul.x = mypoint[0].x; */
/*   extent->ul.y = mypoint[1].y; */
/*   extent->ur.x = mypoint[1].x; */
/*   extent->ur.y = mypoint[1].y; */
/*   extent->lr.x = mypoint[1].x; */
/*   extent->lr.y = mypoint[0].y; */
/*   FT_Done_Face(prep.face); */
/* } */
