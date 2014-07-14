#include "Python.h"
#include "gks.h"
#include "gksshort.h"
#include "ttffonts.h"
#ifdef USEX11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "array.h"
#include "list.h"
#include "color.h"
#include "picture.h"
#include "graph.h"
#include "display.h"
#include "project.h"
#include "workstations.h"
#include "run.h"
#include "cddrs.h"
#include "vcs_legacy_marker.h"
#include <ft2build.h>
#include <cairo/cairo.h>
#include <cairo/cairo-ft.h>
/* #include FT_FREETYPE_H  */

#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0
/* logo */
#include "logo.h"
 cairo_surface_t *logo;
cairo_pattern_t *logo_p;

#include "vcs_legacy_names_length.h"

    FILE *fpin, *fpout, *fperr; /* input, output, and error files for scripts */
    FILE *fpcgm,*frast;		/* cgm and raster files.		*/

    struct runit Run = {NULL,"",NULL};

    struct workstations Wkst[7]={1,"X_WIN",2,"CGM",3,"CAIRO",0,'\0'};

/*  		Set the VCS marker defaults				*/
    struct vcs_legacy_marker   Vma_tab={1,1,1};


/*		The display definition structure.			*/

/*  typedef struct
    {
     Display  *display;
     XID      drawable;
    } Gconid_X_drawable;
*/

#ifdef USEX11
    Gconid_X_drawable connect_id={NULL, (XID)NULL, 0, 0,
                                  (int)NULL, 0, 0, 0,
                                  (Colormap) NULL, NULL, (Pixmap) NULL,NULL,NULL,0};
#else
    Gconid_X_drawable connect_id;
#endif
    char *formid="-w %d -dp %ld -cmp %d";

#ifdef USEX11
    Colormap n_cmap;
#endif

    struct a_tab A_tab={"",0,NULL,NULL};
    struct a_tab A_tmp={"",0,NULL,NULL};

#define NFUNC 2

/* Default "dot" directory, used to be PCMDI_GRAPHICS */

/* postscript default margins */
int MARGINL=.2*72; /* in inches * 72dpi */
int MARGINT=.2*72;
int MARGINR=.2*72; /* in inches * 72dpi */
int MARGINB=.2*72;
int XW = 806;
int YW = 614;
int draw_white_background = 0;

    struct func_list
      {
       char name[17];
       int num_args;
      } funcs[NFUNC]={"sqrt",1,"mean",MAXARGS};


    char Lon30[25][5]={"0","30E","60E","90E","120E","150E","180",
		       "150W","120W","90W","60W","30W",
			"0","30E","60E","90E","120E","150E","180",
		       "150W","120W","90W","60W","30W","0"};
    char Lat20[10][5]={"90S","70S","50S","30S","10S",
		       "10N","30N","50N","70N","90N"};

    struct l_val Lon30_val[25]={-360,Lon30[0],&Lon30_val[1],
				-330,Lon30[1],&Lon30_val[2],
				-300,Lon30[2],&Lon30_val[3],
				-270,Lon30[3],&Lon30_val[4],
				-240,Lon30[4],&Lon30_val[5],
				-210,Lon30[5],&Lon30_val[6],
				-180,Lon30[6],&Lon30_val[7],
				-150,Lon30[7],&Lon30_val[8],
				-120,Lon30[8],&Lon30_val[9],
				 -90,Lon30[9],&Lon30_val[10],
				 -60,Lon30[10],&Lon30_val[11],
				 -30,Lon30[11],&Lon30_val[12],
				   0,Lon30[12],&Lon30_val[13],
				  30,Lon30[13],&Lon30_val[14],
				  60,Lon30[14],&Lon30_val[15],
				  90,Lon30[15],&Lon30_val[16],
				 120,Lon30[16],&Lon30_val[17],
				 150,Lon30[17],&Lon30_val[18],
				 180,Lon30[18],&Lon30_val[19],
				 210,Lon30[19],&Lon30_val[20],
				 240,Lon30[20],&Lon30_val[21],
				 270,Lon30[21],&Lon30_val[22],
				 300,Lon30[22],&Lon30_val[23],
				 330,Lon30[23],&Lon30_val[24],
				 360,Lon30[24],NULL};
    struct l_val Lat20_val[10]={-90,Lat20[0],&Lat20_val[1],
				-70,Lat20[1],&Lat20_val[2],
				-50,Lat20[2],&Lat20_val[3],
				-30,Lat20[3],&Lat20_val[4],
				-10,Lat20[4],&Lat20_val[5],
				 10,Lat20[5],&Lat20_val[6],
				 30,Lat20[6],&Lat20_val[7],
				 50,Lat20[7],&Lat20_val[8],
				 70,Lat20[8],&Lat20_val[9],
				 90,Lat20[9],NULL};
    struct iso Gi_line={0,1,0.0,1.e20,0,"*","default","default",
			"default",0,1.,35.,1.,NULL};
    struct fill_range Gfb_line={0,1.e20,1.e20,"default",NULL};

    struct fill_range Gfi_line={0,1.e20,1.e20,"default",NULL};

    struct fill_range Gfm_line={0,1.e20,1.e20,"default",NULL};

    struct l_tab L_tab[2]={"lon30",25,&Lon30_val[0],&L_tab[1],
			   "lat20",10,&Lat20_val[0],NULL};
    struct gi_attr Gi_a={"linear","*","*","","",
				     "*","*","","",
				     1.e20,1.e20,1.e20,1.e20,
                                     "linear","linear",'n',
			             VCS_DEFAULT_TIME_UNITS,VCS_DEFAULT_CALENDAR,
			             0,0,0,0,
			             &Gi_line};
    struct go_attr Go_a={"linear","*","*","","",
				     "*","*","","",
				     1.e20,1.e20,1.e20,1.e20,
                                     "linear","linear","default",
       			             1,2,3,4,5,6,8,0,0,0, 1,
			             VCS_DEFAULT_TIME_UNITS,VCS_DEFAULT_CALENDAR,
			             0,0,0,0};

    struct gfi_attr Gfi_a={"linear","*","*","","",
				     "*","*","","",
				     1.e20,1.e20,1.e20,1.e20,
                                     "linear","linear",1.e20,
			             VCS_DEFAULT_TIME_UNITS,VCS_DEFAULT_CALENDAR,
			             0,0,0,0,
			             NULL,&Gfi_line};
    struct gfb_attr Gfb_a={"linear","*","*","","",
				     "*","*","","",
				     1.e20,1.e20,1.e20,1.e20,
				     "linear","linear",1.e20,1.e20,
                                     16,239,0,NULL,'n','n',1,
			             VCS_DEFAULT_TIME_UNITS,VCS_DEFAULT_CALENDAR,
			             0,0,0,0,
			             &Gfb_line};
    struct gfm_attr Gfm_a={"linear","*","*","","",
				     "*","*","","",
				     1.e20,1.e20,1.e20,1.e20,
                                     "linear","linear",1,0.,0.,0,
			             VCS_DEFAULT_TIME_UNITS,VCS_DEFAULT_CALENDAR,
			             0,0,0,0,
			             NULL,&Gfm_line};
    struct gv_attr Gv_a={"linear","*","*","","",
				     "*","*","","",
				     1.e20,1.e20,1.e20,1.e20,
				     "linear","linear","default",
                                     1.0,'c',2,1.e20,
			             VCS_DEFAULT_TIME_UNITS,VCS_DEFAULT_CALENDAR,
			             0,0,0,0};
    struct gXy_attr GXy_a={"linear","*","*","","",
				     "*","*","","",
				     1.e20,1.e20,1.e20,1.e20,
			             "linear","linear","default","default",
			             VCS_DEFAULT_TIME_UNITS,VCS_DEFAULT_CALENDAR,
			             0,0,0,0};

    struct gYx_attr GYx_a={"linear","*","*","","",
				     "*","*","","",
				     1.e20,1.e20,1.e20,1.e20,
			             "linear","linear","default","default",
			             VCS_DEFAULT_TIME_UNITS,VCS_DEFAULT_CALENDAR,
			             0,0,0,0};
    struct gXY_attr GXY_a={"linear","*","*","","",
				     "*","*","","",
				     1.e20,1.e20,1.e20,1.e20,
			             "linear","linear","default","default",
			             VCS_DEFAULT_TIME_UNITS,VCS_DEFAULT_CALENDAR,
			             0,0,0,0};

    struct gSp_attr GSp_a={"linear","*","*","","",
				     "*","*","","",
				     1.e20,1.e20,1.e20,1.e20,
			             "linear","linear","default",
 			             VCS_DEFAULT_TIME_UNITS,VCS_DEFAULT_CALENDAR,
			             0,0,0,0};
   struct gfo_attr Gfo_a={"linear","*","*","","",
				     "*","*","","",
				     1.e20,1.e20,1.e20,1.e20,
                                     "linear","linear","default",
			             1,2,3,4,5,6,8,0,0,0, 1,
			             VCS_DEFAULT_TIME_UNITS,VCS_DEFAULT_CALENDAR,
			             0,0,0,0};

    struct gcon_attr Gcon_a={"linear","lon30","lon30","","",
				     "lat20","lat20","","",
			             -180,-90,180,90,"default", 1,
			             VCS_DEFAULT_TIME_UNITS,VCS_DEFAULT_CALENDAR,
			             0,0,0,0};



/*			Choice of default continents for overlay
			when dimensions are longitude ,latitude. */

    struct default_continents Dc={1,"continents","None","Fine","Coarse",
                                    "United States", "Political Borders",
                                    "Rivers"};

    struct gi_tab Gi_tab={"default",&Gi_a,NULL};
    struct go_tab Go_tab={"default",&Go_a,NULL};
    struct gfi_tab Gfi_tab={"default",&Gfi_a,NULL};
    struct gfb_tab Gfb_tab={"default",&Gfb_a,NULL};
    struct gfm_tab Gfm_tab={"default",&Gfm_a,NULL};
    struct gv_tab Gv_tab={"default",&Gv_a,NULL};
    struct gXy_tab GXy_tab={"default",&GXy_a,NULL};
    struct gYx_tab GYx_tab={"default",&GYx_a,NULL};
    struct gXY_tab GXY_tab={"default",&GXY_a,NULL};
    struct gSp_tab GSp_tab={"default",&GSp_a,NULL};
    struct gfo_tab Gfo_tab={"default",&Gfo_a,NULL};
    struct gcon_tab Gcon_tab={"default",&Gcon_a,NULL};

    struct display_tab D_tab={1, "",1,0,-1,"","dummy_for_now",
		"default","default","default",0,
		"","","","","","",
		0,0,0,1, 0,0,0,1,
		0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1,
		0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1,
		0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1,
		0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1,
		0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1,
		0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1,
		0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1,
		1.e20,1.e20,1.e20,1.e20,
		NULL};

    struct c_val std_color[16]={100,100,100,  0  ,0  ,0  ,100,0  ,0  ,
			    0  ,100,0  ,  0  ,0  ,100,100,100,0  ,
			    0  ,100,100,100  ,0  ,100,100,50 , 10,
			    60 ,30 ,10 ,  5  ,10 ,67 ,50 ,50 ,0  ,
			    80 ,80 ,80 ,  80 ,100,80 ,95 ,75 ,75 ,
			    60 ,80 ,100 };

    struct color_table C_tab={"default",
   100,100,100,   0,0,0,  
   85,85,85,  32,32,32,  100,100,100,  100,100,0,

   0,2.7451,100,   0,5.88235,100,   0,9.01961,100,   0,11.7647,100,   
   0,14.902,100,   0,18.0392,100,
   0,21.1765,100,   0,23.9216,100,   0,27.0588,100,   0,30.1961,100,   
   0,33.3333,100,   0,36.0784,100,
   0,39.2157,100,   0,42.3529,100,   0,45.098,100,   0,48.2353,100,   
   0,51.3725,100,   0,54.5098,100,
   0,57.2549,100,   0,60.3922,100,   0,63.5294,100,   0,66.6667,100,   
   0,69.4118,100,   0,72.549,100,
   0,75.6863,100,   0,78.4314,100,   0,81.5686,100,   0,84.7059,100,   
   0,87.8431,100,   0,90.5882,100,
   0,93.7255,100,   0,96.8627,100,   0,100,100,   0,100,96.8627,   
   0,100,93.7255,   0,100,90.5882,
   0,100,87.8431,   0,100,84.7059,   0,100,81.5686,   0,100,78.4314,   
   0,100,75.6863,   0,100,72.549,
   0,100,69.4118,   0,100,66.6667,   0,100,63.5294,   0,100,60.3922,   
   0,100,57.2549,   0,100,54.5098,
   0,100,51.3725,   0,100,48.2353,   0,100,45.098,   0,100,42.3529,   
   0,100,39.2157,   0,100,36.0784,
   0,100,33.3333,   0,100,30.1961,   0,100,27.0588,   0,100,23.9216,   
   0,100,21.1765,   0,100,18.0392,
   0,100,14.902,   0,100,11.7647,   0,100,9.01961,   0,100,5.88235,   
   0,100,2.7451,   0,100,0,
   2.7451,100,0,   5.88235,100,0,   9.01961,100,0,   11.7647,100,0,   
   14.902,100,0,   18.0392,100,0,
   21.1765,100,0,   23.9216,100,0,   27.0588,100,0,   30.1961,100,0,   
   33.3333,100,0,   36.0784,100,0,
   39.2157,100,0,   42.3529,100,0,   45.098,100,0,   48.2353,100,0,   
   51.3725,100,0,   54.5098,100,0,
   57.2549,100,0,   60.3922,100,0,   63.5294,100,0,   66.6667,100,0,   
   69.4118,100,0,   72.549,100,0,
   75.6863,100,0,   78.4314,100,0,   81.5686,100,0,   84.7059,100,0,   
   87.8431,100,0,   90.5882,100,0,
   93.7255,100,0,   96.8627,100,0,   100,100,0,   100,97.6471,0,   
   100,95.6863,0,   100,93.7255,0,
   100,91.7647,0,   100,89.8039,0,   100,87.8431,0,   100,85.4902,0,   
   100,83.5294,0,   100,81.5686,0,
   100,79.6078,0,   100,77.6471,0,   100,75.6863,0,   100,73.7255,0,   
   100,71.3726,0,   100,69.4118,0,
   100,67.451,0,   100,65.4902,0,   100,63.5294,0,   100,61.5686,0,   
   100,59.2157,0,   100,57.2549,0,
   100,55.2941,0,   100,53.3333,0,   100,51.3725,0,   100,49.4118,0,   
   100,47.451,0,   100,45.098,0,
   100,43.1373,0,   100,41.1765,0,   100,39.2157,0,   100,37.2549,0,   
   100,35.2941,0,   100,33.3333,0,
   100,32.1569,0,   100,30.9804,0,   100,30.1961,0,   100,29.0196,0,   
   100,28.2353,0,   100,27.0588,0,
   100,25.8824,0,   100,25.098,0,   100,23.9216,0,   100,23.1373,0,   
   100,21.9608,0,   100,21.1765,0,
   100,20,0,   100,18.8235,0,   100,18.0392,0,   100,16.8627,0,   
   100,16.0784,0,   100,14.902,0,
   100,14.1176,0,   100,12.9412,0,   100,11.7647,0,   100,10.9804,0,   
   100,9.80392,0,   100,9.01961,0,
   100,7.84314,0,   100,7.05882,0,   100,5.88235,0,   100,4.70588,0,   
   100,3.92157,0,   100,2.7451,0,
   100,1.96078,0,   100,0.784314,0,   100,0,0,   98.0392,0,0,   
   96.0784,0,0,   94.1176,0,0,
   92.1569,0,0,   90.1961,0,0,   88.6274,0,0,   86.6667,0,0,   84.7059,0,0,   
   82.7451,0,0,
   80.7843,0,0,   79.2157,0,0,   77.2549,0,0,   75.2941,0,0,   73.3333,0,0,   
   71.3726,0,0,
   69.4118,0,0,   67.8431,0,0,   65.8824,0,0,   63.9216,0,0,   61.9608,0,0,   
   60,0,0,
   58.4314,0,0,   56.4706,0,0,   54.5098,0,0,   52.549,0,0,   50.5882,0,0,   
   48.6275,0,0,
   47.0588,0,0,   45.098,0,0,   43.1373,0,0,   41.1765,0,0,   39.2157,0,0,   
   37.6471,0,0,
   38.4314,0,1.96078,   39.2157,0,3.92157,   40.3922,0,5.88235,   
   41.1765,0,7.84314,   42.3529,0,10.1961,   43.1373,0,12.1569,
   44.3137,0,14.1176,   45.098,0,16.0784,   46.2745,0,18.4314,   
   47.0588,0,20.3922,   48.2353,0,22.3529,   49.0196,0,24.3137,
   50.1961,0,26.2745,   50.9804,0,28.6275,   52.1569,0,30.5882,   
   52.9412,0,32.549,   54.1176,0,34.5098,   54.902,0,36.8627,
   55.6863,0,38.8235,   56.8627,0,40.7843,   57.6471,0,42.7451,   
   58.8235,0,44.7059,   59.6078,0,47.0588,   60.7843,0,49.0196,
   61.5686,0,50.9804,   62.7451,0,52.9412,   63.5294,0,55.2941,   
   64.7059,0,57.2549,   65.4902,0,59.2157,   66.6667,0,61.1765,
   67.451,0,63.1373,   68.6274,0,65.4902,   69.4118,0,67.451,   
   70.5882,0,69.4118,   71.3726,0,71.3726,   72.549,0,73.7255,
        NULL };

    struct table_text Tt_tab={"default","default",1,2,
			      1.0,0.2,
			      1,1,0,
			      0.0,1.0,0.0,1.0,
                              0.0,1.0,0.0,1.0,NULL,NULL,NULL,NULL};

    struct table_chorn To_tab4={"defcentup",0.015,-90.0,'r','c','h',NULL};
    struct table_chorn To_tab3={"defcentdown",0.015,90.0,'r','c','h',&To_tab4};
    struct table_chorn To_tab2={"defright",0.015,0.0,'r','r','h',&To_tab3};
    struct table_chorn To_tab1={"defcenter",0.015,0.0,'r','c','h',&To_tab2};
    struct table_chorn To_tab={"default",0.015,0.0,'r','l','h',&To_tab1};

    struct table_line Tl_tab={"default","default",NULL,0,NULL,0,NULL,0,1,0.0,1.0,0.0,1.0,
                              0.0,1.0,0.0,1.0,NULL,NULL,NULL};

    struct table_fill Tf_tab_auto_0={"AuTo_0","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,NULL};
    struct table_fill Tf_tab_auto_1={"AuTo_1","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_0};
    struct table_fill Tf_tab_auto_2={"AuTo_2","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_1};
    struct table_fill Tf_tab_auto_3={"AuTo_3","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_2};
    struct table_fill Tf_tab_auto_4={"AuTo_4","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_3};
    struct table_fill Tf_tab_auto_5={"AuTo_5","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_4};
    struct table_fill Tf_tab_auto_6={"AuTo_6","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_5};
    struct table_fill Tf_tab_auto_7={"AuTo_7","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_6};
    struct table_fill Tf_tab_auto_8={"AuTo_8","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_7};
    struct table_fill Tf_tab_auto_9={"AuTo_9","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_8};
    struct table_fill Tf_tab_auto_10={"AuTo_10","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_9};
    struct table_fill Tf_tab_auto_11={"AuTo_11","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_10};
    struct table_fill Tf_tab_auto_12={"AuTo_12","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_11};
    struct table_fill Tf_tab_auto_13={"AuTo_13","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_12};
    struct table_fill Tf_tab_auto_14={"AuTo_14","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_13};
    struct table_fill Tf_tab={"default","default",NULL,0,NULL,0,NULL,0,0,0,0.1,0.1,1,
                              0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,NULL,NULL,&Tf_tab_auto_14};

    struct table_mark Tm_tab={"default","default",NULL,0,NULL,0,NULL,0,1,0.0,1.0,0.0,1.0,
                              0.0,1.0,0.0,1.0,NULL,NULL,NULL};
    FT_Face FT_FACE_FONTS[MAX_FONTS];
    cairo_font_face_t *CAIRO_FONT_FACES[MAX_FONTS];
    struct table_FT_VCS_FONTS TTFFONTS_16 = {"AvantGarde","AvantGarde-Book_Bold.ttf",16,0,NULL};
    struct table_FT_VCS_FONTS TTFFONTS_15 = {"Maths4","blsy.ttf",15,0,&TTFFONTS_16};
    struct table_FT_VCS_FONTS TTFFONTS_14 = {"Maths3","jsMath-wasy10.ttf",14,0,&TTFFONTS_15};
    struct table_FT_VCS_FONTS TTFFONTS_13 = {"Maths2","blex.ttf",13,0,&TTFFONTS_14};
    struct table_FT_VCS_FONTS TTFFONTS_12 = {"Maths1","jsMath-msam10.ttf",12,0,&TTFFONTS_13};
    struct table_FT_VCS_FONTS TTFFONTS_11 = {"Russian","Russian.ttf",11,0,&TTFFONTS_12};
    struct table_FT_VCS_FONTS TTFFONTS_10 = {"Hebrew","hebrew.ttf",10,0,&TTFFONTS_11};
    struct table_FT_VCS_FONTS TTFFONTS_9  = {"Greek","Athens_Greek.ttf",9,0,&TTFFONTS_10};
    struct table_FT_VCS_FONTS TTFFONTS_8  = {"Chinese","Chinese_Generic1.ttf",8,0,&TTFFONTS_9};
    struct table_FT_VCS_FONTS TTFFONTS_7  = {"Arabic","Arabic.ttf",7,0,&TTFFONTS_8};
    struct table_FT_VCS_FONTS TTFFONTS_6  = {"Adelon","Adelon_Regular.ttf",5,0,&TTFFONTS_7};
    struct table_FT_VCS_FONTS TTFFONTS_5  = {"Clarendon","Clarendon.ttf",2,0,&TTFFONTS_6};
    struct table_FT_VCS_FONTS TTFFONTS_4  = {"Helvetica","HelvMono.ttf",4,0,&TTFFONTS_5};
    struct table_FT_VCS_FONTS TTFFONTS_3  = {"default","AvantGarde-Book_Bold.ttf",1,0,&TTFFONTS_4};
    struct table_FT_VCS_FONTS TTFFONTS_2  = {"Times","Times_CG_ATT.ttf",6,0,&TTFFONTS_3};
    struct table_FT_VCS_FONTS TTFFONTS    = {"Courier","Courier.ttf",3,0,&TTFFONTS_2};


    int pattern_ary[16]={1,0,1,0, 0,1,0,1, 1,0,1,0, 0,1,0,1};

    struct table_pattern Pat_tab={1,4,4,pattern_ary,NULL};

    struct form Fstd9={"mean","*","%n %g",NULL};
    struct form Fstd8={"min","*","%n %g",&Fstd9};
    struct form Fstd7={"max","*","%n %g",&Fstd8};
    struct form Fstd6={"time","month","%t[Mon/y]",&Fstd7};
    struct form Fstd5={"time","hour","%t[h d/m/y]",&Fstd6};
    struct form Fstd4={"time","season","%t[Sea/y]",&Fstd5};
    struct form Fstd3={"time","S","%t[Season/y]",&Fstd4};
    struct form Fstd2={"time","day","%t[Month/y]",&Fstd3};
    struct form Fstd1={"month","*","%t[Month/y]",&Fstd2};
    struct form Fstd={"time","second","%n %t[h:m:s]",&Fstd1};
    struct table_form Th_tab={"default",&Fstd,NULL};

	    struct p_tab Pic_tab_dud={
                        1, /* normalized flag */
                        0, /* orientation flag, set to landscape */
			"default_dud", /* picture table name.		*/
			1,0.0,0.0,"default","default",/* file name	*/
			1,0.0,0.0,"default","default",/* function	*/
			1,0.0,0.0,"default","default",/* logical mask	*/
			1,0.0,0.0,"default","default",/* transforms	*/
			1,0.0,0.0,"default","default",/* source	*/
			1,0.0,0.0,"default","default",/* name		*/
			1,0.0,0.0,"default","default",/* title	*/
			1,0.0,0.0,"default","default",/* units	*/
			1,0.0,0.0,"default","default",/* crd		*/
			1,0.0,0.0,"default","default",/* crt		*/
			1,0.0,0.0,"default","default",/* com1		*/
			1,0.0,0.0,"default","default",/* com2		*/
			1,0.0,0.0,"default","default",/* com3		*/
			1,0.0,0.0,"default","default",/* com4		*/
			0,0.0,0.0,"default","defcenter",/* xn		*/
			0,0.0,0.0,"default","defcentup",/* yn		*/
			1,0.0,0.0,"default","default",/* zn		*/
			1,0.0,0.0,"default","default",/* tn		*/
			0,0.0,0.0,"default","default",/* xu		*/
			0,0.0,0.0,"default","defcentup",/* yu		*/
			1,0.0 ,0.0,"default","default",/* zu		*/
			1,0.0 ,0.0,"default","default",/* tu		*/

		1,0.0,0.0,"default","default","default",/* xv		*/
		1,0.0,0.0,"default","default","default",/* yv		*/
		1,0.0,0.0,"default","default","default",/* zv		*/
		1,0.0,0.0,"default","default","default",/* tv		*/
		1,0.0,0.0,"default","default","default",/* mean	*/
		1,0.0,0.0,"default","default","default",/* max	*/
		1,0.0,0.0,"default","default","default",/* min	*/

			1,0.0,0.0,"default",		/* xt1		*/
			1,0.0,0.0,"default",		/* xt2		*/
			1,0.0,0.0,"default",		/* xmta		*/
			1,0.0,0.0,"default",		/* xmtb		*/
			1,0.0,0.0,"default",		/* yt1		*/
			1,0.0,0.0,"default",		/* yt2		*/
			1,0.0,0.0,"default",		/* ymta		*/
			1,0.0,0.0,"default",		/* ymtb		*/

			0,0.0,"default","defcenter",	/* xl1		*/
			0,0.0,"default","defcenter",	/* xl2		*/
			0,0.0,"default","defright",	/* yl1		*/
			0,0.0,"default","default",	/* yl2		*/

			1,0.0,0.0,0.0,0.0,"default",/* box1		*/
			0,0.0,0.0,0.0,0.0,"default",/* box2		*/
			0,0.0,0.0,0.0,0.0,"default",/* box3		*/
			0,0.0,0.0,0.0,0.0,"default",/* box4		*/

			1,0.0,0.0,0.0,0.0,"default",/* line1	*/
			1,0.0,0.0,0.0,0.0,"default",/* line2	*/
			0,0.0,0.0,0.0,0.0,"default",/* line3	*/
			0,0.0,0.0,0.0,0.0,"default",/* line4	*/

			1,0.0,0.0,0.0,0.0,"default",
					      "defcenter",
					      "default",/* leg		*/
			1,0.05,0.26,0.95 ,0.86,-999.,		/* dsp		*/
			NULL	/* pointer to the next entry.		*/
			};
	    struct p_tab Pic_tab={
                        1, /* normalized flag */
                        0, /* orientation flag, set to landscape */
			"default", /* picture table name.		*/
			1,0.05,0.013,"default","default",/* file name	*/
			1,0.05,0.013,"default","default",/* function	*/
			1,0.05,0.033,"default","default",/*logical mask	*/
			1,0.05,0.053,"default","default",/* transforms	*/
			1,0.05,0.942,"default","default",/* source	*/
			1,0.05,0.923,"default","default",/* name	*/
			1,0.15,0.923,"default","default",/* title	*/
			1,0.67,0.923,"default","default",/* units	*/
			1,0.75,0.923,"default","default",/* crd		*/
			1,0.85,0.923,"default","default",/* crt		*/
			1,0.1 ,0.955,"default","default",/* com1	*/
			1,0.1 ,0.975,"default","default",/* com2	*/
			1,0.1 ,0.995,"default","default",/* com3	*/
			1,0.1 ,0.999,"default","default",/* com4	*/
			0,0.5 ,0.277,"default","defcenter",/* xn	*/
			0,0.0169,0.420034,"default","defcentup",/* yn	*/
			1,0.0 ,0.995,"default","default",/* zn		*/
			1,0.0 ,0.995,"default","default",/* tn		*/
			0,0.6 ,0.277,"default","default",/* xu		*/
			0,0.02,0.659,"default","defcentup",/* yu	*/
			1,0.0 ,0.995,"default","default",/* zu		*/
			1,0.0 ,0.995,"default","default",/* tu		*/

		1,0.8,0.942,"default","default","default",/* xv		*/
		1,0.8,0.923,"default","default","default",/* yv		*/
		1,0.8,0.903,"default","default","default",/* zv		*/
		1,0.8,0.883,"default","default","default",/* tv		*/
		1,0.05 ,0.90,"default","default","default",/* mean	*/
		1,0.25 ,0.90,"default","default","default",/* max	*/
		1,0.45 ,0.90,"default","default","default",/* min	*/

			1,0.26,0.247,"default",		/* xt1		*/
			1,0.86,0.872,"default",		/* xt2		*/
			0,0.26,.257,"default",		/* xmta		*/
			0,0.86,.863,"default",		/* xmtb		*/
			1,0.05,0.04,"default",		/* yt1		*/
			1,0.95 ,0.96,"default",		/* yt2		*/
			0,0.05,0.045,"default",		/* ymta		*/
			0,0.95,0.955,"default",		/* ymtb		*/

			1,0.235,"default","defcenter",	/* xl1		*/
			0,0.87 ,"default","defcenter",	/* xl2		*/
			1,0.04,"default","defright",	/* yl1		*/
			0,0.96 ,"default","default",	/* yl2		*/

			1,0.05,0.26 ,0.95,0.86,"default",/* box1	*/
			0,0.00,0.30,0.92,0.88,"default",/* box2		*/
			0,0.00,0.32,0.91,0.86,"default",/* box3	*/
			0,0.00,0.00,0.00,0.00,"default",/* box4		*/

			0,0.05,0.56,0.95,0.56,"default",/* line1	*/
			0,0.5 ,0.26,0.5 ,0.86,"default",/* line2	*/
			0,0.0 ,0.53 ,0.9 ,0.53 ,"default",/* line3	*/
			0,0.0 ,0.99 ,0.9 ,0.99 ,"default",/* line4	*/

			1,0.05,0.13,0.95,0.16,"default",
					      "defcenter",
					      "default",/* leg		*/
			1,0.05,0.26,0.95 ,0.86,-999,		/* dsp		*/
			&Pic_tab_dud	/* pointer to the next entry.		*/
			};

/*		Page orientation - portrait, landscape.			*/

    struct orientation Page={1,"landscape"};

/*		CGM file name.						*/

    char cgm_file[1024]="";

/*		META file name.						*/

    char meta_file[1024]="";
/*		META file output type.						*/

    char meta_type[5]="";

/*		Script INPUT file name.					*/

    char script_file[1024]="";

/*		Font INPUT file name.					*/

    char font_file[1024]="";

/*		Script OUTPUT file name.				*/

    char output_file[1024]="";

/*		Error file name.					*/

    char error_file[1024]="";

/*		Sun Raster file name.					*/

    char ras_file[1024]="";

/*              GIF Raster file name.                                   */

    char gif_file[1024]="";

/*              JPEG Raster file name.                                  */

    char jpeg_file[1024]="";
                       
/*              PNG Raster file name.                                   */
                                
    char png_file[1024]="";

/*		netCDF file name.					*/

    char netCDF_file[1024]="";

/*		HDF file name.					*/

    char HDF_file[1024]="";

/*		DRS file name.						*/

    char DRS_file[1024]="";

/*		Projection attributes.  Must be set by display software.*/

    struct projection_attr p_PRJ_mercator={
				"mercator", 	  /* Transverse Mercator Projection.	*/
				9,
				1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20, /*gctp param*/
				NULL
			       };	
    struct projection_attr p_PRJ_ortho={
				"orthographic", 	  /* Orthographic Projection.	*/
				14,
				1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20, /*gctp param*/
				&p_PRJ_mercator
			       };	
    struct projection_attr p_PRJ_lb={
				"lambert", 	  /* Lambert Projection.	*/
				4,
				1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20, /*gctp param*/
				&p_PRJ_ortho
			       };	
    struct projection_attr p_PRJ_p={
				"polar", 	  /* Polar Projection (Dean).	*/
				-3,
				1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20, /*gctp param*/
				&p_PRJ_lb
			       };	
    struct projection_attr p_PRJ_poly={
				"polyconic", 	  /* Polyconic Projection.	*/
				7,
				1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20, /*gctp param*/
				&p_PRJ_p
			       };	
    struct projection_attr p_PRJ_r={
				"robinson", 	  /* Robinson Projection (Dean).	*/
				-1,
				1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20, /*gctp param*/
				&p_PRJ_poly
			       };	
    struct projection_attr p_PRJ_m={
				"mollweide", 	  /* Mollweide Projection (Dean).	*/
				-2,
				1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20, /*gctp param*/
				&p_PRJ_r
			       };	
    struct projection_attr p_PRJ_li={
				"linear", 	  /* Linear Projection (Dean).	*/
				0,
				1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20, /*gctp param*/
				&p_PRJ_m
			       };	
    struct projection_attr p_PRJ_list={
				"default", 	  /* Active Projection.	*/
				0,
				1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20, /*gctp param*/
				&p_PRJ_li
			       };	
    struct project_attr p_PRJ={
				"linear", 0,	  /* Active Projection.	*/
				0.,0.,0.,0.,	  /* Projection Constants.*/
				0.,0.,0.,0.,	  /* Projection Constants.*/
				0,
				1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20, /*gctp param*/
				0.,0.,0.,0.,	  /* Projection Constants.*/
				NULL
			       };	
									
    char *base_dir;
    char dirbase[1024];

/*		Table of increments in i and j for isoline following.	*/

    int IN[14][2][2] = {
			0,0,0,1, 0,1,1,1, 0,0,1,1, 1,1,0,1, 0,0,0,1,
			0,1,0,1, 0,0,0,1, 0,1,0,0, 0,1,0,1, 0,1,0,0,
			0,1,1,1, 1,1,0,0, 1,1,0,1, 0,1,0,0
		       };
    int JN[14][2][2] = {
			0,1,0,0, 0,0,0,1, 0,1,0,1, 0,1,1,1, 0,1,1,1,
			0,0,1,1, 0,1,1,1, 1,1,0,1, 1,1,0,0, 0,0,0,1,
			1,1,0,1, 0,1,0,1, 0,1,0,0, 0,0,0,1
		       };

/*  Lists of array attribute member names				*/

/*			Character strings				*/

       char A_strg[38][12]={"File","Function","LogicalMask","Transform",
			    "Source","Name","Title","Units",
			    "Type","CrDate","CrTime",
			    "source","name","title","units","type",
			    "crdate","crtime",
			    "comment#1","comment#2","comment#3","comment#4",
			    "XName","YName","ZName","TName",
			    "XUnits","YUnits","ZUnits","TUnits",
			    "xname","yname","zname","tname",
			    "xunits","yunits","zunits","tunits"
			   };

/*			Integers					*/

	char  A_intg[5][NDS][8]={"XSize","YSize","ZSize","TSize",
				 "XKwrap","YKwrap","ZKwrap","TKwrap",
				 "xsize","ysize","zsize","tsize",
				 "xjump","yjump","zjump","tjump",
				 "xinterp","yinterp","zinterp","tinterp"
				};

/*			Scalar floating point				*/

	char A_sflt[5][NDS][8]={"XCycle","YCycle","ZCycle","TCycle",
				"XFirst","YFirst","ZFirst","TFirst",
				"XLast","YLast","ZLast","TLast",
				"xfirst","yfirst","zfirst","tfirst",
				"xlast","ylast","zlast","tlast"
			       };

/*			Vector floating point				*/

	char A_vflt[6][NDS][8]={"XValue","YValue","ZValue","TValue",
				"XBound","YBound","ZBound","TBound",
				"XWeight","YWeight","ZWeight","TWeight",
				"xvalue","yvalue","zvalue","tvalue",
				"xbound","ybound","zbound","tbound",
				"xweight","yweight","zweight","tweight"
			       };

/*			Integer adjustable indices			*/

	int I=1;
	int J=1;
	int K=1;
	int L=1;
	int M=1;
	int N=1;

/*		Loop pause value and Loop exit flag			*/
	int loop_pause;
	int loop_exit;

/*		Flag to indicate interaction off (=1) or on (=0).
		This flag is only used to determine whether to output
		changes to attributes and commands to the script file
		when the {-o filename} option is given on the command
		line.  When reading a script file its contents need not
		be added to the script file being saved.  It will instead
		contain a "Run(file)" command except for initial.attributes
		file.							*/
	int Inactive=1;

/*		Flag to indicate which method to use for changing the
		graphics canvas orientation in the function
		change_orientation() in procPage.c.  It is different when
		in pure batch mode (i.e. at the beginning when a -i file
		is used and a graphics canvas is open.			*/
	int Batch_mode=1;

/*		Flag to indicate whether the user wishes to defer the 
		VCS Canvas update for a later time. The Inactive resource
		flag must be set to TRUE to have an effect. This flag has
		no effect while running in batch mode.			*/
	int user_defer_update=0;

/*		Flag to bring up the interactive interface (GUI) or to exit
		VCS when the finished running the script.		*/
	int do_interactive = 1;


	extern void	interact();


/*			Integer last used segment number;		*/

	int segment_num=0;

/*		Update required indication.				*/

	int update_ind=0;

/*		Hints flag - true = print hints for each panel.		*/

	int hints_flg=1;

/*		Control Panel flag - true = power control panel.	*/

	int control_panel=0;

/*		Active colormap name.					*/

	char active_colors[17]={"default"};

/*		Lists of picture element names				*/

	char Pt_elem[22][12]={"File"  ,"Function"   ,"LogicalMask",
			     "Transform","source"   ,"name"   ,"title"  ,
			    "units"   ,"crdate" ,"crtime" ,
			    "comment#1","comment#2","comment#3","comment#4",
			    "xname"  ,"yname"  ,"zname"  ,"tname"  ,
			    "xunits"  ,"yunits"  ,"zunits"  , "tunits"
			   };
	char Pf_elem[7][10]= {"xvalue"  ,"yvalue"  ,"zvalue"  ,"tvalue"  ,
			    "mean","max" ,"min" , };
	char Pxt_elem[4][10]={"xtic#1" ,"xtic#2" ,"xmintic#a","xmintic#b"};
	char Pyt_elem[4][10]={"ytic#1" ,"ytic#2" ,"ymintic#a","ymintic#b"};
	char Pxl_elem[2][10]={"xlabel#1" ,"xlabel#2" };
	char Pyl_elem[2][10]={"ylabel#1" ,"ylabel#2" };
	char Pbx_elem[8][10]={"box#1"  ,"box#2"  ,"box#3"  ,"box#4"  ,
			     "line#1"  ,"line#2"  ,"line#3"  , "line#4"  };
	char Pleg_elem[10]  ={"legend" };
	char Pdsp_elem[10]  ={"data" };

/*		Lists of bundle names.					*/

	char P_text[5][3]  ={"p","x" ,"y" ,"Tt","To"};
	char P_fmt[6][3]   ={"p","x","y","Th","Tt","To"};
	char P_x_tic[4][3] ={"p","y1","y2","Tl"};
	char P_y_tic[4][3] ={"p","x1","x2","Tl"};
	char P_x_lab[4][3] ={"p","y" ,"Tt","To"};
	char P_y_lab[4][3] ={"p","x" ,"Tt","To"};
	char P_box[6][3]   ={"p","x1","y1","x2","y2","Tl"};
	char P_leg[8][3]   ={"p","x1","y1","x2","y2","Tt","To","Tl"};
	char P_dsp[5][3]   ={"p","x1","y1","x2","y2"};

/*		Default continents selection and attribute names.	*/

	char Dc_attr[2][16]={"select","line"};

/*		Lists of isoline display attribute names.		*/

	char Gi_attr[17][16]={"projection","xticlabels#1","xticlabels#2",
			      "xmtics#1","xmtics#2",
			      "yticlabels#1","yticlabels#2",
			      "ymtics#1","ymtics#2",
			      "datawc","xaxisconvert","yaxisconvert",
			      "make_labels","lines",
			      "datawc_tunits","datawc_calendar","idatawc"};

	char Gi_iso[13][16] ={"id","priority","level","increment","hilite_ci",
			     "label","Tl",
			     "Tt","To","clockwise","length","angle","spacing"};

/*		Lists of outline display attribute names.		*/

	char Go_attr[17][16]={"projection","xticlabels#1","xticlabels#2",
			      "xmtics#1","xmtics#2",
			      "yticlabels#1","yticlabels#2",
			      "ymtics#1","ymtics#2",
			      "datawc","xaxisconvert","yaxisconvert",
			      "Tl","outline",
			      "datawc_tunits","datawc_calendar","idatawc"};


/*		Lists of continents display attribute names.		*/

	char Gcon_attr[15][16]={"projection","xticlabels#1","xticlabels#2",
				"xmtics#1","xmtics#2",
				"yticlabels#1","yticlabels#2",
				"ymtics#1","ymtics#2",
				"datawc","Tl", "Type",
				"datawc_tunits","datawc_calendar","idatawc"};


/*		Lists of isofill display attribute names.		*/

	char Gfi_attr[18][16]={"projection","xticlabels#1","xticlabels#2",
			       "xmtics#1","xmtics#2",
			       "yticlabels#1","yticlabels#2",
			       "ymtics#1","ymtics#2","datawc",
			       "xaxisconvert","yaxisconvert","range",
			       "legend","missing",
			       "datawc_tunits","datawc_calendar","idatawc"};
	char Gfi_iso[4][16] ={"id","level1","level2","Tf"};
	char Gfm_iso[4][16] ={"id","level1","level2","Tf"};

/*		Lists of boxfill display attribute names.		*/

	char Gfb_attr[25][16]={"projection","xticlabels#1","xticlabels#2",
			       "xmtics#1","xmtics#2",
			       "yticlabels#1","yticlabels#2",
			       "ymtics#1","ymtics#2",
			       "datawc","xaxisconvert","yaxisconvert",
			       "level_1","level_2","color_1","color_2",
			       "boxfill_type", "legend",
			       "ext_1","ext_2","missing", 
			       "datawc_tunits","datawc_calendar","idatawc",
			       "range"};

/*		Lists of projection attribute names.		*/

	char Proj_attr[2][16]={"parameters","type"};

/*		Lists of meshfill display attribute names.		*/

	char Gfm_attr[20][16]={"projection","xticlabels#1","xticlabels#2",
			       "xmtics#1","xmtics#2",
			       "yticlabels#1","yticlabels#2",
			       "ymtics#1","ymtics#2","datawc",
			       "xaxisconvert","yaxisconvert","range","missing","wrap",
			       "mesh","legend",
			       "datawc_tunits","datawc_calendar","idatawc"};

/*		Lists of vector display attribute names.		*/

	char Gv_attr[20][16]={"projection","xticlabels#1","xticlabels#2",
			      "xmtics#1","xmtics#2",
			      "yticlabels#1","yticlabels#2",
			      "ymtics#1","ymtics#2",
			      "datawc","xaxisconvert","yaxisconvert",
			      "Tl","vector_scale",
			      "vector_align","vector_type","ref_vector",
			      "datawc_tunits","datawc_calendar","idatawc"};

/*		Lists of X(y) vs y display attribute names.		*/

	char GXy_attr[17][16]={"projection","xticlabels#1","xticlabels#2",
			       "xmtics#1","xmtics#2",
			       "yticlabels#1","yticlabels#2",
			       "ymtics#1","ymtics#2",
			       "datawc","xaxisconvert","yaxisconvert","Tl","Tm",
			       "datawc_tunits","datawc_calendar","idatawc"};


/*		Lists of Y(x) vs x display attribute names.		*/

	char GYx_attr[17][16]={"projection","xticlabels#1","xticlabels#2",
			       "xmtics#1","xmtics#2",
			       "yticlabels#1","yticlabels#2",
			       "ymtics#1","ymtics#2",
			       "datawc","xaxisconvert","yaxisconvert","Tl","Tm",
			       "datawc_tunits","datawc_calendar","idatawc"};

/*		Lists of X(t) vs Y(t) display attribute names.		*/

	char GXY_attr[17][16]={"projection","xticlabels#1","xticlabels#2",
			       "xmtics#1","xmtics#2",
			       "yticlabels#1","yticlabels#2",
			       "ymtics#1","ymtics#2",
			       "datawc","xaxisconvert","yaxisconvert","Tl","Tm",
			       "datawc_tunits","datawc_calendar","idatawc"};


/*		Lists of ScatterPlot display attribute names.		*/

	char GSp_attr[16][16]={"projection","xticlabels#1","xticlabels#2",
			       "xmtics#1","xmtics#2",
			       "yticlabels#1","yticlabels#2",
			       "ymtics#1","ymtics#2",
			       "datawc","xaxisconvert","yaxisconvert","Tm",
			       "datawc_tunits","datawc_calendar","idatawc"};

/*		Lists of fill outline display attribute names.		*/

	char Gfo_attr[17][16]={"projection","xticlabels#1","xticlabels#2",
			       "xmtics#1","xmtics#2",
			       "yticlabels#1","yticlabels#2",
			       "ymtics#1","ymtics#2",
			       "datawc","xaxisconvert","yaxisconvert",
			       "Tf","outline",
			       "datawc_tunits","datawc_calendar","idatawc"};

/* struct to save text attributes when setting so we can reuse it later when drawing lines */
        struct table_text *plot_pTt; 
        struct table_chorn *plot_pTo;
/* holder for meta data and x drawing */
char text_hpath,text_vpath,text_path;
float text_height,text_angle;
int text_color,text_font;


/*		Display types available.				*/

    struct displays d_type[NTYPES] =
	 {"Isoline"   ,1, 2,0,0,0,0,0,"A(x,y)","","","","","",
	  "Outline"   ,1, 2,0,0,0,0,0,"I(x,y)","","","","","",
	  "Continents",0, 0,0,0,0,0,0,"",      "","","","","",
	  "Isofill"   ,1, 2,0,0,0,0,0,"A(x,y)","","","","","",
	  "Outfill"   ,1, 2,0,0,0,0,0,"I(x,y)","","","","","",
	  "Boxfill"   ,1, 2,0,0,0,0,0,"A(x,y)","","","","","",
	  "Vector"    ,2, 2,2,0,0,0,0,"U(x,y)","V(x,y)","","","","",
	  "Xyvsy"     ,1, 1,0,0,0,0,0,"X(y)","","","","","",
	  "Yxvsx"     ,1, 1,0,0,0,0,0,"Y(x)","","","","","",
	  "XvsY"      ,2, 1,1,0,0,0,0,"X(t)","Y(t)","","","","",
	  "Scatter"   ,2, 4,4,0,0,0,0,"X(x,y,z,t)","Y(x,y,z,t)","","","","",
	  "Meshfill"  ,2, 1,3,0,0,0,0,"A(x)","M(x,2,n)","","","","",
	  "text"      ,0, 0,0,0,0,0,0,"",      "","","","","",
	  "fillarea"  ,0, 0,0,0,0,0,0,"",      "","","","","",
	  "marker"  ,0, 0,0,0,0,0,0,"",      "","","","","",
	  "line"  ,0, 0,0,0,0,0,0,"",      "","","","",""
	 };

/*		List of display command element names.			*/
/*		The six names below (a,b,c,d,e,f) are aliased by
		the six "A(x,y)",... strings in d_type above.		*/ 

	char Display_attr[13][17] ={"off","priority","continents","type",
			"template","_template_origin","graph","a","b","c","d","e","f"};

/*		List of projections.					*/

	char PRJ_names[PRJ_TYPES][17]={"linear","robinson","mollweide","polar"};

/*			Default error file				*/

	char errf[]="ERROR_GRAPHICS";

/*      GKS error file name                                             */
        char *ptre;

         FT_Library ft_library;    

    int 
    vcs_legacy_main()
	/*int argc;   No longer needed for Python interface
	char *argv[];*/
      {
/*		This program is a test for later development of script
		processing.						*/
        int argc = 0;
        char **argv = NULL;

	char *getenv();
	FILE *fopen(), fpcdat;
	int i,j,k;
	int got_font_or_geom = 0;
        float fv=1.0;
	char *ptri, *ptro;
	int mode=0777;
        char str[2], buf[1024];
	char logo_data2[388680];
	struct display_tab *dtab;
        extern struct table_line Tl_tab;
        extern struct table_mark Tm_tab;
        extern struct table_fill Tf_tab;
	extern struct table_FT_VCS_FONTS TTFFONTS;
	struct table_FT_VCS_FONTS  *current_font;
	
	int *pi;
        static int read_initial_attribute = 0;
	FILE *logo_file,*logo_file2;
	char c;

/*        f_init();
*/
/*			Additional initialization for the 'default' 	*/
/*			line, marker and fillarea secondary attributes	*/
        get_int_size_and_set_value( &Tl_tab.ltyp, 1, (int)1 );   Tl_tab.ltyp_size=1;
        get_float_size_and_set_value( &Tl_tab.lwsf, 1, &fv );    Tl_tab.lwsf_size=1.0;
        get_int_size_and_set_value( &Tl_tab.lci, 1, (int)1 );  Tl_tab.lci_size=1;
        get_int_size_and_set_value( &Tm_tab.mtyp, 1, (int)1 );   Tm_tab.mtyp_size=1;
        get_float_size_and_set_value(&Tm_tab.msize, 1, &fv);     Tm_tab.msize_size=1.0;
        get_int_size_and_set_value( &Tm_tab.mci, 1, (int)1 );  Tm_tab.mci_size=1;
        get_int_size_and_set_value( &Tf_tab.fais, 1, (int)1 );   Tf_tab.fais_size=1;
        get_int_size_and_set_value( &Tf_tab.fasi, 1, (int)1 );   Tf_tab.fasi_size=1;
        get_int_size_and_set_value( &Tf_tab.faci, 1, (int)1 ); Tf_tab.faci_size=1;
	/* Use extended name lengths at cddrs interface */
	cw_set_string_option(CW_EXTENDED);

/*			Find a base directory.				*/

	if ((base_dir=getenv(DOT_DIRECTORY_ENV)) == NULL)
	  {
	   if ((base_dir=getenv("HOME")) == NULL || strlen(base_dir) ==0) {
               strcpy(dirbase,"./");
               strcat(dirbase,DOT_DIRECTORY);
           }
	   else 
	     {
	      strcpy(dirbase,base_dir);
	      strcat(dirbase,"/");
	      strcat(dirbase,DOT_DIRECTORY);
	     }
	  }
	else  strcpy(dirbase,base_dir);
	base_dir=dirbase;
	
	if (mkdir(base_dir,mode) != 0 && errno != EEXIST)
	  {
	   printf ("Error - you don't have a base directory.\n");
	   printf ("The environment variable");
           printf (DOT_DIRECTORY_ENV);
	   printf (" or HOME needs to be set!\n");
	   return 0;
	  }

	/* First time around add user's home to path of fonts */
	if (read_initial_attribute==0)
	  {
	    /* logo stuff */
	    logo = cairo_image_surface_create_for_data(&logo_data[0],logo_format,logo_width,logo_height,logo_stride);
	    logo_p = cairo_pattern_create_for_surface(logo);
	    i = FT_Init_FreeType( &ft_library );
	    if (i!=0)
	      {
		err_warn(1,fperr,"Error - could not load the FT library!\n");
		return 0;
	      }
	    current_font = &TTFFONTS;
	    while (current_font!=NULL)
	      {
		strcpy(font_file,base_dir);
		strcat(font_file,"/");
		strcat(font_file,current_font->path);
		strcpy(current_font->path,font_file);
		/* add code here to check it actually exists ? */
		current_font=current_font->next;
	      }
	  }
	strcpy(script_file,base_dir);
	strcat(script_file,"/");
	strcat(script_file,"initial.attributes");

	fpin=fpout=fperr=NULL;
	ptri=ptro=ptre=NULL;

	if (argc >= 2)
	  {
	   for (i=1; i < argc; i++)
	     {
	      if (strncmp (argv[i],"-e",2) == 0)
	        {
		 if (strlen(argv[i]) > (size_t)2) j=2;
		 else if (++i < argc) j=0;
		 else return -1;
		 ptre=argv[i];
		 ptre+=j;
		 if ((strchr(ptre,'/')==NULL) && base_dir != NULL)
		   {
		    strcpy(error_file,base_dir);
		    strcat(error_file,"/");
		    strcat(error_file,ptre);
		   }
		 else strcpy(error_file,ptre);
		 ptre=error_file;
		}
	     }
	  }

	if (ptre != NULL && (fperr=fopen(ptre,"w")) == NULL)
	  {
	   printf ("****Error opening error message file:  %s \n",ptre);
	   return -1;
	  }
	else if (ptre == NULL)
	  {
	   if (base_dir != NULL)
	     {
	      strcpy(error_file,base_dir);
	      strcat(error_file,"/");
	      strcat(error_file,errf);
	     }
	   else
	     strcpy(error_file,errf);

	   ptre=error_file;
	   if ((fperr=fopen(ptre,"w"))==NULL)
	     {
	      printf ("****Error opening default error message file:  %s\n",
					ptre);
	      return -1;
	     }
	  }

	/* First of all initialize the FT library */
/*			open GKS 					*/
        gopks(fperr,0);

/*		Read the initial attributes script if is exists.	*/

	if (access(script_file,R_OK) < 0)
	  {
	   printf("Warning - no initial.attributes file.\n");
	  }
	else
	  {
	   if (fperr == NULL) fperr=stdout;
           /* For normalization purposes, only read in the initial attribute one time, since 
            * there may be templates that are already normalized...
            */
           if (read_initial_attribute == 0) k=rscript();
           read_initial_attribute = 1;
	   if (k == 0) err_warn(1,fperr,
		"Error - from read of initial attributes: %d\n",k);
	   if (fpin != NULL)
	      fclose(fpin);
	  }

	script_file[0]='\0';

	fpin=fpout=NULL;
	ptri=ptro=ptre=NULL;

	if (argc >= 2)
	  {
	   for (i=1; i < argc; i++)
	     {
	      if (strncmp (argv[i],"-i",2) == 0)
	        {
                 do_interactive = 1;
		 if (strlen(argv[i]) > (size_t)2) j=2;
		 else if (++i < argc) j=0;
		 else return -1;
		 ptri=argv[i];
		 ptri+=j;
		 if (strncmp("./",ptri,2) == 0) ptri+=2;
		 if (strchr(ptri,'/')==NULL)
		   {
		    get_a_path(script_file);
		    strcat(script_file,"/");
		    strcat(script_file,ptri);
		   }
		 else strcpy(script_file,ptri);
		 ptri=script_file;
		}
	      else if (strncmp (argv[i],"-ni",3) == 0)
	        {
                 do_interactive = 0;
		 if (strlen(argv[i]) > (size_t)3) j=3;
		 else if (++i < argc) j=0;
		 else return -1;
		 ptri=argv[i];
		 ptri+=j;
		 if (strncmp("./",ptri,3) == 0) ptri+=3;
		 if (strchr(ptri,'/')==NULL)
		   {
		    get_a_path(script_file);
		    strcat(script_file,"/");
		    strcat(script_file,ptri);
		   }
		 else strcpy(script_file,ptri);
		 ptri=script_file;
		}
	      else if (strncmp (argv[i],"-o",2) == 0)
	        {
                 do_interactive = 1;
		 if (strlen(argv[i]) > (size_t)2) j=2;
		 else if (++i < argc) j=0;
		 else return -1;
		 ptro=argv[i];
		 ptro+=j;
		 if ((strchr(ptro,'/')==NULL) && base_dir != NULL)
		   {
		    strcpy(output_file,base_dir);
		    strcat(output_file,"/");
		    strcat(output_file,ptro);
		   }
		 else strcpy(output_file,ptro);

/*			Put a ".scr" extension on the name if needed.	*/

		 if ( (j=strlen(ptro)) < 4 || strcmp(".scr",&ptro[j-4]) != 0)
		   {
		    strcat(output_file,".scr");
		   }
		 ptro=output_file;
		}
	      else if (strncmp (argv[i],"-e",2) == 0)
	        {
                 do_interactive = 1;
		 printf ("Error file is:  %s.\n",argv[i]);
		}
	      else if (strncmp (argv[i],"-geom",5) == 0)
	        {
                 do_interactive = 1;
	         got_font_or_geom = 1;
		}
	      else if (strncmp (argv[i],"-quiet",6) == 0)
	        {
                 do_interactive = 1;
		}
	      else if (strncmp (argv[i],"-font",5) == 0)
	        {
                 do_interactive = 1;
	         got_font_or_geom = 1;
		}
	      else
		{
	         if (got_font_or_geom == 0)
		    printf ("Argument %s not used.\n",argv[i]);
	         got_font_or_geom = 1;
		}
	     };
	  };

	if (ptro != NULL && (fpout=fopen(ptro,"w")) == NULL)
	  {
/*   printf("script_file1 = %s\n", script_file);*/
	   printf ("****Error opening script output file:  %s \n",ptro);
	   return -1;
	  }
	if (fpout != NULL) fprintf(fpout,"canvas(open)\n");

/*			Open Workstation Independent Segment Storage.	*/

	gopwk(7,NULL,"WISS");

	gacwk(7);

	gsclip(GNOCLIP);


/*		If there is an input file defined,
		read and process the script.				*/

	strcpy(buf, script_file);
	if ((ptri != NULL) && (k=run_script() == 0)) {
/*   printf("script_file2 = %s\n", script_file);*/
	   printf ("****Error opening script output file:  %s \n",buf);
	} 

	return 0;
      }
