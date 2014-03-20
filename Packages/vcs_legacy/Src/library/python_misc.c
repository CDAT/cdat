/************************************************************************
 *             PCMDI's Climate Data Analysis Tool - (CDAT)              *
 *              Command Line User Interface Popup Window                *
 *                         Developed at LLNL                            *
 *                                                                      *
 *                                                                      *
 *      Copyright (C) 1997. The Regents of the University of California.*
 *      All rights reserved.                                            *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *      Author: Dean N. Williams                 			*
 *                                                                      *
 *      Date: 03/18/97                                                  *
 *                                                                      *
 *      File name: python_misc.c                                        *
 *                                                                      *
 *                                                                      *
 *      Langague: ANSI C                                                *
 *                                                                      *
 *                                                                      *
 *      Comments:                                                       *
 *               This software uses ANSI C and X/Motif to provide the   *
 *               CDAT user with miscellaneous function CDAT/VCS         *
 *               routines.                                              *
 *                                                                      *
 *                                                                      *
 *      Modifications:                                                  *
 *                                                                      *
 *                                                                      *
 *      Contact:                                                        *
 *                                                                      *
 *              Dean N. Williams                    			*
 *                                                                      *
 *              LLNL                                                    *
 *              PO Box 808, L-264                                       *
 *              Livermore, CA                                           *
 *              94550                                                   *
 *                                                                      *
 *              (510) 423-0145                      			*
 *                                                                      *
 *                                                                      *
 ************************************************************************
 */
/****************************************************************************
*** Includes ****************************************************************
*****************************************************************************/
#include "Python.h"
#include "gks.h"
#include "gksshort.h"
#ifdef USEX11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
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
#include "my_access_mode.h"
#include "vcs_legacy_canvas.h"

#define  MAX_PATH_LEN 1028
/****************************************************************************
*** Prototypes **************************************************************
*****************************************************************************/
 
struct name_list {
        char                  name[MAX_PATH_LEN]; /* Name of primative */
        struct name_list      *next;
};
typedef struct name_list      NAMELIST;
typedef NAMELIST              *NAMELIST_LINK;

/****************************************************************************
*** Global Variables and Procedures *****************************************
*****************************************************************************/
int start_stop_loop;

/****************************************************************************
*** External Global Variables ***********************************************
*****************************************************************************/
#ifdef USEX11
extern Widget   w_out;  /* Motif widget for the CDAT Output window */
#endif
extern struct workstations Wkst[];
extern Gconid_X_drawable connect_id;
extern struct orientation Page;
extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */



/* C. Doutriaux 12-26-2012
This manipulates the templates ratio
*/

/* void templateRatio(char *name) { */
/*   PyObject *pyTemplate; */
/*   PyObject *canvas; */
/*   PyObject *res; */
/*   printf("Ok we ARE actually calling that thing\n"); */
/*   canvas = getPyCanvas(1); /\* borrowed ref *\/ */
/*   if (PyErr_Occurred()) { */
/*     PyErr_Print(); */
/*     printf("Error getting canvas... doh....\n"); */
/*   } */
/*   pyTemplate = PyObject_CallMethod(canvas,"gettemplate","s",name); /\* New ref *\/ */
/*   if (PyErr_Occurred()) { */
/*     PyErr_Print(); */
/*     printf("Error getting template %s... doh....\n",name); */
/*   } */
/*   res = PyObject_CallMethod(pyTemplate,"ratio","d",.25); */
/*   if (PyErr_Occurred()) { */
/*     PyErr_Print(); */
/*     printf("Error during template ratio %f... doh....\n",.25); */
/*   } */
/*   printf("Looks like we did ok\n"); */
/*   Py_XDECREF(pyTemplate); */
/*   Py_XDECREF(res); */

/*   return 0; */
/* } */

void duplicateTemplate(char *src,char *dst){
  PyObject *pyTemplate;
  PyObject *canvas;
  PyObject *res;
  printf("Ok we ARE actually calling that thing\n");
  canvas = getPyCanvas(1); /* borrowed ref */
  if (PyErr_Occurred()) {
    PyErr_Print();
    printf("Error getting canvas... doh....\n");
  }
  pyTemplate = PyObject_CallMethod(canvas,"createtemplate","ss",src,dst); /* New ref */
  if (PyErr_Occurred()) {
    PyErr_Print();
    printf("Error creating template %s... doh....\n",src);
  }
  Py_XDECREF(pyTemplate);
}

void deleteTemplate(char *name){
  PyObject *pyTemplate;
  PyObject *canvas;
  PyObject *res;
  printf("Ok we ARE actually calling that thing\n");
  canvas = getPyCanvas(1); /* borrowed ref */
  if (PyErr_Occurred()) {
    PyErr_Print();
    printf("Error getting canvas... doh....\n");
  }
  pyTemplate = PyObject_CallMethod(canvas,"removeP","s",name); /* New ref */
  if (PyErr_Occurred()) {
    PyErr_Print();
    printf("Error deleting template %s... doh....\n",name);
  }
  Py_XDECREF(pyTemplate);
}



/*	Shutdown the VCS xgks workstations and close xgks and output
 *	files.
 */
int
python_close_canvas(void)
{
/*                      Shutdown all GKS workstations that existed
                        and GKS itself.                                 */

	/* Shutdown the VCS Canvas, WISS, and CGM output
         * workstations
	 */
        /*printf("If this is called, then an error has occured.\n");*/
        shutdown(connect_id,2);
        shutdown(connect_id,1);
        shutdown(connect_id,7);

	/* close gks */
        gclks();
        if (fpout != NULL) fclose(fpout);
        if (fperr != NULL) fclose(fperr);

	return 1;
}

/*      Process the reassignment of the active colormap.  A python 
 *	COLOR command.  The string defining the command must be in 
 *	str[]. The colormap name must be less than 16 characters.
 */
char *
python_colormap(char *str)
{
	struct color_table *ptab;
	int c, ier=1;
        extern struct color_table C_tab;
        extern int setXcolormap();
        extern int set_active_colors();
	extern int vcs_legacy_canvas_update();
        extern Gconid_X_drawable 	connect_id;
#ifdef USEX11
	extern Visual                   *visual;
#endif
	extern char active_colors[17];

	/* Set the colormap name */
	strncpy(active_colors,str,17); active_colors[16] = '\0';

	/* Get the colormap structure */
        strncpy(active_colors,str,17);
        active_colors[16] = '\0';
        for (ptab=&C_tab;ptab != NULL &&
            (c=strcmp(ptab->name,active_colors))!=0;
            ptab=ptab->next);

	/* Set the new colormap */
        if (ptab == NULL)
           return  ("Error - The specified colormap was not found.");
        else if (ptab != NULL) {
           set_active_colors(); /* set new color map */
#ifdef USEX11
           if (connect_id.display != NULL) {
              if (visual->class == PseudoColor) /* change the color on the screen now! */
                 ier = setXcolormap(ptab); /* set colormap for the display */
	     /* Update the VCS Canvas with the new colormap. */
	     if (ier == 1) vcs_legacy_canvas_update(0);

	     if (ier == 1)
	        return NULL;
	     else 
                return  ("Error - The specified colormap was not found.");
           } else
             return NULL;
#elif defined (QTWM)
          /* Nothing to do */
#else
	   fprintf(stderr,"pymisc colormapchange code here?\n");
#endif
	}

        return NULL;
}

/*
 * Set a individual color cell in the active colormap. 
 * If default is the active colormap, then return an 
 * error string.
 */
char *
python_setcolorcell(int cell, int R, int G, int B)
{
	int				i,c,*pid;
	char 				buf[100];
	Gcobundl 			xgkscolor;
        Gintlist 			wsid;
	struct color_table 		*ptab;
        extern struct color_table 	C_tab;
#ifdef X11OUT
        extern Colormap 		n_cmap;
        extern Visual 			*visual;
        XColor xc;
#endif
        extern Gconid_X_drawable 	connect_id;
	extern char 			active_colors[17];
	extern int 			vcs_legacy_canvas_update();


	if (cmpncs(active_colors, "default") == 0) {
          return  ("Error - Cannot modify the default colormap color cell.");
	} else {
#ifdef X11OUT
           xc.pixel=cell;
	   xc.pad  =0;
           xc.red  =(int)(R*2.550+0.5)<<8;
           xc.green=(int)(G*2.550+0.5)<<8;
           xc.blue =(int)(B*2.550+0.5)<<8;
           xc.flags=DoRed|DoGreen|DoBlue;
           if (visual->class == PseudoColor) { /* change the color on the screen now! */
              xgkscolor.red=xc.red/100.;
              xgkscolor.green=xc.green/100.;
              xgkscolor.blue=xc.blue/100.;
           } else {
              xgkscolor.red=R/100.;
              xgkscolor.green=G/100.;
              xgkscolor.blue=B/100.;
	   }
#endif
	   /* Set the colormap in memory */
           for (ptab=&C_tab;ptab != NULL &&
               (c=strcmp(ptab->name,active_colors))!=0;
               ptab=ptab->next);
	   if (ptab == NULL) {
              return  ("Error - Colormap was not set!");
	   }
	   ptab->cval[cell].red = R;
	   ptab->cval[cell].green = G;
	   ptab->cval[cell].blue = B;

	   gqopwk(&wsid);
    	   for (i=0,pid=wsid.integers;i<wsid.number;pid++,i++)
              gscr(*pid,cell,&xgkscolor);
           if (wsid.number > 0 && wsid.integers != NULL)
    	       free((char *)wsid.integers);
#ifdef X11OUT
           if (connect_id.display != NULL)
              if (visual->class == PseudoColor) /* change the color on the screen now! */
	         XStoreColor (connect_id.display,n_cmap,&xc);
              else /* Update the VCS Canvas with the new colormap. */
	         vcs_legacy_canvas_update(0);
#endif
	}

	return NULL;
}

/*
 *    	Python cgm command to VCS.  
 */
int
python_cgm(str, app, connect_id, dptr)
char str[];
int app;
Gconid_X_drawable connect_id;
display_name_list *dptr;
{
	int	ier;
	extern int cgmeta();
	extern int trimbl();

	trimbl(str,256);
	ier = cgmeta(str,app, connect_id, dptr); /* Append or replace cgm file */

	return ier;

}

/*
 *    	Python raster command to VCS.  
 */
int
python_raster(char *str, int app)
{
        int     ier;
        extern int raster_dump();
        extern int trimbl();

        trimbl(str,256);
        ier = raster_dump(str,app); /* Append or replace raster file */

        return ier;

}

/*
 *      Python GIF command to VCS.
 */
int
python_gif(char *str, int app)
{
        int     ier;
        extern int raster_dump();
        extern int trimbl();

        trimbl(str,256);
        ier = gif_dump(str,app); /* Replace raster file */

        return ier;

}


/*
 *      Python primary or secondary list command.
 */
int
python_list_element(char *str)
{
        int     ier=0;
	int	print_template_list();
	int	print_data_list();
	int	print_boxfill_list();
	int	print_continents_list();
	int	print_isofill_list();
	int	print_isoline_list();
	int	print_outfill_list();
	int	print_outline_list();
	int	print_scatter_list();
	int	print_vector_list();
	int	print_xvsy_list();
	int	print_xyvsy_list();
	int	print_yxvsx_list();
	int	print_colormap_list();
	int	print_fillarea_list();
	int	print_format_list();
	int	print_line_list();
	int	print_list_list();
	int	print_marker_list();
	int	print_text_list();
	int	print_texto_list();
	int	print_plot_list();
	int	print_meshfill_list();

	if (cmpncs(str, "Template") == 0) {
	   print_template_list();
	} else if (cmpncs(str, "Data") == 0) {
	   print_data_list();
	} else if (cmpncs(str, "Boxfill") == 0) {
           print_boxfill_list();
	} else if (cmpncs(str, "Continents") == 0) {
 	   print_continents_list();
	} else if (cmpncs(str, "Isofill") == 0) {
           print_isofill_list();
	} else if (cmpncs(str, "Isoline") == 0) {
           print_isoline_list();
	} else if (cmpncs(str, "Outfill") == 0) {
           print_outfill_list();
	} else if (cmpncs(str, "Outline") == 0) {
           print_outline_list();
	} else if (cmpncs(str, "Scatter") == 0) {
	   print_scatter_list();
	} else if (cmpncs(str, "Vector") == 0) {
	   print_vector_list();
	} else if (cmpncs(str, "XvsY") == 0) {
           print_xvsy_list();
	} else if (cmpncs(str, "Xyvsy") == 0) {
           print_xyvsy_list();
	} else if (cmpncs(str, "Yxvsx") == 0) {
           print_yxvsx_list();
	} else if (cmpncs(str, "Colormap") == 0) {
           print_colormap_list();
	} else if (cmpncs(str, "Fillarea") == 0) {
           print_fillarea_list();
	} else if (cmpncs(str, "Format") == 0) {
           print_format_list();
	} else if (cmpncs(str, "Line") == 0) {
           print_line_list();
	} else if (cmpncs(str, "List") == 0) {
           print_list_list();
	} else if (cmpncs(str, "Marker") == 0) {
           print_marker_list();
	} else if (cmpncs(str, "Text") == 0) {
           print_text_list();
           print_texto_list();
	} else if (cmpncs(str, "TextTable") == 0) {
           print_text_list();
	} else if (cmpncs(str, "TextOrientation") == 0) {
           print_texto_list();
	} else if (cmpncs(str, "plot") == 0) {
           print_plot_list();
	} else if (cmpncs(str, "meshfill") == 0) {
           print_meshfill_list();
	} else if (cmpncs(str, "projection") == 0) {
           print_projection_list();
	} else {
	   PySys_WriteStdout("Error - Element is not listed in VCS!\n");
	}

        return ier;

}

NAMELIST_LINK free_list_of_names(NAMELIST_LINK * hnptr)
{
        NAMELIST_LINK   nptr, tnptr;

        tnptr = *hnptr;
        while (tnptr != NULL) {
             nptr = tnptr;
             tnptr = tnptr->next;
             free((char *) nptr);
        }

        *hnptr = NULL;

        return * hnptr;

}

int print_template_list(void)
{
	int 			i, index_flg=1;
	char  			buf[1024];
        struct p_tab    	*ptab;
        extern struct p_tab 	Pic_tab;
        NAMELIST_LINK   	nptr, tnptr, pnptr, hnptr=NULL;

        /* Set up the name list in alphabetical order */
        for(ptab=&Pic_tab;ptab != NULL; ptab=ptab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, ptab->name);
           nptr->next = NULL;

           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("***********************Template Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) {
	   if (index_flg) {
	      sprintf(buf, "(%4d):   ", i);
	      PySys_WriteStdout(buf);
	      index_flg = 0;
	   }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
	   if ((i%3) == 0) {
	      PySys_WriteStdout("\n");
              index_flg = 1;
	   }
	}
        PySys_WriteStdout("\n***********************End Template Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_data_list(void)
{
        int                     i, j, data_dim_num, index_flg=1;
        char                    buf[MAX_PATH_LEN], buf2[1024];
        extern struct a_tab     A_tab;
        struct a_tab            *ptab;


	PySys_WriteStdout("***********************Data Names List*****************************\n");
        for (j=1,ptab=&A_tab; ptab != NULL; j++, ptab=ptab->next) {
            if ((ptab->name[0] != '\0') && (ptab->pA_attr != NULL)) {
               /* Get the number of dimensions */
               data_dim_num = ptab->pA_attr->ND;
               for (i = 0; i < ptab->pA_attr->ND; ++i)
                  if (*ptab->pA_attr->xs[i] == 1)
                     --data_dim_num;
               sprintf(buf, "%s (%dD)",ptab->name, data_dim_num);
	       if (ptab->pA_attr->af != NULL)
                  strcat(buf,"c");
               if (index_flg) {
	 	  sprintf(buf2,"(%4d):   ", j);
                  PySys_WriteStdout(buf2);
                  index_flg = 0;
               }
               sprintf(buf2, "%17s   ", buf);
               PySys_WriteStdout(buf2);
               if ((j%3) == 0) {
                 PySys_WriteStdout("\n");
                 index_flg = 1;
               }
            }
        }
	PySys_WriteStdout("\n***********************End Data Names List*************************\n");
        return 1;
}

int print_boxfill_list(void)
{
	int			i, index_flg=1;
	char			buf[1024];
        struct gfb_tab          *gfbtab;
	extern struct gfb_tab   Gfb_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
        /* Set up the name list in alphabetical order */
        for(gfbtab=&Gfb_tab;gfbtab != NULL; gfbtab=gfbtab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, gfbtab->name);
           nptr->next = NULL;

           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("***********************Boxfill Names List**************************\n");
	for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) {
	   if (index_flg) {
              sprintf(buf,"(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
	}
        PySys_WriteStdout("\n***********************End Boxfill Names List**********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_continents_list(void)
{
	int 			i, index_flg=1;
	char			buf[1024];
        struct gcon_tab         *gcontab;
        extern struct gcon_tab  Gcon_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(gcontab=&Gcon_tab;gcontab != NULL; gcontab=gcontab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, gcontab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("***********************Continents Names List***********************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) {
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
	}

	PySys_WriteStdout("\n***********************End Continents Names List*******************\n");

        hnptr = free_list_of_names( &hnptr );                                   

        return 1;
}

int print_isofill_list(void)
{
        int                     i, index_flg=1;
	char			buf[1024];
        struct gfi_tab          *gfitab;
        extern struct gfi_tab   Gfi_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(gfitab=&Gfi_tab;gfitab != NULL; gfitab=gfitab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, gfitab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("************************Isofill Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) {
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
	}
	PySys_WriteStdout("\n************************End Isofill Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );                                   

        return 1;
}

int print_meshfill_list(void)
{
        int                     i, index_flg=1;
	char			buf[1024];
        struct gfm_tab          *gfmtab;
        extern struct gfm_tab   Gfm_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(gfmtab=&Gfm_tab;gfmtab != NULL; gfmtab=gfmtab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, gfmtab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("************************Meshfill Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) {
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
	}
	PySys_WriteStdout("\n************************End Meshfill Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}
int print_projection_list(void)
{
        int                     i, index_flg=1;
	char			buf[1024];
        struct projection_attr          *pj;
        extern struct projection_attr   p_PRJ_list;

	PySys_WriteStdout("************************Projection Names List*************************\n");
        for(i=1, pj=&p_PRJ_list;pj != NULL; i++, pj=pj->next) {
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",pj->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
	}
	PySys_WriteStdout("\n************************End Projection Names List*********************\n");
        return 1;
}

int print_isoline_list(void)
{
	int 			i, index_flg=1;
	char			buf[1024];
        struct gi_tab           *gitab;
        extern struct gi_tab    Gi_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(gitab=&Gi_tab;gitab != NULL; gitab=gitab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, gitab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("************************Isoline Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) { 
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n************************End Isoline Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );                                   

        return 1;
}

int print_outfill_list(void)
{
	int 			i, index_flg=1;
	char			buf[1024];
        struct gfo_tab          *gfotab;
        extern struct gfo_tab   Gfo_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(gfotab=&Gfo_tab;gfotab != NULL; gfotab=gfotab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, gfotab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("************************Outfill Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) {
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n************************End Outfill Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_outline_list(void)
{
	int 			i, index_flg=1;
	char			buf[1024];
        struct go_tab           *gotab;
        extern struct go_tab    Go_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(gotab=&Go_tab;gotab != NULL; gotab=gotab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, gotab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("************************Outline Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) { 
           if (index_flg) {
              sprintf(buf,"(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n************************End Outline Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_scatter_list(void)
{
	int 			i, index_flg=1;
	char			buf[1024];
        struct gSp_tab          *gSptab;
        extern struct gSp_tab   GSp_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(gSptab=&GSp_tab;gSptab != NULL; gSptab=gSptab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, gSptab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("************************Scatter Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) {
           if (index_flg) {
              sprintf(buf,"(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n************************End Scatter Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_vector_list(void)
{
	int 			i, index_flg=1;
	char			buf[1024];
        struct gv_tab           *gvtab;
        extern struct gv_tab    Gv_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;

        /* Set up the name list in alphabetical order */
        for(gvtab=&Gv_tab;gvtab != NULL; gvtab=gvtab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, gvtab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("************************Vector Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) { 
           if (index_flg) {
              sprintf(buf,"(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n************************End Vector Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );  

        return 1;
}

int print_xvsy_list(void)
{
	int 			i, index_flg=1;
	char			buf[1024];
        struct gXY_tab          *gXYtab;
        extern struct gXY_tab   GXY_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(gXYtab=&GXY_tab;gXYtab != NULL; gXYtab=gXYtab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, gXYtab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("**************************XvsY Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) {
           if (index_flg) {
              sprintf(buf,"(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n**************************End XvsY Names List*********************\n");

        hnptr = free_list_of_names( &hnptr ); 

        return 1;
}

int print_xyvsy_list(void)
{
	int 			i, index_flg=1;
	char			buf[1024];
        struct gXy_tab          *gXytab;
        extern struct gXy_tab   GXy_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(gXytab=&GXy_tab;gXytab != NULL; gXytab=gXytab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, gXytab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("************************Xyvsy Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) {
           if (index_flg) {
              sprintf(buf,"(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }

	PySys_WriteStdout("\n************************End Xyvsy Names List*********************\n");

        hnptr = free_list_of_names( &hnptr ); 

        return 1;
}

int print_yxvsx_list(void)
{
	int 			i, index_flg=1;
	char			buf[1024];
        struct gYx_tab          *gYxtab;
        extern struct gYx_tab   GYx_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(gYxtab=&GYx_tab;gYxtab != NULL; gYxtab=gYxtab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, gYxtab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("************************Yxvsx Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) {
           if (index_flg) {
              sprintf(buf,"(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n************************End Yxvsx Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_colormap_list(void)
{
	int 				i, index_flg=1;
	char				buf[1024];
        struct color_table 		*ptab;
        extern struct color_table 	C_tab;
        NAMELIST_LINK           	nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(ptab=&C_tab;ptab != NULL; ptab=ptab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, ptab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("***********************Colormap Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) { 
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n***********************End Colormap Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_fillarea_list(void)
{
	int 				i, index_flg=1;
	char				buf[1024];
        struct table_fill               *ptab;
        extern struct table_fill        Tf_tab;
        NAMELIST_LINK           	nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(ptab=&Tf_tab;ptab != NULL; ptab=ptab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, ptab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("***********************Fillarea Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) { 
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n***********************End Fillarea Names List*********************\n");

        hnptr = free_list_of_names( &hnptr ); 

        return 1;
}

int print_format_list(void)
{
	int 				i, index_flg=1;
	char				buf[1024];
        struct table_form               *thtab;
        extern struct table_form        Th_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(thtab=&Th_tab;thtab != NULL; thtab=thtab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, thtab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("*************************Format Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) { 
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n*************************End Format Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_line_list(void)
{
	int 				i, index_flg=1;
	char				buf[1024];
        struct table_line               *tltab;
        extern struct table_line        Tl_tab;
        NAMELIST_LINK           	nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(tltab=&Tl_tab;tltab != NULL; tltab=tltab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, tltab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("***************************Line Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) { 
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n***************************End Line Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_list_list(void)
{
	int 			i, index_flg=1;
	char			buf[1024];
        struct l_tab    	*ptab;
        extern struct 		l_tab L_tab[2];
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(ptab=&L_tab[0];ptab != NULL; ptab=ptab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, ptab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("***************************List Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) { 
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n***************************End List Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_marker_list(void)
{
	int 				i, index_flg=1;
	char				buf[1024];
        struct table_mark               *tmtab;
        extern struct table_mark        Tm_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(tmtab=&Tm_tab;tmtab != NULL; tmtab=tmtab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, tmtab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("*************************Marker Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) { 
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n*************************End Marker Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_text_list(void)
{
	int 				i, index_flg=1;
	char				buf[1024];
        struct table_text    		*ptab;
        extern struct table_text        Tt_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(ptab=&Tt_tab;ptab != NULL; ptab=ptab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, ptab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("***************************Text Names List*************************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) { 
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n***************************End Text Names List*********************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_texto_list(void)
{
	int 				i, index_flg=1;
	char				buf[1024];
        struct table_chorn              *totab;
        extern struct table_chorn       To_tab;
        NAMELIST_LINK           nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(totab=&To_tab;totab != NULL; totab=totab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, totab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

	PySys_WriteStdout("***********************Text Orientation Names List*****************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) { 
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
	PySys_WriteStdout("\n***********************End Text Orientation Names List*************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int print_plot_list(void)
{
        int                             i, index_flg=1;
        char                            buf[1024];
        struct display_tab              *dtab;
        extern struct display_tab       D_tab;
        NAMELIST_LINK           	nptr, tnptr, pnptr, hnptr=NULL;
 
        /* Set up the name list in alphabetical order */
        for(dtab=&D_tab;dtab != NULL; dtab=dtab->next) {
           if ((nptr=(NAMELIST_LINK)malloc(sizeof(NAMELIST))) == NULL) {
            err_warn(1,fperr,"Error - Cannot Store any more names in memory!\n");
            return 0;
           }
           strcpy(nptr->name, dtab->name);
           nptr->next = NULL;
 
           if (hnptr == NULL)
              hnptr = nptr;
           else{
              tnptr = pnptr = hnptr;
              while (tnptr != NULL) {
                 if (strcmp(nptr->name, tnptr->name) < 0) {
                   if (tnptr == hnptr) {
                      nptr->next = hnptr;
                      hnptr = nptr;
                      break;
                   } else {
                      nptr->next = tnptr;
                      pnptr->next = nptr;
                      break;
                   }
                 } else
                    pnptr = tnptr;
                    tnptr = tnptr->next;
              }
              if (tnptr == NULL) pnptr->next = nptr;
           }
        }

        PySys_WriteStdout("***********************Display Plot Names List*****************\n");
        for(i=1, tnptr = hnptr; tnptr != NULL; i++, tnptr=tnptr->next) { 
           if (index_flg) {
              sprintf(buf, "(%4d):   ", i);
              PySys_WriteStdout(buf);
              index_flg = 0;
           }
           sprintf(buf,"%17s   ",tnptr->name);
           PySys_WriteStdout(buf);
           if ((i%3) == 0) {
              PySys_WriteStdout("\n");
              index_flg = 1;
           }
        }
        PySys_WriteStdout("\n***********************End Display Plot Names List*************\n");

        hnptr = free_list_of_names( &hnptr );

        return 1;
}

int python_display(char *a_name[17], char *template, char *type, char *graphics, char *d_name)
/* char a_name[][17]; */
/* char template[]; */
/* char type[]; */
/* char graphics[]; */
/* char d_name[]; */
{
        extern int update_ind;
	extern int vcs_legacy_canvas_update();
        extern Gconid_X_drawable 	connect_id;
        extern void draw_logo(cairo_t *cr);

	if (d_name == NULL)
	  return 0;

	/* Set the VCS display table with the new settings */
	d_update(type,0,d_name,"0","-2",graphics,template,a_name);

	/* Needed to update the canvas */
	/* Draw the plot in the VCS Canvas window */
        if ((cmpncs(type,"line") == 0) || (cmpncs(type,"marker") == 0) ||
            (cmpncs(type,"fillarea") == 0) || (cmpncs(type,"text") == 0)) {
	   vcs_legacy_draw_primatives( d_name );
	   draw_logo(connect_id.cr);
        } else {
	  vcs_legacy_canvas_update(0);
	}
	return 1;

}

/* routine to output stdout & stderr to cdat and xterm window */
void pyoutput(char *buf, int bell)
{
      PySys_WriteStdout(buf);
      PySys_WriteStdout("\n");
}

/* routine to output stdout & stderr to cdat and xterm window */
void pyoutput2(char *buf, int bell)
{
      PySys_WriteStdout(buf);
}

void convert_to_upper_case( char *return_str )
{
        int     i;

        for (i = 0; i < (int)strlen(return_str); ++i) {
            if (islower(return_str[i]))
                return_str[i] = toupper(return_str[i]);
        }
}

/* Open the HARD_COPY file in the user's $HOME/DOT_DIRECTORY directory
 * and return the gplot string. The string indicates the location of gplot.
 */
#ifndef X11OUT
#define FALSE 0
#define TRUE 1
#endif

int read_HARD_COPY(char *gplot_str, int orientation)
{
        int             ierr, ffd, rfd, xfd, n, stop_reading_hardcopy;
        char            *home_dir;
        char            hardcopy_file[1024], hardcopy_str[1024];
	char		hold_str[1024];
	char		*gplot_str_ptr;
	FILE            *fp;
	void            convert_to_upper_case();

        /* Create the CGM file name in the user's PCMDI_GRAPHICS
         * directory. If PCMDI_GRAPHICS has not been created, then
         * use the user's home directory.
         */
        home_dir = (char *) getenv("HOME");
        if (home_dir == NULL) {
           pyoutput("Error - The user's home directory was not found!", 1);
           return 1;
        }

        strcpy(hardcopy_file, home_dir);
        strcat(hardcopy_file, "/");
        strcat(hardcopy_file, DOT_DIRECTORY);
        strcat(hardcopy_file, "/HARD_COPY");

        ffd = access(hardcopy_file, F_OK);    /* check to see if the */
        rfd = access(hardcopy_file, R_OK);    /* HARD_COPY file exist */
        if (ffd != 0) { /* The file does not exist! */
           pyoutput("Error - The HARD_COPY file does not exist!", 1);
           return 1;
	}
        if (rfd != 0) { /* The file cannot be read! */
           pyoutput("Error - The HARD_COPY file can not be read!", 1);
           return 1;
	}

        /* Open the HARD_COPY file and receive the printers and GPLOT
         * command lines.
         */
        if ((fp=fopen(hardcopy_file,"r")) == NULL) {
           pyoutput("Error - reading HARD_COPY file.", 1);
        } else {
           stop_reading_hardcopy = FALSE;
           n = 1;
           while (!stop_reading_hardcopy) {
              fgets(hardcopy_str,MAX_PATH_LEN,fp);
              if (feof(fp) != 0)
                 stop_reading_hardcopy = TRUE;
              if ((hardcopy_str[0] != '#') && (hardcopy_str[0] != '\n')) {
                 strcpy(hold_str, hardcopy_str);
                 convert_to_upper_case(hold_str);
                 if ((strncmp(hold_str, "LANDSCAPE", 9)==0) &&
	             (orientation == 0)) {/*Get Landscape*/
                    gplot_str_ptr = strstr(hardcopy_str, "/");
                    if (gplot_str_ptr == NULL) {
                       pyoutput("Error - Landscape command line in HARD_COPY file is incorrect.", 1);
                       return 1;
                    }
                    gplot_str[0] = '\0';
                    strcpy(gplot_str, gplot_str_ptr);
                    gplot_str[strlen(gplot_str)-1] = '\0';
                    stop_reading_hardcopy = TRUE;
                 } else if ((strncmp(hold_str,"PORTRAIT",8)==0) &&
	             (orientation == 1)) {/*Get Portrait*/
                    gplot_str_ptr = strstr(hardcopy_str, "/");
                    if (gplot_str_ptr == NULL) {
                       pyoutput("Error - Portrait command line in HARD_COPY file is incorrect.\n", 1);
                       return 1;
                    }
                    gplot_str[0] = '\0';
                    strcpy(gplot_str, gplot_str_ptr);
                    gplot_str[strlen(gplot_str)-1] = '\0';
                    stop_reading_hardcopy = TRUE;
                 }
              }
           }
           fclose(fp);
        }

	return 0;
}

/*
void X11_mainloop(app_context)
XtAppContext app_context;
{
        XEvent                  report;
        int                     event_loop;

        event_loop = XtAppPending(app_context);
        while (start_stop_loop) {
               XtAppNextEvent(app_context, &report);
               XtDispatchEvent(&report);
               event_loop = XtAppPending(app_context);
        }
}

void cdat_X11_mainloop(app_context, start_stop_loop_flg)
XtAppContext app_context;
int start_stop_loop_flg;
{
	if (start_stop_loop_flg) {
	   start_stop_loop = 1; * Start up the X11 mainloop *

	   * Process all X11 Events, then return to Python *
	   X11_mainloop(app_context);
	} else {
	   start_stop_loop = 0; * Stop the X11 mainloop *
	}
}
*/

void call_guwk_update(int wkst_id)
{
	guwk(wkst_id,GPERFORM);
}

/*
 * Return the normalized page value for x or y.
 */
float
plnorm(int x_or_y, float value)
{
        extern struct orientation       Page;
        float                           canvas_ratio_l=0., canvas_ratio_p=0.;
#ifdef X11WM
        XWindowAttributes       	xwa;
        Dimension                       xwidth, yheight;
#else
	extern void vcs_legacy_Qt_get_window_dimensions_by_id(int id,int *x, int *y,int *w,int *h);
        Gint                       xwidth, yheight;
	Grectangle                      xwa;
	int x,y,w,h;
#endif
/*                      Get the width and height of the VCS Canvas.     */
#ifdef X11WM
        if (connect_id.drawable == 0) {
#else
	  if (connect_id.cr == NULL) {
#endif
           canvas_ratio_l = 0.758800507;
           canvas_ratio_p = 0.758800507;
        } else {
#ifdef X11WM
           XGetWindowAttributes(connect_id.display,
                                connect_id.drawable, &xwa);
#elif defined (QTWM)
	   vcs_legacy_Qt_get_window_dimensions_by_id(connect_id.wkst_id,&x,&y,&w,&h);
	   xwa.height=h;
	   xwa.width=w;
#else
	   printf("insert your WM getgeom here\n");
#endif
           canvas_ratio_l = (float) xwa.height / (float) xwa.width;
           canvas_ratio_p = (float) xwa.width / (float) xwa.height;
        }

        if (strcmp(Page.page_orient,"landscape") == 0) {
           if (x_or_y == 0)
              return value;
           else
              return (value*canvas_ratio_l);
        } else { /* must be portriat */
           if (x_or_y == 1)
              return value;
           else
              return (value*canvas_ratio_p);
        }
}


/*
 * Convert the x or y values between 0 and 1.
 */
float
gnorm(int x_or_y, float value, int normalized_flg, int orientation_flg)
{
        extern struct orientation       Page;
        float                           canvas_ratio_l=0., canvas_ratio_p=0.;
/* #ifdef X11WM */
/*         XWindowAttributes       	xwa; */
/*         Dimension                       xwidth, yheight; */
/* #else */
/*         Gint                       xwidth, yheight; */
/* 	Grectangle                      xwa; */
/* #endif */

/* 	printf("in gnorm we got: %i,%f,%i,%i\n",x_or_y,value,normalized_flg,orientation_flg); */
/*      if normalized flag is 1 then return value. Done...              */
        if (normalized_flg == 1) return value;

/*      Set a fix width and height ratios for the VCS Canvas.     
        To avoid alternative placements for plots, we must fix the 
           landscape and protrait ratios.				*/
	canvas_ratio_l = 0.758800507;
	canvas_ratio_p = 0.760843;
        if (orientation_flg == 0) { /* must be stored landscape, see initial.attributes file */
           if (x_or_y == 0)
              return value;
           else
              return (value/canvas_ratio_l);
        } else { /* must be portriat */
           if (x_or_y == 0)
              return value;
           else
              return (value/canvas_ratio_p);
        }
}

/*
 * Return the normalized page value for x or y. This is the reverse of gnorm and
 * used only to save the template editor's x,y values to the initial.attributes 
 * file or to a VCS script file.
 */
float
glnorm(int x_or_y, float value, int normalized_flg, int orientation)
{
        extern struct orientation       Page;
        float                           canvas_ratio_l=0., canvas_ratio_p=0.;
#ifdef X11WM
        XWindowAttributes       	xwa;
        Dimension                       xwidth, yheight;
#else
        Gint                       xwidth, yheight;
	Grectangle                      xwa;
	int x,y,w,h;
	extern void vcs_legacy_Qt_get_window_dimensions_by_id(int id,int *x, int *y,int *w,int *h);
#endif
/*      if normalized flag is 0 then return value. Done...              */
        if (normalized_flg == 0) return value;

/*                      Get the width and height of the VCS Canvas.     */
#ifdef X11WM
        if (connect_id.drawable == 0) {
#else
	  if (connect_id.cr == NULL) {
#endif
           canvas_ratio_l = 0.758800507;
           canvas_ratio_p = 0.758800507;
        } else {
#ifdef X11WM
           XGetWindowAttributes(connect_id.display,
                                connect_id.drawable, &xwa);
#elif defined (QTWM)
	   vcs_legacy_Qt_get_window_dimensions_by_id(connect_id.wkst_id,&x,&y,&w,&h);
	   xwa.height=h;
	   xwa.width=w;
#else
	   printf("insert here your WM getgeom func\n");
#endif
           canvas_ratio_l = (float) xwa.height / (float) xwa.width;
           canvas_ratio_p = (float) xwa.width / (float) xwa.height;
        }

        if (orientation == 0) { /* must be landscape */
           if (x_or_y == 0)
              return value;
           else
              return (value*canvas_ratio_l);
        } else { /* must be portriat */
           if (x_or_y == 1)
              return value;
           else
              return (value*canvas_ratio_p);
        }
}


/*******************************************************************************
        END OF FILE
*******************************************************************************/
