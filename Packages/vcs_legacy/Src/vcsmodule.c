/*
###############################################################################
#                                                                             #
# Module:       vcs_legacymodule.c                                                   #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Visualization Control System (VCS) Python extension and       #
#               embeded routines. (Also see Canvas.py for the Python API.)    #
#                                                                             #
# Version:      5.0                                                           #
#                                                                             #
###############################################################################
*/

#include "Python.h"
#include "numpy/arrayobject.h"
#include "cdunif.h"
#include "slabapi.h"
#include "gks.h" /* include gks routines */
#include "xgks.h" /* include gks routines */
#include "gksshort.h"
#include "ttffonts.h"
#include "array.h"
#include "display.h"
#include "picture.h"
#include <string.h>
#include "workstations.h"
#include "graph.h"
#include "color.h"
#include "list.h"
#ifdef USEX11
#include <X11/Xlib.h>
#include <X11/Xlibint.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#endif
#include <unistd.h>  /* Includes math functions (e.g., sin, cos, etc.). */
#include <math.h>
/* #include "clui_output.h" */
#include "vcs_legacy_canvas.h"
#include "color_editor.h"
#include "project.h"
#include <cairo/cairo.h>
#include <cairo/cairo-ft.h>


#define PyInit_VCS init_vcs_legacy
#define MAX_NAME 1024

#define heartbeat(s, t) ;
/*
#undef heartbeat
#define heartbeat(s, t) PySys_WriteStdout(s##"\n", t); 
*/
static PyObject *PyVCS_Error;
static PyObject *VCS_Error; /*DUBOIS*/

/* Set the global start and end index for the hyperslab */
extern int index_s[], index_e[];

int	vcs_legacy_open_ct=0;
int     canvas_workstation_id=8;

/* XGKS initialization */
#ifdef USEX11
Display *display=NULL,*display2=NULL;                /* display reference */
int screen;                      /* screen index we're running on */
XVisualInfo vinfo;               /* for finding desired visual */
Visual *visual;                  /* hopefully we get one we want */
GC gc;                           /* graphics context */
extern Colormap n_cmap;                 /* virtual normal colormap */
#endif
int not_using_gui;		 /* FLAG to determine if GUI is used */
int vcs_legacy_gui=0;		 	 /* FLAG to determine if VCS GUI is used */
int namecount=1;		 /* Used for unique plot names */


extern PyInterpreterState * mainInterpreterState;
extern PyThreadState * mainThreadState;
extern PyThreadState * myThreadState;


/* Catch all the warning messages and do nothing. */

extern Gconid_X_drawable connect_id; /* VCS canvas drawable id */
extern struct workstations Wkst[]; /* VCS XGKS canvas workstations IDs */

extern char  *repstr(char  *s2,char  *s1);
extern float *repflt(float *f2,float *f1);
extern int   *repint(int *i2,int *i1);
extern void pyoutput(char *buf, int bell);
extern void pyoutput2(char *buf, int bell);
#ifdef CAIRODRAW
extern void cairogqtxx(int wkid, Gpoint pxy, char *str, Gextent *extent);
#else
extern void gqtxx(int wkid, Gpoint pxy, Gchar *str, Gextent *extent);
#endif
extern int cmpncs(char *s1, char *s2);


/* Structure for VCS graphics methods used to set min and max attributes */
typedef struct graphics_method_list {  /* Store the graphics methods names */
  char *g_type;                        /* Graphics type */
  char *g_name;                        /* Graphics method name */
  struct graphics_method_list *next;   /* Pointer to the next structure */
} graphics_method_list;

#include "pyvcs_legacy.h"


/* C. DOUTRIAUX June 16th, 2009 */
/* EVENT RELATED FUNCTION ARE NOW EXTERNALS FROM THIS NEED THE DECALRATION HERE */
 extern PyObject *PyVCS_screen_data_flag(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_screen_template_flag(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_screen_gm_flag(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_checkmode_data_flag(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_screen_mode(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_select_one(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_unselect_one(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_select_all(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_unselect_all(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_Xsync_discard(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_BLOCK_X_SERVER(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_UNBLOCK_X_SERVER(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_Xpending(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_stopxmainloop(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_startxmainloop(PyVCScanvas_Object *self, PyObject *args);
 extern PyObject *PyVCS_get_selected_display_graphic_method(PyVCScanvas_Object *self,PyObject *args);


staticforward PyTypeObject PyVCScanvas_Type;

/* Structure to keep track of VCScanvas_Objects.
 * This is needed for animation on the specified
 * canvas.
 */
struct vcs_legacycanvas_list {
	PyVCScanvas_Object 	*self;        /* Hold the canvas information */
	PyObject 		*slab;	      /* Hold slab information */
	PyObject 		*slab2;	      /* Hold slab information */
        Gconid_X_drawable       connect_id;   /* VCS canvas drawable ID */
	char			template[50]; /* Hold the VCS template */
	char			graphics[50]; /* Hold the VCS graphics */
	char			type[50];     /* Hold the VCS type */
	char			d_name[MAX_NAME]; /* Hold the display name */
        struct vcs_legacycanvas_list    *next;
};
typedef struct vcs_legacycanvas_list  	VCSCANVASLIST;
typedef VCSCANVASLIST        	*VCSCANVASLIST_LINK;

VCSCANVASLIST_LINK	        head_canvas_list=NULL;

/* Struct for FONTS */
	extern struct table_FT_VCS_FONTS TTFFONTS;


/* Check for the type of screen Visual that the X server can support.
 * In order for VCS to work, the X server must support the PseudoColor
 * class for 8 bpp mode and DirectColor class for 24 bpp or 32 bpp mode.
 * If either of these two visuals are not supported then the vcs_legacy module
 * will error exit.
 */
static char *visual_class[] = {
"StaticGray",
"GrayScale",
"StaticColor",
"PseudoColor",
"TrueColor",
"DirectColor"
};




/* Since Python doesn't have an X11 mainloop, we must process all the events
 * before we move on.
 */
void process_cdat_events(void) {
/* #ifdef X11OUT */
/*   XEvent                  report; */
/* #endif */
/*   int                     event_loop; */

/* #ifdef X11OUT */
/*        if (connect_id.display == NULL) return; */
/* #endif */
        /* Flush, sync, and update the request buffer before returning to
	 * Python
        XFlush( connect_id.display );
        XSync( connect_id.display, FALSE );
         */

/*
	event_loop = XPending(connect_id.display);
        while (event_loop != 0) {
               XNextEvent(connect_id.display, &report);
               *
                * Below is a comment to DNW:
                *
	   	* Handle_VCS_X_Events is not yet implemented, but
		* will be needed to select values back from the canvas.
		* I am reseving this code for the future, when I come
		* back to add events that will be executed as a result
		* of selecting portions of the VCS Canvas.
                *
               event_loop = XPending(connect_id.display);
               printf(" event_loop = %d\n", event_loop);
        }
*/

        return;
}

/* This function sets the correct VCS Canvas and XGKS workstation ID */
void setup_canvas_globals(PyVCScanvas_Object *self)
{
	/* Set the correct drawable (that is, VCS Canvas) */

        connect_id = self->connect_id;
	//connect_id.wkst_id = self->canvas_id;
        /* Set the proper XGKS workstation ID */

	/* DNW and CD removing this as it confuses everything */
        //Wkst[0].id = self->wkst_id;
	/* CD put it back on for background plots only */
	if (self->wkst_id==8) Wkst[0].id = 8;
}

/* Called when Python is exited or the object name is reused. Deallocates
 * the VCS Canvas Object.
 */
static void
PyVCScanvas_Dealloc(PyVCScanvas_Object *self)
{
	canvas_display_list 		*dptr, *tdptr;
	static PyVCScanvas_Object 	*save_self=NULL;
        PyObject * 			PyVCS_clear(PyVCScanvas_Object *self, PyObject *args);
	extern void 			remove_vcs_legacy_connection_information(Gconid_X_drawable connect_id, int wkst_id);
	extern int 			shutdown(Gconid_X_drawable connect_id, int wks);
/* 	extern int 			animating(); */
/* 	extern void			view_all_animation_panels(); */
/* 	extern void 			animate_quit_cb(int w); */
	extern Gint gdacwk(Gint ws_id);
	extern void gclwk( Gint ws_id );

	/* If the GUI was not stated (i.e., cdatgui), then we need to
	 * process all the X events before we move on.
	 */
	if (not_using_gui)
	   process_cdat_events();

	if (self == NULL) /* Must have been called from animation. */
	   self = save_self;

	if (self == NULL) /* Already garbage collected! */
	   return;

	/* Free all resouces associated with the animation popup window.
	 * That is, destroy the animation object window and all of its
         * popup descendants and widgets. Then free all resources associated
	 * with the animation popup window and its descendants.
	 */
/*DNW	if (self->connect_id.animate_popup != 0) {
	   *View all animation panels before the VCS objects are destroyed*
	   view_all_animation_panels(self->connect_id.animate_popup);

           if (!animating(self->connect_id.animate_popup )) {
              animate_quit_cb(self->connect_id.animate_popup, NULL,NULL);
	      XtDestroyWidget(self->connect_id.animate_popup);
           } else {
	     save_self = self; * Save, I'll be right back! *
	     return ; * can not garbage collect at this time *
                      * must stop animation process first *
	   }
	}
DNW*/

	/* Make sure everything is clear in the VCS Canvas *
	PyVCS_clear(self,NULL);*/

	/* Shut down the xgks workstation *
	if ((self->vcs_legacy_gui != 1) && (self->connect_id.drawable != 0)) {
	   XDestroyWindow(self->connect_id.display,self->connect_id.drawable);
	   self->connect_id.drawable = (XID) NULL;
	   shutdown(self->connect_id, self->wkst_id);
           XFree( &self->connect_id.drawable );
	} */

	/* Free the template and graphics method names */
	if (self->template_name != NULL)
           free((char *) self->template_name);	
	if (self->graphics_name != NULL)
           free((char *) self->graphics_name);	

	/* Free the VCS display list and display names used in the
         * VCS picture description forms.
	 */
	if (self->dlist != NULL) {
	   dptr=self->dlist;
	   while (dptr != NULL) {
	        if (dptr->display_name != NULL)
                   free((char *) dptr->display_name);
   
                tdptr = dptr;
	        dptr = dptr->next;
                free((char *) tdptr);
	   }
	}

#ifdef X11WM
	/* Free all resouces associated with the VCS Canvas popup window.
	 * That is, destroy the VCS Canvas object window and all of its
         * popup descendants and widgets. Then free all resources associated
	 * with the VCS Canvas popup window and its descendants.
	 */
	if ((self->vcs_legacy_gui != 1) && (self->connect_id.drawable != 0)) {
	   self->connect_id.drawable = (XID) NULL;
	   shutdown(self->connect_id, self->wkst_id);
	}
#endif
	/* Free the connection information used in the VCS library */
	if (self->vcs_legacy_gui != 1)
	   remove_vcs_legacy_connection_information(self->connect_id,self->wkst_id);

	/* Keep track of how many VCS Canvases that are opened. There can
	 * only be (at most) 8 opened at any given time. Decrement the 
         * vcs_legacy open counter.
  	--vcs_legacy_open_ct;
	 */
        --canvas_workstation_id;

#ifdef X11WM
        /* Disconnect the X server */
        XCloseDisplay( self->connect_id.display );
#elif defined QTWM
	vcs_legacy_Qt_destroy_window(self->connect_id.wkst_id);
#else
	fprintf(stderr,"insert here your close win fnc\n")
#endif
	/* deactivate and close the workstation */
        gdacwk( self->wkst_id );
        gclwk( self->wkst_id );


/*	pyoutput("The VCS Canvas has been garbage collected!", 1);*/
	if (self->vcs_legacy_gui == 1) /* Check for VCS canvas */
	  pyoutput("\nYou have just garbage collected the main VCS Canvas. \nTo reuse the main VCS Canvas from CDAT, you must restart VCS.\n", 1);

	/* Delete VCS Canvas object */
  	PyObject_Del(self);

}

/*- support: visuals -------------------------------------------------------*/
/*
 * gc_create()
 *      Construct a graphics context.
 */
#ifdef USEX11
void gc_create(/* ARGS UNUSED */)
{
        XGCValues values;

        gc = XCreateGC(connect_id.display, connect_id.drawable, 0L, &values);
}

/*
 * visual_find - find the visual we need. Currently, VCS can handle 8-bit PseudoColor, 8-bit StaticColor,
 *		 16-bit TrueColor, 24-bit TrueColor, and 32-bit TrueColor visual classes and depth. Use
 *		 'xdpyinfo' to check the display's color class, visual, and depth.
 */
int visual_find(/* ARGS UNUSED */)
{
	XVisualInfo visual_info;
	int default_depth;

	default_depth = DefaultDepth(display, screen);
	//fprintf(stderr,"default depth is: %i\n",default_depth);
	if (default_depth == 8) {
           if (XMatchVisualInfo(display,screen,default_depth,
                                PseudoColor,&visual_info) == 0) {
              PySys_WriteStdout("Failure: VCS cannot find the PseudoColor visual class for 8 bpp mode. \n");
              exit(1);
	   }
        } else if (default_depth == 16) {
           if (XMatchVisualInfo(display,screen,default_depth,
                                TrueColor,&visual_info) == 0) {
              PySys_WriteStdout("Failure: VCS cannot find the TrueColor visual class for 16 bpp mode. \n");
              exit(1);
           }
        } else if (default_depth == 24) {
           if (XMatchVisualInfo(display,screen,default_depth,
                                TrueColor,&visual_info) == 0) {
              PySys_WriteStdout("Failure: VCS cannot find the TrueColor visual class for 24 bpp mode. \n");
              exit(1);
           }
	} else if (default_depth == 32) {
           if (XMatchVisualInfo(display,screen,default_depth,
                                TrueColor,&visual_info) == 0) {
              PySys_WriteStdout("Failure: VCS cannot find the TrueColor visual class for 32 bpp mode. \n");
              exit(1);
	   }
	} else {
              PySys_WriteStdout("Failure: VCS could not fine 8-bit PseudoColor or 16-, 24-, or -32 bit TrueColor visual classes.\n         Please set your X server's 'Color Depth' to 8-bit PseudoColor, 8-bit StaticColor, 16-bit\n         TrueColor, 24-bit TrueColor, or 32-bit TrueColor mode. To check your display's visual\n         class and depth, type: 'xdpyinfo' at the prompt.\n");
              exit(1);
	}

        if (XMatchVisualInfo(display,screen,visual_info.depth,visual_info.class,&vinfo) == 0) {
           PySys_WriteStdout("Failure: Can not find visual class. \n");
        }
        visual = vinfo.visual;
	//fprintf(stderr,"in visual_find visual set to %p\n",visual);
	/* Return the visual depth of the display */
	return visual_info.depth;
}
/*
 * normal_cmap_create()
 *      Create a virtual normal colormap.
 */
void normal_cmap_create( void /* ARGS UNUSED */)
{
        n_cmap = XCreateColormap(
                 display, RootWindow(display, screen), visual, AllocNone
        );
}
/*
 * normal_cmap_emulate_default2()
 *      Copy default colormap entries over to new
 *      custom normal colormap.
 *
 *      This does not gaurantee a fix for colormap flashing but may help to
 *      reduce it.
 *
 */
void normal_cmap_emulate_default2(void/* ARGS UNUSED */)
{
        Status stat;            /* return value */
        Colormap n_cmap_def;    /* normal default colormap */
        unsigned long pmask[1]; /* array for plane masks */
        unsigned long *index;   /* malloc'ed array for color indices */
        XColor *color;          /* malloc'ed array for color replication */
        int ncolors;            /* number of colors to copy */
        int i;                  /* index */
	/* 
	 * For the purpose of 16-bit, 24-bit, and 32-bit True Color visuals, use the
	 * static default screen colormap. Therefore the below is not needed. For 8-bit
         * pseudo color and eventually 24-bit direct color, we want a dynamic color map
	 * where we can read and write into each color cell. For now only the 8-bit
	 * color map is intented for the below. I'm not sure if we will ever expand to
	 * direct color.
	 */
	if (visual->class == PseudoColor) { /* maintain the 8-bit pseudo color functionality */
           /* get default colormap and determine how many colors to copy */
           n_cmap_def = DefaultColormap(display, screen);
           ncolors = DisplayCells(display, screen);
           if (ncolors > NUM_COLORS) {
              ncolors = NUM_COLORS; /*limit how many default colors we copy*/
           }
   
           /*
            * allocate a bunch of read/write colors cells.  since this colormap
            * was just created these colors should end up being the lower colors.
            */
           index = (unsigned long *) Xmalloc(ncolors*sizeof(index[0]));
           color = (XColor *) Xmalloc(ncolors*sizeof(XColor));
           stat = XAllocColorCells(display,n_cmap,True,pmask,0,index,ncolors);
           if (! stat) {
              PySys_WriteStdout("Failure: Default color allocation failed. \n");
              exit((int) 1);
           }
   
           /* map out the color entries we will query for */
           for (i=0; i<ncolors; i++)
           color[i].pixel = index[i];
   
           /* transfer colors using query/store */
           XQueryColors(display, n_cmap_def, color, ncolors);
           XStoreColors(display, n_cmap    , color, ncolors);
   
           /* cleanup */
           XFree((char *) index);
           XFree((char *) color);
	}
}
#endif

/* Initialize VCS Canvas object. */
static PyObject *
PyVCS_init(PyObject *self, PyObject *args)
{
  	PyVCScanvas_Object 		*vcs_legacycanvas;
/* 	Gintlist 			wsid; */
/* 	int				ctid=0, *pid; */
        int                             winfo_id;
	int 				initialize_X(void);
	double size;
	/* In VCS, the workstation ID 2 represents the CGM ouput.
	 * Therefore, start the count at workstation count at 3.
	static int canvas_workstation_id=8;
         */
#ifdef USEQT
	extern int vcs_legacy_Qt_init_window(PyVCScanvas_Object *vcs_legacycanvas);
#endif

/*   	char buf[MAX_NAME]; */

	/* Initialize X11 if VCS was imported without the GUI front-end */
/*	if (app_context == NULL) {
 	   if (initialize_X() == 0) {
              PyErr_SetString(PyExc_TypeError, "The DISPLAY environment is not properly set.\n");
	      return NULL;
	   }
	}*/
	/* Set the workstation id to wiss. The avoids confussion until the VCS Canvas
	 * is opened.
         */
	Wkst[0].id = 1; 

 	if (initialize_X() == 0) {
              PyErr_SetString(PyExc_TypeError, "The DISPLAY environment is not properly set.\n");
	      return NULL;
	}
	/* Initialize the VCS canvas as a new Python object */
  	vcs_legacycanvas = PyObject_NEW(PyVCScanvas_Object, &PyVCScanvas_Type);

  	/* Set the VCS initialization flag to 0 (not initialized) *
        wsid.number = 0;
        wsid.integers = NULL;
        gqopwk(&wsid);
        for (i=0,pid=wsid.integers;i<wsid.number;pid++,i++) {
	    ++ctid;
             printf("the wrkstation id = %d\n", *pid);
        }
        if (wsid.number > 0 && wsid.integers != NULL)
            free((char *)wsid.integers);
	if (ctid == 1) canvas_workstation_id = 8;
	*/

        vcs_legacycanvas->wkst_id = canvas_workstation_id;
        ++canvas_workstation_id;

	/* In VCS, the workstation ID 7 represents the Workstation 
         * Independences Storage Segment (WISS). Therefore, do not use
         * this ID number. Skip over it and continue the sequential 
	 * count.
	if (canvas_workstation_id == 6)
	   ++canvas_workstation_id;
	*/

	/* Initialize the VCS Canvas to 0. It has not been displayed yet. */
	vcs_legacycanvas->virgin = 0;

	/* Initialize the  VCS Canvas animation to 1. The canvas has not
         * yet done an animation.
         */
  	vcs_legacycanvas->virgin_animation = 1;

	/* Set the orientation flag to landscape=0 */
	vcs_legacycanvas->orientation = 0;

	/* Initialize the canvas counter */
	vcs_legacycanvas->vcs_legacy_min = 1e20; vcs_legacycanvas->vcs_legacy_max = -1e20;
	vcs_legacycanvas->vcs_legacy_ext1 = 0, vcs_legacycanvas->vcs_legacy_ext2 = 0;

        if ((vcs_legacycanvas->template_name =
           (char *) malloc((strlen("default")+1)*sizeof(char)+1)) == NULL) {
           PyErr_SetString(PyExc_TypeError, "No memory for the template name.");
           return NULL;
        }/* else {
           strcpy(vcs_legacycanvas->template_name, "default");
	   sprintf(buf, "'Template' is currently set to P_%s.", 
                   vcs_legacycanvas->template_name);
           pyoutput(buf, 1);
        }*/
	
        if ((vcs_legacycanvas->graphics_name =
            (char *) malloc((strlen("default")+1)*sizeof(char)+1)) == NULL) {
           PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
           return NULL;
        }/* else {
           strcpy(vcs_legacycanvas->graphics_name, "default");
           strcpy(vcs_legacycanvas->graphics_type, "Boxfill");
	   sprintf(buf,"Graphics method 'Boxfill' is currently set to Gfb_%s.", vcs_legacycanvas->graphics_name);
           pyoutput(buf, 0);
        }*/

	

	/* Initialize to NULL */
        vcs_legacycanvas->gui = 0;
	vcs_legacycanvas->connect_id = connect_id;
	vcs_legacycanvas->connect_id.cr = NULL;
	vcs_legacycanvas->connect_id.surface=NULL;
#ifdef USEX11
	vcs_legacycanvas->connect_id.display = connect_id.display;
        vcs_legacycanvas->connect_id.drawable = (XID) NULL;
#endif
        if(PyArg_ParseTuple(args,"|id", &winfo_id, &size)) {
           if ( winfo_id != -99) {
                 vcs_legacycanvas->gui = 1;
                 /*connect_id.drawable = (XID) winfo_id;*/
                 /*vcs_legacycanvas->connect_id.drawable = (XID) winfo_id;*/
#ifdef X11DRAW
                 vcs_legacycanvas->gui_drawable = (XID) winfo_id; /* must set the drawable in PyVCS_open */
#endif
           }
        }
#ifdef USEX11
	vcs_legacycanvas->connect_id.canvas_popup = 0;
	vcs_legacycanvas->connect_id.canvas_drawable = 0;
	vcs_legacycanvas->connect_id.animate_popup = 0;
        vcs_legacycanvas->connect_id.canvas_pixmap = (Pixmap)NULL;  /*used as the backing store*/
        vcs_legacycanvas->connect_id.app_context = 0;
        vcs_legacycanvas->connect_id.app_shell = 0;
        vcs_legacycanvas->connect_id.cf_io_text = 0;
        vcs_legacycanvas->connect_id.n_cmap = connect_id.n_cmap;
        vcs_legacycanvas->connect_id.visual = NULL;
#endif
	vcs_legacycanvas->dlist = NULL;
	vcs_legacycanvas->glist = NULL;
        vcs_legacycanvas->stopxmainloop = 0;
	vcs_legacycanvas->havexmainloop = 0;
        vcs_legacycanvas->number_of_frames = 0;
        vcs_legacycanvas->frame_count = 0;
        vcs_legacycanvas->savecontinents = -999;
        vcs_legacycanvas->background = NULL;
	vcs_legacycanvas->canvas_id = vcs_legacycanvas->wkst_id - 7; /* canvas is closeed */
	connect_id.wkst_id = vcs_legacycanvas->canvas_id;
	vcs_legacycanvas->connect_id.wkst_id = vcs_legacycanvas->canvas_id;
	vcs_legacycanvas->orig_ratio = size;
/*        fprintf(stderr, "INIT 1: canvas_pixmap %d = %d\n", vcs_legacycanvas->canvas_id, vcs_legacycanvas->connect_id.canvas_pixmap);*/
        ++vcs_legacy_open_ct; /* Increment the VCS open counter */
        vcs_legacycanvas->canvas_id = vcs_legacy_open_ct;

	/* Set the VCS GUI flag */
	if (vcs_legacy_gui == 0)
	   vcs_legacycanvas->vcs_legacy_gui = 0;
	else
	   vcs_legacycanvas->vcs_legacy_gui = vcs_legacy_gui++;

#ifdef QTWM
       vcs_legacy_Qt_init_window(vcs_legacycanvas);
#endif
  	/* return the VCS canvas object to python */
  	return (PyObject *)vcs_legacycanvas;
}

/* Open VCS Canvas object. This routine really just manages the
 * VCS canvas. It will popup the VCS Canvas for viewing. 
 */
static PyObject *
PyVCS_open(PyVCScanvas_Object *self, PyObject *args)
{
	int ier, tmp = -99;
	extern int procCanvas(char *str, Gconid_X_drawable **connect_id_in, int canvas_id, double ratio, char *gui, int *tok);
	extern int clearCanvas(Gconid_X_drawable connect_id);
	int c;
#ifdef X11WM
        struct color_table *pctab;
        extern struct color_table C_tab;
	extern char 	active_colors[]; /*colormap name*/
#endif
#ifdef X11WM
	extern Pixmap create_pixmap(Gconid_X_drawable connect_id);
#endif
	extern void vcs_legacy_canvas_open_cb(Gconid_X_drawable connect_id);
	extern void store_vcs_legacy_connection_information(Gconid_X_drawable connect_id,int wkst_id);
	extern struct orientation       Page;
	/* If the GUI was not stated (i.e., cdatgui), then we need to
	 * process all the X events before we move on.
	 */
	if (not_using_gui)
	   process_cdat_events();

        if (self->orientation == 0 ) /* Set the page orientation before plotting */
           strcpy(Page.page_orient,"landscape");
        else
           strcpy(Page.page_orient,"portrait");

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	/* Get how many VCS Canvases are open. If there are more than
         * 8 canvases, then do not open another and tell the user that
         * there can only be a maximum of 8 VCS canvases open at
         * any given time. The user must close an already existing
         * VCS Canvas.
         */
/*	if (vcs_legacy_open_ct > 8) {
           pyoutput("Error - There can be a maximum 8 VCS Canvases.\n       CDAT cannot exceed this number. Please try reusing one\n       of the existing VCS Canvases.",1);
	   --vcs_legacy_open_ct; * Decrement the VCS open counter back to 8 *
	   *Py_INCREF ((PyObject *)Py_None);
           return Py_None;*
	}*/

	/* Set the proper VCS Canvas */
	/* DNW and CD took that out it was confusing the XGKS when opening multiple ws */
	/*	Wkst[0].id = self->wkst_id; */
	/* CD put it back on for background plots only */
	if (self->wkst_id==8) Wkst[0].id = 8;
	if (self->virgin==0) {
	   if (self->vcs_legacy_gui != 1)
#ifdef X11WM
	     if (self->gui == 1) self->connect_id.drawable = self->gui_drawable; /* set to the Tk drawable */
#endif
#ifdef QTWM
           store_vcs_legacy_connection_information(self->connect_id, Wkst[0].id);
#endif
              ier = procCanvas("open", &self->connect_id, self->canvas_id, self->orig_ratio,self->gui, &tmp); /* Open the VCS Canvas */
           store_vcs_legacy_connection_information(self->connect_id, Wkst[0].id);
           setup_canvas_globals(self);
           //self->connect_id = connect_id; /* Set the connect_id */
	   /* Set up the VCS Canvas and XGKS workstation */
	} else {
	   /* Set up the VCS Canvas and XGKS workstation */
           setup_canvas_globals(self);
	   if (self->vcs_legacy_gui != 1) /* Popup VCS canvas */
	      vcs_legacy_canvas_open_cb(self->connect_id);
	}

	/* The VCS Canvas has been opened and displayed on the screen */
	self->virgin = 1;

#ifdef X11DRAW
        /* This is needed when an open canvas is open for the first time */
   	if (self->connect_id.canvas_pixmap != (Pixmap) NULL) {
              XFreePixmap(self->connect_id.display, self->connect_id.canvas_pixmap);
              self->connect_id.canvas_pixmap = (Pixmap) NULL;
          }
#elif defined (CAIRODRAW)
	/* with cairo no need for extra pixmap it is all drawn in bg */
#else
	fprintf(stderr,"insert here your WM free pixmap function\n");
#endif
        /* set the keyboard focus to the VCS Canvas *
        if (self->connect_id.drawable != 0 )
              XSetInputFocus( self->connect_id.display, self->connect_id.drawable,
                 RevertToParent, (Time) CurrentTime);
        XFlush( self->connect_id.display );
        XSync( self->connect_id.display, False);*/
#ifdef X11WM
        if (visual->class == PseudoColor) {  /* Only do this for 8-bit PseudoColor */
              for(pctab=&C_tab;pctab != NULL && (c=strcmp(pctab->name,active_colors))!=0;
                                        pctab=pctab->next);
              setXcolormap(pctab);
        }
#endif
/*      This causes the plot to clear each time plot is called, since plot calls PyVCS_open
        keep an eye on this to make sure it was not needed for something else.
        XClearWindow(self->connect_id.display,self->connect_id.drawable);
*/

	/* Return NULL Python Object */
	Py_INCREF ((PyObject *)Py_None);
  	return Py_None;
}



int
update_gui_canvas_counter( PyVCScanvas_Object *self)
{
        PyObject *mdict = NULL, *main = NULL, *dkeys=NULL,*dvalues=NULL, *dstring=NULL;
        PyObject *dlist=NULL, *dvalue=NULL, *dnum=NULL;
        PyObject* tattribute=NULL;
        PyObject* result=NULL;

        PyObject* testattribute;
        char *test_str=NULL;

        int i, dsize;
        int canvas_num=0;

        /*PY_ENTER_THREADS
        PY_GRAB_THREAD*/

        main = PyImport_ImportModule("__main__");
        mdict = PyModule_GetDict( main ); /* borrowed ref */
        dsize = PyDict_Size( mdict );
        dkeys = PyDict_Keys( mdict);
        dvalues = PyDict_Values( mdict );

        /*printf( "frame count: %i\n",self->frame_count); */
        for (i = 0; i < dsize; i++) {
             dlist = PyList_GetItem(dkeys, i); /* borrowed ref */
             dvalue=PyList_GetItem(dvalues, i); /* borrowed ref */
              if (PyString_Check(dlist)) {      /* get the canvas object */
                dnum = PyObject_CallMethod(dvalue, "canvasid", (char*)0);
                canvas_num = (int) PyInt_AsLong (dnum);
                if (canvas_num == self->canvas_id) {
                   tattribute = PyObject_GetAttrString(dvalue, "canvas_gui");
                   result = PyObject_CallMethod(tattribute, "update_animation", "i", self->frame_count);
                }
              }
        }


        Py_XDECREF( main );
        Py_XDECREF( dkeys );
        Py_XDECREF( dvalues );
        Py_XDECREF( dnum );
        Py_XDECREF( tattribute );
        Py_XDECREF( result );

/*        PY_RELEASE_THREAD
        PY_LEAVE_THREADS*/

        return 1;
}

int
update_end_of_animation( PyVCScanvas_Object *self)
{
        PyObject *mdict = NULL, *main = NULL, *dkeys=NULL,*dvalues=NULL, *dstring=NULL;
        PyObject *dlist=NULL, *dvalue=NULL, *dnum=NULL;
        PyObject* tattribute=NULL;
        PyObject* result=NULL;

        PyObject* testattribute;
        char *test_str=NULL;

        int i, dsize;
        int canvas_num=0;

/*         PY_ENTER_THREADS */
/*         PY_GRAB_THREAD */

        main = PyImport_ImportModule("__main__");
        mdict = PyModule_GetDict( main ); /* borrowed ref */
        dsize = PyDict_Size( mdict );
        dkeys = PyDict_Keys( mdict);
        dvalues = PyDict_Values( mdict );

        /*printf( "frame count: %i\n",self->frame_count); */
        for (i = 0; i < dsize; i++) {
             dlist = PyList_GetItem(dkeys, i); /* borrowed ref */
             dvalue=PyList_GetItem(dvalues, i); /* borrowed ref */
              if (PyString_Check(dlist)) {      /* get the canvas object */
                dnum = PyObject_CallMethod(dvalue, "canvasid", (char*)0);
                canvas_num = (int) PyInt_AsLong (dnum);
                if (canvas_num == self->canvas_id) {
                   tattribute = PyObject_GetAttrString(dvalue, "canvas_gui");
                   result = PyObject_CallMethod(tattribute, "update_end_of_animation_creation",  (char *)0);
                }
              }
        }


        Py_XDECREF( main );
        Py_XDECREF( dkeys );
        Py_XDECREF( dvalues );
        Py_XDECREF( dnum );
        Py_XDECREF( tattribute );
        Py_XDECREF( result );

/*        PY_RELEASE_THREAD */
/*         PY_LEAVE_THREADS */

        return 1;
}

/* Charles Doutriaux 11/21/2006 
 * Returns the "VCS Canvas Python Object */

PyObject * 
getPyCanvas( int canvas_id)
{
  PyObject *main = NULL, *dvalues=NULL, *canvas=NULL;
  PyObject *dvalue=NULL, *dnum=NULL;
  int i, dsize;
  int canvas_num=0;
      
  main = PyImport_ImportModule("vcs_legacy");
  dvalues = PyObject_GetAttrString(main, "canvaslist");
  dsize = PyList_Size( dvalues );
  
  for (i = 0; i < dsize; i++) {
    dvalue=PyList_GetItem(dvalues, i); /* borrowed ref */
    dnum = PyObject_CallMethod(dvalue, "canvasid", (char*)0);
    canvas_num = (int) PyInt_AsLong (dnum);
    if (canvas_num == canvas_id) {
      canvas = PyList_GetItem(dvalues, i); /* borrowed ref */
    }
    Py_XDECREF( dnum );
  }
  
  Py_XDECREF( main );
  Py_XDECREF( dvalues );
  return canvas;
  
}



/* Tell if VCS Canvas is threaded.
 */
static PyObject *
PyVCS_THREADED(PyVCScanvas_Object *self, PyObject *args)
{
        /* Return Python Object */
        return Py_BuildValue("i", self->havexmainloop);
}



/*
        dtab=&D_tab;
        while (dtab != NULL) {
              if (dtab->off == 0) dtab->off = 1;
              dtab = dtab->next;
        }
*/
int undisplay_resize_plot(PyVCScanvas_Object *self)
{
        char                            *display_name;
        extern int                     	update_ind;
        extern int                      vcs_legacy_canvas_update();
        struct display_tab      	*pd;
	extern struct display_tab 	D_tab;
        extern char *                   return_display_name();
	int off;

        display_name=return_display_name(self->connect_id);
        while (display_name!=NULL) {
            for (pd=&D_tab;pd != NULL;pd=pd->next) {
               if (strcmp(display_name,pd->name)==0) {
		 off = pd->off;
                  if (pd->off == 0) pd->off = 1;

               }
            }
            display_name=return_display_name(self->connect_id);
        }

        /* Update the display if needed */
/*         update_ind = 1;  */
/*         vcs_legacy_canvas_update(0); */

#ifdef X11WM
        /* Remove the backing store pixmap */
        if ( (self->connect_id.display != NULL) && (self->connect_id.drawable != 0) ) {
           XClearWindow(self->connect_id.display, self->connect_id.drawable);
           XFlush( self->connect_id.display );
           XSync( self->connect_id.display, FALSE );
        }
        if (self->connect_id.canvas_pixmap != (Pixmap) NULL) {
              XFreePixmap(self->connect_id.display, self->connect_id.canvas_pixmap);
              self->connect_id.canvas_pixmap = (Pixmap) NULL;
        }
#elif defined (QTWM)
	extern  void vcs_legacy_Qt_clear_window_by_id(int id);
	vcs_legacy_Qt_clear_window_by_id(self->connect_id.wkst_id);
#else
	fprintf(stderr,"insert here your WM clear and remove backing store pxmap\n");
#endif
	return off;
}

/*
        dtab=&D_tab;
        while (dtab != NULL) {
              if (dtab->off == 1) {
                 dtab->off = 0;

                 for (pi=&dtab->F_seg[0]; pi <= &dtab->dsp_seg[3]; pi+=4) {
                    if (*pi > 0)
                       gdsg(*pi);
                    *pi=0;
                    *(pi+1)=0;
                    *(pi+2)=0;
                    *(pi+3)=1;
                 }

              }

              dtab = dtab->next;
        }
*/
void display_resize_plot(PyVCScanvas_Object *self, int off)
{
        int				 *pi;
        char                            *display_name;
        extern int                     	update_ind;
        extern int                      vcs_legacy_canvas_update();
        struct display_tab      	*pd;
	extern struct display_tab 	D_tab;
#ifdef X11WM
	extern Pixmap 			copy_pixmap(Gconid_X_drawable connect_id,int canvas_id);
#endif
        extern char *                   return_display_name();

        display_name=return_display_name(self->connect_id);
        while (display_name!=NULL) {
            for (pd=&D_tab;pd != NULL;pd=pd->next) {
               if (strcmp(display_name,pd->name)==0) {
		 if ((pd->off == 1) && (off!=1)) {
                     pd->off = 0;

                     for (pi=&pd->F_seg[0]; pi <= &pd->dsp_seg[3]; pi+=4) {
                        if (*pi > 0)
                           gdsg(*pi);
                        *pi=0;
                        *(pi+1)=0;
                        *(pi+2)=0;
                        *(pi+3)=1;
                     }
                  }
               }
            }
            display_name=return_display_name(self->connect_id);
        }


        /* Update the display if needed */
        update_ind = 1; 
        vcs_legacy_canvas_update(1);
/*         vcs_legacy_canvas_update(0); */

#ifdef X11WM
        /* Copy the current VCS canvas to the pixmap (i.e., backing_store) */
        if (self->connect_id.canvas_pixmap != (Pixmap) NULL) {
              XFreePixmap(connect_id.display, self->connect_id.canvas_pixmap);
              self->connect_id.canvas_pixmap = (Pixmap) NULL;
        }
        self->connect_id.canvas_pixmap = copy_pixmap(self->connect_id, self->canvas_id);
#elif defined QTWM
	/* nothing to do */
#else
	fprintf(stderr,"insert here your WM copy pixmap to backing store\n");
#endif
}

/* Return the canvas ID number. This identifies the canvas and this
 * ID number is shown at the top of the Canvas and animation frame.
 */
static PyObject *
PyVCS_canvas_id(PyVCScanvas_Object *self, PyObject *args)
{
        /* Return canvas id number */
        return Py_BuildValue("i", self->canvas_id);
}

/* Set the window information ID to the VCS Canvas. This will attach
 * the two together.
 */
static PyObject *
PyVCS_connect_gui_and_canvas(PyVCScanvas_Object *self, PyObject *args)
{
        int                             winfo_id;

#ifdef X11WM
        if(PyArg_ParseTuple(args,"|i", &winfo_id)) {
           if ( winfo_id != -99) {
                 self->connect_id.drawable = (XID) winfo_id;
           }
        }
#endif
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Open the VCS Canvas for animation, if the canvas is not already open.*/
void open_canavas_for_animation()
{
  	PyVCS_open(NULL, NULL);
}

/* Close a VCS Canvas.  This routine does not deallocate the 
 * VCS Canvas object. It will only unmanage and pop down the 
 * VCS Canvas.
 */
PyObject *PyVCS_close(PyVCScanvas_Object *self, PyObject *args)
{
        struct display_tab      	*dtab;
	extern struct display_tab 	D_tab;
	graphics_method_list		*gptr, *tgptr;
        canvas_display_list 		*cdptr, *tcdptr;
	int 				i,gnarray,ier, tmp = -99;
        char 				a_name[6][17];
	int 				graphics_num_of_arrays();
	extern void 			remove_vcs_legacy_connection_information(Gconid_X_drawable connect_id, int wkst_id);
	extern int 			removeA(char *a_name);
	extern int              	removeGfb_name();
	extern int 			clear_display();
	extern int 			shutdown(Gconid_X_drawable connect_id, int wks);
	extern void 			vcs_legacy_canvas_quit_cb();
        extern void		        dispatch_the_next_event();
	extern Gint gdacwk(Gint ws_id);
	extern void gclwk( Gint ws_id );

#ifdef VCSQT
	extern int vcs_legacy_close_Qt_window(int index);
#endif
        /* Keep track of how many VCS Canvases that are opened. There can
         * only be (at most) 8 opened at any given time. Decrement the 
         * vcs_legacy open counter.
         */
        --vcs_legacy_open_ct;
        if (vcs_legacy_open_ct < 0) vcs_legacy_open_ct = 0;

        /* If the VCS Canvas is not open, then return. */
#ifdef USEX11
        if (self->connect_id.drawable == 0) {
#else
	  if (self->connect_id.cr == NULL) {
#endif
           PyErr_SetString(PyExc_TypeError, "Must first open VCS (i.e., x.open()).");
           PyVCS_UNBLOCK_X_SERVER(self, args); /* Restart the X main loop! */
  	   return NULL;
	}

	/* Set up the VCS Canvas and XGKS workstation */
	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	/* If the GUI was not stated (i.e., cdatgui), then we need to
	 * process all the X events before we move on.
	 */
	if (not_using_gui)
	   process_cdat_events();


        setup_canvas_globals(self);

        /* Popdown VCS canvas and free the connection information used for animation */
	if (self->vcs_legacy_gui != 1) {
	   vcs_legacy_canvas_quit_cb(self->connect_id);
	   remove_vcs_legacy_connection_information(self->connect_id,self->wkst_id);
        }

#ifdef VCSQT
	vcs_legacy_close_Qt_window(self->wkst_id);
#endif

	/* Remove the display from the VCS picture form and
	 * remove all the data from the VCS data table 
         */
  	cdptr = self->dlist;
	while (cdptr != NULL) {
	   dtab=&D_tab;
           while ((dtab != NULL) &&
                  (strcmp(dtab->name, cdptr->display_name) != 0))
                 dtab = dtab->next;
	   if (dtab == NULL) break;/* must have been removed from animation */
	   gnarray = graphics_num_of_arrays(dtab->type);
	   for (i=0; i<gnarray; i++)
               strcpy(a_name[i], dtab->a[i]);
	   clear_display(cdptr->display_name);
	   for (i=0; i<gnarray; i++)
	      removeA(a_name[i]);
	   tcdptr = cdptr;
	   cdptr = cdptr->next;
           free((char *) tcdptr->display_name);
           free((char *) tcdptr);
	}
  	self->dlist = NULL;
        dispatch_the_next_event();

	/* If the GUI was not stated (i.e., cdatgui), then we need to
	 * process all the X events before we move on.
	 */
	if (not_using_gui)
	   process_cdat_events();

  	/* Remove the temporary graphics methods used to create set 
        * minimum and maximum plots. 
        */
	gptr = self->glist;
	while (gptr != NULL) {
	    tgptr = gptr;
	    gptr = gptr->next;
	    if (strcmp(tgptr->g_type, "Boxfill") == 0)
	       removeGfb_name(tgptr->g_name);
	    free((char *) tgptr->g_type);
	    free((char *) tgptr->g_name);
	    free((char *) tgptr);
	}
	self->glist = NULL;

        /* Remove the backing store pixmap */
#ifdef X11WM
        if (self->connect_id.canvas_pixmap != (Pixmap) NULL) {
              XFreePixmap(self->connect_id.display, self->connect_id.canvas_pixmap);
              self->connect_id.canvas_pixmap = (Pixmap) NULL;
        }
#elif defined QTWM 
	/* nothing to do for Qt */
#else
	fprintf(stderr,"insert your WM pixmap backing store clean up here\n");
#endif
        /* Free all resouces associated with the VCS Canvas popup window.
	 * That is, destroy the VCS Canvas object window and all of its
	 * popup descendants and widgets. Then free all resources associated
	 * with the VCS Canvas popup window and its descendants.
	 */
	        /* Shut down the xgks workstation */
        if (self->vcs_legacy_gui != 1) {
#ifdef X11WM
	  if (self->connect_id.drawable != 0) {
	    self->connect_id.drawable = (XID) NULL;
#else
	    if (self->connect_id.cr != NULL ) {
#endif
	    shutdown(self->connect_id, self->wkst_id);
	  }
	}
	/* Set the flag to 0, meaning the VCS canvas has been created,
         * but is closed.
         */
	self->virgin = 0;

	/* deactivate and close the workstation */
        gdacwk( self->wkst_id );
        gclwk( self->wkst_id );

        setup_canvas_globals(self);

	/* Return NULL Python Object */
	Py_INCREF (Py_None);
  	return Py_None;
}

/*
 * This function is used mainly for the updating of the static color visuals (i.e., TrueColor).
 * But can be used to update or redraw the VCS Canvas. It will go through the entire picture
 * template and set the flag to redraw every segment on the plot (or in some cases multiple
 * plots).
 */
 PyObject *PyVCS_updateVCSsegments( PyVCScanvas_Object *self, PyObject *args)
{
        int                             	MODE, hold_continents;
        static int				in_process = 0;
        canvas_display_list             	*cdptr;
        struct display_tab              	*dtab;
        extern struct display_tab       	D_tab;
        struct a_attr 				*pa;
        struct a_tab            		*ptab;
        extern struct a_tab     		A_tab;
        extern int                     	 	update_ind;
        extern int                      	vcs_legacy_canvas_update();
#ifdef X11WM
	extern Pixmap 				copy_pixmap(Gconid_X_drawable connect_id,int canvas_id);
#endif
	extern struct default_continents 	Dc;

        /* If visual class is PseudoColor, then return because the colormap
         * is changed dynamically.
         */

#ifdef X11WM
        if (visual->class == PseudoColor) {
           Py_INCREF (Py_None);
           return Py_None;
	}
#elif defined QTWM 
	/* nothing to do for Qt */
#else
	fprintf(stderr,"insert here your WM pseudo color colormap thing\n");
#endif
        /* Indicate that the  data segment has been changed by the color table. */
        cdptr = self->dlist;
        while (cdptr != NULL) {
           dtab=&D_tab;
           while ((dtab != NULL) &&
                  (strcmp(dtab->name, cdptr->display_name) != 0))
                 dtab = dtab->next;
	   if (dtab != NULL) {
	      dtab->F_seg[3]=1; /* update the file segment */
	      dtab->f_seg[3]=1; /* update the function segment */
	      dtab->lmask_seg[3]=1; /* update the logical mask segment */
	      dtab->trnf_seg[3]=1; /* update the tranformation segment */
	      dtab->s_seg[3]=1; /* update the source segment */
	      dtab->n_seg[3]=1; /* update the name segment */
	      dtab->ti_seg[3]=1; /* update the title segment */
	      dtab->u_seg[3]=1; /* update the units segment */
	      dtab->crd_seg[3]=1; /* update the date segment */
	      dtab->crt_seg[3]=1; /* update the time segment */
	      dtab->com1_seg[3]=1; /* update the  command 1segment */
	      dtab->com2_seg[3]=1; /* update the command 2 segment */
	      dtab->com3_seg[3]=1; /* update the command 3 segment */
	      dtab->com4_seg[3]=1; /* update the command 4 segment */
	      dtab->xn_seg[3]=1; /* update the x name segment */
	      dtab->yn_seg[3]=1; /* update the y name segment */
	      dtab->zn_seg[3]=1; /* update the z name segment */
	      dtab->tn_seg[3]=1; /* update the t name segment */
	      dtab->xu_seg[3]=1; /* update the x unit segment */
	      dtab->yu_seg[3]=1; /* update the y unit segment */
	      dtab->zu_seg[3]=1; /* update the z unit segment */
	      dtab->tu_seg[3]=1; /* update the t unit segment */
	      dtab->xv_seg[3]=1; /* update the x coordinate segment */
	      dtab->yv_seg[3]=1; /* update the y coordinate segment */
	      dtab->zv_seg[3]=1; /* update the z coordinate segment */
	      dtab->tv_seg[3]=1; /* update the t coordinate segment */
	      dtab->mean_seg[3]=1; /* update the mean segment */
	      dtab->max_seg[3]=1; /* update the maximum segment */
	      dtab->min_seg[3]=1; /* update the minimum segment */
	      dtab->xt1_seg[3]=1; /* update the left major ticks segment */
	      dtab->xt2_seg[3]=1; /* update the  right major ticks segment */
	      dtab->xmta_seg[3]=1; /* update the left minor ticks segment */
	      dtab->xmtb_seg[3]=1; /* update the right minor ticks segment */
	      dtab->yt1_seg[3]=1; /* update the bottom major ticks segment */
	      dtab->yt2_seg[3]=1; /* update the top major ticks segment */
	      dtab->ymta_seg[3]=1; /* update the bottom minor ticks segment */
	      dtab->ymtb_seg[3]=1; /* update the top minor ticks segment */
	      dtab->xl1_seg[3]=1; /* update the left line  segment */
	      dtab->xl2_seg[3]=1; /* update the right line segment */
	      dtab->yl1_seg[3]=1; /* update the bottom line segment */
	      dtab->yl2_seg[3]=1; /* update the top line segment */
	      dtab->b1_seg[3]=1; /* update the box 1 segment */
	      dtab->b2_seg[3]=1; /* update the box 2 segment */
	      dtab->b3_seg[3]=1; /* update the box 3 segment */
	      dtab->b4_seg[3]=1; /* update the box 4 segment */
	      dtab->l1_seg[3]=1; /* update the line 1 segment */
	      dtab->l2_seg[3]=1; /* update the line 2 segment */
	      dtab->l3_seg[3]=1; /* update the line 3 segment */
	      dtab->l4_seg[3]=1; /* update the line 4 segment */
	      dtab->leg_seg[3]=1; /* update the legend  segment */
	      dtab->dsp_seg[3]=1; /* update the display  segment */
	   }
           cdptr = cdptr->next;
        }

        if ((args !=NULL) && PyArg_ParseTuple(args,"|i", &MODE)) {
	   if ( (MODE) && (in_process == 0) ) {

              /* Determine if the Continents need to be displayed or not. */
              ptab=&A_tab;
	      pa=ptab->pA_attr;
                 hold_continents = Dc.selected;
              if (pa != NULL) { /* If pa is NULL then do nothing. Check needed for the Mac. */
                 while ((ptab != NULL) && (strcmp(ptab->name, dtab->name+4) != 0))
                    ptab=ptab->next;

                 if (ptab != NULL) { /* If ptab is NULL, then do nothing. Check needed for the Mac. */
/*                     Py_INCREF (Py_None); */
/*                     return Py_None; */
                 
	         pa=ptab->pA_attr;
                 hold_continents = Dc.selected;
                 if ( (pa != NULL) && ( (!doexist("longitude",pa->XN[0])) || (!doexist("latitude",pa->XN[1])) ) )
		   Dc.selected = 0; /* When doing template editing always do the simplest continents for speed.  When done editing the original template will be redrawn. */
		 }
	      }

                 /* Update the display if needed */
                 in_process = 1;      /* set the flag to stop this function from processing from another part of the code. This is a threads issue */
                 update_ind = MODE; 
	         vcs_legacy_canvas_update(0);
                 in_process = 0;

                 Dc.selected = hold_continents; /* Restore Continent's flag */
#ifdef X11WM
                 /* Copy the current VCS canvas to the pixmap (i.e., backing_store) */
                 if (self->connect_id.canvas_pixmap != (Pixmap) NULL) {
                     XFreePixmap(self->connect_id.display, self->connect_id.canvas_pixmap);
                     self->connect_id.canvas_pixmap = (Pixmap) NULL;
                 }
                 self->connect_id.canvas_pixmap = copy_pixmap(self->connect_id, self->canvas_id);
#elif defined QTWM 
	/* nothing to do for Qt */
#else
		 fprintf(stderr,"insert here your WM cpy to backing stor pixmap func\n");
#endif
	   }
	}

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Change the VCS Canvas orientation to either Portrait or Landscape. */
static PyObject *
PyVCS_orientation(PyVCScanvas_Object *self, PyObject *args)
{
	int 				 ier, hold_continents;
        void                             display_resize_plot();
        int                             undisplay_resize_plot();
        extern struct default_continents Dc;
	extern void 			 set_up_canvas();
	extern int change_orientation(char *type, Gconid_X_drawable **connect_id_in, int where_from);

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

        /* If the VCS Canvas is not open, then return. */
#ifdef USEX11
        if (self->connect_id.drawable == 0) {
#elif defined CAIRODRAW
	  if (self->connect_id.cr == NULL) {
#endif
           PyErr_SetString(PyExc_TypeError, "Must first open VCS (i.e., x.open()).");
           return NULL;
        }

	if (self->vcs_legacy_gui == 1) {  /* Check for VCS canvas */
           PyErr_SetString(PyExc_TypeError, "Can not change page orientation for main VCS Canvas.");
  	   return NULL;
	}

	/* If the GUI was not stated (i.e., cdatgui), then we need to
	 * process all the X events before we move on.
	 */
	if (not_using_gui)
	   process_cdat_events();

	/* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

        /* Use the continents type of the original plot*/
        hold_continents = Dc.selected;

#ifdef X11WM
        if ( (self->connect_id.display != NULL) && (self->connect_id.drawable != 0) )
           XRaiseWindow(self->connect_id.display, self->connect_id.drawable);
#elif defined QTWM
	if (self->connect_id.cr!=NULL)
	  vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#else
	fprintf(stderr,"insert here your raise win func\n");
#endif

	/* Change the VCS Canvas orientation and set object flags */
	if (self->orientation == 0) {
	   self->orientation = 1;
	   set_up_canvas(self->connect_id, "portrait");
	   ier = change_orientation("portrait", &self->connect_id, 3);
	} else {
	   self->orientation = 0;
	   set_up_canvas(self->connect_id, "landscape");
	   ier = change_orientation("landscape", &self->connect_id, 3);
      	}

        /* Set up the magnification table, used for animation */
        setup_the_magnify_table();

        
        display_resize_plot( self,undisplay_resize_plot( self ) );
        Dc.selected = hold_continents; /* Restore continent's flag */

	/* Return NULL Python Object */
   	Py_INCREF(Py_None);
  	return Py_None;
}

/* Set up the VCS Canvas geometry by giving the canvas width, height,
 * x-position, and y-position.
 */
static PyObject *
PyVCS_geometry( PyVCScanvas_Object *self, PyObject *args)
{
	PyObject        *obj;
	int 		ier, first=1, i, argc, y, screen_num;
	int 		width, height, xpos, ypos;
	char 		buf[1024];
	extern void     reset_canvas_geometry();
	extern void	return_canvas_geometry();

	if (self->vcs_legacy_gui == 1) {  /* Check for VCS canvas */
           PyErr_SetString(PyExc_TypeError, "Can not change geometry for main VCS Canvas.");
  	   return NULL;
	}

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

        /* If the VCS Canvas is not open, then do nothing */
#ifdef USEX11
        if (self->connect_id.drawable == 0) {
#else
	  if (self->connect_id.cr == NULL) {
#endif
           PyErr_SetString(PyExc_TypeError, "Must have VCS Canvas opened before setting the geometry.");
           return NULL;
	}

	/* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

#ifdef USEX11
	screen_num = DefaultScreen(self->connect_id.display);
        height=y=DisplayHeight(self->connect_id.display,screen_num);
        width=DisplayWidth(self->connect_id.display,screen_num);
#elif defined QTWM
	extern  void vcs_legacy_Qt_get_desktop_dimensions(int index,int *x, int *y, int *w,int *h);
	vcs_legacy_Qt_get_desktop_dimensions(self->connect_id.wkst_id,&xpos, &ypos, &width,&height);
	y=height;
#else
	fprintf(stderr,"insert here your WM get desktop dims\n");
#endif
        if (self->orientation == 0) {
/*           width=0.60*width; */
/*           height=0.76*width; */
	  height = 0.568359375 * height;
	  width = 1.3127035830618892 * width;
        } else if (self->orientation == 1) {
/*           width=0.48*width; */
/*           height=width/0.76; */
          height=0.7880859375 * height;
          width=0.761786600496278 * width;
        }
	xpos = 2;
	ypos = y-height-30;

  	/* Parse the input argument string */
	/* commented out the null args because python won't let you pass it anyway */
/*   	if (args == NULL) { /\* check for no input *\/ */
/* 	   return_canvas_geometry(self->connect_id, */
/*                                   &width, &height, &xpos, &ypos); */
/*            sprintf(buf, "Info - No arguments given. Using %dx%d+%d+%d as the canvas geometry.", width, height, xpos, ypos); */
/*            PyErr_SetString(PyExc_TypeError, buf); */
/*            return NULL; */
/*         }  */
	if (!PyTuple_Check (args)) { /* check to see if it's Tuple */
           PyErr_SetString(PyExc_TypeError, "Arguments are incorrect.");
           return NULL;
	} else { /* get the geometry */
	   argc = PyTuple_Size (args); /* get the number of arguments */
/*   	   if (argc == 0) { /\* check for no input *\/ */
/* 	      return_canvas_geometry(self->connect_id, */
/*                                      &width, &height, &xpos, &ypos); */
/*               sprintf(buf, "Info - No arguments given. Using %dx%d+%d+%d as the canvas geometry.", width, height, xpos, ypos); */
/*               PyErr_SetString(PyExc_TypeError, buf); */
/*               return NULL; */
/*            } */
	   for (i = 0; i < argc; i++) {
	       obj = PyTuple_GetItem (args, i); /* get argument */
               if(PyInt_Check(obj)) { /*check integer*/
                  if (i == 0)
                     width = (int) PyInt_AsLong(obj);
                  else if (i == 1)
                     height = (int) PyInt_AsLong(obj);
                  else if (i == 2) {
		    y = (int) PyInt_AsLong(obj);
		    if (y!=-999) xpos = y;
		  }
                  else if (i == 3) {
		    y = (int) PyInt_AsLong(obj);
		    if (y!=-999) ypos = y;
		  }
               } else {
                  if (i == 0)
                     sprintf(buf, "Error - Incorrect argument. Using %d as the canvas width.", width);
                  else if (i == 1)
                     sprintf(buf, "Error - Incorrect argument. Using %d as the canvas height.", height);
                  else if (i == 2)
                     sprintf(buf, "Error - Incorrect argument. Using %d for the canvas x-position.", xpos);
                  else if (i == 3)
                     sprintf(buf, "Error - Incorrect argument. Using %d for the canvas y-position.", ypos);
                  pyoutput(buf, 1);
	       }
               ++first;
	   }
	}

	reset_canvas_geometry(self->connect_id, 
		width, height, xpos, ypos);

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Return the current VCS Canvas window attributes.
 */
static PyObject *
PyVCS_canvasinfo(PyVCScanvas_Object *self, PyObject *args)
{

        int screen_x, screen_y; /* these variables will eventually hold the translated window coordinates */
#ifdef X11WM
        XWindowAttributes 	xwa;
        Window child_win; /* this variable is needed by the XTranslateCoordinates function below */
        Window parent_win; /* variable will store the ID of the parent window of our window */
        Window root_win; /* variable will store the ID of the root window of the screen    */
        Window* child_windows; /*  variable will store an array of IDs of the child windows of our window */
#else
	Grectangle xwa,swa;
#ifdef CAIRODRAW
	cairo_format_t cairofmt;
#endif
#endif
	int map_state;
	int depth;
        unsigned int num_child_windows; /* variable will store the number of child windows of our window */
        extern int XW,YW;

        /* Check to see if vcs_legacy has been initalized */
        if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
           return NULL;
        }

        /* If the VCS Canvas is not open, then do nothing */
#ifdef X11WM
        if (self->connect_id.drawable == 0) {
#else
	  if (self->connect_id.cr == NULL) {
#endif
              /* No Canvas up, will send bg dims instead */

           /* PyErr_SetString(PyExc_TypeError, "Must have VCS Canvas opened before querying window information."); */
           /* return NULL; */
            return Py_BuildValue("{s:i, s:i, s:i, s:i, s:i s:i}", "width",XW, "height",YW, "depth",0, "mapstate",0, "x",0, "y",0);
        }
#ifdef X11WM
        /* query the window's attributes */
        XGetWindowAttributes(self->connect_id.display, 
                                self->connect_id.drawable, &xwa);

        /* Make the query for the above values. */
        XQueryTree(self->connect_id.display, self->connect_id.drawable,
           &root_win,
           &parent_win,
           &child_windows, &num_child_windows);

        /* We need to free the list of child IDs, as it was dynamically allocated */
        /* by the XQueryTree function.                                            */
        XFree(child_windows);

        /* next, we make the coordinates translation, from the coordinates system */
        /* of the parent window, to the coordinates system of the root window,    */
        /* which happens to be the same as that of the screen, since the root     */
        /* window always spans the entire screen size.                            */
        /* the 'x' and 'y' values are those previously returned by the            */
        /* XGetWindowAttributes function.                                         */
        XTranslateCoordinates(self->connect_id.display, parent_win, root_win,
                      xwa.x, xwa.y, &screen_x, &screen_y,
                      &child_win);
	depth = xwa.depth;
	map_state = xwa.map_state;
#elif defined QTWM
	vcs_legacy_Qt_get_window_dimensions_by_id(self->connect_id.wkst_id,&xwa.x,&xwa.y,&xwa.width,&xwa.height);
	vcs_legacy_Qt_get_window_visibility_by_id(self->connect_id.wkst_id,&map_state);
	extern  void vcs_legacy_Qt_get_desktop_dimensions(int index,int *x, int *y, int *w,int *h);
	vcs_legacy_Qt_get_desktop_dimensions(self->connect_id.wkst_id,&swa.x,&swa.y,&screen_x,&screen_y);
#ifdef CAIRODRAW
	if (cairo_surface_get_type(self->connect_id.surface) == CAIRO_SURFACE_TYPE_IMAGE) {
	  cairofmt = cairo_image_surface_get_format(self->connect_id.surface);
	  switch (cairofmt) {
	  case CAIRO_FORMAT_ARGB32:
	    depth = 32;
	    break;
	  case CAIRO_FORMAT_RGB24:
	    depth=24;
	    break;
	  case CAIRO_FORMAT_A8:
	    depth = 8;
	    break;
	  case CAIRO_FORMAT_A1:
	    depth = 1;
	    break;
	  default:
	    depth=0;
	}
	}
	else {
	  depth = 0;
	}
#else
	depth = 0;
#endif
#else
	fprintf(stderr,"insert here your WM winidow dims, screen dims, depth, visibility funcs\n");
	xwa.width=0;
	xwa.height=0;
	screen_x=0;
	screen_y=0;
	depth=0;
	map_state=0;
#endif
        return Py_BuildValue("{s:i, s:i, s:i, s:i, s:i s:i}", "width",xwa.width, "height",xwa.height, "depth",depth, "mapstate",map_state, "x",screen_x, "y",screen_y);
}

/* List the primary attributes set names for: template, graphics methods,
 * or data. This routine will also list the secondary attribute set names
 * for: colormap, fill area, format, line, marker,list, text, or text 
 * orientation.
 */
static PyObject *
PyVCS_show(PyVCScanvas_Object *self, PyObject *args)
{
	int ier;
	char *element=NULL;
	extern int python_list_element();

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

  	if(PyArg_ParseTuple(args, "|s", &element)) {
	   if ((element == NULL) || (element[0] == '\0')) {
	      PyErr_SetString(PyExc_TypeError, "No primary or secondary element given.");
              return NULL;
	   } else {
	      ier = python_list_element(element);
	   }
	}

	/* Return NULL Python Object */
   	Py_INCREF(Py_None);
  	return Py_None;
}

/* 
 * 
 * At start-up, VCS reads a script file named initial.attributes that
 * defines the initial appearance of the VCS Interface. Although not
 * required to run VCS, this initial.attributes file contains many
 * predefined settings to aid the beginning user of VCS. The path to
 * the file must be: 
 *
 *       /$HOME/PCMDI_GRAPHICS/initial.attributes 
 *
 * The contents of the initial.attributes file can be customized by
 * the user.
 */
static PyObject *
PyVCS_saveinitialfile(PyObject *self, PyObject *args)
{
	int mode=0777, ffd, wfd;
    	char *base_dir, dirbase[1024], buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
	extern int replace_init();
	FILE *fp;

	/*
         * Find a base directory.
         */
        if ((base_dir=getenv(DOT_DIRECTORY_ENV)) == NULL) {
           if ((base_dir=getenv("HOME")) == NULL || strlen(base_dir) ==0) {
               strcpy(dirbase,"./");
               strcat(dirbase,DOT_DIRECTORY);
           }
           else {
              strcpy(dirbase,base_dir);
              strcat(dirbase,"/");
              strcat(dirbase,DOT_DIRECTORY);
           }
        } else
          strcpy(dirbase,base_dir);
        base_dir=dirbase;
        if (mkdir(base_dir,mode) != 0 && errno != EEXIST)
          {
              strcpy(dirbase,"Error - you don't have a base directory.\nThe environment variable ");
              strcat(dirbase,DOT_DIRECTORY_ENV);
              strcpy(dirbase,"\nor HOME needs to be set!");
           PyErr_SetString(PyExc_ValueError, dirbase);
           return NULL;
          }

	/* 
   	 * Get the PCMDI directory and set replacement name
         */
        strcpy(replace_name, base_dir);
        strcat(replace_name, "/initial.attributes");
        strcpy(initial_script, replace_name);
        strcat (replace_name, "%");

       /* Set up the move command */
       sprintf(mv_command, "/bin/mv %s %s", initial_script, replace_name);

       /* check directory for access */
       ffd = access(initial_script, F_OK);  
       wfd = access(initial_script, W_OK);

       if ((ffd == 0) && (wfd == 0)) { /* The file exist! */
             /* Move the existing file to a new file */
          if ((system (mv_command)) != 0) {
              PyErr_SetString(PyExc_ValueError, "Error - In replacing initial.attributes script file.");
              return NULL;
          }
       }

       /* Create the new initial.attributes script file */
       if ((fp=fopen(initial_script,"w")) == NULL) {
           sprintf(buf, "Error - cannot create file (%s) -\n initial.attributes script file was not created.",initial_script);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
       } else {
           replace_init (fp);
           sprintf(buf, "The old script file\n%s\n has been moved to \n%s.\n",
               initial_script, replace_name);
       }
       fclose(fp);

       return Py_BuildValue("s", buf);
}

/* 
 * The VCS scripting capability serves many purposes. It allows one to save the
 * system state for replay in a later session; to save primary and secondary
 * element attributes for use in later visual presentations; to save a sequence
 * of interactive operations for replay; or to recover from a system failure. 
 */
static PyObject *
PyVCS_scriptstate(PyObject *self, PyObject *args)
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL;
    	char *base_dir, dirbase[1024], buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
	extern int replace_init();
	FILE *fp;

        if(PyArg_ParseTuple(args,"|s", &SCRIPT_NAME)) {
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);

        if ((ffd == 0) && (wfd == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,"w")) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
           dump (fp);
           fclose(fp);
           sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
           return Py_BuildValue("s", buf);
        }
}

/* 
 * Map the VCS Canvas window on top of all its siblings.
 */
static PyObject *
PyVCS_canvasraised(PyVCScanvas_Object *self, PyObject *args)
{
        extern void		        dispatch_the_next_event();

	/* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

#ifdef X11WM
        /* Make sure there is no other window in front of the VCS Canvas */
        if ((self->connect_id.display != NULL) &&
           (self->connect_id.drawable != 0)) {
           XMapRaised(self->connect_id.display, self->connect_id.drawable);
           XFlush( connect_id.display );
           XSync( connect_id.display, FALSE );
        }
#elif defined QTWM
	vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#else
	fprintf(stderr,"insert here your win raise func\n");
#endif
        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 *  Returns 1 if a VCS Canvas is displayed on the screen. Returns a 0 if no VCS Canvas
 *  is displayed on the screen.
 */
static PyObject *
PyVCS_iscanvasdisplayed(PyVCScanvas_Object *self, PyObject *args)
{
#ifdef X11WM
        if (self->connect_id.drawable == 0)
#else
	  if (self->connect_id.cr == NULL )
#endif
           return Py_BuildValue("i", 0);
	else
           return Py_BuildValue("i", 1);
}

#include "pyembed.h"
#include <stdarg.h>
int
Convert_Result(PyObject *presult, char *resFormat, void *resTarget)
{
    if (presult == NULL)            /* error when run? */
        return -1;
    if (resTarget == NULL) {        /* NULL: ignore result */
        Py_DECREF(presult);         /* procedures return None */
        return 0;
    }
    if (! PyArg_Parse(presult, resFormat, resTarget)) { /* convert Python->C */
        Py_DECREF(presult);                             /* may not be a tuple */
        return -1;                                      /* error in convert? */
    }
    if (strcmp(resFormat, "O") != 0)       /* free object unless passed-out */
        Py_DECREF(presult);
    return 0;                              /* 0=success, -1=failure */
}

/* 
 * Get or fetch a data member (attribute) from a known object by name.
 * This function can take all the common Python/C data conversion 
 * types: { "s" = char * : "i" = int   : "l" = long :
 *          "c" = char   : "f" = float : "d" = double:
 *          "O" = PyObject * .
 *
 */
int
Get_Member(PyObject *pobject, char *attrname,
               char *resfmt,  void *cresult)            /* convert to c/c++ */
{
    PyObject *pmemb;                                    /* "pobject.attrname" */
    if (!Py_IsInitialized())
       Py_Initialize();
    pmemb = PyObject_GetAttrString(pobject, attrname);  /* incref'd */
    return Convert_Result(pmemb, resfmt, cresult);      /* do getargs, decref */
}


/* 
 * Assign a data member (attribute) of a known object by name.
 * This function can take all the common Python/C data conversion 
 * types: { "s" = char * : "i" = int   : "l" = long :
 *          "c" = char   : "f" = float : "d" = double:
 *          "O" = PyObject * .
 *
 */
/* Commented out by C.Doutriaux on 07/28, doesn't seem to be used anywhere...*/
/* int */
/* Set_Member(PyObject *pobject, char *attrname, */
/*                char *argfmt,  ... /\* arg, arg... *\/ ) /\* convert to python *\/ */
/* { */
/*     int result; */
/*     PyObject *pval; */
/*     va_list argslist;                             /\* "pobject.attrname = v" *\/ */
/*     va_start(argslist, argfmt); */
/*     if (!Py_IsInitialized()) */
/*        Py_Initialize();                              /\* init if first time *\/ */
/*     pval = Py_VaBuildValue(argfmt, argslist);     /\* input: C->Python *\/ */
/*     if (pval == NULL) */
/*         return -1; */
/*     result = PyObject_SetAttrString(pobject, attrname, pval);     /\* setattr *\/ */
/*     Py_DECREF(pval); */
/*     return result; */
/* }  */

/*
 * Return the VCS display plot (Dp) class member value.
 */
static PyObject *
PyVCS_getDpmember(PyVCScanvas_Object *self, PyObject *args)
{
	int				i;
        char                            *Dp_name, *member=NULL, buf[1024];
        PyObject                        *DP=NULL, *MEMBER=NULL, *listptr=NULL;
        struct display_tab              *dtab;
	extern struct display_tab 	D_tab;

        if(PyArg_ParseTuple(args,"|OO",&DP, &MEMBER)) {
           if (DP == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
        }

        Get_Member(DP,"name", "s", &Dp_name);
        dtab=&D_tab;
        while ((dtab != NULL) && (strcmp(dtab->name, Dp_name) != 0))
              dtab = dtab->next;

        if (dtab == NULL) {
           sprintf(buf,"Cannot find display plot object Dp_%s.",Dp_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        if (cmpncs(member, "off") == 0) {
           return Py_BuildValue("i", dtab->off);
        } else if (cmpncs(member, "priority") == 0) {
           return Py_BuildValue("i", dtab->pri);
        } else if (cmpncs(member, "continents") == 0) {
           return Py_BuildValue("i", dtab->continents);
        } else if (cmpncs(member, "template") == 0) {
           return Py_BuildValue("s", dtab->p_name);
        } else if (cmpncs(member, "g_type") == 0) {
           return Py_BuildValue("s", dtab->type);
        } else if (cmpncs(member, "g_name") == 0) {
           return Py_BuildValue("s", dtab->g_name);
        } else if (cmpncs(member, "_template_origin") == 0) {
           return Py_BuildValue("s", dtab->p_orig_name);
        } else if (cmpncs(member, "array") == 0) {
           listptr = PyList_New(dtab->na);
           for (i=0; i<dtab->na; i++)
              PyList_SetItem(listptr, i, Py_BuildValue("s", dtab->a[i]));
           return listptr;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing display plot object and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the line  object's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setDpmember(PyObject *self, PyObject *args)
{
	int 				i, ier, *pi, MODE, value_int;
	float				value_float;
        char 				*Dp_name, *member=NULL, buf[1024];
        PyObject 			*DP=NULL, *MEMBER=NULL, *VALUE=NULL;
        PyObject                        *l_array=NULL,*b_list=NULL;
        struct display_tab              *dtab=NULL;
        struct a_tab                    *ptab=NULL;
        extern struct display_tab       D_tab;
        extern struct a_tab 		A_tab;
	extern int              	update_ind;
	/*extern int              	chk_mov_Dp();*/
	void 				put_slab_in_VCS_data_struct();
	extern int 			vcs_legacy_canvas_update();
        PyObject			*cuslab_name_obj;
        char				*cuslab_name;

        if(PyArg_ParseTuple(args,"|OOOi", &DP, &MEMBER, &VALUE, &MODE)) {
           if (DP == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }
        cuslab_name_obj = NULL;  /*avoid compiler warning */
        Get_Member(DP,"name", "s", &Dp_name);
        heartbeat("dpgetmember start %s", Dp_name);
        dtab=&D_tab;
        while ((dtab != NULL) && (strcmp(dtab->name, Dp_name) != 0))
              dtab = dtab->next;

        if (dtab == NULL) {
           sprintf(buf,"Cannot find display plot object Dp_%s.",Dp_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	/*
	 * Set the appropriate display plot object attribute.
         */
        heartbeat("dpgetmember member %s", member);
	if (cmpncs(member, "off") == 0) {
	  dtab->off = (int) PyInt_AsLong(VALUE);
        } else if (cmpncs(member, "priority") == 0) {
	  dtab->pri = (int) PyInt_AsLong(VALUE);
        } else if (cmpncs(member, "continents") == 0) {
	  dtab->continents = (int) PyInt_AsLong(VALUE);
        } else if (cmpncs(member, "template") == 0) {
	  strcpy(dtab->p_name,PyString_AsString(VALUE));
        } else if (cmpncs(member, "g_type") == 0) {
	  strcpy(dtab->type,PyString_AsString(VALUE));
        } else if (cmpncs(member, "g_name") == 0) {
	  strcpy(dtab->g_name,PyString_AsString(VALUE));
        } else if (cmpncs(member, "_template_origin") == 0) {
	  strcpy(dtab->p_orig_name,PyString_AsString(VALUE));
        } else if (cmpncs(member, "array") == 0) {
           b_list=PyList_New(PySequence_Size(VALUE)); 
                  /* return new Python list *//*size wrong?? I changed it DUBOIS */
           if(PyErr_Occurred()) return NULL;
           for (i=0; i<PyList_Size(VALUE); i++) {
              cuslab_name_obj = NULL;
              l_array=PyList_GetItem(VALUE, i);
              if (PyErr_Occurred()) goto err;
              if (PyString_Check(l_array)) {      /* get the slab name */
                 strcpy(dtab->a[i], PyString_AsString(l_array));
                 PyList_SetItem(b_list, i, Py_BuildValue("s", dtab->a[i]));
                 if(PyErr_Occurred()) goto err;
              } else if (slabCheck(l_array)) {
                 cuslab_name =  slabAttribute(l_array, "cuslab_name", "cuslab_name_error");
                 if(PyErr_Occurred()) goto err; 
                 ptab=&A_tab;
                 while ((ptab != NULL) && (strcmp(ptab->name, cuslab_name) != 0)) {
                    ptab = ptab->next;
                 }
                 if (ptab != NULL) {    	/* get slab that already exist */
                    strcpy(dtab->a[i], cuslab_name);
                    PyList_SetItem(b_list, i, Py_BuildValue("s",dtab->a[i]));
                 } else {			/* create a new slab name */
                    sprintf(buf, "plot_%d", namecount);
                    ++namecount; /* for unique plot name */
                    cuslab_name = (char *)malloc(strlen(buf)+1);
                    strcpy(cuslab_name, buf);
		    heartbeat("dpsetmember about to call put_slab %s", cuslab_name);
                    cuslab_name_obj = PyString_FromString(cuslab_name);
                    put_slab_in_VCS_data_struct(l_array, dtab->type, cuslab_name_obj, 0, 1);
		    heartbeat("dpsetmember back from call put_slab %s", cuslab_name);
                    if (PyErr_Occurred()) {
                      goto err;
                    } else {
                      strcpy(dtab->a[i], cuslab_name);
                      PyList_SetItem(b_list, i, Py_BuildValue("s",dtab->a[i]));
                    }
                 }
              } else {
                 sprintf(buf,"Data object cannot be used in VCS.");
                 PyErr_SetString(PyExc_TypeError, buf);
                 return NULL;
              }
              Py_XDECREF(cuslab_name_obj);
           }
        }
        heartbeat("dpgetmember progress report %s", Dp_name)
	  if (strcmp(member,"_template_origin")!=0){
	    for (pi=&dtab->F_seg[0]; pi <= &dtab->dsp_seg[3]; pi+=4) {
	      if (*pi > 0)
		gdsg(*pi);
	      if (!dtab->off) {
		*pi=0;
		*(pi+1)=0;
		*(pi+2)=0;
		*(pi+3)=1;
	      }
	    }
	    update_ind = MODE; /* Update the display if needed */
	    vcs_legacy_canvas_update(0);
	  }
	if (b_list == NULL) {
           /* Return NULL Python Object */
           Py_INCREF(Py_None);
           return Py_None;
	} else
          return b_list; /* Return array list of names */
err:
   Py_XDECREF(cuslab_name_obj);
   return NULL;
}

/*
 * Rename an existing display plot object method.
 */
static PyObject *
PyVCS_renameDp(PyObject *self, PyObject *args)
{
        int             		ierr;
        char            		*DP_OLD_NAME=NULL, *DP_NEW_NAME=NULL;
	char				buf[1024];
        struct display_tab              *dtab;
        extern struct display_tab       D_tab;

        if(PyArg_ParseTuple(args,"|ss", &DP_OLD_NAME, &DP_NEW_NAME)) {
           if ((DP_OLD_NAME == NULL) || (DP_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new display object name.");
                 return NULL;
           }
        }

        dtab=&D_tab;
        while ((dtab != NULL) && (strcmp(dtab->name, DP_OLD_NAME) != 0))
              dtab = dtab->next;

        if (dtab == NULL) {
           sprintf(buf,"Cannot find display plot object Dp_%s.",DP_OLD_NAME);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }
        strcpy(dtab->name, DP_NEW_NAME);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Return the number of display plots that are on the VCS Canvas
*/
static PyObject *
PyVCS_return_display_ON_num(PyVCScanvas_Object *self, PyObject *args)
{
	int				on=0;
        struct display_tab              *dtab;
	extern struct display_tab 	D_tab;

        dtab=&D_tab;
        while (dtab != NULL) { 
              if (dtab->off == 0) on += 1;
              dtab = dtab->next; 
        }

        return Py_BuildValue("i", on); 
}

/*
 * Return the VCS Canvas display plot (Dp) list as a Python dictionary.
 */
static PyObject *
PyVCS_return_display_names(PyVCScanvas_Object *self, PyObject *args)
{
	int				i=0, list_size=0;
        PyObject                        *listptr=NULL;
        struct display_tab              *dtab;
	extern struct display_tab 	D_tab;

        dtab=&D_tab;
        while (dtab != NULL) { list_size += 1; dtab = dtab->next; }

	listptr = PyList_New( list_size );
        dtab=&D_tab;
        while (dtab != NULL) {
           PyList_SetItem(listptr, i, Py_BuildValue("s",dtab->name));
           dtab = dtab->next;
	   ++i;
        }

	return listptr;
}

/*
 * Remove the VCS Canvas display plot (Dp) element from the list.
 */
static PyObject *
PyVCS_remove_display_name(PyVCScanvas_Object *self, PyObject *args)
{
	VCSCANVASLIST_LINK              tvptr,vptr;
        canvas_display_list 		*cdptr, *tcdptr, *hcdptr;
	char                            *REMOVE_NAME=NULL;
	int				i, gnarray;
	int				graphics_num_of_arrays();
        char 				a_name[6][17];
        PyObject * 			PyVCS_clear(PyVCScanvas_Object *self, PyObject *args);
        struct display_tab              *dtab, *cdtab;
	extern struct display_tab 	D_tab;
	extern int 			removeA();
	extern int                      copy_disp();         

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide a display name.");
                 return NULL;
           }
        }

#ifdef X11WM
        if (self->connect_id.drawable != 0) {
#else
	  if (self->connect_id.cr != NULL) {
#endif
			           setup_canvas_globals(self);
	  }
	/* Remove the display from the VCS picture form and
	 * remove all the data from the VCS data table 
         */
  	hcdptr = cdptr = self->dlist;
	while ((cdptr != NULL) && (strcmp(cdptr->display_name, REMOVE_NAME) != 0)) {
	   tcdptr = cdptr;
	   cdptr = cdptr->next;
	}

	if (cdptr != NULL) {
	   cdtab=dtab=&D_tab;
           while ((dtab != NULL) && (strcmp(dtab->name, REMOVE_NAME) != 0)) {
		 cdtab = dtab;
                 dtab = dtab->next;
           }
   
	   if (dtab != NULL) {  /* remove display name */
	      gnarray = graphics_num_of_arrays(dtab->type);
	      for (i=0; i<gnarray; i++)
                  strcpy(a_name[i], dtab->a[i]);
              remove_display_name(self->connect_id, REMOVE_NAME); /*remove display name*/
	      for (i=0; i<gnarray; i++)                 /*from VCS Canvas info*/
	         removeA(a_name[i]);

	      if (cdptr == hcdptr)
	         self->dlist = cdptr->next;
	      else
		 tcdptr->next = cdptr->next;
              free((char *) cdptr->display_name);
              free((char *) cdptr);

	      /* Remove the display names and structure from memory. */
	      if (dtab == &D_tab) {
		 copy_disp(dtab->next, &D_tab);
	        /* Make sure everything is clear in the VCS Canvas */
		if ((dtab->next == NULL) && (D_tab.name[0] == '\0'))  PyVCS_clear(self,NULL);
	      } else {
		 cdtab->next = dtab->next;
                 free((char *) dtab);
	      }
	   }
        }

	/* Remove the canvas link list information used for animation */
	tvptr=vptr=head_canvas_list;
#ifdef X11WM
	while ((vptr != NULL) && (vptr->connect_id.drawable !=
               self->connect_id.drawable)) {
#elif defined QTWM
	while ((vptr != NULL) && (vptr->connect_id.cr !=
               self->connect_id.cr)) {
#endif
	     tvptr = vptr;
             vptr = vptr->next;
	}

	if ((tvptr != NULL) && (vptr != NULL)) {
           while (vptr != NULL) {
              if (cmpncs(vptr->d_name, REMOVE_NAME) == 0) {
		 if (vptr == head_canvas_list)
		    head_canvas_list = vptr->next;
		 else
	            tvptr->next = vptr->next;
                 if (vptr->slab != NULL) Py_DECREF(vptr->slab);
                 if (vptr->slab2 != NULL) Py_DECREF(vptr->slab2);
	         free((char *) vptr);
		 break;
	      }
              tvptr = vptr;
              vptr = vptr->next;
           }
	}


        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Return the VCS template orientation (Po) member value.
 */
static PyObject *
PyVCS_getPomember(PyVCScanvas_Object *self, PyObject *args)
{
        int			attribute=0;
        char 			*Pt_name, buf[1024];
        PyObject 		*PT=NULL, *MEMBER=NULL, *ATTRIBUTE;
        struct p_tab          	*pttab;
        extern struct p_tab   	Pic_tab;

        if(PyArg_ParseTuple(args,"|OO",&PT, &ATTRIBUTE)) {
           if (PT == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct template object type.");
                 return NULL;
           }
        }

        Get_Member(PT,"name", "s", &Pt_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pt_name) != 0))
           pttab = pttab->next;

     	if (pttab == NULL) {
	   sprintf(buf,"Cannot find template method P_%s.",Pt_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

        /* Return int Python Object */
        return Py_BuildValue("i",pttab->orientation_flg);
}

/*
 * Return the VCS template text (Pt) member value.
 */
static PyObject *
PyVCS_getPtmember(PyVCScanvas_Object *self, PyObject *args)
{
        char 			*Pt_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject 		*PT=NULL, *MEMBER=NULL, *ATTRIBUTE;
        struct p_tab          	*pttab;
        extern struct p_tab   	Pic_tab;
        extern float gnorm(int x_or_y, float value, int normalized_flg, int orientation);

        if(PyArg_ParseTuple(args,"|OOO",&PT, &MEMBER, &ATTRIBUTE)) {
           if (PT == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct template object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }


        Get_Member(PT,"name", "s", &Pt_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pt_name) != 0))
           pttab = pttab->next;

     	if (pttab == NULL) {
	   sprintf(buf,"Cannot find template method P_%s.",Pt_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "file") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->F.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->F.x=gnorm(0,pttab->F.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->F.y=gnorm(1,pttab->F.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->F.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->F.to);
 	} else if (cmpncs(member, "function") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->f.p); 
           else if (cmpncs(attribute, "x") == 0) 
              return Py_BuildValue("f",pttab->f.x=gnorm(0,pttab->f.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0) 
              return Py_BuildValue("f",pttab->f.y=gnorm(1,pttab->f.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->f.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->f.to);
        } else if (cmpncs(member, "logicalmask") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->lmask.p); 
           else if (cmpncs(attribute, "x") == 0) 
              return Py_BuildValue("f",pttab->lmask.x=gnorm(0,pttab->lmask.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0) 
              return Py_BuildValue("f",pttab->lmask.y=gnorm(1,pttab->lmask.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->lmask.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->lmask.to);
        } else if (cmpncs(member, "transformation") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->trnf.p); 
           else if (cmpncs(attribute, "x") == 0) 
              return Py_BuildValue("f",pttab->trnf.x=gnorm(0,pttab->trnf.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0) 
              return Py_BuildValue("f",pttab->trnf.y=gnorm(1,pttab->trnf.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->trnf.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->trnf.to);
        } else if (cmpncs(member, "source") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->s.p); 
           else if (cmpncs(attribute, "x") == 0) 
              return Py_BuildValue("f",pttab->s.x=gnorm(0,pttab->s.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0) 
              return Py_BuildValue("f",pttab->s.y=gnorm(1,pttab->s.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->s.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->s.to);
        } else if (cmpncs(member, "dataname") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->n.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->n.x=gnorm(0,pttab->n.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->n.y=gnorm(1,pttab->n.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->n.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->n.to);
        } else if (cmpncs(member, "title") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->ti.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->ti.x=gnorm(0,pttab->ti.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->ti.y=gnorm(1,pttab->ti.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->ti.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->ti.to);
        } else if (cmpncs(member, "units") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->u.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->u.x=gnorm(0,pttab->u.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->u.y=gnorm(1,pttab->u.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->u.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->u.to);
        } else if (cmpncs(member, "crdate") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->crd.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->crd.x=gnorm(0,pttab->crd.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->crd.y=gnorm(1,pttab->crd.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->crd.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->crd.to);
        } else if (cmpncs(member, "crtime") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->crt.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->crt.x=gnorm(0,pttab->crt.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->crt.y=gnorm(1,pttab->crt.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->crt.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->crt.to);
        } else if (cmpncs(member, "comment1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->com1.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->com1.x=gnorm(0,pttab->com1.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->com1.y=gnorm(1,pttab->com1.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->com1.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->com1.to);
        } else if (cmpncs(member, "comment2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->com2.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->com2.x=gnorm(0,pttab->com2.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->com2.y=gnorm(1,pttab->com2.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->com2.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->com2.to);
        } else if (cmpncs(member, "comment3") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->com3.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->com3.x=gnorm(0,pttab->com3.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->com3.y=gnorm(1,pttab->com3.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->com3.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->com3.to);
        } else if (cmpncs(member, "comment4") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->com4.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->com4.x=gnorm(0,pttab->com4.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->com4.y=gnorm(1,pttab->com4.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->com4.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->com4.to);
        } else if (cmpncs(member, "xname") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->xn.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->xn.x=gnorm(0,pttab->xn.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->xn.y=gnorm(1,pttab->xn.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->xn.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->xn.to);
        } else if (cmpncs(member, "yname") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->yn.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->yn.x=gnorm(0,pttab->yn.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->yn.y=gnorm(1,pttab->yn.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->yn.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->yn.to);
        } else if (cmpncs(member, "zname") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->zn.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->zn.x=gnorm(0,pttab->zn.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->zn.y=gnorm(1,pttab->zn.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->zn.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->zn.to);
        } else if (cmpncs(member, "tname") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->tn.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->tn.x=gnorm(0,pttab->tn.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->tn.y=gnorm(1,pttab->tn.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->tn.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->tn.to);
        } else if (cmpncs(member, "xunits") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->xu.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->xu.x=gnorm(0,pttab->xu.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->xu.y=gnorm(1,pttab->xu.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->xu.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->xu.to);
        } else if (cmpncs(member, "yunits") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->yu.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->yu.x=gnorm(0,pttab->yu.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->yu.y=gnorm(1,pttab->yu.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->yu.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->yu.to);
        } else if (cmpncs(member, "zunits") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->zu.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->zu.x=gnorm(0,pttab->zu.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->zu.y=gnorm(1,pttab->zu.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->zu.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->zu.to);
        } else if (cmpncs(member, "tunits") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->tu.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->tu.x=gnorm(0,pttab->tu.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->tu.y=gnorm(1,pttab->tu.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->tu.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->tu.to);
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Return the VCS template form (Pf) member value.
 */
static PyObject *
PyVCS_getPfmember(PyVCScanvas_Object *self, PyObject *args)
{
        char                    *Pf_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PF=NULL, *MEMBER=NULL, *ATTRIBUTE;
        struct p_tab            *pttab;
        extern struct p_tab     Pic_tab;
        extern float gnorm(int x_or_y, float value, int normalized_flg, int orientation);

        if(PyArg_ParseTuple(args,"|OOO",&PF, &MEMBER, &ATTRIBUTE)) {
           if (PF == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct template object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }


        Get_Member(PF,"name", "s", &Pf_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pf_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pf_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        if (cmpncs(member, "xvalue") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->xv.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->xv.x=gnorm(0,pttab->xv.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->xv.y=gnorm(1,pttab->xv.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "format") == 0)
              return Py_BuildValue("s",pttab->xv.fmt);
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->xv.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->xv.to);
        } else if (cmpncs(member, "yvalue") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->yv.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->yv.x=gnorm(0,pttab->yv.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->yv.y=gnorm(1,pttab->yv.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "format") == 0)
              return Py_BuildValue("s",pttab->yv.fmt);
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->yv.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->yv.to);
        } else if (cmpncs(member, "zvalue") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->zv.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->zv.x=gnorm(0,pttab->zv.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->zv.y=gnorm(1,pttab->zv.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "format") == 0)
              return Py_BuildValue("s",pttab->zv.fmt);
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->zv.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->zv.to);
        } else if (cmpncs(member, "tvalue") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->tv.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->tv.x=gnorm(0,pttab->tv.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->tv.y=gnorm(1,pttab->tv.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "format") == 0)
              return Py_BuildValue("s",pttab->tv.fmt);
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->tv.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->tv.to);
        } else if (cmpncs(member, "mean") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->mean.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->mean.x=gnorm(0,pttab->mean.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->mean.y=gnorm(1,pttab->mean.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "format") == 0)
              return Py_BuildValue("s",pttab->mean.fmt);
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->mean.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->mean.to);
        } else if (cmpncs(member, "min") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->min.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->min.x=gnorm(0,pttab->min.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->min.y=gnorm(1,pttab->min.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "format") == 0)
              return Py_BuildValue("s",pttab->min.fmt);
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->min.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->min.to);
        } else if (cmpncs(member, "max") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->max.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->max.x=gnorm(0,pttab->max.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->max.y=gnorm(1,pttab->max.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "format") == 0)
              return Py_BuildValue("s",pttab->max.fmt);
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->max.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->max.to);
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Return the VCS template X - Tick Marks (Pxt) member value.
 */
static PyObject *
PyVCS_getPxtmember(PyVCScanvas_Object *self, PyObject *args)
{
        char                    *Pxt_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PXT=NULL, *MEMBER=NULL, *ATTRIBUTE;
        struct p_tab            *pttab;
        extern struct p_tab     Pic_tab;
        extern float gnorm(int x_or_y, float value, int normalized_flg, int orientation);

        if(PyArg_ParseTuple(args,"|OOO",&PXT, &MEMBER, &ATTRIBUTE)) {
           if (PXT == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct template object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }


        Get_Member(PXT,"name", "s", &Pxt_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pxt_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pxt_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        if (cmpncs(member, "xtic1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->xt1.p);
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->xt1.y1=gnorm(1,pttab->xt1.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->xt1.y2=gnorm(1,pttab->xt1.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->xt1.ln);
        } else if (cmpncs(member, "xtic2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->xt2.p);
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->xt2.y1=gnorm(1,pttab->xt2.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->xt2.y2=gnorm(1,pttab->xt2.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->xt2.ln);
        } else if (cmpncs(member, "xmintic1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->xmta.p);
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->xmta.y1=gnorm(1,pttab->xmta.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->xmta.y2=gnorm(1,pttab->xmta.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->xmta.ln);
        } else if (cmpncs(member, "xmintic2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->xmtb.p);
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->xmtb.y1=gnorm(1,pttab->xmtb.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->xmtb.y2=gnorm(1,pttab->xmtb.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->xmtb.ln);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Return the VCS template Y - Tick Marks (Pyt) member value.
 */
static PyObject *
PyVCS_getPytmember(PyVCScanvas_Object *self, PyObject *args)
{
        char                    *Pyt_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PYT=NULL, *MEMBER=NULL, *ATTRIBUTE;
        struct p_tab            *pttab;
        extern struct p_tab     Pic_tab;
        extern float gnorm(int x_or_y, float value, int normalized_flg, int orientation);

        if(PyArg_ParseTuple(args,"|OOO",&PYT, &MEMBER, &ATTRIBUTE)) {
           if (PYT == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct template object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }


        Get_Member(PYT,"name", "s", &Pyt_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pyt_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pyt_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        if (cmpncs(member, "ytic1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->yt1.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->yt1.x1=gnorm(0,pttab->yt1.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->yt1.x2=gnorm(0,pttab->yt1.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->yt1.ln);
        } else if (cmpncs(member, "ytic2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->yt2.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->yt2.x1=gnorm(0,pttab->yt2.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->yt2.x2=gnorm(0,pttab->yt2.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->yt2.ln);
        } else if (cmpncs(member, "ymintic1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->ymta.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->ymta.x1=gnorm(0,pttab->ymta.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->ymta.x2=gnorm(0,pttab->ymta.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->ymta.ln);
        } else if (cmpncs(member, "ymintic2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->ymtb.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->ymtb.x1=gnorm(0,pttab->ymtb.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->ymtb.x2=gnorm(0,pttab->ymtb.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->ymtb.ln);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Return the VCS template X - Labels (Pxl) member value.
 */
static PyObject *
PyVCS_getPxlmember(PyVCScanvas_Object *self, PyObject *args)
{
        char                    *Pxl_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PXL=NULL, *MEMBER=NULL, *ATTRIBUTE;
        struct p_tab            *pttab;
        extern struct p_tab     Pic_tab;
        extern float gnorm(int x_or_y, float value, int normalized_flg, int orientation);

        if(PyArg_ParseTuple(args,"|OOO",&PXL, &MEMBER, &ATTRIBUTE)) {
           if (PXL == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct template object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }


        Get_Member(PXL,"name", "s", &Pxl_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pxl_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pxl_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        if (cmpncs(member, "xlabel1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->xl1.p);
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->xl1.y=gnorm(1,pttab->xl1.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->xl1.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->xl1.to);
        } else if (cmpncs(member, "xlabel2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->xl2.p);
           else if (cmpncs(attribute, "y") == 0)
              return Py_BuildValue("f",pttab->xl2.y=gnorm(1,pttab->xl2.y,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->xl2.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->xl2.to);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Return the VCS template Y - Labels (Pyl) member value.
 */
static PyObject *
PyVCS_getPylmember(PyVCScanvas_Object *self, PyObject *args)
{
        char                    *Pyl_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PYL=NULL, *MEMBER=NULL, *ATTRIBUTE;
        struct p_tab            *pttab;
        extern struct p_tab     Pic_tab;
        extern float gnorm(int x_or_y, float value, int normalized_flg, int orientation);

        if(PyArg_ParseTuple(args,"|OOO",&PYL, &MEMBER, &ATTRIBUTE)) {
           if (PYL == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct template object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }


        Get_Member(PYL,"name", "s", &Pyl_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pyl_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pyl_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        if (cmpncs(member, "ylabel1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->yl1.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->yl1.x=gnorm(0,pttab->yl1.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->yl1.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->yl1.to);
        } else if (cmpncs(member, "ylabel2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->yl2.p);
           else if (cmpncs(attribute, "x") == 0)
              return Py_BuildValue("f",pttab->yl2.x=gnorm(0,pttab->yl2.x,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->yl2.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->yl2.to);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Return the VCS template Boxes and - Lines (Pbl) member value.
 */
static PyObject *
PyVCS_getPblmember(PyVCScanvas_Object *self, PyObject *args)
{
        char                    *Pbl_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PBL=NULL, *MEMBER=NULL, *ATTRIBUTE;
        struct p_tab            *pttab;
        extern struct p_tab     Pic_tab;
        extern float gnorm(int x_or_y, float value, int normalized_flg, int orientation);

        if(PyArg_ParseTuple(args,"|OOO",&PBL, &MEMBER, &ATTRIBUTE)) {
           if (PBL == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct template object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }


        Get_Member(PBL,"name", "s", &Pbl_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pbl_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pbl_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        if (cmpncs(member, "box1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->b1.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->b1.x1=gnorm(0,pttab->b1.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->b1.y1=gnorm(1,pttab->b1.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->b1.x2=gnorm(0,pttab->b1.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->b1.y2=gnorm(1,pttab->b1.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->b1.ln);
        } else if (cmpncs(member, "box2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->b2.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->b2.x1=gnorm(0,pttab->b2.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->b2.y1=gnorm(1,pttab->b2.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->b2.x2=gnorm(0,pttab->b2.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->b2.y2=gnorm(1,pttab->b2.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->b2.ln);
        } else if (cmpncs(member, "box3") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->b3.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->b3.x1=gnorm(0,pttab->b3.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->b3.y1=gnorm(1,pttab->b3.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->b3.x2=gnorm(0,pttab->b3.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->b3.y2=gnorm(1,pttab->b3.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->b3.ln);
        } else if (cmpncs(member, "box4") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->b4.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->b4.x1=gnorm(0,pttab->b4.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->b4.y1=gnorm(1,pttab->b4.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->b4.x2=gnorm(0,pttab->b4.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->b4.y2=gnorm(1,pttab->b4.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->b4.ln);
        } else if (cmpncs(member, "line1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->l1.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->l1.x1=gnorm(0,pttab->l1.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->l1.y1=gnorm(1,pttab->l1.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->l1.x2=gnorm(0,pttab->l1.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->l1.y2=gnorm(1,pttab->l1.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->l1.ln);
        } else if (cmpncs(member, "line2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->l2.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->l2.x1=gnorm(0,pttab->l2.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->l2.y1=gnorm(1,pttab->l2.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->l2.x2=gnorm(0,pttab->l2.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->l2.y2=gnorm(1,pttab->l2.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->l2.ln);
        } else if (cmpncs(member, "line3") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->l3.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->l3.x1=gnorm(0,pttab->l3.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->l3.y1=gnorm(1,pttab->l3.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->l3.x2=gnorm(0,pttab->l3.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->l3.y2=gnorm(1,pttab->l3.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->l3.ln);
        } else if (cmpncs(member, "line4") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->l4.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->l4.x1=gnorm(0,pttab->l4.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->l4.y1=gnorm(1,pttab->l4.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->l4.x2=gnorm(0,pttab->l4.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->l4.y2=gnorm(1,pttab->l4.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->l4.ln);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Return the VCS template Legend Space (Pls) member value.
 */
static PyObject *
PyVCS_getPlsmember(PyVCScanvas_Object *self, PyObject *args)
{
        char                    *Pls_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PLS=NULL, *MEMBER=NULL, *ATTRIBUTE;
        struct p_tab            *pttab;
        extern struct p_tab     Pic_tab;
        extern float gnorm(int x_or_y, float value, int normalized_flg, int orientation);

        if(PyArg_ParseTuple(args,"|OOO",&PLS, &MEMBER, &ATTRIBUTE)) {
           if (PLS == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct template object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }


        Get_Member(PLS,"name", "s", &Pls_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pls_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pls_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        if (cmpncs(member, "legend") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->leg.p);
           else if (cmpncs(attribute, "x1") == 0)
              return Py_BuildValue("f",pttab->leg.x1 = gnorm(0,pttab->leg.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->leg.y1 = gnorm(1,pttab->leg.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->leg.x2 = gnorm(0,pttab->leg.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->leg.y2 = gnorm(1,pttab->leg.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "texttable") == 0)
              return Py_BuildValue("s",pttab->leg.tb);
           else if (cmpncs(attribute, "textorientation") == 0)
              return Py_BuildValue("s",pttab->leg.to);
           else if (cmpncs(attribute, "line") == 0)
              return Py_BuildValue("s",pttab->leg.ln);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Return the VCS template Display Space (Pds) member value.
 */
static PyObject *
PyVCS_getPdsmember(PyVCScanvas_Object *self, PyObject *args)
{
        char                    *Pds_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PDS=NULL, *MEMBER=NULL, *ATTRIBUTE;
        struct p_tab            *pttab;
        extern struct p_tab     Pic_tab;
        extern float gnorm(int x_or_y, float value, int normalized_flg, int orientation);

        if(PyArg_ParseTuple(args,"|OOO",&PDS, &MEMBER, &ATTRIBUTE)) {
           if (PDS == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct template object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }

        Get_Member(PDS,"name", "s", &Pds_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pds_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pds_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        if (cmpncs(member, "data") == 0) {
           if (cmpncs(attribute, "priority") == 0)
              return Py_BuildValue("i",pttab->dsp.p);
           else if (cmpncs(attribute, "x1") == 0)
	     return Py_BuildValue("f",pttab->dsp.x1 = gnorm(0,(float)pttab->dsp.x1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y1") == 0)
              return Py_BuildValue("f",pttab->dsp.y1 = gnorm(1,pttab->dsp.y1,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "x2") == 0)
              return Py_BuildValue("f",pttab->dsp.x2 = gnorm(0,pttab->dsp.x2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "y2") == 0)
              return Py_BuildValue("f",pttab->dsp.y2 = gnorm(1,pttab->dsp.y2,pttab->normalized_flg,pttab->orientation_flg));
           else if (cmpncs(attribute, "_ratio") == 0)
	     return Py_BuildValue("f",pttab->dsp.ratio);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Set the template normalization flag to 1.
 */
static PyObject *
PyVCS_set_normalized_flag(self, args)
  PyObject *self;
  PyObject *args;
{
        char        		*P_NAME=NULL;
        struct p_tab            *ptab;
        extern struct p_tab     Pic_tab;

        if(PyArg_ParseTuple(args,"|s", &P_NAME)) {
            if (P_NAME == NULL) {
                PyErr_SetString(PyExc_TypeError, "Must provide template method name.");
                return NULL;
            }
        }
        for (ptab = &Pic_tab; ptab != NULL; ptab = ptab->next)
            if (strcmp(ptab->name,P_NAME) == 0) break;

        ptab->normalized_flg = 1;

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Return the template normalization flag (i.e., 0 - not normalized or 1 -normalized).
 */
static PyObject *
PyVCS_return_normalized_flag(self, args)
  PyObject *self;
  PyObject *args;
{
        char                    *P_NAME=NULL;
        struct p_tab            *ptab;
        extern struct p_tab     Pic_tab;

        if(PyArg_ParseTuple(args,"|s", &P_NAME)) {
            if (P_NAME == NULL) {
                PyErr_SetString(PyExc_TypeError, "Must provide template method name.");
                return NULL;
            }
        }
        for (ptab = &Pic_tab; ptab != NULL; ptab = ptab->next)
            if (strcmp(ptab->name,P_NAME) == 0) break;

        /* Return the normalized flag value as a Python Object */
        return Py_BuildValue("i", ptab->normalized_flg);
}


/*
 * Find the existing template orientation and set its members.
 */
static PyObject *
PyVCS_setPomember(self, args)
  PyObject *self;
  PyObject *args;
{
	int                     MODE;
        char                    *Pt_name, buf[1024];
        PyObject		*PT=NULL, *VALUE=NULL;
        struct p_tab            *pttab, *get_ptab=NULL;
        extern struct p_tab     Pic_tab;
        extern struct p_tab     *getP(); 
	extern int              chk_mov_P();

        if(PyArg_ParseTuple(args,"|Oi", &PT, &VALUE)) {
           if (PT == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(PT,"name", "s", &Pt_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pt_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pt_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        get_ptab = getP(pttab->name);
        get_ptab->orientation_flg = (int) VALUE;

        chk_mov_P(get_ptab,1);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing template text method and set its members.
 */
static PyObject *
PyVCS_setPtmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int                     MODE;
        char                    *Pt_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject		*PT=NULL, *MEMBER=NULL, *ATTRIBUTE=NULL, *VALUE=NULL;
        struct p_tab            *pttab, *get_ptab=NULL;
        extern struct p_tab     Pic_tab;
        extern struct p_tab     *getP(); 
	extern int              update_ind;
        extern int              vcs_legacy_canvas_update();
	extern int              chk_mov_P();

        if(PyArg_ParseTuple(args,"|OOOOi",&PT,&MEMBER,&ATTRIBUTE,&VALUE,&MODE)) {
           if (PT == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }

        Get_Member(PT,"name", "s", &Pt_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pt_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pt_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        /*
         * Set the template attribute. But first
         * get the template structure.
         */
        get_ptab = getP(pttab->name);
        if (cmpncs(member, "file") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->F.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->F.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->F.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->F.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->F.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "function") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->f.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->f.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->f.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->f.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->f.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "logicalmask") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->lmask.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->lmask.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->lmask.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->lmask.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->lmask.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "transformation") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->trnf.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->trnf.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->trnf.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->trnf.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->trnf.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "source") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->s.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->s.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->s.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->s.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->s.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "dataname") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->n.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->n.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->n.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->n.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->n.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "title") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->ti.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->ti.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->ti.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->ti.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->ti.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "units") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->u.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->u.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->u.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->u.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->u.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "crdate") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->crd.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->crd.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->crd.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->crd.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->crd.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "crtime") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->crt.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->crt.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->crt.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->crt.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->crt.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "comment1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->com1.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->com1.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->com1.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->com1.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->com1.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "comment2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->com2.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->com2.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->com2.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->com2.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->com2.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "comment3") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->com3.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->com3.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->com3.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->com3.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->com3.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "comment4") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->com4.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->com4.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->com4.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->com4.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->com4.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "xname") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->xn.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->xn.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->xn.y = (float) PyFloat_AsDouble(VALUE); 
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->xn.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->xn.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "yname") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->yn.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->yn.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->yn.y = (float) PyFloat_AsDouble(VALUE); 
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->yn.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->yn.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "zname") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->zn.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->zn.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->zn.y = (float) PyFloat_AsDouble(VALUE); 
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->zn.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->zn.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "tname") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->tn.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->tn.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->tn.y = (float) PyFloat_AsDouble(VALUE); 
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->tn.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->tn.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "xunits") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->xu.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->xu.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->xu.y = (float) PyFloat_AsDouble(VALUE); 
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->xu.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->xu.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "yunits") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->yu.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->yu.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->yu.y = (float) PyFloat_AsDouble(VALUE); 
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->yu.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->yu.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "zunits") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->zu.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->zu.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->zu.y = (float) PyFloat_AsDouble(VALUE); 
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->zu.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->zu.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "tunits") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->tu.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->tu.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->tu.y = (float) PyFloat_AsDouble(VALUE); 
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->tu.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->tu.to, PyString_AsString(VALUE));
	}

        chk_mov_P(get_ptab,1);
        update_ind = MODE; /* Update the display if needed */
        vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing template format method and set its members.
 */
static PyObject *
PyVCS_setPfmember(self, args)
  PyObject *self;
  PyObject *args;
{
        int                     MODE;
        char                    *Pf_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PF=NULL, *MEMBER=NULL, *ATTRIBUTE=NULL, *VALUE=NULL;
        struct p_tab            *pttab, *get_ptab=NULL;
        extern struct p_tab     Pic_tab;
        extern struct p_tab     *getP();
        extern int              update_ind;
        extern int              vcs_legacy_canvas_update();
        extern int              chk_mov_P();

        if(PyArg_ParseTuple(args,"|OOOOi",&PF,&MEMBER,&ATTRIBUTE,&VALUE,&MODE)) {
           if (PF == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }

        Get_Member(PF,"name", "s", &Pf_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pf_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pf_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        /*
         * Set the template attribute. But first
         * get the template structure.
         */
        get_ptab = getP(pttab->name);
        if (cmpncs(member, "xvalue") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->xv.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->xv.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->xv.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "format") == 0)
               strcpy(get_ptab->xv.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->xv.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->xv.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "yvalue") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->yv.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->yv.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->yv.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "format") == 0)
               strcpy(get_ptab->yv.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->yv.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->yv.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "zvalue") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->zv.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->zv.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->zv.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "format") == 0)
               strcpy(get_ptab->zv.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->zv.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->zv.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "tvalue") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->tv.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->tv.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->tv.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "format") == 0)
               strcpy(get_ptab->tv.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->tv.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->tv.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "mean") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->mean.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->mean.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->mean.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "format") == 0)
               strcpy(get_ptab->mean.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->mean.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->mean.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "min") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->min.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->min.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->min.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "format") == 0)
               strcpy(get_ptab->min.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->min.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->min.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "max") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->max.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->max.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->max.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "format") == 0)
               strcpy(get_ptab->max.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->max.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->max.to, PyString_AsString(VALUE));
	}

        chk_mov_P(get_ptab,1);
        update_ind = MODE; /* Update the display if needed */
        vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing template X - tick marks method and set its members.
 */
static PyObject *
PyVCS_setPxtmember(self, args)
  PyObject *self;
  PyObject *args;
{
        int                     MODE;
        char                    *Pxt_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PXT=NULL, *MEMBER=NULL, *ATTRIBUTE=NULL, *VALUE=NULL;
        struct p_tab            *pttab, *get_ptab=NULL;
        extern struct p_tab     Pic_tab;
        extern struct p_tab     *getP();
        extern int              update_ind;
        extern int              vcs_legacy_canvas_update();
        extern int              chk_mov_P();

        if(PyArg_ParseTuple(args,"|OOOOi",&PXT,&MEMBER,&ATTRIBUTE,&VALUE,&MODE)) {
           if (PXT == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }

        Get_Member(PXT,"name", "s", &Pxt_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pxt_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pxt_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        /*
         * Set the template attribute. But first
         * get the template structure.
         */
        get_ptab = getP(pttab->name);
        if (cmpncs(member, "xtic1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->xt1.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->xt1.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->xt1.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->xt1.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "xtic2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->xt2.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->xt2.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->xt2.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->xt2.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "xmintic1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->xmta.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->xmta.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->xmta.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->xmta.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "xmintic2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->xmtb.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->xmtb.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->xmtb.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->xmtb.ln, PyString_AsString(VALUE));
	}

        chk_mov_P(get_ptab,1);
        update_ind = MODE; /* Update the display if needed */
        vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing template Y - tick marks method and set its members.
 */
static PyObject *
PyVCS_setPytmember(self, args)
  PyObject *self;
  PyObject *args;
{
        int                     MODE;
        char                    *Pyt_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PYT=NULL, *MEMBER=NULL, *ATTRIBUTE=NULL, *VALUE=NULL;
        struct p_tab            *pttab, *get_ptab=NULL;
        extern struct p_tab     Pic_tab;
        extern struct p_tab     *getP();
        extern int              update_ind;
        extern int              vcs_legacy_canvas_update();
        extern int              chk_mov_P();

        if(PyArg_ParseTuple(args,"|OOOOi",&PYT,&MEMBER,&ATTRIBUTE,&VALUE,&MODE)) {
           if (PYT == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }

        Get_Member(PYT,"name", "s", &Pyt_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pyt_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pyt_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        /*
         * Set the template attribute. But first
         * get the template structure.
         */
        get_ptab = getP(pttab->name);
        if (cmpncs(member, "ytic1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->yt1.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->yt1.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->yt1.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->yt1.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "ytic2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->yt2.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->yt2.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->yt2.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->yt2.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "ymintic1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->ymta.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->ymta.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->ymta.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->ymta.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "ymintic2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->ymtb.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->ymtb.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->ymtb.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->ymtb.ln, PyString_AsString(VALUE));
	}

        chk_mov_P(get_ptab,1);
        update_ind = MODE; /* Update the display if needed */
        vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing template X - labels method and set its members.
 */
static PyObject *
PyVCS_setPxlmember(self, args)
  PyObject *self;
  PyObject *args;
{
        int                     MODE;
        char                    *Pxl_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PXL=NULL, *MEMBER=NULL, *ATTRIBUTE=NULL, *VALUE=NULL;
        struct p_tab            *pttab, *get_ptab=NULL;
        extern struct p_tab     Pic_tab;
        extern struct p_tab     *getP();
        extern int              update_ind;
        extern int              vcs_legacy_canvas_update();
        extern int              chk_mov_P();

        if(PyArg_ParseTuple(args,"|OOOOi",&PXL,&MEMBER,&ATTRIBUTE,&VALUE,&MODE)) {
           if (PXL == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }

        Get_Member(PXL,"name", "s", &Pxl_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pxl_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pxl_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        /*
         * Set the template attribute. But first
         * get the template structure.
         */
        get_ptab = getP(pttab->name);
        if (cmpncs(member, "xlabel1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->xl1.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->xl1.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->xl1.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->xl1.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "xlabel2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->xl2.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "y") == 0)
               get_ptab->xl2.y = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->xl2.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->xl2.to, PyString_AsString(VALUE));
	}

        chk_mov_P(get_ptab,1);
        update_ind = MODE; /* Update the display if needed */
        vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing template Y - labels method and set its members.
 */
static PyObject *
PyVCS_setPylmember(self, args)
  PyObject *self;
  PyObject *args;
{
        int                     MODE;
        char                    *Pyl_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PYL=NULL, *MEMBER=NULL, *ATTRIBUTE=NULL, *VALUE=NULL;
        struct p_tab            *pttab, *get_ptab=NULL;
        extern struct p_tab     Pic_tab;
        extern struct p_tab     *getP();
        extern int              update_ind;
        extern int              vcs_legacy_canvas_update();
        extern int              chk_mov_P();

        if(PyArg_ParseTuple(args,"|OOOOi",&PYL,&MEMBER,&ATTRIBUTE,&VALUE,&MODE)) {
           if (PYL == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }

        Get_Member(PYL,"name", "s", &Pyl_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pyl_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pyl_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        /*
         * Set the template attribute. But first
         * get the template structure.
         */
        get_ptab = getP(pttab->name);
        if (cmpncs(member, "ylabel1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->yl1.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->yl1.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->yl1.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->yl1.to, PyString_AsString(VALUE));
        } else if (cmpncs(member, "ylabel2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->yl2.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x") == 0)
               get_ptab->yl2.x = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->yl2.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->yl2.to, PyString_AsString(VALUE));
	}

        chk_mov_P(get_ptab,1);
        update_ind = MODE; /* Update the display if needed */
        vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing template boxes and lines method and set its members.
 */
static PyObject *
PyVCS_setPblmember(self, args)
  PyObject *self;
  PyObject *args;
{
        int                     MODE;
        char                    *Pbl_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PBL=NULL, *MEMBER=NULL, *ATTRIBUTE=NULL, *VALUE=NULL;
        struct p_tab            *pttab, *get_ptab=NULL;
        extern struct p_tab     Pic_tab;
        extern struct p_tab     *getP();
        extern int              update_ind;
        extern int              vcs_legacy_canvas_update();
        extern int              chk_mov_P();

        if(PyArg_ParseTuple(args,"|OOOOi",&PBL,&MEMBER,&ATTRIBUTE,&VALUE,&MODE)) {
           if (PBL == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }

        Get_Member(PBL,"name", "s", &Pbl_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pbl_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pbl_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        /*
         * Set the template attribute. But first
         * get the template structure.
         */
        get_ptab = getP(pttab->name);
        if (cmpncs(member, "box1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->b1.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->b1.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->b1.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->b1.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->b1.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->b1.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "box2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->b2.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->b2.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->b2.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->b2.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->b2.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->b2.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "box3") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->b3.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->b3.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->b3.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->b3.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->b3.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->b3.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "box4") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->b4.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->b4.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->b4.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->b4.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->b4.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->b4.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "line1") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->l1.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->l1.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->l1.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->l1.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->l1.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->l1.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "line2") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->l2.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->l2.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->l2.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->l2.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->l2.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->l2.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "line3") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->l3.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->l3.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->l3.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->l3.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->l3.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->l3.ln, PyString_AsString(VALUE));
        } else if (cmpncs(member, "line4") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->l4.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->l4.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->l4.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->l4.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->l4.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->l4.ln, PyString_AsString(VALUE));
	}

        chk_mov_P(get_ptab,1);
        update_ind = MODE; /* Update the display if needed */
        vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing template legend space method and set its members.
 */
static PyObject *
PyVCS_setPlsmember(self, args)
  PyObject *self;
  PyObject *args;
{
        int                     MODE;
        char                    *Pls_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PLS=NULL, *MEMBER=NULL, *ATTRIBUTE=NULL, *VALUE=NULL;
        struct p_tab            *pttab, *get_ptab=NULL;
        extern struct p_tab     Pic_tab;
        extern struct p_tab     *getP();
        extern int              update_ind;
        extern int              vcs_legacy_canvas_update();
        extern int              chk_mov_P();

        if(PyArg_ParseTuple(args,"|OOOOi",&PLS,&MEMBER,&ATTRIBUTE,&VALUE,&MODE)) {
           if (PLS == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }

        Get_Member(PLS,"name", "s", &Pls_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pls_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pls_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        /*
         * Set the template attribute. But first
         * get the template structure.
         */
        get_ptab = getP(pttab->name);
        if (cmpncs(member, "legend") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->leg.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
               get_ptab->leg.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->leg.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->leg.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->leg.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "line") == 0)
               strcpy(get_ptab->leg.ln, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "texttable") == 0)
               strcpy(get_ptab->leg.tb, PyString_AsString(VALUE));
           else if (cmpncs(attribute, "textorientation") == 0)
               strcpy(get_ptab->leg.to, PyString_AsString(VALUE));
	}

        chk_mov_P(get_ptab,1);
        update_ind = MODE; /* Update the display if needed */
        vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing template data space method and set its members.
 */
static PyObject *
PyVCS_setPdsmember(self, args)
  PyObject *self;
  PyObject *args;
{
        int                     MODE;
        char                    *Pds_name, *member=NULL, *attribute=NULL, buf[1024];
        PyObject                *PDS=NULL, *MEMBER=NULL, *ATTRIBUTE=NULL, *VALUE=NULL;
        struct p_tab            *pttab, *get_ptab=NULL;
        extern struct p_tab     Pic_tab;
        extern struct p_tab     *getP();
        extern int              update_ind;
        extern int              vcs_legacy_canvas_update();
        extern int              chk_mov_P();

        if(PyArg_ParseTuple(args,"|OOOOi",&PDS,&MEMBER,&ATTRIBUTE,&VALUE,&MODE)) {
           if (PDS == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }

           if (ATTRIBUTE != NULL) {
              attribute = PyString_AsString(ATTRIBUTE);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply an attribute name.");
              return NULL;
           }
        }

        Get_Member(PDS,"name", "s", &Pds_name);
        pttab=&Pic_tab;
        while ((pttab != NULL) &&
               (strcmp(pttab->name, Pds_name) != 0))
           pttab = pttab->next;

        if (pttab == NULL) {
           sprintf(buf,"Cannot find template method P_%s.",Pds_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

        /*
         * Set the template attribute. But first
         * get the template structure.
         */
        get_ptab = getP(pttab->name);
        if (cmpncs(member, "data") == 0) {
           if (cmpncs(attribute, "priority") == 0)
               get_ptab->dsp.p = (int) PyInt_AsLong(VALUE);
           else if (cmpncs(attribute, "x1") == 0)
	       get_ptab->dsp.x1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y1") == 0)
               get_ptab->dsp.y1 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "x2") == 0)
               get_ptab->dsp.x2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "y2") == 0)
               get_ptab->dsp.y2 = (float) PyFloat_AsDouble(VALUE);
           else if (cmpncs(attribute, "_ratio") == 0)
               get_ptab->dsp.ratio = (float) PyFloat_AsDouble(VALUE);
	}

        chk_mov_P(get_ptab,1);
        update_ind = MODE; /* Update the display if needed */
        vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Copy the values from the temporary template to the 
 * real template (ie. copy .ASD001 to ASD)
 */
static PyObject *
PyVCS_syncP(self,args)
    PyObject *self;
    PyObject *args;
{
        int         ierr;
        char        *P_SRC=NULL, *P_NAME=NULL;
        char        copy_name[1024];
        extern struct p_tab 		Pic_tab;
        struct p_tab 		        *ptab,*gtab;
        extern int  copyP_attr();
    
        if(PyArg_ParseTuple(args,"|ss", &P_SRC, &P_NAME)) 
        {
            if (P_SRC == NULL) 
            {
                 PyErr_SetString(PyExc_TypeError, "Must provide source template method name.");
                 return NULL;
            }
            if (P_NAME == NULL)
                sprintf(copy_name, "%s", "default");
            else
                sprintf(copy_name, "%s", P_NAME);
            if (strcmp(P_NAME,"default") == 0)
            {
                PyErr_SetString(PyExc_ValueError,"Cannot change default template.");
                return NULL;
            }
        }
        for (ptab = &Pic_tab; ptab != NULL; ptab = ptab->next)
            if (strcmp(ptab->name,P_SRC) == 0) break;
        for (gtab = &Pic_tab; gtab != NULL; gtab = gtab->next)
            if (strcmp(gtab->name,P_NAME) == 0) break;
        copyP_attr(gtab,ptab);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new template method by copying from an existing
 * template method. If no source copy name argument is given,
 * then the default template method will be used to replicate
 * the new template method.
 */
static PyObject *
PyVCS_copyP(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *P_SRC=NULL, *P_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_P_name();
              
        if(PyArg_ParseTuple(args,"|ss", &P_SRC, &P_NAME)) {
           if (P_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source template method name.");
                 return NULL;
           }

           if (P_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", P_NAME);
        }

        ierr = copy_P_name(P_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating template method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Get a Python dictionary object and convert its key values to VCS's
 * data values. Then convert the dictionary's values to the to VCS's
 * string values. Return the new VCS list name.
 */
char *
return_vcs_legacy_list(VALUE, member)
PyObject *VALUE;
char	 *member;
{
	int			i,j,ct=1,list_name_found=1;
	char			*list_name, *value_str, buf[1024];
 	struct l_val            *tval, *hval=NULL, *pval;
        struct l_tab            *ptab;
	PyObject 		*itempk,*itempv,*pkeys,*pvalues;
	extern struct l_tab     L_tab[2];
	extern int              chk_mov_L();

	i = PyDict_Size(VALUE);
        pkeys = PyDict_Keys(VALUE);
	pvalues = PyDict_Values(VALUE);
	for (j = 0; j < i; j++) {
	   /* malloc the VCS list structure */
	   if ((tval=(struct l_val *) malloc(sizeof(*tval))) == NULL) {
	      PyErr_SetString(PyExc_MemoryError, "Error - can't allocate memory for new VCS list!");
              return NULL;
	   }

	   /* Set the data value for the list */
           itempk=PyList_GetItem(pkeys, j);
           if (PyInt_Check(itempk)) {
              tval->data = (float) PyInt_AsLong(itempk);
           } else if (PyFloat_Check(itempk)) {
              tval->data = (float) PyFloat_AsDouble(itempk);
           } else {
	      PyErr_SetString(PyExc_IndexError, "Invalid dictionary key value.");
              return NULL;
           }

           /* Set the string value for the list */
           itempv=PyList_GetItem(pvalues, j);
           if (PyString_Check(itempv))
              value_str = PyString_AsString(itempv);
           else {
	      PyErr_SetString(PyExc_IndexError, "Invalid dictionary string value.");
              return NULL;
           }
           tval->str = (char *) malloc(strlen(value_str) * sizeof(char) + 1);
           strcpy(tval->str, value_str);

	   /* Set the next list pointer to NIL */
           tval->next = NULL;

           /* Put the structure into the link list */
           if (hval != NULL) {
              pval->next = tval;
              pval = tval;
           } else
             hval = pval = tval;
        }

        /* create a unique name for the new VCS list */
	list_name = (char *) malloc(strlen(member)+1);
	strcpy(list_name, member);
	while (list_name_found) {
           list_name_found=0;
	   ptab=&L_tab[0];
           while (ptab != NULL) {
              if (cmpncs(ptab->name,list_name) == 0) {
                 list_name_found = 1;
                 free((char *) list_name);
                 sprintf(buf,"%s_%d",member, ct++);
                 list_name = (char *) malloc(strlen(buf)+1);
                 strcpy(list_name, buf);
	         break;
              } else
                 ptab=ptab->next;
           }
	}

        /* Create new list entry */
        if ((ptab=(struct l_tab *) malloc(sizeof(L_tab))) == NULL) {
           PyErr_SetString(PyExc_MemoryError, "Error - can't allocate memory for new VCS list!");
           return NULL;
	}

        strcpy(ptab->name, list_name);
	free((char *) list_name);
	ptab->count = i;
        ptab->val = hval;
        ptab->next = NULL;

	/* Put the new VCS list into the list table */
        chk_mov_L(ptab);

	/* Set value_str */
	return (ptab->name);
}

/* 
 * Creates a vcs_legacy internal list from an existing dictionary 
 */
static PyObject *
PyVCS_dictionarytovcs_legacylist(PyVCScanvas_Object *self, PyObject *args)
{
	char *name=NULL;
	PyObject  *Pydic=NULL;

	if(PyArg_ParseTuple(args,"|Os",&Pydic, &name)) {
           if (Pydic == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }
	}
	if (PyDict_Check(Pydic)!=1)
	  {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type, should be a dictionary.");
                 return NULL;
           }
	name = return_vcs_legacy_list(Pydic,name);
        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}


/*
 * Return the VCS list names as a Python list.
 */
PyObject *
PyVCS_list_to_PyDict(char * list_name)
{
        int                             i;
        PyObject                        *dictptr=NULL;
	struct l_tab 			*ptab;
	struct l_val 			*pv, *npv, *ppv, *hpv=NULL;
        extern struct l_tab 		L_tab[2];

        for (ptab=&L_tab[0]; ptab != NULL; ptab=ptab->next) {
            if (cmpnbl(list_name,ptab->name) == 0) {
    /*		 No need to sort since Python won't keep the order anyway.... 			
 	       * Sort the VCS list from smallest value to largest *
	       for (i=0, pv=ptab->val; i<ptab->count; i++, pv = pv->next) {
	          if ((npv=(struct l_val *)malloc(sizeof(struct l_val)))==NULL) {
                     PyErr_SetString(PyExc_TypeError, "Error - out of memory for getting List values./n");
                      return NULL;
		  }
	          if ((npv->str=(char *)malloc(strlen(pv->str)+1))==NULL) {
                     PyErr_SetString(PyExc_TypeError, "Error - out of memory for getting List values./n");
                      return NULL;
                  }
		  npv->data=pv->data;
		  strcpy(npv->str,pv->str);
	          npv->next=NULL;

	          if (hpv == NULL)
		      hpv = ppv = npv;
		   else {
		      for (ppv=hpv; ppv != NULL; ppv=ppv->next) {
			 if ((hpv->data >= npv->data)) {
				 npv->next = hpv;
				 hpv = npv;
				 break;
			 } else if ((ppv->next != NULL) && (ppv->next->data >= npv->data)) {
				 npv->next = ppv->next;
			         ppv->next = npv;
				 break;
		         } else if (ppv->next == NULL) {
				 ppv->next = npv;
				 break;
			 }
		      }
		   }
	       }
	       */

	       /* Create the Python Dictionary */
               dictptr = PyDict_New( );
	       for (i=0, pv=ptab->val; i<ptab->count; i++, pv = pv->next)
	          PyDict_SetItem(dictptr, Py_BuildValue("f",pv->data), Py_BuildValue("s",pv->str));

	       /* Remove the created link list *
	       pv = hpv;
	       while (pv != NULL) {
		  npv = pv;
		  pv = pv->next;
		  free((char *) npv->str);
		  free((char *) npv);
	       }
	       */
	    }
	}

        /* Return the Python Dictionary of VCS List values */
        return dictptr;
}

/* 
 * Return the VCS boxfill (Gfb) graphics method member value. 
 */
static PyObject *
PyVCS_getGfbmember(PyVCScanvas_Object *self, PyObject *args)
{
	char *Gfb_name, *member=NULL, buf[1024];
	int i=0, ct=0;
	PyObject *GFB=NULL, *MEMBER=NULL, *tup, *lp;
	struct gfb_tab          *gfbtab;
	struct fill_range       *pfiso=NULL;
    	extern struct gfb_tab   Gfb_tab;

	if(PyArg_ParseTuple(args,"|OO",&GFB, &MEMBER)) {
           if (GFB == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


	Get_Member(GFB,"name", "s", &Gfb_name);
	gfbtab=&Gfb_tab;
        while ((gfbtab != NULL) &&
               (strcmp(gfbtab->name, Gfb_name) != 0))
           gfbtab = gfbtab->next;

     	if (gfbtab == NULL) {
	   sprintf(buf,"Cannot find boxfill graphics method Gfb_%s.",Gfb_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) {
           return Py_BuildValue("s", gfbtab->pGfb_attr->proj);
	} else if (cmpncs(member, "xticlabels1") == 0) {
           return Py_BuildValue("s", gfbtab->pGfb_attr->xtl1);
	} else if (cmpncs(member, "xticlabels2") == 0) {
           return Py_BuildValue("s", gfbtab->pGfb_attr->xtl2);
	} else if (cmpncs(member, "xmtics1") == 0) {
           return Py_BuildValue("s", gfbtab->pGfb_attr->xmt1);
	} else if (cmpncs(member, "xmtics2") == 0) {
           return Py_BuildValue("s", gfbtab->pGfb_attr->xmt2);
	} else if (cmpncs(member, "yticlabels1") == 0) {
           return Py_BuildValue("s", gfbtab->pGfb_attr->ytl1);
	} else if (cmpncs(member, "yticlabels2") == 0) {
           return Py_BuildValue("s", gfbtab->pGfb_attr->ytl2);
	} else if (cmpncs(member, "ymtics1") == 0) {
           return Py_BuildValue("s", gfbtab->pGfb_attr->ymt1);
	} else if (cmpncs(member, "ymtics2") == 0) {
           return Py_BuildValue("s", gfbtab->pGfb_attr->ymt2);
	} else if (cmpncs(member, "datawc_y1") == 0) {
           return Py_BuildValue("f",gfbtab->pGfb_attr->dsp[1]);
	} else if (cmpncs(member, "datawc_y2") == 0) {
           return Py_BuildValue("f",gfbtab->pGfb_attr->dsp[3]);
	} else if (cmpncs(member, "datawc_x1") == 0) {
           return Py_BuildValue("f",gfbtab->pGfb_attr->dsp[0]);
	} else if (cmpncs(member, "datawc_x2") == 0) {
           return Py_BuildValue("f",gfbtab->pGfb_attr->dsp[2]);
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
           return Py_BuildValue("i",gfbtab->pGfb_attr->idsp[1]);
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
           return Py_BuildValue("i",gfbtab->pGfb_attr->idsp[3]);
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
           return Py_BuildValue("i",gfbtab->pGfb_attr->idsp[0]);
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
           return Py_BuildValue("i",gfbtab->pGfb_attr->idsp[2]);
	} else if (cmpncs(member, "datawc_calendar") == 0) {
           return Py_BuildValue("i",gfbtab->pGfb_attr->calendar);
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
           return Py_BuildValue("s",gfbtab->pGfb_attr->timeunits);
	} else if ((cmpncs(member, "xaxisconvert") == 0) &&
                   ((cmpncs(gfbtab->pGfb_attr->xat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "xaxisconvert") == 0) {
           return Py_BuildValue("s", gfbtab->pGfb_attr->xat);
	} else if ((cmpncs(member, "yaxisconvert") == 0) &&
                   ((cmpncs(gfbtab->pGfb_attr->yat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "yaxisconvert") == 0) {
           return Py_BuildValue("s", gfbtab->pGfb_attr->yat);
	} else if (cmpncs(member, "level_1") == 0) {
           return Py_BuildValue("f", gfbtab->pGfb_attr->lev1);
	} else if (cmpncs(member, "level_2") == 0) {
           return Py_BuildValue("f", gfbtab->pGfb_attr->lev2);
	} else if (cmpncs(member, "color_1") == 0) {
           return Py_BuildValue("i", (int)gfbtab->pGfb_attr->color_1);
	} else if (cmpncs(member, "color_2") == 0) {
           return Py_BuildValue("i", gfbtab->pGfb_attr->color_2);
	} else if (cmpncs(member, "boxfill_type") == 0) {
           if (gfbtab->pGfb_attr->boxfill_type == 0)
              return Py_BuildValue("s", "linear");
/*           else if (gfbtab->pGfb_attr->boxfill_type == 1)
              return Py_BuildValue("s", "list");*/
           else if (gfbtab->pGfb_attr->boxfill_type == 2)
              return Py_BuildValue("s", "log10");
           else if (gfbtab->pGfb_attr->boxfill_type == 3)
              return Py_BuildValue("s", "custom");
           else
              return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "legend") == 0) {
           if ((gfbtab->pGfb_attr->legend == NULL) || (cmpnbl(gfbtab->pGfb_attr->legend, "") == 0))
              return Py_BuildValue("");
           else
              return PyVCS_list_to_PyDict(gfbtab->pGfb_attr->legend);
	} else if (cmpncs(member, "ext_1") == 0) {
           if (gfbtab->pGfb_attr->ext_1 == 110)
              return Py_BuildValue("s", "n");
           else if (gfbtab->pGfb_attr->ext_1 == 121)
              return Py_BuildValue("s", "y");
	} else if (cmpncs(member, "ext_2") == 0) {
           if (gfbtab->pGfb_attr->ext_2 == 110)
              return Py_BuildValue("s", "n");
           else if (gfbtab->pGfb_attr->ext_2 == 121)
              return Py_BuildValue("s", "y");
	} else if (cmpncs(member, "missing") == 0) {
           return Py_BuildValue("i", gfbtab->pGfb_attr->missing);
	} else if (cmpncs(member, "levels") == 0) {
           /* Get the box fill area structure */
           pfiso = gfbtab->pGfb_attr->line;
           while (pfiso != NULL) {
              pfiso = pfiso->next;
	      ct++;
	   }
           pfiso = gfbtab->pGfb_attr->line;
           tup = PyTuple_New(ct);
           while (pfiso != NULL) {
              lp = Py_BuildValue("[d,d]", pfiso->lev1, pfiso->lev2);
              PyTuple_SetItem(tup, i, lp);
              pfiso = pfiso->next;
              i++;
           }
           return tup;
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new boxfill graphics method by copying from an existing
 * boxfill graphics method. If no source copy name argument is given,
 * then the default boxfill graphics method will be used to replicate
 * the new boxfill graphics method.
 */
static PyObject *
PyVCS_copyGfb(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GFB_SRC=NULL, *GFB_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Gfb_name();
              
        if(PyArg_ParseTuple(args,"|ss", &GFB_SRC, &GFB_NAME)) {
           if (GFB_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source boxfill graphics method name.");
                 return NULL;
           }

           if (GFB_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", GFB_NAME);
        }

        ierr = copy_Gfb_name(GFB_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating boxfill graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}


/* 
 * Rename an existing template method.
 */
static PyObject *
PyVCS_renameP(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *P_OLD_NAME=NULL, *P_NEW_NAME=NULL;
        extern int      renameP_name();
 
        if(PyArg_ParseTuple(args,"|ss", &P_OLD_NAME, &P_NEW_NAME)) {
           if ((P_OLD_NAME == NULL) || (P_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new template method name.");
                 return NULL;
           }
        }

        ierr = renameP_name(P_OLD_NAME, P_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming template method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing template method.
 */
static PyObject *
PyVCS_removeP(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide a template name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeP_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed template object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The template object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Script out an existing template object.
 */
static PyObject *
PyVCS_scriptP(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *P_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
	char mode2[2];
	extern int dump_single_template();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &P_NAME, &SCRIPT_NAME, &MODE)) {
           if (P_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the template name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);

        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_template(fp, P_NAME) == 0) {
              sprintf(buf, "Error - Cannot save template script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Rename an existing boxfill graphics method.
 */
static PyObject *
PyVCS_renameGfb(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GFB_OLD_NAME=NULL, *GFB_NEW_NAME=NULL;
        extern int      renameGfb_name();
 
        if(PyArg_ParseTuple(args,"|ss", &GFB_OLD_NAME, &GFB_NEW_NAME)) {
           if ((GFB_OLD_NAME == NULL) || (GFB_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new boxfill graphics method name.");
                 return NULL;
           }
        }

        ierr = renameGfb_name(GFB_OLD_NAME, GFB_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming boxfill graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;


}

/* 
 * Remove an existing boxfill graphics method.
 */
static PyObject *
PyVCS_removeGfb(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the boxfill file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeGfb_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed boxfill object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The boxfill object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing boxfill graphics method.
 */
static PyObject *
PyVCS_scriptGfb(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GFB_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
	char mode2[2];
	extern int dump_single_boxfill();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &GFB_NAME, &SCRIPT_NAME, &MODE)) {
           if (GFB_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the boxfill name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);

        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_boxfill(fp, GFB_NAME) == 0) {
              sprintf(buf, "Error - Cannot save boxfill script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/*
 * Find the existing boxfill graphics method and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setGfbmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			i,j=0,n,ct=0,sct=0,color_index,style_index;
	int 			MODE, value_int;	
	long 			value_long;
	float 			value_float;
	double 			value_double;
	char                    *Tf_name;
	char 			buf[1024], *style;
        char 			*Gfb_name, *str=NULL, *member=NULL;
	char			*value_str=NULL;
        PyObject 		*GFB=NULL, *MEMBER=NULL, *VALUE=NULL;
	PyObject                *listit,*tup, *sindices;
	PyObject 		*itempk, *itempv, *pkeys, *pvalues;
	struct gfb_tab          *get_gfbtab=NULL;
	/*static PyObject *	PyVCS_backing_store(); Mac OS X 10.4 build didn't like this */
	extern int              update_ind;
	struct fill_range       *pfiso, *next_pfiso, *pfiso_new, *tpfiso;
        struct gfb_tab          *gfbtab;
        extern struct gfb_tab   Gfb_tab;
	extern struct gfb_tab   *getGfb();
	extern int              chk_mov_Gfb();
	extern int 		vcs_legacy_canvas_update();
	char * 			return_new_fillarea_attribute();

        if(PyArg_ParseTuple(args,"|OOOi", &GFB, &MEMBER, &VALUE, &MODE)) {
           if (GFB == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(GFB,"name", "s", &Gfb_name);
        gfbtab=&Gfb_tab;
        while ((gfbtab != NULL) &&
               (strcmp(gfbtab->name, Gfb_name) != 0))
           gfbtab = gfbtab->next;

	if (MEMBER != NULL) {
           member = PyString_AsString(MEMBER);
/*	   sprintf(buf, "print 'member = %s'", member);
           PyRun_SimpleString(buf);*/
	}

	if (VALUE != NULL) {
	   if (PyString_Check(VALUE)) { /*check string*/
              value_str = PyString_AsString(VALUE);
/*	      sprintf(buf, "print 'value = %s'", value_str);
              PyRun_SimpleString(buf);*/
	   } else if (PyInt_Check(VALUE)) { /*check for int*/
              value_int = (int) PyInt_AsLong(VALUE);
/*	      sprintf(buf, "print 'value = %d'", value_int);
              PyRun_SimpleString(buf);*/
	   } else if (PyFloat_Check(VALUE)) { /*check for float*/
              value_float = (float) PyFloat_AsDouble(VALUE);
/*	      sprintf(buf, "print 'value = %g'", value_float);
              PyRun_SimpleString(buf);*/
	   } else if (PyLong_Check(VALUE)) { /* check for long*/
              value_long = PyLong_AsLong(VALUE);
/*	      sprintf(buf, "print 'value = %g'", value_long);
              PyRun_SimpleString(buf);*/
	   } else if (PyFloat_Check(VALUE)) { /*check for double*/
              value_double = PyFloat_AsDouble(VALUE);
/*	      sprintf(buf, "print 'value = %g'", value_double);
              PyRun_SimpleString(buf);*/
	   } else if (PyDict_Check(VALUE)) { /*check for dictionary*/
	      value_str = return_vcs_legacy_list(VALUE, member);
	   }
	}

	/*
	 * Set the appropriate boxfill attribute. But first 
	 * get the boxfill structure.
         */
	get_gfbtab = getGfb(gfbtab->name);
	if (cmpncs(member, "projection") == 0) {
	   strcpy(get_gfbtab->pGfb_attr->proj, value_str);
	} else if (cmpncs(member, "xticlabels1") == 0) {
	   strcpy(get_gfbtab->pGfb_attr->xtl1, value_str);
	} else if (cmpncs(member, "xticlabels2") == 0) {
	   strcpy(get_gfbtab->pGfb_attr->xtl2, value_str);
	} else if (cmpncs(member, "xmtics1") == 0) {
	   strcpy(get_gfbtab->pGfb_attr->xmt1, value_str);
	} else if (cmpncs(member, "xmtics2") == 0) {
	   strcpy(get_gfbtab->pGfb_attr->xmt2, value_str);
	} else if (cmpncs(member, "yticlabels1") == 0) {
	   strcpy(get_gfbtab->pGfb_attr->ytl1, value_str);
	} else if (cmpncs(member, "yticlabels2") == 0) {
	   strcpy(get_gfbtab->pGfb_attr->ytl2, value_str);
	} else if (cmpncs(member, "ymtics1") == 0) {
	   strcpy(get_gfbtab->pGfb_attr->ymt1, value_str);
	} else if (cmpncs(member, "ymtics2") == 0) {
	   strcpy(get_gfbtab->pGfb_attr->ymt2, value_str);
	} else if (cmpncs(member, "datawc_x1") == 0) {
	   get_gfbtab->pGfb_attr->dsp[0] = value_float;
	} else if (cmpncs(member, "datawc_y1") == 0) {
	   get_gfbtab->pGfb_attr->dsp[1] = value_float;
	} else if (cmpncs(member, "datawc_x2") == 0) {
	   get_gfbtab->pGfb_attr->dsp[2] = value_float;
	} else if (cmpncs(member, "datawc_y2") == 0) {
	   get_gfbtab->pGfb_attr->dsp[3] = value_float;
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   get_gfbtab->pGfb_attr->idsp[0] = value_int;
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   get_gfbtab->pGfb_attr->idsp[1] = value_int;
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   get_gfbtab->pGfb_attr->idsp[2] = value_int;
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   get_gfbtab->pGfb_attr->idsp[3] = value_int;
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   get_gfbtab->pGfb_attr->calendar = value_int;
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   strcpy(get_gfbtab->pGfb_attr->timeunits, value_str);
	} else if (cmpncs(member, "xaxisconvert") == 0) {
	   strcpy(get_gfbtab->pGfb_attr->xat, value_str);
	} else if (cmpncs(member, "yaxisconvert") == 0) {
	   strcpy(get_gfbtab->pGfb_attr->yat, value_str);
	} else if (cmpncs(member, "level_1") == 0) {
           get_gfbtab->pGfb_attr->lev1 = value_float;
	} else if (cmpncs(member, "level_2") == 0) {
           get_gfbtab->pGfb_attr->lev2 = value_float;
	} else if (cmpncs(member, "color_1") == 0) {
           get_gfbtab->pGfb_attr->color_1 = value_int;
	} else if (cmpncs(member, "color_2") == 0) {
           get_gfbtab->pGfb_attr->color_2 = value_int;
	} else if (cmpncs(member, "boxfill_type") == 0) {
	   if (cmpncs(value_str, "linear") == 0) 
              get_gfbtab->pGfb_attr->boxfill_type = 0;
/*           else if (cmpncs(value_str, "list") == 0) 
              get_gfbtab->pGfb_attr->boxfill_type = 1;*/
           else if (cmpncs(value_str, "log10") == 0) 
              get_gfbtab->pGfb_attr->boxfill_type = 2;
           else if (cmpncs(value_str, "custom") == 0) 
              get_gfbtab->pGfb_attr->boxfill_type = 3;
	} else if (cmpncs(member, "legend") == 0) {
           if (value_str != NULL) {
	      if (get_gfbtab->pGfb_attr->legend != NULL) {
		 free((char *) get_gfbtab->pGfb_attr->legend);
		 get_gfbtab->pGfb_attr->legend = NULL;
              }
	      get_gfbtab->pGfb_attr->legend = (char *)malloc(strlen(value_str)*sizeof(char)+1);
	      strcpy(get_gfbtab->pGfb_attr->legend, value_str);
	   } else {
              if (get_gfbtab->pGfb_attr->legend != NULL) {
                 free((char *) get_gfbtab->pGfb_attr->legend);
                 get_gfbtab->pGfb_attr->legend = NULL;
              }
	   }
	} else if (cmpncs(member, "ext_1") == 0) {
           get_gfbtab->pGfb_attr->ext_1 = value_int;
	} else if (cmpncs(member, "ext_2") == 0) {
           get_gfbtab->pGfb_attr->ext_2 = value_int;
	} else if (cmpncs(member, "missing") == 0) {
           get_gfbtab->pGfb_attr->missing = value_int;
	} else if (cmpncs(member, "levels") == 0) {
	   /* get the style values */
           Get_Member(GFB,"fillareastyle", "s", &style);

	   /* get the style index values */
           Get_Member(GFB,"fillareaindices", "O", &sindices);

	   /* get the color values */
           Get_Member(GFB,"fillareacolors", "O", &listit);

           /* Free the current fill_range link list */
	   pfiso = next_pfiso = get_gfbtab->pGfb_attr->line;
           while (pfiso != NULL) {
              next_pfiso = pfiso->next;
              free ((char *) pfiso);
              pfiso = next_pfiso;
           }
           get_gfbtab->pGfb_attr->line = tpfiso = NULL;
   
	   if (PyTuple_Check(VALUE)) { /*check for tuple*/
              /* Create the new fill_range link list */
              for (i=0; i<PyTuple_Size(VALUE); i++) {
                 tup = PyTuple_GetItem(VALUE, i);
                 if (PyList_Check(tup)) { /* check for list */
                    for (j=0; j<(PyList_Size(tup)-1); j++) {
                       /* malloc the new iso struct */
                       if ((pfiso_new = (struct fill_range *)malloc(
                                         sizeof(struct fill_range)))==NULL) {
		          sprintf(buf,"No memory for new isofill id(%d).\n",i);
                          PyErr_SetString(PyExc_MemoryError, buf);
                          return NULL;
                       }

                       strcpy(pfiso_new->fill_name, "default");
                       if ((sindices==Py_None) || (sct>=PyList_Size(sindices))) {
                          if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                             color_index = 16+ct;
                          else
                             color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));
                          strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfb_name, ct, style, 0, color_index));
                       } else if ((PyInt_Check(PyList_GetItem(sindices,sct))) ||
                                   (PyFloat_Check(PyList_GetItem(sindices,sct)))) {
                             style_index = (int) PyInt_AsLong(PyList_GetItem(sindices,sct));
                          if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                             color_index = 16+ct;
                          else
                             color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));
                          strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfb_name, ct, style, style_index, color_index));
                       } else { /* must be a fillarea object */
                          Tf_name = PyString_AsString(PyList_GetItem(sindices,sct));
                          strcpy(pfiso_new->fill_name, Tf_name);
                       }
                       pfiso_new->id = sct+1;
                       pfiso_new->lev1 = (float) PyFloat_AsDouble(PyList_GetItem(tup,j));
                       pfiso_new->lev2 = (float) PyFloat_AsDouble(PyList_GetItem(tup,j+1));
                       ct++;sct++;
                       pfiso_new->next = NULL;
          
                       /* Add to the new fill range link list */
                       if (tpfiso == NULL)
                          get_gfbtab->pGfb_attr->line = tpfiso = pfiso_new;
                       else {
                          tpfiso->next = pfiso_new;
                          tpfiso = pfiso_new;
                       }
                    }
                 } else {
		    if ((PyInt_Check(tup)) || (PyFloat_Check(tup))) {
                    /* malloc the new iso struct */
                    if ((pfiso_new = (struct fill_range *)malloc(
                                      sizeof(struct fill_range)))==NULL) {
		       sprintf(buf,"No memory for new isofill id(%d).\n",i);
                       PyErr_SetString(PyExc_MemoryError, buf);
                       return NULL;
                    }

                    strcpy(pfiso_new->fill_name, "default");
                    if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                       color_index = 16+ct;
                    else
                       color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));

		    if ((sindices==Py_None) || (sct>=PyList_Size(sindices))) {
                       style_index = sct;
                       strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfb_name, i, style, style_index, color_index));
                    } else if (PyInt_Check(PyList_GetItem(sindices,sct))) {
                       style_index = (int) PyInt_AsLong(PyList_GetItem(sindices,sct));
                       strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfb_name, i, style, style_index, color_index));
                    } else { /* must be a fillarea object */
                       Tf_name = PyString_AsString(PyList_GetItem(sindices,sct));
                       strcpy(pfiso_new->fill_name, Tf_name);
                    }

                    pfiso_new->id = sct+1;
                    pfiso_new->lev1 = (float) PyFloat_AsDouble(tup);
                    tup = PyTuple_GetItem(VALUE, i+1);
                    pfiso_new->lev2 = (float) PyFloat_AsDouble(tup);
                    ct++;sct++;
                    pfiso_new->next = NULL;

                    /* Add to the new fill range link list */
                    if (tpfiso == NULL)
                       get_gfbtab->pGfb_attr->line = tpfiso = pfiso_new;
                    else {
                       tpfiso->next = pfiso_new;
                       tpfiso = pfiso_new;
                    }
                    if (i == (PyTuple_Size(VALUE)-2)) break;

                 } else {
		    PyErr_SetString(PyExc_ValueError, "Must be either integer or float values.");
                    return NULL;
                 }
              }
            }
           } else if (PyList_Check(VALUE)) { /* check for list */
              /* Create the new fill_range link list */
              for (j=0; j<(PyList_Size(VALUE)-1); j++) {
                 /* malloc the new iso struct */
                 if ((pfiso_new = (struct fill_range *)malloc(
                                   sizeof(struct fill_range)))==NULL) {
                    sprintf(buf,"No memory for new boxfill id(%d).\n",j);
                    PyErr_SetString(PyExc_MemoryError, buf);
                    return NULL;
                 }
                 if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                    color_index = 16+ct;
                 else
                    color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));

                 if ((sindices==Py_None) || (sct>=PyList_Size(sindices))) {
                    style_index = sct;
                    strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfb_name, j, style, style_index, color_index));    
                 } else if (PyInt_Check(PyList_GetItem(sindices,sct))) {
                    style_index = (int) PyInt_AsLong(PyList_GetItem(sindices,sct));
                    strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfb_name, j, style, style_index, color_index));    
                 } else { /* must be a fillarea object */
                    Tf_name = PyString_AsString(PyList_GetItem(sindices,sct));
                    strcpy(pfiso_new->fill_name, Tf_name);
                 }

                 pfiso_new->id = j;
                 pfiso_new->lev1 = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,j));              
                 pfiso_new->lev2 = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,j+1));            
                 ct++; sct++;
                 pfiso_new->next = NULL;
                 
                 /* Add to the new fill range link list */
                 if (tpfiso == NULL)
                    get_gfbtab->pGfb_attr->line = tpfiso = pfiso_new;
                 else {
                    tpfiso->next = pfiso_new;
                    tpfiso = pfiso_new;
                 }
              }
	   }
	}

	chk_mov_Gfb(get_gfbtab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

	/* 
	 * Call the Set_Member function to assign the data
	 * member of the known object by name. This function
	 * can take all the common Python/C data conversion 
	 * types: { "s" = char * : "i" = int   : "l" = long :
	 *          "c" = char   : "f" = float : "d" = double:
	 *          "O" = PyObject * .
	 *
	 * But in this case below, I am sending only the PyObject
	 * down. No need to convert to C then back to PyObject.
	if (member!=NULL)
           Set_Member(GFB, member, "O", VALUE);
	 */

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}
/* 
 * Return the VCS isofill (Gfi) graphics method member value. 
 */
static PyObject *
PyVCS_getGfimember(PyVCScanvas_Object *self, PyObject *args)
{
	char 				*Gfi_name, *member=NULL, buf[1024];
	int 				i=0, ct=0;
	PyObject 			*GFI=NULL, *MEMBER=NULL, *tup, *lp;
	struct gfi_tab          	*gfitab;
	struct fill_range       	*pfiso=NULL;
    	extern struct gfi_tab   	Gfi_tab;
	extern struct table_fill	Tf_tab;
	struct table_fill 		*ftab;

	if(PyArg_ParseTuple(args,"|OO",&GFI, &MEMBER)) {
           if (GFI == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


	Get_Member(GFI,"name", "s", &Gfi_name);
	gfitab=&Gfi_tab;
        while ((gfitab != NULL) &&
               (strcmp(gfitab->name, Gfi_name) != 0))
           gfitab = gfitab->next;

     	if (gfitab == NULL) {
	   sprintf(buf,"Cannot find isofill graphics method Gfi_%s.",Gfi_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) {
           return Py_BuildValue("s", gfitab->pGfi_attr->proj);
	} else if (cmpncs(member, "xticlabels1") == 0) {
           return Py_BuildValue("s", gfitab->pGfi_attr->xtl1);
	} else if (cmpncs(member, "xticlabels2") == 0) {
           return Py_BuildValue("s", gfitab->pGfi_attr->xtl2);
	} else if (cmpncs(member, "xmtics1") == 0) {
           return Py_BuildValue("s", gfitab->pGfi_attr->xmt1);
	} else if (cmpncs(member, "xmtics2") == 0) {
           return Py_BuildValue("s", gfitab->pGfi_attr->xmt2);
	} else if (cmpncs(member, "yticlabels1") == 0) {
           return Py_BuildValue("s", gfitab->pGfi_attr->ytl1);
	} else if (cmpncs(member, "yticlabels2") == 0) {
           return Py_BuildValue("s", gfitab->pGfi_attr->ytl2);
	} else if (cmpncs(member, "ymtics1") == 0) {
           return Py_BuildValue("s", gfitab->pGfi_attr->ymt1);
	} else if (cmpncs(member, "ymtics2") == 0) {
           return Py_BuildValue("s", gfitab->pGfi_attr->ymt2);
	} else if (cmpncs(member, "datawc_y1") == 0) {
           return Py_BuildValue("f",gfitab->pGfi_attr->dsp[1]);
	} else if (cmpncs(member, "datawc_y2") == 0) {
           return Py_BuildValue("f",gfitab->pGfi_attr->dsp[3]);
	} else if (cmpncs(member, "datawc_x1") == 0) {
           return Py_BuildValue("f",gfitab->pGfi_attr->dsp[0]);
	} else if (cmpncs(member, "datawc_x2") == 0) {
           return Py_BuildValue("f",gfitab->pGfi_attr->dsp[2]);
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
           return Py_BuildValue("i",gfitab->pGfi_attr->idsp[1]);
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
           return Py_BuildValue("i",gfitab->pGfi_attr->idsp[3]);
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
           return Py_BuildValue("i",gfitab->pGfi_attr->idsp[0]);
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
           return Py_BuildValue("i",gfitab->pGfi_attr->idsp[2]);
	} else if (cmpncs(member, "datawc_calendar") == 0) {
           return Py_BuildValue("i",gfitab->pGfi_attr->calendar);
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
           return Py_BuildValue("s",gfitab->pGfi_attr->timeunits);
	} else if ((cmpncs(member, "xaxisconvert") == 0) &&
                   ((cmpncs(gfitab->pGfi_attr->xat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "xaxisconvert") == 0) {
           return Py_BuildValue("s", gfitab->pGfi_attr->xat);
	} else if ((cmpncs(member, "yaxisconvert") == 0) &&
                   ((cmpncs(gfitab->pGfi_attr->yat, "\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "yaxisconvert") == 0) {
           return Py_BuildValue("s", gfitab->pGfi_attr->yat);
	} else if (cmpncs(member, "missing") == 0) {
           return Py_BuildValue("f", gfitab->pGfi_attr->missing);
	} else if (cmpncs(member, "levels") == 0) {
           /* Get the fill isoline structure */
           pfiso = gfitab->pGfi_attr->line;
           while (pfiso != NULL) {
              pfiso = pfiso->next;
	      ct++;
	   }
           pfiso = gfitab->pGfi_attr->line;
           tup = PyTuple_New(ct);
           while (pfiso != NULL) {
              lp = Py_BuildValue("[d,d]", pfiso->lev1, pfiso->lev2);
              PyTuple_SetItem(tup, i, lp);
              pfiso = pfiso->next;
              i++;
           }
           return tup;
	} else if (cmpncs(member, "fillareacolors") == 0) {
           /* Get the fill isoline structure */
           pfiso = gfitab->pGfi_attr->line;
           while (pfiso != NULL) {
              pfiso = pfiso->next;
	      ct++;
	   }
           pfiso = gfitab->pGfi_attr->line;
           tup = PyList_New(ct);
           while (pfiso != NULL) {
	       for (ftab=&Tf_tab; ftab!=NULL && cmpnbl(pfiso->fill_name,ftab->name)!=0;
                    ftab=ftab->next);
               lp = Py_BuildValue("i", ftab->faci[0]);
               PyList_SetItem(tup, i, lp);
               pfiso = pfiso->next;
               i++;
           }
           return tup;
/*	} else if (cmpncs(member, "fillareaindices") == 0) {   Not used at the moment maybe later when
           * Get the fill isoline structure *                  Hatch cgm output is fixed.
           pfiso = gfitab->pGfi_attr->line;
           while (pfiso != NULL) {
              pfiso = pfiso->next;
	      ct++;
	   }
           pfiso = gfitab->pGfi_attr->line;
           tup = PyList_New(ct);
           while (pfiso != NULL) {
	       for (ftab=&Tf_tab; ftab!=NULL && cmpnbl(pfiso->fill_name,ftab->name)!=0;
                    ftab=ftab->next);
               lp = Py_BuildValue("i", ftab->fasi[0]);
               PyList_SetItem(tup, i, lp);
               pfiso = pfiso->next;
               i++;
           }
           return tup;    */
	}  else if (cmpncs(member, "legend") == 0) {
	    if ((gfitab->pGfi_attr->legend == NULL) || (cmpnbl(gfitab->pGfi_attr->legend, "") == 0))
	       return Py_BuildValue("");
	    else
	       return PyVCS_list_to_PyDict(gfitab->pGfi_attr->legend);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* Create and return the name of the new fillarea attribute */
char * return_new_fillarea_attribute(Gfi_name,id, style, style_index, color_index)
char *Gfi_name;
int  id;
char *style;
int  style_index;
int  color_index;
{
	int				j,ct=0;
	char 				buf[1024], *fill_name;
	Gflfac                  	hat_table;
	Gintlist                	pat_table;
        struct table_fill               *pf,*p1;
	extern struct table_fill 	Tf_tab;
        extern int                      chk_mov_Tf();

	/* first does the little magic on style_index to remove indices 17 and 19 and convert them to 18 and 20 */
	if (style_index == 18) 
	  style_index = 20;
	else if (style_index==17)
	  style_index = 18;
        /*
         * Set new attributes for Tf.
         * Create a new fillarea structure and copy to it.
         */
        if((pf=(struct table_fill *)malloc(sizeof(struct table_fill)))==NULL) {
            PyErr_SetString(PyExc_MemoryError, "No memory for new fillarea attribute.");
            return NULL;
        }

	/* Set the style index of the hatch or pattern */
	/* nullify the set of attributes               */
        pf->priority = 1;
        for (j=0; j < 4; j++) {
            if (j == 0 || j == 2) {
               pf->fvp[j]=0.0;
               pf->fwc[j]=0.0;
            } else {
               pf->fvp[j]=1.0;
               pf->fwc[j]=1.0;
            }
        }
	strcpy(pf->proj,"default");
        pf->fx = NULL;
        pf->fy = NULL;
        pf->x=0.0;
        pf->y=0.0;
        pf->w=0.1;
        pf->h=0.1;
        pf->fais = NULL; pf->fais_size = 0;
        pf->fasi = NULL; pf->fasi_size = 0;
        pf->faci = NULL; pf->faci_size = 0;

        /* Set the fillarea's name */
	/*sprintf(buf,"%s_%d", Gfi_name, id);*/
	for (p1=&Tf_tab;p1 != NULL;p1=p1->next)
           ct++;
	sprintf(buf,"GEN_%d", ct);
        strcpy(pf->name,buf);

        /* Set the interior style to solid */
	if (cmpncs(style, "solid") == 0) {
           if (pf->fais!=NULL) { free((char *) pf->fais); pf->fais=NULL; }
           if (pf->fasi!=NULL) { free((char *) pf->fasi); pf->fasi=NULL; }
           if((pf->fais=(int *) malloc(sizeof(int)))==NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for fill values not found.");
                  return NULL;
           }
           if ((pf->fasi = (int *) malloc( sizeof(int)))== NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for fill values not found.");
                  return NULL;
           }
           pf->fais[0] = 1; pf->fais_size = 1;
           pf->fasi[0] = 1; pf->fasi_size = 1;
	} else if (cmpncs(style, "pattern") == 0) {
           if (pf->fais!=NULL) { free((char *) pf->fais); pf->fais=NULL; }
           if (pf->fasi!=NULL) { free((char *) pf->fasi); pf->fasi=NULL; }
           if ((pf->fais = (int *) malloc( sizeof(int)))== NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for fill values not found.");
                  return NULL;
           }
           if ((pf->fasi = (int *) malloc( sizeof(int)))== NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for fill values not found.");
                  return NULL;
           }
           pf->fais[0] = 2; pf->fais_size = 1;
           /* Below was to difficult. I changed the code in xgks */
/*           gqepai(104, &pat_table); * Inquire the list of pattern indices *
           pf->fasi[0] = pat_table.integers[style_index-1]; *Set pattern style*/
           pf->fasi[0] = style_index;
           pf->fasi_size = 1;
           /*pf->fasi=style_index;*/
	} else if (cmpncs(style, "hatch") == 0) {
           if (pf->fais!=NULL) { free((char *) pf->fais); pf->fais=NULL; }
           if (pf->fasi!=NULL) { free((char *) pf->fasi); pf->fasi=NULL; }
           if ((pf->fais = (int *) malloc( sizeof(int)))== NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for fill values not found.");
                  return NULL;
           }
           if ((pf->fasi = (int *) malloc( sizeof(int)))== NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for fill values not found.");
                  return NULL;
           }
           pf->fais[0] = 3;pf->fais_size = 1;
           /* Below was to difficult. I changed the code in xgks */
/*           gqfaf("CANVAS", &hat_table); * Inquire the fill area facilities *
           pf->fasi[0]=hat_table.hatches.integers[style_index-1];*Set hatch style*/
           pf->fasi[0]= style_index;
           pf->fasi_size = 1;
	}

        /* Set the color index used for the fillarea */
        if (pf->faci!=NULL) { free((char *) pf->faci); pf->faci=NULL; }
        if ((pf->faci = (int *) malloc( sizeof(int)) )== NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for fill values not found.");
                  return NULL;
        }
        pf->faci[0] = color_index; pf->faci_size = 1;

        /* Set the new structure in the list */
        chk_mov_Tf (pf);
	return pf->name;
}

/*
 * Find the existing isofill graphics method and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setGfimember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			i,j=0,n,ct=0,sct=0,color_index,style_index;
	int 			MODE, value_int;	
	long 			value_long;
	float 			value_float;
	double 			value_double;
	char			*Tf_name;
	char 			buf[1024], *style;
        char 			*Gfi_name, *str=NULL, *member=NULL;
	char			*value_str=NULL;
        PyObject 		*GFI=NULL, *MEMBER=NULL, *VALUE=NULL;
        PyObject		*listit,*tup, *sindices;
	struct gfi_tab          *get_gfitab=NULL;
	extern int              update_ind;
	struct fill_range       *pfiso, *next_pfiso, *pfiso_new, *tpfiso;
        struct gfi_tab          *gfitab;
        extern struct gfi_tab   Gfi_tab;
	extern struct gfi_tab   *getGfi();
	extern int              chk_mov_Gfi();
	extern int 		vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &GFI, &MEMBER, &VALUE, &MODE)) {
           if (GFI == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(GFI,"name", "s", &Gfi_name);
        gfitab=&Gfi_tab;
        while ((gfitab != NULL) &&
               (strcmp(gfitab->name, Gfi_name) != 0))
           gfitab = gfitab->next;

	if (MEMBER != NULL) {
           member = PyString_AsString(MEMBER);
/*	   sprintf(buf, "print 'member = %s'", member);
           PyRun_SimpleString(buf);*/
	}

	if (VALUE != NULL) {
	   if (PyString_Check(VALUE)) { /*check string*/
              value_str = PyString_AsString(VALUE);
	   } else if (PyInt_Check(VALUE)) { /*check for int*/
              value_int = (int) PyInt_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for float*/
              value_float = (float) PyFloat_AsDouble(VALUE);
	   } else if (PyLong_Check(VALUE)) { /* check for long*/
              value_long = PyLong_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for double*/
              value_double = PyFloat_AsDouble(VALUE);
	   } else if (PyDict_Check(VALUE)) { /*check for dictionary*/
	      value_str = return_vcs_legacy_list(VALUE, member);
	   }
	}

	/*
	 * Set the appropriate isofill attribute. But first 
	 * get the isofill structure.
         */
	get_gfitab = getGfi(gfitab->name);
	if (cmpncs(member, "projection") == 0) {
	   strcpy(get_gfitab->pGfi_attr->proj, value_str);
	} else if (cmpncs(member, "xticlabels1") == 0) {
	   strcpy(get_gfitab->pGfi_attr->xtl1, value_str);
	} else if (cmpncs(member, "xticlabels2") == 0) {
	   strcpy(get_gfitab->pGfi_attr->xtl2, value_str);
	} else if (cmpncs(member, "xmtics1") == 0) {
	   strcpy(get_gfitab->pGfi_attr->xmt1, value_str);
	} else if (cmpncs(member, "xmtics2") == 0) {
	   strcpy(get_gfitab->pGfi_attr->xmt2, value_str);
	} else if (cmpncs(member, "yticlabels1") == 0) {
	   strcpy(get_gfitab->pGfi_attr->ytl1, value_str);
	} else if (cmpncs(member, "yticlabels2") == 0) {
	   strcpy(get_gfitab->pGfi_attr->ytl2, value_str);
	} else if (cmpncs(member, "ymtics1") == 0) {
	   strcpy(get_gfitab->pGfi_attr->ymt1, value_str);
	} else if (cmpncs(member, "ymtics2") == 0) {
	   strcpy(get_gfitab->pGfi_attr->ymt2, value_str);
	} else if (cmpncs(member, "datawc_x1") == 0) {
	   get_gfitab->pGfi_attr->dsp[0] = value_float;
	} else if (cmpncs(member, "datawc_y1") == 0) {
	   get_gfitab->pGfi_attr->dsp[1] = value_float;
	} else if (cmpncs(member, "datawc_x2") == 0) {
	   get_gfitab->pGfi_attr->dsp[2] = value_float;
	} else if (cmpncs(member, "datawc_y2") == 0) {
	   get_gfitab->pGfi_attr->dsp[3] = value_float;
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   get_gfitab->pGfi_attr->idsp[0] = value_int;
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   get_gfitab->pGfi_attr->idsp[1] = value_int;
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   get_gfitab->pGfi_attr->idsp[2] = value_int;
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   get_gfitab->pGfi_attr->idsp[3] = value_int;
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   get_gfitab->pGfi_attr->calendar = value_int;
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   strcpy(get_gfitab->pGfi_attr->timeunits, value_str);
	} else if (cmpncs(member, "xaxisconvert") == 0) {
	   strcpy(get_gfitab->pGfi_attr->xat, value_str);
	} else if (cmpncs(member, "yaxisconvert") == 0) {
	   strcpy(get_gfitab->pGfi_attr->yat, value_str);
	} else if (cmpncs(member, "missing") == 0) {
	   get_gfitab->pGfi_attr->missing = value_int;
	} else if (cmpncs(member, "legend") == 0) {
           if (value_str != NULL) {
              if (get_gfitab->pGfi_attr->legend != NULL) {
                 free((char *) get_gfitab->pGfi_attr->legend);
                 get_gfitab->pGfi_attr->legend = NULL;
              }
	      get_gfitab->pGfi_attr->legend = (char *)malloc(strlen(value_str)*sizeof(char)+1);
	      strcpy(get_gfitab->pGfi_attr->legend, value_str);
	   } else {
              if (get_gfitab->pGfi_attr->legend != NULL) {
                 free((char *) get_gfitab->pGfi_attr->legend);
                 get_gfitab->pGfi_attr->legend = NULL;
              }
	   }
	} else if (cmpncs(member, "levels") == 0) {
	   /* get the style values */
           Get_Member(GFI,"fillareastyle", "s", &style);

	   /* get the style index values */
           Get_Member(GFI,"fillareaindices", "O", &sindices);

	   /* get the color values */
           Get_Member(GFI,"fillareacolors", "O", &listit);

           /* Free the current fill_range link list */
	   pfiso = next_pfiso = get_gfitab->pGfi_attr->line;
           while (pfiso != NULL) {
              next_pfiso = pfiso->next;
              free ((char *) pfiso);
              pfiso = next_pfiso;
           }
           get_gfitab->pGfi_attr->line = tpfiso = NULL;
   
	   if (PyTuple_Check(VALUE)) { /*check for tuple*/
              /* Create the new fill_range link list */
              for (i=0; i<PyTuple_Size(VALUE); i++) {
                 tup = PyTuple_GetItem(VALUE, i);
                 if (PyList_Check(tup)) { /* check for list */
                    for (j=0; j<(PyList_Size(tup)-1); j++) {
                       /* malloc the new iso struct */
                       if ((pfiso_new = (struct fill_range *)malloc(
                                         sizeof(struct fill_range)))==NULL) {
		          sprintf(buf,"No memory for new isofill id(%d).\n",i);
                          PyErr_SetString(PyExc_MemoryError, buf);
                          return NULL;
                       }

                       strcpy(pfiso_new->fill_name, "default");
                       if ((sindices==Py_None) || (sct>=PyList_Size(sindices))) {
                          if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                             color_index = 16+ct;
                          else
                             color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));
                          strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfi_name, ct, style, 0, color_index));
                       } else if ((PyInt_Check(PyList_GetItem(sindices,sct))) ||
                                   (PyFloat_Check(PyList_GetItem(sindices,sct)))) {
                             style_index = (int) PyInt_AsLong(PyList_GetItem(sindices,sct));
                          if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                             color_index = 16+ct;
                          else
                             color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));
                          strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfi_name, ct, style, style_index, color_index));
                       } else { /* must be a fillarea object */
                          Tf_name = PyString_AsString(PyList_GetItem(sindices,sct));
                          strcpy(pfiso_new->fill_name, Tf_name);
                       }
                       pfiso_new->id = sct+1;
                       pfiso_new->lev1 = (float) PyFloat_AsDouble(PyList_GetItem(tup,j));
                       pfiso_new->lev2 = (float) PyFloat_AsDouble(PyList_GetItem(tup,j+1));
                       ct++;sct++;
                       pfiso_new->next = NULL;
          
                       /* Add to the new fill range link list */
                       if (tpfiso == NULL)
                          get_gfitab->pGfi_attr->line = tpfiso = pfiso_new;
                       else {
                          tpfiso->next = pfiso_new;
                          tpfiso = pfiso_new;
                       }
                    }
                 } else {
		    if ((PyInt_Check(tup)) || (PyFloat_Check(tup))) {
                    /* malloc the new iso struct */
                    if ((pfiso_new = (struct fill_range *)malloc(
                                      sizeof(struct fill_range)))==NULL) {
		       sprintf(buf,"No memory for new isofill id(%d).\n",i);
                       PyErr_SetString(PyExc_MemoryError, buf);
                       return NULL;
                    }

                    strcpy(pfiso_new->fill_name, "default");
                    if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                       color_index = 16+ct;
                    else
                       color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));

		    if ((sindices==Py_None) || (sct>=PyList_Size(sindices))) {
                       style_index = sct;
                       strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfi_name, i, style, style_index, color_index));
                    } else if (PyInt_Check(PyList_GetItem(sindices,sct))) {
                       style_index = (int) PyInt_AsLong(PyList_GetItem(sindices,sct));
                       strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfi_name, i, style, style_index, color_index));
                    } else { /* must be a fillarea object */
                       Tf_name = PyString_AsString(PyList_GetItem(sindices,sct));
                       strcpy(pfiso_new->fill_name, Tf_name);
                    }

                    pfiso_new->id = sct+1;
                    pfiso_new->lev1 = (float) PyFloat_AsDouble(tup);
                    tup = PyTuple_GetItem(VALUE, i+1);
                    pfiso_new->lev2 = (float) PyFloat_AsDouble(tup);
                    ct++;sct++;
                    pfiso_new->next = NULL;

                    /* Add to the new fill range link list */
                    if (tpfiso == NULL)
                       get_gfitab->pGfi_attr->line = tpfiso = pfiso_new;
                    else {
                       tpfiso->next = pfiso_new;
                       tpfiso = pfiso_new;
                    }
                    if (i == (PyTuple_Size(VALUE)-2)) break;

                 } else {
		    PyErr_SetString(PyExc_ValueError, "Must be either integer or float values.");
                    return NULL;
                 }
              }
            }
           } else if (PyList_Check(VALUE)) { /* check for list */
              /* Create the new fill_range link list */
              for (j=0; j<(PyList_Size(VALUE)-1); j++) {
                 /* malloc the new iso struct */
                 if ((pfiso_new = (struct fill_range *)malloc(
                                   sizeof(struct fill_range)))==NULL) {
                    sprintf(buf,"No memory for new isofill id(%d).\n",j);
                    PyErr_SetString(PyExc_MemoryError, buf);
                    return NULL;
                 }
                 if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                    color_index = 16+ct;
                 else
                    color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));

                 if ((sindices==Py_None) || (sct>=PyList_Size(sindices))) {
                    style_index = sct;
                    strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfi_name, j, style, style_index, color_index));    
                 } else if (PyInt_Check(PyList_GetItem(sindices,sct))) {
                    style_index = (int) PyInt_AsLong(PyList_GetItem(sindices,sct));
                    strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfi_name, j, style, style_index, color_index));    
                 } else { /* must be a fillarea object */
                    Tf_name = PyString_AsString(PyList_GetItem(sindices,sct));
                    strcpy(pfiso_new->fill_name, Tf_name);
                 }

                 pfiso_new->id = j;
                 pfiso_new->lev1 = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,j));              
                 pfiso_new->lev2 = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,j+1));            
                 ct++; sct++;
                 pfiso_new->next = NULL;
                 
                 /* Add to the new fill range link list */
                 if (tpfiso == NULL)
                    get_gfitab->pGfi_attr->line = tpfiso = pfiso_new;
                 else {
                    tpfiso->next = pfiso_new;
                    tpfiso = pfiso_new;
                 }
              }
	   }
	}

	chk_mov_Gfi(get_gfitab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

	/* 
	 * Call the Set_Member function to assign the data
	 * member of the known object by name. This function
	 * can take all the common Python/C data conversion 
	 * types: { "s" = char * : "i" = int   : "l" = long :
	 *          "c" = char   : "f" = float : "d" = double:
	 *          "O" = PyObject * .
	 *
	 * But in this case below, I am sending only the PyObject
	 * down. No need to convert to C then back to PyObject.
	if (member!=NULL)
           Set_Member(GFI, member, "O", VALUE);
	 */

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new isofill graphics method by copying from an existing
 * isofill graphics method. If no source copy name argument is given,
 * then the default isofill graphics method will be used to replicate
 * the new isofill graphics method.
 */
static PyObject *
PyVCS_copyGfi(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GFI_SRC=NULL, *GFI_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Gfi_name();
              
        if(PyArg_ParseTuple(args,"|ss", &GFI_SRC, &GFI_NAME)) {
           if (GFI_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source isofill graphics method name.");
                 return NULL;
           }

           if (GFI_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", GFI_NAME);
        }

        ierr = copy_Gfi_name(GFI_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating isofill graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing isofill graphics method.
 */
static PyObject *
PyVCS_renameGfi(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GFI_OLD_NAME=NULL, *GFI_NEW_NAME=NULL;
        extern int      renameGfi_name();
 
        if(PyArg_ParseTuple(args,"|ss", &GFI_OLD_NAME, &GFI_NEW_NAME)) {
           if ((GFI_OLD_NAME == NULL) || (GFI_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new isofill graphics method name.");
                 return NULL;
           }
        }

        ierr = renameGfi_name(GFI_OLD_NAME, GFI_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming isofill graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;


}

/* 
 * Remove an existing isofill graphics method.
 */
static PyObject *
PyVCS_removeGfi(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        /* Return NULL Python Object or Python String Object */
        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the isofill file name.");
                 return NULL;
           }
        }
	if (removeGfi_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed isofill object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The isofill object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing isofill graphics method.
 */
static PyObject *
PyVCS_scriptGfi(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GFI_NAME=NULL,*MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_isofill();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &GFI_NAME, &SCRIPT_NAME, &MODE)) {
           if (GFI_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the isofill name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
	if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
		/* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_isofill(fp, GFI_NAME) == 0) {
              sprintf(buf, "Error - Cannot save isofill script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS isoline (Gi) graphics method member value. 
 */
static PyObject *
PyVCS_getGimember(PyVCScanvas_Object *self, PyObject *args)
{
	char 				*Gi_name, *member=NULL, buf[1024];
	int 				i=0, ct=0;
	PyObject 			*GI=NULL, *MEMBER=NULL, *tup, *lp;
	struct gi_tab          		*gitab;
	struct iso              	*piso=NULL;
    	extern struct gi_tab   		Gi_tab;
        struct table_line               *ltab;
	extern struct table_line 	Tl_tab;

	if(PyArg_ParseTuple(args,"|OO",&GI, &MEMBER)) {
           if (GI == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


	Get_Member(GI,"name", "s", &Gi_name);
	gitab=&Gi_tab;
        while ((gitab != NULL) &&
               (strcmp(gitab->name, Gi_name) != 0))
           gitab = gitab->next;

     	if (gitab == NULL) {
	   sprintf(buf,"Cannot find isofill graphics method Gi_%s.",Gi_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) {
           return Py_BuildValue("s", gitab->pGi_attr->proj);
	} else if (cmpncs(member, "xticlabels1") == 0) {
           return Py_BuildValue("s", gitab->pGi_attr->xtl1);
	} else if (cmpncs(member, "xticlabels2") == 0) {
           return Py_BuildValue("s", gitab->pGi_attr->xtl2);
	} else if (cmpncs(member, "xmtics1") == 0) {
           return Py_BuildValue("s", gitab->pGi_attr->xmt1);
	} else if (cmpncs(member, "xmtics2") == 0) {
           return Py_BuildValue("s", gitab->pGi_attr->xmt2);
	} else if (cmpncs(member, "yticlabels1") == 0) {
           return Py_BuildValue("s", gitab->pGi_attr->ytl1);
	} else if (cmpncs(member, "yticlabels2") == 0) {
           return Py_BuildValue("s", gitab->pGi_attr->ytl2);
	} else if (cmpncs(member, "ymtics1") == 0) {
           return Py_BuildValue("s", gitab->pGi_attr->ymt1);
	} else if (cmpncs(member, "ymtics2") == 0) {
           return Py_BuildValue("s", gitab->pGi_attr->ymt2);
	} else if (cmpncs(member, "datawc_y1") == 0) {
           return Py_BuildValue("f",gitab->pGi_attr->dsp[1]);
	} else if (cmpncs(member, "datawc_y2") == 0) {
           return Py_BuildValue("f",gitab->pGi_attr->dsp[3]);
	} else if (cmpncs(member, "datawc_x1") == 0) {
           return Py_BuildValue("f",gitab->pGi_attr->dsp[0]);
	} else if (cmpncs(member, "datawc_x2") == 0) {
           return Py_BuildValue("f",gitab->pGi_attr->dsp[2]);
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   return Py_BuildValue("i",gitab->pGi_attr->idsp[0]);
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   return Py_BuildValue("i",gitab->pGi_attr->idsp[1]);
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   return Py_BuildValue("i",gitab->pGi_attr->idsp[2]);
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   return Py_BuildValue("i",gitab->pGi_attr->idsp[3]);
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   return Py_BuildValue("i",gitab->pGi_attr->calendar);
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   return Py_BuildValue("s",gitab->pGi_attr->timeunits);
	} else if ((cmpncs(member, "xaxisconvert") == 0) &&
                   ((cmpncs(gitab->pGi_attr->xat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "xaxisconvert") == 0) {
           return Py_BuildValue("s", gitab->pGi_attr->xat);
	} else if ((cmpncs(member, "yaxisconvert") == 0) &&
                   ((cmpncs(gitab->pGi_attr->yat, "\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "yaxisconvert") == 0) {
           return Py_BuildValue("s", gitab->pGi_attr->yat);
	} else if (cmpncs(member, "label") == 0) {
           if (gitab->pGi_attr->labels == 121)
              return Py_BuildValue("s", "y");
           else if (gitab->pGi_attr->labels == 110)
              return Py_BuildValue("s", "n");
	} else if (cmpncs(member, "level") == 0) {
           /* Get the isoline structure */
           piso = gitab->pGi_attr->line;
           while (piso != NULL) {
              piso = piso->next;
	      ct++;
	   }
           piso = gitab->pGi_attr->line;
           tup = PyList_New(ct);
           while (piso != NULL) {
              lp = Py_BuildValue("[d,d]", piso->lev,piso->incr);
              PyList_SetItem(tup, i, lp);
              piso = piso->next;
              i++;
           }
           return tup;
	} else if (cmpncs(member, "linecolors") == 0) {
           /* Get the isoline structure */
           piso = gitab->pGi_attr->line;
           while (piso != NULL) {
              piso = piso->next;
	      ct++;
	   }
           piso = gitab->pGi_attr->line;
           tup = PyList_New(ct);
           while (piso != NULL) {
	       for (ltab=&Tl_tab; ltab!=NULL && cmpnbl(piso->lb,ltab->name)!=0;
                    ltab=ltab->next);
               lp = Py_BuildValue("i", ltab->lci[0]);
               PyList_SetItem(tup, i, lp);
               piso = piso->next;
               i++;
           }
           return tup;
	} else if (cmpncs(member, "line") == 0) {
           /* Get the isoline structure */
           piso = gitab->pGi_attr->line;
           while (piso != NULL) {
              piso = piso->next;
	      ct++;
	   }
           piso = gitab->pGi_attr->line;
           tup = PyList_New(ct);
           while (piso != NULL) {
	       for (ltab=&Tl_tab; ltab!=NULL && cmpnbl(piso->lb,ltab->name)!=0;
                    ltab=ltab->next);
	       if (ltab->ltyp[0] == 1)
                  lp = Py_BuildValue("s", "solid");
	       else if (ltab->ltyp[0] == 2)
                  lp = Py_BuildValue("s", "dash");
	       else if (ltab->ltyp[0] == 3)
                  lp = Py_BuildValue("s", "dot");
	       else if (ltab->ltyp[0] == 4)
                  lp = Py_BuildValue("s", "dash-dot");
	       else if (ltab->ltyp[0] == -3)
                  lp = Py_BuildValue("s", "long-dash");
               PyList_SetItem(tup, i, lp);
               piso = piso->next;
               i++;
           }
           return tup;
	} else if (cmpncs(member, "linewidths") == 0) {
           /* Get the isoline structure */
           piso = gitab->pGi_attr->line;
           while (piso != NULL) {
              piso = piso->next;
	      ct++;
	   }
           piso = gitab->pGi_attr->line;
           tup = PyList_New(ct);
           while (piso != NULL) {
	       for (ltab=&Tl_tab; ltab!=NULL && cmpnbl(piso->lb,ltab->name)!=0;
                    ltab=ltab->next);
               lp = Py_BuildValue("f", ltab->lwsf[0]);
               PyList_SetItem(tup, i, lp);
               piso = piso->next;
               i++;
           }
           return tup;
	} else if (cmpncs(member, "clockwise") == 0) {
           /* Get the isoline structure */
           piso = gitab->pGi_attr->line;
           while (piso != NULL) {
              piso = piso->next;
	      ct++;
	   }
           piso = gitab->pGi_attr->line;
           tup = PyList_New(ct);
           while (piso != NULL) {
               lp = Py_BuildValue("i", piso->cw);
               PyList_SetItem(tup, i, lp);
               piso = piso->next;
               i++;
           }
           return tup;
	} else if (cmpncs(member, "scale") == 0) {
           /* Get the isoline structure */
           piso = gitab->pGi_attr->line;
           while (piso != NULL) {
              piso = piso->next;
	      ct++;
	   }
           piso = gitab->pGi_attr->line;
           tup = PyList_New(ct);
           while (piso != NULL) {
               lp = Py_BuildValue("f", piso->ls);
               PyList_SetItem(tup, i, lp);
               piso = piso->next;
               i++;
           }
           return tup;
	} else if (cmpncs(member, "angle") == 0) {
           /* Get the isoline structure */
           piso = gitab->pGi_attr->line;
           while (piso != NULL) {
              piso = piso->next;
	      ct++;
	   }
           piso = gitab->pGi_attr->line;
           tup = PyList_New(ct);
           while (piso != NULL) {
               lp = Py_BuildValue("f", piso->angle);
               PyList_SetItem(tup, i, lp);
               piso = piso->next;
               i++;
           }
           return tup;
	} else if (cmpncs(member, "spacing") == 0) {
           /* Get the isoline structure */
           piso = gitab->pGi_attr->line;
           while (piso != NULL) {
              piso = piso->next;
	      ct++;
	   }
           piso = gitab->pGi_attr->line;
           tup = PyList_New(ct);
           while (piso != NULL) {
               lp = Py_BuildValue("f", piso->spc);
               PyList_SetItem(tup, i, lp);
               piso = piso->next;
               i++;
           }
           return tup;
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Return the VCS meshfill (Gfm) graphics method member value. 
 */
static PyObject *
PyVCS_getGfmmember(PyVCScanvas_Object *self, PyObject *args)
{
	char *Gfm_name, *member=NULL, buf[1024];
	int i=0, ct=0;
	PyObject *GFM=NULL, *MEMBER=NULL, *tup, *lp;
	struct gfm_tab          *gfmtab;
	struct fill_range       *pfiso=NULL;
    	extern struct gfm_tab   Gfm_tab;

	if(PyArg_ParseTuple(args,"|OO",&GFM, &MEMBER)) {
           if (GFM == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


	Get_Member(GFM,"name", "s", &Gfm_name);
	gfmtab=&Gfm_tab;
        while ((gfmtab != NULL) &&
               (strcmp(gfmtab->name, Gfm_name) != 0))
           gfmtab = gfmtab->next;

     	if (gfmtab == NULL) {
	   sprintf(buf,"Cannot find meshfill graphics method Gfm_%s.",Gfm_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}


	if (cmpncs(member, "projection") == 0) {
           return Py_BuildValue("s", gfmtab->pGfm_attr->proj);
	} else if (cmpncs(member, "xticlabels1") == 0) {
           return Py_BuildValue("s", gfmtab->pGfm_attr->xtl1);
	} else if (cmpncs(member, "xticlabels2") == 0) {
           return Py_BuildValue("s", gfmtab->pGfm_attr->xtl2);
	} else if (cmpncs(member, "xmtics1") == 0) {
           return Py_BuildValue("s", gfmtab->pGfm_attr->xmt1);
	} else if (cmpncs(member, "xmtics2") == 0) {
           return Py_BuildValue("s", gfmtab->pGfm_attr->xmt2);
	} else if (cmpncs(member, "yticlabels1") == 0) {
           return Py_BuildValue("s", gfmtab->pGfm_attr->ytl1);
	} else if (cmpncs(member, "yticlabels2") == 0) {
           return Py_BuildValue("s", gfmtab->pGfm_attr->ytl2);
	} else if (cmpncs(member, "ymtics1") == 0) {
           return Py_BuildValue("s", gfmtab->pGfm_attr->ymt1);
	} else if (cmpncs(member, "ymtics2") == 0) {
           return Py_BuildValue("s", gfmtab->pGfm_attr->ymt2);
	} else if (cmpncs(member, "datawc_y1") == 0) {
           return Py_BuildValue("f",gfmtab->pGfm_attr->dsp[1]);
	} else if (cmpncs(member, "datawc_y2") == 0) {
           return Py_BuildValue("f",gfmtab->pGfm_attr->dsp[3]);
	} else if (cmpncs(member, "datawc_x1") == 0) {
           return Py_BuildValue("f",gfmtab->pGfm_attr->dsp[0]);
	} else if (cmpncs(member, "datawc_x2") == 0) {
           return Py_BuildValue("f",gfmtab->pGfm_attr->dsp[2]);
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
           return Py_BuildValue("i",gfmtab->pGfm_attr->idsp[1]);
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
           return Py_BuildValue("i",gfmtab->pGfm_attr->idsp[3]);
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
           return Py_BuildValue("i",gfmtab->pGfm_attr->idsp[0]);
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
           return Py_BuildValue("i",gfmtab->pGfm_attr->idsp[2]);
	} else if (cmpncs(member, "datawc_calendar") == 0) {
           return Py_BuildValue("i",gfmtab->pGfm_attr->calendar);
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
           return Py_BuildValue("s",gfmtab->pGfm_attr->timeunits);
	} else if ((cmpncs(member, "xaxisconvert") == 0) &&
                   ((cmpncs(gfmtab->pGfm_attr->xat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "xaxisconvert") == 0) {
           return Py_BuildValue("s", gfmtab->pGfm_attr->xat);
	} else if ((cmpncs(member, "yaxisconvert") == 0) &&
                   ((cmpncs(gfmtab->pGfm_attr->yat, "\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "yaxisconvert") == 0) {
           return Py_BuildValue("s", gfmtab->pGfm_attr->yat);
	} else if (cmpncs(member, "missing") == 0) {
           return Py_BuildValue("i", gfmtab->pGfm_attr->missing);
	} else if (cmpncs(member, "mesh") == 0) {
           return Py_BuildValue("i", gfmtab->pGfm_attr->mesh);
	} else if (cmpncs(member, "wrap") == 0) {
           return Py_BuildValue("[f,f]", gfmtab->pGfm_attr->ywrap,gfmtab->pGfm_attr->xwrap);
	} else if (cmpncs(member, "levels") == 0) {
           /* Get the fill isoline structure */
	  ct=0;
           pfiso = gfmtab->pGfm_attr->line;
           while (pfiso != NULL) {
              pfiso = pfiso->next;
	      ct++;
	   }
           pfiso = gfmtab->pGfm_attr->line;
           tup = PyTuple_New(ct);
	   i=0;
           while (pfiso != NULL) {
              lp = Py_BuildValue("[d,d]", pfiso->lev1, pfiso->lev2);
              PyTuple_SetItem(tup, i, lp);
              pfiso = pfiso->next;
              i++;
           }
           return tup;
	
	}  else if (cmpncs(member, "legend") == 0) {
	    if ((gfmtab->pGfm_attr->legend == NULL) || (cmpnbl(gfmtab->pGfm_attr->legend, "") == 0))
	      {
		return Py_BuildValue("");
	      }
	    else
	      {
		return PyVCS_list_to_PyDict(gfmtab->pGfm_attr->legend);
	      }
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* Create and return the name of the new fillarea attribute */
/* char * return_new_fillarea_attribute2(Gfm_name,id, style, style_index, color_index) */
/* char *Gfm_name; */
/* int  id; */
/* char *style; */
/* int  style_index; */
/* int  color_index; */
/* { */
/* 	int				j,ct=0; */
/* 	char 				buf[1024], *fill_name; */
/* 	Gflfac                  	hat_table; */
/* 	Gintlist                	pat_table; */
/*         struct table_fill               *pf,*p1; */
/* 	extern struct table_fill 	Tf_tab; */
/*         extern int                      chk_mov_Tf(); */

/*         /\* */
/*          * Set new attributes for Tf. */
/*          * Create a new fillarea structure and copy to it. */
/*          *\/ */
/*         if((pf=(struct table_fill *)malloc(sizeof(struct table_fill)))==NULL) { */
/*             PyErr_SetString(PyExc_MemoryError, "No memory for new fillarea attribute."); */
/*             return NULL; */
/*         } */

/* 	/\* Set the style index of the hatch or pattern *\/ */
/* 	/\* nullify the set of attributes               *\/ */
/*         pf->priority = 1; */
/*         for (j=0; j < 4; j++) { */
/*             if (j == 0 || j == 2) { */
/*                pf->fvp[j]=0.0; */
/*                pf->fwc[j]=0.0; */
/*             } else { */
/*                pf->fvp[j]=1.0; */
/*                pf->fwc[j]=1.0; */
/*             } */
/*         } */
/*         pf->fx = NULL; */
/*         pf->fy = NULL; */
/*         pf->x=0.0; */
/*         pf->y=0.0; */
/*         pf->w=0.1; */
/*         pf->h=0.1; */
/*         pf->fais = NULL; pf->fais_size = 0; */
/*         pf->fasi = NULL; pf->fasi_size = 0; */
/*         pf->faci = NULL; pf->faci_size = 0; */

/*         /\* Set the fillarea's name *\/ */
/* 	/\*sprintf(buf,"%s_%d", Gfm_name, id);*\/ */
/* 	for (p1=&Tf_tab;p1 != NULL;p1=p1->next) */
/*            ct++; */
/* 	sprintf(buf,"GEN_%d", ct); */
/* 	fill_name = (char *) malloc(strlen(buf)*sizeof(char)+1); */
/*         strcpy(pf->name,buf); */
/*         strcpy(fill_name,buf); */

/*         /\* Set the interior style to solid *\/ */
/* 	if (cmpncs(style, "solid") == 0) { */
/*            if (pf->fais!=NULL) { free((char *) pf->fais); pf->fais=NULL; } */
/*            if (pf->fasi!=NULL) { free((char *) pf->fasi); pf->fasi=NULL; } */
/*            if((pf->fais=(int *) malloc(sizeof(int)))==NULL) { */
/*                   PyErr_SetString(VCS_Error,"Error - memory for fill values not found."); */
/*                   return NULL; */
/*            } */
/*            if ((pf->fasi = (int *) malloc( sizeof(int)))== NULL) { */
/*                   PyErr_SetString(VCS_Error,"Error - memory for fill values not found."); */
/*                   return NULL; */
/*            } */
/*            pf->fais[0] = 1; pf->fais_size = 1; */
/*            pf->fasi[0] = 1; pf->fasi_size = 1; */
/* 	} else if (cmpncs(style, "pattern") == 0) { */
/*            if (pf->fais!=NULL) { free((char *) pf->fais); pf->fais=NULL; } */
/*            if (pf->fasi!=NULL) { free((char *) pf->fasi); pf->fasi=NULL; } */
/*            if ((pf->fais = (int *) malloc( sizeof(int)))== NULL) { */
/*                   PyErr_SetString(VCS_Error,"Error - memory for fill values not found."); */
/*                   return NULL; */
/*            } */
/*            if ((pf->fasi = (int *) malloc( sizeof(int)))== NULL) { */
/*                   PyErr_SetString(VCS_Error,"Error - memory for fill values not found."); */
/*                   return NULL; */
/*            } */
/*            pf->fais[0] = 2; pf->fais_size = 1; */
/*            /\* Below was to difficult. I changed the code in xgks *\/ */
/* /\*           gqepai(104, &pat_table); * Inquire the list of pattern indices * */
/*            pf->fasi[0] = pat_table.integers[style_index-1]; *Set pattern style*\/ */
/*            pf->fasi[0] = style_index; */
/*            pf->fasi_size = 1; */
/*            /\*pf->fasi=style_index;*\/ */
/* 	} else if (cmpncs(style, "hatch") == 0) { */
/*            if (pf->fais!=NULL) { free((char *) pf->fais); pf->fais=NULL; } */
/*            if (pf->fasi!=NULL) { free((char *) pf->fasi); pf->fasi=NULL; } */
/*            if ((pf->fais = (int *) malloc( sizeof(int)))== NULL) { */
/*                   PyErr_SetString(VCS_Error,"Error - memory for fill values not found."); */
/*                   return NULL; */
/*            } */
/*            if ((pf->fasi = (int *) malloc( sizeof(int)))== NULL) { */
/*                   PyErr_SetString(VCS_Error,"Error - memory for fill values not found."); */
/*                   return NULL; */
/*            } */
/*            pf->fais[0] = 3;pf->fais_size = 1; */
/*            /\* Below was to difficult. I changed the code in xgks *\/ */
/* /\*           gqfaf("CANVAS", &hat_table); * Inquire the fill area facilities * */
/*            pf->fasi[0]=hat_table.hatches.integers[style_index-1];*Set hatch style*\/ */
/*            pf->fasi[0]= style_index; */
/*            pf->fasi_size = 1; */
/* 	} */

/*         /\* Set the color index used for the fillarea *\/ */
/*         if (pf->faci!=NULL) { free((char *) pf->faci); pf->faci=NULL; } */
/*         if ((pf->faci = (int *) malloc( sizeof(int)) )== NULL) { */
/*                   PyErr_SetString(VCS_Error,"Error - memory for fill values not found."); */
/*                   return NULL; */
/*         } */
/*         pf->faci[0] = color_index; pf->faci_size = 1; */

/*         /\* Set the new structure in the list *\/ */
/*         chk_mov_Tf (pf); */

/* 	return fill_name; */
/* } */

/*
 * Find the existing isofill graphics method and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setGfmmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			i,j=0,n,ct=0,sct=0,color_index,style_index;
	int 			MODE, value_int;	
	long 			value_long;
	float 			value_float;
	double 			value_double;
	char			*Tf_name;
	char 			buf[1024], *style;
        char 			*Gfm_name, *str=NULL, *member=NULL;
	char			*value_str=NULL;
        PyObject 		*GFM=NULL, *MEMBER=NULL, *VALUE=NULL;
        PyObject		*listit,*tup, *sindices, *wrap;
	struct gfm_tab          *get_gfmtab=NULL;
	extern int              update_ind;
	struct fill_range       *pfiso, *next_pfiso, *pfiso_new, *tpfiso;
        struct gfm_tab          *gfmtab;
        extern struct gfm_tab   Gfm_tab;
	extern struct gfm_tab   *getGfm();
	extern int              chk_mov_Gfm();
	extern int 		vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &GFM, &MEMBER, &VALUE, &MODE)) {
           if (GFM == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(GFM,"name", "s", &Gfm_name);
        gfmtab=&Gfm_tab;
        while ((gfmtab != NULL) &&
               (strcmp(gfmtab->name, Gfm_name) != 0))
           gfmtab = gfmtab->next;

	if (MEMBER != NULL) {
           member = PyString_AsString(MEMBER);
/*	   sprintf(buf, "print 'member = %s'", member);
           PyRun_SimpleString(buf);*/
	}

	if (VALUE != NULL) {
	   if (PyString_Check(VALUE)) { /*check string*/
              value_str = PyString_AsString(VALUE);
	   } else if (PyInt_Check(VALUE)) { /*check for int*/
              value_int = (int) PyInt_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for float*/
              value_float = (float) PyFloat_AsDouble(VALUE);
	   } else if (PyLong_Check(VALUE)) { /* check for long*/
              value_long = PyLong_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for double*/
              value_double = PyFloat_AsDouble(VALUE);
	   } else if (PyDict_Check(VALUE)) { /*check for dictionary*/
	      value_str = return_vcs_legacy_list(VALUE, member);
	   }
	}

	/*
	 * Set the appropriate meshfill attribute. But first 
	 * get the meshfill structure.
         */
	get_gfmtab = getGfm(gfmtab->name);
	if (cmpncs(member, "projection") == 0) {
	   strcpy(get_gfmtab->pGfm_attr->proj, value_str);
	} else if (cmpncs(member, "xticlabels1") == 0) {
	   strcpy(get_gfmtab->pGfm_attr->xtl1, value_str);
	} else if (cmpncs(member, "xticlabels2") == 0) {
	   strcpy(get_gfmtab->pGfm_attr->xtl2, value_str);
	} else if (cmpncs(member, "xmtics1") == 0) {
	   strcpy(get_gfmtab->pGfm_attr->xmt1, value_str);
	} else if (cmpncs(member, "xmtics2") == 0) {
	   strcpy(get_gfmtab->pGfm_attr->xmt2, value_str);
	} else if (cmpncs(member, "yticlabels1") == 0) {
	   strcpy(get_gfmtab->pGfm_attr->ytl1, value_str);
	} else if (cmpncs(member, "yticlabels2") == 0) {
	   strcpy(get_gfmtab->pGfm_attr->ytl2, value_str);
	} else if (cmpncs(member, "ymtics1") == 0) {
	   strcpy(get_gfmtab->pGfm_attr->ymt1, value_str);
	} else if (cmpncs(member, "ymtics2") == 0) {
	   strcpy(get_gfmtab->pGfm_attr->ymt2, value_str);
	} else if (cmpncs(member, "datawc_x1") == 0) {
	   get_gfmtab->pGfm_attr->dsp[0] = value_float;
	} else if (cmpncs(member, "datawc_y1") == 0) {
	   get_gfmtab->pGfm_attr->dsp[1] = value_float;
	} else if (cmpncs(member, "datawc_x2") == 0) {
	   get_gfmtab->pGfm_attr->dsp[2] = value_float;
	} else if (cmpncs(member, "datawc_y2") == 0) {
	   get_gfmtab->pGfm_attr->dsp[3] = value_float;
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   get_gfmtab->pGfm_attr->idsp[0] = value_int;
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   get_gfmtab->pGfm_attr->idsp[1] = value_int;
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   get_gfmtab->pGfm_attr->idsp[2] = value_int;
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   get_gfmtab->pGfm_attr->idsp[3] = value_int;
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   get_gfmtab->pGfm_attr->calendar = value_int;
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   strcpy(get_gfmtab->pGfm_attr->timeunits, value_str);
	} else if (cmpncs(member, "xaxisconvert") == 0) {
	   strcpy(get_gfmtab->pGfm_attr->xat, value_str);
	} else if (cmpncs(member, "yaxisconvert") == 0) {
	   strcpy(get_gfmtab->pGfm_attr->yat, value_str);
	} else if (cmpncs(member, "missing") == 0) {
	   get_gfmtab->pGfm_attr->missing = value_int;
	} else if (cmpncs(member, "mesh") == 0) {
	   get_gfmtab->pGfm_attr->mesh = value_int;
	} else if (cmpncs(member, "legend") == 0) {
           if (value_str != NULL) {
              if (get_gfmtab->pGfm_attr->legend != NULL) {
                 free((char *) get_gfmtab->pGfm_attr->legend);
                 get_gfmtab->pGfm_attr->legend = NULL;
              }
	      get_gfmtab->pGfm_attr->legend = (char *)malloc(strlen(value_str)*sizeof(char)+1);
	      strcpy(get_gfmtab->pGfm_attr->legend, value_str);
	   } else {
              if (get_gfmtab->pGfm_attr->legend != NULL) {
                 free((char *) get_gfmtab->pGfm_attr->legend);
                 get_gfmtab->pGfm_attr->legend = NULL;
              }
	   }
	} else if (cmpncs(member, "wrap") == 0 ){
	  if ((VALUE==NULL) || (VALUE == Py_None )) { 
	    get_gfmtab->pGfm_attr->xwrap=0. ; 
	    get_gfmtab->pGfm_attr->ywrap=0.;
	  }
	  else {
	    wrap=PyList_GetItem(VALUE,0);
	    if (wrap==Py_None) {
	      get_gfmtab->pGfm_attr->ywrap=0.;
	    }
	    else {
	      get_gfmtab->pGfm_attr->ywrap=(float)PyFloat_AsDouble(wrap);
	    }
	    wrap=PyList_GetItem(VALUE,1);
	    if (wrap==Py_None) {
	      get_gfmtab->pGfm_attr->xwrap=0.;
		}
	    else {
	      get_gfmtab->pGfm_attr->xwrap=(float)PyFloat_AsDouble(wrap);
	    }
	  }
	} else if (cmpncs(member, "levels") == 0) {
	   /* get the style values */
           Get_Member(GFM,"fillareastyle", "s", &style);

	   /* get the style index values */
           Get_Member(GFM,"fillareaindices", "O", &sindices);

	   /* get the color values */
           Get_Member(GFM,"fillareacolors", "O", &listit);
           /* Free the current fill_range link list */
	   pfiso = next_pfiso = get_gfmtab->pGfm_attr->line;
           while (pfiso != NULL) {
              next_pfiso = pfiso->next;
              free ((char *) pfiso);
              pfiso = next_pfiso;
           }
           get_gfmtab->pGfm_attr->line = tpfiso = NULL;
	   
	   if (PyTuple_Check(VALUE)) { /*check for tuple*/
              /* Create the new fill_range link list */
              for (i=0; i<PyTuple_Size(VALUE); i++) {
                 tup = PyTuple_GetItem(VALUE, i);
                 if (PyList_Check(tup)) { /* check for list */
                    for (j=0; j<(PyList_Size(tup)-1); j++) {
                       /* malloc the new iso struct */
                       if ((pfiso_new = (struct fill_range *)malloc(
                                         sizeof(struct fill_range)))==NULL) {
		          sprintf(buf,"No memory for new isofill id(%d).\n",i);
                          PyErr_SetString(PyExc_MemoryError, buf);
                          return NULL;
                       }

                       strcpy(pfiso_new->fill_name, "default");
                       if ((sindices==Py_None) || (sct>=PyList_Size(sindices))) {
                          if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                             color_index = 16+ct;
                          else
                             color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));
                          strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfm_name, ct, style, 0, color_index));
                       } else if ((PyInt_Check(PyList_GetItem(sindices,sct))) ||
                                   (PyFloat_Check(PyList_GetItem(sindices,sct)))) {
                             style_index = (int) PyInt_AsLong(PyList_GetItem(sindices,sct));
                          if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                             color_index = 16+ct;
                          else
                             color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));
                          strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfm_name, ct, style, style_index, color_index));
                       } else { /* must be a fillarea object */
                          Tf_name = PyString_AsString(PyList_GetItem(sindices,sct));
                          strcpy(pfiso_new->fill_name, Tf_name);
                       }
                       pfiso_new->id = sct+1;
                       pfiso_new->lev1 = (float) PyFloat_AsDouble(PyList_GetItem(tup,j));
                       pfiso_new->lev2 = (float) PyFloat_AsDouble(PyList_GetItem(tup,j+1));
                       ct++;sct++;
                       pfiso_new->next = NULL;
          
                       /* Add to the new fill range link list */
                       if (tpfiso == NULL)
                          get_gfmtab->pGfm_attr->line = tpfiso = pfiso_new;
                       else {
                          tpfiso->next = pfiso_new;
                          tpfiso = pfiso_new;
                       }
                    }
                 } else {
		    if ((PyInt_Check(tup)) || (PyFloat_Check(tup))) {
                    /* malloc the new iso struct */
                    if ((pfiso_new = (struct fill_range *)malloc(
                                      sizeof(struct fill_range)))==NULL) {
		       sprintf(buf,"No memory for new isofill id(%d).\n",i);
                       PyErr_SetString(PyExc_MemoryError, buf);
                       return NULL;
                    }

                    strcpy(pfiso_new->fill_name, "default");
                    if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                       color_index = 16+ct;
                    else
                       color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));

		    if ((sindices==Py_None) || (sct>=PyList_Size(sindices))) {
                       style_index = sct;
                       strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfm_name, i, style, style_index, color_index));
                    } else if (PyInt_Check(PyList_GetItem(sindices,sct))) {
                       style_index = (int) PyInt_AsLong(PyList_GetItem(sindices,sct));
                       strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfm_name, i, style, style_index, color_index));
                    } else { /* must be a fillarea object */
                       Tf_name = PyString_AsString(PyList_GetItem(sindices,sct));
                       strcpy(pfiso_new->fill_name, Tf_name);
                    }

                    pfiso_new->id = sct+1;
                    pfiso_new->lev1 = (float) PyFloat_AsDouble(tup);
                    tup = PyTuple_GetItem(VALUE, i+1);
                    pfiso_new->lev2 = (float) PyFloat_AsDouble(tup);
                    ct++;sct++;
                    pfiso_new->next = NULL;

                    /* Add to the new fill range link list */
                    if (tpfiso == NULL)
                       get_gfmtab->pGfm_attr->line = tpfiso = pfiso_new;
                    else {
                       tpfiso->next = pfiso_new;
                       tpfiso = pfiso_new;
                    }
                    if (i == (PyTuple_Size(VALUE)-2)) break;

                 } else {
		    PyErr_SetString(PyExc_ValueError, "Must be either integer or float values.");
                    return NULL;
                 }
              }
            }
           } else if (PyList_Check(VALUE)) { /* check for list */
              /* Create the new fill_range link list */
              for (j=0; j<(PyList_Size(VALUE)-1); j++) {
                 /* malloc the new iso struct */
                 if ((pfiso_new = (struct fill_range *)malloc(
                                   sizeof(struct fill_range)))==NULL) {
                    sprintf(buf,"No memory for new isofill id(%d).\n",j);
                    PyErr_SetString(PyExc_MemoryError, buf);
                    return NULL;
                 }
                 if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                    color_index = 16+ct;
                 else
                    color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));

                 if ((sindices==Py_None) || (sct>=PyList_Size(sindices))) {
                    style_index = sct;
                    strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfm_name, j, style, style_index, color_index));    
                 } else if (PyInt_Check(PyList_GetItem(sindices,sct))) {
                    style_index = (int) PyInt_AsLong(PyList_GetItem(sindices,sct));
                    strcpy(pfiso_new->fill_name, return_new_fillarea_attribute(Gfm_name, j, style, style_index, color_index));    
                 } else { /* must be a fillarea object */
                    Tf_name = PyString_AsString(PyList_GetItem(sindices,sct));
                    strcpy(pfiso_new->fill_name, Tf_name);
                 }

                 pfiso_new->id = j;
                 pfiso_new->lev1 = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,j));              
                 pfiso_new->lev2 = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,j+1));            
                 ct++; sct++;
                 pfiso_new->next = NULL;
                 
                 /* Add to the new fill range link list */
                 if (tpfiso == NULL)
                    get_gfmtab->pGfm_attr->line = tpfiso = pfiso_new;
                 else {
                    tpfiso->next = pfiso_new;
                    tpfiso = pfiso_new;
                 }
              }
	   }
	}


	chk_mov_Gfm(get_gfmtab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

	/* 
	 * Call the Set_Member function to assign the data
	 * member of the known object by name. This function
	 * can take all the common Python/C data conversion 
	 * types: { "s" = char * : "i" = int   : "l" = long :
	 *          "c" = char   : "f" = float : "d" = double:
	 *          "O" = PyObject * .
	 *
	 * But in this case below, I am sending only the PyObject
	 * down. No need to convert to C then back to PyObject.
	if (member!=NULL)
           Set_Member(GFM, member, "O", VALUE);
	 */

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new isofill graphics method by copying from an existing
 * isofill graphics method. If no source copy name argument is given,
 * then the default isofill graphics method will be used to replicate
 * the new isofill graphics method.
 */
static PyObject *
PyVCS_copyGfm(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GFM_SRC=NULL, *GFM_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Gfm_name();
              
        if(PyArg_ParseTuple(args,"|ss", &GFM_SRC, &GFM_NAME)) {
           if (GFM_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source isofill graphics method name.");
                 return NULL;
           }

           if (GFM_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", GFM_NAME);
        }

        ierr = copy_Gfm_name(GFM_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating isofill graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing isofill graphics method.
 */
static PyObject *
PyVCS_renameGfm(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GFM_OLD_NAME=NULL, *GFM_NEW_NAME=NULL;
        extern int      renameGfm_name();
 
        if(PyArg_ParseTuple(args,"|ss", &GFM_OLD_NAME, &GFM_NEW_NAME)) {
           if ((GFM_OLD_NAME == NULL) || (GFM_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new isofill graphics method name.");
                 return NULL;
           }
        }

        ierr = renameGfm_name(GFM_OLD_NAME, GFM_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming isofill graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;


}

/* 
 * Remove an existing meshfill graphics method.
 */
static PyObject *
PyVCS_removeGfm(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the meshfill file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeGfm_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed meshfill object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The meshfill object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/*
 * Script out an existing meshfill graphics method.
 */
static PyObject *
PyVCS_scriptGfm(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GFM_NAME=NULL,*MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_isofill();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &GFM_NAME, &SCRIPT_NAME, &MODE)) {
           if (GFM_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the meshfill name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
	if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
		/* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_meshfill(fp, GFM_NAME) == 0) {
              sprintf(buf, "Error - Cannot save isofill script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}



/* Check if projection exist */
static PyObject *
PyVCS_checkProj(self,args)
  PyVCScanvas_Object *self;
  PyObject *args;
{
  char *Proj_name;
  struct projection_attr         *pj;
  extern struct projection_attr  p_PRJ_list;
  if(PyArg_ParseTuple(args,"|s",&Proj_name)) {
    if (Proj_name == NULL) {
      PyErr_SetString(PyExc_TypeError, "Not correct object type.");
      return NULL;
    }
  }

  pj=&p_PRJ_list;
  while ((pj != NULL) &&
	 (strcmp(pj->name, Proj_name) != 0)){
    pj = pj->next;}
  if (pj == NULL) 
    {
      /* does not exist return 0 */
           return Py_BuildValue("i",0);
    }
  else
    {
      /* does exist */
           return Py_BuildValue("i",1);
    }
}
    
/* 
 * Return the VCS projection (Proj) member value. 
 */
static PyObject *
PyVCS_getProjmember(PyVCScanvas_Object *self, PyObject *args)
{
	char *Proj_name, *member=NULL, buf[1024];
	int i=0;
	PyObject *PROJ=NULL, *MEMBER=NULL, *tup, *lp;
	struct projection_attr         *projtab;
    	extern struct projection_attr  p_PRJ_list;

	if(PyArg_ParseTuple(args,"|OO",&PROJ, &MEMBER)) {
           if (PROJ == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


        Get_Member(PROJ,"name", "s", &Proj_name);
	projtab=&p_PRJ_list;
        while ((projtab != NULL) &&
               (strcmp(projtab->name, Proj_name) != 0))
           projtab = projtab->next;

     	if (projtab == NULL) {
	   sprintf(buf,"Cannot find projection %s.",Proj_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "type") == 0) {
           return Py_BuildValue("i", projtab->proj_type);
	} else if (cmpncs(member, "parameters") == 0) {
           tup = PyList_New(15);
	   for (i=0;i<15;i++)
	     {
	       lp = Py_BuildValue("f",projtab->parm[i]);
	       PyList_SetItem(tup, i, lp);
	     }
	   return tup;
/*            return Py_BuildValue("[f,f,f,f,f,f,f,f,f,f,f,f,f,f,f]", projtab->parm); */
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing projection and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setProjmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			i,j=0,n,ct=0,sct=0,color_index,style_index;
	int 			MODE, value_int;	
	long 			value_long;
	float 			value_float;
	double 			value_double;
	char			*Tf_name;
	char 			buf[1024], *style;
        char 			*Proj_name, *str=NULL, *member=NULL;
	char			*value_str=NULL;
        PyObject 		*PROJ=NULL, *MEMBER=NULL, *VALUE=NULL;
        PyObject		*listit,*tup, *sindices, *wrap;
	extern int              update_ind;
        struct projection_attr        *projtab;
        extern struct projection_attr  p_PRJ_list;
	extern int              chk_mov_Proj();
	extern int 		vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &PROJ, &MEMBER, &VALUE, &MODE)) {
           if (PROJ == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(PROJ,"name", "s", &Proj_name);
        projtab=&p_PRJ_list;
        while ((projtab != NULL) &&
               (strcmp(projtab->name, Proj_name) != 0))
           projtab = projtab->next;

/* 	projtab=(struct projection_attr *)getProj(Proj_name); */
	if (MEMBER != NULL) {
           member = PyString_AsString(MEMBER);
	}

	/* Set the projection attributes */
	if (cmpncs(member, "type") == 0) {
	  if (PyInt_Check(VALUE)) 
	    { 
	      projtab->proj_type=(int)PyInt_AsLong(VALUE);
	    }
	  else if (PyLong_Check(VALUE)) 
	    {
	      projtab->proj_type=(int)PyLong_AsLong(VALUE);
	    }
	  }
	else if (cmpncs(member, "parameters") == 0) {
	  if (PyList_Check(VALUE))
	    {
	      for (i=0;i<15;i++){
		projtab->parm[i]=PyFloat_AsDouble(PyNumber_Float(PyList_GetItem(VALUE,i)));
	      }
	    }
	  else if (PyTuple_Check(VALUE))
	    {
	      for (i=0;i<15;i++){
		projtab->parm[i]=PyFloat_AsDouble(PyNumber_Float(PyTuple_GetItem(VALUE,i)));
	      }
	    }
	}


/* 	chk_mov_Proj(projtab); */
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new projection by copying from an existing one
 *  If no source copy name argument is given, then the default projection will be 
 * used to replicate theprojection
 */
static PyObject *
PyVCS_copyProj(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *PROJ_SRC=NULL, *PROJ_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Proj_name();
              
        if(PyArg_ParseTuple(args,"|ss", &PROJ_SRC, &PROJ_NAME)) {
           if (PROJ_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source projection.");
                 return NULL;
           }

           if (PROJ_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", PROJ_NAME);
        }

        ierr = copy_Proj_name(PROJ_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating projection.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing isofill graphics method.
 */
static PyObject *
PyVCS_renameProj(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *PROJ_OLD_NAME=NULL, *PROJ_NEW_NAME=NULL;
        extern int      renameProj_name();
 
        if(PyArg_ParseTuple(args,"|ss", &PROJ_OLD_NAME, &PROJ_NEW_NAME)) {
           if ((PROJ_OLD_NAME == NULL) || (PROJ_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new projection.");
                 return NULL;
           }
        }

        ierr = renameProj_name(PROJ_OLD_NAME, PROJ_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming projection.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing isofill graphics method.
 */
static PyObject *
PyVCS_removeProj(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the projection name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeProj_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed projection object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The projection object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/*
 * Script out an existing meshfill graphics method.
 */
static PyObject *
PyVCS_scriptProj(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GFM_NAME=NULL,*MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &GFM_NAME, &SCRIPT_NAME, &MODE)) {
           if (GFM_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the projection name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
	if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
		/* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_projection(fp, GFM_NAME) == 0) {
              sprintf(buf, "Error - Cannot save projection script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}





/* Create and return the name of the new line attribute */
char * return_new_line_attribute(Gi_name,id, line_index, color_index, line_width)
char 	*Gi_name;
int  	id;
int  	line_index;
int  	color_index;
float 	line_width;
{
        int                             j,ct=0;
        char                            buf[1024], *line_name;
        struct table_line               *pl,*p1;
	extern struct table_line 	Tl_tab;
        extern int                      chk_mov_Tl();

        /*
         * Set new attributes for Tl.
         * Create a new line structure and copy to it.
         */
        if((pl=(struct table_line *)malloc(sizeof(struct table_line)))==NULL) {
            PyErr_SetString(PyExc_MemoryError, "No memory for new line attribute.");
            return NULL;
        }

        /* nullify the set of attributes */
        pl->priority = 1;
        for (j=0; j < 4; j++) {
            if (j == 0 || j == 2) {
               pl->lvp[j]=0.0;
               pl->lwc[j]=0.0;
            } else {
               pl->lvp[j]=1.0;
               pl->lwc[j]=1.0;
            }
        }
        pl->lx = NULL;
        pl->ly = NULL;
        pl->ltyp = NULL; pl->ltyp_size = 0;
        pl->lwsf = NULL; pl->lwsf_size = 0;
        pl->lci  = NULL; pl->lci_size = 0;

        /* Create and set the new line name */
	/*sprintf(buf,"%s_%d", Gi_name, id);*/
        for (p1=&Tl_tab;p1 != NULL;p1=p1->next)
            ct++;
	sprintf(buf,"GEN_%d", ct);
        line_name = (char *) malloc(strlen(buf)*sizeof(char)+1);
        strcpy(pl->name,buf);
        strcpy(line_name,buf);

	/* Set the line type */ 
        if((pl->ltyp=(int *) malloc(sizeof(int)))==NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for line values not found.");
                  return NULL;
           }
	pl->ltyp[0] = line_index; pl->ltyp_size = 1;

        /* Set the linewidth scale factor */
        if((pl->lwsf=(float *) malloc(sizeof(float)))==NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for line values not found.");
                  return NULL;
           }
        pl->lwsf[0] = line_width; pl->lwsf_size = 1;

	/* Get the color index used for the line */ 
        if((pl->lci=(int *) malloc(sizeof(int)))==NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for line values not found.");
                  return NULL;
           }
        pl->lci[0] = color_index; pl->lci_size = 1;
	pl->next = NULL;

	/* Set the new structure in the list */
        chk_mov_Tl (pl); 

	return line_name;
}

/* Create and return the name of the new marker attribute */
char * return_new_marker_attribute(Gi_name,id, marker_index, color_index, size_index)
char *Gi_name;
int  id;
int  marker_index;
int  color_index;
int  size_index;
{
        int                             j,ct=0;
        char                            buf[1024], *marker_name;
        struct table_mark               *pm,*p1;
        extern struct table_mark 	Tm_tab;
        extern int                      chk_mov_Tm();

        /*
         * Set new attributes for Tm.
         * Create a new marker structure and copy to it.
         */
        if((pm=(struct table_mark *)malloc(sizeof(struct table_mark)))==NULL) {
            PyErr_SetString(PyExc_MemoryError, "No memory for new marker attribute.");
            return NULL;
        }

        /* nullify the set of attributes               */
        pm->priority = 1;
        for (j=0; j < 4; j++) {
            if (j == 0 || j == 2) {
               pm->mvp[j]=0.0;
               pm->mwc[j]=0.0;
            } else {
               pm->mvp[j]=1.0;
               pm->mwc[j]=1.0;
            }
        }
        pm->mx = NULL;
        pm->my = NULL;
        pm->mtyp  = NULL; pm->mtyp_size = 0;
        pm->msize = NULL; pm->msize_size = 0;
        pm->mci   = NULL; pm->mci_size = 0;

        /* Create and set the new marker name */
	/*sprintf(buf,"%s_%d", Gi_name, id);*/
        for (p1=&Tm_tab;p1 != NULL;p1=p1->next)
            ct++;
	sprintf(buf,"GEN_%d", ct);
        marker_name = (char *) malloc(strlen(buf)*sizeof(char)+1);
        strcpy(pm->name,buf);
        strcpy(marker_name,buf);

	/* Set the marker type */ 
        if((pm->mtyp=(int *) malloc(sizeof(int)))==NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for marker values not found.");
                  return NULL;
           }
	pm->mtyp[0] = marker_index;pm->mtyp_size = 1;

        /* Set the markerwidth scale factor */
        if((pm->msize=(float *) malloc(sizeof(float)))==NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for marker values not found.");
                  return NULL;
           }
        pm->msize[0] = size_index; pm->msize_size = 1;

	/* Get the color index used for the marker */ 
        if((pm->mci=(int *) malloc(sizeof(int)))==NULL) {
                  PyErr_SetString(VCS_Error,"Error - memory for marker values not found.");
                  return NULL;
           }
        pm->mci[0] = color_index; pm->mci_size = 1;
	pm->next = NULL;

	/* Set the new structure in the list */
        chk_mov_Tm (pm); 

	return marker_name;
}

/* Create and return the name of the new text attribute */
char * return_new_text_attribute(Gi_name,id, font_index, color_index)
char *Gi_name;
int  id;
int  font_index;
int  color_index;
{
        int				ct=0;
        char                            buf[1024], *text_name;
	struct table_text       	*pt,*p1;
        extern struct table_text        Tt_tab;
        extern int                      chk_mov_Tt();
	extern int              update_ind;

        /*
         * Set new attributes for Tt.
         * Create a new line structure and copy to it.
         */
        if((pt=(struct table_text *)malloc(sizeof(struct table_text)))==NULL) {
            PyErr_SetString(PyExc_MemoryError, "No memory for new text attribute.");
            return NULL;
        }

        /* Create and set the new text name */
        /*sprintf(buf,"%s_%d", Gi_name, id);*/
        for (p1=&Tt_tab;p1 != NULL;p1=p1->next)
            ct++;
        sprintf(buf,"GEN_%d", ct);
        text_name = (char *) malloc(strlen(buf)*sizeof(char)+1);
        strcpy(pt->name,buf);
        strcpy(text_name,buf);


	/* Set the text font type */
	pt->txfont = font_index;

	/* Set the text precision */
	pt->txpr = 2;

	/* Set the text expansion */
	pt->txexp = 1.0;

      	/* Set the text spacing */
        pt->txsp = 0.2;

	/* Set the text colour index */
        pt->txci = color_index;

	/* Set the text fillin colour index */
        pt->txfci = 240;

	pt->next=NULL;

        /* Set the new structure in the list */
        chk_mov_Tt (pt);
	vcs_legacy_canvas_update(0);

        return text_name;
}

/*
 * Find the existing isoline graphics method and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setGimember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			i,j=0,n,ct=0,lct=1,color_index=0,line_index,lst_size=0;
	int 			MODE, value_int, font_index=1, text_color_index=0;	
	long 			value_long;
	float 			value_float,width_val=1.0;
	double 			value_double;
	char 			buf[1024], *Tf_name=NULL, *tptr;
        char 			*Gi_name, *Tl_name=NULL, *str=NULL, *member=NULL;
	char			*value_str=NULL;
        PyObject 		*GI=NULL, *MEMBER=NULL, *VALUE=NULL;
	PyObject 		*itempk,*itempv,*pkeys,*pvalues;
        PyObject		*listit,*tup, *lindices, *listtt, *listitc, *listlw;
	PyObject                *listitclock, *listitscale, *listitangle, *listitspacing;
	int                     line_clock;
	float line_scale, line_angle, line_spacing;
	struct gi_tab           *get_gitab=NULL;
	extern int              update_ind;
	struct iso       	*piso, *next_piso, *piso_new, *tpiso;
        struct gi_tab           *gitab;
        extern struct gi_tab    Gi_tab;
	extern struct gi_tab    *getGi();
	extern int              chk_mov_Gi();
	extern int 		vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &GI, &MEMBER, &VALUE, &MODE)) {
           if (GI == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(GI,"name", "s", &Gi_name);
        gitab=&Gi_tab;
        while ((gitab != NULL) &&
               (strcmp(gitab->name, Gi_name) != 0))
           gitab = gitab->next;

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	if (VALUE != NULL) {
	   if (PyString_Check(VALUE)) { /*check string*/
              value_str = PyString_AsString(VALUE);
	   } else if (PyInt_Check(VALUE)) { /*check for int*/
              value_int = (int) PyInt_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for float*/
              value_float = (float) PyFloat_AsDouble(VALUE);
	   } else if (PyLong_Check(VALUE)) { /* check for long*/
              value_long = PyLong_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for double*/
              value_double = PyFloat_AsDouble(VALUE);
	   } else if (PyDict_Check(VALUE)) { /*check for dictionary*/
              value_str = return_vcs_legacy_list(VALUE, member);
	   }
	}

	/*
	 * Set the appropriate isoline attribute. But first 
	 * get the isoline structure.
         */
	get_gitab = getGi(gitab->name);
	if (cmpncs(member, "projection") == 0) {
	   strcpy(get_gitab->pGi_attr->proj, value_str);
	} else if (cmpncs(member, "xticlabels1") == 0) {
	   strcpy(get_gitab->pGi_attr->xtl1, value_str);
	} else if (cmpncs(member, "xticlabels2") == 0) {
	   strcpy(get_gitab->pGi_attr->xtl2, value_str);
	} else if (cmpncs(member, "xmtics1") == 0) {
	   strcpy(get_gitab->pGi_attr->xmt1, value_str);
	} else if (cmpncs(member, "xmtics2") == 0) {
	   strcpy(get_gitab->pGi_attr->xmt2, value_str);
	} else if (cmpncs(member, "yticlabels1") == 0) {
	   strcpy(get_gitab->pGi_attr->ytl1, value_str);
	} else if (cmpncs(member, "yticlabels2") == 0) {
	   strcpy(get_gitab->pGi_attr->ytl2, value_str);
	} else if (cmpncs(member, "ymtics1") == 0) {
	   strcpy(get_gitab->pGi_attr->ymt1, value_str);
	} else if (cmpncs(member, "ymtics2") == 0) {
	   strcpy(get_gitab->pGi_attr->ymt2, value_str);
	} else if (cmpncs(member, "datawc_x1") == 0) {
	   get_gitab->pGi_attr->dsp[0] = value_float;
	} else if (cmpncs(member, "datawc_y1") == 0) {
	   get_gitab->pGi_attr->dsp[1] = value_float;
	} else if (cmpncs(member, "datawc_x2") == 0) {
	   get_gitab->pGi_attr->dsp[2] = value_float;
	} else if (cmpncs(member, "datawc_y2") == 0) {
	   get_gitab->pGi_attr->dsp[3] = value_float;
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   get_gitab->pGi_attr->idsp[0] = value_int;
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   get_gitab->pGi_attr->idsp[1] = value_int;
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   get_gitab->pGi_attr->idsp[2] = value_int;
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   get_gitab->pGi_attr->idsp[3] = value_int;
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   get_gitab->pGi_attr->calendar = value_int;
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   strcpy(get_gitab->pGi_attr->timeunits, value_str);
	} else if (cmpncs(member, "xaxisconvert") == 0) {
	   strcpy(get_gitab->pGi_attr->xat, value_str);
	} else if (cmpncs(member, "yaxisconvert") == 0) {
	   strcpy(get_gitab->pGi_attr->yat, value_str);
	} else if (cmpncs(member, "label") == 0) {
           if (cmpncs(value_str, "y") == 0)
	      get_gitab->pGi_attr->labels = 121;
           else
	      get_gitab->pGi_attr->labels = 110;
	} else if (cmpncs(member, "level") == 0) {
           /* get the line type values */
           Get_Member(GI,"line", "O", &lindices);

           /* get the line color values */
           Get_Member(GI,"linecolors", "O", &listit);

           /* get the line width values */
           Get_Member(GI,"linewidths", "O", &listlw);

           /* get the text values */
           Get_Member(GI,"text", "O", &listtt);

           /* get the text color values */
           Get_Member(GI,"textcolors", "O", &listitc);

           /* get the clockwise values */
           Get_Member(GI,"clockwise", "O", &listitclock);

           /* get the length scale values */
           Get_Member(GI,"scale", "O", &listitscale);

           /* get the angle values */
           Get_Member(GI,"angle", "O", &listitangle);

           /* get the spacing values */
           Get_Member(GI,"spacing", "O", &listitspacing);

           /* Free the current isoline range link list */
           piso = next_piso = get_gitab->pGi_attr->line;
           while (piso != NULL) {
              next_piso = piso->next;
              free ((char *) piso);
              piso = next_piso;
           }
           get_gitab->pGi_attr->line = tpiso = NULL;

           if (PyList_Check(VALUE)) { /* check for list */
              /* Create the new isoline range link list */
	      lst_size = PyList_Size(VALUE);
              for (j=0; j<lst_size; j++) {
                 /* malloc the new iso struct */
                 if ((piso_new = (struct iso *)malloc(
                                   sizeof(struct iso)))==NULL) {
                    sprintf(buf,"No memory for new isoline id(%d).\n",j);
                    PyErr_SetString(PyExc_MemoryError, buf);
                    return NULL;
                 }

                 if ((lindices==Py_None) || (lct>PyList_Size(lindices))) {
                    line_index = 1;
		 } else {
	             if (cmpncs("solid", PyString_AsString(PyList_GetItem(lindices,ct)))==0)
                        line_index = 1;
	             else if (cmpncs("dash", PyString_AsString(PyList_GetItem(lindices,ct)))==0)
                        line_index = 2;
	             else if (cmpncs("dot", PyString_AsString(PyList_GetItem(lindices,ct)))==0)
                        line_index = 3;
	             else if (cmpncs("dash-dot", PyString_AsString(PyList_GetItem(lindices,ct)))==0)
                        line_index = 4;
	             else if (cmpncs("long-dash", PyString_AsString(PyList_GetItem(lindices,ct)))==0)
                        line_index = -3;
                     else {
                        Tl_name = PyString_AsString(PyList_GetItem(lindices,ct));
			if (Tl_name == NULL)
                           line_index = (int) PyInt_AsLong(PyList_GetItem(lindices,ct));
			else
                           line_index = 999;
                     }
		 }
                 if ((listit == Py_None) || (ct >= PyList_Size(listit)))
                    color_index = 241; /*color_index = 16+ct;*/
                 else
                    color_index = (int) PyInt_AsLong(PyList_GetItem(listit,ct));

                 if ((listlw == Py_None) || (ct >= PyList_Size(listlw)))
                 {   width_val = 1.0;
		 }
                 else {
		   width_val = (float) PyFloat_AsDouble(PyList_GetItem(listlw,ct));
		 }

                 if ((listtt == Py_None) || (ct >= PyList_Size(listtt)))
                    font_index = 1;
                 else if (PyString_Check(PyList_GetItem(listtt,ct))) {
                    Tf_name = PyString_AsString(PyList_GetItem(listtt,ct));
                    font_index = 999;
                 }else
                    font_index = (float) PyInt_AsLong(PyList_GetItem(listtt,ct));

                 if ((listitc == Py_None) || (ct >= PyList_Size(listitc)))
                    text_color_index = 241;
                 else
                    text_color_index = (int) PyInt_AsLong(PyList_GetItem(listitc,ct));

                 if ((listitclock == Py_None) || (ct >= PyList_Size(listitclock)))
                    line_clock = 0; /*color_index = 16+ct;*/
                 else
                    line_clock = (int) PyInt_AsLong(PyList_GetItem(listitclock,ct));

                 if ((listitscale == Py_None) || (ct >= PyList_Size(listitscale)))
                    line_scale = 1.; /*color_index = 16+ct;*/
                 else
                    line_scale = (float) PyFloat_AsDouble(PyList_GetItem(listitscale,ct));

                 if ((listitangle == Py_None) || (ct >= PyList_Size(listitangle)))
                    line_angle = 35.; /*color_index = 16+ct;*/
                 else {
                    line_angle = (float) PyFloat_AsDouble(PyList_GetItem(listitangle,ct));
		 }

                 if ((listitspacing == Py_None) || (ct >= PyList_Size(listitspacing)))
                    line_spacing = 1.; /*color_index = 16+ct;*/
                 else
                    line_spacing = (float) PyFloat_AsDouble(PyList_GetItem(listitspacing,ct));

	         tup=PyList_GetItem(VALUE,j);
                 piso_new->id = j;
                 piso_new->p = 1;
                 piso_new->lev = (float) PyFloat_AsDouble(PyList_GetItem(tup,0));
                 piso_new->incr = (float) PyFloat_AsDouble(PyList_GetItem(tup,1));
                 piso_new->hici = 0;
		 piso_new->cw=line_clock;
		 piso_new->ls=line_scale;
		 piso_new->angle=line_angle;
		 piso_new->spc=line_spacing;
                 strcpy(piso_new->lab, "*");
                 if ((lindices==Py_None) && (listit == Py_None) && (listlw == Py_None)) {
		   strcpy(piso_new->lb,"default");
		 }
                 else {
                    if (line_index != 999) {
                       if ((listit == Py_None) && (listlw == Py_None))
                          strcpy(piso_new->lb, return_new_line_attribute(Gi_name, ct, line_index, 241, 1.0));
		       else
                          strcpy(piso_new->lb, return_new_line_attribute(Gi_name, ct, line_index, color_index,width_val));
                    } else {
                       strcpy(piso_new->lb, Tl_name);
                    }
	         }
                 if (font_index != 999) {
                    if (listtt == Py_None)
                       strcpy(piso_new->tb,"default");
                    else {
                       strcpy(piso_new->tb, return_new_text_attribute(Gi_name,ct,font_index,text_color_index));
		    }
                    strcpy(piso_new->to,"default");/* Use 'default' text orientation */
                 } else { /* must be a text object */
                     if (strncmp(Tf_name,"__Tt__.",7) == 0) {
                         strcpy(piso_new->tb, Tf_name+7);
                         strcpy(piso_new->to,"default");
                     } else if (strncmp(Tf_name,"__To__.",7) == 0) {
                         strcpy(piso_new->to, Tf_name+7);
                         strcpy(piso_new->tb,"default");
                     } else { /* must be text combined */
                         tptr = strstr(Tf_name, "__");
                         strncpy(piso_new->tb, Tf_name,(strlen(Tf_name)-strlen(tptr)));
                         piso_new->tb[(strlen(Tf_name)-strlen(tptr))] = '\0';
                         strcpy(piso_new->to, tptr+2);
                     }
	         }
		 ct++;lct++;
                 piso_new->next = NULL;

                 /* Add to the new fill range link list */
                 if (tpiso == NULL)
                    get_gitab->pGi_attr->line = tpiso = piso_new;
                 else {
                    tpiso->next = piso_new;
                    tpiso = piso_new;
                 }
              }
	   }
	}

	chk_mov_Gi(get_gitab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new isoline graphics method by copying from an existing
 * isoline graphics method. If no source copy name argument is given,
 * then the default isoline graphics method will be used to replicate
 * the new isoline graphics method.
 */
static PyObject *
PyVCS_copyGi(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GI_SRC=NULL, *GI_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Gi_name();
              
        if(PyArg_ParseTuple(args,"|ss", &GI_SRC, &GI_NAME)) {
           if (GI_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source isoline graphics method name.");
                 return NULL;
           }

           if (GI_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", GI_NAME);
        }

        ierr = copy_Gi_name(GI_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating isoline graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing isoline graphics method.
 */
static PyObject *
PyVCS_renameGi(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GI_OLD_NAME=NULL, *GI_NEW_NAME=NULL;
        extern int      renameGi_name();
 
        if(PyArg_ParseTuple(args,"|ss", &GI_OLD_NAME, &GI_NEW_NAME)) {
           if ((GI_OLD_NAME == NULL) || (GI_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new isoline graphics method name.");
                 return NULL;
           }
        }

        ierr = renameGi_name(GI_OLD_NAME, GI_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming isoline graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing isoline graphics method.
 */
static PyObject *
PyVCS_removeGi(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the isoline file name.");
                 return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
	if (removeGi_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed isoline object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The isoline object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing isoline graphics method.
 */
static PyObject *
PyVCS_scriptGi(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GI_NAME=NULL,*MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_isoline();
	FILE *fp;

        if(PyArg_ParseTuple(args,"|sss", &GI_NAME, &SCRIPT_NAME, &MODE)) {
           if (GI_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the isoline name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_isoline(fp, GI_NAME) == 0) {
              sprintf(buf, "Error - Cannot save isoline script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS outline (Go) graphics method member value. 
 */
static PyObject *
PyVCS_getGomember(PyVCScanvas_Object *self, PyObject *args)
{
	char 			*Go_name, *member=NULL, buf[1024];
	int 			i=0, ct=0;
	PyObject 		*GO=NULL, *MEMBER=NULL, *tup, *lp;
	struct go_tab          	*gotab;
    	extern struct go_tab   	Go_tab;
	struct go_attr          *pgo;

	if(PyArg_ParseTuple(args,"|OO",&GO, &MEMBER)) {
           if (GO == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


	Get_Member(GO,"name", "s", &Go_name);
	gotab=&Go_tab;
        while ((gotab != NULL) &&
               (strcmp(gotab->name, Go_name) != 0))
           gotab = gotab->next;

     	if (gotab == NULL) {
	   sprintf(buf,"Cannot find isofill graphics method Go_%s.",Go_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) {
           return Py_BuildValue("s", gotab->pGo_attr->proj);
	} else if (cmpncs(member, "xticlabels1") == 0) {
           return Py_BuildValue("s", gotab->pGo_attr->xtl1);
	} else if (cmpncs(member, "xticlabels2") == 0) {
           return Py_BuildValue("s", gotab->pGo_attr->xtl2);
	} else if (cmpncs(member, "xmtics1") == 0) {
           return Py_BuildValue("s", gotab->pGo_attr->xmt1);
	} else if (cmpncs(member, "xmtics2") == 0) {
           return Py_BuildValue("s", gotab->pGo_attr->xmt2);
	} else if (cmpncs(member, "yticlabels1") == 0) {
           return Py_BuildValue("s", gotab->pGo_attr->ytl1);
	} else if (cmpncs(member, "yticlabels2") == 0) {
           return Py_BuildValue("s", gotab->pGo_attr->ytl2);
	} else if (cmpncs(member, "ymtics1") == 0) {
           return Py_BuildValue("s", gotab->pGo_attr->ymt1);
	} else if (cmpncs(member, "ymtics2") == 0) {
           return Py_BuildValue("s", gotab->pGo_attr->ymt2);
	} else if (cmpncs(member, "datawc_y1") == 0) {
           return Py_BuildValue("f",gotab->pGo_attr->dsp[1]);
	} else if (cmpncs(member, "datawc_y2") == 0) {
           return Py_BuildValue("f",gotab->pGo_attr->dsp[3]);
	} else if (cmpncs(member, "datawc_x1") == 0) {
           return Py_BuildValue("f",gotab->pGo_attr->dsp[0]);
	} else if (cmpncs(member, "datawc_x2") == 0) {
           return Py_BuildValue("f",gotab->pGo_attr->dsp[2]);
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
           return Py_BuildValue("i",gotab->pGo_attr->idsp[1]);
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
           return Py_BuildValue("i",gotab->pGo_attr->idsp[3]);
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
           return Py_BuildValue("i",gotab->pGo_attr->idsp[0]);
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
           return Py_BuildValue("i",gotab->pGo_attr->idsp[2]);
	} else if (cmpncs(member, "datawc_calendar") == 0) {
           return Py_BuildValue("i",gotab->pGo_attr->calendar);
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
           return Py_BuildValue("s",gotab->pGo_attr->timeunits);
	} else if ((cmpncs(member, "xaxisconvert") == 0) &&
                   ((cmpncs(gotab->pGo_attr->xat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "xaxisconvert") == 0) {
           return Py_BuildValue("s", gotab->pGo_attr->xat);
	} else if ((cmpncs(member, "yaxisconvert") == 0) &&
                   ((cmpncs(gotab->pGo_attr->yat, "\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "yaxisconvert") == 0) {
           return Py_BuildValue("s", gotab->pGo_attr->yat);
	} else if (cmpncs(member, "outline") == 0) {
           /* Get the outline structure */
           pgo = gotab->pGo_attr;
           tup = PyList_New(0);
           for (i=0; i<pgo->n; i++) {
              lp = Py_BuildValue("i", pgo->out[i]);
              PyList_Append(tup, lp);
           }
           return tup;
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing outline graphics method and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setGomember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			i,j=0,n,ct=0,lct=1,color_index=0,line_index;
	int 			MODE, value_int, font_index=1, text_color_index=0;	
	long 			value_long;
	float 			value_float, width_index=1.0;
	double 			value_double;
	char 			buf[1024];
        char 			*Go_name, *str=NULL, *member=NULL;
	char			*value_str=NULL, *Tl_name=NULL;
        PyObject 		*GO=NULL, *MEMBER=NULL, *VALUE=NULL;
	PyObject 		*itempk,*itempv,*pkeys,*pvalues;
        PyObject		*listit,*tup, *line_obj, *listtt, *color_obj, *width_obj;
	struct go_tab           *get_gotab=NULL;
	extern int              update_ind;
	struct go_attr          *pgo;
        struct go_tab           *gotab;
        extern struct go_tab    Go_tab;
	extern struct go_tab    *getGo();
	extern int              chk_mov_Go();
	extern int 		vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &GO, &MEMBER, &VALUE, &MODE)) {
           if (GO == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(GO,"name", "s", &Go_name);
        gotab=&Go_tab;
        while ((gotab != NULL) &&
               (strcmp(gotab->name, Go_name) != 0))
           gotab = gotab->next;

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	if (VALUE != NULL) {
	   if (PyString_Check(VALUE)) { /*check string*/
              value_str = PyString_AsString(VALUE);
	   } else if (PyInt_Check(VALUE)) { /*check for int*/
              value_int = (int) PyInt_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for float*/
              value_float = (float) PyFloat_AsDouble(VALUE);
	   } else if (PyLong_Check(VALUE)) { /* check for long*/
              value_long = PyLong_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for double*/
              value_double = PyFloat_AsDouble(VALUE);
	   } else if (PyDict_Check(VALUE)) { /*check for dictionary*/
              value_str = return_vcs_legacy_list(VALUE, member);
	   }
	}

	/*
	 * Set the appropriate outline attribute. But first 
	 * get the outline structure.
         */
	get_gotab = getGo(gotab->name);
	if (cmpncs(member, "projection") == 0) {
	   strcpy(get_gotab->pGo_attr->proj, value_str);
	} else if (cmpncs(member, "xticlabels1") == 0) {
	   strcpy(get_gotab->pGo_attr->xtl1, value_str);
	} else if (cmpncs(member, "xticlabels2") == 0) {
	   strcpy(get_gotab->pGo_attr->xtl2, value_str);
	} else if (cmpncs(member, "xmtics1") == 0) {
	   strcpy(get_gotab->pGo_attr->xmt1, value_str);
	} else if (cmpncs(member, "xmtics2") == 0) {
	   strcpy(get_gotab->pGo_attr->xmt2, value_str);
	} else if (cmpncs(member, "yticlabels1") == 0) {
	   strcpy(get_gotab->pGo_attr->ytl1, value_str);
	} else if (cmpncs(member, "yticlabels2") == 0) {
	   strcpy(get_gotab->pGo_attr->ytl2, value_str);
	} else if (cmpncs(member, "ymtics1") == 0) {
	   strcpy(get_gotab->pGo_attr->ymt1, value_str);
	} else if (cmpncs(member, "ymtics2") == 0) {
	   strcpy(get_gotab->pGo_attr->ymt2, value_str);
	} else if (cmpncs(member, "datawc_x1") == 0) {
	   get_gotab->pGo_attr->dsp[0] = value_float;
	} else if (cmpncs(member, "datawc_y1") == 0) {
	   get_gotab->pGo_attr->dsp[1] = value_float;
	} else if (cmpncs(member, "datawc_x2") == 0) {
	   get_gotab->pGo_attr->dsp[2] = value_float;
	} else if (cmpncs(member, "datawc_y2") == 0) {
	   get_gotab->pGo_attr->dsp[3] = value_float;
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   get_gotab->pGo_attr->idsp[0] = value_int;
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   get_gotab->pGo_attr->idsp[1] = value_int;
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   get_gotab->pGo_attr->idsp[2] = value_int;
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   get_gotab->pGo_attr->idsp[3] = value_int;
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   get_gotab->pGo_attr->calendar = value_int;
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   strcpy(get_gotab->pGo_attr->timeunits, value_str);
	} else if (cmpncs(member, "xaxisconvert") == 0) {
	   strcpy(get_gotab->pGo_attr->xat, value_str);
	} else if (cmpncs(member, "yaxisconvert") == 0) {
	   strcpy(get_gotab->pGo_attr->yat, value_str);
	} else if (cmpncs(member, "outline") == 0) {
           /* get the line values */
           Get_Member(GO,"line", "O", &line_obj);

           /* get the color values */
           Get_Member(GO,"linecolor", "O", &color_obj);

           /* get the color values */
           Get_Member(GO,"linewidth", "O", &width_obj);

           /* Get the outline structure */
           pgo = get_gotab->pGo_attr;

	   /* Clear all outline values */
	   for (j=0; j < pgo->n; j++)
               pgo->out[j] = 0;

           if (line_obj==Py_None)
              line_index = 1; /* default to solid line */
           else {
              if (cmpncs("solid", PyString_AsString(line_obj))==0)
                 line_index = 1;
              else if (cmpncs("dash", PyString_AsString(line_obj))==0)
                 line_index = 2;
              else if (cmpncs("dot", PyString_AsString(line_obj))==0)
                 line_index = 3;
              else if (cmpncs("dash-dot", PyString_AsString(line_obj))==0)
                 line_index = 4;
              else if (cmpncs("long-dash", PyString_AsString(line_obj))==0)
                 line_index = -3;
              else { /* Must be a line object */
                 Tl_name = PyString_AsString(line_obj);
                 line_index = 999;
              }
          }
          if (color_obj == Py_None)
             color_index = 241; /* set color to default black color*/
          else
             color_index = (int) PyInt_AsLong(color_obj);

          if (width_obj == Py_None)
             width_index = 1.0; /* set width to default size 1.0*/
          else
             width_index = (float) PyFloat_AsDouble(width_obj);

           if (line_index != 999) {
	      if ((line_obj==Py_None) && (color_obj == Py_None) && (width_obj == Py_None))
                 strcpy(pgo->lb,"default");
              else
                 strcpy(pgo->lb,return_new_line_attribute(Go_name, 0, line_index, color_index, width_index));
           } else /* must be a line object */
               strcpy(pgo->lb, Tl_name);

           if (PyList_Check(VALUE)) { /* check for list */
              /* Set the outline values */
              for (j=0; j<PyList_Size(VALUE); j++)
                 pgo->out[j] = (int) PyInt_AsLong(PyList_GetItem(VALUE,j));
              pgo->n = j;
	   }
	}

	chk_mov_Go(get_gotab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new outline graphics method by copying from an existing
 * outline graphics method. If no source copy name argument is given,
 * then the default outline graphics method will be used to replicate
 * the new outline graphics method.
 */
static PyObject *
PyVCS_copyGo(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GO_SRC=NULL, *GO_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Go_name();
              
        if(PyArg_ParseTuple(args,"|ss", &GO_SRC, &GO_NAME)) {
           if (GO_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source outline graphics method name.");
                 return NULL;
           }

           if (GO_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", GO_NAME);
        }

        ierr = copy_Go_name(GO_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating outline graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing outline graphics method.
 */
static PyObject *
PyVCS_renameGo(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GO_OLD_NAME=NULL, *GO_NEW_NAME=NULL;
        extern int      renameGo_name();
 
        if(PyArg_ParseTuple(args,"|ss", &GO_OLD_NAME, &GO_NEW_NAME)) {
           if ((GO_OLD_NAME == NULL) || (GO_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new outline graphics method name.");
                 return NULL;
           }
        }

        ierr = renameGo_name(GO_OLD_NAME, GO_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming outline graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing outline graphics method.
 */
static PyObject *
PyVCS_removeGo(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the outline file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeGo_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed outline object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The outline object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing outline graphics method.
 */
static PyObject *
PyVCS_scriptGo(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GO_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_outline();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &GO_NAME, &SCRIPT_NAME, &MODE)) {
           if (GO_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the outline name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);

        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_outline(fp, GO_NAME) == 0) {
              sprintf(buf, "Error - Cannot save outline script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS outfill (Gfo) graphics method member value. 
 */
static PyObject *
PyVCS_getGfomember(PyVCScanvas_Object *self, PyObject *args)
{
	char 			*Gfo_name, *member=NULL, buf[1024];
	int 			i=0, ct=0;
	PyObject 		*GFO=NULL, *MEMBER=NULL, *tup, *lp;
	struct gfo_tab         	*gfotab;
    	extern struct gfo_tab  	Gfo_tab;
	struct gfo_attr         *pgfo;

	if(PyArg_ParseTuple(args,"|OO",&GFO, &MEMBER)) {
           if (GFO == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


	Get_Member(GFO,"name", "s", &Gfo_name);
	gfotab=&Gfo_tab;
        while ((gfotab != NULL) &&
               (strcmp(gfotab->name, Gfo_name) != 0))
           gfotab = gfotab->next;

     	if (gfotab == NULL) {
	   sprintf(buf,"Cannot find isofill graphics method Gfo_%s.",Gfo_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) {
           return Py_BuildValue("s", gfotab->pGfo_attr->proj);
	} else if (cmpncs(member, "xticlabels1") == 0) {
           return Py_BuildValue("s", gfotab->pGfo_attr->xtl1);
	} else if (cmpncs(member, "xticlabels2") == 0) {
           return Py_BuildValue("s", gfotab->pGfo_attr->xtl2);
	} else if (cmpncs(member, "xmtics1") == 0) {
           return Py_BuildValue("s", gfotab->pGfo_attr->xmt1);
	} else if (cmpncs(member, "xmtics2") == 0) {
           return Py_BuildValue("s", gfotab->pGfo_attr->xmt2);
	} else if (cmpncs(member, "yticlabels1") == 0) {
           return Py_BuildValue("s", gfotab->pGfo_attr->ytl1);
	} else if (cmpncs(member, "yticlabels2") == 0) {
           return Py_BuildValue("s", gfotab->pGfo_attr->ytl2);
	} else if (cmpncs(member, "ymtics1") == 0) {
           return Py_BuildValue("s", gfotab->pGfo_attr->ymt1);
	} else if (cmpncs(member, "ymtics2") == 0) {
           return Py_BuildValue("s", gfotab->pGfo_attr->ymt2);
	} else if (cmpncs(member, "datawc_y1") == 0) {
           return Py_BuildValue("f",gfotab->pGfo_attr->dsp[1]);
	} else if (cmpncs(member, "datawc_y2") == 0) {
           return Py_BuildValue("f",gfotab->pGfo_attr->dsp[3]);
	} else if (cmpncs(member, "datawc_x1") == 0) {
           return Py_BuildValue("f",gfotab->pGfo_attr->dsp[0]);
	} else if (cmpncs(member, "datawc_x2") == 0) {
           return Py_BuildValue("f",gfotab->pGfo_attr->dsp[2]);
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
           return Py_BuildValue("i",gfotab->pGfo_attr->idsp[1]);
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
           return Py_BuildValue("i",gfotab->pGfo_attr->idsp[3]);
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
           return Py_BuildValue("i",gfotab->pGfo_attr->idsp[0]);
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
           return Py_BuildValue("i",gfotab->pGfo_attr->idsp[2]);
	} else if (cmpncs(member, "datawc_calendar") == 0) {
           return Py_BuildValue("i",gfotab->pGfo_attr->calendar);
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
           return Py_BuildValue("s",gfotab->pGfo_attr->timeunits);
	} else if ((cmpncs(member, "xaxisconvert") == 0) &&
                   ((cmpncs(gfotab->pGfo_attr->xat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "xaxisconvert") == 0) {
           return Py_BuildValue("s", gfotab->pGfo_attr->xat);
	} else if ((cmpncs(member, "yaxisconvert") == 0) &&
                   ((cmpncs(gfotab->pGfo_attr->yat, "\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "yaxisconvert") == 0) {
           return Py_BuildValue("s", gfotab->pGfo_attr->yat);
	} else if (cmpncs(member, "outfill") == 0) {
           /* Get the outfill structure */
           pgfo = gfotab->pGfo_attr;
           tup = PyList_New(0);
           for (i=0; i<pgfo->n; i++) {
              lp = Py_BuildValue("i", pgfo->out[i]);
              PyList_Append(tup, lp);
           }
           return tup;
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing outfill graphics method and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setGfomember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			i,j=0,n,ct=0,lct=1,color_index=0;
	int 			MODE, value_int, style_index=1, text_color_index=0;	
	long 			value_long;
	float 			value_float;
	double 			value_double;
	char 			buf[1024], *style;
        char 			*Gfo_name, *str=NULL, *member=NULL;
	char			*value_str=NULL;
        PyObject 		*GFO=NULL, *MEMBER=NULL, *VALUE=NULL;
	PyObject 		*itempk,*itempv,*pkeys,*pvalues, *index_obj;
        PyObject		*listit,*tup, *listtt, *color_obj;
	struct gfo_tab          *get_gfotab=NULL;
	extern int              update_ind;
	struct gfo_attr         *pgfo;
        struct gfo_tab          *gfotab;
        extern struct gfo_tab   Gfo_tab;
	extern struct gfo_tab   *getGfo();
	extern int              chk_mov_Gfo();
	extern int 		vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &GFO, &MEMBER, &VALUE, &MODE)) {
           if (GFO == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(GFO,"name", "s", &Gfo_name);
        gfotab=&Gfo_tab;
        while ((gfotab != NULL) &&
               (strcmp(gfotab->name, Gfo_name) != 0))
           gfotab = gfotab->next;

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	if (VALUE != NULL) {
	   if (PyString_Check(VALUE)) { /*check string*/
              value_str = PyString_AsString(VALUE);
	   } else if (PyInt_Check(VALUE)) { /*check for int*/
              value_int = (int) PyInt_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for float*/
              value_float = (float) PyFloat_AsDouble(VALUE);
	   } else if (PyLong_Check(VALUE)) { /* check for long*/
              value_long = PyLong_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for double*/
              value_double = PyFloat_AsDouble(VALUE);
	   } else if (PyDict_Check(VALUE)) { /*check for dictionary*/
              value_str = return_vcs_legacy_list(VALUE, member);
	   }
	}

	/*
	 * Set the appropriate outfill attribute. But first 
	 * get the outfill structure.
         */
	get_gfotab = getGfo(gfotab->name);
	if (cmpncs(member, "projection") == 0) {
	   strcpy(get_gfotab->pGfo_attr->proj, value_str);
	} else if (cmpncs(member, "xticlabels1") == 0) {
	   strcpy(get_gfotab->pGfo_attr->xtl1, value_str);
	} else if (cmpncs(member, "xticlabels2") == 0) {
	   strcpy(get_gfotab->pGfo_attr->xtl2, value_str);
	} else if (cmpncs(member, "xmtics1") == 0) {
	   strcpy(get_gfotab->pGfo_attr->xmt1, value_str);
	} else if (cmpncs(member, "xmtics2") == 0) {
	   strcpy(get_gfotab->pGfo_attr->xmt2, value_str);
	} else if (cmpncs(member, "yticlabels1") == 0) {
	   strcpy(get_gfotab->pGfo_attr->ytl1, value_str);
	} else if (cmpncs(member, "yticlabels2") == 0) {
	   strcpy(get_gfotab->pGfo_attr->ytl2, value_str);
	} else if (cmpncs(member, "ymtics1") == 0) {
	   strcpy(get_gfotab->pGfo_attr->ymt1, value_str);
	} else if (cmpncs(member, "ymtics2") == 0) {
	   strcpy(get_gfotab->pGfo_attr->ymt2, value_str);
	} else if (cmpncs(member, "datawc_x1") == 0) {
	   get_gfotab->pGfo_attr->dsp[0] = value_float;
	} else if (cmpncs(member, "datawc_y1") == 0) {
	   get_gfotab->pGfo_attr->dsp[1] = value_float;
	} else if (cmpncs(member, "datawc_x2") == 0) {
	   get_gfotab->pGfo_attr->dsp[2] = value_float;
	} else if (cmpncs(member, "datawc_y2") == 0) {
	   get_gfotab->pGfo_attr->dsp[3] = value_float;
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   get_gfotab->pGfo_attr->idsp[0] = value_int;
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   get_gfotab->pGfo_attr->idsp[1] = value_int;
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   get_gfotab->pGfo_attr->idsp[2] = value_int;
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   get_gfotab->pGfo_attr->idsp[3] = value_int;
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   get_gfotab->pGfo_attr->calendar = value_int;
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   strcpy(get_gfotab->pGfo_attr->timeunits, value_str);
	} else if (cmpncs(member, "xaxisconvert") == 0) {
	   strcpy(get_gfotab->pGfo_attr->xat, value_str);
	} else if (cmpncs(member, "yaxisconvert") == 0) {
	   strcpy(get_gfotab->pGfo_attr->yat, value_str);
	} else if (cmpncs(member, "outfill") == 0) {
           /* get the line values */
           Get_Member(GFO,"fillareastyle", "s", &style);

           /* get the color values */
           Get_Member(GFO,"fillareacolor", "O", &color_obj);

           /* get the color values */
           Get_Member(GFO,"fillareaindex", "O", &index_obj);

           /* Get the outfill structure */
           pgfo = get_gfotab->pGfo_attr;

	   /* Clear all outfill values */
	   for (j=0; j < pgfo->n; j++)
               pgfo->out[j] = 0;

          if (color_obj == Py_None)
             color_index = 241; /* set color to default black color*/
          else
             color_index = (int) PyInt_AsLong(color_obj);

          if (index_obj == Py_None)
             style_index = 1; /* set index to default 1 */
          else
             style_index = (int) PyInt_AsLong(index_obj);

           if ((strcmp(style,"solid") == 0) || (strcmp(style,"hatch") == 0) ||
              (strcmp(style,"pattern") == 0)) {
	      if ((index_obj == Py_None) && (color_obj == Py_None))
                 strcpy(pgfo->f,"default");
              else
                 strcpy(pgfo->f,return_new_fillarea_attribute(Gfo_name, 0, style, style_index, color_index));
           } else /* Must be a fillarea */
             strcpy(pgfo->f,style);

           if (PyList_Check(VALUE)) { /* check for list */
              /* Set the outfill values */
              for (j=0; j<PyList_Size(VALUE); j++)
                 pgfo->out[j] = (int) PyInt_AsLong(PyList_GetItem(VALUE,j));
              pgfo->n = j;
	   }
	}

	chk_mov_Gfo(get_gfotab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new outfill graphics method by copying from an existing
 * outfill graphics method. If no source copy name argument is given,
 * then the default outfill graphics method will be used to replicate
 * the new outfill graphics method.
 */
static PyObject *
PyVCS_copyGfo(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GFO_SRC=NULL, *GFO_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Gfo_name();
              
        if(PyArg_ParseTuple(args,"|ss", &GFO_SRC, &GFO_NAME)) {
           if (GFO_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source outfill graphics method name.");
                 return NULL;
           }

           if (GFO_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", GFO_NAME);
        }

        ierr = copy_Gfo_name(GFO_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating outfill graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing outfill graphics method.
 */
static PyObject *
PyVCS_renameGfo(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GFO_OLD_NAME=NULL, *GFO_NEW_NAME=NULL;
        extern int      renameGfo_name();
 
        if(PyArg_ParseTuple(args,"|ss", &GFO_OLD_NAME, &GFO_NEW_NAME)) {
           if ((GFO_OLD_NAME == NULL) || (GFO_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new outfill graphics method name.");
                 return NULL;
           }
        }

        ierr = renameGfo_name(GFO_OLD_NAME, GFO_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming outfill graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing outfill graphics method.
 */
static PyObject *
PyVCS_removeGfo(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        /* Return NULL Python Object or Python String Object */
        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the outfill file name.");
                 return NULL;
           }
        }
	if (removeGfo_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed outfill object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The outfill object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing outfill graphics method.
 */
static PyObject *
PyVCS_scriptGfo(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GFO_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_outfill();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &GFO_NAME, &SCRIPT_NAME, &MODE)) {
           if (GFO_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the outfill name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_outfill(fp, GFO_NAME) == 0) {
              sprintf(buf, "Error - Cannot save outfill script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS Xyvsy (GXy) graphics method member value. 
 */
static PyObject *
PyVCS_getGXymember(PyVCScanvas_Object *self, PyObject *args)
{
	char 			*GXy_name, *member=NULL, buf[1024];
	int 			i=0, ct=0;
	PyObject 		*GXY=NULL, *MEMBER=NULL, *tup, *lp;
	struct gXy_tab         	*gXytab;
    	extern struct gXy_tab  	GXy_tab;
	struct gXy_attr         *pgXy;

	if(PyArg_ParseTuple(args,"|OO",&GXY, &MEMBER)) {
           if (GXY == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


	Get_Member(GXY,"name", "s", &GXy_name);
	gXytab=&GXy_tab;
        while ((gXytab != NULL) &&
               (strcmp(gXytab->name, GXy_name) != 0))
           gXytab = gXytab->next;

     	if (gXytab == NULL) {
	   sprintf(buf,"Cannot find Xyvsy graphics method GXy_%s.",GXy_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) {
           return Py_BuildValue("s", gXytab->pGXy_attr->proj);
	} else if (cmpncs(member, "xticlabels1") == 0) {
           return Py_BuildValue("s", gXytab->pGXy_attr->xtl1);
	} else if (cmpncs(member, "xticlabels2") == 0) {
           return Py_BuildValue("s", gXytab->pGXy_attr->xtl2);
	} else if (cmpncs(member, "xmtics1") == 0) {
           return Py_BuildValue("s", gXytab->pGXy_attr->xmt1);
	} else if (cmpncs(member, "xmtics2") == 0) {
           return Py_BuildValue("s", gXytab->pGXy_attr->xmt2);
	} else if (cmpncs(member, "yticlabels1") == 0) {
           return Py_BuildValue("s", gXytab->pGXy_attr->ytl1);
	} else if (cmpncs(member, "yticlabels2") == 0) {
           return Py_BuildValue("s", gXytab->pGXy_attr->ytl2);
	} else if (cmpncs(member, "ymtics1") == 0) {
           return Py_BuildValue("s", gXytab->pGXy_attr->ymt1);
	} else if (cmpncs(member, "ymtics2") == 0) {
           return Py_BuildValue("s", gXytab->pGXy_attr->ymt2);
	} else if (cmpncs(member, "datawc_y1") == 0) {
           return Py_BuildValue("f",gXytab->pGXy_attr->dsp[1]);
	} else if (cmpncs(member, "datawc_y2") == 0) {
           return Py_BuildValue("f",gXytab->pGXy_attr->dsp[3]);
	} else if (cmpncs(member, "datawc_x1") == 0) {
           return Py_BuildValue("f",gXytab->pGXy_attr->dsp[0]);
	} else if (cmpncs(member, "datawc_x2") == 0) {
           return Py_BuildValue("f",gXytab->pGXy_attr->dsp[2]);
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
           return Py_BuildValue("i",gXytab->pGXy_attr->idsp[1]);
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
           return Py_BuildValue("i",gXytab->pGXy_attr->idsp[3]);
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
           return Py_BuildValue("i",gXytab->pGXy_attr->idsp[0]);
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
           return Py_BuildValue("i",gXytab->pGXy_attr->idsp[2]);
	} else if (cmpncs(member, "datawc_calendar") == 0) {
           return Py_BuildValue("i",gXytab->pGXy_attr->calendar);
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
           return Py_BuildValue("s",gXytab->pGXy_attr->timeunits);
	} else if ((cmpncs(member, "xaxisconvert") == 0) &&
                   ((cmpncs(gXytab->pGXy_attr->xat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "xaxisconvert") == 0) {
           return Py_BuildValue("s", gXytab->pGXy_attr->xat);
        } else if ((cmpncs(member, "yaxisconvert") == 0) &&
                   ((cmpncs(gXytab->pGXy_attr->yat, "\0") == 0))) {
           return Py_BuildValue("s", "linear");
        } else if (cmpncs(member, "yaxisconvert") == 0) {
           return Py_BuildValue("s", gXytab->pGXy_attr->yat);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing Xyvsy graphics method and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setGXymember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			i,j=0,n,ct=0,lct=1,line_index,lcolor_index=0;
	int 			MODE, value_int, marker_index, mcolor_index=0;	
	int			msize_index=0;
	long 			value_long;
	float 			value_float, lwidth_index=1.0;
	double 			value_double;
	char 			buf[1024], *style;
        char 			*GXy_name, *str=NULL, *member=NULL;
	char			*value_str=NULL, *Tl_name=NULL, *Tm_name=NULL;
        PyObject 		*GXY=NULL, *MEMBER=NULL, *VALUE=NULL;
	PyObject 		*itempk,*itempv,*pkeys,*pvalues, *line_obj;
        PyObject		*marker_obj, *mcolor_obj, *lcolor_obj, *lwidth_obj, *msize_obj;
	struct gXy_tab          *get_gXytab=NULL;
	extern int              update_ind;
	struct gXy_attr         *pgXy;
        struct gXy_tab          *gXytab;
        extern struct gXy_tab   GXy_tab;
	extern struct gXy_tab   *getGXy();
	extern int              chk_mov_GXy();
	extern int 		vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &GXY, &MEMBER, &VALUE, &MODE)) {
           if (GXY == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(GXY,"name", "s", &GXy_name);
        gXytab=&GXy_tab;
        while ((gXytab != NULL) &&
               (strcmp(gXytab->name, GXy_name) != 0))
           gXytab = gXytab->next;

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	if (VALUE != NULL) {
	   if (PyString_Check(VALUE)) { /*check string*/
              value_str = PyString_AsString(VALUE);
	   } else if (PyInt_Check(VALUE)) { /*check for int*/
              value_int = (int) PyInt_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for float*/
              value_float = (float) PyFloat_AsDouble(VALUE);
	   } else if (PyLong_Check(VALUE)) { /* check for long*/
              value_long = PyLong_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for double*/
              value_double = PyFloat_AsDouble(VALUE);
	   } else if (PyDict_Check(VALUE)) { /*check for dictionary*/
              value_str = return_vcs_legacy_list(VALUE, member);
	   }
	}

	/*
	 * Set the appropriate Xyvsy attribute. But first 
	 * get the Xyvsy structure.
         */
	get_gXytab = getGXy(gXytab->name);
	if (cmpncs(member, "projection") == 0) {
	   strcpy(get_gXytab->pGXy_attr->proj, value_str);
	} else if (cmpncs(member, "xticlabels1") == 0) {
	   strcpy(get_gXytab->pGXy_attr->xtl1, value_str);
	} else if (cmpncs(member, "xticlabels2") == 0) {
	   strcpy(get_gXytab->pGXy_attr->xtl2, value_str);
	} else if (cmpncs(member, "xmtics1") == 0) {
	   strcpy(get_gXytab->pGXy_attr->xmt1, value_str);
	} else if (cmpncs(member, "xmtics2") == 0) {
	   strcpy(get_gXytab->pGXy_attr->xmt2, value_str);
	} else if (cmpncs(member, "yticlabels1") == 0) {
	   strcpy(get_gXytab->pGXy_attr->ytl1, value_str);
	} else if (cmpncs(member, "yticlabels2") == 0) {
	   strcpy(get_gXytab->pGXy_attr->ytl2, value_str);
	} else if (cmpncs(member, "ymtics1") == 0) {
	   strcpy(get_gXytab->pGXy_attr->ymt1, value_str);
	} else if (cmpncs(member, "ymtics2") == 0) {
	   strcpy(get_gXytab->pGXy_attr->ymt2, value_str);
	} else if (cmpncs(member, "datawc_x1") == 0) {
	   get_gXytab->pGXy_attr->dsp[0] = value_float;
	} else if (cmpncs(member, "datawc_y1") == 0) {
	   get_gXytab->pGXy_attr->dsp[1] = value_float;
	} else if (cmpncs(member, "datawc_x2") == 0) {
	   get_gXytab->pGXy_attr->dsp[2] = value_float;
	} else if (cmpncs(member, "datawc_y2") == 0) {
	   get_gXytab->pGXy_attr->dsp[3] = value_float;
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   get_gXytab->pGXy_attr->idsp[0] = value_int;
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   get_gXytab->pGXy_attr->idsp[1] = value_int;
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   get_gXytab->pGXy_attr->idsp[2] = value_int;
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   get_gXytab->pGXy_attr->idsp[3] = value_int;
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   get_gXytab->pGXy_attr->calendar = value_int;
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   strcpy(get_gXytab->pGXy_attr->timeunits, value_str);
	} else if (cmpncs(member, "xaxisconvert") == 0) {
	   strcpy(get_gXytab->pGXy_attr->xat, value_str);
	} else if (cmpncs(member, "yaxisconvert") == 0) {
	   strcpy(get_gXytab->pGXy_attr->yat, value_str);
	} else if ((cmpncs(member, "line") == 0) || (cmpncs(member, "linecolor") == 0) ||
                   (cmpncs(member, "linewidth") == 0) ||
		   (cmpncs(member, "marker") == 0) || (cmpncs(member, "markercolor") == 0) ||
		   (cmpncs(member, "markersize") == 0)) {
           /* get the line values */
           Get_Member(GXY,"line", "O", &line_obj);

           /* get the line color values */
           Get_Member(GXY,"linecolor", "O", &lcolor_obj);

           /* get the line width values */
           Get_Member(GXY,"linewidth", "O", &lwidth_obj);

           /* get the marker values */
           Get_Member(GXY,"marker", "O", &marker_obj);

           /* get the marker color values */
           Get_Member(GXY,"markercolor", "O", &mcolor_obj);

           /* get the marker size values */
           Get_Member(GXY,"markersize", "O", &msize_obj);

           /* Get the Xyvsy structure */
           pgXy = get_gXytab->pGXy_attr;

           if (line_obj==Py_None)
              line_index = 1; /* default to solid line */
           else {
              if (cmpncs("solid", PyString_AsString(line_obj))==0)
                 line_index = 1;
              else if (cmpncs("dash", PyString_AsString(line_obj))==0)
                 line_index = 2;
              else if (cmpncs("dot", PyString_AsString(line_obj))==0)
                 line_index = 3;
              else if (cmpncs("dash-dot", PyString_AsString(line_obj))==0)
                 line_index = 4;
              else if (cmpncs("long-dash", PyString_AsString(line_obj))==0)
                 line_index = -3;
              else {
                 Tl_name = PyString_AsString(line_obj);
                 line_index = 999;
              }
           }
           if (line_index != 999) {
              if (lcolor_obj == Py_None)
                 lcolor_index = 241; /* set color to default black color*/
              else
                 lcolor_index = (int) PyInt_AsLong(lcolor_obj);

              if (lwidth_obj == Py_None)
                 lwidth_index = 1.0; /* set width to default size of 1.0*/
              else
                 lwidth_index = (float) PyFloat_AsDouble(lwidth_obj);

	      if ((line_obj==Py_None) && (lcolor_obj == Py_None) && (lwidth_obj == Py_None))
                 strcpy(pgXy->lb,"default");
              else
                 strcpy(pgXy->lb,return_new_line_attribute(GXy_name,0,line_index,lcolor_index,lwidth_index));
           } else /* must be a line object */
               strcpy(pgXy->lb, Tl_name);

           if (marker_obj==Py_None)
              marker_index = 0; /* default to no markers */
           else {
              if (cmpncs("dot", PyString_AsString(marker_obj))==0)
                 marker_index = 1;
              else if (cmpncs("plus", PyString_AsString(marker_obj))==0)
                 marker_index = 2;
              else if (cmpncs("star", PyString_AsString(marker_obj))==0)
                 marker_index = 3;
              else if (cmpncs("circle", PyString_AsString(marker_obj))==0)
                 marker_index = 4;
              else if (cmpncs("cross", PyString_AsString(marker_obj))==0)
                 marker_index = 5;
              else if (cmpncs("diamond", PyString_AsString(marker_obj))==0)
                 marker_index = 6;
              else if (cmpncs("triangle_up", PyString_AsString(marker_obj))==0)
                 marker_index = 7;
              else if (cmpncs("triangle_down", PyString_AsString(marker_obj))==0)
                 marker_index = 8;
              else if (cmpncs("triangle_left", PyString_AsString(marker_obj))==0)
                 marker_index = 9;
              else if (cmpncs("triangle_right", PyString_AsString(marker_obj))==0)
                 marker_index = 10;
              else if (cmpncs("square", PyString_AsString(marker_obj))==0)
                 marker_index = 11;
              else if (cmpncs("diamond_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 12;
              else if (cmpncs("triangle_up_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 13;
              else if (cmpncs("triangle_down_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 14;
              else if (cmpncs("triangle_left_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 15;
              else if (cmpncs("triangle_right_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 16;
              else if (cmpncs("square_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 17;
              else if (cmpncs("hurricane", PyString_AsString(marker_obj))==0)
                 marker_index = 18;
	      /* Arulalan Weather Markers */
 	      else if (cmpncs("w00", PyString_AsString(marker_obj))==0)
                 marker_index = 100;
              else if (cmpncs("w01", PyString_AsString(marker_obj))==0)
                 marker_index = 101;
              else if (cmpncs("w02", PyString_AsString(marker_obj))==0)
                 marker_index = 102;
              else if (cmpncs("w03", PyString_AsString(marker_obj))==0)
                 marker_index = 103;
 	      else if (cmpncs("w04", PyString_AsString(marker_obj))==0)
                 marker_index = 104;
              else if (cmpncs("w05", PyString_AsString(marker_obj))==0)
                 marker_index = 105;
              else if (cmpncs("w06", PyString_AsString(marker_obj))==0)
                 marker_index = 106;
              else if (cmpncs("w07", PyString_AsString(marker_obj))==0)
                 marker_index = 107;
              else if (cmpncs("w08", PyString_AsString(marker_obj))==0)
                 marker_index = 108;
              else if (cmpncs("w09", PyString_AsString(marker_obj))==0)
                 marker_index = 109;
              else if (cmpncs("w10", PyString_AsString(marker_obj))==0)
                 marker_index = 110;
              else if (cmpncs("w11", PyString_AsString(marker_obj))==0)
                 marker_index = 111;
              else if (cmpncs("w12", PyString_AsString(marker_obj))==0)
                 marker_index = 112;
              else if (cmpncs("w13", PyString_AsString(marker_obj))==0)
                 marker_index = 113;
	      else if (cmpncs("w14", PyString_AsString(marker_obj))==0)
                 marker_index = 114;
              else if (cmpncs("w15", PyString_AsString(marker_obj))==0)
                 marker_index = 115;
              else if (cmpncs("w16", PyString_AsString(marker_obj))==0)
                 marker_index = 116;
              else if (cmpncs("w17", PyString_AsString(marker_obj))==0)
                 marker_index = 117;
              else if (cmpncs("w18", PyString_AsString(marker_obj))==0)
                 marker_index = 118;
              else if (cmpncs("w19", PyString_AsString(marker_obj))==0)
                 marker_index = 119;
	      else if (cmpncs("w20", PyString_AsString(marker_obj))==0)
                 marker_index = 120;
              else if (cmpncs("w21", PyString_AsString(marker_obj))==0)
                 marker_index = 121;
              else if (cmpncs("w22", PyString_AsString(marker_obj))==0)
                 marker_index = 122;
              else if (cmpncs("w23", PyString_AsString(marker_obj))==0)
                 marker_index = 123;
 	      else if (cmpncs("w24", PyString_AsString(marker_obj))==0)
                 marker_index = 124;
              else if (cmpncs("w25", PyString_AsString(marker_obj))==0)
                 marker_index = 125;
              else if (cmpncs("w26", PyString_AsString(marker_obj))==0)
                 marker_index = 126;
              else if (cmpncs("w27", PyString_AsString(marker_obj))==0)
                 marker_index = 127;
              else if (cmpncs("w28", PyString_AsString(marker_obj))==0)
                 marker_index = 128;
              else if (cmpncs("w29", PyString_AsString(marker_obj))==0)
                 marker_index = 129;
              else if (cmpncs("w30", PyString_AsString(marker_obj))==0)
                 marker_index = 130;
              else if (cmpncs("w31", PyString_AsString(marker_obj))==0)
                 marker_index = 131;
              else if (cmpncs("w32", PyString_AsString(marker_obj))==0)
                 marker_index = 132;
              else if (cmpncs("w33", PyString_AsString(marker_obj))==0)
                 marker_index = 133;
	      else if (cmpncs("w34", PyString_AsString(marker_obj))==0)
                 marker_index = 134;
              else if (cmpncs("w35", PyString_AsString(marker_obj))==0)
                 marker_index = 135;
              else if (cmpncs("w36", PyString_AsString(marker_obj))==0)
                 marker_index = 136;
              else if (cmpncs("w37", PyString_AsString(marker_obj))==0)
                 marker_index = 137;
              else if (cmpncs("w38", PyString_AsString(marker_obj))==0)
                 marker_index = 138;
              else if (cmpncs("w39", PyString_AsString(marker_obj))==0)
                 marker_index = 139;
	      else if (cmpncs("w40", PyString_AsString(marker_obj))==0)
                 marker_index = 140;
              else if (cmpncs("w41", PyString_AsString(marker_obj))==0)
                 marker_index = 141;
              else if (cmpncs("w42", PyString_AsString(marker_obj))==0)
                 marker_index = 142;
              else if (cmpncs("w43", PyString_AsString(marker_obj))==0)
                 marker_index = 143;
 	      else if (cmpncs("w44", PyString_AsString(marker_obj))==0)
                 marker_index = 144;
              else if (cmpncs("w45", PyString_AsString(marker_obj))==0)
                 marker_index = 145;
              else if (cmpncs("w46", PyString_AsString(marker_obj))==0)
                 marker_index = 146;
              else if (cmpncs("w47", PyString_AsString(marker_obj))==0)
                 marker_index = 147;
              else if (cmpncs("w48", PyString_AsString(marker_obj))==0)
                 marker_index = 148;
              else if (cmpncs("w49", PyString_AsString(marker_obj))==0)
                 marker_index = 149;
              else if (cmpncs("w50", PyString_AsString(marker_obj))==0)
                 marker_index = 150;
              else if (cmpncs("w51", PyString_AsString(marker_obj))==0)
                 marker_index = 151;
              else if (cmpncs("w52", PyString_AsString(marker_obj))==0)
                 marker_index = 152;
              else if (cmpncs("w53", PyString_AsString(marker_obj))==0)
                 marker_index = 153;
	      else if (cmpncs("w54", PyString_AsString(marker_obj))==0)
                 marker_index = 154;
              else if (cmpncs("w55", PyString_AsString(marker_obj))==0)
                 marker_index = 155;
              else if (cmpncs("w56", PyString_AsString(marker_obj))==0)
                 marker_index = 156;
              else if (cmpncs("w57", PyString_AsString(marker_obj))==0)
                 marker_index = 157;
              else if (cmpncs("w58", PyString_AsString(marker_obj))==0)
                 marker_index = 158;
              else if (cmpncs("w59", PyString_AsString(marker_obj))==0)
                 marker_index = 159;
	      else if (cmpncs("w60", PyString_AsString(marker_obj))==0)
                 marker_index = 160;
              else if (cmpncs("w61", PyString_AsString(marker_obj))==0)
                 marker_index = 161;
              else if (cmpncs("w62", PyString_AsString(marker_obj))==0)
                 marker_index = 162;
              else if (cmpncs("w63", PyString_AsString(marker_obj))==0)
                 marker_index = 163;
 	      else if (cmpncs("w64", PyString_AsString(marker_obj))==0)
                 marker_index = 164;
              else if (cmpncs("w65", PyString_AsString(marker_obj))==0)
                 marker_index = 165;
              else if (cmpncs("w66", PyString_AsString(marker_obj))==0)
                 marker_index = 166;
              else if (cmpncs("w67", PyString_AsString(marker_obj))==0)
                 marker_index = 167;
              else if (cmpncs("w68", PyString_AsString(marker_obj))==0)
                 marker_index = 168;
              else if (cmpncs("w69", PyString_AsString(marker_obj))==0)
                 marker_index = 169;
              else if (cmpncs("w70", PyString_AsString(marker_obj))==0)
                 marker_index = 170;
              else if (cmpncs("w71", PyString_AsString(marker_obj))==0)
                 marker_index = 171;
              else if (cmpncs("w72", PyString_AsString(marker_obj))==0)
                 marker_index = 172;
              else if (cmpncs("w73", PyString_AsString(marker_obj))==0)
                 marker_index = 173;
	      else if (cmpncs("w74", PyString_AsString(marker_obj))==0)
                 marker_index = 174;
              else if (cmpncs("w75", PyString_AsString(marker_obj))==0)
                 marker_index = 175;
              else if (cmpncs("w76", PyString_AsString(marker_obj))==0)
                 marker_index = 176;
              else if (cmpncs("w77", PyString_AsString(marker_obj))==0)
                 marker_index = 177;
              else if (cmpncs("w78", PyString_AsString(marker_obj))==0)
                 marker_index = 178;
              else if (cmpncs("w79", PyString_AsString(marker_obj))==0)
                 marker_index = 179;
	      else if (cmpncs("w80", PyString_AsString(marker_obj))==0)
                 marker_index = 180;
              else if (cmpncs("w81", PyString_AsString(marker_obj))==0)
                 marker_index = 181;
              else if (cmpncs("w82", PyString_AsString(marker_obj))==0)
                 marker_index = 182;
              else if (cmpncs("w83", PyString_AsString(marker_obj))==0)
                 marker_index = 183;
 	      else if (cmpncs("w84", PyString_AsString(marker_obj))==0)
                 marker_index = 184;
              else if (cmpncs("w85", PyString_AsString(marker_obj))==0)
                 marker_index = 185;
              else if (cmpncs("w86", PyString_AsString(marker_obj))==0)
                 marker_index = 186;
              else if (cmpncs("w87", PyString_AsString(marker_obj))==0)
                 marker_index = 187;
              else if (cmpncs("w88", PyString_AsString(marker_obj))==0)
                 marker_index = 188;
              else if (cmpncs("w89", PyString_AsString(marker_obj))==0)
                 marker_index = 189;
              else if (cmpncs("w90", PyString_AsString(marker_obj))==0)
                 marker_index = 190;
              else if (cmpncs("w91", PyString_AsString(marker_obj))==0)
                 marker_index = 191;
              else if (cmpncs("w92", PyString_AsString(marker_obj))==0)
                 marker_index = 192;
              else if (cmpncs("w93", PyString_AsString(marker_obj))==0)
                 marker_index = 193;
	      else if (cmpncs("w94", PyString_AsString(marker_obj))==0)
                 marker_index = 194;
              else if (cmpncs("w95", PyString_AsString(marker_obj))==0)
                 marker_index = 195;
              else if (cmpncs("w96", PyString_AsString(marker_obj))==0)
                 marker_index = 196;
              else if (cmpncs("w97", PyString_AsString(marker_obj))==0)
                 marker_index = 197;
              else if (cmpncs("w98", PyString_AsString(marker_obj))==0)
                 marker_index = 198;
              else if (cmpncs("w99", PyString_AsString(marker_obj))==0)
                 marker_index = 199;
	      else if (cmpncs("w200", PyString_AsString(marker_obj))==0)
                 marker_index = 200;
              else if (cmpncs("w201", PyString_AsString(marker_obj))==0)
                 marker_index = 201;
              else if (cmpncs("w202", PyString_AsString(marker_obj))==0)
                 marker_index = 202;
             else {
                 Tm_name = PyString_AsString(marker_obj);
                 marker_index = 999;
              }
          }
          if (marker_index != 999) {
             if (mcolor_obj == Py_None)
                mcolor_index = 241; /* set color to default black color*/
             else
                mcolor_index = (int) PyInt_AsLong(mcolor_obj);
             if (msize_obj == Py_None)
                msize_index = 7; /* set marker size to default size of 7*/
             else
                msize_index = (int) PyInt_AsLong(msize_obj);

              if ((marker_obj==Py_None) && (mcolor_obj == Py_None) && (msize_obj == Py_None))
                 strcpy(pgXy->mb,"\0");
              else
                 strcpy(pgXy->mb,return_new_marker_attribute(GXy_name, 0, marker_index, mcolor_index, msize_index));
          } else /* must be a marker object */
               strcpy(pgXy->mb, Tm_name);
	}

	chk_mov_GXy(get_gXytab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new Xyvsy graphics method by copying from an existing
 * Xyvsy graphics method. If no source copy name argument is given,
 * then the default Xyvsy graphics method will be used to replicate
 * the new Xyvsy graphics method.
 */
static PyObject *
PyVCS_copyGXy(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GXY_SRC=NULL, *GXY_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_GXy_name();
              
        if(PyArg_ParseTuple(args,"|ss", &GXY_SRC, &GXY_NAME)) {
           if (GXY_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source Xyvsy graphics method name.");
                 return NULL;
           }

           if (GXY_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", GXY_NAME);
        }

        ierr = copy_GXy_name(GXY_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating Xyvsy graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing Xyvsy graphics method.
 */
static PyObject *
PyVCS_renameGXy(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GXY_OLD_NAME=NULL, *GXY_NEW_NAME=NULL;
        extern int      renameGXy_name();
 
        if(PyArg_ParseTuple(args,"|ss", &GXY_OLD_NAME, &GXY_NEW_NAME)) {
           if ((GXY_OLD_NAME == NULL) || (GXY_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new Xyvsy graphics method name.");
                 return NULL;
           }
        }

        ierr = renameGXy_name(GXY_OLD_NAME, GXY_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming Xyvsy graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing Xyvsy graphics method.
 */
static PyObject *
PyVCS_removeGXy(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the Xyvsy file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeGXy_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed Xyvsy object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The Xyvsy object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing Xyvsy graphics method.
 */
static PyObject *
PyVCS_scriptGXy(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GXY_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_xyvsy();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &GXY_NAME, &SCRIPT_NAME, &MODE)) {
           if (GXY_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the Xyvsy name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_xyvsy(fp, GXY_NAME) == 0) {
              sprintf(buf, "Error - Cannot save Xyvsy script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS Yxvsx (GYx) graphics method member value. 
 */
static PyObject *
PyVCS_getGYxmember(PyVCScanvas_Object *self, PyObject *args)
{
	char 			*GYx_name, *member=NULL, buf[1024];
	int 			i=0, ct=0;
	PyObject 		*GYX=NULL, *MEMBER=NULL, *tup, *lp;
	struct gYx_tab         	*gYxtab;
    	extern struct gYx_tab  	GYx_tab;
	struct gYx_attr         *pgYx;

	if(PyArg_ParseTuple(args,"|OO",&GYX, &MEMBER)) {
           if (GYX == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


	Get_Member(GYX,"name", "s", &GYx_name);
	gYxtab=&GYx_tab;
        while ((gYxtab != NULL) &&
               (strcmp(gYxtab->name, GYx_name) != 0))
           gYxtab = gYxtab->next;

     	if (gYxtab == NULL) {
	   sprintf(buf,"Cannot find Yxvsx graphics method GYx_%s.",GYx_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) {
           return Py_BuildValue("s", gYxtab->pGYx_attr->proj);
	} else if (cmpncs(member, "xticlabels1") == 0) {
           return Py_BuildValue("s", gYxtab->pGYx_attr->xtl1);
	} else if (cmpncs(member, "xticlabels2") == 0) {
           return Py_BuildValue("s", gYxtab->pGYx_attr->xtl2);
	} else if (cmpncs(member, "xmtics1") == 0) {
           return Py_BuildValue("s", gYxtab->pGYx_attr->xmt1);
	} else if (cmpncs(member, "xmtics2") == 0) {
           return Py_BuildValue("s", gYxtab->pGYx_attr->xmt2);
	} else if (cmpncs(member, "yticlabels1") == 0) {
           return Py_BuildValue("s", gYxtab->pGYx_attr->ytl1);
	} else if (cmpncs(member, "yticlabels2") == 0) {
           return Py_BuildValue("s", gYxtab->pGYx_attr->ytl2);
	} else if (cmpncs(member, "ymtics1") == 0) {
           return Py_BuildValue("s", gYxtab->pGYx_attr->ymt1);
	} else if (cmpncs(member, "ymtics2") == 0) {
           return Py_BuildValue("s", gYxtab->pGYx_attr->ymt2);
	} else if (cmpncs(member, "datawc_y1") == 0) {
           return Py_BuildValue("f",gYxtab->pGYx_attr->dsp[1]);
	} else if (cmpncs(member, "datawc_y2") == 0) {
           return Py_BuildValue("f",gYxtab->pGYx_attr->dsp[3]);
	} else if (cmpncs(member, "datawc_x1") == 0) {
           return Py_BuildValue("f",gYxtab->pGYx_attr->dsp[0]);
	} else if (cmpncs(member, "datawc_x2") == 0) {
           return Py_BuildValue("f",gYxtab->pGYx_attr->dsp[2]);
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
           return Py_BuildValue("i",gYxtab->pGYx_attr->idsp[1]);
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
           return Py_BuildValue("i",gYxtab->pGYx_attr->idsp[3]);
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
           return Py_BuildValue("i",gYxtab->pGYx_attr->idsp[0]);
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
           return Py_BuildValue("i",gYxtab->pGYx_attr->idsp[2]);
	} else if (cmpncs(member, "datawc_calendar") == 0) {
           return Py_BuildValue("i",gYxtab->pGYx_attr->calendar);
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
           return Py_BuildValue("s",gYxtab->pGYx_attr->timeunits);
        } else if ((cmpncs(member, "xaxisconvert") == 0) &&
                   ((cmpncs(gYxtab->pGYx_attr->xat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
        } else if (cmpncs(member, "xaxisconvert") == 0) {
           return Py_BuildValue("s", gYxtab->pGYx_attr->xat);
	} else if ((cmpncs(member, "yaxisconvert") == 0) &&
                   ((cmpncs(gYxtab->pGYx_attr->yat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
	} else if (cmpncs(member, "yaxisconvert") == 0) {
           return Py_BuildValue("s", gYxtab->pGYx_attr->yat);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing Yxvsx graphics method and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setGYxmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			i,j=0,n,ct=0,lct=1,line_index,lcolor_index=0;
	int 			MODE, value_int, marker_index, mcolor_index=0;	
	int			msize_index=0;
	long 			value_long;
	float 			value_float, lwidth_index=1.0;
	double 			value_double;
	char 			buf[1024], *style;
        char 			*GYx_name, *str=NULL, *member=NULL;
	char			*value_str=NULL, *Tl_name=NULL, *Tm_name=NULL;
        PyObject 		*GYX=NULL, *MEMBER=NULL, *VALUE=NULL;
	PyObject 		*itempk,*itempv,*pkeys,*pvalues, *line_obj;
        PyObject		*marker_obj, *mcolor_obj, *lcolor_obj, *lwidth_obj, *msize_obj;
	struct gYx_tab          *get_gYxtab=NULL;
	extern int              update_ind;
	struct gYx_attr         *pgYx;
        struct gYx_tab          *gYxtab;
        extern struct gYx_tab   GYx_tab;
	extern struct gYx_tab   *getGYx();
	extern int              chk_mov_GYx();
	extern int 		vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &GYX, &MEMBER, &VALUE, &MODE)) {
           if (GYX == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(GYX,"name", "s", &GYx_name);
        gYxtab=&GYx_tab;
        while ((gYxtab != NULL) &&
               (strcmp(gYxtab->name, GYx_name) != 0))
           gYxtab = gYxtab->next;

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	if (VALUE != NULL) {
	   if (PyString_Check(VALUE)) { /*check string*/
              value_str = PyString_AsString(VALUE);
	   } else if (PyInt_Check(VALUE)) { /*check for int*/
              value_int = (int) PyInt_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for float*/
              value_float = (float) PyFloat_AsDouble(VALUE);
	   } else if (PyLong_Check(VALUE)) { /* check for long*/
              value_long = PyLong_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for double*/
              value_double = PyFloat_AsDouble(VALUE);
	   } else if (PyDict_Check(VALUE)) { /*check for dictionary*/
              value_str = return_vcs_legacy_list(VALUE, member);
	   }
	}

	/*
	 * Set the appropriate Yxvsx attribute. But first 
	 * get the Yxvsx structure.
         */
	get_gYxtab = getGYx(gYxtab->name);
	if (cmpncs(member, "projection") == 0) {
	   strcpy(get_gYxtab->pGYx_attr->proj, value_str);
	} else if (cmpncs(member, "xticlabels1") == 0) {
	   strcpy(get_gYxtab->pGYx_attr->xtl1, value_str);
	} else if (cmpncs(member, "xticlabels2") == 0) {
	   strcpy(get_gYxtab->pGYx_attr->xtl2, value_str);
	} else if (cmpncs(member, "xmtics1") == 0) {
	   strcpy(get_gYxtab->pGYx_attr->xmt1, value_str);
	} else if (cmpncs(member, "xmtics2") == 0) {
	   strcpy(get_gYxtab->pGYx_attr->xmt2, value_str);
	} else if (cmpncs(member, "yticlabels1") == 0) {
	   strcpy(get_gYxtab->pGYx_attr->ytl1, value_str);
	} else if (cmpncs(member, "yticlabels2") == 0) {
	   strcpy(get_gYxtab->pGYx_attr->ytl2, value_str);
	} else if (cmpncs(member, "ymtics1") == 0) {
	   strcpy(get_gYxtab->pGYx_attr->ymt1, value_str);
	} else if (cmpncs(member, "ymtics2") == 0) {
	   strcpy(get_gYxtab->pGYx_attr->ymt2, value_str);
	} else if (cmpncs(member, "datawc_x1") == 0) {
	   get_gYxtab->pGYx_attr->dsp[0] = value_float;
	} else if (cmpncs(member, "datawc_y1") == 0) {
	   get_gYxtab->pGYx_attr->dsp[1] = value_float;
	} else if (cmpncs(member, "datawc_x2") == 0) {
	   get_gYxtab->pGYx_attr->dsp[2] = value_float;
	} else if (cmpncs(member, "datawc_y2") == 0) {
	   get_gYxtab->pGYx_attr->dsp[3] = value_float;
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   get_gYxtab->pGYx_attr->idsp[0] = value_int;
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   get_gYxtab->pGYx_attr->idsp[1] = value_int;
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   get_gYxtab->pGYx_attr->idsp[2] = value_int;
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   get_gYxtab->pGYx_attr->idsp[3] = value_int;
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   get_gYxtab->pGYx_attr->calendar = value_int;
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   strcpy(get_gYxtab->pGYx_attr->timeunits, value_str);
	} else if (cmpncs(member, "xaxisconvert") == 0) {
	   strcpy(get_gYxtab->pGYx_attr->xat, value_str);
        } else if (cmpncs(member, "yaxisconvert") == 0) {
           strcpy(get_gYxtab->pGYx_attr->yat, value_str);
	} else if ((cmpncs(member, "line") == 0) || (cmpncs(member, "linecolor") == 0) ||
                   (cmpncs(member, "linewidth") == 0) ||
		   (cmpncs(member, "marker") == 0) || (cmpncs(member, "markercolor") == 0) ||
		   (cmpncs(member, "markersize") == 0)) {
           /* get the line values */
           Get_Member(GYX,"line", "O", &line_obj);

           /* get the line color values */
           Get_Member(GYX,"linecolor", "O", &lcolor_obj);

           /* get the line width values */
           Get_Member(GYX,"linewidth", "O", &lwidth_obj);

           /* get the marker values */
           Get_Member(GYX,"marker", "O", &marker_obj);

           /* get the marker color values */
           Get_Member(GYX,"markercolor", "O", &mcolor_obj);

           /* get the marker size values */
           Get_Member(GYX,"markersize", "O", &msize_obj);

           /* Get the Yxvsx structure */
           pgYx = get_gYxtab->pGYx_attr;

           if (line_obj==Py_None)
              line_index = 1; /* default to solid line */
           else {
              if (cmpncs("solid", PyString_AsString(line_obj))==0)
                 line_index = 1;
              else if (cmpncs("dash", PyString_AsString(line_obj))==0)
                 line_index = 2;
              else if (cmpncs("dot", PyString_AsString(line_obj))==0)
                 line_index = 3;
              else if (cmpncs("dash-dot", PyString_AsString(line_obj))==0)
                 line_index = 4;
              else if (cmpncs("long-dash", PyString_AsString(line_obj))==0)
                 line_index = -3;
              else {
                 Tl_name = PyString_AsString(line_obj);
                 line_index = 999;
              }
           }
           if (line_index != 999) {
             if (lcolor_obj == Py_None)
                lcolor_index = 241; /* set color to default black color*/
             else
                lcolor_index = (int) PyInt_AsLong(lcolor_obj);

             if (lwidth_obj == Py_None)
                lwidth_index = 1.0; /* set width to default size 1.0*/
             else
                lwidth_index = (float) PyFloat_AsDouble(lwidth_obj);
   
	      if ((line_obj==Py_None) && (lcolor_obj == Py_None) && (lwidth_obj == Py_None))
                 strcpy(pgYx->lb,"default");
              else
                 strcpy(pgYx->lb,return_new_line_attribute(GYx_name,0,line_index,lcolor_index,lwidth_index));
	   } else /* must be a line object */
               strcpy(pgYx->lb, Tl_name);

           if (marker_obj==Py_None)
              marker_index = 0; /* default to no markers */
           else {
              if (cmpncs("dot", PyString_AsString(marker_obj))==0)
                 marker_index = 1;
              else if (cmpncs("plus", PyString_AsString(marker_obj))==0)
                 marker_index = 2;
              else if (cmpncs("star", PyString_AsString(marker_obj))==0)
                 marker_index = 3;
              else if (cmpncs("circle", PyString_AsString(marker_obj))==0)
                 marker_index = 4;
              else if (cmpncs("cross", PyString_AsString(marker_obj))==0)
                 marker_index = 5;
              else if (cmpncs("diamond", PyString_AsString(marker_obj))==0)
                 marker_index = 6;
              else if (cmpncs("triangle_up", PyString_AsString(marker_obj))==0)
                 marker_index = 7;
              else if (cmpncs("triangle_down", PyString_AsString(marker_obj))==0)
                 marker_index = 8;
              else if (cmpncs("triangle_left", PyString_AsString(marker_obj))==0)
                 marker_index = 9;
              else if (cmpncs("triangle_right", PyString_AsString(marker_obj))==0)
                 marker_index = 10;
              else if (cmpncs("square", PyString_AsString(marker_obj))==0)
                 marker_index = 11;
              else if (cmpncs("diamond_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 12;
              else if (cmpncs("triangle_up_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 13;
              else if (cmpncs("triangle_down_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 14;
              else if (cmpncs("triangle_left_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 15;
              else if (cmpncs("triangle_right_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 16;
              else if (cmpncs("square_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 17;
              else if (cmpncs("hurricane", PyString_AsString(marker_obj))==0)
                 marker_index = 18;
	      /* Arulalan Weather Markers */
	      else if (cmpncs("w00", PyString_AsString(marker_obj))==0)
                 marker_index = 100;
              else if (cmpncs("w01", PyString_AsString(marker_obj))==0)
                 marker_index = 101;
              else if (cmpncs("w02", PyString_AsString(marker_obj))==0)
                 marker_index = 102;
              else if (cmpncs("w03", PyString_AsString(marker_obj))==0)
                 marker_index = 103;
 	      else if (cmpncs("w04", PyString_AsString(marker_obj))==0)
                 marker_index = 104;
              else if (cmpncs("w05", PyString_AsString(marker_obj))==0)
                 marker_index = 105;
              else if (cmpncs("w06", PyString_AsString(marker_obj))==0)
                 marker_index = 106;
              else if (cmpncs("w07", PyString_AsString(marker_obj))==0)
                 marker_index = 107;
              else if (cmpncs("w08", PyString_AsString(marker_obj))==0)
                 marker_index = 108;
              else if (cmpncs("w09", PyString_AsString(marker_obj))==0)
                 marker_index = 109;
              else if (cmpncs("w10", PyString_AsString(marker_obj))==0)
                 marker_index = 110;
              else if (cmpncs("w11", PyString_AsString(marker_obj))==0)
                 marker_index = 111;
              else if (cmpncs("w12", PyString_AsString(marker_obj))==0)
                 marker_index = 112;
              else if (cmpncs("w13", PyString_AsString(marker_obj))==0)
                 marker_index = 113;
	      else if (cmpncs("w14", PyString_AsString(marker_obj))==0)
                 marker_index = 114;
              else if (cmpncs("w15", PyString_AsString(marker_obj))==0)
                 marker_index = 115;
              else if (cmpncs("w16", PyString_AsString(marker_obj))==0)
                 marker_index = 116;
              else if (cmpncs("w17", PyString_AsString(marker_obj))==0)
                 marker_index = 117;
              else if (cmpncs("w18", PyString_AsString(marker_obj))==0)
                 marker_index = 118;
              else if (cmpncs("w19", PyString_AsString(marker_obj))==0)
                 marker_index = 119;
	      else if (cmpncs("w20", PyString_AsString(marker_obj))==0)
                 marker_index = 120;
              else if (cmpncs("w21", PyString_AsString(marker_obj))==0)
                 marker_index = 121;
              else if (cmpncs("w22", PyString_AsString(marker_obj))==0)
                 marker_index = 122;
              else if (cmpncs("w23", PyString_AsString(marker_obj))==0)
                 marker_index = 123;
 	      else if (cmpncs("w24", PyString_AsString(marker_obj))==0)
                 marker_index = 124;
              else if (cmpncs("w25", PyString_AsString(marker_obj))==0)
                 marker_index = 125;
              else if (cmpncs("w26", PyString_AsString(marker_obj))==0)
                 marker_index = 126;
              else if (cmpncs("w27", PyString_AsString(marker_obj))==0)
                 marker_index = 127;
              else if (cmpncs("w28", PyString_AsString(marker_obj))==0)
                 marker_index = 128;
              else if (cmpncs("w29", PyString_AsString(marker_obj))==0)
                 marker_index = 129;
              else if (cmpncs("w30", PyString_AsString(marker_obj))==0)
                 marker_index = 130;
              else if (cmpncs("w31", PyString_AsString(marker_obj))==0)
                 marker_index = 131;
              else if (cmpncs("w32", PyString_AsString(marker_obj))==0)
                 marker_index = 132;
              else if (cmpncs("w33", PyString_AsString(marker_obj))==0)
                 marker_index = 133;
	      else if (cmpncs("w34", PyString_AsString(marker_obj))==0)
                 marker_index = 134;
              else if (cmpncs("w35", PyString_AsString(marker_obj))==0)
                 marker_index = 135;
              else if (cmpncs("w36", PyString_AsString(marker_obj))==0)
                 marker_index = 136;
              else if (cmpncs("w37", PyString_AsString(marker_obj))==0)
                 marker_index = 137;
              else if (cmpncs("w38", PyString_AsString(marker_obj))==0)
                 marker_index = 138;
              else if (cmpncs("w39", PyString_AsString(marker_obj))==0)
                 marker_index = 139;
	      else if (cmpncs("w40", PyString_AsString(marker_obj))==0)
                 marker_index = 140;
              else if (cmpncs("w41", PyString_AsString(marker_obj))==0)
                 marker_index = 141;
              else if (cmpncs("w42", PyString_AsString(marker_obj))==0)
                 marker_index = 142;
              else if (cmpncs("w43", PyString_AsString(marker_obj))==0)
                 marker_index = 143;
 	      else if (cmpncs("w44", PyString_AsString(marker_obj))==0)
                 marker_index = 144;
              else if (cmpncs("w45", PyString_AsString(marker_obj))==0)
                 marker_index = 145;
              else if (cmpncs("w46", PyString_AsString(marker_obj))==0)
                 marker_index = 146;
              else if (cmpncs("w47", PyString_AsString(marker_obj))==0)
                 marker_index = 147;
              else if (cmpncs("w48", PyString_AsString(marker_obj))==0)
                 marker_index = 148;
              else if (cmpncs("w49", PyString_AsString(marker_obj))==0)
                 marker_index = 149;
              else if (cmpncs("w50", PyString_AsString(marker_obj))==0)
                 marker_index = 150;
              else if (cmpncs("w51", PyString_AsString(marker_obj))==0)
                 marker_index = 151;
              else if (cmpncs("w52", PyString_AsString(marker_obj))==0)
                 marker_index = 152;
              else if (cmpncs("w53", PyString_AsString(marker_obj))==0)
                 marker_index = 153;
	      else if (cmpncs("w54", PyString_AsString(marker_obj))==0)
                 marker_index = 154;
              else if (cmpncs("w55", PyString_AsString(marker_obj))==0)
                 marker_index = 155;
              else if (cmpncs("w56", PyString_AsString(marker_obj))==0)
                 marker_index = 156;
              else if (cmpncs("w57", PyString_AsString(marker_obj))==0)
                 marker_index = 157;
              else if (cmpncs("w58", PyString_AsString(marker_obj))==0)
                 marker_index = 158;
              else if (cmpncs("w59", PyString_AsString(marker_obj))==0)
                 marker_index = 159;
	      else if (cmpncs("w60", PyString_AsString(marker_obj))==0)
                 marker_index = 160;
              else if (cmpncs("w61", PyString_AsString(marker_obj))==0)
                 marker_index = 161;
              else if (cmpncs("w62", PyString_AsString(marker_obj))==0)
                 marker_index = 162;
              else if (cmpncs("w63", PyString_AsString(marker_obj))==0)
                 marker_index = 163;
 	      else if (cmpncs("w64", PyString_AsString(marker_obj))==0)
                 marker_index = 164;
              else if (cmpncs("w65", PyString_AsString(marker_obj))==0)
                 marker_index = 165;
              else if (cmpncs("w66", PyString_AsString(marker_obj))==0)
                 marker_index = 166;
              else if (cmpncs("w67", PyString_AsString(marker_obj))==0)
                 marker_index = 167;
              else if (cmpncs("w68", PyString_AsString(marker_obj))==0)
                 marker_index = 168;
              else if (cmpncs("w69", PyString_AsString(marker_obj))==0)
                 marker_index = 169;
              else if (cmpncs("w70", PyString_AsString(marker_obj))==0)
                 marker_index = 170;
              else if (cmpncs("w71", PyString_AsString(marker_obj))==0)
                 marker_index = 171;
              else if (cmpncs("w72", PyString_AsString(marker_obj))==0)
                 marker_index = 172;
              else if (cmpncs("w73", PyString_AsString(marker_obj))==0)
                 marker_index = 173;
	      else if (cmpncs("w74", PyString_AsString(marker_obj))==0)
                 marker_index = 174;
              else if (cmpncs("w75", PyString_AsString(marker_obj))==0)
                 marker_index = 175;
              else if (cmpncs("w76", PyString_AsString(marker_obj))==0)
                 marker_index = 176;
              else if (cmpncs("w77", PyString_AsString(marker_obj))==0)
                 marker_index = 177;
              else if (cmpncs("w78", PyString_AsString(marker_obj))==0)
                 marker_index = 178;
              else if (cmpncs("w79", PyString_AsString(marker_obj))==0)
                 marker_index = 179;
	      else if (cmpncs("w80", PyString_AsString(marker_obj))==0)
                 marker_index = 180;
              else if (cmpncs("w81", PyString_AsString(marker_obj))==0)
                 marker_index = 181;
              else if (cmpncs("w82", PyString_AsString(marker_obj))==0)
                 marker_index = 182;
              else if (cmpncs("w83", PyString_AsString(marker_obj))==0)
                 marker_index = 183;
 	      else if (cmpncs("w84", PyString_AsString(marker_obj))==0)
                 marker_index = 184;
              else if (cmpncs("w85", PyString_AsString(marker_obj))==0)
                 marker_index = 185;
              else if (cmpncs("w86", PyString_AsString(marker_obj))==0)
                 marker_index = 186;
              else if (cmpncs("w87", PyString_AsString(marker_obj))==0)
                 marker_index = 187;
              else if (cmpncs("w88", PyString_AsString(marker_obj))==0)
                 marker_index = 188;
              else if (cmpncs("w89", PyString_AsString(marker_obj))==0)
                 marker_index = 189;
              else if (cmpncs("w90", PyString_AsString(marker_obj))==0)
                 marker_index = 190;
              else if (cmpncs("w91", PyString_AsString(marker_obj))==0)
                 marker_index = 191;
              else if (cmpncs("w92", PyString_AsString(marker_obj))==0)
                 marker_index = 192;
              else if (cmpncs("w93", PyString_AsString(marker_obj))==0)
                 marker_index = 193;
	      else if (cmpncs("w94", PyString_AsString(marker_obj))==0)
                 marker_index = 194;
              else if (cmpncs("w95", PyString_AsString(marker_obj))==0)
                 marker_index = 195;
              else if (cmpncs("w96", PyString_AsString(marker_obj))==0)
                 marker_index = 196;
              else if (cmpncs("w97", PyString_AsString(marker_obj))==0)
                 marker_index = 197;
              else if (cmpncs("w98", PyString_AsString(marker_obj))==0)
                 marker_index = 198;
              else if (cmpncs("w99", PyString_AsString(marker_obj))==0)
                 marker_index = 199;
	      else if (cmpncs("w200", PyString_AsString(marker_obj))==0)
                 marker_index = 200;
              else if (cmpncs("w201", PyString_AsString(marker_obj))==0)
                 marker_index = 201;
              else if (cmpncs("w202", PyString_AsString(marker_obj))==0)
                 marker_index = 202;
              else {
                 Tm_name = PyString_AsString(marker_obj);
                 marker_index = 999;
              }
          }
          if (marker_index != 999) {
             if (mcolor_obj == Py_None)
                mcolor_index = 241; /* set color to default black color*/
             else
                mcolor_index = (int) PyInt_AsLong(mcolor_obj);
             if (msize_obj == Py_None)
                msize_index = 7; /* set marker size to default size of 7*/
             else
                msize_index = (int) PyInt_AsLong(msize_obj);

              if ((marker_obj==Py_None) && (mcolor_obj == Py_None) && (msize_obj == Py_None))
                 strcpy(pgYx->mb,"\0");
              else
                 strcpy(pgYx->mb,return_new_marker_attribute(GYx_name, 0, marker_index, mcolor_index, msize_index));
          } else /* must be a marker object */
               strcpy(pgYx->mb, Tm_name);
	}

	chk_mov_GYx(get_gYxtab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new Yxvsx graphics method by copying from an existing
 * Yxvsx graphics method. If no source copy name argument is given,
 * then the default Yxvsx graphics method will be used to replicate
 * the new Yxvsx graphics method.
 */
static PyObject *
PyVCS_copyGYx(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GYX_SRC=NULL, *GYX_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_GYx_name();
              
        if(PyArg_ParseTuple(args,"|ss", &GYX_SRC, &GYX_NAME)) {
           if (GYX_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source Yxvsx graphics method name.");
                 return NULL;
           }

           if (GYX_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", GYX_NAME);
        }

        ierr = copy_GYx_name(GYX_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating Yxvsx graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing Yxvsx graphics method.
 */
static PyObject *
PyVCS_renameGYx(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GYX_OLD_NAME=NULL, *GYX_NEW_NAME=NULL;
        extern int      renameGYx_name();
 
        if(PyArg_ParseTuple(args,"|ss", &GYX_OLD_NAME, &GYX_NEW_NAME)) {
           if ((GYX_OLD_NAME == NULL) || (GYX_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new Yxvsx graphics method name.");
                 return NULL;
           }
        }

        ierr = renameGYx_name(GYX_OLD_NAME, GYX_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming Yxvsx graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing Yxvsx graphics method.
 */
static PyObject *
PyVCS_removeGYx(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the Yxvsx file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeGYx_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed Yxvsx object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The Yxvsx object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing Yxvsx graphics method.
 */
static PyObject *
PyVCS_scriptGYx(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GYX_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_yxvsx();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &GYX_NAME, &SCRIPT_NAME, &MODE)) {
           if (GYX_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the Yxvsx name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_yxvsx(fp, GYX_NAME) == 0) {
              sprintf(buf, "Error - Cannot save Yxvsx script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS  (GXY) graphics method member value. 
 */
static PyObject *
PyVCS_getGXYmember(PyVCScanvas_Object *self, PyObject *args)
{
	char 			*GXY_name, *member=NULL, buf[1024];
	int 			i=0, ct=0;
	PyObject 		*GXY=NULL, *MEMBER=NULL, *tup, *lp;
	struct gXY_tab         	*gXYtab;
    	extern struct gXY_tab  	GXY_tab;
	struct gXY_attr         *pgXY;

	if(PyArg_ParseTuple(args,"|OO",&GXY, &MEMBER)) {
           if (GXY == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


	Get_Member(GXY,"name", "s", &GXY_name);
	gXYtab=&GXY_tab;
        while ((gXYtab != NULL) &&
               (strcmp(gXYtab->name, GXY_name) != 0))
           gXYtab = gXYtab->next;

     	if (gXYtab == NULL) {
	   sprintf(buf,"Cannot find XvsY graphics method GXY_%s.",GXY_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) {
           return Py_BuildValue("s", gXYtab->pGXY_attr->proj);
	} else if (cmpncs(member, "xticlabels1") == 0) {
           return Py_BuildValue("s", gXYtab->pGXY_attr->xtl1);
	} else if (cmpncs(member, "xticlabels2") == 0) {
           return Py_BuildValue("s", gXYtab->pGXY_attr->xtl2);
	} else if (cmpncs(member, "xmtics1") == 0) {
           return Py_BuildValue("s", gXYtab->pGXY_attr->xmt1);
	} else if (cmpncs(member, "xmtics2") == 0) {
           return Py_BuildValue("s", gXYtab->pGXY_attr->xmt2);
	} else if (cmpncs(member, "yticlabels1") == 0) {
           return Py_BuildValue("s", gXYtab->pGXY_attr->ytl1);
	} else if (cmpncs(member, "yticlabels2") == 0) {
           return Py_BuildValue("s", gXYtab->pGXY_attr->ytl2);
	} else if (cmpncs(member, "ymtics1") == 0) {
           return Py_BuildValue("s", gXYtab->pGXY_attr->ymt1);
	} else if (cmpncs(member, "ymtics2") == 0) {
           return Py_BuildValue("s", gXYtab->pGXY_attr->ymt2);
	} else if (cmpncs(member, "datawc_y1") == 0) {
           return Py_BuildValue("f",gXYtab->pGXY_attr->dsp[1]);
	} else if (cmpncs(member, "datawc_y2") == 0) {
           return Py_BuildValue("f",gXYtab->pGXY_attr->dsp[3]);
	} else if (cmpncs(member, "datawc_x1") == 0) {
           return Py_BuildValue("f",gXYtab->pGXY_attr->dsp[0]);
	} else if (cmpncs(member, "datawc_x2") == 0) {
           return Py_BuildValue("f",gXYtab->pGXY_attr->dsp[2]);
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
           return Py_BuildValue("i",gXYtab->pGXY_attr->idsp[1]);
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
           return Py_BuildValue("i",gXYtab->pGXY_attr->idsp[3]);
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
           return Py_BuildValue("i",gXYtab->pGXY_attr->idsp[0]);
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
           return Py_BuildValue("i",gXYtab->pGXY_attr->idsp[2]);
	} else if (cmpncs(member, "datawc_calendar") == 0) {
           return Py_BuildValue("i",gXYtab->pGXY_attr->calendar);
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
           return Py_BuildValue("s",gXYtab->pGXY_attr->timeunits);
        } else if ((cmpncs(member, "xaxisconvert") == 0) &&
                   ((cmpncs(gXYtab->pGXY_attr->xat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
        } else if (cmpncs(member, "xaxisconvert") == 0) {
           return Py_BuildValue("s", gXYtab->pGXY_attr->xat);
        } else if ((cmpncs(member, "yaxisconvert") == 0) &&
                   ((cmpncs(gXYtab->pGXY_attr->yat, "\0") == 0))) {
           return Py_BuildValue("s", "linear");
        } else if (cmpncs(member, "yaxisconvert") == 0) {
           return Py_BuildValue("s", gXYtab->pGXY_attr->yat);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing XvsY graphics method and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setGXYmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			i,j=0,n,ct=0,lct=1,line_index,lcolor_index=0;
	int 			MODE, value_int, marker_index, mcolor_index=0;	
	int			msize_index=0;
	long 			value_long;
	float 			value_float, lwidth_index=1.0;
	double 			value_double;
	char 			buf[1024], *style;
        char 			*GXY_name, *str=NULL, *member=NULL;
	char			*value_str=NULL, *Tl_name=NULL, *Tm_name=NULL;
        PyObject 		*GXY=NULL, *MEMBER=NULL, *VALUE=NULL;
	PyObject 		*itempk,*itempv,*pkeys,*pvalues, *line_obj;
        PyObject		*marker_obj, *mcolor_obj, *lcolor_obj, *lwidth_obj, *msize_obj;
	struct gXY_tab          *get_gXYtab=NULL;
	extern int              update_ind;
	struct gXY_attr         *pgXY;
        struct gXY_tab          *gXYtab;
        extern struct gXY_tab   GXY_tab;
	extern struct gXY_tab   *getGXY();
	extern int              chk_mov_GXY();
	extern int 		vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &GXY, &MEMBER, &VALUE, &MODE)) {
           if (GXY == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(GXY,"name", "s", &GXY_name);
        gXYtab=&GXY_tab;
        while ((gXYtab != NULL) &&
               (strcmp(gXYtab->name, GXY_name) != 0))
           gXYtab = gXYtab->next;

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	if (VALUE != NULL) {
	   if (PyString_Check(VALUE)) { /*check string*/
              value_str = PyString_AsString(VALUE);
	   } else if (PyInt_Check(VALUE)) { /*check for int*/
              value_int = (int) PyInt_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for float*/
              value_float = (float) PyFloat_AsDouble(VALUE);
	   } else if (PyLong_Check(VALUE)) { /* check for long*/
              value_long = PyLong_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for double*/
              value_double = PyFloat_AsDouble(VALUE);
	   } else if (PyDict_Check(VALUE)) { /*check for dictionary*/
              value_str = return_vcs_legacy_list(VALUE, member);
	   }
	}

	/*
	 * Set the appropriate XvsY attribute. But first 
	 * get the XvsY structure.
         */
	get_gXYtab = getGXY(gXYtab->name);
	if (cmpncs(member, "projection") == 0) {
	   strcpy(get_gXYtab->pGXY_attr->proj, value_str);
	} else if (cmpncs(member, "xticlabels1") == 0) {
	   strcpy(get_gXYtab->pGXY_attr->xtl1, value_str);
	} else if (cmpncs(member, "xticlabels2") == 0) {
	   strcpy(get_gXYtab->pGXY_attr->xtl2, value_str);
	} else if (cmpncs(member, "xmtics1") == 0) {
	   strcpy(get_gXYtab->pGXY_attr->xmt1, value_str);
	} else if (cmpncs(member, "xmtics2") == 0) {
	   strcpy(get_gXYtab->pGXY_attr->xmt2, value_str);
	} else if (cmpncs(member, "yticlabels1") == 0) {
	   strcpy(get_gXYtab->pGXY_attr->ytl1, value_str);
	} else if (cmpncs(member, "yticlabels2") == 0) {
	   strcpy(get_gXYtab->pGXY_attr->ytl2, value_str);
	} else if (cmpncs(member, "ymtics1") == 0) {
	   strcpy(get_gXYtab->pGXY_attr->ymt1, value_str);
	} else if (cmpncs(member, "ymtics2") == 0) {
	   strcpy(get_gXYtab->pGXY_attr->ymt2, value_str);
	} else if (cmpncs(member, "datawc_x1") == 0) {
	   get_gXYtab->pGXY_attr->dsp[0] = value_float;
	} else if (cmpncs(member, "datawc_y1") == 0) {
	   get_gXYtab->pGXY_attr->dsp[1] = value_float;
	} else if (cmpncs(member, "datawc_x2") == 0) {
	   get_gXYtab->pGXY_attr->dsp[2] = value_float;
	} else if (cmpncs(member, "datawc_y2") == 0) {
	   get_gXYtab->pGXY_attr->dsp[3] = value_float;
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   get_gXYtab->pGXY_attr->idsp[0] = value_int;
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   get_gXYtab->pGXY_attr->idsp[1] = value_int;
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   get_gXYtab->pGXY_attr->idsp[2] = value_int;
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   get_gXYtab->pGXY_attr->idsp[3] = value_int;
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   get_gXYtab->pGXY_attr->calendar = value_int;
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   strcpy(get_gXYtab->pGXY_attr->timeunits, value_str);
	} else if (cmpncs(member, "xaxisconvert") == 0) {
	   strcpy(get_gXYtab->pGXY_attr->xat, value_str);
	} else if (cmpncs(member, "yaxisconvert") == 0) {
	   strcpy(get_gXYtab->pGXY_attr->yat, value_str);
	} else if ((cmpncs(member, "line") == 0) || (cmpncs(member, "linecolor") == 0) ||
                   (cmpncs(member, "linewidth") == 0) ||
		   (cmpncs(member, "marker") == 0) || (cmpncs(member, "markercolor") == 0) ||
		   (cmpncs(member, "markersize") == 0)) {
           /* get the line values */
           Get_Member(GXY,"line", "O", &line_obj);

           /* get the line color values */
           Get_Member(GXY,"linecolor", "O", &lcolor_obj);

           /* get the line width values */
           Get_Member(GXY,"linewidth", "O", &lwidth_obj);

           /* get the marker values */
           Get_Member(GXY,"marker", "O", &marker_obj);

           /* get the marker color values */
           Get_Member(GXY,"markercolor", "O", &mcolor_obj);

           /* get the marker size values */
           Get_Member(GXY,"markersize", "O", &msize_obj);

           /* Get the XvsY structure */
           pgXY = get_gXYtab->pGXY_attr;

           if (line_obj==Py_None)
              line_index = 1; /* default to solid line */
           else {
              if (cmpncs("solid", PyString_AsString(line_obj))==0)
                 line_index = 1;
              else if (cmpncs("dash", PyString_AsString(line_obj))==0)
                 line_index = 2;
              else if (cmpncs("dot", PyString_AsString(line_obj))==0)
                 line_index = 3;
              else if (cmpncs("dash-dot", PyString_AsString(line_obj))==0)
                 line_index = 4;
              else if (cmpncs("long-dash", PyString_AsString(line_obj))==0)
                 line_index = -3;
              else {
                 Tl_name = PyString_AsString(line_obj);
                 line_index = 999;
              }
           }
           if (line_index != 999) {
              if (lcolor_obj == Py_None)
                 lcolor_index = 241; /* set color to default black color*/
              else
                 lcolor_index = (int) PyInt_AsLong(lcolor_obj);

              if (lwidth_obj == Py_None)
                 lwidth_index = 1.0; /* set width to default size 1.0*/
              else
                 lwidth_index = (float) PyFloat_AsDouble(lwidth_obj);
   
	      if ((line_obj==Py_None) && (lcolor_obj == Py_None) && (lwidth_obj == Py_None))
                 strcpy(pgXY->lb,"default");
              else
                 strcpy(pgXY->lb,return_new_line_attribute(GXY_name,0,line_index,lcolor_index,lwidth_index));
           } else /* must be a line object */
               strcpy(pgXY->lb, Tl_name);

           if (marker_obj==Py_None)
              marker_index = 0; /* default to no markers */
           else {
              if (cmpncs("dot", PyString_AsString(marker_obj))==0)
                 marker_index = 1;
              else if (cmpncs("plus", PyString_AsString(marker_obj))==0)
                 marker_index = 2;
              else if (cmpncs("star", PyString_AsString(marker_obj))==0)
                 marker_index = 3;
              else if (cmpncs("circle", PyString_AsString(marker_obj))==0)
                 marker_index = 4;
              else if (cmpncs("cross", PyString_AsString(marker_obj))==0)
                 marker_index = 5;
              else if (cmpncs("diamond", PyString_AsString(marker_obj))==0)
                 marker_index = 6;
              else if (cmpncs("triangle_up", PyString_AsString(marker_obj))==0)
                 marker_index = 7;
              else if (cmpncs("triangle_down", PyString_AsString(marker_obj))==0)
                 marker_index = 8;
              else if (cmpncs("triangle_left", PyString_AsString(marker_obj))==0)
                 marker_index = 9;
              else if (cmpncs("triangle_right", PyString_AsString(marker_obj))==0)
                 marker_index = 10;
              else if (cmpncs("square", PyString_AsString(marker_obj))==0)
                 marker_index = 11;
              else if (cmpncs("diamond_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 12;
              else if (cmpncs("triangle_up_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 13;
              else if (cmpncs("triangle_down_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 14;
              else if (cmpncs("triangle_left_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 15;
              else if (cmpncs("triangle_right_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 16;
              else if (cmpncs("square_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 17;
              else if (cmpncs("hurricane", PyString_AsString(marker_obj))==0)
                 marker_index = 18;
	      /* Arulalan Weather Markers */
	      else if (cmpncs("w00", PyString_AsString(marker_obj))==0)
                 marker_index = 100;
              else if (cmpncs("w01", PyString_AsString(marker_obj))==0)
                 marker_index = 101;
              else if (cmpncs("w02", PyString_AsString(marker_obj))==0)
                 marker_index = 102;
              else if (cmpncs("w03", PyString_AsString(marker_obj))==0)
                 marker_index = 103;
 	      else if (cmpncs("w04", PyString_AsString(marker_obj))==0)
                 marker_index = 104;
              else if (cmpncs("w05", PyString_AsString(marker_obj))==0)
                 marker_index = 105;
              else if (cmpncs("w06", PyString_AsString(marker_obj))==0)
                 marker_index = 106;
              else if (cmpncs("w07", PyString_AsString(marker_obj))==0)
                 marker_index = 107;
              else if (cmpncs("w08", PyString_AsString(marker_obj))==0)
                 marker_index = 108;
              else if (cmpncs("w09", PyString_AsString(marker_obj))==0)
                 marker_index = 109;
              else if (cmpncs("w10", PyString_AsString(marker_obj))==0)
                 marker_index = 110;
              else if (cmpncs("w11", PyString_AsString(marker_obj))==0)
                 marker_index = 111;
              else if (cmpncs("w12", PyString_AsString(marker_obj))==0)
                 marker_index = 112;
              else if (cmpncs("w13", PyString_AsString(marker_obj))==0)
                 marker_index = 113;
	      else if (cmpncs("w14", PyString_AsString(marker_obj))==0)
                 marker_index = 114;
              else if (cmpncs("w15", PyString_AsString(marker_obj))==0)
                 marker_index = 115;
              else if (cmpncs("w16", PyString_AsString(marker_obj))==0)
                 marker_index = 116;
              else if (cmpncs("w17", PyString_AsString(marker_obj))==0)
                 marker_index = 117;
              else if (cmpncs("w18", PyString_AsString(marker_obj))==0)
                 marker_index = 118;
              else if (cmpncs("w19", PyString_AsString(marker_obj))==0)
                 marker_index = 119;
	      else if (cmpncs("w20", PyString_AsString(marker_obj))==0)
                 marker_index = 120;
              else if (cmpncs("w21", PyString_AsString(marker_obj))==0)
                 marker_index = 121;
              else if (cmpncs("w22", PyString_AsString(marker_obj))==0)
                 marker_index = 122;
              else if (cmpncs("w23", PyString_AsString(marker_obj))==0)
                 marker_index = 123;
 	      else if (cmpncs("w24", PyString_AsString(marker_obj))==0)
                 marker_index = 124;
              else if (cmpncs("w25", PyString_AsString(marker_obj))==0)
                 marker_index = 125;
              else if (cmpncs("w26", PyString_AsString(marker_obj))==0)
                 marker_index = 126;
              else if (cmpncs("w27", PyString_AsString(marker_obj))==0)
                 marker_index = 127;
              else if (cmpncs("w28", PyString_AsString(marker_obj))==0)
                 marker_index = 128;
              else if (cmpncs("w29", PyString_AsString(marker_obj))==0)
                 marker_index = 129;
              else if (cmpncs("w30", PyString_AsString(marker_obj))==0)
                 marker_index = 130;
              else if (cmpncs("w31", PyString_AsString(marker_obj))==0)
                 marker_index = 131;
              else if (cmpncs("w32", PyString_AsString(marker_obj))==0)
                 marker_index = 132;
              else if (cmpncs("w33", PyString_AsString(marker_obj))==0)
                 marker_index = 133;
	      else if (cmpncs("w34", PyString_AsString(marker_obj))==0)
                 marker_index = 134;
              else if (cmpncs("w35", PyString_AsString(marker_obj))==0)
                 marker_index = 135;
              else if (cmpncs("w36", PyString_AsString(marker_obj))==0)
                 marker_index = 136;
              else if (cmpncs("w37", PyString_AsString(marker_obj))==0)
                 marker_index = 137;
              else if (cmpncs("w38", PyString_AsString(marker_obj))==0)
                 marker_index = 138;
              else if (cmpncs("w39", PyString_AsString(marker_obj))==0)
                 marker_index = 139;
	      else if (cmpncs("w40", PyString_AsString(marker_obj))==0)
                 marker_index = 140;
              else if (cmpncs("w41", PyString_AsString(marker_obj))==0)
                 marker_index = 141;
              else if (cmpncs("w42", PyString_AsString(marker_obj))==0)
                 marker_index = 142;
              else if (cmpncs("w43", PyString_AsString(marker_obj))==0)
                 marker_index = 143;
 	      else if (cmpncs("w44", PyString_AsString(marker_obj))==0)
                 marker_index = 144;
              else if (cmpncs("w45", PyString_AsString(marker_obj))==0)
                 marker_index = 145;
              else if (cmpncs("w46", PyString_AsString(marker_obj))==0)
                 marker_index = 146;
              else if (cmpncs("w47", PyString_AsString(marker_obj))==0)
                 marker_index = 147;
              else if (cmpncs("w48", PyString_AsString(marker_obj))==0)
                 marker_index = 148;
              else if (cmpncs("w49", PyString_AsString(marker_obj))==0)
                 marker_index = 149;
              else if (cmpncs("w50", PyString_AsString(marker_obj))==0)
                 marker_index = 150;
              else if (cmpncs("w51", PyString_AsString(marker_obj))==0)
                 marker_index = 151;
              else if (cmpncs("w52", PyString_AsString(marker_obj))==0)
                 marker_index = 152;
              else if (cmpncs("w53", PyString_AsString(marker_obj))==0)
                 marker_index = 153;
	      else if (cmpncs("w54", PyString_AsString(marker_obj))==0)
                 marker_index = 154;
              else if (cmpncs("w55", PyString_AsString(marker_obj))==0)
                 marker_index = 155;
              else if (cmpncs("w56", PyString_AsString(marker_obj))==0)
                 marker_index = 156;
              else if (cmpncs("w57", PyString_AsString(marker_obj))==0)
                 marker_index = 157;
              else if (cmpncs("w58", PyString_AsString(marker_obj))==0)
                 marker_index = 158;
              else if (cmpncs("w59", PyString_AsString(marker_obj))==0)
                 marker_index = 159;
	      else if (cmpncs("w60", PyString_AsString(marker_obj))==0)
                 marker_index = 160;
              else if (cmpncs("w61", PyString_AsString(marker_obj))==0)
                 marker_index = 161;
              else if (cmpncs("w62", PyString_AsString(marker_obj))==0)
                 marker_index = 162;
              else if (cmpncs("w63", PyString_AsString(marker_obj))==0)
                 marker_index = 163;
 	      else if (cmpncs("w64", PyString_AsString(marker_obj))==0)
                 marker_index = 164;
              else if (cmpncs("w65", PyString_AsString(marker_obj))==0)
                 marker_index = 165;
              else if (cmpncs("w66", PyString_AsString(marker_obj))==0)
                 marker_index = 166;
              else if (cmpncs("w67", PyString_AsString(marker_obj))==0)
                 marker_index = 167;
              else if (cmpncs("w68", PyString_AsString(marker_obj))==0)
                 marker_index = 168;
              else if (cmpncs("w69", PyString_AsString(marker_obj))==0)
                 marker_index = 169;
              else if (cmpncs("w70", PyString_AsString(marker_obj))==0)
                 marker_index = 170;
              else if (cmpncs("w71", PyString_AsString(marker_obj))==0)
                 marker_index = 171;
              else if (cmpncs("w72", PyString_AsString(marker_obj))==0)
                 marker_index = 172;
              else if (cmpncs("w73", PyString_AsString(marker_obj))==0)
                 marker_index = 173;
	      else if (cmpncs("w74", PyString_AsString(marker_obj))==0)
                 marker_index = 174;
              else if (cmpncs("w75", PyString_AsString(marker_obj))==0)
                 marker_index = 175;
              else if (cmpncs("w76", PyString_AsString(marker_obj))==0)
                 marker_index = 176;
              else if (cmpncs("w77", PyString_AsString(marker_obj))==0)
                 marker_index = 177;
              else if (cmpncs("w78", PyString_AsString(marker_obj))==0)
                 marker_index = 178;
              else if (cmpncs("w79", PyString_AsString(marker_obj))==0)
                 marker_index = 179;
	      else if (cmpncs("w80", PyString_AsString(marker_obj))==0)
                 marker_index = 180;
              else if (cmpncs("w81", PyString_AsString(marker_obj))==0)
                 marker_index = 181;
              else if (cmpncs("w82", PyString_AsString(marker_obj))==0)
                 marker_index = 182;
              else if (cmpncs("w83", PyString_AsString(marker_obj))==0)
                 marker_index = 183;
 	      else if (cmpncs("w84", PyString_AsString(marker_obj))==0)
                 marker_index = 184;
              else if (cmpncs("w85", PyString_AsString(marker_obj))==0)
                 marker_index = 185;
              else if (cmpncs("w86", PyString_AsString(marker_obj))==0)
                 marker_index = 186;
              else if (cmpncs("w87", PyString_AsString(marker_obj))==0)
                 marker_index = 187;
              else if (cmpncs("w88", PyString_AsString(marker_obj))==0)
                 marker_index = 188;
              else if (cmpncs("w89", PyString_AsString(marker_obj))==0)
                 marker_index = 189;
              else if (cmpncs("w90", PyString_AsString(marker_obj))==0)
                 marker_index = 190;
              else if (cmpncs("w91", PyString_AsString(marker_obj))==0)
                 marker_index = 191;
              else if (cmpncs("w92", PyString_AsString(marker_obj))==0)
                 marker_index = 192;
              else if (cmpncs("w93", PyString_AsString(marker_obj))==0)
                 marker_index = 193;
	      else if (cmpncs("w94", PyString_AsString(marker_obj))==0)
                 marker_index = 194;
              else if (cmpncs("w95", PyString_AsString(marker_obj))==0)
                 marker_index = 195;
              else if (cmpncs("w96", PyString_AsString(marker_obj))==0)
                 marker_index = 196;
              else if (cmpncs("w97", PyString_AsString(marker_obj))==0)
                 marker_index = 197;
              else if (cmpncs("w98", PyString_AsString(marker_obj))==0)
                 marker_index = 198;
              else if (cmpncs("w99", PyString_AsString(marker_obj))==0)
                 marker_index = 199;
	      else if (cmpncs("w200", PyString_AsString(marker_obj))==0)
                 marker_index = 200;
              else if (cmpncs("w201", PyString_AsString(marker_obj))==0)
                 marker_index = 201;
              else if (cmpncs("w202", PyString_AsString(marker_obj))==0)
                 marker_index = 202;
              else {
                 Tm_name = PyString_AsString(marker_obj);
                 marker_index = 999;
              }
          }
          if (marker_index != 999) {
             if (mcolor_obj == Py_None)
                mcolor_index = 241; /* set color to default black color*/
             else
                mcolor_index = (int) PyInt_AsLong(mcolor_obj);
             if (msize_obj == Py_None)
                msize_index = 7; /* set marker size to default size of 7*/
             else
                msize_index = (int) PyInt_AsLong(msize_obj);
   
              if ((marker_obj==Py_None) && (mcolor_obj == Py_None) && (msize_obj == Py_None))
                 strcpy(pgXY->mb,"\0");
              else
                 strcpy(pgXY->mb,return_new_marker_attribute(GXY_name, 0, marker_index, mcolor_index, msize_index));
          } else /* must be a marker object */
               strcpy(pgXY->mb, Tm_name);
	}

	chk_mov_GXY(get_gXYtab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new XvsY graphics method by copying from an existing
 * XvsY graphics method. If no source copy name argument is given,
 * then the default XvsY graphics method will be used to replicate
 * the new XvsY graphics method.
 */
static PyObject *
PyVCS_copyGXY(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GXY_SRC=NULL, *GXY_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_GXY_name();
              
        if(PyArg_ParseTuple(args,"|ss", &GXY_SRC, &GXY_NAME)) {
           if (GXY_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source XvsY graphics method name.");
                 return NULL;
           }

           if (GXY_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", GXY_NAME);
        }

        ierr = copy_GXY_name(GXY_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating XvsY graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing XvsY graphics method.
 */
static PyObject *
PyVCS_renameGXY(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GXY_OLD_NAME=NULL, *GXY_NEW_NAME=NULL;
        extern int      renameGXY_name();
 
        if(PyArg_ParseTuple(args,"|ss", &GXY_OLD_NAME, &GXY_NEW_NAME)) {
           if ((GXY_OLD_NAME == NULL) || (GXY_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new XvsY graphics method name.");
                 return NULL;
           }
        }

        ierr = renameGXY_name(GXY_OLD_NAME, GXY_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming XvsY graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing XvsY graphics method.
 */
static PyObject *
PyVCS_removeGXY(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the XvsY file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeGXY_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed XvsY object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The XvsY object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing XvsY graphics method.
 */
static PyObject *
PyVCS_scriptGXY(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GXY_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_xvsy();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &GXY_NAME, &SCRIPT_NAME, &MODE)) {
           if (GXY_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the XvsY name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_xvsy(fp, GXY_NAME) == 0) {
              sprintf(buf, "Error - Cannot save XvsY script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS  (Gv) graphics method member value. 
 */
static PyObject *
PyVCS_getGvmember(PyVCScanvas_Object *self, PyObject *args)
{
	char 			*Gv_name, *member=NULL, buf[1024];
	int 			i=0, ct=0;
	PyObject 		*GV=NULL, *MEMBER=NULL, *tup, *lp;
	struct gv_tab         	*gvtab;
    	extern struct gv_tab  	Gv_tab;
	struct gv_attr         *pgv;

	if(PyArg_ParseTuple(args,"|OO",&GV, &MEMBER)) {
           if (GV == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


	Get_Member(GV,"name", "s", &Gv_name);
	gvtab=&Gv_tab;
        while ((gvtab != NULL) &&
               (strcmp(gvtab->name, Gv_name) != 0))
	    gvtab = gvtab->next;

     	if (gvtab == NULL) {
	   sprintf(buf,"Cannot find vector graphics method Gv_%s.",Gv_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) {
           return Py_BuildValue("s", gvtab->pGv_attr->proj);
	} else if (cmpncs(member, "xticlabels1") == 0) {
           return Py_BuildValue("s", gvtab->pGv_attr->xtl1);
	} else if (cmpncs(member, "xticlabels2") == 0) {
           return Py_BuildValue("s", gvtab->pGv_attr->xtl2);
	} else if (cmpncs(member, "xmtics1") == 0) {
           return Py_BuildValue("s", gvtab->pGv_attr->xmt1);
	} else if (cmpncs(member, "xmtics2") == 0) {
           return Py_BuildValue("s", gvtab->pGv_attr->xmt2);
	} else if (cmpncs(member, "yticlabels1") == 0) {
           return Py_BuildValue("s", gvtab->pGv_attr->ytl1);
	} else if (cmpncs(member, "yticlabels2") == 0) {
           return Py_BuildValue("s", gvtab->pGv_attr->ytl2);
	} else if (cmpncs(member, "ymtics1") == 0) {
           return Py_BuildValue("s", gvtab->pGv_attr->ymt1);
	} else if (cmpncs(member, "ymtics2") == 0) {
           return Py_BuildValue("s", gvtab->pGv_attr->ymt2);
	} else if (cmpncs(member, "datawc_y1") == 0) {
           return Py_BuildValue("f",gvtab->pGv_attr->dsp[1]);
	} else if (cmpncs(member, "datawc_y2") == 0) {
           return Py_BuildValue("f",gvtab->pGv_attr->dsp[3]);
	} else if (cmpncs(member, "datawc_x1") == 0) {
           return Py_BuildValue("f",gvtab->pGv_attr->dsp[0]);
	} else if (cmpncs(member, "datawc_x2") == 0) {
           return Py_BuildValue("f",gvtab->pGv_attr->dsp[2]);
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
           return Py_BuildValue("i",gvtab->pGv_attr->idsp[1]);
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
           return Py_BuildValue("i",gvtab->pGv_attr->idsp[3]);
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
           return Py_BuildValue("i",gvtab->pGv_attr->idsp[0]);
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
           return Py_BuildValue("i",gvtab->pGv_attr->idsp[2]);
	} else if (cmpncs(member, "datawc_calendar") == 0) {
           return Py_BuildValue("i",gvtab->pGv_attr->calendar);
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
           return Py_BuildValue("s",gvtab->pGv_attr->timeunits);
        } else if ((cmpncs(member, "xaxisconvert") == 0) &&
                   ((cmpncs(gvtab->pGv_attr->xat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
        } else if (cmpncs(member, "xaxisconvert") == 0) {
           return Py_BuildValue("s", gvtab->pGv_attr->xat);
        } else if ((cmpncs(member, "yaxisconvert") == 0) &&
                   ((cmpncs(gvtab->pGv_attr->yat, "\0") == 0))) {
           return Py_BuildValue("s", "linear");
        } else if (cmpncs(member, "yaxisconvert") == 0) {
           return Py_BuildValue("s", gvtab->pGv_attr->yat);
        } else if (cmpncs(member, "scale") == 0) {
           return Py_BuildValue("f", gvtab->pGv_attr->vsf);
        } else if (cmpncs(member, "alignment") == 0) {
           if (gvtab->pGv_attr->vpos == 99)
              return Py_BuildValue("s", "center");
           else if (gvtab->pGv_attr->vpos == 104)
              return Py_BuildValue("s", "head");
           else if (gvtab->pGv_attr->vpos == 116)
              return Py_BuildValue("s", "tail");
        } else if (cmpncs(member, "type") == 0) {
           if (gvtab->pGv_attr->vtype == 1)
              return Py_BuildValue("s", "barbs");
           else if (gvtab->pGv_attr->vtype == 2)
              return Py_BuildValue("s", "arrows");
           else if (gvtab->pGv_attr->vtype == 3)
              return Py_BuildValue("s", "solidarrows");
        } else if (cmpncs(member, "reference") == 0) {
           return Py_BuildValue("f", gvtab->pGv_attr->vlen);
	}




        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing vector graphics method and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setGvmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			i,j=0,n,ct=0,lct=1,line_index,color_index=0;
	int 			MODE, value_int, marker_index, mcolor_index=0;	
	int			msize_index=0;
	long 			value_long;
	float 			value_float, width_index=1.0;
	double 			value_double;
	char 			buf[1024], *style;
        char 			*Gv_name, *str=NULL, *member=NULL;
	char			*value_str=NULL, *Tl_name=NULL;
        PyObject 		*GV=NULL, *MEMBER=NULL, *VALUE=NULL;
	PyObject 		*itempk,*itempv,*pkeys,*pvalues, *line_obj;
        PyObject		*marker_obj, *mcolor_obj, *color_obj, *width_obj,*msize_obj;
	struct gv_tab          *get_gvtab=NULL;
	extern int              update_ind;
	struct gv_attr         *pgv;
        struct gv_tab          *gvtab;
        extern struct gv_tab   Gv_tab;
	extern struct gv_tab   *getGv();
	extern int              chk_mov_Gv();
	extern int 		vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &GV, &MEMBER, &VALUE, &MODE)) {
           if (GV == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(GV,"name", "s", &Gv_name);
        gvtab=&Gv_tab;
        while ((gvtab != NULL) &&
               (strcmp(gvtab->name, Gv_name) != 0))
           gvtab = gvtab->next;

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	if (VALUE != NULL) {
	   if (PyString_Check(VALUE)) { /*check string*/
              value_str = PyString_AsString(VALUE);
	   } else if (PyInt_Check(VALUE)) { /*check for int*/
              value_int = (int) PyInt_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for float*/
              value_float = (float) PyFloat_AsDouble(VALUE);
	   } else if (PyLong_Check(VALUE)) { /* check for long*/
              value_long = PyLong_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for double*/
              value_double = PyFloat_AsDouble(VALUE);
	   } else if (PyDict_Check(VALUE)) { /*check for dictionary*/
              value_str = return_vcs_legacy_list(VALUE, member);
	   }
	}

	/*
	 * Set the appropriate vector attribute. But first 
	 * get the vector structure.
         */
	get_gvtab = getGv(gvtab->name);
	if (cmpncs(member, "projection") == 0) {
	   strcpy(get_gvtab->pGv_attr->proj, value_str);
	} else if (cmpncs(member, "xticlabels1") == 0) {
	   strcpy(get_gvtab->pGv_attr->xtl1, value_str);
	} else if (cmpncs(member, "xticlabels2") == 0) {
	   strcpy(get_gvtab->pGv_attr->xtl2, value_str);
	} else if (cmpncs(member, "xmtics1") == 0) {
	   strcpy(get_gvtab->pGv_attr->xmt1, value_str);
	} else if (cmpncs(member, "xmtics2") == 0) {
	   strcpy(get_gvtab->pGv_attr->xmt2, value_str);
	} else if (cmpncs(member, "yticlabels1") == 0) {
	   strcpy(get_gvtab->pGv_attr->ytl1, value_str);
	} else if (cmpncs(member, "yticlabels2") == 0) {
	   strcpy(get_gvtab->pGv_attr->ytl2, value_str);
	} else if (cmpncs(member, "ymtics1") == 0) {
	   strcpy(get_gvtab->pGv_attr->ymt1, value_str);
	} else if (cmpncs(member, "ymtics2") == 0) {
	   strcpy(get_gvtab->pGv_attr->ymt2, value_str);
	} else if (cmpncs(member, "datawc_x1") == 0) {
	   get_gvtab->pGv_attr->dsp[0] = value_float;
	} else if (cmpncs(member, "datawc_y1") == 0) {
	   get_gvtab->pGv_attr->dsp[1] = value_float;
	} else if (cmpncs(member, "datawc_x2") == 0) {
	   get_gvtab->pGv_attr->dsp[2] = value_float;
	} else if (cmpncs(member, "datawc_y2") == 0) {
	   get_gvtab->pGv_attr->dsp[3] = value_float;
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   get_gvtab->pGv_attr->idsp[0] = value_int;
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   get_gvtab->pGv_attr->idsp[1] = value_int;
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   get_gvtab->pGv_attr->idsp[2] = value_int;
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   get_gvtab->pGv_attr->idsp[3] = value_int;
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   get_gvtab->pGv_attr->calendar = value_int;
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   strcpy(get_gvtab->pGv_attr->timeunits, value_str);
	} else if (cmpncs(member, "xaxisconvert") == 0) {
	   strcpy(get_gvtab->pGv_attr->xat, value_str);
	} else if (cmpncs(member, "yaxisconvert") == 0) {
	   strcpy(get_gvtab->pGv_attr->yat, value_str);
	} else if ((cmpncs(member, "line") == 0) || (cmpncs(member, "linecolor") == 0) ||
                    (cmpncs(member, "linewidth") == 0)) {
           /* get the line values */
           Get_Member(GV,"line", "O", &line_obj);

           /* get the line color values */
           Get_Member(GV,"linecolor", "O", &color_obj);

           /* get the line width values */
           Get_Member(GV,"linewidth", "O", &width_obj);

           /* Get the vector structure */
           pgv = get_gvtab->pGv_attr;

           if (line_obj==Py_None)
              line_index = 1; /* default to solid line */
           else {
              if (cmpncs("solid", PyString_AsString(line_obj))==0)
                 line_index = 1;
              else if (cmpncs("dash", PyString_AsString(line_obj))==0)
                 line_index = 2;
              else if (cmpncs("dot", PyString_AsString(line_obj))==0)
                 line_index = 3;
              else if (cmpncs("dash-dot", PyString_AsString(line_obj))==0)
                 line_index = 4;
              else if (cmpncs("long-dash", PyString_AsString(line_obj))==0)
                 line_index = -3;
              else {
                 Tl_name = PyString_AsString(line_obj);
                 line_index = 999;
              }
           }
           if (line_index != 999) {
              if (color_obj == Py_None)
                 color_index = 241; /* set color to default black color*/
              else
                 color_index = (int) PyInt_AsLong(color_obj);

              if (width_obj == Py_None)
                 width_index = 1.0; /* set width to default size 1.0*/
              else
                 width_index = (float) PyFloat_AsDouble(width_obj);

	      if ((line_obj==Py_None) && (color_obj == Py_None) && (width_obj == Py_None))
                 strcpy(pgv->lb,"default");
              else
                 strcpy(pgv->lb,return_new_line_attribute(Gv_name,0,line_index,color_index,width_index));
           } else /* must be a line object */
               strcpy(pgv->lb, Tl_name);

	} else if (cmpncs(member, "scale") == 0) {
	   get_gvtab->pGv_attr->vsf = value_float;
	} else if (cmpncs(member, "alignment") == 0) {
	   if (cmpncs(value_str, "head") == 0)
	      get_gvtab->pGv_attr->vpos = 104;
	   else if (cmpncs(value_str, "center") == 0)
	      get_gvtab->pGv_attr->vpos = 99;
	   else if (cmpncs(value_str, "tail") == 0)
	      get_gvtab->pGv_attr->vpos = 116;
	} else if (cmpncs(member, "type") == 0) {
           if (cmpncs(value_str, "barbs") == 0)
	      get_gvtab->pGv_attr->vtype = 1;
           else if (cmpncs(value_str, "arrows") == 0)
	      get_gvtab->pGv_attr->vtype = 2;
           else if (cmpncs(value_str, "solidarrows") == 0)
	      get_gvtab->pGv_attr->vtype = 3;
	} else if (cmpncs(member, "reference") == 0) {
	   get_gvtab->pGv_attr->vlen = value_float;
	}

	chk_mov_Gv(get_gvtab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new vector graphics method by copying from an existing
 * vector graphics method. If no source copy name argument is given,
 * then the default vector graphics method will be used to replicate
 * the new vector graphics method.
 */
static PyObject *
PyVCS_copyGv(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GV_SRC=NULL, *GV_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Gv_name();
              
        if(PyArg_ParseTuple(args,"|ss", &GV_SRC, &GV_NAME)) {
           if (GV_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source vector graphics method name.");
                 return NULL;
           }

           if (GV_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", GV_NAME);
        }

        ierr = copy_Gv_name(GV_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating vector graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing vector graphics method.
 */
static PyObject *
PyVCS_renameGv(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GV_OLD_NAME=NULL, *GV_NEW_NAME=NULL;
        extern int      renameGv_name();
 
        if(PyArg_ParseTuple(args,"|ss", &GV_OLD_NAME, &GV_NEW_NAME)) {
           if ((GV_OLD_NAME == NULL) || (GV_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new vector graphics method name.");
                 return NULL;
           }
        }

        ierr = renameGv_name(GV_OLD_NAME, GV_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming vector graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing vector graphics method.
 */
static PyObject *
PyVCS_removeGv(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the vector file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeGv_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed vector object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The vector object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing vector graphics method.
 */
static PyObject *
PyVCS_scriptGv(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GV_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_vector();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &GV_NAME, &SCRIPT_NAME, &MODE)) {
           if (GV_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the vector name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_vector(fp, GV_NAME) == 0) {
              sprintf(buf, "Error - Cannot save vector script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS  (GSp) graphics method member value. 
 */
static PyObject *
PyVCS_getGSpmember(PyVCScanvas_Object *self, PyObject *args)
{
	char 			*GSp_name, *member=NULL, buf[1024];
	PyObject 		*GSP=NULL, *MEMBER=NULL;
	struct gSp_tab         	*gSptab;
    	extern struct gSp_tab  	GSp_tab;

	if(PyArg_ParseTuple(args,"|OO",&GSP, &MEMBER)) {
           if (GSP == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


	Get_Member(GSP,"name", "s", &GSp_name);
	gSptab=&GSp_tab;
        while ((gSptab != NULL) &&
               (strcmp(gSptab->name, GSp_name) != 0))
           gSptab = gSptab->next;

     	if (gSptab == NULL) {
	   sprintf(buf,"Cannot find isofill graphics method GSp_%s.",GSp_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) {
           return Py_BuildValue("s", gSptab->pGSp_attr->proj);
	} else if (cmpncs(member, "xticlabels1") == 0) {
           return Py_BuildValue("s", gSptab->pGSp_attr->xtl1);
	} else if (cmpncs(member, "xticlabels2") == 0) {
           return Py_BuildValue("s", gSptab->pGSp_attr->xtl2);
	} else if (cmpncs(member, "xmtics1") == 0) {
           return Py_BuildValue("s", gSptab->pGSp_attr->xmt1);
	} else if (cmpncs(member, "xmtics2") == 0) {
           return Py_BuildValue("s", gSptab->pGSp_attr->xmt2);
	} else if (cmpncs(member, "yticlabels1") == 0) {
           return Py_BuildValue("s", gSptab->pGSp_attr->ytl1);
	} else if (cmpncs(member, "yticlabels2") == 0) {
           return Py_BuildValue("s", gSptab->pGSp_attr->ytl2);
	} else if (cmpncs(member, "ymtics1") == 0) {
           return Py_BuildValue("s", gSptab->pGSp_attr->ymt1);
	} else if (cmpncs(member, "ymtics2") == 0) {
           return Py_BuildValue("s", gSptab->pGSp_attr->ymt2);
	} else if (cmpncs(member, "datawc_y1") == 0) {
           return Py_BuildValue("f",gSptab->pGSp_attr->dsp[1]);
	} else if (cmpncs(member, "datawc_y2") == 0) {
           return Py_BuildValue("f",gSptab->pGSp_attr->dsp[3]);
	} else if (cmpncs(member, "datawc_x1") == 0) {
           return Py_BuildValue("f",gSptab->pGSp_attr->dsp[0]);
	} else if (cmpncs(member, "datawc_x2") == 0) {
           return Py_BuildValue("f",gSptab->pGSp_attr->dsp[2]);
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
           return Py_BuildValue("i",gSptab->pGSp_attr->idsp[1]);
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
           return Py_BuildValue("i",gSptab->pGSp_attr->idsp[3]);
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
           return Py_BuildValue("i",gSptab->pGSp_attr->idsp[0]);
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
           return Py_BuildValue("i",gSptab->pGSp_attr->idsp[2]);
	} else if (cmpncs(member, "datawc_calendar") == 0) {
           return Py_BuildValue("i",gSptab->pGSp_attr->calendar);
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
           return Py_BuildValue("s",gSptab->pGSp_attr->timeunits);
        } else if ((cmpncs(member, "xaxisconvert") == 0) &&
                   ((cmpncs(gSptab->pGSp_attr->xat,"\0") == 0))) {
           return Py_BuildValue("s", "linear");
        } else if (cmpncs(member, "xaxisconvert") == 0) {
           return Py_BuildValue("s", gSptab->pGSp_attr->xat);
        } else if ((cmpncs(member, "yaxisconvert") == 0) &&
                   ((cmpncs(gSptab->pGSp_attr->yat, "\0") == 0))) {
           return Py_BuildValue("s", "linear");
        } else if (cmpncs(member, "yaxisconvert") == 0) {
           return Py_BuildValue("s", gSptab->pGSp_attr->yat);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing scatter graphics method and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setGSpmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			MODE, value_int, marker_index, mcolor_index=0;	
	int			msize_index=0;
	long 			value_long;
	float 			value_float;
	double 			value_double;
        char 			*GSp_name, *member=NULL;
	char			*value_str=NULL, *Tm_name=NULL;
        PyObject 		*GSP=NULL, *MEMBER=NULL, *VALUE=NULL;
        PyObject		*marker_obj, *mcolor_obj, *msize_obj;
	struct gSp_tab          *get_gSptab=NULL;
	extern int              update_ind;
	struct gSp_attr         *pgSp;
        struct gSp_tab          *gSptab;
        extern struct gSp_tab   GSp_tab;
	extern struct gSp_tab   *getGSp();
	extern int              chk_mov_GSp();
	extern int 		vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &GSP, &MEMBER, &VALUE, &MODE)) {
           if (GSP == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(GSP,"name", "s", &GSp_name);
        gSptab=&GSp_tab;
        while ((gSptab != NULL) &&
               (strcmp(gSptab->name, GSp_name) != 0))
           gSptab = gSptab->next;

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	if (VALUE != NULL) {
	   if (PyString_Check(VALUE)) { /*check string*/
              value_str = PyString_AsString(VALUE);
	   } else if (PyInt_Check(VALUE)) { /*check for int*/
              value_int = (int) PyInt_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for float*/
              value_float = (float) PyFloat_AsDouble(VALUE);
	   } else if (PyLong_Check(VALUE)) { /* check for long*/
              value_long = PyLong_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for double*/
              value_double = PyFloat_AsDouble(VALUE);
	   } else if (PyDict_Check(VALUE)) { /*check for dictionary*/
              value_str = return_vcs_legacy_list(VALUE, member);
	   }
	}

	/*
	 * Set the appropriate scatter attribute. But first 
	 * get the scatter structure.
         */
	get_gSptab = getGSp(gSptab->name);
	if (cmpncs(member, "projection") == 0) {
	   strcpy(get_gSptab->pGSp_attr->proj, value_str);
	} else if (cmpncs(member, "xticlabels1") == 0) {
	   strcpy(get_gSptab->pGSp_attr->xtl1, value_str);
	} else if (cmpncs(member, "xticlabels2") == 0) {
	   strcpy(get_gSptab->pGSp_attr->xtl2, value_str);
	} else if (cmpncs(member, "xmtics1") == 0) {
	   strcpy(get_gSptab->pGSp_attr->xmt1, value_str);
	} else if (cmpncs(member, "xmtics2") == 0) {
	   strcpy(get_gSptab->pGSp_attr->xmt2, value_str);
	} else if (cmpncs(member, "yticlabels1") == 0) {
	   strcpy(get_gSptab->pGSp_attr->ytl1, value_str);
	} else if (cmpncs(member, "yticlabels2") == 0) {
	   strcpy(get_gSptab->pGSp_attr->ytl2, value_str);
	} else if (cmpncs(member, "ymtics1") == 0) {
	   strcpy(get_gSptab->pGSp_attr->ymt1, value_str);
	} else if (cmpncs(member, "ymtics2") == 0) {
	   strcpy(get_gSptab->pGSp_attr->ymt2, value_str);
	} else if (cmpncs(member, "datawc_x1") == 0) {
	   get_gSptab->pGSp_attr->dsp[0] = value_float;
	} else if (cmpncs(member, "datawc_y1") == 0) {
	   get_gSptab->pGSp_attr->dsp[1] = value_float;
	} else if (cmpncs(member, "datawc_x2") == 0) {
	   get_gSptab->pGSp_attr->dsp[2] = value_float;
	} else if (cmpncs(member, "datawc_y2") == 0) {
	   get_gSptab->pGSp_attr->dsp[3] = value_float;
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   get_gSptab->pGSp_attr->idsp[0] = value_int;
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   get_gSptab->pGSp_attr->idsp[1] = value_int;
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   get_gSptab->pGSp_attr->idsp[2] = value_int;
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   get_gSptab->pGSp_attr->idsp[3] = value_int;
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   get_gSptab->pGSp_attr->calendar = value_int;
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   strcpy(get_gSptab->pGSp_attr->timeunits, value_str);
	} else if (cmpncs(member, "xaxisconvert") == 0) {
	   strcpy(get_gSptab->pGSp_attr->xat, value_str);
	} else if (cmpncs(member, "yaxisconvert") == 0) {
	   strcpy(get_gSptab->pGSp_attr->yat, value_str);
	} else if ((cmpncs(member, "marker") == 0) ||
                   (cmpncs(member, "markercolor") == 0) ||
		   (cmpncs(member, "markersize") == 0)) {
           /* get the marker values */
           Get_Member(GSP,"marker", "O", &marker_obj);

           /* get the marker color values */
           Get_Member(GSP,"markercolor", "O", &mcolor_obj);

           /* get the marker size values */
           Get_Member(GSP,"markersize", "O", &msize_obj);

           /* Get the scatter structure */
           pgSp = get_gSptab->pGSp_attr;

           if (marker_obj==Py_None)
              marker_index = 0; /* default to no markers */
           else {
              if (cmpncs("dot", PyString_AsString(marker_obj))==0)
                 marker_index = 1;
              else if (cmpncs("plus", PyString_AsString(marker_obj))==0)
                 marker_index = 2;
              else if (cmpncs("star", PyString_AsString(marker_obj))==0)
                 marker_index = 3;
              else if (cmpncs("circle", PyString_AsString(marker_obj))==0)
                 marker_index = 4;
              else if (cmpncs("cross", PyString_AsString(marker_obj))==0)
                 marker_index = 5;
              else if (cmpncs("diamond", PyString_AsString(marker_obj))==0)
                 marker_index = 6;
              else if (cmpncs("triangle_up", PyString_AsString(marker_obj))==0)
                 marker_index = 7;
              else if (cmpncs("triangle_down", PyString_AsString(marker_obj))==0)
                 marker_index = 8;
              else if (cmpncs("triangle_left", PyString_AsString(marker_obj))==0)
                 marker_index = 9;
              else if (cmpncs("triangle_right", PyString_AsString(marker_obj))==0)
                 marker_index = 10;
              else if (cmpncs("square", PyString_AsString(marker_obj))==0)
                 marker_index = 11;
              else if (cmpncs("diamond_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 12;
              else if (cmpncs("triangle_up_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 13;
              else if (cmpncs("triangle_down_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 14;
              else if (cmpncs("triangle_left_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 15;
              else if (cmpncs("triangle_right_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 16;
              else if (cmpncs("square_fill", PyString_AsString(marker_obj))==0)
                 marker_index = 17;
              else if (cmpncs("hurricane", PyString_AsString(marker_obj))==0)
                 marker_index = 18;
	      /*Arulalan Weather Markers */
	      else if (cmpncs("w00", PyString_AsString(marker_obj))==0)
                 marker_index = 100;
              else if (cmpncs("w01", PyString_AsString(marker_obj))==0)
                 marker_index = 101;
              else if (cmpncs("w02", PyString_AsString(marker_obj))==0)
                 marker_index = 102;
              else if (cmpncs("w03", PyString_AsString(marker_obj))==0)
                 marker_index = 103;
 	      else if (cmpncs("w04", PyString_AsString(marker_obj))==0)
                 marker_index = 104;
              else if (cmpncs("w05", PyString_AsString(marker_obj))==0)
                 marker_index = 105;
              else if (cmpncs("w06", PyString_AsString(marker_obj))==0)
                 marker_index = 106;
              else if (cmpncs("w07", PyString_AsString(marker_obj))==0)
                 marker_index = 107;
              else if (cmpncs("w08", PyString_AsString(marker_obj))==0)
                 marker_index = 108;
              else if (cmpncs("w09", PyString_AsString(marker_obj))==0)
                 marker_index = 109;
              else if (cmpncs("w10", PyString_AsString(marker_obj))==0)
                 marker_index = 110;
              else if (cmpncs("w11", PyString_AsString(marker_obj))==0)
                 marker_index = 111;
              else if (cmpncs("w12", PyString_AsString(marker_obj))==0)
                 marker_index = 112;
              else if (cmpncs("w13", PyString_AsString(marker_obj))==0)
                 marker_index = 113;
	      else if (cmpncs("w14", PyString_AsString(marker_obj))==0)
                 marker_index = 114;
              else if (cmpncs("w15", PyString_AsString(marker_obj))==0)
                 marker_index = 115;
              else if (cmpncs("w16", PyString_AsString(marker_obj))==0)
                 marker_index = 116;
              else if (cmpncs("w17", PyString_AsString(marker_obj))==0)
                 marker_index = 117;
              else if (cmpncs("w18", PyString_AsString(marker_obj))==0)
                 marker_index = 118;
              else if (cmpncs("w19", PyString_AsString(marker_obj))==0)
                 marker_index = 119;
	      else if (cmpncs("w20", PyString_AsString(marker_obj))==0)
                 marker_index = 120;
              else if (cmpncs("w21", PyString_AsString(marker_obj))==0)
                 marker_index = 121;
              else if (cmpncs("w22", PyString_AsString(marker_obj))==0)
                 marker_index = 122;
              else if (cmpncs("w23", PyString_AsString(marker_obj))==0)
                 marker_index = 123;
 	      else if (cmpncs("w24", PyString_AsString(marker_obj))==0)
                 marker_index = 124;
              else if (cmpncs("w25", PyString_AsString(marker_obj))==0)
                 marker_index = 125;
              else if (cmpncs("w26", PyString_AsString(marker_obj))==0)
                 marker_index = 126;
              else if (cmpncs("w27", PyString_AsString(marker_obj))==0)
                 marker_index = 127;
              else if (cmpncs("w28", PyString_AsString(marker_obj))==0)
                 marker_index = 128;
              else if (cmpncs("w29", PyString_AsString(marker_obj))==0)
                 marker_index = 129;
              else if (cmpncs("w30", PyString_AsString(marker_obj))==0)
                 marker_index = 130;
              else if (cmpncs("w31", PyString_AsString(marker_obj))==0)
                 marker_index = 131;
              else if (cmpncs("w32", PyString_AsString(marker_obj))==0)
                 marker_index = 132;
              else if (cmpncs("w33", PyString_AsString(marker_obj))==0)
                 marker_index = 133;
	      else if (cmpncs("w34", PyString_AsString(marker_obj))==0)
                 marker_index = 134;
              else if (cmpncs("w35", PyString_AsString(marker_obj))==0)
                 marker_index = 135;
              else if (cmpncs("w36", PyString_AsString(marker_obj))==0)
                 marker_index = 136;
              else if (cmpncs("w37", PyString_AsString(marker_obj))==0)
                 marker_index = 137;
              else if (cmpncs("w38", PyString_AsString(marker_obj))==0)
                 marker_index = 138;
              else if (cmpncs("w39", PyString_AsString(marker_obj))==0)
                 marker_index = 139;
	      else if (cmpncs("w40", PyString_AsString(marker_obj))==0)
                 marker_index = 140;
              else if (cmpncs("w41", PyString_AsString(marker_obj))==0)
                 marker_index = 141;
              else if (cmpncs("w42", PyString_AsString(marker_obj))==0)
                 marker_index = 142;
              else if (cmpncs("w43", PyString_AsString(marker_obj))==0)
                 marker_index = 143;
 	      else if (cmpncs("w44", PyString_AsString(marker_obj))==0)
                 marker_index = 144;
              else if (cmpncs("w45", PyString_AsString(marker_obj))==0)
                 marker_index = 145;
              else if (cmpncs("w46", PyString_AsString(marker_obj))==0)
                 marker_index = 146;
              else if (cmpncs("w47", PyString_AsString(marker_obj))==0)
                 marker_index = 147;
              else if (cmpncs("w48", PyString_AsString(marker_obj))==0)
                 marker_index = 148;
              else if (cmpncs("w49", PyString_AsString(marker_obj))==0)
                 marker_index = 149;
              else if (cmpncs("w50", PyString_AsString(marker_obj))==0)
                 marker_index = 150;
              else if (cmpncs("w51", PyString_AsString(marker_obj))==0)
                 marker_index = 151;
              else if (cmpncs("w52", PyString_AsString(marker_obj))==0)
                 marker_index = 152;
              else if (cmpncs("w53", PyString_AsString(marker_obj))==0)
                 marker_index = 153;
	      else if (cmpncs("w54", PyString_AsString(marker_obj))==0)
                 marker_index = 154;
              else if (cmpncs("w55", PyString_AsString(marker_obj))==0)
                 marker_index = 155;
              else if (cmpncs("w56", PyString_AsString(marker_obj))==0)
                 marker_index = 156;
              else if (cmpncs("w57", PyString_AsString(marker_obj))==0)
                 marker_index = 157;
              else if (cmpncs("w58", PyString_AsString(marker_obj))==0)
                 marker_index = 158;
              else if (cmpncs("w59", PyString_AsString(marker_obj))==0)
                 marker_index = 159;
	      else if (cmpncs("w60", PyString_AsString(marker_obj))==0)
                 marker_index = 160;
              else if (cmpncs("w61", PyString_AsString(marker_obj))==0)
                 marker_index = 161;
              else if (cmpncs("w62", PyString_AsString(marker_obj))==0)
                 marker_index = 162;
              else if (cmpncs("w63", PyString_AsString(marker_obj))==0)
                 marker_index = 163;
 	      else if (cmpncs("w64", PyString_AsString(marker_obj))==0)
                 marker_index = 164;
              else if (cmpncs("w65", PyString_AsString(marker_obj))==0)
                 marker_index = 165;
              else if (cmpncs("w66", PyString_AsString(marker_obj))==0)
                 marker_index = 166;
              else if (cmpncs("w67", PyString_AsString(marker_obj))==0)
                 marker_index = 167;
              else if (cmpncs("w68", PyString_AsString(marker_obj))==0)
                 marker_index = 168;
              else if (cmpncs("w69", PyString_AsString(marker_obj))==0)
                 marker_index = 169;
              else if (cmpncs("w70", PyString_AsString(marker_obj))==0)
                 marker_index = 170;
              else if (cmpncs("w71", PyString_AsString(marker_obj))==0)
                 marker_index = 171;
              else if (cmpncs("w72", PyString_AsString(marker_obj))==0)
                 marker_index = 172;
              else if (cmpncs("w73", PyString_AsString(marker_obj))==0)
                 marker_index = 173;
	      else if (cmpncs("w74", PyString_AsString(marker_obj))==0)
                 marker_index = 174;
              else if (cmpncs("w75", PyString_AsString(marker_obj))==0)
                 marker_index = 175;
              else if (cmpncs("w76", PyString_AsString(marker_obj))==0)
                 marker_index = 176;
              else if (cmpncs("w77", PyString_AsString(marker_obj))==0)
                 marker_index = 177;
              else if (cmpncs("w78", PyString_AsString(marker_obj))==0)
                 marker_index = 178;
              else if (cmpncs("w79", PyString_AsString(marker_obj))==0)
                 marker_index = 179;
	      else if (cmpncs("w80", PyString_AsString(marker_obj))==0)
                 marker_index = 180;
              else if (cmpncs("w81", PyString_AsString(marker_obj))==0)
                 marker_index = 181;
              else if (cmpncs("w82", PyString_AsString(marker_obj))==0)
                 marker_index = 182;
              else if (cmpncs("w83", PyString_AsString(marker_obj))==0)
                 marker_index = 183;
 	      else if (cmpncs("w84", PyString_AsString(marker_obj))==0)
                 marker_index = 184;
              else if (cmpncs("w85", PyString_AsString(marker_obj))==0)
                 marker_index = 185;
              else if (cmpncs("w86", PyString_AsString(marker_obj))==0)
                 marker_index = 186;
              else if (cmpncs("w87", PyString_AsString(marker_obj))==0)
                 marker_index = 187;
              else if (cmpncs("w88", PyString_AsString(marker_obj))==0)
                 marker_index = 188;
              else if (cmpncs("w89", PyString_AsString(marker_obj))==0)
                 marker_index = 189;
              else if (cmpncs("w90", PyString_AsString(marker_obj))==0)
                 marker_index = 190;
              else if (cmpncs("w91", PyString_AsString(marker_obj))==0)
                 marker_index = 191;
              else if (cmpncs("w92", PyString_AsString(marker_obj))==0)
                 marker_index = 192;
              else if (cmpncs("w93", PyString_AsString(marker_obj))==0)
                 marker_index = 193;
	      else if (cmpncs("w94", PyString_AsString(marker_obj))==0)
                 marker_index = 194;
              else if (cmpncs("w95", PyString_AsString(marker_obj))==0)
                 marker_index = 195;
              else if (cmpncs("w96", PyString_AsString(marker_obj))==0)
                 marker_index = 196;
              else if (cmpncs("w97", PyString_AsString(marker_obj))==0)
                 marker_index = 197;
              else if (cmpncs("w98", PyString_AsString(marker_obj))==0)
                 marker_index = 198;
              else if (cmpncs("w99", PyString_AsString(marker_obj))==0)
                 marker_index = 199;
	      else if (cmpncs("w200", PyString_AsString(marker_obj))==0)
                 marker_index = 200;
              else if (cmpncs("w201", PyString_AsString(marker_obj))==0)
                 marker_index = 201;
              else if (cmpncs("w202", PyString_AsString(marker_obj))==0)
                 marker_index = 202;
              else {
                 Tm_name = PyString_AsString(marker_obj);
                 marker_index = 999;
              }
          }
          if (marker_index != 999) {
             if (mcolor_obj == Py_None)
                mcolor_index = 241; /* set color to default black color*/
             else
                mcolor_index = (int) PyInt_AsLong(mcolor_obj);
             if (msize_obj == Py_None)
                msize_index = 7; /* set marker size to default size of 7*/
             else
                msize_index = (int) PyInt_AsLong(msize_obj);
   
              if ((marker_obj==Py_None) && (mcolor_obj == Py_None) && (msize_obj == Py_None))
                 strcpy(pgSp->mb,"\0");
              else
                 strcpy(pgSp->mb,return_new_marker_attribute(GSp_name, 0, marker_index, mcolor_index, msize_index));
          } else /* must be a marker object */
               strcpy(pgSp->mb, Tm_name);
	}

	chk_mov_GSp(get_gSptab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new scatter graphics method by copying from an existing
 * scatter graphics method. If no source copy name argument is given,
 * then the default scatter graphics method will be used to replicate
 * the new scatter graphics method.
 */
static PyObject *
PyVCS_copyGSp(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GSP_SRC=NULL, *GSP_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_GSp_name();
              
        if(PyArg_ParseTuple(args,"|ss", &GSP_SRC, &GSP_NAME)) {
           if (GSP_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source scatter graphics method name.");
                 return NULL;
           }

           if (GSP_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", GSP_NAME);
        }

        ierr = copy_GSp_name(GSP_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating scatter graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing scatter graphics method.
 */
static PyObject *
PyVCS_renameGSp(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GSP_OLD_NAME=NULL, *GSP_NEW_NAME=NULL;
        extern int      renameGSp_name();
 
        if(PyArg_ParseTuple(args,"|ss", &GSP_OLD_NAME, &GSP_NEW_NAME)) {
           if ((GSP_OLD_NAME == NULL) || (GSP_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new scatter graphics method name.");
                 return NULL;
           }
        }

        ierr = renameGSp_name(GSP_OLD_NAME, GSP_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming scatter graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing scatter graphics method.
 */
static PyObject *
PyVCS_removeGSp(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the scatter file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeGSp_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed scatter object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The scatter object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing scatter graphics method.
 */
static PyObject *
PyVCS_scriptGSp(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GSP_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_scatter();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &GSP_NAME, &SCRIPT_NAME, &MODE)) {
           if (GSP_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the scatter name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_scatter(fp, GSP_NAME) == 0) {
              sprintf(buf, "Error - Cannot save scatter script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS continents (Gcon) graphics method member value. 
 */
static PyObject *
PyVCS_getGconmember(PyVCScanvas_Object *self, PyObject *args)
{
	char 			*Gcon_name, *member=NULL, buf[1024];
	int 			i=0, ct=0;
	PyObject 		*GCON=NULL, *MEMBER=NULL, *tup, *lp;
	struct gcon_tab        	*gcontab;
    	extern struct gcon_tab 	Gcon_tab;
	struct gcon_attr        *pgcon;

	if(PyArg_ParseTuple(args,"|OO",&GCON, &MEMBER)) {
           if (GCON == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}


	Get_Member(GCON,"name", "s", &Gcon_name);
	gcontab=&Gcon_tab;
        while ((gcontab != NULL) &&
               (strcmp(gcontab->name, Gcon_name) != 0))
           gcontab = gcontab->next;

     	if (gcontab == NULL) {
	   sprintf(buf,"Cannot find isofill graphics method Gcon_%s.",Gcon_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) {
           return Py_BuildValue("s", gcontab->pGcon_attr->proj);
	} else if (cmpncs(member, "xticlabels1") == 0) {
           return Py_BuildValue("s", gcontab->pGcon_attr->xtl1);
	} else if (cmpncs(member, "xticlabels2") == 0) {
           return Py_BuildValue("s", gcontab->pGcon_attr->xtl2);
	} else if (cmpncs(member, "xmtics1") == 0) {
           return Py_BuildValue("s", gcontab->pGcon_attr->xmt1);
	} else if (cmpncs(member, "xmtics2") == 0) {
           return Py_BuildValue("s", gcontab->pGcon_attr->xmt2);
	} else if (cmpncs(member, "yticlabels1") == 0) {
           return Py_BuildValue("s", gcontab->pGcon_attr->ytl1);
	} else if (cmpncs(member, "yticlabels2") == 0) {
           return Py_BuildValue("s", gcontab->pGcon_attr->ytl2);
	} else if (cmpncs(member, "ymtics1") == 0) {
           return Py_BuildValue("s", gcontab->pGcon_attr->ymt1);
	} else if (cmpncs(member, "ymtics2") == 0) {
           return Py_BuildValue("s", gcontab->pGcon_attr->ymt2);
	} else if (cmpncs(member, "datawc_y1") == 0) {
           return Py_BuildValue("f",gcontab->pGcon_attr->dsp[1]);
	} else if (cmpncs(member, "datawc_y2") == 0) {
           return Py_BuildValue("f",gcontab->pGcon_attr->dsp[3]);
	} else if (cmpncs(member, "datawc_x1") == 0) {
           return Py_BuildValue("f",gcontab->pGcon_attr->dsp[0]);
	} else if (cmpncs(member, "datawc_x2") == 0) {
           return Py_BuildValue("f",gcontab->pGcon_attr->dsp[2]);
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
           return Py_BuildValue("i",gcontab->pGcon_attr->idsp[1]);
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
           return Py_BuildValue("i",gcontab->pGcon_attr->idsp[3]);
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
           return Py_BuildValue("i",gcontab->pGcon_attr->idsp[0]);
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
           return Py_BuildValue("i",gcontab->pGcon_attr->idsp[2]);
	} else if (cmpncs(member, "datawc_calendar") == 0) {
           return Py_BuildValue("i",gcontab->pGcon_attr->calendar);
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
           return Py_BuildValue("s",gcontab->pGcon_attr->timeunits);
	} else if (cmpncs(member, "type") == 0) {
           return Py_BuildValue("i",gcontab->pGcon_attr->cont_type);
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing continents graphics method and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the graphics method's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setGconmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 			i,j=0,n,ct=0,lct=1,color_index=0,line_index;
	int 			MODE, value_int, type_index=0, text_color_index=0;	
	long 			value_long;
	float 			value_float, width_index=1.0;
	double 			value_double;
	char 			*Tl_name, buf[1024];
        char 			*Gcon_name, *str=NULL, *member=NULL;
	char			*value_str=NULL;
        PyObject 		*GCON=NULL, *MEMBER=NULL, *VALUE=NULL;
	PyObject 		*itempk,*itempv,*pkeys,*pvalues;
        PyObject		*listit,*type_obj, *line_obj, *listtt, *color_obj, *width_obj;
	struct gcon_tab         *get_gcontab=NULL;
	extern int              update_ind;
	struct gcon_attr        *pgcon;
        struct gcon_tab         *gcontab;
        extern struct gcon_tab  Gcon_tab;
	extern struct gcon_tab  *getGcon();
	extern int              chk_mov_Gcon();
	extern int 		vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &GCON, &MEMBER, &VALUE, &MODE)) {
           if (GCON == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(GCON,"name", "s", &Gcon_name);
        gcontab=&Gcon_tab;
        while ((gcontab != NULL) &&
               (strcmp(gcontab->name, Gcon_name) != 0))
           gcontab = gcontab->next;

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	if (VALUE != NULL) {
	   if (PyString_Check(VALUE)) { /*check string*/
              value_str = PyString_AsString(VALUE);
	   } else if (PyInt_Check(VALUE)) { /*check for int*/
              value_int = (int) PyInt_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for float*/
              value_float = (float) PyFloat_AsDouble(VALUE);
	   } else if (PyLong_Check(VALUE)) { /* check for long*/
              value_long = PyLong_AsLong(VALUE);
	   } else if (PyFloat_Check(VALUE)) { /*check for double*/
              value_double = PyFloat_AsDouble(VALUE);
	   } else if (PyDict_Check(VALUE)) { /*check for dictionary*/
              value_str = return_vcs_legacy_list(VALUE, member);
	   }
	}

	/*
	 * Set the appropriate continents attribute. But first 
	 * get the continents structure.
         */
	get_gcontab = getGcon(gcontab->name);
	if (cmpncs(member, "projection") == 0) {
	   strcpy(get_gcontab->pGcon_attr->proj, value_str);
	} else if (cmpncs(member, "xticlabels1") == 0) {
	   strcpy(get_gcontab->pGcon_attr->xtl1, value_str);
	} else if (cmpncs(member, "xticlabels2") == 0) {
	   strcpy(get_gcontab->pGcon_attr->xtl2, value_str);
	} else if (cmpncs(member, "xmtics1") == 0) {
	   strcpy(get_gcontab->pGcon_attr->xmt1, value_str);
	} else if (cmpncs(member, "xmtics2") == 0) {
	   strcpy(get_gcontab->pGcon_attr->xmt2, value_str);
	} else if (cmpncs(member, "yticlabels1") == 0) {
	   strcpy(get_gcontab->pGcon_attr->ytl1, value_str);
	} else if (cmpncs(member, "yticlabels2") == 0) {
	   strcpy(get_gcontab->pGcon_attr->ytl2, value_str);
	} else if (cmpncs(member, "ymtics1") == 0) {
	   strcpy(get_gcontab->pGcon_attr->ymt1, value_str);
	} else if (cmpncs(member, "ymtics2") == 0) {
	   strcpy(get_gcontab->pGcon_attr->ymt2, value_str);
	} else if (cmpncs(member, "datawc_x1") == 0) {
	   get_gcontab->pGcon_attr->dsp[0] = value_float;
	} else if (cmpncs(member, "datawc_y1") == 0) {
	   get_gcontab->pGcon_attr->dsp[1] = value_float;
	} else if (cmpncs(member, "datawc_x2") == 0) {
	   get_gcontab->pGcon_attr->dsp[2] = value_float;
	} else if (cmpncs(member, "datawc_y2") == 0) {
	   get_gcontab->pGcon_attr->dsp[3] = value_float;
	} else if (cmpncs(member, "_tdatawc_x1") == 0) {
	   get_gcontab->pGcon_attr->idsp[0] = value_int;
	} else if (cmpncs(member, "_tdatawc_y1") == 0) {
	   get_gcontab->pGcon_attr->idsp[1] = value_int;
	} else if (cmpncs(member, "_tdatawc_x2") == 0) {
	   get_gcontab->pGcon_attr->idsp[2] = value_int;
	} else if (cmpncs(member, "_tdatawc_y2") == 0) {
	   get_gcontab->pGcon_attr->idsp[3] = value_int;
	} else if (cmpncs(member, "datawc_calendar") == 0) {
	   get_gcontab->pGcon_attr->calendar = value_int;
	} else if (cmpncs(member, "datawc_timeunits") == 0) {
	   strcpy(get_gcontab->pGcon_attr->timeunits, value_str);
	} else if ((cmpncs(member, "type") == 0) || (cmpncs(member, "line") == 0) ||
		   (cmpncs(member, "linecolor") == 0) || (cmpncs(member, "linewidth") == 0)) {
           /* get the type value */
           Get_Member(GCON,"type", "i", &type_index);

           /* get the line values */
           Get_Member(GCON,"line", "O", &line_obj);

           /* get the color values */
           Get_Member(GCON,"linecolor", "O", &color_obj);

           /* get the width values */
           Get_Member(GCON,"linewidth", "O", &width_obj);

           /* Get the continents structure */
           pgcon = get_gcontab->pGcon_attr;

           if (line_obj==Py_None) { /* default to solid line */
              if (color_obj == Py_None)
                 color_index = 241; /* set color to default black color*/
              else
                 color_index = (int) PyInt_AsLong(color_obj);

              if (width_obj == Py_None)
                 width_index = 1.0; /* set width to default size 1.0*/
              else
                 width_index = (float) PyFloat_AsDouble(width_obj);
    
	       if ((line_obj==Py_None) && (color_obj == Py_None) && (width_obj == Py_None))
                  strcpy(pgcon->lb,"default");
               else
                  strcpy(pgcon->lb,return_new_line_attribute(Gcon_name,0,1,color_index, width_index));
           } else if (PyString_Check(line_obj)) {
              if (cmpncs("solid", PyString_AsString(line_obj))==0)
                 line_index = 1;
              else if (cmpncs("dash", PyString_AsString(line_obj))==0)
                 line_index = 2;
              else if (cmpncs("dot", PyString_AsString(line_obj))==0)
                 line_index = 3;
              else if (cmpncs("dash-dot", PyString_AsString(line_obj))==0)
                 line_index = 4;
              else if (cmpncs("long-dash", PyString_AsString(line_obj))==0)
                 line_index = -3;
              else {
                 Tl_name = PyString_AsString(line_obj);
                 line_index = 999;
              }
              if (line_index != 999) {
                 if (color_obj == Py_None)
                    color_index = 241; /* set color to default black color*/
                 else
                    color_index = (int) PyInt_AsLong(color_obj);
       
                 if (width_obj == Py_None)
                    width_index = 1.0; /* set width to default size 1.0*/
                 else
                    width_index = (float) PyFloat_AsDouble(width_obj);
       
	          if ((line_obj==Py_None) && (color_obj == Py_None) && (width_obj == Py_None))
                     strcpy(pgcon->lb,"default");
                  else
                     strcpy(pgcon->lb,return_new_line_attribute(Gcon_name, 0, line_index, color_index, width_index));
              } else /* must be a line object */
               strcpy(pgcon->lb, Tl_name);
           }

           pgcon->cont_type = (int) type_index;
	}

	chk_mov_Gcon(get_gcontab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new contintents graphics method by copying from an existing
 * contintents graphics method. If no source copy name argument is given,
 * then the default continents graphics method will be used to replicate
 * the new continents graphics method.
 */
static PyObject *
PyVCS_copyGcon(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GCON_SRC=NULL, *GCON_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Gcon_name();
              
        if(PyArg_ParseTuple(args,"|ss", &GCON_SRC, &GCON_NAME)) {
           if (GCON_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source continents graphics method name.");
                 return NULL;
           }

           if (GCON_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", GCON_NAME);
        }

        ierr = copy_Gcon_name(GCON_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating continents graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing continents graphics method.
 */
static PyObject *
PyVCS_renameGcon(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *GCON_OLD_NAME=NULL, *GCON_NEW_NAME=NULL;
        extern int      renameGcon_name();
 
        if(PyArg_ParseTuple(args,"|ss", &GCON_OLD_NAME, &GCON_NEW_NAME)) {
           if ((GCON_OLD_NAME == NULL) || (GCON_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new continents graphics method name.");
                 return NULL;
           }
        }

        ierr = renameGcon_name(GCON_OLD_NAME, GCON_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming continents graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing continents graphics method.
 */
static PyObject *
PyVCS_removeGcon(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the continents file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeGcon_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed continents object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The continents object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing continents graphics method.
 */
static PyObject *
PyVCS_scriptGcon(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *GCON_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_continents();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &GCON_NAME, &SCRIPT_NAME, &MODE)) {
           if (GCON_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the continents name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }
        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_continents(fp, GCON_NAME) == 0) {
              sprintf(buf, "Error - Cannot save continents script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS colormap (Cp) class member value. 
 */
static PyObject *
PyVCS_getCpmember(PyVCScanvas_Object *self, PyObject *args)
{
        char                            *Cp_name, buf[1024],*member=NULL;
	int				i,c,imember;
        PyObject                        *CP=NULL, *MEMBER=NULL;
        PyObject                        *listptr=NULL, *tupleptr=NULL, *dictptr=NULL;
        struct color_table              *Cptab;
	extern struct c_val 		std_color[16];
        extern struct color_table       C_tab;
              
        if(PyArg_ParseTuple(args,"|OO",&CP, &MEMBER)) {
           if (CP == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              if (PyInt_Check(MEMBER))       /* check for int */
                 i = imember = (int)PyInt_AsLong(MEMBER);
	      else
	         member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a index number.");
              return NULL;
           }
        }     

        Get_Member(CP,"name", "s", &Cp_name);
        for(Cptab=&C_tab;Cptab != NULL && (c=strcmp(Cptab->name,Cp_name))!=0;
                                        Cptab=Cptab->next);

        if (c != 0) {
           sprintf(buf,"Cannot find colormap class object Cp_%s.",Cp_name);
           PyErr_SetString(PyExc_TypeError, buf);
           return NULL;
        }

        if (PyInt_Check(MEMBER)) {       /* check for int */
           if ((imember >= 0) && (imember < 240)) {
              listptr = Py_BuildValue("[i,i,i]", (int )(Cptab->cval[i].red+0.5),
                        (int )(Cptab->cval[i].green+0.5),(int )(Cptab->cval[i].blue+0.5));
              return listptr;
           }
           if ((imember >= 240) && (imember < 256)) {
              tupleptr = Py_BuildValue("(i,i,i)", (int )(std_color[i-240].red+0.5),
                         (int )(std_color[i-240].green+0.5),(int )(std_color[i-240].blue+0.5));
              return tupleptr;
           }
        }

	if (cmpncs(member, "index") == 0) {
	   dictptr = PyDict_New();
	   if (strcmp(Cp_name, "default") != 0) {
	      for (i=0; i<240; i++) {
                 listptr = Py_BuildValue("[i,i,i]", (int )(Cptab->cval[i].red+0.5),
                          (int )(Cptab->cval[i].green+0.5),(int )(Cptab->cval[i].blue+0.5));
	         PyDict_SetItem(dictptr, Py_BuildValue("i",i), listptr);
	      }
	   } else {
	      for (i=0; i<240; i++) {
                 listptr = Py_BuildValue("(i,i,i)", (int )(Cptab->cval[i].red+0.5),
                          (int )(Cptab->cval[i].green+0.5),(int )(Cptab->cval[i].blue+0.5));
	         PyDict_SetItem(dictptr, Py_BuildValue("i",i), listptr);
	      }
	   }
           for (i=240;i<256;i++) {
              tupleptr = Py_BuildValue("(i,i,i)", (int )(std_color[i-240].red+0.5),
                          (int )(std_color[i-240].green+0.5),(int )(std_color[i-240].blue+0.5));
	      PyDict_SetItem(dictptr, Py_BuildValue("i",i), tupleptr);
           }

           return dictptr;
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing colormap object and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the colormap  object's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setCpmember(self, args)
  PyObject *self;
  PyObject *args;
{
        int                             MODE, KEY, value_int,i,c;
        float                           value_float;
        char                            *Cp_name, *member=NULL, buf[1024];
        PyObject                        *CP=NULL, *MEMBER=NULL, *VALUE=NULL;
        PyVCScanvas_Object		*CANVAS=NULL;
        struct color_table              *get_Cptab=NULL, *Cptab=NULL;
        extern char                     active_colors[]; /*colormap name*/
        extern struct color_table       C_tab;
        extern int                      update_ind;
        extern int                      set_active_colors();
	extern int 			vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOiOi", &CANVAS, &CP, &MEMBER, &KEY, &VALUE, &MODE)) {
           if (CP == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(CP,"name", "s", &Cp_name);
        for(Cptab=&C_tab;Cptab != NULL && (c=strcmp(Cptab->name,Cp_name))!=0;
                                        Cptab=Cptab->next);
        if (c != 0) {
           sprintf(buf,"Cannot find colormap class object Cp_%s.",Cp_name);
           PyErr_SetString(PyExc_TypeError, buf);
           return NULL;
        }

        if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

        /*
         * Set the appropriate colormap attribute. But first
         * get the colormap structure.
         */
        for(get_Cptab=&C_tab;get_Cptab != NULL && (c=strcmp(get_Cptab->name,Cptab->name))!=0;
                                        get_Cptab=get_Cptab->next);
        if (cmpncs(member, "index") == 0) {
           if (PyList_Check(VALUE)) { /* check for list */
               get_Cptab->cval[KEY].red   = (int) PyInt_AsLong(PyList_GetItem(VALUE, 0));
               get_Cptab->cval[KEY].green = (int) PyInt_AsLong(PyList_GetItem(VALUE, 1));
               get_Cptab->cval[KEY].blue  = (int) PyInt_AsLong(PyList_GetItem(VALUE, 2));
           } else {
              sprintf(buf,"Invalid object type for Cp_%s.",Cp_name);
              PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	   }
        }

        /* DNW 10/11/04 - This slows down IaGraph substantially for the 2nd,
         *                3rd, 4th, etc. plots. Keep an eye out for this 
         *                commented out command below to see if it effects
         *                other parts of VCS.
         *
        set_active_colors(); DNW - This slows down IaGraphics substantially
                                   for the second plot. */

	if ((cmpncs(active_colors, Cp_name) == 0) && (MODE == 1)) {
	   PyVCS_updateVCSsegments(CANVAS, NULL);
           update_ind = MODE; /* Update the display if needed */
	   vcs_legacy_canvas_update(0);
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Create a new colormap object method by copying from an existing
 * colormap object method. If no source copy name argument is given,
 * then the default colormap object method will be used to replicate
 * the new colormap object.
 */
static PyObject *
PyVCS_copyCp(self, args)
  PyObject *self;
  PyObject *args;
{
        int             		ierr,c;
        char            		*TC_SRC=NULL, *TC_NAME=NULL;
        char            		copy_name[1024],buf[1024];
        struct color_table              *Cptab;
        extern struct color_table       C_tab;
	extern int      		save_colors();

        if(PyArg_ParseTuple(args,"|ss", &TC_SRC, &TC_NAME)) {
           if (TC_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source colormap graphics method name.");
                 return NULL;
           }

           if (TC_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", TC_NAME);
        }

        for(Cptab=&C_tab;Cptab != NULL && (c=strcmp(Cptab->name,TC_SRC))!=0;
                                        Cptab=Cptab->next);
	if (c != 0) {
           sprintf(buf,"Cannot find colormap class object Cp_%s.",TC_SRC);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }
        ierr = save_colors(copy_name, Cptab->cval);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating colormap graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Rename an existing colormap secondary object.
 */
static PyObject *
PyVCS_renameCp(self, args)
  PyObject *self;
  PyObject *args;
{
        int             		c,ierr;
        char            		buf[1024];
        char            		*CP_OLD_NAME=NULL, *CP_NEW_NAME=NULL;
        struct color_table              *Cptab;
        extern struct color_table       C_tab;

        if(PyArg_ParseTuple(args,"|ss", &CP_OLD_NAME, &CP_NEW_NAME)) {
           if ((CP_OLD_NAME == NULL) || (CP_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new colormap object name.");
                 return NULL;
           }
        }

        for(Cptab=&C_tab;Cptab != NULL && (c=strcmp(Cptab->name,CP_OLD_NAME))!=0;
                                        Cptab=Cptab->next);
	if (c != 0) {
           sprintf(buf,"Cannot find colormap class object Cp_%s.",CP_OLD_NAME);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }
	if (strlen(CP_OLD_NAME) > 16) {
           sprintf(buf,"Colormap (Cp_%s) name cannot be longer than 16.",CP_OLD_NAME);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}
	strcpy(Cptab->name,CP_OLD_NAME);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Remove an existing colormap secondary object. 
 */
static PyObject *
PyVCS_removeCp(self, args)
  PyObject *self;
  PyObject *args;
{
        char *REMOVE_NAME=NULL, buf[1024];
	extern int      removeC();

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the colormap file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
        if (removeC(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed colormap object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
        } else {
           sprintf(buf,"The colormap object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
        }
}

/*
 * Script out an existing colormap object.
 */
static PyObject *
PyVCS_scriptCp(self, args)
  PyObject *self;
  PyObject *args;
{
        int ffd, wfd;
        char *SCRIPT_NAME=NULL, *CP_NAME=NULL, *MODE=NULL, buf[1024];
        char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
        extern int dump_single_colormap();
        FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &CP_NAME, &SCRIPT_NAME, &MODE)) {
           if (CP_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the colormap name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }


        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command colormap */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
           sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
           if (dump_single_colormap(fp, CP_NAME) == 0) {
              sprintf(buf, "Error - Cannot save colormap script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}

/* 
 * Return the VCS line (Tl) class member value. 
 */
static PyObject *
PyVCS_getTlmember(PyVCScanvas_Object *self, PyObject *args)
{
        int                             i,j,npts;
	char 				*Tl_name, *member=NULL, buf[1024];
	PyObject 			*TL=NULL, *MEMBER=NULL;
        PyObject                        *x=NULL, *y=NULL, **listptr, *vptr, *v;
	struct table_line    		*Tltab;
	struct array_segments 		*aptr;
	extern struct table_line        Tl_tab;
   	extern struct table_line 	*getTl();

	if(PyArg_ParseTuple(args,"|OO",&TL, &MEMBER)) {
           if (TL == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}

	Get_Member(TL,"name", "s", &Tl_name);
	Tltab=getTl(Tl_name);

     	if (Tltab == NULL) {
	   sprintf(buf,"Cannot find line class object Tl_%s.",Tl_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) 
	  {
	    return Py_BuildValue("s", Tltab->proj);
	  }
	else if (cmpncs(member, "type") == 0) {
           if (Tltab->ltyp == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           npts = Tltab->ltyp_size;
           v=PyList_New(npts);
           for (i=0; i<npts; i++) {
              if (Tltab->ltyp[i] == 1)
                 PyList_SetItem(v, i, Py_BuildValue("s", "solid"));
              else if (Tltab->ltyp[i] == 2)
                 PyList_SetItem(v, i, Py_BuildValue("s", "dash"));
              else if (Tltab->ltyp[i] == 3)
                 PyList_SetItem(v, i, Py_BuildValue("s", "dot"));
              else if (Tltab->ltyp[i] == 4)
                 PyList_SetItem(v, i, Py_BuildValue("s", "dash-dot"));
              else if (Tltab->ltyp[i] == -3)
                 PyList_SetItem(v, i, Py_BuildValue("s", "long-dash"));
           }
           return v;
	} else if (cmpncs(member, "width") == 0) {
           if (Tltab->lwsf == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           npts = Tltab->lwsf_size;
           v=PyList_New(npts);
           for (i=0; i<npts; i++)
              PyList_SetItem(v, i, Py_BuildValue("d", *(Tltab->lwsf)));
           return v;
	} else if (cmpncs(member, "color") == 0) {
           if (Tltab->lci == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           npts = Tltab->lci_size;
           v=PyList_New(npts);
           for (i=0; i<npts; i++)
              PyList_SetItem(v, i, Py_BuildValue("i", *(Tltab->lci)));
           return v;
	} else if (cmpncs(member, "priority") == 0) {
           return Py_BuildValue("i", Tltab->priority);
	} else if (cmpncs(member, "viewport") == 0) {
           return Py_BuildValue("[f,f,f,f]", Tltab->lvp[0], Tltab->lvp[1],
                                Tltab->lvp[2], Tltab->lvp[3]);
	} else if (cmpncs(member, "worldcoordinate") == 0) {
           return Py_BuildValue("[f,f,f,f]", Tltab->lwc[0], Tltab->lwc[1],
                                Tltab->lwc[2], Tltab->lwc[3]);
	} else if (cmpncs(member, "x") == 0) {
           if (Tltab->lx == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           x=PyList_New(Tltab->lx->nsegs);
           listptr = (PyObject **) malloc(Tltab->lx->nsegs*sizeof(PyObject));
           j = 0;
           aptr = Tltab->lx->ps;
           while (aptr != NULL) {
              listptr[j]=PyList_New(aptr->npts);
              for (i=0; i<(aptr->npts); i++) {
                  PyList_SetItem(listptr[j], i, Py_BuildValue("f", aptr->pts[i])); 
              }
             PyList_SetItem(x, j, listptr[j]); ++j;
             aptr = aptr->next;
           }
           return x;
	} else if (cmpncs(member, "y") == 0) {
           if (Tltab->ly == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           y=PyList_New(Tltab->ly->nsegs);
           listptr = (PyObject **) malloc(Tltab->ly->nsegs*sizeof(PyObject));
           j = 0;
           aptr = Tltab->ly->ps;
           while (aptr != NULL) {
              listptr[j]=PyList_New(aptr->npts);
              for (i=0; i<(aptr->npts); i++) {
                  PyList_SetItem(listptr[j], i, Py_BuildValue("f", aptr->pts[i]));
              }
             PyList_SetItem(y, j, listptr[j]); ++j;
             aptr = aptr->next;
           }
           return y;
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

int store_pts(struct array_segments **lptr, PyObject *VALUE)
   {
      int i;
      struct array_segments *ptr;

      /* Set up the array pointer */
      if ((ptr = (struct array_segments *) malloc(sizeof(
           struct array_segments))) == NULL) {
           PyErr_SetString(VCS_Error, "Error - table entry memory for points not found.\n");
           return 0;
      }

      if (PyList_Check(VALUE))
         ptr->npts = PyList_Size(VALUE);
      else
         ptr->npts = PyTuple_Size(VALUE);
      if ((ptr->pts = (float *) malloc( ptr->npts * sizeof(float))) == NULL) {
           PyErr_SetString(VCS_Error, "Error - table entry memory for points not found.\n");
           return 0;
      }
      ptr->next = NULL;

      for (i = 0 ; i < ptr->npts; i++) {
         if (PyList_Check(VALUE))
            ptr->pts[i] = (float) PyFloat_AsDouble(PyList_GetItem(VALUE, i));
         else
            ptr->pts[i] = (float) PyFloat_AsDouble(PyTuple_GetItem(VALUE, i));
      }

      *lptr = ptr; /* return points */
      return 1;
   }

int store_cpts(struct char_segments **lptr, PyObject *VALUE)
   {
      int i;
      struct char_segments *ptr;

      /* Set up the char pointer */
      if ((ptr = (struct char_segments *) malloc(sizeof(
           struct char_segments))) == NULL) {
           PyErr_SetString(VCS_Error, "Error - table entry memory for strings not found.\n");
           return 0;
      }

      ptr->npts = PyString_Size(VALUE);
      if ((ptr->cpts = (char *) malloc( ptr->npts * sizeof(char)+1)) == NULL) {
           PyErr_SetString(VCS_Error, "Error - table entry memory for strings not found.\n");
           return 0;
      }
      ptr->next = NULL;

      strcpy( ptr->cpts, PyString_AsString(VALUE) );

      *lptr = ptr; /* return strings */
      return 1;
   }


/*
 * Find the existing line object and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the line  object's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setTlmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 				i,j,MODE,npts,value_int;
	float				value_float;
        char 				*Tl_name, *member=NULL;
        PyObject 			*TL=NULL, *MEMBER=NULL, *VALUE=NULL;
        PyObject			*listptr=NULL,*tup;
        struct array_segments		*pts, *tpts;
	struct table_line       	*get_Tltab=NULL;
	extern struct table_line        Tl_tab;
   	extern struct table_line 	*getTl();
	struct table_line    		*Tltab;
	extern int              	update_ind;
	extern int              	chk_mov_Tl();
	extern int 			vcs_legacy_canvas_update();
	extern void			free_points();

        if(PyArg_ParseTuple(args,"|OOOi", &TL, &MEMBER, &VALUE, &MODE)) {
           if (TL == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(TL,"name", "s", &Tl_name);
        Tltab=getTl(Tl_name);

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	/*
	 * Set the appropriate line attribute. But first 
	 * get the line structure.
         */
	get_Tltab = getTl(Tltab->name);
	if (cmpncs(member, "projection") == 0) {
	  strcpy(get_Tltab->proj, PyString_AsString(VALUE));
	}
	else if (cmpncs(member, "type") == 0) {
           if (get_Tltab->ltyp!=NULL) { free((char *) get_Tltab->ltyp); get_Tltab->ltyp=NULL; }
           npts = 1;
           if (PyList_Check(VALUE)) /* check for list */
              npts = PyList_Size(VALUE);

           get_Tltab->ltyp_size = npts;
           if ((get_Tltab->ltyp = (int *) malloc(npts * sizeof(int))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for line type values not found.");
                  return NULL;
           }

           get_Tltab->ltyp[0] = 1; /* set to default solid fillarea */
           if (PyList_Check(VALUE)) { /* check for list */
             for (i=0; i<npts; i++) {
              if (cmpncs("solid", PyString_AsString(PyList_GetItem(VALUE,i))) == 0)
                 get_Tltab->ltyp[i] = 1; 
              else if (cmpncs("dash", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tltab->ltyp[i] = 2;
              else if (cmpncs("dot", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tltab->ltyp[i] = 3;
              else if (cmpncs("dash-dot", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tltab->ltyp[i] = 4;
              else if (cmpncs("long-dash", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tltab->ltyp[i] = -3;
             }
           }
        } else if (cmpncs(member, "color") == 0) {
           if (get_Tltab->lci!=NULL) { free((char *) get_Tltab->lci); get_Tltab->lci=NULL; }
           npts = 1;
           if (PyList_Check(VALUE)) /* check for list */
              npts = PyList_Size(VALUE);

           get_Tltab->lci_size = npts;
           if ((get_Tltab->lci = (int *) malloc(npts * sizeof(int))) == NULL) {
                  PyErr_SetString(VCS_Error,
                 "Error - table entry memory for color values not found.");
                  return NULL;
           }

           get_Tltab->lci[0] = 241; /* set to delult value */
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<npts; i++)
                 get_Tltab->lci[i] = (int) PyInt_AsLong(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "width") == 0) {
           if (get_Tltab->lwsf!=NULL) { free((char *) get_Tltab->lwsf); get_Tltab->lwsf=NULL; }
           npts = 1;
           if (PyList_Check(VALUE)) /* check for list */
              npts = PyList_Size(VALUE);

           get_Tltab->lwsf_size = npts;
           if ((get_Tltab->lwsf = (float *) malloc(npts * sizeof(float))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for line size values not found.");
                  return NULL;
           }

           get_Tltab->lwsf[0] = 1; /* set to delult value */
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<npts; i++)
                 get_Tltab->lwsf[i] = (int) PyInt_AsLong(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "priority") == 0) {
           if (VALUE == Py_None)
              get_Tltab->priority = 1;
           else
             get_Tltab->priority = (int) PyInt_AsLong(VALUE);
        } else if (cmpncs(member, "viewport") == 0) {
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<PyList_Size(VALUE); i++)
                 get_Tltab->lvp[i] = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "worldcoordinate") == 0) {
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<PyList_Size(VALUE); i++)
                 get_Tltab->lwc[i] = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "x") == 0) {
           free_points( &get_Tltab->lx );
           if (PyList_Check(VALUE)) { /* check for list */
              tup = PyList_GetItem(VALUE, 0);
              /* Set up the pointer struct that will point the list of segments */
              if ((get_Tltab->lx =
                  (struct points_struct *) malloc( sizeof(struct points_struct))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for points not found.");
                  return NULL;
              }
              get_Tltab->lx->ps = NULL; 

              if ( (PyList_Check(tup)) || (PyTuple_Check(tup)) ) { /* check for list or tuple */
                 get_Tltab->lx->nsegs = PyList_Size(VALUE);

                 for (i = 0; i < get_Tltab->lx->nsegs; i++) {
/*                    if ((pts = (struct array_segments *) malloc(sizeof(
                        struct array_segments))) == NULL) {
                      PyErr_SetString(VCS_Error,"Error - table entry memory for points not found.");
                      return NULL;
                    }*/

                    store_pts( &pts, PyList_GetItem(VALUE,i) );

                    if (get_Tltab->lx->ps == NULL) {
                       tpts = get_Tltab->lx->ps = pts;
                    } else {
                       tpts->next = pts;
                       tpts = pts;
                    }
                 }
              } else if ((PyInt_Check(tup)) || (PyFloat_Check(tup))) {
                 get_Tltab->lx->nsegs = 1;
                 get_Tltab->lx->ps = NULL; 
                 store_pts( &get_Tltab->lx->ps, VALUE );
              } else {
                 PyErr_SetString(VCS_Error,"Error - Must be a Python List or Tuple.");
                 Py_INCREF(Py_None);
                 return Py_None;
              }
           }
        } else if (cmpncs(member, "y") == 0) {
           free_points( &get_Tltab->ly );
           if (PyList_Check(VALUE)) { /* check for list */
              tup = PyList_GetItem(VALUE, 0);
              /* Set up the pointer struct that will point the list of segments */
              if ((get_Tltab->ly =
                  (struct points_struct *) malloc( sizeof(struct points_struct))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for points not found.");
                  return NULL;
              }
              get_Tltab->ly->ps = NULL;

              if ( (PyList_Check(tup)) || (PyTuple_Check(tup)) ) { /* check for list or tuple */
                 get_Tltab->ly->nsegs = PyList_Size(VALUE);

                 for (i = 0; i < get_Tltab->ly->nsegs; i++) {
/*                    if ((pts = (struct array_segments *) malloc(sizeof(
                        struct array_segments))) == NULL) {
                      PyErr_SetString(VCS_Error,"Error - table entry memory for points not found.");
                      return NULL;
                    }*/

                    store_pts( &pts, PyList_GetItem(VALUE,i) );

                    if (get_Tltab->ly->ps == NULL) {
                       tpts = get_Tltab->ly->ps = pts;
                    } else {
                       tpts->next = pts;
                       tpts = pts;
                    }
                 }
              } else if ((PyInt_Check(tup)) || (PyFloat_Check(tup))) {
                 get_Tltab->ly->nsegs = 1;
                 get_Tltab->ly->ps = NULL;
                 store_pts( &get_Tltab->ly->ps, VALUE );
              } else {
                 PyErr_SetString(VCS_Error,"Error - Must be a Python List or Tuple.");
                 Py_INCREF(Py_None);
                 return Py_None;
              }
           }
        }

	chk_mov_Tl(get_Tltab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new line object method by copying from an existing
 * line object method. If no source copy name argument is given,
 * then the default line object method will be used to replicate
 * the new line object.
 */
static PyObject *
PyVCS_copyTl(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *TL_SRC=NULL, *TL_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Tl_name();
              
        if(PyArg_ParseTuple(args,"|ss", &TL_SRC, &TL_NAME)) {
           if (TL_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source line graphics method name.");
                 return NULL;
           }

           if (TL_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", TL_NAME);
        }

        ierr = copy_Tl_name(TL_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating line graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing line object method.
 */
static PyObject *
PyVCS_renameTl(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *TL_OLD_NAME=NULL, *TL_NEW_NAME=NULL;
        extern int      renameTl_name();
 
        if(PyArg_ParseTuple(args,"|ss", &TL_OLD_NAME, &TL_NEW_NAME)) {
           if ((TL_OLD_NAME == NULL) || (TL_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new line object name.");
                 return NULL;
           }
        }

        ierr = renameTl_name(TL_OLD_NAME, TL_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming line graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing line graphics method.
 */
static PyObject *
PyVCS_removeTl(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the line file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeTl_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed line object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The line object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing line graphics method.
 */
static PyObject *
PyVCS_scriptTl(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
        long loc;
	char *SCRIPT_NAME=NULL, *TL_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_line();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &TL_NAME, &SCRIPT_NAME, &MODE)) {
           if (TL_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the line name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }


        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command line */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
           if (ftell(fp) != 0) fprintf (fp,"\n"); /* Start at the next line down */
	   if (dump_single_line(fp, TL_NAME) == 0) {
              sprintf(buf, "Error - Cannot save line script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS marker (Tm) class member value. 
 */
static PyObject *
PyVCS_getTmmember(PyVCScanvas_Object *self, PyObject *args)
{
        int                             i,j,npts;
	char 				*Tm_name, *member=NULL, buf[1024];
	PyObject 			*TM=NULL, *MEMBER=NULL;
        PyObject                        *x=NULL, *y=NULL, **listptr, *v;
	struct table_mark    		*Tmtab;
        struct array_segments           *aptr;
	extern struct table_mark        Tm_tab;
   	extern struct table_mark 	*getTm();

	if(PyArg_ParseTuple(args,"|OO",&TM, &MEMBER)) {
           if (TM == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}

	Get_Member(TM,"name", "s", &Tm_name);
	Tmtab=getTm(Tm_name);

     	if (Tmtab == NULL) {
	   sprintf(buf,"Cannot find marker class object Tm_%s.",Tm_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) 
	  {
	    return Py_BuildValue("s", Tmtab->proj);
	  }
	else if (cmpncs(member, "type") == 0) {
           if (Tmtab->mtyp == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           npts = Tmtab->mtyp_size;
           v=PyList_New(npts);
           for (i=0; i<npts; i++) {
              if (Tmtab->mtyp[i] == 1)
                 PyList_SetItem(v, i, Py_BuildValue("s", "dot"));
              else if (Tmtab->mtyp[i] == 2)
                 PyList_SetItem(v, i, Py_BuildValue("s", "plus"));
              else if (Tmtab->mtyp[i] == 3)
                 PyList_SetItem(v, i, Py_BuildValue("s", "star"));
              else if (Tmtab->mtyp[i] == 4)
                 PyList_SetItem(v, i, Py_BuildValue("s", "circle"));
              else if (Tmtab->mtyp[i] == 5)
                 PyList_SetItem(v, i, Py_BuildValue("s", "cross"));
              else if (Tmtab->mtyp[i] == 6)
                 PyList_SetItem(v, i, Py_BuildValue("s", "diamond"));
              else if (Tmtab->mtyp[i] == 7)
                 PyList_SetItem(v, i, Py_BuildValue("s", "triangle_up"));
              else if (Tmtab->mtyp[i] == 8)
                 PyList_SetItem(v, i, Py_BuildValue("s", "triangle_down"));
              else if (Tmtab->mtyp[i] == 9)
                 PyList_SetItem(v, i, Py_BuildValue("s", "triangle_left"));
              else if (Tmtab->mtyp[i] == 10)
                 PyList_SetItem(v, i, Py_BuildValue("s", "triangle_right"));
              else if (Tmtab->mtyp[i] == 11)
                 PyList_SetItem(v, i, Py_BuildValue("s", "square"));
              else if (Tmtab->mtyp[i] == 12)
                 PyList_SetItem(v, i, Py_BuildValue("s", "diamond_fill"));
              else if (Tmtab->mtyp[i] == 13)
                 PyList_SetItem(v, i, Py_BuildValue("s", "triangle_up_fill"));
              else if (Tmtab->mtyp[i] == 14)
                 PyList_SetItem(v, i, Py_BuildValue("s", "triangle_down_fill"));
              else if (Tmtab->mtyp[i] == 15)
                 PyList_SetItem(v, i, Py_BuildValue("s", "triangle_left_fill"));
              else if (Tmtab->mtyp[i] == 16)
                 PyList_SetItem(v, i, Py_BuildValue("s", "triangle_right_fill"));
              else if (Tmtab->mtyp[i] == 17)
                 PyList_SetItem(v, i, Py_BuildValue("s", "square_fill"));
              else if (Tmtab->mtyp[i] == 18)
                 PyList_SetItem(v, i, Py_BuildValue("s", "hurricane"));
	      /* Arulalan Weather Markers */
	      else if (Tmtab->mtyp[i] == 100)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w00"));
              else if (Tmtab->mtyp[i] == 101)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w01"));
              else if (Tmtab->mtyp[i] == 102)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w02"));
              else if (Tmtab->mtyp[i] == 103)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w03"));
 	      else if (Tmtab->mtyp[i] == 104)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w04"));
              else if (Tmtab->mtyp[i] == 105)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w05"));
              else if (Tmtab->mtyp[i] == 106)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w06"));
              else if (Tmtab->mtyp[i] == 107)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w07"));
              else if (Tmtab->mtyp[i] == 108)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w08"));
              else if (Tmtab->mtyp[i] == 109)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w09"));
              else if (Tmtab->mtyp[i] == 110)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w10"));
              else if (Tmtab->mtyp[i] == 111)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w11"));
              else if (Tmtab->mtyp[i] == 112)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w12"));
              else if (Tmtab->mtyp[i] == 113)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w13"));
	      else if (Tmtab->mtyp[i] == 114)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w14"));
              else if (Tmtab->mtyp[i] == 115)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w15"));
              else if (Tmtab->mtyp[i] == 116)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w16"));
              else if (Tmtab->mtyp[i] == 117)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w17"));
              else if (Tmtab->mtyp[i] == 118)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w18"));
              else if (Tmtab->mtyp[i] == 119)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w19"));
	      else if (Tmtab->mtyp[i] == 120)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w20"));
              else if (Tmtab->mtyp[i] == 121)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w21"));
              else if (Tmtab->mtyp[i] == 122)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w22"));
              else if (Tmtab->mtyp[i] == 123)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w23"));
	      else if (Tmtab->mtyp[i] == 124)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w24"));
              else if (Tmtab->mtyp[i] == 125)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w25"));
              else if (Tmtab->mtyp[i] == 126)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w26"));
              else if (Tmtab->mtyp[i] == 127)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w27"));
              else if (Tmtab->mtyp[i] == 128)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w28"));
              else if (Tmtab->mtyp[i] == 129)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w29"));
	      else if (Tmtab->mtyp[i] == 130)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w30"));
              else if (Tmtab->mtyp[i] == 131)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w31"));
              else if (Tmtab->mtyp[i] == 132)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w32"));
              else if (Tmtab->mtyp[i] == 133)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w33"));
	      else if (Tmtab->mtyp[i] == 134)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w34"));
              else if (Tmtab->mtyp[i] == 135)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w35"));
              else if (Tmtab->mtyp[i] == 136)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w36"));
              else if (Tmtab->mtyp[i] == 137)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w37"));
              else if (Tmtab->mtyp[i] == 138)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w38"));
              else if (Tmtab->mtyp[i] == 139)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w39"));
	      else if (Tmtab->mtyp[i] == 140)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w40"));
              else if (Tmtab->mtyp[i] == 141)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w41"));
              else if (Tmtab->mtyp[i] == 142)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w42"));
              else if (Tmtab->mtyp[i] == 143)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w43"));
	      else if (Tmtab->mtyp[i] == 144)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w44"));
              else if (Tmtab->mtyp[i] == 145)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w45"));
              else if (Tmtab->mtyp[i] == 146)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w46"));
              else if (Tmtab->mtyp[i] == 147)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w47"));
              else if (Tmtab->mtyp[i] == 148)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w48"));
              else if (Tmtab->mtyp[i] == 149)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w49"));
	      else if (Tmtab->mtyp[i] == 150)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w50"));
              else if (Tmtab->mtyp[i] == 151)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w51"));
              else if (Tmtab->mtyp[i] == 152)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w52"));
              else if (Tmtab->mtyp[i] == 153)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w53"));
	      else if (Tmtab->mtyp[i] == 154)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w54"));
              else if (Tmtab->mtyp[i] == 155)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w55"));
              else if (Tmtab->mtyp[i] == 156)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w56"));
              else if (Tmtab->mtyp[i] == 157)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w57"));
              else if (Tmtab->mtyp[i] == 158)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w58"));
              else if (Tmtab->mtyp[i] == 159)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w59"));
	      else if (Tmtab->mtyp[i] == 160)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w60"));
              else if (Tmtab->mtyp[i] == 161)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w61"));
              else if (Tmtab->mtyp[i] == 162)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w62"));
              else if (Tmtab->mtyp[i] == 163)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w63"));
	      else if (Tmtab->mtyp[i] == 164)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w64"));
              else if (Tmtab->mtyp[i] == 165)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w65"));
              else if (Tmtab->mtyp[i] == 166)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w66"));
              else if (Tmtab->mtyp[i] == 167)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w67"));
              else if (Tmtab->mtyp[i] == 168)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w68"));
              else if (Tmtab->mtyp[i] == 169)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w69"));
	      else if (Tmtab->mtyp[i] == 170)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w70"));
              else if (Tmtab->mtyp[i] == 171)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w71"));
              else if (Tmtab->mtyp[i] == 172)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w72"));
              else if (Tmtab->mtyp[i] == 173)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w73"));
	      else if (Tmtab->mtyp[i] == 174)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w74"));
              else if (Tmtab->mtyp[i] == 175)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w75"));
              else if (Tmtab->mtyp[i] == 176)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w76"));
              else if (Tmtab->mtyp[i] == 177)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w77"));
              else if (Tmtab->mtyp[i] == 178)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w78"));
              else if (Tmtab->mtyp[i] == 179)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w79"));
	      else if (Tmtab->mtyp[i] == 180)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w80"));
              else if (Tmtab->mtyp[i] == 181)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w81"));
              else if (Tmtab->mtyp[i] == 182)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w82"));
              else if (Tmtab->mtyp[i] == 183)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w83"));
	      else if (Tmtab->mtyp[i] == 184)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w84"));
              else if (Tmtab->mtyp[i] == 185)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w85"));
              else if (Tmtab->mtyp[i] == 186)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w86"));
              else if (Tmtab->mtyp[i] == 187)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w87"));
              else if (Tmtab->mtyp[i] == 188)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w88"));
              else if (Tmtab->mtyp[i] == 189)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w89"));
	      else if (Tmtab->mtyp[i] == 190)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w90"));
              else if (Tmtab->mtyp[i] == 191)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w91"));
              else if (Tmtab->mtyp[i] == 192)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w92"));
              else if (Tmtab->mtyp[i] == 193)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w93"));
	      else if (Tmtab->mtyp[i] == 194)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w94"));
              else if (Tmtab->mtyp[i] == 195)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w95"));
              else if (Tmtab->mtyp[i] == 196)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w96"));
              else if (Tmtab->mtyp[i] == 197)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w97"));
              else if (Tmtab->mtyp[i] == 198)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w98"));
              else if (Tmtab->mtyp[i] == 199)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w99"));
	      else if (Tmtab->mtyp[i] == 200)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w200"));
              else if (Tmtab->mtyp[i] == 201)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w201"));
              else if (Tmtab->mtyp[i] == 202)
                 PyList_SetItem(v, i, Py_BuildValue("s", "w202"));
           }
           return v;
	} else if (cmpncs(member, "size") == 0) {
           if (Tmtab->msize == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           npts = Tmtab->msize_size;
           v=PyList_New(npts);
           for (i=0; i<npts; i++)
              PyList_SetItem(v, i, Py_BuildValue("d", *(Tmtab->msize)));
           return v;
	} else if (cmpncs(member, "color") == 0) {
           if (Tmtab->mci == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           npts = Tmtab->mci_size;
           v=PyList_New(npts);
           for (i=0; i<npts; i++)
              PyList_SetItem(v, i, Py_BuildValue("i", *(Tmtab->mci)));
           return v;
        } else if (cmpncs(member, "priority") == 0) {
           return Py_BuildValue("i", Tmtab->priority);
        } else if (cmpncs(member, "viewport") == 0) {
           return Py_BuildValue("[f,f,f,f]", Tmtab->mvp[0], Tmtab->mvp[1],
                                Tmtab->mvp[2], Tmtab->mvp[3]);
        } else if (cmpncs(member, "worldcoordinate") == 0) {
           return Py_BuildValue("[f,f,f,f]", Tmtab->mwc[0], Tmtab->mwc[1],
                                Tmtab->mwc[2], Tmtab->mwc[3]);
        } else if (cmpncs(member, "x") == 0) {
           if (Tmtab->mx == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           x=PyList_New(Tmtab->mx->nsegs);
           listptr = (PyObject **) malloc(Tmtab->mx->nsegs*sizeof(PyObject));
           j = 0;
           aptr = Tmtab->mx->ps;
           while (aptr != NULL) {
              listptr[j]=PyList_New(aptr->npts);
              for (i=0; i<(aptr->npts); i++) {
                  PyList_SetItem(listptr[j], i, Py_BuildValue("f", aptr->pts[i]));
              }
             PyList_SetItem(x, j, listptr[j]); ++j;
             aptr = aptr->next;
           }
           return x;
        } else if (cmpncs(member, "y") == 0) {
           if (Tmtab->my == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           y=PyList_New(Tmtab->my->nsegs);
           listptr = (PyObject **) malloc(Tmtab->my->nsegs*sizeof(PyObject));
           j = 0;
           aptr = Tmtab->my->ps;
           while (aptr != NULL) {
              listptr[j]=PyList_New(aptr->npts);
              for (i=0; i<(aptr->npts); i++) {
                  PyList_SetItem(listptr[j], i, Py_BuildValue("f", aptr->pts[i]));
              }
             PyList_SetItem(y, j, listptr[j]); ++j;
             aptr = aptr->next;
           }
           return y;
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing marker object and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the marker  object's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setTmmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 				i,j,MODE,npts,value_int;
	float				value_float;
        char 				*Tm_name, *member=NULL;
        PyObject 			*TM=NULL, *MEMBER=NULL, *VALUE=NULL;
        PyObject			*tup;
        struct array_segments		*pts, *tpts;
	struct table_mark       	*get_Tmtab=NULL;
	extern struct table_mark        Tm_tab;
   	extern struct table_mark 	*getTm();
	struct table_mark    		*Tmtab;
	extern int              	update_ind;
	extern int              	chk_mov_Tm();
	extern int 			vcs_legacy_canvas_update();
        extern void                     free_points();

        if(PyArg_ParseTuple(args,"|OOOi", &TM, &MEMBER, &VALUE, &MODE)) {
           if (TM == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(TM,"name", "s", &Tm_name);
        Tmtab=getTm(Tm_name);

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	/*
	 * Set the appropriate marker attribute. But first 
	 * get the marker structure.
         */
	get_Tmtab = getTm(Tmtab->name);
	if (cmpncs(member, "projection") == 0) {
	  strcpy(get_Tmtab->proj, PyString_AsString(VALUE));
	}
	else if (cmpncs(member, "type") == 0) {
           if (get_Tmtab->mtyp!=NULL) { free((char *) get_Tmtab->mtyp); get_Tmtab->mtyp=NULL; }
           npts = 1;
           if (PyList_Check(VALUE)) /* check for list */
              npts = PyList_Size(VALUE);

           get_Tmtab->mtyp_size = npts;
           if ((get_Tmtab->mtyp = (int *) malloc(npts * sizeof(int))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for marker values not found.");
                  return NULL;
           }

           get_Tmtab->mtyp[0] = 1; /* set to default dot marker */
           if (PyList_Check(VALUE)) { /* check for list */
             for (i=0; i<npts; i++) {
              if (cmpncs("dot", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 1; 
              else if (cmpncs("plus", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 2;
              else if (cmpncs("star", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 3;
              else if (cmpncs("circle", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 4;
              else if (cmpncs("cross", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 5;
              else if (cmpncs("diamond", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 6;
              else if (cmpncs("triangle_up", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 7;
              else if (cmpncs("triangle_down", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 8;
              else if (cmpncs("triangle_left", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 9;
              else if (cmpncs("triangle_right", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 10;
              else if (cmpncs("square", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 11;
              else if (cmpncs("diamond_fill", PyString_AsString(PyList_GetItem(VALUE,i)))==0) 
                 get_Tmtab->mtyp[i] = 12;
              else if (cmpncs("triangle_up_fill", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 13;
              else if (cmpncs("triangle_down_fill", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 14;
              else if (cmpncs("triangle_left_fill", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 15;
              else if (cmpncs("triangle_right_fill", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 16;
              else if (cmpncs("square_fill", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 17;
              else if (cmpncs("hurricane", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 18;
	      /* Arulalan Weather Markers */
	      else if (cmpncs("w00", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 100;
              else if (cmpncs("w01", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 101;
              else if (cmpncs("w02", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 102;
              else if (cmpncs("w03", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 103;
 	      else if (cmpncs("w04", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 104;
              else if (cmpncs("w05", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 105;
              else if (cmpncs("w06", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 106;
              else if (cmpncs("w07", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 107;
              else if (cmpncs("w08", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 108;
              else if (cmpncs("w09", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 109;
              else if (cmpncs("w10", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 110;
              else if (cmpncs("w11", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 111;
              else if (cmpncs("w12", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 112;
              else if (cmpncs("w13", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 113;
	      else if (cmpncs("w14", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 114;
              else if (cmpncs("w15", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 115;
              else if (cmpncs("w16", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 116;
              else if (cmpncs("w17", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 117;
              else if (cmpncs("w18", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 118;
              else if (cmpncs("w19", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 119;
	      else if (cmpncs("w20", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 120;
              else if (cmpncs("w21", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 121;
              else if (cmpncs("w22", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 122;
              else if (cmpncs("w23", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 123;
	      else if (cmpncs("w24", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 124;
              else if (cmpncs("w25", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 125;
              else if (cmpncs("w26", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 126;
              else if (cmpncs("w27", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 127;
              else if (cmpncs("w28", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 128;
              else if (cmpncs("w29", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 129;
	      else if (cmpncs("w30", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 130;
              else if (cmpncs("w31", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 131;
              else if (cmpncs("w32", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 132;
              else if (cmpncs("w33", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 133;
	      else if (cmpncs("w34", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 134;
              else if (cmpncs("w35", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 135;
              else if (cmpncs("w36", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 136;
              else if (cmpncs("w37", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 137;
              else if (cmpncs("w38", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 138;
              else if (cmpncs("w39", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 139;
	      else if (cmpncs("w40", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 140;
              else if (cmpncs("w41", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 141;
              else if (cmpncs("w42", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 142;
              else if (cmpncs("w43", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 143;
	      else if (cmpncs("w44", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 144;
              else if (cmpncs("w45", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 145;
              else if (cmpncs("w46", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 146;
              else if (cmpncs("w47", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 147;
              else if (cmpncs("w48", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 148;
              else if (cmpncs("w49", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 149;
	      else if (cmpncs("w50", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 150;
              else if (cmpncs("w51", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 151;
              else if (cmpncs("w52", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 152;
              else if (cmpncs("w53", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 153;
	      else if (cmpncs("w54", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 154;
              else if (cmpncs("w55", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 155;
              else if (cmpncs("w56", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 156;
              else if (cmpncs("w57", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 157;
              else if (cmpncs("w58", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 158;
              else if (cmpncs("w59", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 159;
	      else if (cmpncs("w60", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 160;
              else if (cmpncs("w61", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 161;
              else if (cmpncs("w62", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 162;
              else if (cmpncs("w63", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 163;
	      else if (cmpncs("w64", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 164;
              else if (cmpncs("w65", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 165;
              else if (cmpncs("w66", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 166;
              else if (cmpncs("w67", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 167;
              else if (cmpncs("w68", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 168;
              else if (cmpncs("w69", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 169;
	      else if (cmpncs("w70", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 170;
              else if (cmpncs("w71", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 171;
              else if (cmpncs("w72", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 172;
              else if (cmpncs("w73", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 173;
	      else if (cmpncs("w74", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 174;
              else if (cmpncs("w75", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 175;
              else if (cmpncs("w76", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 176;
              else if (cmpncs("w77", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 177;
              else if (cmpncs("w78", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 178;
              else if (cmpncs("w79", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 179;
	      else if (cmpncs("w80", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 180;
              else if (cmpncs("w81", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 181;
              else if (cmpncs("w82", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 182;
              else if (cmpncs("w83", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 183;
	      else if (cmpncs("w84", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 184;
              else if (cmpncs("w85", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 185;
              else if (cmpncs("w86", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 186;
              else if (cmpncs("w87", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 187;
              else if (cmpncs("w88", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 188;
              else if (cmpncs("w89", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 189;
	      else if (cmpncs("w90", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 190;
              else if (cmpncs("w91", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 191;
              else if (cmpncs("w92", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 192;
              else if (cmpncs("w93", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 193;
	      else if (cmpncs("w94", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 194;
              else if (cmpncs("w95", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 195;
              else if (cmpncs("w96", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 196;
              else if (cmpncs("w97", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 197;
              else if (cmpncs("w98", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 198;
              else if (cmpncs("w99", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 199;
	      else if (cmpncs("w200", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 200;
              else if (cmpncs("w201", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 201;
              else if (cmpncs("w202", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                 get_Tmtab->mtyp[i] = 202;
             }
           }
        } else if (cmpncs(member, "color") == 0) {
           if (get_Tmtab->mci!=NULL) { free((char *) get_Tmtab->mci); get_Tmtab->mci=NULL; }
           npts = 1;
           if (PyList_Check(VALUE)) /* check for list */
              npts = PyList_Size(VALUE);

           get_Tmtab->mci_size = npts;
           if ((get_Tmtab->mci = (int *) malloc(npts * sizeof(int))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for marker color values not found.");
                  return NULL;
           }

           get_Tmtab->mci[0] = 241; /* set to default value */
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<npts; i++)
                 get_Tmtab->mci[i] = (int) PyInt_AsLong(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "size") == 0) {
           if (get_Tmtab->msize!=NULL) { free((char *) get_Tmtab->msize); get_Tmtab->msize=NULL; }
           npts = 1;
           if (PyList_Check(VALUE)) /* check for list */
              npts = PyList_Size(VALUE);

           get_Tmtab->msize_size = npts;
           if ((get_Tmtab->msize = (float *) malloc(npts * sizeof(float))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for marker size values not found.");
                  return NULL;
           }

           get_Tmtab->msize[0] = 1; /* set to default value */
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<npts; i++)
                 get_Tmtab->msize[i] = (int) PyInt_AsLong(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "priority") == 0) {
           if (VALUE == Py_None)
              get_Tmtab->priority = 1;
           else
              get_Tmtab->priority = (int) PyInt_AsLong(VALUE);
        } else if (cmpncs(member, "viewport") == 0) {
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<PyList_Size(VALUE); i++)
                 get_Tmtab->mvp[i] = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "worldcoordinate") == 0) {
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<PyList_Size(VALUE); i++)
                 get_Tmtab->mwc[i] = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "x") == 0) {
           free_points( &get_Tmtab->mx );
           if (PyList_Check(VALUE)) { /* check for list */
              tup = PyList_GetItem(VALUE, 0);
              /* Set up the pointer struct that will point the list of segments */
              if ((get_Tmtab->mx =
                  (struct points_struct *) malloc( sizeof(struct points_struct))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for points not found.");
                  return NULL;
              }
              get_Tmtab->mx->ps = NULL;

              if ( (PyList_Check(tup)) || (PyTuple_Check(tup)) ) { /* check for list or tuple */
                 get_Tmtab->mx->nsegs = PyList_Size(VALUE);

                 for (i = 0; i < get_Tmtab->mx->nsegs; i++) {
/*                    if ((pts = (struct array_segments *) malloc(sizeof(
                        struct array_segments))) == NULL) {
                      PyErr_SetString(VCS_Error,"Error - table entry memory for points not found. ");
                      return NULL;
                    }*/

                    store_pts( &pts, PyList_GetItem(VALUE,i) );

                    if (get_Tmtab->mx->ps == NULL) {
                       tpts = get_Tmtab->mx->ps = pts;
                    } else {
                       tpts->next = pts;
                       tpts = pts;
                    }
                 }
              } else if ((PyInt_Check(tup)) || (PyFloat_Check(tup))) {
                 get_Tmtab->mx->nsegs = 1;
                 get_Tmtab->mx->ps = NULL;
                 store_pts( &get_Tmtab->mx->ps, VALUE );
              } else {
                 PyErr_SetString(VCS_Error,"Error - Must be a Python List or Tuple.");
                 Py_INCREF(Py_None);
                 return Py_None;
              }
           }
        } else if (cmpncs(member, "y") == 0) {
           free_points( &get_Tmtab->my );
           if (PyList_Check(VALUE)) { /* check for list */
              tup = PyList_GetItem(VALUE, 0);
              /* Set up the pointer struct that will point the list of segments */
              if ((get_Tmtab->my =
                  (struct points_struct *) malloc( sizeof(struct points_struct))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for points not found.");
                  return NULL;
              }
              get_Tmtab->my->ps = NULL;

              if ( (PyList_Check(tup)) || (PyTuple_Check(tup)) ) { /* check for list or tuple */
                 get_Tmtab->my->nsegs = PyList_Size(VALUE);

                 for (i = 0; i < get_Tmtab->my->nsegs; i++) {
/*                    if ((pts = (struct array_segments *) malloc(sizeof(
                        struct array_segments))) == NULL) {
                      PyErr_SetString(VCS_Error,"Error - table entry memory for points not found. ");
                      return NULL;
                    }*/

                    store_pts( &pts, PyList_GetItem(VALUE,i) );

                    if (get_Tmtab->my->ps == NULL) {
                       tpts = get_Tmtab->my->ps = pts;
                    } else {
                       tpts->next = pts;
                       tpts = pts;
                    }
                 }
              } else if ((PyInt_Check(tup)) || (PyFloat_Check(tup))) {
                 get_Tmtab->my->nsegs = 1;
                 get_Tmtab->my->ps = NULL;
                 store_pts( &get_Tmtab->my->ps, VALUE );
              } else {
                 PyErr_SetString(VCS_Error,"Error - Must be a Python List or Tuple.");
                 Py_INCREF(Py_None);
                 return Py_None;
              }
           }
        }

	chk_mov_Tm(get_Tmtab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new marker object method by copying from an existing
 * marker object method. If no source copy name argument is given,
 * then the default marker object method will be used to replicate
 * the new marker object.
 */
static PyObject *
PyVCS_copyTm(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *TM_SRC=NULL, *TM_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Tm_name();
              
        if(PyArg_ParseTuple(args,"|ss", &TM_SRC, &TM_NAME)) {
           if (TM_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source marker graphics method name.");
                 return NULL;
           }

           if (TM_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", TM_NAME);
        }

        ierr = copy_Tm_name(TM_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating marker graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing marker object method.
 */
static PyObject *
PyVCS_renameTm(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *TM_OLD_NAME=NULL, *TM_NEW_NAME=NULL;
        extern int      renameTm_name();
 
        if(PyArg_ParseTuple(args,"|ss", &TM_OLD_NAME, &TM_NEW_NAME)) {
           if ((TM_OLD_NAME == NULL) || (TM_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new marker object name.");
                 return NULL;
           }
        }

        ierr = renameTm_name(TM_OLD_NAME, TM_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming marker graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing marker graphics method.
 */
static PyObject *
PyVCS_removeTm(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the marker file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeTm_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed marker object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The marker object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing marker graphics method.
 */
static PyObject *
PyVCS_scriptTm(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *TM_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_marker();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &TM_NAME, &SCRIPT_NAME, &MODE)) {
           if (TM_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the marker name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command marker */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_marker(fp, TM_NAME) == 0) {
              sprintf(buf, "Error - Cannot save marker script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS fillarea (Tf) class member value. 
 */
static PyObject *
PyVCS_getTfmember(PyVCScanvas_Object *self, PyObject *args)
{
        int                             i,j,npts;
	char 				*Tf_name, *member=NULL, buf[1024];
	PyObject 			*TF=NULL, *MEMBER=NULL;
        PyObject                        *x=NULL, *y=NULL, **listptr, *v;
	struct table_fill    		*Tftab;
        struct array_segments           *aptr;
	extern struct table_fill        Tf_tab;
   	extern struct table_fill 	*getTf();

	if(PyArg_ParseTuple(args,"|OO",&TF, &MEMBER)) {
           if (TF == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}

	Get_Member(TF,"name", "s", &Tf_name);
	Tftab=getTf(Tf_name);

     	if (Tftab == NULL) {
	   sprintf(buf,"Cannot find fillarea class object Tf_%s.",Tf_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) 
	  {
	    return Py_BuildValue("s", Tftab->proj);
	  }
	else if (cmpncs(member, "style") == 0) {
           if (Tftab->fais == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           npts = Tftab->fais_size;
           v=PyList_New(npts);
           for (i=0; i<npts; i++) {
              if (Tftab->fais[i] == 1)
                PyList_SetItem(v, i, Py_BuildValue("s", "solid"));
              else if (Tftab->fais[i] == 2)
                PyList_SetItem(v, i, Py_BuildValue("s", "pattern"));
              else if (Tftab->fais[i] == 3)
                PyList_SetItem(v, i, Py_BuildValue("s", "hatch"));
           }
           return v;
	} else if (cmpncs(member, "index") == 0) {
           if (Tftab->fasi == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           npts = Tftab->fasi_size;
           v=PyList_New(npts);
           for (i=0; i<npts; i++)
              PyList_SetItem(v, i, Py_BuildValue("i", Tftab->fasi[i]));
           return v;
	} else if (cmpncs(member, "color") == 0) {
           if (Tftab->faci == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           npts = Tftab->faci_size;
           v=PyList_New(npts);
           for (i=0; i<npts; i++)
              PyList_SetItem(v, i, Py_BuildValue("i", Tftab->faci[i]));
           return v;
        } else if (cmpncs(member, "priority") == 0) {
           return Py_BuildValue("i", Tftab->priority);
        } else if (cmpncs(member, "viewport") == 0) {
           return Py_BuildValue("[f,f,f,f]", Tftab->fvp[0], Tftab->fvp[1],
                                Tftab->fvp[2], Tftab->fvp[3]);
        } else if (cmpncs(member, "worldcoordinate") == 0) {
           return Py_BuildValue("[f,f,f,f]", Tftab->fwc[0], Tftab->fwc[1],
                                Tftab->fwc[2], Tftab->fwc[3]);
        } else if (cmpncs(member, "x") == 0) {
           if (Tftab->fx == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           x=PyList_New(Tftab->fx->nsegs);
           listptr = (PyObject **) malloc(Tftab->fx->nsegs*sizeof(PyObject));
           j = 0;
           aptr = Tftab->fx->ps;
           while (aptr != NULL) {
              listptr[j]=PyList_New(aptr->npts);
              for (i=0; i<(aptr->npts); i++) {
                  PyList_SetItem(listptr[j], i, Py_BuildValue("f", aptr->pts[i]));
              }
             PyList_SetItem(x, j, listptr[j]); ++j;
             aptr = aptr->next;
           }
           return x;
        } else if (cmpncs(member, "y") == 0) {
           if (Tftab->fy == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           y=PyList_New(Tftab->fy->nsegs);
           listptr = (PyObject **) malloc(Tftab->fy->nsegs*sizeof(PyObject));
           j = 0;
           aptr = Tftab->fy->ps;
           while (aptr != NULL) {
              listptr[j]=PyList_New(aptr->npts);
              for (i=0; i<(aptr->npts); i++) {
                  PyList_SetItem(listptr[j], i, Py_BuildValue("f", aptr->pts[i]));
              }
             PyList_SetItem(y, j, listptr[j]); ++j;
             aptr = aptr->next;
           }
           return y;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing fillarea object and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the fillarea  object's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setTfmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 				i,j,MODE,npts,value_int;
	float				value_float;
        char 				*Tf_name, *member=NULL;
        PyObject 			*TF=NULL, *MEMBER=NULL, *VALUE=NULL;
        PyObject                        *tup;
        struct array_segments           *pts, *tpts;
	struct table_fill       	*get_Tftab=NULL;
	extern struct table_fill        Tf_tab;
   	extern struct table_fill 	*getTf();
	struct table_fill    		*Tftab;
	extern int              	update_ind;
	extern int              	chk_mov_Tf();
	extern int 			vcs_legacy_canvas_update();
        extern void                     free_points();

        if(PyArg_ParseTuple(args,"|OOOi", &TF, &MEMBER, &VALUE, &MODE)) {
           if (TF == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(TF,"name", "s", &Tf_name);
        Tftab=getTf(Tf_name);

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	/*
	 * Set the appropriate fillarea attribute. But first 
	 * get the fillarea structure.
         */
	get_Tftab = getTf(Tftab->name);
	if (cmpncs(member, "projection") == 0) {
	  strcpy(get_Tftab->proj, PyString_AsString(VALUE));
	}
	else if (cmpncs(member, "style") == 0) {
           if (get_Tftab->fais!=NULL) { free((char *) get_Tftab->fais); get_Tftab->fais=NULL; }
           npts = 1;
           if (PyList_Check(VALUE)) /* check for list */
              npts = PyList_Size(VALUE);

           get_Tftab->fais_size = npts;
           if ((get_Tftab->fais = (int *) malloc(npts * sizeof(int))) == NULL) {
                  PyErr_SetString(VCS_Error,
                 "Error - table entry memory for color values not found.");
                  return NULL;
           }

           get_Tftab->fais[0] = 1; /* set to default solid fillarea */
           if (PyList_Check(VALUE)) { /* check for list */
             for (i=0; i<npts; i++) {
               if (cmpncs("solid", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                  get_Tftab->fais[i] = 1;
               else if (cmpncs("pattern", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                  get_Tftab->fais[i] = 2;
               else if (cmpncs("hatch", PyString_AsString(PyList_GetItem(VALUE,i)))==0)
                  get_Tftab->fais[i] = 3;
             }
           }
        } else if (cmpncs(member, "color") == 0) {
           if (get_Tftab->faci!=NULL) { free((char *) get_Tftab->faci); get_Tftab->faci=NULL; }
           npts = 1;
           if (PyList_Check(VALUE)) /* check for list */
              npts = PyList_Size(VALUE);

           get_Tftab->faci_size = npts;
           if ((get_Tftab->faci = (int *) malloc(npts * sizeof(int))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for color values not found.");
                  return NULL;
           }

           get_Tftab->faci[0] = 241; /* set to default value */
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<npts; i++)
                 get_Tftab->faci[i] = (int) PyInt_AsLong(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "index") == 0) {
           if (get_Tftab->fasi!=NULL) { free((char *) get_Tftab->fasi); get_Tftab->fasi=NULL; }
           npts = 1;
           if (PyList_Check(VALUE)) /* check for list */
              npts = PyList_Size(VALUE);

           get_Tftab->fasi_size = npts;
           if ((get_Tftab->fasi = (int *) malloc(npts * sizeof(int))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for color values not found.");
                  return NULL;
           }

           get_Tftab->fasi[0] = 1; /* set to default value */
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<npts; i++)
                 get_Tftab->fasi[i] = (int) PyInt_AsLong(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "priority") == 0) {
           if (VALUE == Py_None)
              get_Tftab->priority = 1;
           else
              get_Tftab->priority = (int) PyInt_AsLong(VALUE);
        } else if (cmpncs(member, "viewport") == 0) {
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<PyList_Size(VALUE); i++)
                 get_Tftab->fvp[i] = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "worldcoordinate") == 0) {
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<PyList_Size(VALUE); i++)
                 get_Tftab->fwc[i] = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "x") == 0) {
           free_points( &get_Tftab->fx );
           if (PyList_Check(VALUE)) { /* check for list */
              tup = PyList_GetItem(VALUE, 0);
              /* Set up the pointer struct that will point the list of segments */
              if ((get_Tftab->fx =
                  (struct points_struct *) malloc( sizeof(struct points_struct))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for points not found.");
                  return NULL;
              }
              get_Tftab->fx->ps = NULL;

              if ( (PyList_Check(tup)) || (PyTuple_Check(tup)) ) { /* check for list or tuple */
                 get_Tftab->fx->nsegs = PyList_Size(VALUE);

                 for (i = 0; i < get_Tftab->fx->nsegs; i++) {
/*                    if ((pts = (struct array_segments *) malloc(sizeof(
                        struct array_segments))) == NULL) {
                      PyErr_SetString(VCS_Error,"Error - table entry memory for points not found. ");
                      return NULL;
                    }*/

                    store_pts( &pts, PyList_GetItem(VALUE,i) );

                    if (get_Tftab->fx->ps == NULL) {
                       tpts = get_Tftab->fx->ps = pts;
                    } else {
                       tpts->next = pts;
                       tpts = pts;
                    }
                 }
              } else if ((PyInt_Check(tup)) || (PyFloat_Check(tup))) {
                 get_Tftab->fx->nsegs = 1;
                 get_Tftab->fx->ps = NULL;
                 store_pts( &get_Tftab->fx->ps, VALUE );
              } else {
                 PyErr_SetString(VCS_Error,"Error - Must be a Python List or Tuple.");
                 Py_INCREF(Py_None);
                 return Py_None;
              }
           }
        } else if (cmpncs(member, "y") == 0) {
           free_points( &get_Tftab->fy );
           if (PyList_Check(VALUE)) { /* check for list */
              tup = PyList_GetItem(VALUE, 0);
              /* Set up the pointer struct that will point the list of segments */
              if ((get_Tftab->fy =
                  (struct points_struct *) malloc( sizeof(struct points_struct))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for points not found.");
                  return NULL;
              }
              get_Tftab->fy->ps = NULL;

              if ( (PyList_Check(tup)) || (PyTuple_Check(tup)) ) { /* check for list or tuple */
                 get_Tftab->fy->nsegs = PyList_Size(VALUE);

                 for (i = 0; i < get_Tftab->fy->nsegs; i++) {
/*                    if ((pts = (struct array_segments *) malloc(sizeof(
                        struct array_segments))) == NULL) {
                      PyErr_SetString(VCS_Error,"Error - table entry memory for points not found. ");
                      return NULL;
                    }*/

                    store_pts( &pts, PyList_GetItem(VALUE,i) );

                    if (get_Tftab->fy->ps == NULL) {
                       tpts = get_Tftab->fy->ps = pts;
                    } else {
                       tpts->next = pts;
                       tpts = pts;
                    }
                 }
              } else if ((PyInt_Check(tup)) || (PyFloat_Check(tup))) {
                 get_Tftab->fy->nsegs = 1;
                 get_Tftab->fy->ps = NULL;
                 store_pts( &get_Tftab->fy->ps, VALUE );
              } else {
                 PyErr_SetString(VCS_Error,"Error - Must be a Python List or Tuple.");
                 Py_INCREF(Py_None);
                 return Py_None;
              }
           }
        }


	chk_mov_Tf(get_Tftab);
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new fillarea object method by copying from an existing
 * fillarea object method. If no source copy name argument is given,
 * then the default fillarea object method will be used to replicate
 * the new fillarea object.
 */
static PyObject *
PyVCS_copyTf(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *TF_SRC=NULL, *TF_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Tf_name();
              
        if(PyArg_ParseTuple(args,"|ss", &TF_SRC, &TF_NAME)) {
           if (TF_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source fillarea graphics method name.");
                 return NULL;
           }

           if (TF_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", TF_NAME);
        }

        ierr = copy_Tf_name(TF_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating fillarea graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing fillarea object method.
 */
static PyObject *
PyVCS_renameTf(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *TF_OLD_NAME=NULL, *TF_NEW_NAME=NULL;
        extern int      renameTf_name();
 
        if(PyArg_ParseTuple(args,"|ss", &TF_OLD_NAME, &TF_NEW_NAME)) {
           if ((TF_OLD_NAME == NULL) || (TF_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new fillarea object name.");
                 return NULL;
           }
        }

        ierr = renameTf_name(TF_OLD_NAME, TF_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming fillarea graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing fillarea graphics method.
 */
static PyObject *
PyVCS_removeTf(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the fillarea file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeTf_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed fillarea object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The fillarea object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing fillarea graphics method.
 */
static PyObject *
PyVCS_scriptTf(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *TF_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
	char mode2[2];
	extern int dump_single_fillarea();
	FILE *fp;


        if(PyArg_ParseTuple(args,"|sss", &TF_NAME, &SCRIPT_NAME, &MODE)) {
           if (TF_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the fillarea name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command fillarea */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_fillarea(fp, TF_NAME) == 0) {
              sprintf(buf, "Error - Cannot save fillarea script to output file - %s.",
                   SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS text table (Tt) class member value. 
 */
static PyObject *
PyVCS_getTtmember(PyVCScanvas_Object *self, PyObject *args)
{
        int                             i,j;
	char 				*Tt_name, *member=NULL, buf[1024];
	PyObject 			*TT=NULL, *MEMBER=NULL;
        PyObject                        *s=NULL, *x=NULL, *y=NULL, **listptr;
	struct table_text    		*Tttab;
        struct char_segments            *sptr;
        struct array_segments           *aptr;
	extern struct table_text        Tt_tab;
   	extern struct table_text 	*getTt();

	if(PyArg_ParseTuple(args,"|OO",&TT, &MEMBER)) {
           if (TT == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}

	Get_Member(TT,"name", "s", &Tt_name);
	Tttab=getTt(Tt_name);

     	if (Tttab == NULL) {
	   sprintf(buf,"Cannot find text table class object Tt_%s.",Tt_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "projection") == 0) 
	  {
	    return Py_BuildValue("s", Tttab->proj);
	  }
	else if (cmpncs(member, "string") == 0) {
           if (Tttab->ts == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           s=PyList_New(Tttab->ts->nsegs);
           listptr = (PyObject **) malloc(Tttab->ts->nsegs*sizeof(PyObject));
           j = 0;
           sptr = Tttab->ts->ss;
           while (sptr != NULL) {
              listptr[j]=PyList_New(1);
              PyList_SetItem(s, j, Py_BuildValue("s", sptr->cpts));
              ++j;
              sptr = sptr->next;
           }
           return s;
	}else if (cmpncs(member, "font") == 0) {
           return Py_BuildValue("i", (int) Tttab->txfont);
	} else if (cmpncs(member, "spacing") == 0) {
           return Py_BuildValue("i", (int) (Tttab->txsp*10));
	} else if (cmpncs(member, "expansion") == 0) {
           return Py_BuildValue("i", (int) (Tttab->txexp*100));
	} else if (cmpncs(member, "color") == 0) {
           return Py_BuildValue("i", Tttab->txci);
	} else if (cmpncs(member, "fillincolor") == 0) {
           return Py_BuildValue("i", Tttab->txfci);
        } else if (cmpncs(member, "priority") == 0) {
           return Py_BuildValue("i", Tttab->priority);
        } else if (cmpncs(member, "viewport") == 0) {
           return Py_BuildValue("[f,f,f,f]", Tttab->tvp[0], Tttab->tvp[1],
                                Tttab->tvp[2], Tttab->tvp[3]);
        } else if (cmpncs(member, "worldcoordinate") == 0) {
           return Py_BuildValue("[f,f,f,f]", Tttab->twc[0], Tttab->twc[1],
                                Tttab->twc[2], Tttab->twc[3]);
        } else if (cmpncs(member, "x") == 0) {
           if (Tttab->tx == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           x=PyList_New(Tttab->tx->nsegs);
           listptr = (PyObject **) malloc(Tttab->tx->nsegs*sizeof(PyObject));
           j = 0;
           aptr = Tttab->tx->ps;
           while (aptr != NULL) {
              listptr[j]=PyList_New(aptr->npts);
              for (i=0; i<(aptr->npts); i++) {
                  PyList_SetItem(listptr[j], i, Py_BuildValue("f", aptr->pts[i]));
              }
             PyList_SetItem(x, j, listptr[j]); ++j;
             aptr = aptr->next;
           }
           return x;
        } else if (cmpncs(member, "y") == 0) {
           if (Tttab->ty == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
           }
           y=PyList_New(Tttab->ty->nsegs);
           listptr = (PyObject **) malloc(Tttab->ty->nsegs*sizeof(PyObject));
           j = 0;
           aptr = Tttab->ty->ps;
           while (aptr != NULL) {
              listptr[j]=PyList_New(aptr->npts);
              for (i=0; i<(aptr->npts); i++) {
                  PyList_SetItem(listptr[j], i, Py_BuildValue("f", aptr->pts[i]));
              }
             PyList_SetItem(y, j, listptr[j]); ++j;
             aptr = aptr->next;
           }
           return y;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing text table object and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the text table  object's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setTtmember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 				i,j,MODE, value_int;
	float				value_float;
        char 				*Tt_name, *member=NULL;
        PyObject 			*TT=NULL, *MEMBER=NULL, *VALUE=NULL;
        PyObject                        *tup;
        struct array_segments           *pts, *tpts;
        struct char_segments            *cpts, *ctpts;
	struct table_text       	*get_Tttab=NULL;
	extern struct table_text        Tt_tab;
   	extern struct table_text 	*getTt();
	struct table_text    		*Tttab;
	extern int              	update_ind;
	extern int              	chk_mov_Tt();
	extern int 			vcs_legacy_canvas_update();
        extern void                     free_points();

        if(PyArg_ParseTuple(args,"|OOOi", &TT, &MEMBER, &VALUE, &MODE)) {
           if (TT == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(TT,"name", "s", &Tt_name);
        Tttab=getTt(Tt_name);

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	/*
	 * Set the appropriate text table attribute. But first 
	 * get the text table structure.
         */
	get_Tttab = getTt(Tttab->name);
	if (cmpncs(member, "projection") == 0) {
	  strcpy(get_Tttab->proj, PyString_AsString(VALUE));
	}
        else if (cmpncs(member, "font") == 0) {
	   if (VALUE == Py_None)
              get_Tttab->txfont = 1;
           else
              get_Tttab->txfont = (int) PyInt_AsLong(VALUE);
        } else if (cmpncs(member, "spacing") == 0) {
	   if (VALUE == Py_None)
              get_Tttab->txsp = .2;
           else
              get_Tttab->txsp = (float) (PyInt_AsLong(VALUE) / 10.0);
        } else if (cmpncs(member, "expansion") == 0) {
	   if (VALUE == Py_None)
              get_Tttab->txexp = 1.0;
           else
              get_Tttab->txexp = (float) (PyInt_AsLong(VALUE) / 100.0);
        } else if (cmpncs(member, "color") == 0) {
	   if (VALUE == Py_None)
              get_Tttab->txci = 241;
           else{
              get_Tttab->txci = (int) PyInt_AsLong(VALUE);
		    }
        } else if (cmpncs(member, "fillincolor") == 0) {
	   if (VALUE == Py_None)
              get_Tttab->txfci = 240;
           else{
              get_Tttab->txfci = (int) PyInt_AsLong(VALUE);
		    }
        } else if (cmpncs(member, "priority") == 0) {
           if (VALUE == Py_None)
              get_Tttab->priority = 1;
           else
              get_Tttab->priority = (int) PyInt_AsLong(VALUE);
        } else if (cmpncs(member, "viewport") == 0) {
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<PyList_Size(VALUE); i++)
                 get_Tttab->tvp[i] = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "worldcoordinate") == 0) {
           if (PyList_Check(VALUE)) { /* check for list */
              for (i=0; i<PyList_Size(VALUE); i++)
                 get_Tttab->twc[i] = (float) PyFloat_AsDouble(PyList_GetItem(VALUE,i));
           }
        } else if (cmpncs(member, "x") == 0) {
           free_points( &get_Tttab->tx ); 
           if (PyList_Check(VALUE)) { /* check for list */
              tup = PyList_GetItem(VALUE, 0);
              /* Set up the pointer struct that will point the list of segments */
              if ((get_Tttab->tx =
                  (struct points_struct *) malloc( sizeof(struct points_struct))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for points not found.");
                  return NULL;
              }
              get_Tttab->tx->ps = NULL;

              if ( (PyList_Check(tup)) || (PyTuple_Check(tup)) ) { /* check for list or tuple */
                 get_Tttab->tx->nsegs = PyList_Size(VALUE);

                 for (i = 0; i < get_Tttab->tx->nsegs; i++) {
/*                    if ((pts = (struct array_segments *) malloc(sizeof(
                        struct array_segments))) == NULL) {
                      PyErr_SetString(VCS_Error,"Error - table entry memory for points not found. ");
                      return NULL;
                    }*/

                    store_pts( &pts, PyList_GetItem(VALUE,i) );

                    if (get_Tttab->tx->ps == NULL) {
                       tpts = get_Tttab->tx->ps = pts;
                    } else {
                       tpts->next = pts;
                       tpts = pts;
                    }
                 }
              } else if ((PyInt_Check(tup)) || (PyFloat_Check(tup))) {
                 get_Tttab->tx->nsegs = 1;
                 get_Tttab->tx->ps = NULL;
                 store_pts( &get_Tttab->tx->ps, VALUE );
              } else {
                 PyErr_SetString(VCS_Error,"Error - Must be a Python List or Tuple.");
                 Py_INCREF(Py_None);
                 return Py_None;
              }
           }
        } else if (cmpncs(member, "y") == 0) {
           free_points( &get_Tttab->ty );
           if (PyList_Check(VALUE)) { /* check for list */
              tup = PyList_GetItem(VALUE, 0);
              /* Set up the pointer struct that will point the list of segments */
              if ((get_Tttab->ty =
                  (struct points_struct *) malloc( sizeof(struct points_struct))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for points not found.");
                  return NULL;
              }
              get_Tttab->ty->ps = NULL;

              if ( (PyList_Check(tup)) || (PyTuple_Check(tup)) ) { /* check for list or tuple */
                 get_Tttab->ty->nsegs = PyList_Size(VALUE);

                 for (i = 0; i < get_Tttab->ty->nsegs; i++) {
/*                    if ((pts = (struct array_segments *) malloc(sizeof(
                        struct array_segments))) == NULL) {
                      PyErr_SetString(VCS_Error,"Error - table entry memory for points not found. ");
                      return NULL;
                    }*/

                    store_pts( &pts, PyList_GetItem(VALUE,i) );

                    if (get_Tttab->ty->ps == NULL) {
                       tpts = get_Tttab->ty->ps = pts;
                    } else {
                       tpts->next = pts;
                       tpts = pts;
                    }
                 }
              } else if ((PyInt_Check(tup)) || (PyFloat_Check(tup))) {
                 get_Tttab->ty->nsegs = 1;
                 get_Tttab->ty->ps = NULL;
                 store_pts( &get_Tttab->ty->ps, VALUE );
              } else {
                 PyErr_SetString(VCS_Error,"Error - Must be a Python List or Tuple.");
                 Py_INCREF(Py_None);
                 return Py_None;
              }
           }
        } else if (cmpncs(member, "string") == 0) {
           free_strings( &get_Tttab->ts );
           if (PyList_Check(VALUE)) { /* check for list */
              tup = PyList_GetItem(VALUE, 0);
              /* Set up the pointer struct that will point the list of segments */
              if ((get_Tttab->ts =
                  (struct strings_struct *) malloc( sizeof(struct strings_struct))) == NULL) {
                  PyErr_SetString(VCS_Error,"Error - table entry memory for strings not found.");
                  return NULL;
              }
              get_Tttab->ts->ss = NULL;

              if (PyString_Check(tup)) { /* check for string */
                 get_Tttab->ts->nsegs = PyList_Size(VALUE);

                 for (i = 0; i < get_Tttab->ts->nsegs; i++) {
                    store_cpts( &cpts, PyList_GetItem(VALUE,i) );

                    if (get_Tttab->ts->ss == NULL) {
                       ctpts = get_Tttab->ts->ss = cpts;
                    } else {
                       ctpts->next = cpts;
                       ctpts = cpts;
                    }
                    cpts = cpts->next;
                 }
/*              } else if (PyString_Check(tup)) {
                 get_Tttab->ts->nsegs = 1;
                 get_Tttab->ts->ss = NULL;
                 store_cpts( &get_Tttab->ts->ss, VALUE );*/
              } else {
                 PyErr_SetString(VCS_Error,"Error - Must be a Python List or Tuple.");
                 Py_INCREF(Py_None);
                 return Py_None;
              }
           }
        }

	/* Set the text precision (it will always equals 2)*/
	get_Tttab->txpr = 2;

/* 	printf("Update_ind is before ch_tt: %s,%d, color:%d\n",get_Tttab->name,update_ind,get_Tttab->txci); */
	chk_mov_Tt(get_Tttab);
/* 	printf("Update_ind is after ch_tt: %d\n",update_ind); */
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new text table object method by copying from an existing
 * text table object method. If no source copy name argument is given,
 * theet the default text table object method will be used to replicate
 * the new text table object.
 */
static PyObject *
PyVCS_copyTt(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *TT_SRC=NULL, *TT_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_Tt_name();
              
        if(PyArg_ParseTuple(args,"|ss", &TT_SRC, &TT_NAME)) {
           if (TT_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source text table graphics method name.");
                 return NULL;
           }

           if (TT_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", TT_NAME);
        }

        ierr = copy_Tt_name(TT_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating text table graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing text table object method.
 */
static PyObject *
PyVCS_renameTt(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *TT_OLD_NAME=NULL, *TT_NEW_NAME=NULL;
        extern int      renameTt_name();
 
        if(PyArg_ParseTuple(args,"|ss", &TT_OLD_NAME, &TT_NEW_NAME)) {
           if ((TT_OLD_NAME == NULL) || (TT_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new text table object name.");
                 return NULL;
           }
        }

        ierr = renameTt_name(TT_OLD_NAME, TT_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming text table graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing text table graphics method.
 */
static PyObject *
PyVCS_removeTt(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the text table file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeTt_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed text table object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The text table object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing text table graphics method.
 */
static PyObject *
PyVCS_scriptTt(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *TT_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
        char mode2[2];
	extern int dump_single_textt();
	FILE *fp;

        if(PyArg_ParseTuple(args,"|sss", &TT_NAME, &SCRIPT_NAME, &MODE)) {
           if (TT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the text table name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command text table */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object */
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_textt(fp, TT_NAME) == 0) {
              sprintf(buf, "Error - Cannot save text table script to output file - %s.",
                   SCRIPT_NAME);
              fclose(fp);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Return the VCS text orientation (To) class member value. 
 */
static PyObject *
PyVCS_getTomember(PyVCScanvas_Object *self, PyObject *args)
{
	char 				*To_name, *member=NULL, buf[1024];
	PyObject 			*TO=NULL, *MEMBER=NULL;
	struct table_chorn   		*Totab;
	extern struct table_chorn       To_tab;
   	extern struct table_chorn	*getTo();

	if(PyArg_ParseTuple(args,"|OO",&TO, &MEMBER)) {
           if (TO == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not correct object type.");
                 return NULL;
           }

           if (MEMBER != NULL) {
              member = PyString_AsString(MEMBER);
           } else {
              PyErr_SetString(PyExc_TypeError, "Must supply a member name.");
              return NULL;
           }
	}

	Get_Member(TO,"name", "s", &To_name);
	Totab=getTo(To_name);

     	if (Totab == NULL) {
	   sprintf(buf,"Cannot find text orientation class object To_%s.",To_name);
           PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	}

	if (cmpncs(member, "height") == 0) {
           return Py_BuildValue("i", (int) (Totab->chh*1000.0));
	} else if (cmpncs(member, "angle") == 0) {
           return Py_BuildValue("i", (int) Totab->chua);
	} else if (cmpncs(member, "path") == 0) {
           if (Totab->chpath == 114)
              return Py_BuildValue("s", "right");
           else if (Totab->chpath == 108)
              return Py_BuildValue("s", "left");
           else if (Totab->chpath == 117)
              return Py_BuildValue("s", "up");
           else if (Totab->chpath == 100)
              return Py_BuildValue("s", "down");
	} else if (cmpncs(member, "halign") == 0) {
           if (Totab->chalh == 108)
              return Py_BuildValue("s", "left");
           else if (Totab->chalh == 99)
              return Py_BuildValue("s", "center");
           else if (Totab->chalh == 114)
              return Py_BuildValue("s", "right");
	} else if (cmpncs(member, "valign") == 0) {
           if (Totab->chalv == 116)
              return Py_BuildValue("s", "top");
           else if (Totab->chalv == 99)
              return Py_BuildValue("s", "cap");
           else if (Totab->chalv == 104)
              return Py_BuildValue("s", "half");
           else if (Totab->chalv == 98)
              return Py_BuildValue("s", "base");
           else if (Totab->chalv == 115)
              return Py_BuildValue("s", "bottom");
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Find the existing text orientation object and set its member.
 * If the canvas mode is set to 1, then the plot will be redrawn
 * accordingly. If the canvas mode is set to 0, then nothing will
 * happen to the plot, but the text orientation  object's attribute will
 * be changed.
 */
static PyObject *
PyVCS_setTomember(self, args)
  PyObject *self;
  PyObject *args;
{
	int 				MODE, value_int;
	float				value_float;
        char 				*To_name, *member=NULL;
        PyObject 			*TO=NULL, *MEMBER=NULL, *VALUE=NULL;
	struct table_chorn      	*get_Totab=NULL;
	extern struct table_chorn       To_tab;
   	extern struct table_chorn	*getTo();
	struct table_chorn   		*Totab;
	extern int              	update_ind;
	extern int              	chk_mov_To();
	extern int 			vcs_legacy_canvas_update();

        if(PyArg_ParseTuple(args,"|OOOi", &TO, &MEMBER, &VALUE, &MODE)) {
           if (TO == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Not the correct object type.");
                 return NULL;
           }
        }

        Get_Member(TO,"name", "s", &To_name);
        Totab=getTo(To_name);

	if (MEMBER != NULL)
           member = PyString_AsString(MEMBER);

	/*
	 * Set the appropriate text orientation attribute. But first 
	 * get the text orientation structure.
         */
	get_Totab = getTo(Totab->name);
        if (cmpncs(member, "height") == 0) {
	   if (VALUE == Py_None)
              get_Totab->chh = 0.015;
           else
             get_Totab->chh = (float) (PyFloat_AsDouble(VALUE) / 1000.0);
        } else if (cmpncs(member, "angle") == 0) {
	   if (VALUE == Py_None)
              get_Totab->chua = 0.015;
           else
             get_Totab->chua = (float) PyInt_AsLong(VALUE);
	} else if (cmpncs(member, "path") == 0) {
           if (VALUE==Py_None)
              get_Totab->chpath = 114; /* default to right text path */
           else {
              if (cmpncs("right", PyString_AsString(VALUE))==0)
                 get_Totab->chpath = 114; 
              else if (cmpncs("left", PyString_AsString(VALUE))==0)
                 get_Totab->chpath = 108;
              else if (cmpncs("up", PyString_AsString(VALUE))==0)
                 get_Totab->chpath = 117;
              else if (cmpncs("down", PyString_AsString(VALUE))==0)
                 get_Totab->chpath = 100;
           }
	} else if (cmpncs(member, "halign") == 0) {
           if (VALUE==Py_None)
              get_Totab->chalh = 108; /* default to left horizontal alignment  */
           else {
              if (cmpncs("left", PyString_AsString(VALUE))==0)
		{
		  get_Totab->chalh = 108;
		}
              else if (cmpncs("center", PyString_AsString(VALUE))==0){
		get_Totab->chalh = 99;
	      }
              else if (cmpncs("right", PyString_AsString(VALUE))==0){
		get_Totab->chalh = 114; 
	      }
          }
	} else if (cmpncs(member, "valign") == 0) {
           if (VALUE==Py_None)
              get_Totab->chalv = 104; /* default to half vertical alignment  */
           else {
              if (cmpncs("top", PyString_AsString(VALUE))==0)
                 get_Totab->chalv = 116; 
              else if (cmpncs("cap", PyString_AsString(VALUE))==0)
                 get_Totab->chalv = 99;
              else if (cmpncs("half", PyString_AsString(VALUE))==0)
                 get_Totab->chalv = 104;
              else if (cmpncs("base", PyString_AsString(VALUE))==0)
                 get_Totab->chalv = 98;
              else if (cmpncs("bottom", PyString_AsString(VALUE))==0)
                 get_Totab->chalv = 115;
           }
        }

/* 	printf("before check To update_ind is: %s, %d\n",get_Totab->name,update_ind); */
	chk_mov_To(get_Totab);
/* 	printf("after check To update_ind is: %d\n",update_ind); */
        update_ind = MODE; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Create a new text orientation object method by copying from an existing
 * text orientation object method. If no source copy name argument is given,
 * then the default text orientation object method will be used to replicate
 * the new text orientation object.
 */
static PyObject *
PyVCS_copyTo(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *TO_SRC=NULL, *TO_NAME=NULL;
        char            copy_name[1024];
        extern int      copy_To_name();
              
        if(PyArg_ParseTuple(args,"|ss", &TO_SRC, &TO_NAME)) {
           if (TO_SRC == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Must provide source text orientation graphics method name.");
                 return NULL;
           }

           if (TO_NAME == NULL)
              sprintf(copy_name, "%s", "default");
           else
              sprintf(copy_name, "%s", TO_NAME);
        }

        ierr = copy_To_name(TO_SRC, copy_name);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error creating text orientation graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Rename an existing text orientation object method.
 */
static PyObject *
PyVCS_renameTo(self, args)
  PyObject *self;
  PyObject *args;
{
        int             ierr;
        char            *TO_OLD_NAME=NULL, *TO_NEW_NAME=NULL;
        extern int      renameTo_name();
 
        if(PyArg_ParseTuple(args,"|ss", &TO_OLD_NAME, &TO_NEW_NAME)) {
           if ((TO_OLD_NAME == NULL) || (TO_NEW_NAME == NULL)) {
                 PyErr_SetString(PyExc_TypeError, "Must provide new text orientation object name.");
                 return NULL;
           }
        }

        ierr = renameTo_name(TO_OLD_NAME, TO_NEW_NAME);
        if (ierr==0) {
           PyErr_SetString(PyExc_ValueError, "Error renaming text orientation graphics method.");
           return NULL;
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Remove an existing text orientation graphics method.
 */
static PyObject *
PyVCS_removeTo(self, args)
  PyObject *self;
  PyObject *args;
{
	char *REMOVE_NAME=NULL, buf[1024];

        if(PyArg_ParseTuple(args,"|s", &REMOVE_NAME)) {
           if (REMOVE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the text orientation file name.");
                 return NULL;
           }
        }

        /* Return Python String Object */
	if (removeTo_name(REMOVE_NAME) == 1) {
           sprintf(buf,"Removed text orientation object (%s).", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	} else {
           sprintf(buf,"The text orientation object (%s) was not removed.", REMOVE_NAME);
           return Py_BuildValue("s", buf);
	}
}

/* 
 * Script out an existing text table graphics method.
 */
static PyObject *
PyVCS_scriptTo(self, args)
  PyObject *self;
  PyObject *args;
{
	int ffd, wfd;
	char *SCRIPT_NAME=NULL, *TO_NAME=NULL, *MODE=NULL, buf[1024];
	char replace_name[1024], initial_script[1024], mv_command[1024];
	char mode2[2];
	extern int dump_single_texto();
	FILE *fp;

        if(PyArg_ParseTuple(args,"|sss", &TO_NAME, &SCRIPT_NAME, &MODE)) {
           if (TO_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the text table name.");
                 return NULL;
           }
           if (SCRIPT_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide the script file name.");
                 return NULL;
           }
        }

        if ((MODE == NULL) || (MODE[0] == '\0') || (MODE[0] == ' ')) {
           strcpy(mode2,"a");
        } else if (strcmp(MODE,"w") == 0) {
           strcpy(mode2, "w");
        } else {
           strcpy(mode2, "a");
        }

        /* check for directory and file access */
        ffd = access(SCRIPT_NAME, F_OK);
        wfd = access(SCRIPT_NAME, W_OK);
        if ((ffd == 0) && (wfd == 0) && (strcmp(mode2,"w") == 0)) { /* The file exist! */
           /* Get the replacement name and command text table */
           strcpy(replace_name, SCRIPT_NAME);
           strcat (replace_name, "%");
           sprintf(mv_command, "/bin/mv %s %s", SCRIPT_NAME, replace_name);
           if ((system (mv_command)) != 0) {
              sprintf(buf,"Error - In replacing %s script file.", SCRIPT_NAME);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           }
        }

        /* Return NULL Python Object or Python String Object*/
        if ((fp=fopen(SCRIPT_NAME,mode2)) == NULL) {
	   sprintf(buf, "Error - opening file (%s) - script dump was not made.\n",SCRIPT_NAME);
           PyErr_SetString(PyExc_ValueError, buf);
           return NULL;
        } else {
	   if (dump_single_texto(fp, TO_NAME) == 0) {
              sprintf(buf, "Error - Cannot save text table script to output file - %s.",
                   SCRIPT_NAME);
              fclose(fp);
              PyErr_SetString(PyExc_ValueError, buf);
              return NULL;
           } else
              fclose(fp);
              sprintf(buf,"The script file was saved to (%s).", SCRIPT_NAME);
              return Py_BuildValue("s", buf);
        }
}


/* 
 * Update the VCS canvas by redrawing the plot. Redrawing
 * occurs only if something new or different is displayed
 * on the plot.
 */
static PyObject *
PyVCS_updatecanvas(PyVCScanvas_Object *self, PyObject *args)
{
	extern int              update_ind;
	extern int 		vcs_legacy_canvas_update();

        /*
         * Make sure the Canvas is in front.
         */
#ifdef X11WM
        if ( (self->connect_id.display != NULL) && (self->connect_id.drawable != 0))
           XRaiseWindow(self->connect_id.display, self->connect_id.drawable);
#elif defined QTWM
        if (self->connect_id.cr != NULL)
	  vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#else
	fprintf(stderr,"insert here your raise func\n");
#endif
        update_ind = 1; /* Update the display if needed */
	vcs_legacy_canvas_update(0);

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* 
 * Update the VCS canvas by redrawing the plot. Redrawing
 * occurs only if something new or different is displayed
 * on the plot. This function has a specical check for the
 * continents.
 */
static PyObject *
PyVCS_updatecanvas_continents(PyVCScanvas_Object *self, PyObject *args)
{
        int                             	 hold_continents;
        canvas_display_list             	*cdptr;
        struct display_tab              	*dtab;
        extern struct display_tab       	D_tab;
        struct a_attr 				*pa;
        struct a_tab            		*ptab;
        extern struct a_tab     		A_tab;
        extern int                     	 	update_ind;
        extern int                      	vcs_legacy_canvas_update();
	extern struct default_continents 	Dc;

        /*
         * Make sure the Canvas is in front.
         */
#ifdef X11WM
        if ( (self->connect_id.display != NULL) && (self->connect_id.drawable != 0)
 )
           XRaiseWindow(self->connect_id.display, self->connect_id.drawable);
#elif defined QTWM
        if (self->connect_id.cr != NULL)
	  vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#else
	fprintf(stderr,"insert here your raise func\n");
#endif
        /* Determine if the Continents need to be displayed or not. */
        hold_continents = Dc.selected;
        cdptr = self->dlist;
        while (cdptr != NULL) {
           dtab=&D_tab;
           while ((dtab != NULL) &&
                  (strcmp(dtab->name, cdptr->display_name) != 0))
                 dtab = dtab->next;
           if (dtab != NULL) {  /* must have been removed from animation */

              ptab=&A_tab;
	      pa=ptab->pA_attr;
              while ((ptab != NULL) && (strcmp(ptab->name, dtab->g_name) != 0)) {
                 ptab=ptab->next;
              }
              if ( (pa != NULL) && ((!doexist("longitude",pa->XN[0])) ||
                   (!doexist("latitude",pa->XN[1]))) )
                 Dc.selected = 0;
           }
           cdptr = cdptr->next;
        }

        /* Update the display if needed */
        update_ind = 1;
	vcs_legacy_canvas_update(0);

        Dc.selected = hold_continents; /* Restore Continent's flag */


        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/*
 * Return the template names as a Python list.
 */
PyObject *
PyVCS_listtemplate(/* no args */)
{
        int                             i,ct=0;
        struct p_tab    		*ptab;
        extern struct p_tab 		Pic_tab;
        PyObject                        *listptr;

        ptab=&Pic_tab;

        while (ptab != NULL) {
            /* Ignore template names that begin with a '.' -Jen =) */
            if (ptab->name[0] != '.')
                ++ct;
            ptab=ptab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,ptab=&Pic_tab;ptab != NULL; i++, ptab=ptab->next)
            /* Ignore template names that begin with a '.' -Jen =) */
            if (ptab->name[0] != '.')
               PyList_SetItem(listptr, i, Py_BuildValue("s", ptab->name));

        /* Return the list of line names */
        return listptr;
}

/*
 * Return the boxfill names as a Python list.
 */
PyObject *
PyVCS_listboxfill(/* no args */)
{
        int                             i,ct=0;
        struct gfb_tab          	*gfbtab;
        extern struct gfb_tab   	Gfb_tab;
        PyObject                        *listptr;

        gfbtab=&Gfb_tab;
        while (gfbtab != NULL) {
            ++ct;
            gfbtab=gfbtab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,gfbtab=&Gfb_tab;gfbtab != NULL; i++, gfbtab=gfbtab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", gfbtab->name));

        /* Return the list of line names */
        return listptr;
}


/*
 * Return the continents names as a Python list.
 */
PyObject *
PyVCS_listcontinents(/* no args */)
{
        int                             i,ct=0;
        struct gcon_tab         	*gcontab;
        extern struct gcon_tab  	Gcon_tab;
        PyObject                        *listptr;

        gcontab=&Gcon_tab;
        while (gcontab != NULL) {
            ++ct;
            gcontab=gcontab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,gcontab=&Gcon_tab;gcontab != NULL; i++, gcontab=gcontab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", gcontab->name));

        /* Return the list of line names */
        return listptr;
}

/*
 * Return the isofill names as a Python list.
 */
PyObject *
PyVCS_listisofill(/* no args */)
{
        int                             i,ct=0;
        struct gfi_tab          	*gfitab;
        extern struct gfi_tab   	Gfi_tab;
        PyObject                        *listptr;

        gfitab=&Gfi_tab;
        while (gfitab != NULL) {
            ++ct;
            gfitab=gfitab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,gfitab=&Gfi_tab;gfitab != NULL; i++, gfitab=gfitab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", gfitab->name));

        /* Return the list of line names */
        return listptr;
}
/*
 * Return the meshfill names as a Python list.
 */
PyObject *
PyVCS_listmeshfill(/* no args */)
{
        int                             i,ct=0;
        struct gfm_tab          	*gfmtab;
        extern struct gfm_tab   	Gfm_tab;
        PyObject                        *listptr;

        gfmtab=&Gfm_tab;
        while (gfmtab != NULL) {
            ++ct;
            gfmtab=gfmtab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,gfmtab=&Gfm_tab;gfmtab != NULL; i++, gfmtab=gfmtab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", gfmtab->name));

        /* Return the list of line names */
        return listptr;
}

/*
 * Return the projection names as a Python list.
 */
PyObject *
PyVCS_listprojection(/* no args */)
{
        int                             i,ct=0;
        struct projection_attr           *pj;
        extern struct projection_attr   	p_PRJ_list;
        PyObject                        *listptr;

        pj=&p_PRJ_list;
        while (pj != NULL) {
            ++ct;
            pj=pj->next;
        }

        listptr = PyList_New(ct);
        for(i=0,pj=&p_PRJ_list;pj != NULL; i++, pj=pj->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", pj->name));

        /* Return the list of line names */
        return listptr;
}

/*
 * Return the isoline names as a Python list.
 */
PyObject *
PyVCS_listisoline(/* no args */)
{
        int                             i,ct=0;
        struct gi_tab           	*gitab;
        extern struct gi_tab    	Gi_tab;
        PyObject                        *listptr;

        gitab=&Gi_tab;
        while (gitab != NULL) {
            ++ct;
            gitab=gitab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,gitab=&Gi_tab;gitab != NULL; i++, gitab=gitab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", gitab->name));

        /* Return the list of line names */
        return listptr;
}

/*
 * Return the outfill names as a Python list.
 */
PyObject *
PyVCS_listoutfill(/* no args */)
{
        int                             i,ct=0;
        struct gfo_tab          	*gfotab;
        extern struct gfo_tab   	Gfo_tab;
        PyObject                        *listptr;

        gfotab=&Gfo_tab;
        while (gfotab != NULL) {
            ++ct;
            gfotab=gfotab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,gfotab=&Gfo_tab;gfotab != NULL; i++, gfotab=gfotab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", gfotab->name));

        /* Return the list of line names */
        return listptr;
}

/*
 * Return the outline names as a Python list.
 */
PyObject *
PyVCS_listoutline(/* no args */)
{
        int                             i,ct=0;
        struct go_tab           	*gotab;
        extern struct go_tab    	Go_tab;
        PyObject                        *listptr;

        gotab=&Go_tab;
        while (gotab != NULL) {
            ++ct;
            gotab=gotab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,gotab=&Go_tab;gotab != NULL; i++, gotab=gotab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", gotab->name));

        /* Return the list of line names */
        return listptr;
}

/*
 * Return the scatter names as a Python list.
 */
PyObject *
PyVCS_listscatter(/* no args */)
{
        int                             i,ct=0;
        struct gSp_tab          	*gSptab;
        extern struct gSp_tab   	GSp_tab;
        PyObject                        *listptr;

        gSptab=&GSp_tab;
        while (gSptab != NULL) {
            ++ct;
            gSptab=gSptab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,gSptab=&GSp_tab;gSptab != NULL; i++, gSptab=gSptab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", gSptab->name));

        /* Return the list of line names */
        return listptr;
}

/*
 * Return the vector names as a Python list.
 */
PyObject *
PyVCS_listvector(/* no args */)
{
        int                             i,ct=0;
        struct gv_tab           	*gvtab;
        extern struct gv_tab    	Gv_tab;
        PyObject                        *listptr;

        gvtab=&Gv_tab;
        while (gvtab != NULL) {
            ++ct;
            gvtab=gvtab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,gvtab=&Gv_tab;gvtab != NULL; i++, gvtab=gvtab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", gvtab->name));

        /* Return the list of line names */
        return listptr;
}

/*
 * Return the XvsY names as a Python list.
 */
PyObject *
PyVCS_listxvsy(/* no args */)
{
        int                             i,ct=0;
        struct gXY_tab          	*gXYtab;
        extern struct gXY_tab   	GXY_tab;
        PyObject                        *listptr;

        gXYtab=&GXY_tab;
        while (gXYtab != NULL) {
            ++ct;
            gXYtab=gXYtab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,gXYtab=&GXY_tab;gXYtab != NULL; i++, gXYtab=gXYtab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", gXYtab->name));

        /* Return the list of line names */
        return listptr;
}

/*
 * Return the Xyvsy names as a Python list.
 */
PyObject *
PyVCS_listxyvsy(/* no args */)
{
        int                             i,ct=0;
        struct gXy_tab          	*gXytab;
        extern struct gXy_tab   	GXy_tab;
        PyObject                        *listptr;

        gXytab=&GXy_tab;
        while (gXytab != NULL) {
            ++ct;
            gXytab=gXytab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,gXytab=&GXy_tab;gXytab != NULL; i++, gXytab=gXytab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", gXytab->name));

        /* Return the list of line names */
        return listptr;
}

/*
 * Return the Yxvsx names as a Python list.
 */
PyObject *
PyVCS_listyxvsx(/* no args */)
{
        int                             i,ct=0;
        struct gYx_tab          	*gYxtab;
        extern struct gYx_tab   	GYx_tab;
        PyObject                        *listptr;

        gYxtab=&GYx_tab;
        while (gYxtab != NULL) {
            ++ct;
            gYxtab=gYxtab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,gYxtab=&GYx_tab;gYxtab != NULL; i++, gYxtab=gYxtab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", gYxtab->name));

        /* Return the list of line names */
        return listptr;
}

/*
 * Return the line names as a Python list.
 */
PyObject *
PyVCS_listline(/* no args */)
{
	int				i,ct=0;
        struct table_line               *tltab;
        extern struct table_line        Tl_tab;
        PyObject 			*listptr;

        tltab=&Tl_tab;
        while (tltab != NULL) {
            ++ct;
            tltab=tltab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,tltab=&Tl_tab;tltab != NULL; i++, tltab=tltab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", tltab->name));

        /* Return the list of line names */
	return listptr;
}

/*
 * Return the marker names as a Python list.
 */
PyObject *
PyVCS_listmarker(/* no args */)
{
        int                             i,ct=0;
        struct table_mark               *tmtab;
        extern struct table_mark        Tm_tab;
        PyObject                        *listptr;

        tmtab=&Tm_tab;
        while (tmtab != NULL) {
            ++ct;
            tmtab=tmtab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,tmtab=&Tm_tab;tmtab != NULL; i++, tmtab=tmtab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", tmtab->name));

        /* Return the list of marker names */
        return listptr;
}

/*
 * Return the fill area names as a Python list.
 */
PyObject *
PyVCS_listfillarea(/* no args */)
{
        int                             i,ct=0;
        struct table_fill               *tftab;
        extern struct table_fill        Tf_tab;
        PyObject                        *listptr;

        tftab=&Tf_tab;
        while (tftab != NULL) {
            ++ct;
            tftab=tftab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,tftab=&Tf_tab;tftab != NULL; i++, tftab=tftab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", tftab->name));
 
        /* Return the list of marker names */
        return listptr;
}

/*
 * Return the text table names as a Python list.
 */
PyObject *
PyVCS_listtexttable(/* no args */)
{
        int                             i,ct=0;
        struct table_text               *tttab;
        extern struct table_text        Tt_tab;
        PyObject 			*listptr;

        tttab=&Tt_tab;
        while (tttab != NULL) {
            ++ct;
            tttab=tttab->next;
        }

	listptr = PyList_New(ct);
	for(i=0,tttab=&Tt_tab;tttab != NULL; i++, tttab=tttab->next)
	   PyList_SetItem(listptr, i, Py_BuildValue("s", tttab->name));

        /* Return the list of text table names */
        return listptr;
}

/*
 * Return the text orientation names as a Python list.
 */
PyObject *
PyVCS_listtextorientation(/* no args */)
{
        int                             i,ct=0;
        struct table_chorn              *totab;
        extern struct table_chorn       To_tab;
        PyObject                        *listptr;

        totab=&To_tab;
        while (totab != NULL) {
            ++ct;
            totab=totab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,totab=&To_tab;totab != NULL; i++, totab=totab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", totab->name));

        /* Return the list of text orientation names */
        return listptr;
}

/*
 * Return the text orientation names as a Python list.
 */
PyObject *
PyVCS_listfont(/* no args */)
{
        int                             i,ct=0;
	extern struct table_FT_VCS_FONTS TTFFONTS;
	struct table_FT_VCS_FONTS *current_font;
        PyObject                        *listptr;

        current_font=&TTFFONTS;
        while (current_font != NULL) {
            ++ct;
            current_font=current_font->next;
        }

        listptr = PyList_New(ct);
        for(i=0,current_font=&TTFFONTS;current_font != NULL; i++, current_font=current_font->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", current_font->name));

        /* Return the list of text orientation names */
        return listptr;
}

/*
 * Return the color map names as a Python list.
 */
PyObject *
PyVCS_listcolormap(/* no args */)
{
        int                             i,ct=0;
        struct color_table              *ctab;
        extern struct color_table       C_tab;
        PyObject                        *listptr;

        ctab=&C_tab;
        while (ctab != NULL) {
            ++ct;
            ctab=ctab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,ctab=&C_tab;ctab != NULL; i++, ctab=ctab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", ctab->name));

        /* Return the list of text orientation names */
        return listptr;
}

/*
 * Return the format names as a Python list.
 */
PyObject *
PyVCS_listformat(/* no args */)
{
        int                             i,ct=0;
        struct table_form               *thtab;
        extern struct table_form        Th_tab;
        PyObject                        *listptr;

        thtab=&Th_tab;
        while (thtab != NULL) {
            ++ct;
            thtab=thtab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,thtab=&Th_tab;thtab != NULL; i++, thtab=thtab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", thtab->name));

        /* Return the list of text orientation names */
        return listptr;
}

/*
 * Return the VCS list names as a Python list.
 */
PyObject *
PyVCS_listlist(/* no args */)
{
        int                             i,ct=0;
        struct l_tab            	*ltab;
        extern struct l_tab 		L_tab[2];
        PyObject                        *listptr;

        ltab=&L_tab[0];
        while (ltab != NULL) {
            ++ct;
            ltab=ltab->next;
        }

        listptr = PyList_New(ct);
        for(i=0,ltab=&L_tab[0];ltab != NULL; i++, ltab=ltab->next)
           PyList_SetItem(listptr, i, Py_BuildValue("s", ltab->name));

        /* Return the list of text orientation names */
        return listptr;
}

/* Return a Python list of VCS elements. That is, this function returns:
 * template, data, boxfill, continent, isofill, isoline, outfill, outline,
 * scatter vector, xvsy, xyvsy, yxvsy, colormap, line, text, marker, 
 * fillarea, format, and list.
 */
static PyObject *
PyVCS_listelements(PyVCScanvas_Object *self, PyObject *args)
{
	char		*element=NULL;
	PyObject 	*listptr=NULL;

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

        if(PyArg_ParseTuple(args, "|s", &element)) {
           if ((element == NULL) || (element[0] == '\0')) {
              /* Get the number of variable attributes */
              listptr = PyList_New(24);
              PyList_SetItem(listptr, 0, Py_BuildValue("s", "template"));
              PyList_SetItem(listptr, 1, Py_BuildValue("s", "boxfill"));
              PyList_SetItem(listptr, 2, Py_BuildValue("s", "continents"));
              PyList_SetItem(listptr, 3, Py_BuildValue("s", "isofill"));
              PyList_SetItem(listptr, 4, Py_BuildValue("s", "isoline"));
              PyList_SetItem(listptr, 5, Py_BuildValue("s", "outfill"));
              PyList_SetItem(listptr, 6, Py_BuildValue("s", "outline"));
              PyList_SetItem(listptr, 7, Py_BuildValue("s", "scatter"));
              PyList_SetItem(listptr, 8, Py_BuildValue("s", "taylordiagram"));
              PyList_SetItem(listptr, 9, Py_BuildValue("s", "vector"));
              PyList_SetItem(listptr, 10, Py_BuildValue("s", "xvsy"));
              PyList_SetItem(listptr, 11, Py_BuildValue("s", "xyvsy"));
              PyList_SetItem(listptr, 12, Py_BuildValue("s", "yxvsx"));
              PyList_SetItem(listptr, 13, Py_BuildValue("s", "colormap"));
              PyList_SetItem(listptr, 14, Py_BuildValue("s", "fillarea"));
              PyList_SetItem(listptr, 15, Py_BuildValue("s", "format"));
              PyList_SetItem(listptr, 16, Py_BuildValue("s", "line"));
              PyList_SetItem(listptr, 17, Py_BuildValue("s", "list"));
              PyList_SetItem(listptr, 18, Py_BuildValue("s", "marker"));
              PyList_SetItem(listptr, 19, Py_BuildValue("s", "texttable"));
              PyList_SetItem(listptr, 20, Py_BuildValue("s", "textorientation"));
              PyList_SetItem(listptr, 21, Py_BuildValue("s", "meshfill"));
              PyList_SetItem(listptr, 22, Py_BuildValue("s", "projection"));
              PyList_SetItem(listptr, 23, Py_BuildValue("s", "font"));
           } else if (cmpncs(element, "template") == 0)
             listptr = PyVCS_listtemplate();
           else if (cmpncs(element, "boxfill") == 0)
             listptr = PyVCS_listboxfill();
           else if (cmpncs(element, "continents") == 0)
             listptr = PyVCS_listcontinents();
           else if (cmpncs(element, "isofill") == 0)
             listptr = PyVCS_listisofill();
           else if (cmpncs(element, "meshfill") == 0)
             listptr = PyVCS_listmeshfill();
           else if (cmpncs(element, "projection") == 0)
             listptr = PyVCS_listprojection();
           else if (cmpncs(element, "isoline") == 0)
             listptr = PyVCS_listisoline();
           else if (cmpncs(element, "outfill") == 0)
             listptr = PyVCS_listoutfill();
           else if (cmpncs(element, "outline") == 0)
             listptr = PyVCS_listoutline();
           else if (cmpncs(element, "scatter") == 0)
             listptr = PyVCS_listscatter();
           else if (cmpncs(element, "vector") == 0)
             listptr = PyVCS_listvector();
           else if (cmpncs(element, "xyvsy") == 0)
             listptr = PyVCS_listxyvsy();
           else if (cmpncs(element, "yxvsx") == 0)
             listptr = PyVCS_listyxvsx();
           else if (cmpncs(element, "xvsy") == 0)
             listptr = PyVCS_listxvsy();
           else if (cmpncs(element, "line") == 0)
             listptr = PyVCS_listline();
           else if (cmpncs(element, "marker") == 0)
             listptr = PyVCS_listmarker();
           else if (cmpncs(element, "fillarea") == 0)
             listptr = PyVCS_listfillarea();
           else if (cmpncs(element, "texttable") == 0)
             listptr = PyVCS_listtexttable();
           else if (cmpncs(element, "textorientation") == 0)
             listptr = PyVCS_listtextorientation();
           else if (cmpncs(element, "font") == 0)
             listptr = PyVCS_listfont();
           else if (cmpncs(element, "colormap") == 0)
             listptr = PyVCS_listcolormap();
           else if (cmpncs(element, "format") == 0)
             listptr = PyVCS_listformat();
           else if (cmpncs(element, "list") == 0)
             listptr = PyVCS_listlist();
        }

        /* Return the list of dimension attributes */
        return listptr;
}

/* Set the default primary elements: template and graphics methods and 
 * the secondary element color. Keep in mind the template, determines 
 * the appearance of each segment; the graphic method specifies the display
 * technique; and the data defines what is to be displayed. Note the data
 * cannot be set. The colormap can be set here or by the colormap GUI.
 */
static PyObject *
PyVCS_set(PyVCScanvas_Object *self, PyObject *args)
{
        int 		ier;
        char    	*ierr;
        char 		*element=NULL, *name=NULL, buf[MAX_NAME];
	extern char *	python_colormap();
	extern int 	python_display();

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

        if(PyArg_ParseTuple(args, "|ss", &element, &name)) {
	   /* check for element */
           if ((element == NULL) || (element[0] == '\0')) {
              PyErr_SetString(PyExc_TypeError, "No primary element type given.");
              return NULL;
           } 

	   /* check for element name */
           if ((name == NULL) || (name[0] == '\0')) {
              pyoutput("Warning - No element name given. Using 'default' name.", 1);
	      name = (char *) malloc(strlen("default")+1);
	      strcpy(name,"default");
           }

	   /* Set the element and element name */
	   if (cmpncs(element, "Template") == 0) {
	      if (self->template_name != NULL)
		    free((char *) self->template_name);
	      if ((self->template_name = 
                 (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
	         PyErr_SetString(PyExc_TypeError, "No memory for the template name.");
		 return NULL;
	      } else {
		strcpy(self->template_name, name);
		sprintf(buf, "Default 'Template' now set to P_%s.", self->template_name);
	        pyoutput(buf,0);
	      }
           } else if (cmpncs(element, "colormap") == 0) {
	      ierr = python_colormap(name); /* set the new VCS colormap */
              Py_INCREF(Py_None);
	      return Py_None;
           } else if (cmpncs(element, "Boxfill") == 0) {
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if ((self->graphics_name = 
                    (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
	         PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
                 return NULL;
              } else {
                strcpy(self->graphics_name, name);
                strcpy(self->graphics_type, "Boxfill");
                sprintf(buf, "Default graphics method 'Boxfill' now set to Gfb_%s", self->graphics_name);
	        pyoutput(buf,0);
              }
           } else if (cmpncs(element, "Meshfill") == 0) {
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if ((self->graphics_name = 
                    (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
	         PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
                 return NULL;
              } else {
                strcpy(self->graphics_name, name);
                strcpy(self->graphics_type, "Meshfill");
                sprintf(buf, "Default graphics method 'Meshfill' now set to Gfm_%s", self->graphics_name);
	        pyoutput(buf,0);
              }
           } else if (cmpncs(element, "Continent") == 0) {
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if ((self->graphics_name = 
                 (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
                 PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
                 return NULL;
              } else {
                strcpy(self->graphics_name, name);
                strcpy(self->graphics_type, "Continents");
                sprintf(buf, "Default graphics method 'Continent' now set to Gcon_%s", self->graphics_name);
	        pyoutput(buf,0);
              }
           } else if (cmpncs(element, "Isofill") == 0) {
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if ((self->graphics_name = 
                 (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
                 PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
                 return NULL;
              } else {
                strcpy(self->graphics_name, name);
                strcpy(self->graphics_type, "Isofill");
                sprintf(buf, "Default graphics method 'Isofill' now set to Gfi_%s", self->graphics_name);
	        pyoutput(buf,0);
              }
           } else if (cmpncs(element, "Isoline") == 0) {
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if ((self->graphics_name = 
                 (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
                 PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
                 return NULL;
              } else {
                strcpy(self->graphics_name, name);
                strcpy(self->graphics_type, "Isoline");
                sprintf(buf, "Default graphics method 'Isoline' now set to Gi_%s", self->graphics_name);
                pyoutput(buf, 0);
              }
           } else if (cmpncs(element, "Outfill") == 0) {
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if ((self->graphics_name = 
                 (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
                 PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
                 return NULL;
              } else {
                strcpy(self->graphics_name, name);
                strcpy(self->graphics_type, "Outfill");
                sprintf(buf, "Default graphics method 'Outfill' now set to Gfo_%s", self->graphics_name);
                pyoutput(buf, 0);
              }
           } else if (cmpncs(element, "Outline") == 0) {
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if ((self->graphics_name = 
                 (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
                 PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
                 return NULL;
              } else {
                strcpy(self->graphics_name, name);
                strcpy(self->graphics_type, "Outline");
                sprintf(buf,"Default graphics method 'Outline' now set to Go_%s", self->graphics_name);
                pyoutput(buf, 0);
              }
           } else if (cmpncs(element, "Scatter") == 0) {
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if ((self->graphics_name = 
                 (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
                 PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
                 return NULL;
              } else {
                strcpy(self->graphics_name, name);
                strcpy(self->graphics_type, "Scatter");
                sprintf(buf, "Default graphics method 'Scatter' now set to GSp_%s", self->graphics_name);
                pyoutput(buf, 0);
              }
           } else if (cmpncs(element, "Vector") == 0) {
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if ((self->graphics_name = 
                 (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
                 PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
                 return NULL;
              } else {
                strcpy(self->graphics_name, name);
                strcpy(self->graphics_type, "Vector");
                sprintf(buf, "Default graphics method 'Vector' now set to Gv_%s", self->graphics_name);
                pyoutput(buf, 0);
              }
           } else if (cmpncs(element, "XvsY") == 0) {
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if ((self->graphics_name = 
                 (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
                 PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
                 return NULL;
              } else {
                strcpy(self->graphics_name, name);
                strcpy(self->graphics_type, "XvsY");
                sprintf(buf, "Default graphics method 'XvsY' now set to GXY_%s", self->graphics_name);
                pyoutput(buf, 0);
              }
           } else if (cmpncs(element, "Xyvsy") == 0) {
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if ((self->graphics_name = 
                 (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
                 PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
                 return NULL;
              } else {
                strcpy(self->graphics_name, name);
                strcpy(self->graphics_type, "Xyvsy");
                sprintf(buf, "Default graphics method 'Xyvsy' now set to GXy_%s", self->graphics_name);
                pyoutput(buf, 0);
              }
           } else if (cmpncs(element, "Yxvsx") == 0) {
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if (self->graphics_name != NULL)
                 free((char *) self->graphics_name);
              if ((self->graphics_name = 
                 (char *) malloc((strlen(name)+1)*sizeof(char)+1)) == NULL) {
                 PyErr_SetString(PyExc_TypeError, "No memory for the graphics name.");
                 return NULL;
              } else {
                strcpy(self->graphics_name, name);
                strcpy(self->graphics_type, "Yxvsx");
                sprintf(buf, "Default graphics method 'Yxvsx' now set to GYx_%s", self->graphics_name);
                pyoutput(buf, 0);
              }
           } else {
              PyErr_SetString(PyExc_TypeError, "Incorrect primary element name. Element name must be \n        'template' or one of the graphics methods.");
              return NULL;
	   }

	   /* Setup the VCS display to show the plot on the VCS Canvas. */
	   /* Update the display if necessary */
/* DNW have to work on the display link list
	   ier = python_display(self->a_name, self->template_name,
                                self->graphics_type, self->graphics_name,
                                self->display_name);
*/
        }

        /* Return null python object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* Set the default plotting region for variables that have more
 * dimension values than the graphics method. This will also be
 * used for animating plots over the third and fourth dimensions.
 */
static PyObject *
PyVCS_plotregion(PyVCScanvas_Object *self, PyObject *args)
{
	int     	argc,i,j=0,k=0,skip,first,missing_end=0;
	char 		buf[100], *str;
	PyObject 	*obj;

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

  	/* Parse the input argument string */
  	if (args == NULL) { /* check for no input */
           PyErr_SetString(PyExc_TypeError, "No arguments given.");
           return NULL;
        } else if (!PyTuple_Check (args)) { /* check to see if it's Tuple */
           PyErr_SetString(PyExc_TypeError, "Arguments are incorrect.");
           return NULL;
	} else {
           sprintf(buf, "Error - Incorrect argument.\n");
	   argc = PyTuple_Size (args); /* get the number of arguments */
  	   if (argc == 0) { /* check for no input */
              PyErr_SetString(PyExc_TypeError, "No arguments given.");
              return NULL;
           }
	   first = 1;
	   for (i = 0; i < argc; i++) {
	       skip = 0;
	       obj = PyTuple_GetItem (args, i); /* get argument */
               if(PyString_Check(obj)) { /*check for ':' or '*' wildcards*/
	         if (missing_end)
                    goto stop;
                 str = PyString_AsString(obj); 
	         if ((strcmp(str, ":") == 0) || (strcmp(str, "*") == 0)) {
                     skip = 1;
                     ++j;
                     ++k;
	         } else {
                     PyErr_SetString(PyExc_TypeError, "Missing end value.");
                     return NULL;
	         }
               }
	       if (!skip) {
                  if (first) {
                     first = 0;
                     missing_end = 1;
                     if(PyInt_Check(obj)) { /* check for integer */
                       index_s[j] = (int) PyInt_AsLong(obj);
                     } else if(PyFloat_Check(obj)) { /* check for float */
                       index_s[j] = (int) PyFloat_AsDouble(obj);
                     } else {
                       PyErr_SetString(PyExc_TypeError, buf);
                       return NULL;
	             }
                     j++;
                  } else {
                     first = 1;
                     missing_end = 0;
                     if(PyInt_Check(obj)) { /* check for integer */
                       index_e[k] = (int) PyInt_AsLong(obj);
                     } else if(PyFloat_Check(obj)) { /* check for float */
                       index_e[k] = (int) PyFloat_AsDouble(obj);
                     } else {
                       PyErr_SetString(PyExc_TypeError, buf);
                       return NULL;
                     }
                     k++;
                  }
	       }
	   }
	}

stop:	if (missing_end) {
           index_s[(j-1)] = -1;
           PyErr_SetString(PyExc_TypeError, "Missing end value.");
	   return NULL;
	}

        /* Return null python object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* Set the plotting region to default values. */
static PyObject *
PyVCS_resetplotregion(PyVCScanvas_Object *self, PyObject *args)
{
	int i;

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

 	/* Reset the default plot region */
	for (i=0; i<CU_MAX_VAR_DIMS; i++) {
	   index_s[i] = -1;
	   index_e[i] = -1;
	}

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

char *
get_background(template,type,graphics,background)
  char * *template;
  char * *type;
  char * *graphics;
  char * background;
{
	char		*rstr=NULL;

	if ( (*template != NULL) && (strcmp(*template,"bg")==0) ) {
            rstr = (char *) malloc(strlen(*template) + 1);
            strcpy(rstr,*template);
	    *template = NULL;
	} else if ( (*type != NULL) && (strcmp(*type,"bg")==0) ) {
            rstr = (char *) malloc(strlen(*type) + 1);
            strcpy(rstr,*type);
	    *type = NULL;
	} else if ( (*graphics != NULL) && (strcmp(*graphics,"bg")==0) ) {
            rstr = (char *) malloc(strlen(*graphics) + 1);
            strcpy(rstr,*graphics);
	    *graphics = NULL;
	} else if ( (background != NULL) && (strcmp(background,"bg")==0) ) {
            rstr = (char *) malloc(strlen(background) + 1);
            strcpy(rstr,background);
	}

	return rstr;
}

void get_min_max_from_data(int which_slab, float *min, float *max)
{
       VCSCANVASLIST_LINK vptr=head_canvas_list;
       int i, len=1;
       int *idata;
       float *fdata;

       *min=1.e20;
       *max=-1.e20;
/*
       if (vptr->slab->descr->type == 102) {
          if (which_slab == 0) {
             if (vptr->slab == NULL) return;
             for (i=0; i<vptr->slab->nd; i++)
                len*=vptr->slab->dimensions[i];
             fdata=(float *)vptr->slab->data;
             for (i=0; i<len; i++) {
                  if (fdata[i] != (float) vptr->slab->missing) {
                       *min=(fdata[i] < *min)? fdata[i] : *min;
                       *max=(fdata[i] > *max)? fdata[i] : *max;
                  }
             }
          } else {
             if (vptr->slab2 == NULL) return;
             for (i=0; i<vptr->slab2->nd; i++)
                len*=vptr->slab2->dimensions[i];
             fdata=(float *)vptr->slab2->data;
             for (i=0; i<len; i++) {
                  if (fdata[i] != (float) vptr->slab2->missing) {
                       *min=(fdata[i] < *min)? fdata[i] : *min;
                       *max=(fdata[i] > *max)? fdata[i] : *max;
                  }
             }
          }
       } else {
          if (which_slab == 0) {
             if (vptr->slab == NULL) return;
             for (i=0; i<vptr->slab->nd; i++)
                len*=vptr->slab->dimensions[i];
             idata=(int *)vptr->slab->data;
             for (i=0; i<len; i++) {
                  if (idata[i] != (int) vptr->slab->missing) {
                       *min=(idata[i] < *min)? idata[i] : *min;
                       *max=(idata[i] > *max)? idata[i] : *max;
                  }
             }
          } else {
             if (vptr->slab2 == NULL) return;
             for (i=0; i<vptr->slab2->nd; i++)
                len*=vptr->slab2->dimensions[i];
             idata=(int *)vptr->slab2->data;
             for (i=0; i<len; i++) {
                  if (idata[i] != (int) vptr->slab2->missing) {
                       *min=(idata[i] < *min)? idata[i] : *min;
                       *max=(idata[i] > *max)? idata[i] : *max;
                  }
             }
          }
       }
*/
}

void set_animation_graphics_method(char *g_name)
{
       static PyArrayObject *slab=NULL,*slab2=NULL;
       VCSCANVASLIST_LINK vptr=head_canvas_list;
       static char hold_g_name[100];    /* Hold animation graphics name */

       if (strcmp(g_name,"") == 0) {
          strcpy(vptr->graphics, hold_g_name);
/*
          vptr->slab = slab;
          vptr->slab2 = slab2;
*/
       } else {
          strcpy(hold_g_name, vptr->graphics);
          strcpy(vptr->graphics, g_name);
/*
          slab = vptr->slab;
          slab2 = vptr->slab2;
*/
       }
}

void free_animation_list()
{
        VCSCANVASLIST_LINK              tvptr,vptr;

        /* Remove the canvas link list information used for animation */
        tvptr=vptr=head_canvas_list;

        while (tvptr != NULL) {
           tvptr = vptr->next;
           free((char *) vptr);
           vptr = tvptr;
        }

        head_canvas_list = NULL;
}


/* 
 * Draw the data (or arrayobject) in the VCS Canvas. If no data is given, 
 * then an error is returned. The template and graphics method are optional.
 * If the template or graphics method are not given, then the default
 * settings will be used. See the PyVCS_set routine to set the template
 * and graphics method.
 */
/*
 * This routine expects slabs to be CDMS TransientVariable objects, floating point type.
 */
PyObject *
PyVCS_plot(PyVCScanvas_Object *self, PyObject *args)
{
	PyObject		*hold, *slab=NULL, *hold2, *slab2=NULL;
        PyObject        	*cuslab_name;
        PyThreadState 		*_save;
	VCSCANVASLIST_LINK      tptr=NULL, vptr=head_canvas_list;
        canvas_display_list 	*cdptr, *tcdptr;
	char 			*template, *graphics, *type, *bgopt;
	char			template2[100], graphics2[100], type2[100];
/* 	static char		template3[100], graphics3[100], type3[100]; */
	char 			buf[MAX_NAME],d_name[MAX_NAME], a_name[6][17];
        char            	s_name[2][MAX_NAME];
	int 			i, ier, rank, rank2, *shape, *shape2;
#ifdef X11WM
        XWindowAttributes 	xwa;
        int 			map_state=IsUnmapped;
#endif
	int doing_animation = 0;
/* 	struct a_tab    	*ptab; */
/* 	static PyVCScanvas_Object 	*save_self=NULL; */
        extern struct a_tab 	A_tab;
	struct p_tab    	*ttab;
        extern struct p_tab 	Pic_tab;
	void put_slab_in_VCS_data_struct();
	int slabRank(PyObject* slab);
	int* slabShape(PyObject* slab);
	int graphics_num_of_dims();
	int set_plot_minmax();
	extern int python_display();
	extern void store_display_name();
	extern void deactivate_all_other_wksts();
#ifdef X11WM
	extern Pixmap copy_pixmap(Gconid_X_drawable connect_id,int canvas_id);
#endif
	extern struct orientation       Page;

	char animbg[4];
	strcpy(animbg,"bg");
	/* If the GUI was not stated (i.e., cdatgui), then we need to
	 * process all the X events before we move on.
	 */
	if (not_using_gui)
	   process_cdat_events();

 	/* Check to see if self and args are NULL. If so, then
	 * the routine was called from animation. If called from animation,
	 * then use static values that were set for self and slab.
         */
	if ((self == NULL) && (args == NULL)) {
           /* The animation cannot remember previous data displayed on past
	    * VCS Canvases. The user must reload the plot image in order to 
	    * animation on previous plots.
            */
           doing_animation = 1;
           _save = PyEval_SaveThread();
           Py_BLOCK_THREADS

           if (vptr == NULL) {
              PyErr_SetString(VCS_Error, "The VCS animation cannot remember data displayed on a past canvas. You must redisplay the plot the canvas in order to animate it.");
              return NULL;
           }
an_loop:
	   /* Point to the correct structure */
#ifdef X11DRAW
	   while (connect_id.drawable != vptr->connect_id.drawable)
                 vptr = vptr->next;
#else
	   while (connect_id.cr !=  vptr->connect_id.cr)
	     vptr = vptr->next;
#endif
           if (vptr == NULL) {
              Py_INCREF(Py_None);
              return Py_None;
	   }
           self  = vptr->self;
           slab  = vptr->slab;
           slab2 = vptr->slab2;
           strcpy(template2, vptr->template);
           strcpy(graphics2, vptr->graphics); 
           strcpy(type2, vptr->type);
/* DUBOIS - is this right? this is what old code did in effect */
           self->background = NULL; 
	} else {
	   /* Get slab and primary attributes. */
  	   if(!PyArg_ParseTuple(args,"OOssss",
              &hold, &hold2,&template,&type,&graphics,&bgopt)) {
               return NULL;
           }
           if (strcmp(bgopt,"bg")==0) {
              self->background = bgopt;
           } else {
              self->background = NULL;
           }
	   if (doing_animation==1) {
	     self->background = animbg;
	     fprintf(stderr,"ok we set animbg and bg to: %s, %s\n",animbg,self->background);
	     sleep(6);
	   }
           if (hold == Py_None)
              slab = NULL;
	   else 
              slab = hold;
           if (hold2 == Py_None)
              slab2 = NULL;
	   else 
              slab2 = hold2;
	   if ( ((slab == NULL) || (!slabCheck(slab))) &&
                   (cmpncs(type, "continents") != 0) &&
                   (cmpncs(type, "line") != 0) &&
                   (cmpncs(type, "marker") != 0) &&
                   (cmpncs(type, "fillarea") != 0) &&
                   (cmpncs(type, "text") != 0) ) {
		 PyErr_SetString(VCS_Error, "Array must be a CDMS TransientVariable.");
                 return NULL;
	   } 

	   if (self == NULL) {
		 PyErr_SetString(VCS_Error, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
                 return NULL;
	   }

           strcpy(template2, template);
           ier = graphics_num_of_dims(type, 0,1);
           if (ier == -1) {
                pyoutput("Error - Unknown graphics method.", 1);
                pyoutput(type, 1);
                pyoutput("CDAT will use default graphics method boxfill.",1);
                strcpy(type2, "boxfill");
           } else {
                strcpy(type2, type);
	   }
	   if (((cmpncs(type2, "Vector") == 0) || (cmpncs(type2, "Scatter") == 0) ||
		  (cmpncs(type2, "XvsY") == 0)) && ((slab2 == NULL) || 
                  (!slabCheck(slab2)))) {
		  PyErr_SetString(VCS_Error, "Array must be a CDMS TransientVariable for second input.");
                  return NULL;
           }
           strcpy(graphics2, graphics);

           /* Create the canvas link list */
           if((tptr=(VCSCANVASLIST_LINK)malloc(sizeof(VCSCANVASLIST)))==NULL){
             PyErr_SetString(VCS_Error, "Can not create 'VCS Canvas Link List'!");
	     return NULL;
           }

           /* Save VCS Canvas info in structure. This is needed for 
            * animation.
            */
/* DUBOIS -- shouldn't there be some incref'ing here ? */
           tptr->self  = self;
           tptr->slab  = slab;
           self->frame_count = 0; /* New data then new animation frame count */
           if (slab != NULL) Py_INCREF(slab);
           tptr->slab2 = slab2;
           if (slab2 != NULL) Py_INCREF(slab2);
           strcpy(tptr->template, template2);
           strcpy(tptr->graphics, graphics2); 
           strcpy(tptr->type, type2);
           tptr->next = NULL;

	   /* Link structure in list */
	   if (vptr == NULL)
	      head_canvas_list = tptr;
	   else {
	     while (vptr->next != NULL)
                   vptr = vptr->next;
             vptr->next = tptr;
	   }
	}
heartbeat("template name %s", template2);
heartbeat("graphics method %s", type2);
heartbeat("graphics option %s", graphics2);
	/* Find a unique plot name in VCS data table. Always put new slab
         * into the VCS data table. Only clear when the user selects the 
         * clear button.
	 */
        s_name[0][0] = '\0';
        s_name[1][0] = '\0';
        if (slab != NULL) { /* do the same for the second slab, if needed */
	   sprintf(s_name[0], "plot_%d", namecount);
heartbeat("Slab name set to %s", s_name[0]);
           ++namecount; /* for unique plot name */
           cuslab_name = PyString_FromString(s_name[0]);
           if(!cuslab_name) {
               PyErr_SetString(PyExc_RuntimeError, "Cannot create slab name.");
               return NULL;
           }
/* DUBOIS -- we set an attribute in the input; necessary? */
           if(PyObject_SetAttrString(slab, "cuslab_name", cuslab_name) == -1) {
               PyErr_SetString(PyExc_RuntimeError, "Cannot set slab name.");
               Py_DECREF(cuslab_name);
               return NULL;
           }
           Py_DECREF(cuslab_name);
	}
        if (slab2 != NULL) { /* do the same for the second slab, if needed */
	   sprintf(s_name[1], "plot_%d", namecount);
heartbeat("Slab2 name set to %s", s_name[0]);
           ++namecount; /* for unique plot name */
           cuslab_name = PyString_FromString(s_name[1]);
           if(!cuslab_name) {
               PyErr_SetString(PyExc_RuntimeError, "Cannot create slab2 name.");
               return NULL;
           }
           if(PyObject_SetAttrString(slab2, "cuslab_name", cuslab_name) == -1) {
               PyErr_SetString(PyExc_RuntimeError, "Cannot set slab2 name.");
               Py_DECREF(cuslab_name);
               return NULL;
           }
           Py_DECREF(cuslab_name);
	}

	/* Put the slab and its information into VCS's data table with
	 * a 'c' followed by it to indicate that it is a computed value.
	 * (e.g., psl (3D)c).
	 */
	if (slab != NULL) {
           heartbeat("%s", "Putting slab 1");
           slabCheck(slab); /* Temporary bug fix for animation. Without this command, no animation frames are produced. */
	   put_slab_in_VCS_data_struct(slab, type2, s_name[0], self->frame_count, doing_animation, 1);
	   if ((self == NULL) && (args == NULL) && (slab != NULL)) Py_DECREF(slab);
	}
	if (slab2 != NULL) { /* do the same for the second slab, if needed */
	   slabCheck(slab2);
           heartbeat("%s", "Putting slab 2");
	   if (cmpncs(type2, "meshfill")==0) /* meshfill needs special care for dimensions agreement */
	     {
	       shape=(int *)slabShape(slab);
	       shape2=(int *)slabShape(slab2);
	       rank=slabRank(slab);
	       rank2=slabRank(slab2);
	       if (shape2[rank2-2]!=2)
		 {
		       PyErr_SetString(PyExc_RuntimeError, "slab2 2nd to last dimension must be of length 2");
		       return NULL;
		 }
	       for (i=0;i<rank2-2;i=i+1)
		 {
/* 		   fprintf(stderr,"rank, rank2, i, shape[i], shape2[i] %i, %i, %i, %i, %i\n",rank,rank2,i,shape2[rank2-i-3],shape[rank-i-1]); */
		   if (shape2[rank2-i-3]!=shape[rank-1])
		     {
		       PyErr_SetString(PyExc_RuntimeError, "slab and slab2 shapes not compatible ");
		       return NULL;
		     }
		 }
	       if (rank!=rank2-2)
		 {
		   /* mesh is not repeating itself for ever and ever after using a copy for extra dims !*/
		   put_slab_in_VCS_data_struct(slab2,type2, s_name[1], self->frame_count, doing_animation, 2);
		 }
	       else
		 {
		   put_slab_in_VCS_data_struct(slab2,type2, s_name[1], self->frame_count, doing_animation, 2);
		 }
	       free(shape);
	       free(shape2);
	     }
	   else
	     {
	       put_slab_in_VCS_data_struct(slab2,type2, s_name[1], self->frame_count, doing_animation, 2);
	     }
           if(PyErr_Occurred()) return NULL;
	   if ((self == NULL) && (args == NULL) && (slab2 != NULL)) Py_DECREF(slab2);
	}

	/* Check to see if the template name is in the template table.
	 * If not, then use the default setting.
         */
	ttab=&Pic_tab;
	while ((ttab != NULL)&&(strcmp(ttab->name,template2) != 0)) {
           ttab=ttab->next;
        }
	if (ttab == NULL) {
	   if (self->template_name != NULL)
	         free((char *) self->template_name);
           if ((self->template_name =
             (char *) malloc((strlen("default")+1)*sizeof(char)+1)) == NULL) {  
             PyErr_SetString(VCS_Error, "No memory for the template name.");
             return NULL;
           } else {
             strcpy(self->template_name, "default");
             strcpy(template2, "default");
             sprintf(buf,"'Template' is currently set to P_%s.\n", self->template_name);
	     /*pyoutput(buf,0);*/
           }
	}

	/* Check to see if the graphics name is in the graphics table.
	 * If not, then use the default setting.
         */
        heartbeat("VCS_plot progress report %s", s_name[0])
	if (slab != NULL) {
	   /* Set the array names from the slab name. *
	   strncpy(self->a_name[0], s_name[0], 17);*/

	   /* Create a unique display name from the slab name */
	   sprintf(d_name, "dpy_%s", s_name[0]);
        } else { /* must be a continents plot, so set d_name manually */
	   /* Create a unique display name from the slab name */
	   sprintf(d_name, "dpy_cont%d", namecount);
           ++namecount; /* for unique plot name */
	}

        /* Create a display name structure for linked list */
        cdptr = (canvas_display_list *)malloc(sizeof(canvas_display_list));
        if ((cdptr->display_name = (char *) malloc((
             strlen(d_name)+1)*sizeof(char)+1)) == NULL) {
	     pyoutput("Error - No memory for the display name.", 1);
	} else {
           strcpy(cdptr->display_name, d_name);
	}
        if (doing_animation != 1)
           strcpy(tptr->d_name, d_name); /* Store d_name in animation structure */
        heartbeat("VCS_plot progress report d_name %s", d_name)
	cdptr->next = NULL;

	if (self->dlist == NULL)
	   self->dlist = cdptr;
	else {
	   tcdptr = self->dlist;
	   while (tcdptr->next != NULL)
	       tcdptr = tcdptr->next;
	   tcdptr->next = cdptr;
	}

        heartbeat("VCS_plot progress report 2 d_name %s", d_name)
#ifdef X11WM
        if ((self->connect_id.display != NULL) &&
           (self->connect_id.drawable != 0)) {
           XGetWindowAttributes(self->connect_id.display, 
                             self->connect_id.drawable, &xwa);
           map_state = xwa.map_state;
        }
#elif defined (QTWM)
	/* not needed we put it later as a isViewable func */
#else
	printf("insert your WM map state call here\n");
#endif
        /* If the VCS Canvas is not open, then open it for the 1st time */
        if (self->background == NULL) {
          heartbeat("VCS_plot progress report 3 d_name %s", d_name)
#ifdef X11DRAW
          if (self->connect_id.canvas_drawable == 0) {
#else
	  if (self->connect_id.cr == NULL) {
#endif
             heartbeat("VCS_plot progress report 4 d_name %s", d_name)
             PyVCS_open(self, args);
             heartbeat("VCS_plot progress report 5 d_name %s", d_name)
          }

#ifdef X11WM
          /* Make sure the VCS canvas is viewable. */
	  else if (map_state != IsViewable) {
             heartbeat("VCS_plot progress report 6 d_name %s", d_name)
             PyVCS_open(self, args);
             heartbeat("VCS_plot progress report 7 d_name %s", d_name)
          }
#elif defined (QTWM)
	  //fprintf(stderr,"insert here QT is window viewable call\n");
#else
	  fprintf(stderr,"insert your WM map state is viewable call here\n");
#endif
	  /* If the VCS canvas has been created but closed, then open it */
	  else if (self->virgin == 2) {
             heartbeat("VCS_plot progress report 8 d_name %s", d_name)
             PyVCS_open(self, args);
             heartbeat("VCS_plot progress report 9 d_name %s", d_name)
          }
          heartbeat("VCS_plot progress report 10 d_name %s", d_name)

	  /* Set up the VCS Canvas and XGKS workstation */
          setup_canvas_globals(self);

	  /* Must have display name for the VCS Canvas needs. That is, it
           * is needed for canvas resize and refresh.
           */
          heartbeat("VCS_plot progress report before store d_name %s", d_name)
	  store_display_name(self->connect_id, d_name);
          heartbeat("VCS_plot progress report after store d_name %s", d_name)
        } else {
          Wkst[0].id = 2; /* cgm workstation */
	}

	deactivate_all_other_wksts(Wkst[0].id);

	/* Save the connection ID for the animation loop (malloc'ed above) */
	if (tptr != NULL)
           tptr->connect_id = connect_id;

	/* Set the graphics method's minimum and maximum values and 
	 * extensions.
	set_plot_minmax(self, type2, graphics2, namecount);
	 */

	/* Setup the VCS display to show the plot on the VCS Canvas. */
	if (slab != NULL)
	   strcpy(a_name[0], s_name[0]);
	if (slab2 != NULL)
	   strcpy(a_name[1], s_name[1]);
        heartbeat("before call python_display %s", d_name)

/*         To ensure the full completion of the plot, we have to grab the pointer
 	   and keyboard and give it to the VCS Canvas.  *
           *
           * I need this commented out or the Template editor will 
           * seg fault on Linux 9.x platforms.
           *
         if ((self->connect_id.display != NULL) && (self->connect_id.drawable != 0)) {
 	   XSync(self->connect_id.display,FALSE);
            XGrabPointer(self->connect_id.display, self->connect_id.drawable, True, 0,
               GrabModeSync, GrabModeAsync, None, None, (Time) CurrentTime);
 	   XSync(self->connect_id.display,FALSE);
            XGrabKeyboard(self->connect_id.display, self->connect_id.drawable, True,
               GrabModeSync, GrabModeAsync, (Time) CurrentTime);
 	   XSync(self->connect_id.display,FALSE);
 	   }
         */
	  
        /* Display the plot on the VCS Canvas */
        if (self->orientation == 0 ) /* Set the page orientation before plotting */
           strcpy(Page.page_orient,"landscape");
        else
           strcpy(Page.page_orient,"portrait");

	ier = python_display(a_name, template2, type2, graphics2, d_name);
#ifdef X11WM
	int screen_num;
	GC gc;
	screen_num = DefaultScreen(connect_id.display);

        gc = DefaultGC(connect_id.display,screen_num);

	XGetWindowAttributes(connect_id.display, connect_id.drawable, &xwa);
/* 	fprintf(stderr,"depth, cdepth, Xw, Yw: %i,%i,%i, %i\n",xwa.depth,cairo_xlib_surface_get_depth(connect_id.surface),xwa.width,xwa.height); */
/* 	fprintf(stderr,"cairo: status surf, status cr: %s, %s\n",cairo_status_to_string(cairo_surface_status(connect_id.surface)),cairo_status_to_string(cairo_status(connect_id.cr))); */
/* 	fprintf(stderr,"cairo: width, heiight %i,%i\n",cairo_image_surface_get_width(self->connect_id.surface), */
/* 	       cairo_image_surface_get_height(self->connect_id.surface)); */
/* 	tmppix = XCreatePixmapFromBitmapData(self->connect_id.display,self->connect_id.drawable, */
/* 					     cairo_image_surface_get_data(self->connect_id.surface), */
/* 					     cairo_image_surface_get_width(self->connect_id.surface), */
/* 					     cairo_image_surface_get_height(self->connect_id.surface), */
/* 					     1,1, */
/* 					     24); */
  
        /*   XSync(connect_id.display,FALSE);
           XFlush(connect_id.display);*/
/* 	XCopyArea(connect_id.display, connect_id.draw_pixmap, */
/* 		  connect_id.drawable, gc, 0,0, xwa.width, xwa.height, 0, 0); */

	//XFreePixmap(connect_id.display,tmppix);

        if (self->connect_id.display != NULL) {
	   XSync(self->connect_id.display,FALSE);
           XFlush(self->connect_id.display);
        }
/* 	fprintf(stderr,"ok this is where i want to update for now\n"); */
#elif defined (QTWM)
	vcs_legacy_Qt_repaint_window_by_id(self->connect_id.wkst_id); 

#else
	printf("insert here your WM sync and flush functions\n");
#endif



/*         Okay! Release the pointer and keyboard *
         if ((self->connect_id.display != NULL) && (self->connect_id.drawable != 0)) {
 	   XSync(self->connect_id.display,FALSE);
            XUngrabPointer(self->connect_id.display, (Time) CurrentTime);
 	   XSync(self->connect_id.display,FALSE);
            XUngrabKeyboard(self->connect_id.display, (Time) CurrentTime);
 	   XSync(self->connect_id.display,FALSE);
 	   }
         */
	
        heartbeat("after call python_display %s", d_name)

	/* copy the current VCS canvas to the pixmap (i.e., backing_store) */
        if (self->background == NULL) {
#ifdef X11WM
   	  if (self->connect_id.canvas_pixmap != (Pixmap) NULL) {
              XFreePixmap(self->connect_id.display, self->connect_id.canvas_pixmap);
              self->connect_id.canvas_pixmap = (Pixmap) NULL;
          }
	  self->connect_id.canvas_pixmap = copy_pixmap(self->connect_id, self->canvas_id);
#elif defined QTWM
	  /* nothing to do for QT */
#else
	  fprintf(stderr,"insert here your WM pixmap cache function\n");
#endif
        }
        /*DEAN - 9/06/06 if ((doing_animation == 1) && (vptr->next == NULL)) {*/
        if (doing_animation == 1) {
            Py_UNBLOCK_THREADS
            PyEval_RestoreThread(_save);
            ++self->frame_count;
        } else if ((doing_animation == 1) && (vptr->next != NULL)) {
	     vptr = vptr->next;
             goto an_loop;
        }

#ifdef X11WM
        if (self->connect_id.display != NULL) {
	   XSync(self->connect_id.display,FALSE);
           XFlush(self->connect_id.display);
        }
#elif defined (QTWM)
	vcs_legacy_Qt_repaint_window_by_id(self->connect_id.wkst_id);

#else
	printf("insert here your WM sync and flush functions\n");
#endif

        if ( (doing_animation == 1) && (self->gui == 1) ) {
	  update_gui_canvas_counter(self );
	}


        return Py_BuildValue("s",d_name);
}

static PyObject *
PyVCS_savecontinentstype(PyVCScanvas_Object *self, PyObject *args)
{
	int 			cont_type;

        /* Save the continent's type for resize */
        if(!PyArg_ParseTuple(args, "i", &cont_type)) {
           PyErr_SetString(PyExc_TypeError, "Error - Must provide a continents number 0 through 11.");
           return NULL;
        } else {
           self->savecontinents = cont_type;
	}

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
  	return Py_None;
}

/* Set the default continents. One has the option of using continental maps
 * that are predefined or that are user-defined. Predefined continental maps 
 * are either internal to VCS or are specified by external files. User-defined 
 * continental maps are specified by additional external files that must be 
 * read as input. 
 */
static PyObject *
PyVCS_setcontinentstype(PyVCScanvas_Object *self, PyObject *args)
{
	int 					cont_type;
	extern struct default_continents 	Dc;

        if(!PyArg_ParseTuple(args, "i", &cont_type)) {
           PyErr_SetString(PyExc_TypeError, "Error - Must provide a continents number 0 through 11.");
           return NULL;
        } else {
	   if ((cont_type < 0) || (cont_type > 12)) cont_type = 1;
           Dc.selected = cont_type; /* Change the setting of the continent's type */
	}

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
  	return Py_None;
}

/* Get the default continents type.
 */
static PyObject *
PyVCS_getcontinentstype(PyVCScanvas_Object *self, PyObject *args)
{
        extern struct default_continents        Dc;

	/* Return the continents type */
        return Py_BuildValue("i",Dc.selected);
}

/* Set up the default VCS minimum and maximum values. That is, no matter
 * what the data's minimum and maximum values are they will be set to
 * the given value. If the underflow extension is set to 1, then the
 * underflow arrow will be shown on the plot. If the overflow extension
 * is set to 1, the overflow arrow will shown of on the plot.
 * Must specify at least the minimum and maximum values.
 */
static PyObject *
PyVCS_setminmax(PyVCScanvas_Object *self, PyObject *args)
{
	PyObject        *obj;
	int 		ier, i, argc, y;
	char 		buf[1024];

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	self->vcs_legacy_min = 1e20; self->vcs_legacy_max = -1e20;
	self->vcs_legacy_ext1 = 0, self->vcs_legacy_ext2 = 0;
  	/* Parse the input argument string */
  	if (args == NULL) { /* check for no input */
              sprintf(buf, "Info - No arguments given. %d - VCS Canvas wil use\n        minimum and maximum values obtained from the data.", self->canvas_id);
           PyErr_SetString(PyExc_TypeError, buf);
           return NULL;
        } else if (!PyTuple_Check (args)) { /* check to see if it's Tuple */
           PyErr_SetString(PyExc_TypeError, "Arguments are incorrect.");
           return NULL;
	} else { /* get the minimum, maximum, and extension values */
	   argc = PyTuple_Size (args); /* get the number of arguments */
  	   if (argc == 0) { /* check for no input */
              sprintf(buf, "Info - No arguments given. %d - VCS Canvas will use\n        minimum and maximum values obtained from the data.", self->canvas_id);
              PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
           } else if (argc == 1) { /* check for only one input */
              sprintf(buf, "Info - Not enough arguments given. %d - VCS Canvas will\n        use minimum and maximum values obtained from the data.", self->canvas_id);
              PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
	   }
	   for (i = 0; i < argc; i++) {
	       obj = PyTuple_GetItem (args, i); /* get argument */
               if(PyInt_Check(obj)) { /* check for integer */
                  if (i == 0)
                     self->vcs_legacy_min = (double) PyInt_AsLong(obj);
                  else if (i == 1)
                     self->vcs_legacy_max = (double) PyInt_AsLong(obj);
                  else if (i == 2)
                     self->vcs_legacy_ext1 = (int) PyInt_AsLong(obj);
                  else if (i == 3)
                     self->vcs_legacy_ext2 = (int) PyInt_AsLong(obj);
               } else if(PyFloat_Check(obj)) { /* check for float */
                  if (i == 0)
                     self->vcs_legacy_min = (double) PyFloat_AsDouble(obj);
                  else if (i == 1)
                     self->vcs_legacy_max = (double) PyFloat_AsDouble(obj);
                  else if (i == 2)
                     self->vcs_legacy_ext1 = (int) PyFloat_AsDouble(obj);
                  else if (i == 3)
                     self->vcs_legacy_ext2 = (int) PyFloat_AsDouble(obj);
               } else {
                  if (i == 0)
                     sprintf(buf, "Error - Incorrect minimum argument. Using minimum value from data.");
                  else if (i == 1)
                     sprintf(buf, "Error - Incorrect maximum argument. Using maximum value from data.");
                  else if (i == 2)
                     sprintf(buf, "Error - Incorrect underflow argument. Will not display underflow arrow on plot.");
                  else if (i == 3)
                     sprintf(buf, "Error - Incorrect overflow argument. Will not display overflow arrow on plot.");
                  pyoutput(buf, 1);
	       }
	   }
	}
	if (self->vcs_legacy_min == 1e20)
           sprintf(buf, 
             "Info - %d. - VCS Canvas default minimum value will be set by the data.", self->canvas_id);
	else
           sprintf(buf, "Info - %d. - VCS Canvas default minimum value is set to %g.", self->canvas_id, self->vcs_legacy_min);
        pyoutput(buf, 1);
	if (self->vcs_legacy_max == -1e20)
           sprintf(buf, 
             "Info - %d. - VCS Canvas default maximum value will be set by the data.", self->canvas_id);
	else
           sprintf(buf, "Info - %d. - VCS Canvas default maximum value is set to %g.", self->canvas_id, self->vcs_legacy_max);
        pyoutput(buf, 1);
	if (self->vcs_legacy_ext1 == 0)
           sprintf(buf,
                 "Info - %d. - VCS Canvas underflow extension is not set.", self->canvas_id);
	else
           sprintf(buf,
                 "Info - %d. - VCS Canvas underflow arrow will be displayed.", self->canvas_id);
        pyoutput(buf, 1);
	if (self->vcs_legacy_ext2 == 0)
           sprintf(buf, 
                 "Info - %d. - VCS Canvas overflow extension is not set.", self->canvas_id);
	else
           sprintf(buf, 
                 "Info - %d. - VCS Canvas overflow arrow will be displayed.", self->canvas_id);
        pyoutput(buf, 1);

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* This routine will allow Python the ability to read in a VCS script.
 * This is handy when working with the VCS interface and CDAT.
 */
static PyObject *
PyVCS_scriptrun(PyVCScanvas_Object *self, PyObject *args)
{
	char *vcs_legacy_script=NULL;
	char buf[1024];
	int tmp = -99, ier;
	extern int procRun();

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

  	if(PyArg_ParseTuple(args, "|s", &vcs_legacy_script)) {
	   if ((vcs_legacy_script == NULL) || (vcs_legacy_script[0] == '\0')) {
	      PyErr_SetString(PyExc_TypeError, "No VCS script name given.");
              return NULL;
	   } else {
	      procRun(vcs_legacy_script, &tmp);
/*              sprintf(buf, "Read VCS script %s.",vcs_legacy_script);
 	      pyoutput(buf, 1);*/
	   }
	}

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
  	return Py_None;
}

/* In VCS it is necessary to clear all the graphics from a page. This
 * routine will clear all the VCS displays on a page (i.e., the VCS
 * Canvas).
 */
PyObject *
PyVCS_clear(PyVCScanvas_Object *self, PyObject *args)
{
	VCSCANVASLIST_LINK     		tvptr,vptr;
        struct display_tab      	*dtab;
	extern struct display_tab 	D_tab;
        canvas_display_list 		*cdptr, *tcdptr;
	graphics_method_list		*gptr, *tgptr;
	int				i, gnarray;
	int				graphics_num_of_arrays();
        char 				a_name[6][17];
	extern int 			clear_display();
#ifdef X11WM
	extern Pixmap			copy_pixmap(Gconid_X_drawable connect_id,int canvas_id);
#endif
        extern int 			clearCanvas();
	extern int              	removeGfb_name();
	extern int 			removeA();
        extern void		        dispatch_the_next_event();

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	/* DNW - June 1, 2000, this function re-sets the global connect_id which 
		causes a problem when "x=vcs_legacy.init()" is called back-to-back. Also
		causes problems when "x.clear()" is called before "x.plot(s)". In
		fact, "self" has its own connect_id, so removing the global set to
		connect_id is okay. I'll keep watch!

		Note: If this doesn't work for any reason, then set a placement holder
		for connect_id and restore its value at the end of this routine.

		August 17, 2000, put in the if check to test for canvas. If two canvases
		were up, then VCS would become confused as to which canvas to clear.

	        Set up the VCS Canvas and XGKS workstation */
#ifdef X11DRAW
        if (self->connect_id.canvas_drawable != 0)
#else
	  if (self->connect_id.cr != NULL )
#endif
           setup_canvas_globals(self);

	/* Remove the display from the VCS picture form and
	 * remove all the data from the VCS data table 
         */
  	cdptr = self->dlist;
	while (cdptr != NULL) {
	   dtab=&D_tab;
           while ((dtab != NULL) &&
                 (strcmp(dtab->name, cdptr->display_name) != 0))
                 dtab = dtab->next;
	   if (dtab == NULL) break;/* must have been removed from animation */
	   gnarray = graphics_num_of_arrays(dtab->type);
	   for (i=0; i<gnarray; i++)
               strcpy(a_name[i], dtab->a[i]);
	   clear_display(cdptr->display_name);
           remove_display_name(self->connect_id, cdptr->display_name); /*remove display name*/
	   for (i=0; i<gnarray; i++)                 /*from VCS Canvas info*/
	      removeA(a_name[i]);
	   tcdptr = cdptr;
	   cdptr = cdptr->next;
           free((char *) tcdptr->display_name);
           free((char *) tcdptr);
	}
  	self->dlist = NULL;

  	/* Remove the temporary graphics methods used to create set 
        * minimum and maximum plots. 
        */
	gptr = self->glist;
	while (gptr != NULL) {
	    tgptr = gptr;
	    gptr = gptr->next;
	    if (strcmp(tgptr->g_type, "Boxfill") == 0)
	       removeGfb_name(tgptr->g_name);
	    free((char *) tgptr->g_type);
	    free((char *) tgptr->g_name);
	    free((char *) tgptr);
	}
	self->glist = NULL;

	/* Remove the canvas link list information used for animation */
	tvptr=vptr=head_canvas_list;
#ifdef X11WM
	while ((vptr != NULL) && (vptr->connect_id.drawable !=
               self->connect_id.drawable)) {
#elif defined QTWM
	while ((vptr != NULL) && (vptr->connect_id.cr !=
               self->connect_id.cr)) {
#endif
	     tvptr = vptr;
             vptr = vptr->next;
	}
	if ((tvptr != NULL) && (vptr != NULL)) {
           while (vptr != NULL) {
              tvptr = vptr;
              vptr = vptr->next;
              if (tvptr->slab != NULL) Py_DECREF(tvptr->slab);
              if (tvptr->slab2 != NULL) Py_DECREF(tvptr->slab2);
	      free((char *) tvptr);
           }
	   head_canvas_list = NULL;
	}

        /*
         * Make sure the VCS canvas is up and running
         * before copying the blank canvas to the pixmap backingstore.
        if (self->connect_id.drawable != 0) {
     	   *    clearCanvas(); * blank the VCS Canvas *
	   self->connect_id.canvas_pixmap = copy_pixmap(self->connect_id);
	}
         */
#ifdef X11WM
        /* Remove the backing store pixmap */
        if ( (self->connect_id.display != NULL) && (self->connect_id.drawable != 0) )
           XClearWindow(self->connect_id.display, self->connect_id.drawable);
        if (self->connect_id.canvas_pixmap != (Pixmap) NULL) {
              /*printf("CLEAR 1: canvas_pixmap %d = %d\n", self->canvas_id, self->connect_id.canvas_pixmap);*/
              XFreePixmap(self->connect_id.display, self->connect_id.canvas_pixmap);
              /*self->connect_id.canvas_pixmap=create_pixmap(self->connect_id);*/
              self->connect_id.canvas_pixmap = (Pixmap) NULL;
              /*printf("CLEAR 2: canvas_pixmap %d = %d\n", self->canvas_id, self->connect_id.canvas_pixmap);*/
        }
#elif defined QTWM
	extern  void vcs_legacy_Qt_clear_window_by_id(int id);
	vcs_legacy_Qt_clear_window_by_id(self->connect_id.wkst_id);
#else
	printf("insert here your WM clear and pixamp backing store removal func\n");
#endif
        dispatch_the_next_event();

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
  	return Py_None;
}

/* In VCS it is necessary to clear all the graphics from a page. This
 * routine will clear all the VCS displays that were created during the
 * animation process.
 */
void animation_clear()
{
	PyVCScanvas_Object 		*self;
	VCSCANVASLIST_LINK     		tvptr,vptr;
	int				i, gnarray;
        struct display_tab      	*dtab;
	extern struct display_tab 	D_tab;
        canvas_display_list 		*cdptr, *tcdptr;
	int				graphics_num_of_arrays();
        char 				a_name[6][17];
	char				dname[MAX_NAME];
	extern int 			clear_display();
	extern int 			removeA();

         /* Called from animation, use static values that were set for self.
         */
        vptr=head_canvas_list;
#ifdef X11WM
        while ((vptr != NULL) && (vptr->connect_id.drawable !=
               connect_id.drawable)) {
#elif defined QTWM
        while ((vptr != NULL) && (vptr->connect_id.cr !=
               connect_id.cr)) {
 #endif
             vptr = vptr->next;
        }
        if (vptr == NULL) return ;
        self = vptr->self;

        /* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

        /* Remove the display from the VCS picture form and
         * remove all the data from the VCS data table
         */
        cdptr = self->dlist;
        while (cdptr != NULL) {
           dtab=&D_tab;
           while ((dtab != NULL) &&
                 (strcmp(dtab->name, cdptr->display_name) != 0))
                 dtab = dtab->next;
           if (dtab != NULL) {  /* must have been removed from animation */
             gnarray = graphics_num_of_arrays(dtab->type);
             for (i=0; i<gnarray; i++)
               strcpy(a_name[i], dtab->a[i]);
	     strcpy(dname, cdptr->display_name);
             clear_display(dname);
             remove_display_name(vptr->connect_id,cdptr->display_name); /*remove display name*/
             for (i=0; i<gnarray; i++)                 /*from VCS Canvas info*/
                removeA(a_name[i]);
           }
           tcdptr = cdptr;
           cdptr = cdptr->next;
           free((char *) tcdptr->display_name);
           free((char *) tcdptr);
        }
        self->dlist = NULL;
}

/*
 * It is necessary to change the colormap. This routine will change the
 * VCS color map. The color map is the same as in VCS. Thus, the user does
 * not have to learn something new.
 */
static PyObject *
PyVCS_setcolormap(PyVCScanvas_Object *self, PyObject *args)
{
        char    *ierr;
	char	*colormap_name=NULL;
	extern char 	active_colors[]; /*colormap name*/
	extern char * python_colormap();

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

  	if(!PyArg_ParseTuple(args, "s", &colormap_name)) {
           PyErr_SetString(PyExc_TypeError, "Incorrect parameter value.");
           return NULL;
	   /*if ((colormap_name == NULL) || (colormap_name[0] == '\0')) {
              colormap_name = (char *) malloc(strlen(active_colors)+1);
              strcpy(colormap_name,active_colors);
	   }*/
	}

	ierr = python_colormap(colormap_name); /* set the new VCS colormap */
        if (ierr != NULL) {
            PyErr_SetString(PyExc_TypeError, ierr);
            return NULL;
        }

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
  	return Py_None;
}

/*
 * It is necessary to change the individual color cell in a colormap.
 * This routine will not change the color cell of the default colormap.
 */
static PyObject *
PyVCS_setcolorcell(PyVCScanvas_Object *self, PyObject *args)
{
        char    *ierr;
	int	cell, r, g, b;
        extern char * python_setcolorcell();

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

        if(!PyArg_ParseTuple(args, "iiii", &cell, &r, &g, &b)) {
           PyErr_SetString(PyExc_TypeError, "Error - Four integer values must be given (i.e., cell index, R, G, and B).");
           return NULL;
        } else {
	   if ((cell < 0) || (cell > 239)) {
              PyErr_SetString(PyExc_ValueError, "Error - Cell index value must be in range 0 to 239.");
              return NULL;
               
           } else if ((r < 0) || (r > 100)) {
              PyErr_SetString(PyExc_ValueError, "Error - Red (R) index value must be in range 0 to 100.");
              return NULL;
           } else if ((g < 0) || (g > 100)) {
              PyErr_SetString(PyExc_ValueError, "Error - Green (G) index value must be in range 0 to 100.");
              return NULL;
           } else if ((b < 0) || (b > 100)) {
              PyErr_SetString(PyExc_ValueError, "Error - Blue (B) index value must be in range 0 to 100.");
              return NULL;
           }
           ierr=python_setcolorcell(cell, r, g, b); /* set the new index color cell */
           if (ierr != NULL) {
              PyErr_SetString(PyExc_TypeError, ierr);
              return NULL;
           }
	}

        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/*
 * It is necessary to retrieve the individual color cell (i.e., RGB) in a colormap.
 * This routine will return the color cell RGB value in a colormap.
 */
static PyObject *
PyVCS_getcolorcell(PyVCScanvas_Object *self, PyObject *args)
{
        int             		c, cell;
	char				buf[1024];
        struct color_table              *Cptab=NULL;
	PyObject                        *listptr,*R,*G,*B;
	extern struct c_val 		std_color[16];
        extern struct color_table       C_tab;
	extern char 			active_colors[]; /*colormap name*/

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

        /* Return NULL Python Object or Python List */
        if(!PyArg_ParseTuple(args, "i", &cell)) {
           PyErr_SetString(PyExc_TypeError, "Error - Integer value must be given for the color cell.");
           return NULL;
        } else {
           if ((cell < 0) || (cell > 255)) {
              PyErr_SetString(PyExc_ValueError, "Error - Cell index value must be in range 0 to 255.");
              return NULL;
           }

           for(Cptab=&C_tab;Cptab != NULL && (c=strcmp(Cptab->name,active_colors))!=0;
                                        Cptab=Cptab->next);
           if (c != 0) {
              sprintf(buf,"Cannot find colormap class object Cp_%s.",active_colors);
              PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
           }

	   if (cell < 240) {
              R = Py_BuildValue("i", (int) (Cptab->cval[cell].red+0.5));
              G = Py_BuildValue("i", (int) (Cptab->cval[cell].green+0.5));
              B = Py_BuildValue("i", (int) (Cptab->cval[cell].blue+0.5));
           } else {
              R = Py_BuildValue("i", (int) (std_color[cell-240].red+0.5));
              G = Py_BuildValue("i", (int) (std_color[cell-240].green+0.5));
              B = Py_BuildValue("i", (int) (std_color[cell-240].blue+0.5));
           }
           listptr = PyList_New(3);
           PyList_SetItem(listptr, 0, R);
           PyList_SetItem(listptr, 1, G);
           PyList_SetItem(listptr, 2, B);

           return listptr;

        }
}

/*
 * It is necessary to retrieve the active colormap name.
 * This routine will return the active colormap name. 
 */
static PyObject *
PyVCS_getcolormapname(PyVCScanvas_Object *self, PyObject *args)
{
        int                             c, cell;
        char                            buf[1024];
        struct color_table              *Cptab=NULL;
        PyObject                        *listptr,*R,*G,*B;
        extern struct color_table       C_tab;
        extern char                     active_colors[]; /*colormap name*/

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

        for(Cptab=&C_tab;Cptab != NULL && (c=strcmp(Cptab->name,active_colors))!=0;
                                        Cptab=Cptab->next);
        if (c != 0) {
              sprintf(buf,"Cannot find colormap class object Cp_%s.",active_colors);
              PyErr_SetString(PyExc_TypeError, buf);
              return NULL;
        }

	return Py_BuildValue("s", active_colors);
}

/* To save a graphics plot in CDAT the user can call CGM along with the
 * name of the output. This routine will save the displayed image on the
 * VCS canvas as a binary vector graphics that can be imported into
 * MSWord or Framemaker. CGM files are an ISO standards output format.
 */
static PyObject *
PyVCS_cgm(PyVCScanvas_Object *self, PyObject *args)
{
	char *cgm_name, *mode=NULL;
	int app=0;
	extern int python_cgm();

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	/* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

        /*
         * Make sure the VCS canvas is up and running.
         * If the VCS Canvas is not open, then return.
*/
#ifdef X11WM
        if (self->connect_id.display == 0) {
           PyErr_SetString(PyExc_TypeError, "VCS Canvas is not displayed.");
	   return NULL;
        }
#endif

	if (!PyArg_ParseTuple(args, "s|s", &cgm_name, &mode)) {
	   PyErr_SetString(PyExc_TypeError, "Must provide an output cgm name.");
	   return NULL;
	}

        if (strlen(cgm_name) == 0) { /* cgm_name not given */
	   PyErr_SetString(PyExc_TypeError, "Must provide an output cgm name.");
	   return NULL;
	}

        /* Set the cgm write mode to append (1), if mode is "a" or "A". */
        if (mode != NULL) {
	   if (cmpncs(mode, "a") == 0)
               app = 1;
	}

	python_cgm(cgm_name, app, self->connect_id, self->dlist); /* call to create cgm file */

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
  	return Py_None;
}
/* Charles' attempt at plugging in svg output
 */
static PyObject *
PyVCS_svg(PyVCScanvas_Object *self, PyObject *args)
{
	char *ps_name, *mode=NULL;
	int app=0;
	int W,H;
	extern int XW ;
	extern int YW ;
	int ier;
	extern int trimbl();
	extern int out_meta();
	extern char meta_type[5];


        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	/* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

        /*
         * Make sure the VCS canvas is up and running.
         * If the VCS Canvas is not open, then return.
*/
#ifdef X11WM
        if (self->connect_id.display == 0) {
           PyErr_SetString(PyExc_TypeError, "VCS Canvas is not displayed.");
	   return NULL;
        }
#endif

	if (!PyArg_ParseTuple(args, "sii", &ps_name, &W, &H)) {
	   PyErr_SetString(PyExc_TypeError, "Must provide an output ps name.");
	   return NULL;
	}


        if (strlen(ps_name) == 0) { /* cgm_name not given */
	   PyErr_SetString(PyExc_TypeError, "Must provide an output ps name.");
	   return NULL;
	}

	XW = W;
	YW = H;

	strcpy(meta_type,"svg");
	trimbl(ps_name,256);
	ier = out_meta(ps_name,app, self->connect_id, self->dlist); /* Append or replace svg file */

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
  	return Py_None;
}
/* PNG font won't scale if output dims are not set BEFORE plotting, this function does set them */
static PyObject *
PyVCS_setbgoutputdimensions(PyVCScanvas_Object *self, PyObject *args)
{
	int W,H;
	extern int XW ;
	extern int YW ;
	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}
	if (!PyArg_ParseTuple(args, "ii", &W,&H)) {
	   PyErr_SetString(PyExc_TypeError, "Must provide a width and a height");
	   return NULL;
	}
        //printf("Replacing values (%i,%i) with (%i,%i)",XW,YW,W,H);
	XW=W;
	YW=H;
	/* Return NULL Python Object */
        Py_INCREF (Py_None);
  	return Py_None;
}

/* Charles' attempt at plugging in png output
 */
static PyObject *
PyVCS_png(PyVCScanvas_Object *self, PyObject *args)
{
	char *ps_name, *mode=NULL;
	int app=0;
	/* int W,H; */
	/* extern int XW ; */
	/* extern int YW ; */
	int ier,draw_white_bg;
        extern int draw_white_background;
	extern int trimbl();
	extern int out_meta();
	extern char meta_type[5];


        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	/* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);
        /*
         * Make sure the VCS canvas is up and running.
         * If the VCS Canvas is not open, then return.
         */
#ifdef X11WM
        if (self->connect_id.drawable == 0) {
	    PyErr_SetString(PyExc_TypeError, "VCS Canvas is not displayed.");
	   return NULL;
        }
#endif

	if (!PyArg_ParseTuple(args, "si", &ps_name,&draw_white_bg)) {
	   PyErr_SetString(PyExc_TypeError, "Must provide an output png name");
	   return NULL;
	}


        if (strlen(ps_name) == 0) { /* cgm_name not given */
	   PyErr_SetString(PyExc_TypeError, "Must provide an output png name.");
	   return NULL;
	}

        if (draw_white_bg>1) { /* white bg is either 0 or 1 */
	   PyErr_SetString(PyExc_TypeError, "White bg must be 0 or 1.");
	   return NULL;
	}

        draw_white_background = draw_white_bg;

	/* XW = W; */
	/* YW = H; */

	strcpy(meta_type,"png");
	trimbl(ps_name,256);
	ier = out_meta(ps_name,app, self->connect_id, self->dlist); /* Append or replace svg file */

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
  	return Py_None;
}
/* Charles' attempt at plugging in direct pdf output
 */
static PyObject *
PyVCS_pdf(PyVCScanvas_Object *self, PyObject *args)
{
	char *ps_name, *mode=NULL;
	int app=0;
	int W,H;
	extern int XW ;
	extern int YW ;
	int ier;
	extern int trimbl();
	extern int out_meta();
	extern char meta_type[5];


        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	/* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

        /*
         * Make sure the VCS canvas is up and running.
         * If the VCS Canvas is not open, then return.
         */
#ifdef X11WM
        if (self->connect_id.drawable == 0) {
           PyErr_SetString(PyExc_TypeError, "VCS Canvas is not displayed.");
	   return NULL;
        }
#endif

	if (!PyArg_ParseTuple(args, "sii", &ps_name, &W, &H)) {
	   PyErr_SetString(PyExc_TypeError, "Must provide an output pdf name and width/height");
	   return NULL;
	}


        if (strlen(ps_name) == 0) { /* cgm_name not given */
	   PyErr_SetString(PyExc_TypeError, "Must provide an output png name.");
	   return NULL;
	}

	XW = W;
	YW = H;

	strcpy(meta_type,"pdf");
	trimbl(ps_name,256);
	ier = out_meta(ps_name,app, self->connect_id, self->dlist); /* Append or replace svg file */

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
  	return Py_None;
}

/* Charles' attempt at plugging in postscript output
 */
static PyObject *
PyVCS_postscript(PyVCScanvas_Object *self, PyObject *args)
{
	char *ps_name, *mode=NULL;
	int app=0;
	int W,H,T,B,L,R;
	extern int XW ;
	extern int YW ;
	extern int MARGINL;
	extern int MARGINT;
	extern int MARGINR;
	extern int MARGINB;
	
	int ier;
	extern int trimbl();
	extern int out_meta();
	extern char meta_type[5];

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	/* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

        /*
         * Make sure the VCS canvas is up and running.
         * If the VCS Canvas is not open, then return.
*/
#ifdef X11WM
        if (self->connect_id.display == 0) {
#else
	  if (self->connect_id.cr == NULL) {
#endif
           PyErr_SetString(PyExc_TypeError, "VCS Canvas is not displayed.");
	   return NULL;
        }

	if (!PyArg_ParseTuple(args, "siiiiii", &ps_name, &W, &H, &R, &L, &T, &B)) {
	   PyErr_SetString(PyExc_TypeError, "Must provide an output ps name.");
	   return NULL;
	}


        if (strlen(ps_name) == 0) { /* cgm_name not given */
	   PyErr_SetString(PyExc_TypeError, "Must provide an output ps name.");
	   return NULL;
	}

	XW = W;
	YW = H;
	//MARGINL = L;
	//MARGINR = R;
	//MARGINT = T;
	//MARGINB = B;

/* 	printf("in ps vcs_legacy i got %i,%i,%i,%i,%i,%i\n",W,H,L,R,T,B); */
	strcpy(meta_type,"ps");
	trimbl(ps_name,256);
	ier = out_meta(ps_name,app, self->connect_id, self->dlist); /* Append or replace svg file */


	/* Return NULL Python Object */
        Py_INCREF (Py_None);
  	return Py_None;
}

/* In some cases, the user may want to save the plot out as an raster
 * file. This routine allows the user to save the VCS canvas output as
 * a SUN raster file. This file can be converted to other formats with
 * the aid of xv and other such image tools found freely on the web.
 */
static PyObject *
PyVCS_raster(PyVCScanvas_Object *self, PyObject *args)
{
        char *raster_name=NULL, *mode=NULL;
        int app=1;
        extern int python_raster();

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	/* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

        /*
         * Make sure the VCS canvas is up and running.
         * If the VCS Canvas is not open, then return.
         */
#ifdef X11WM
        if (self->connect_id.drawable == 0) {
#else
	  if (self->connect_id.cr == NULL) {
#endif
	   PyErr_SetString(PyExc_TypeError, "VCS Canvas is not displayed.");
           return NULL;
        }

        if (!PyArg_ParseTuple(args, "s|s", &raster_name, &mode)) {
	   PyErr_SetString(PyExc_TypeError, "Must provide an output raster name.");
           return NULL;
	}

        if (strlen(raster_name) == 0) { /* raster_name not given */
	   PyErr_SetString(PyExc_TypeError, "Must provide an output raster name.");
           return NULL;
        }

        /* Set the raster write mode to replace (0), if mode is "r" or "R". */
        if (mode != NULL) {
	   if (cmpncs(mode, "r") == 0)
               app = 0;
	}

        python_raster(raster_name, app); /* call to create raster file */

        /* Return null python object */
        Py_INCREF (Py_None);
        return Py_None;
}

/*
 * This routine allows the user to save the VCS canvas in one of the many
 * GhostScript (gs) file types (also known as devices). To view other
 * GhostScript devices, issue the command "gs --help" at the terminal
 * prompt. Device names include: bmp256, epswrite, jpeg, jpeggray,
 * pdfwrite, png256, png16m, sgirgb, tiffpack, and tifflzw. By default
 * the device = 'png256'.
 */
static PyObject *
PyVCS_gs(PyVCScanvas_Object *self, PyObject *args)
{
        char *file_name=NULL, *device=NULL;
        char *orientation=NULL, *resolution=NULL;
	char extn[4]={".gs"}, command_str[2048], buf[2048];
	char ps_name[]="/tmp/temp_postscript_file.ps";
	FILE * pfp;
	PyObject *newargs;
	PyObject * PyVCS_postscript();

        char *gstr=NULL, gif_geom[12]={"72x72"};
	int orientation_num=0, merge_num=0,ffd;

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	/* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

        /*
         * Make sure the VCS canvas is up and running.
         * If the VCS Canvas is not open, then return.
         */
#ifdef X11WM
        if (self->connect_id.drawable == 0) {
#else
	  if (self->connect_id.cr == NULL) {
#endif
           PyErr_SetString(PyExc_TypeError, "VCS Canvas is not displayed.");
           return NULL;
        }

	if (!PyArg_ParseTuple(args, "s|sss", &file_name, &device, &orientation, &resolution)) {
	   PyErr_SetString(PyExc_TypeError, "Must provide an output GhostScript file name.");
           return NULL;
	}
	/* Create postscript file, but first create the proper arg list */
	/* Removed by C. Doutriaux not needed anymore with new postscript */
/*         if (orientation == NULL) */
/* 	  newargs = PyTuple_New(1); */
/*         else { */
/* 	  newargs = PyTuple_New(2); */
/*           PyTuple_SetItem(newargs, 1, PyString_FromString("r")); /\* get 2nd argument *\/ */
/* /\*           PyTuple_SetItem(newargs, 2, PyTuple_GetItem (args, 2)); /\\* get 3rd argument *\\/ *\/ */
/* 	} */
	newargs = PyTuple_New(7);
        PyTuple_SetItem(newargs, 0, PyString_FromString(ps_name)); /* get 1st argument */
        PyTuple_SetItem(newargs, 1, PyInt_FromLong(612)); /* get 1st argument */
        PyTuple_SetItem(newargs, 2, PyInt_FromLong(792)); /* get 1st argument */
        PyTuple_SetItem(newargs, 3, PyInt_FromLong(0)); /* get 1st argument */
        PyTuple_SetItem(newargs, 4, PyInt_FromLong(0)); /* get 1st argument */
        PyTuple_SetItem(newargs, 5, PyInt_FromLong(0)); /* get 1st argument */
        PyTuple_SetItem(newargs, 6, PyInt_FromLong(0)); /* get 1st argument */
	PyVCS_postscript(self, newargs); /* create postscript file */



	/* create the gs command */
        sprintf(command_str, "gs -r%s -q -dBATCH -dNOPAUSE -sDEVICE=%s -sOutputFile=%s %s\n", resolution, device, file_name, ps_name);

	/* use the popen call to create the gp image */
       	if ((pfp=popen(command_str,"w")) == NULL)
           PyErr_SetString(PyExc_ValueError, "Error - Could not create GIF file.");
       	else
	   pclose(pfp);

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/*
 * In some cases, the user may want to save the file out as a raster or 
 * a Encapsulated PostScript file. The routine allows the user to save 
 * the VCS Canvas output as a GIF raster file or an Encapsulated PostScript
 * file. These files can be converted to other formats with the aid of xv
 * and other such imaging tools found freely on the web.
 */
static PyObject *
PyVCS_gif_or_eps(PyVCScanvas_Object *self, PyObject *args)
{
        char buf[2048], *file_type=NULL, *gp_name=NULL, *merge=NULL;
        char *orientation=NULL, *sptr=NULL;
        char *gstr=NULL, gif_geom[12]={"72x72"};
	char extn[5]={".eps"}, command_str[2048], gp2_name[2048],crap[2048];
	char ps_name[]="/tmp/temp_postscript_file.ps";
	FILE * pfp;
	int orientation_num=0, merge_num=0,ffd;
	PyObject *newargs;
	PyObject * PyVCS_postscript();
        /*extern int python_gif();*/

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	/* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

        /*
         * Make sure the VCS canvas is up and running.
         * If the VCS Canvas is not open, then return.
         */
#ifdef X11WM
        if (self->connect_id.drawable == 0) {
#else
	  if (self->connect_id.cr == NULL) {
#endif
           PyErr_SetString(PyExc_TypeError, "VCS Canvas is not displayed.");
           return NULL;
        }

	if (!PyArg_ParseTuple(args, "ss|sss", &file_type, &gp_name, &merge, &orientation, &gstr)) {
	   PyErr_SetString(PyExc_TypeError, "Must provide an output GIF name.");
           return NULL;
	}

        if (strcmp(file_type, "gif") == 0) /* get file_type */
           strcpy(extn,".gif");
        else /* must be eps and cannot merge files yet. */
           merge_num = 0;

        if (strlen(gp_name) == 0) { /* gp_name not given */
	   PyErr_SetString(PyExc_TypeError, "Must provide an output GIF name.");
           return NULL;
        }

	/* Set the merge flag to 1 if the user wants to append an existing file */
        if (merge != NULL) {
	   if (cmpncs(merge, "A") == 0)
               merge_num = 1;
	}

        /* Set the orientation to portrait. Landscape is the default setting.
         * If orientation is "p" or "P", the set to portrait. Anything else
 	 * will set the orientation to Landscape.
	 */
        if (orientation != NULL) {
	   if (cmpncs(orientation, "P") == 0)
               orientation_num = 1;
	}

	/* commented out by C. Doutriaux */
	/* Not needed anymore with new postscript direct output */
	/* call to create postscript file, but first create the proper arg list */
/*         if (orientation == NULL) */
/* 	  newargs = PyTuple_New(1); */
/*         else { */
/* 	  newargs = PyTuple_New(2); */
/*           PyTuple_SetItem(newargs, 1, PyString_FromString("r")); /\* get 2nd argument *\/ */
/*           PyTuple_SetItem(newargs, 2, PyTuple_GetItem (args, 3)); /\* get 3rd argument *\/ */
/* 	} */
	newargs = PyTuple_New(7);
        PyTuple_SetItem(newargs, 0, PyString_FromString(ps_name)); /* get 1st argument */
        PyTuple_SetItem(newargs, 1, PyInt_FromLong(612)); /* get 1st argument */
        PyTuple_SetItem(newargs, 2, PyInt_FromLong(792)); /* get 1st argument */
        PyTuple_SetItem(newargs, 3, PyInt_FromLong(0)); /* get 1st argument */
        PyTuple_SetItem(newargs, 4, PyInt_FromLong(0)); /* get 1st argument */
        PyTuple_SetItem(newargs, 5, PyInt_FromLong(0)); /* get 1st argument */
        PyTuple_SetItem(newargs, 6, PyInt_FromLong(0)); /* get 1st argument */
	PyVCS_postscript(self, newargs);

        /* Set the geometry for the gif output only */
        if (gstr != NULL)
           strcpy( gif_geom, gstr );

	sptr = strstr(gp_name, extn);
        if (sptr == NULL)
           sprintf(buf, "%s%s",gp_name, extn);
        else if ((sptr != NULL) && (strcmp(sptr,extn) == 0) )
           sprintf(buf, "%s",gp_name);
        else
           sprintf(buf, "%s%s",gp_name,extn);
        strcpy(gp2_name, buf);

        if (merge_num == 1) {
	    ffd = access(buf, F_OK);    /* check to see if file exist */
            if (ffd != 0) /* The file does not exist! */
               merge_num = 0; /* no need to merge if nothing is there */
            else
	       sprintf(buf, "%sa",buf); /* generate the merge file name */
	}

	/* create the gif command */
        if (strcmp(file_type, "gif") == 0) {
	   if (orientation_num == 0) 	/* Landscape */
	     sprintf(command_str, "gs -r%s -q -dBATCH -sDEVICE=ppmraw -sOutputFile=- %s | pnmflip -cw | ppmquant 256 | ppmtogif > %s", gif_geom, ps_name, buf);
	   else				/* Portrait */
	     sprintf(command_str, "gs -r%s -q -dBATCH -sDEVICE=ppmraw -sOutputFile=- %s | ppmquant 256 | ppmtogif > %s", gif_geom, ps_name, buf);
	} else {
/* 	   if (orientation_num == 0) 	/\* Landscape *\/ */
/* 	     sprintf(command_str, "gs -r%s -q -dBATCH -sDEVICE=ppmraw -sOutputFile=- %s | pnmflip -cw | pnmtops > %s", gif_geom, ps_name, buf); */
/* 	   else				/\* Portrait *\/ */
/* 	     sprintf(command_str, "gs -r%s -q -dBATCH -sDEVICE=ppmraw -sOutputFile=- %s | pnmtops > %s", gif_geom, ps_name, buf); */
	     sprintf(command_str, "ps2epsi %s %s", ps_name, buf);
        }

/* This code below, contains the crop portion of the executable.
	if (orientation_num == 0) 	* Landscape *
	  sprintf(command_str, "gs -r72x72 -q -dBATCH -sDEVICE=ppmraw -sOutputFile=- %s | pnmflip -cw | pnmcrop | pnmtops > %s", ps_name, buf);
	else				* Portrait *
	  sprintf(command_str, "gs -r72x72 -q -dBATCH -sDEVICE=ppmraw -sOutputFile=- %s | pnmcrop | pnmtops > %s", ps_name, buf);
*/

	/* use the popen call to create the gp image */
       	if ((pfp=popen(command_str,"w")) == NULL)
           PyErr_SetString(PyExc_ValueError, "Error - Could not create GIF file.");
       	else
	  {
/* 	    fprintf(stderr,"Scanning!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"); */
/* 	    fscanf(pfp,crap); */
/* 	    fprintf(stderr,"Closing!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"); */
	    pclose(pfp);
	  }
	/* Merge gp file */
    	if (merge_num == 1) {
	   /*sprintf(command_str, "gifmerge -192,192,192 -notransp -l0 -50 %s %s > %sm", gp2_name, buf, gp2_name);*/
	   sprintf(command_str, "gifsicle -m -l --delay 15 %s %s > %sm", gp2_name, buf, gp2_name);

       	   if ((pfp=popen(command_str,"w")) == NULL)
              PyErr_SetString(PyExc_ValueError, "Error - Could not create GIF file.");
       	   else
	      pclose(pfp);

           /* Remove the old merged gp file from the directory */
           sprintf(command_str,"/bin/rm -f %s", gp2_name);
           system (command_str);

           /* Move the temporaray merged file to the wanted gp name */
           sprintf(command_str,"/bin/mv %sm %s", gp2_name, gp2_name);
           system (command_str);

           /* Remove the temporary merged file from the directory */
           sprintf(command_str,"/bin/rm -f %s", buf);
           system (command_str);
	}

        /* Remove the temporary postscript file from the directory */
        sprintf(command_str,"/bin/rm -f %s", ps_name);
        system (command_str);

        sprintf(buf,"%s", gp2_name);
        return Py_BuildValue("s", buf);
}

/* Postscript output is another form of vector graphics. It is larger than
 * its CGM output counter part because it is stored out as ASCII. To save 
 * out a postscript file, CDAT (via VCS) will first create a cgm file in 
 * the user's PCMDI_GRAPHICS directory. Then it will use gplot to convert
 * the cgm file to a postscript file in the location the user has 
 * chosen.
 */
PyObject *
PyVCS_postscript_old(PyVCScanvas_Object *self, PyObject *args)
{
	int		ierr, val, orientation_num=0, append_flg=0, ffd, rfd, wfd;
	char		*append=NULL, *orientation=NULL, *home_dir;
	char            temp_cgm_file[1024], gplot_str[1024];
	char		temp_str[2048], command_str[2048], append_str[2048];
	char		*postscript_filename;
	char		postscript_file[1024], tpostscript_file[1024], post_extension[10];
	extern int 	python_cgm();
	extern int	read_HARD_COPY();

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	/* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

        /*
         * Make sure the VCS canvas is up and running.
         * If the VCS Canvas is not open, then return.
         */
#ifdef X11WM
        if (self->connect_id.display == 0) {
#else
	  if (self->connect_id.cr == NULL) {
#endif
            PyErr_SetString(PyExc_TypeError, "VCS Canvas is not displayed.");
	    return NULL;
        }

	if (!PyArg_ParseTuple(args, "s|ss", &postscript_filename, &append, &orientation)) {
	   PyErr_SetString(PyExc_TypeError, "Must provide an output postscript name.");
           return NULL;
	}

        /* Set the append flag to 1 if the user wants to append an existing file */
        if (append != NULL) {
           if (cmpncs(append, "A") == 0)
               append_flg = 1;
        }

        /* Set the orientation to portrait. Landscape is the default setting.
         * If orientation is "p" or "P", the set to portrait. Anything else
 	 * will set the orientation to Landscape.
	 */
        if (orientation != NULL) {
	   if (cmpncs(orientation, "P") == 0)
               orientation_num = 1;
	}

	/* Get the postscript directory and filename */
	if (postscript_filename[0] != '/') {
	   strcpy(postscript_file,"./");
	   strcat(postscript_file, postscript_filename);
	} else
	   strcpy(postscript_file, postscript_filename);
	val = strlen(postscript_file) - 3;
	strcpy(post_extension, postscript_file + val);
	if (strcmp(post_extension, ".ps") != 0)
           strcat(postscript_file, ".ps");

	/* Create the CGM file name in the user's PCMDI_GRAPHICS 
	 * directory. If PCMDI_GRAPHICS has not been created, then
	 * use the user's home directory.
	 */
 	home_dir = (char *) getenv("HOME");
	if (home_dir == NULL) {
           PyErr_SetString(PyExc_TypeError, "The user's home directory was not found!");
	   return NULL;
	}
	sprintf(temp_str, "%s/%s", home_dir, DOT_DIRECTORY);
	ffd = access(temp_str, F_OK);    /* check to see if the */
        rfd = access(temp_str, R_OK);    /* HARD_COPY file exist */
        wfd = access(temp_str, W_OK);    /* HARD_COPY file exist */
	if (ffd != 0) { /* The file does not exist! */
           sprintf(temp_str,"The %s directory does not exist!",DOT_DIRECTORY);
           PyErr_SetString(PyExc_TypeError, temp_str);
           return NULL;
        }
        if (rfd != 0) { /* The file cannot be read! */
           sprintf(temp_str,"The %s directory cannot be read!",DOT_DIRECTORY);
           PyErr_SetString(PyExc_TypeError, temp_str);
           return NULL;
        }
        if (wfd != 0) { /* The file does not have write permission! */
           sprintf(temp_str,"The %s directory does not have write permission!",DOT_DIRECTORY);
           PyErr_SetString(PyExc_TypeError, temp_str);
           return NULL;
        }
	sprintf(temp_cgm_file, "%s/7eilfyraropmetCGM.cgm", temp_str);
	python_cgm(temp_cgm_file, 0, self->connect_id, self->dlist); /* call to create cgm file */

	/* Must open the HARD_COPY file to obtain the gplot information *
	if ((ierr = read_HARD_COPY(gplot_str, orientation_num)) == 1) {
	   sprintf(temp_str,"/bin/rm -f %s", temp_cgm_file);
	   system (temp_str);
           PyErr_SetString(PyExc_TypeError, "Could read HARD_COPY file.");
	   return NULL;
	}
*/
	if (orientation_num == 0) /*Get Landscape*/
          strcpy(gplot_str,"gplot -dPSC -r90 -x-1.75 -D -X12.5 -Y10");
	else /*Get Portrait*/
	  strcpy(gplot_str,"gplot -dPSC -D -X10 -Y12.5");

	/* Must use system call to convert cgm file to postscript */
	strcpy(command_str, gplot_str);

	/* Add the CGM file name to the GPLOT command string */
        strcat(command_str, " ");
        strcat(command_str, temp_cgm_file);
        strcat(command_str, " ");
        if (append_flg == 1) {
           sprintf(tpostscript_file, "%st", postscript_file);
           strcat(command_str, tpostscript_file);
        } else
           strcat(command_str, postscript_file);

	/* Convert the CGM file to Postscript and save in file */
	if ((ierr = system (command_str)) == 0) {
	   sprintf(temp_str,"/bin/rm -f %s", temp_cgm_file);
	   system (temp_str);
           sprintf(temp_str, "Error - Could not create postscript file (%s).", temp_cgm_file);
	   PyErr_SetString(PyExc_TypeError, temp_str);
	   return NULL;
        }/* else {
           sprintf(temp_str,"Saving - Postscript file (%s).",postscript_file);
           pyoutput( temp_str, 1);
        }*/
 
        if (append_flg == 1) {
           sprintf(append_str, "cat %s >> %s\n", tpostscript_file, postscript_file);
	   if ((ierr = system (append_str)) == 1) {
                 sprintf(append_str, "ierr = %d: cp %s %s", ierr, tpostscript_file, postscript_file);
	         system (append_str);
           }
           sprintf(append_str, "/bin/rm -f %s", tpostscript_file);
	   system (append_str);
        }

	/* Remove the temporaray CGM file in the user's PCMDI_GRAPHICS
	 * directory.
	 */
	sprintf(command_str,"/bin/rm -f %s", temp_cgm_file);
	system (command_str);

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* This function creates a temporary cgm file and then sends it to the    
 * specified printer. Once the printer received the information, then the 
 * temporary cgm file is deleted. The temporary cgm file is created in the
 * user's PCMDI_GRAPHICS directory. 
 */
static PyObject *
PyVCS_printer(PyVCScanvas_Object *self, PyObject *args)
{
        int             ierr, val, orientation_num=0, ffd, rfd, wfd;
        char            *printer_name, *orientation=NULL;
        char            temp_cgm_file[1024], gplot_str[1024];
        char            temp_str[2048], command_str[2048];
        char            printer_type_str[1024];
        char            *printer_type, *home_dir;
        char            postscript_file[1024], post_extension[10];
        extern int      read_HARD_COPY();
	int W,H,T,B,L,R;
	extern int XW ;
	extern int YW ;
	extern int MARGINL;
	extern int MARGINT;
	extern int MARGINR;
	extern int MARGINB;
	int ier;
	extern int trimbl();
	extern int out_meta();
	extern char meta_type[5];

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

        /* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

        /*
         * Make sure the VCS canvas is up and running.
         * If the VCS Canvas is not open, then return.
         */
#ifdef X11WM
        if (self->connect_id.drawable == 0) {
#else
	  if (self->connect_id.cr == NULL) {
#endif
	    PyErr_SetString(PyExc_TypeError, "VCS Canvas is not displayed.");
            return NULL;
        }

        if (!PyArg_ParseTuple(args, "siiiiii", &printer_name,  &W, &H, &R, &L, &T, &B)) {
	   PyErr_SetString(PyExc_TypeError, "Must provide printer name.");
           return NULL;
	}

	/* Get the correct UNIX printer command for ATT or BSD */
        strcpy(printer_type_str, " | lp -o fit-to-page -d");
        if ((printer_type=(char *)getenv((const char *)"PRINTER"))!=NULL)
           strcpy(printer_type_str, " | lpr -o fit-to-page -P");

        /* Set the orientation to portrait. Landscape is the default setting.
         * If orientation is "p" or "P", the set to portrait. Anything else
         * will set the orientation to Landscape.
         */
        if (orientation != NULL) {
           if (cmpncs(orientation, "P") == 0)
               orientation_num = 1;
        }
        /* Create the CGM file name in the user's PCMDI_GRAPHICS
         * directory. If PCMDI_GRAPHICS has not been created, then
         * use the user's home directory.
         */
        home_dir = (char *) getenv("HOME");
        if (home_dir == NULL) {
           PyErr_SetString(PyExc_TypeError, "The user's home directory was not found!");
           return NULL;
        }
        sprintf(temp_str, "%s/%s", home_dir, DOT_DIRECTORY);
        ffd = access(temp_str, F_OK);    /* check to see if the */
        rfd = access(temp_str, R_OK);    /* HARD_COPY file exist */
        wfd = access(temp_str, W_OK);    /* HARD_COPY file exist */
	if (ffd != 0) { /* The file does not exist! */
           sprintf(temp_str,"The %s directory does not exist!",DOT_DIRECTORY);
           PyErr_SetString(PyExc_TypeError, temp_str);
           return NULL;
        }
        if (rfd != 0) { /* The file cannot be read! */
           sprintf(temp_str,"The %s directory cannot be read!",DOT_DIRECTORY);
           PyErr_SetString(PyExc_TypeError, temp_str);
           return NULL;
        }
        if (wfd != 0) { /* The file does not have write permission! */
           sprintf(temp_str,"The %s directory does not have write permission!",DOT_DIRECTORY);
           PyErr_SetString(PyExc_TypeError, temp_str);
           return NULL;
        }
        sprintf(temp_cgm_file, "%s/7eilfyraropmetCGM.ps", temp_str);
/*         python_cgm(temp_cgm_file, 0, self->connect_id, self->dlist); /\* call to create cgm file *\/ */
	XW = W;
	YW = H;
	MARGINL = L;
	MARGINR = R;
	MARGINT = T;
	MARGINB = B;

	strcpy(meta_type,"ps");
	trimbl(temp_cgm_file,256);
	ier = out_meta(temp_cgm_file,0, self->connect_id, self->dlist); /* Append or replace svg file */

        /* Must open the HARD_COPY file to obtain the gplot information *
        if ((ierr = read_HARD_COPY(gplot_str, orientation_num)) == 1) {
           sprintf(temp_str,"/bin/rm -f %s", temp_cgm_file);
           system (temp_str);
           PyErr_SetString(PyExc_TypeError, "Could read HARD_COPY file.");
           return NULL;
        }
*/
/* 	strcpy(gplot_str,"gplot -dPSC -D -X10 -Y12.5"); */

        /* Must use system call to convert cgm file to postscript */
        strcpy(command_str, "more");

        /* Add the CGM file name to the GPLOT command string */
        strcat(command_str, "  ");
        strcat(command_str, temp_cgm_file);
        strcat(command_str, " 2> /dev/null");
        strcat(command_str, printer_type_str);
        strcat(command_str, printer_name);

        /* Convert the CGM file to Postscript and save in file */
	if ((ierr = system (command_str)) != 0) {
           sprintf( temp_str, "Error - Could not send VCS Canvas plot(s) to (%s).", printer_name);
           pyoutput( temp_str, 0);
	}/* else {
           sprintf( temp_str, "Printing - VCS Canvas plot(s) sent to printer (%s).", printer_name);
           pyoutput( temp_str, 1);
	}*/

        /* Remove the temporaray CGM file in the user's PCMDI_GRAPHICS
         * directory.
         */
        sprintf(command_str,"/bin/rm -f %s", temp_cgm_file);
        system (command_str);

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/*
 * This function displays graphics segments, which are currently stored
 * in the frame buffer, on the VCS Canvas. That is, if the plot function
 * was called with the option bg = 1 (i.e., background mode), then the
 * plot is produced in the frame buffer and not visible to the user. In
 * order to view  the graphics segments, this function will copy the
 * contents of the frame buffer to the VCS Canvas, where the graphics
 * can be viewed by the user.
 */

int segCompareInt(const void *a, const void *b)
{
        return (*(const int *)a) - (*(const int *)b);
}

PyObject *PyVCS_showbg(PyVCScanvas_Object *self, PyObject *args)
{
       canvas_display_list 		*cdptr;
       int i, *psg, dct=0, *aseg;
       Gintlist pseg;
       struct display_tab  *dtab;
       extern struct display_tab  D_tab;
#ifdef X11WM
       extern Pixmap copy_pixmap(Gconid_X_drawable connect_id,int canvas_id);

       /* If the VCS Canvas is not open, then open it! */
       if (self->connect_id.canvas_drawable == 0)
#else
	 if (self->connect_id.cr == NULL)
#endif
           PyVCS_open(self, args);

       cdptr = self->dlist;
       while (cdptr != NULL) {
	   dtab=&D_tab; /* Get the appropriate display */
           while ((dtab != NULL) &&
                 (strcmp(dtab->name, cdptr->display_name) != 0))
                 dtab = dtab->next;
	   if (dtab == NULL) break;/* must have been removed from animation */

           for (psg=&dtab->F_seg[0];psg != &dtab->dsp_seg[4];psg+=4)
                 if (*psg > 0) ++dct; /* Get the number of segments */

           if ((aseg=(int *)malloc(dct*sizeof(int)))==NULL) { /* malloc the segment array */
                PyErr_SetString(PyExc_TypeError, "Error - memory overflow in creating segment array.");
                return NULL;
           }

           dct = 0;   /* Store the segments in malloc'ed array */
           for (psg=&dtab->F_seg[0];psg != &dtab->dsp_seg[4];psg+=4)
                 if (*psg > 0) { aseg[dct] = *psg; ++dct; }

           qsort(aseg, dct, sizeof(int), segCompareInt); /* Sort the array in ascending order */

           for (i = 0; i < dct; i++) {  /* Show the segments on the plot */
               gcsgwk(self->wkst_id, aseg[i]);
           } 

	  free((char *) aseg); /* Free the segment array */

	   cdptr = cdptr->next;
           dct = 0;
       }

#ifdef X11WM
       /* Copy the current VCS canvas to the pixmap (i.e., backing_store) */
       if (self->connect_id.canvas_pixmap != (Pixmap) NULL) {
           XFreePixmap(self->connect_id.display, self->connect_id.canvas_pixmap);
           self->connect_id.canvas_pixmap = (Pixmap) NULL;
       }
       self->connect_id.canvas_pixmap = copy_pixmap(self->connect_id, self->canvas_id);
#elif defined (QTWM)
       /* fprintf(stderr,"workstation: %i, %i\n",self->wkst_id,self->connect_id.wkst_id); */
       vcs_legacy_Qt_repaint_window_by_id(self->connect_id.wkst_id);
#else
       fprintf(stderr,"insert here your WM copy vcs_legacy to backing store pixmap\n");
#endif
        /* Return null python object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* This function creates backing store image that will be displayed when
 * the VCS Canvas is brought to the front.
 */
 PyObject *PyVCS_backing_store( PyVCScanvas_Object *self, PyObject *args)
{
 
#ifdef X11WM
       extern Pixmap copy_pixmap(Gconid_X_drawable connect_id,int canvas_id);

        if (self->connect_id.canvas_pixmap != (Pixmap) NULL) {
           XFreePixmap(self->connect_id.display, self->connect_id.canvas_pixmap);
           self->connect_id.canvas_pixmap = (Pixmap) NULL;
        }
        self->connect_id.canvas_pixmap = copy_pixmap(self->connect_id, self->canvas_id);
#elif defined (QTWM)
	//vcs_legacy_Qt_repaint_window_by_id(self->wkst_id);
#else
	printf("insert hre your WM create backing store image\n");
#endif 
	/* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* 
 * This function is no longer needed.
 *
 * This function turns the VCS Canvas updating (or refreshing) on and off.
 */
/* static PyObject * */
/* PyVCS_refreshcanvas(self, args) */
/*   PyVCScanvas_Object *self; */
/*   PyObject *args; */
/* { */
/*         char            temp_str[200]; */
/* 	int		update_value; */
/* 	extern int 	user_defer_update; */

/* 	/\* Check to see if vcs_legacy has been initalized *\/ */
/* 	if (self == NULL) { */
/*            PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init())."); */
/*   	   return NULL; */
/* 	} */

/* 	if (self->vcs_legacy_gui != 0) {  /\* Check for VCS canvas *\/ */
/*            PyErr_SetString(PyExc_TypeError, "Use the 'Update' mechanism provided in VCS."); */
/*   	   return NULL; */
/* 	} */

/* 	if (!PyArg_ParseTuple(args, "|i", &update_value)) { */
/*            sprintf( temp_str, "Notice - The VCS Canvas is refreshing manually. \n        Use the 'update' function to update the VCS Canvas."); */
/*            pyoutput( temp_str, 1); */
/* 	   user_defer_update = 1; */
/* 	} */

/* 	if (update_value == 0) { */
/*            sprintf( temp_str, "Notice - The VCS Canvas is refreshing automatically."); */
/*            pyoutput( temp_str, 1); */
/* 	   user_defer_update = 0; */
/* 	} else { */
/*            sprintf( temp_str, " Notice - The VCS Canvas is refreshing manually. \n        Use the 'update' function to update the VCS Canvas."); */
/*            pyoutput( temp_str, 1); */
/* 	   user_defer_update = 1; */
/* 	} */

/* 	/\* Return NULL Python Object *\/ */
/*         Py_INCREF (Py_None); */
/*         return Py_None; */
/* } */

/* /\*  */
/*  * This function is obsolete. */
/*  * */
/*  * This function updates the VCS Canvas. */
/*  *\/ */
/* static PyObject * */
/* PyVCS_flushcanvas(self, args) */
/*   PyVCScanvas_Object *self; */
/*   PyObject *args; */
/* { */
/* 	extern int 	user_defer_update; */
/* 	extern void	call_guwk_update(); */

/* 	/\* Check to see if vcs_legacy has been initalized *\/ */
/* 	if (self == NULL) { */
/*            PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init())."); */
/*   	   return NULL; */
/* 	} */

/* 	if (self->vcs_legacy_gui != 0) {  /\* Check for VCS canvas *\/ */
/*            PyErr_SetString(PyExc_TypeError, "Use the 'Update' mechanism provided in VCS."); */
/*   	   return NULL; */
/* 	} */

/* 	/\* Update the VCS Canvas manually *\/ */
/* 	if (user_defer_update == 1) */
/* 	    call_guwk_update(self->wkst_id); */

/* 	/\* Return NULL Python Object *\/ */
/*         Py_INCREF (Py_None); */
/*         return Py_None; */
/* } */

/*
 * This function flushes all recent and current X11 Events
 */
static PyObject *
PyVCS_flush(PyVCScanvas_Object *self, PyObject *args)
{
	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	if (self->vcs_legacy_gui != 0) {  /* Check for VCS canvas */
           PyErr_SetString(PyExc_TypeError, "Use the 'Update' mechanism provided in VCS.");
  	   return NULL;
	}

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
#ifdef X11WM
        if ( (self->connect_id.display != NULL) && (self->connect_id.drawable != 0) )
           XRaiseWindow(self->connect_id.display, self->connect_id.drawable);
#elif defined QTWM
	if (self->connect_id.cr != NULL)
	  vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#else
	fprintf(stderr,"insert your raise func here\n");
#endif
        if (not_using_gui)
           process_cdat_events();
#ifdef X11WM
	if (self->connect_id.display != NULL) {
	  XSync(self->connect_id.display,FALSE);
	  XFlush(self->connect_id.display);
	}
#elif defined QTWM
	/* nothing to do */
#else
	fprintf(stderr,"insert your sync/flush here\n");
#endif

        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Plot annotation changes that manipulate text associated with data values
 * into memory.
 */
static PyObject *
PyVCS_plot_annotation(self, args)
  PyVCScanvas_Object *self; 
  PyObject *args;
{           
        char    *attr_name=NULL, aname[MAX_PATH_LEN];
        char    *attr_str=NULL, astr[MAX_PATH_LEN];
        char    *array_str=NULL, arystr[MAX_PATH_LEN], buf[1024];
             
        struct display_tab              *dtab=NULL;
        extern struct display_tab       D_tab;

        struct a_tab                    *ptab=NULL;
        extern struct a_tab             A_tab;

        aname[0] = '\0';
        astr[0] = '\0';
        arystr[0] = '\0';
        if ((args != NULL) && PyArg_ParseTuple(args,"|sss", &attr_name, &attr_str, &array_str)) {
           if ((attr_name != NULL) && (attr_name[0] != '\0'))
                strcpy(aname, attr_name);
           if ((attr_str != NULL) && (attr_str[0] != '\0'))
                strcpy(astr, attr_str);
           if ((array_str != NULL) && (array_str[0] != '\0'))
                strcpy(arystr, array_str);
        }

        ptab=&A_tab;
        while ((ptab != NULL) && (strcmp(ptab->name, arystr) != 0)) {
           ptab = ptab->next;
        }
        if (ptab == NULL) {            /* get slab that already exist */
           sprintf(buf,"Cannot find slab object A_%s.",arystr);
           PyErr_SetString(PyExc_TypeError, buf);
           return NULL;
        }


        if ( strcmp(attr_name, "file") == 0 ) {
           ptab->pA_attr->f =repstr(ptab->pA_attr->f, attr_str);
           ptab->pA_attr->F =repstr(ptab->pA_attr->F, attr_str);
        } else if ( strcmp(attr_name, "source") == 0 ) {
           ptab->pA_attr->s =repstr(ptab->pA_attr->s, attr_str);
           ptab->pA_attr->S =repstr(ptab->pA_attr->S, attr_str);
        } else if ( strcmp(attr_name, "dataname") == 0 ) {
           ptab->pA_attr->n =repstr(ptab->pA_attr->n, attr_str);
           ptab->pA_attr->N =repstr(ptab->pA_attr->N, attr_str);
        } else if ( strcmp(attr_name, "title") == 0 ) {
           ptab->pA_attr->ti =repstr(ptab->pA_attr->ti, attr_str);
           ptab->pA_attr->TI =repstr(ptab->pA_attr->TI, attr_str);
        } else if ( strcmp(attr_name, "units") == 0 ) {
           ptab->pA_attr->u =repstr(ptab->pA_attr->u, attr_str);
           ptab->pA_attr->U =repstr(ptab->pA_attr->U, attr_str);
        } else if ( strcmp(attr_name, "xname") == 0 ) {
           ptab->pA_attr->xn[0] =repstr(ptab->pA_attr->xn[0], attr_str);
           ptab->pA_attr->XN[0] =repstr(ptab->pA_attr->XN[0], attr_str);
        } else if ( strcmp(attr_name, "yname") == 0 ) {
           ptab->pA_attr->xn[1] =repstr(ptab->pA_attr->xn[1], attr_str);
           ptab->pA_attr->XN[1] =repstr(ptab->pA_attr->XN[1], attr_str);
        } else if ( strcmp(attr_name, "zname") == 0 ) {
           ptab->pA_attr->xn[2] =repstr(ptab->pA_attr->xn[2], attr_str);
           ptab->pA_attr->XN[2] =repstr(ptab->pA_attr->XN[2], attr_str);
        } else if ( strcmp(attr_name, "tname") == 0 ) {
           ptab->pA_attr->xn[3] =repstr(ptab->pA_attr->xn[3], attr_str);
           ptab->pA_attr->XN[3] =repstr(ptab->pA_attr->XN[3], attr_str);
        } else if ( strcmp(attr_name, "crdate") == 0 ) {
           ptab->pA_attr->crd =repstr(ptab->pA_attr->crd, attr_str);
           ptab->pA_attr->CRD =repstr(ptab->pA_attr->CRD, attr_str);
        } else if ( strcmp(attr_name, "crdtime") == 0 ) {
           ptab->pA_attr->crt =repstr(ptab->pA_attr->crt, attr_str);
           ptab->pA_attr->CRT =repstr(ptab->pA_attr->CRT, attr_str);
        } else if ( strcmp(attr_name, "comment1") == 0 ) {
           ptab->pA_attr->com1 =repstr(ptab->pA_attr->com1, attr_str);
        } else if ( strcmp(attr_name, "comment2") == 0 ) {
           ptab->pA_attr->com2 =repstr(ptab->pA_attr->com2, attr_str);
        } else if ( strcmp(attr_name, "comment3") == 0 ) {
           ptab->pA_attr->com3 =repstr(ptab->pA_attr->com3, attr_str);
        } else if ( strcmp(attr_name, "comment4") == 0 ) {
           ptab->pA_attr->com4 =repstr(ptab->pA_attr->com4, attr_str);
        }

        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Iniialize the animation, by generating the raster image and storing them
 * into memory.
 */
static PyObject *
PyVCS_animate_init(PyVCScanvas_Object *self, PyObject *args)
{
        VCSCANVASLIST_LINK 	vptr=head_canvas_list;
	char 			*save_file=NULL, afile[MAX_PATH_LEN];
	extern void 		update_vcs_legacy_connection_information();
        extern int		create_image_toggle_cb();
	extern int 		animate_module();
	/* extern void vcs_legacy_Qt_animation_created(); */
	/* Check for animation file name */
	afile[0] = '\0';
  	if(PyArg_ParseTuple(args, "|s", &save_file)) {
	   if ((save_file != NULL) && (save_file[0] != '\0'))
		strcpy(afile, save_file);
	}

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}

	if (self->vcs_legacy_gui != 0) {  /* Check for VCS canvas */
           PyErr_SetString(PyExc_TypeError, "Use the 'Animation Control Panel' provided in VCS.");
  	   return NULL;
	}

        /* Check for VCS canvas. If the VCS canvas does not exit, then
         * return.
         */
#ifdef X11WM
        if (self->connect_id.display == NULL) {
#elif defined CAIRODRAW
	  if (self->connect_id.cr == NULL) {
#endif
           PyErr_SetString(PyExc_TypeError, "Error - Must have a VCS Canvas.\n");
           return NULL;
        }

        if (vptr == NULL) {     /* Check for data. Do nothing if not data. */
           Py_INCREF (Py_None);
           return Py_None;
        }

	/* Set up the VCS Canvas and XGKS workstation
        */
        setup_canvas_globals(self);

        animate_module(self->canvas_id);
        self->connect_id.animate_popup = (int )self->canvas_id;
	update_vcs_legacy_connection_information(self->connect_id, self->canvas_id);
	self->virgin_animation = 0;
        self->frame_count = 0;
        self->number_of_frames = create_image_toggle_cb(self->canvas_id, afile, NULL);
        if (self->gui == 1) {
             update_end_of_animation( self );
        }
	/* vcs_legacy_Qt_animation_created(self->canvas_id); */
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Load the animation from a raster file into memory.
 */
static PyObject *
PyVCS_animate_load(PyVCScanvas_Object *self, PyObject *args)
{
	char 			*load_file=NULL, lfile[MAX_PATH_LEN];
	extern void 		update_vcs_legacy_connection_information();
        extern int		load_from_disk();
	extern int 		animate_module();

	/* Check for animation file name */
	lfile[0] = '\0';
  	if(PyArg_ParseTuple(args, "|s", &load_file)) {
	   if ((load_file != NULL) && (load_file[0] != '\0'))
		strcpy(lfile, load_file);
	}

        if (lfile[0] == '\0') {
           PyErr_SetString(PyExc_TypeError, "Error - Must specify a valid Raster file.\n");
           return NULL;
        }

	/* Set up the VCS Canvas and XGKS workstation
        */
        setup_canvas_globals(self);

        animate_module(self->canvas_id);
        self->connect_id.animate_popup = (int )self->canvas_id;
	update_vcs_legacy_connection_information(self->connect_id, self->canvas_id);
	self->virgin_animation = 0;
        self->frame_count = 0;
        self->number_of_frames = load_from_disk(self->canvas_id, lfile, NULL);

        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Return the animation information such as: template name, graphics method,
 * and graphics type.
 */
static PyObject *
PyVCS_animate_info(PyVCScanvas_Object *self, PyObject *args)
{
        VCSCANVASLIST_LINK vptr=head_canvas_list;
        PyObject           *tlistptr=NULL, *gmlistptr=NULL, *gnlistptr=NULL;
        PyObject           *dictptr=NULL;
        int                i, size=0;

        while (vptr != NULL) { ++size; vptr = vptr->next; }
        vptr=head_canvas_list;
	dictptr = PyDict_New();
        tlistptr=PyList_New(size);
        gmlistptr=PyList_New(size);
        gnlistptr=PyList_New(size);
        for (i=0; i<size; i++) {
           PyList_SetItem(tlistptr,  i, Py_BuildValue("s", vptr->template)); 
           PyList_SetItem(gmlistptr, i, Py_BuildValue("s", vptr->type)); 
           PyList_SetItem(gnlistptr, i, Py_BuildValue("s", vptr->graphics)); 
           vptr = vptr->next;
        }
        PyDict_SetItem(dictptr, Py_BuildValue("s","template"), tlistptr);
        PyDict_SetItem(dictptr, Py_BuildValue("s","gtype"), gmlistptr);
        PyDict_SetItem(dictptr, Py_BuildValue("s","gname"), gnlistptr);

        return dictptr;
}

/* Return the number of animate frames stored in Memory */
static PyObject *
PyVCS_animate_number_of_frames(PyVCScanvas_Object *self, PyObject *args)
{
        /* Return the number of animation frames */
  	return Py_BuildValue("i", self->number_of_frames);
}

/* Return the animate frame count  */
static PyObject *
PyVCS_animate_frame_count(self, args) 
  PyVCScanvas_Object *self;
  PyObject *args;
{
        /* Return the number of animation frames */
        return Py_BuildValue("i", self->frame_count);
}


/* Run the animation loop. */
static PyObject *
PyVCS_animate_run(PyVCScanvas_Object *self, PyObject *args)
{
	extern void  RunAnimation();

#ifdef QTWM
	     vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#endif
        RunAnimation(self->canvas_id, NULL, NULL);
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* put a png onto Canvas - no check for sizes!! */
static PyObject *
PyVCS_put_png(PyVCScanvas_Object *self, PyObject *args) {
    char *fnm;
    float zoom;
    int vert,horiz;

#ifdef QTWM
    extern vcs_legacy_Qt_put_image_from_png_file(int, float, int, int, char *);

    //vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
    if (PyArg_ParseTuple(args,"sfii",&fnm,&zoom,&vert,&horiz)) { 
       vcs_legacy_Qt_put_image_from_png_file(self->connect_id.wkst_id, zoom, vert, horiz, fnm);
    }
#endif
    return Py_None;
}
/* Stop the animation loop */
static PyObject *
PyVCS_animate_stop(PyVCScanvas_Object *self, PyObject *args)
{
	extern void 	StopAnimation();

        StopAnimation(self->canvas_id, NULL, NULL);
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;

}

/* Stop the creating animation images */
static PyObject *
PyVCS_animate_stop_create(PyVCScanvas_Object *self, PyObject *args)
{
        extern void     StopAnimationCreate();
        StopAnimationCreate(self->canvas_id, NULL, NULL);
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
 
}

/* Set the min and max values for the animation */
static PyObject *
PyVCS_animate_set_min_max(PyVCScanvas_Object *self, PyObject *args)
{
        float	       min, max;
        extern int     set_animation_min_and_max();
        extern float   animation_min_max[3];
 
        /* Set the animation min and max flag and values */
        animation_min_max[0] = 2;
        animation_min_max[1] = 1e20;
        animation_min_max[2] = 1e20;
        if ((args != NULL) && PyArg_ParseTuple(args,"|ff", &min, &max)) {
              animation_min_max[1] = min;
              animation_min_max[2] = max;
        }

/* DNW - This call will change the graphics method. No longer needed
        set_animation_min_and_max();
*/
 
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
 
}

/* When the animation is stopped, then the user can specify which
 * animation frame to view
 */
static PyObject *
PyVCS_animate_frame(PyVCScanvas_Object *self, PyObject *args)
{
        int 		value=1;
        extern void     ScalePosition();
 
        if ((args != NULL) && PyArg_ParseTuple(args,"|i", &value)) {
           if (value > 0) {
#ifdef QTWM
	     vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#endif
               ScalePosition(self->canvas_id, value, NULL);
           }
        }
 
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
 
}

/* Pause the animation frame by an incremental amount */
static PyObject *
PyVCS_animate_pause(PyVCScanvas_Object *self, PyObject *args)
{
        int 		value=0;
        extern void     ScaleSpeed();
 
        if ((args != NULL) && PyArg_ParseTuple(args,"|i", &value)) {
           if ((value >= 0) && (value <= 100)) {
#ifdef QTWM
	     vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#endif
               ScaleSpeed(self->canvas_id, value, NULL);
           }
        }
 
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
 
}

/* Zoom in on the animation frames */
static PyObject *
PyVCS_animate_zoom(PyVCScanvas_Object *self, PyObject *args)
{
        int 		value=-99;
        extern void     ScaleZoom();
 
        if ((args != NULL) && PyArg_ParseTuple(args,"|i", &value)) {
           if ((value != -99) && (value > 0)) {
#ifdef QTWM
	     vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#endif
               ScaleZoom(self->canvas_id, value, NULL);
           }
        }

        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Pan, in the horizontal direction, a zoomed image or the animation */
static PyObject *
PyVCS_animate_horizontal(PyVCScanvas_Object *self, PyObject *args)
{
        int             value=-999;
        extern void     ScaleHori();
 
        if ((args != NULL) && PyArg_ParseTuple(args,"|i", &value)) {
           if ((value > -101) && (value < 101)) {
#ifdef QTWM
	     vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#endif
               ScaleHori(self->canvas_id, value, NULL);
           }
        }
 
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Pan, in the vertical direction, a zoomed image or the animation */
static PyObject *
PyVCS_animate_vertical(PyVCScanvas_Object *self, PyObject *args)
{
        int             value=-999;
        extern void     ScaleVert();
 
        if ((args != NULL) && PyArg_ParseTuple(args,"|i", &value)) {
           if ((value > -101) && (value < 101)) {
 #ifdef QTWM
	     vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#endif
              ScaleVert(self->canvas_id, value, NULL);
           }
        }
 
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Set the animation direction, forward or backward */
static PyObject *
PyVCS_animate_direction(PyVCScanvas_Object *self, PyObject *args)
{
        int             value=-999;
        extern void     animate_direction_cb();
 
        if ((args != NULL) && PyArg_ParseTuple(args,"|i", &value)) {
           if ((value > 0) && (value < 3)) {
#ifdef QTWM
	     vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#endif
               animate_direction_cb(self->canvas_id, value);
           }
        }
 
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Set the animation mode to cycle or forth and back */
static PyObject *
PyVCS_animate_mode(PyVCScanvas_Object *self, PyObject *args)
{
        int             value=-999;
        extern void     animate_mode_cb();
 
        if ((args != NULL) && PyArg_ParseTuple(args,"|i", &value)) {
           if ((value > 0) && (value < 4)) {
#ifdef QTWM
	     vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#endif
               animate_mode_cb(self->canvas_id, value);
           }
        }
 
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Close out the animation session */
static PyObject *
PyVCS_animate_close(PyVCScanvas_Object *self, PyObject *args)
{
        int             value=-999;
        extern void     animate_quit_cb(int w);
 
	/* Charles Doutriaux 2009-05-14 */
	/* this takes only 1 arg ! */
	/*        animate_quit_cb(self->canvas_id, value); */
        animate_quit_cb(self->canvas_id);
 
        /* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/*
 * Return 0 if the animation is complete or 1 if VCS is animation creating an animation.
 */
static PyObject *
PyVCS_creating_animation(PyVCScanvas_Object *self, PyObject *args)
{
	extern int 	ReturnAnmationCreate_flg();

        return Py_BuildValue("i", ReturnAnmationCreate_flg(self->canvas_id) );
}

/*
 * This function is only used for the VCS Canvas GUI. It returns the slab dimension information. 
 */
static PyObject *
PyVCS_return_dimension_information(PyVCScanvas_Object *self, PyObject *args)
{
        PyObject                        *dim_name_listptr=NULL, *dim_size_listptr=NULL, *dim_units_listptr=NULL;
        PyObject                        *dictptr=NULL;
        canvas_display_list             *cdptr;
        int				i, rank;
        char                            a_name[6][17];
        struct display_tab              *dtab;
        struct a_attr                   *pa;
        struct a_tab                    *ptab=NULL;
        extern struct a_tab             A_tab;
        extern struct display_tab       D_tab;

        /* Find the correct slab that is plotted */
        cdptr = self->dlist;
        while (cdptr != NULL) {
           dtab=&D_tab;
           while ((dtab != NULL) && 
                  (strcmp(dtab->name, cdptr->display_name) != 0))
                 dtab = dtab->next;
           if (dtab != NULL) {  /* we now have the correct display, now get the correct pointer to the slab */
              strcpy(a_name[0], dtab->a[0]); /* get the first array */
              ptab=&A_tab;
              pa=ptab->pA_attr;
              while ((ptab != NULL) && (strcmp(ptab->name, a_name[0]) != 0)) {
                 ptab=ptab->next;
              }
           }
           cdptr = cdptr->next;
        }

	/* Create the Python Dictionary */
        dictptr = PyDict_New( );

	/* Now extract the dimension information */
        rank = pa->ND;
	dim_name_listptr = PyList_New( rank );
	dim_size_listptr = PyList_New( rank );
	dim_units_listptr = PyList_New( rank );
	for (i=0; i<rank; i++) {
              PyList_SetItem(dim_name_listptr, i, Py_BuildValue("s", pa->xn[i]));
              PyList_SetItem(dim_size_listptr, i, Py_BuildValue("i", *pa->xs[i]));
              PyList_SetItem(dim_units_listptr, i, Py_BuildValue("s", pa->xu[i]));
	}
	PyDict_SetItem(dictptr, Py_BuildValue("s","name"), dim_name_listptr);
	PyDict_SetItem(dictptr, Py_BuildValue("s","size"), dim_size_listptr);
	PyDict_SetItem(dictptr, Py_BuildValue("s","units"), dim_units_listptr);

        return dictptr;
}

/*
 * This function is only used for the VCS Canvas GUI. After the template editor (i.e., Edit Plot)
 * has completed, the stored animation data information must be updated to match the edited slab changes.
 */
static PyObject *
PyVCS_update_animation_data(PyVCScanvas_Object *self, PyObject *args)
{
	PyObject		        *slab=NULL, *dobj=NULL;
        extern PyObject		        *slabSetDimensionKey(), *slabSetKey();
	VCSCANVASLIST_LINK              tptr=NULL, vptr=head_canvas_list;
        canvas_display_list             *cdptr;
        int                             slabrank, isLongLat;
        int                             slabRank();
        char                            *dimname;
        char                            *cuslab_name;
        char                            a_name[6][17];
        struct display_tab              *dtab;
        struct a_tab                    *ptab=NULL;
        struct a_attr                   *pa;
        extern struct a_tab             A_tab;
        extern struct display_tab       D_tab;

	/* Get the edited slab and its attributes from the "Edit Plot". */
  	if(!PyArg_ParseTuple(args,"O", &slab)) {
            return NULL;
        }

        /* Find the modified slab */
        cdptr = self->dlist;
        while (cdptr != NULL) {
           dtab=&D_tab;
           while ((dtab != NULL) &&
                  (strcmp(dtab->name, cdptr->display_name) != 0))
                 dtab = dtab->next;
           if (dtab != NULL) {  /* we now have the correct display, now get the correct pointer to the slab */
              strcpy(a_name[0], dtab->a[0]); /* get the first array */
              ptab=&A_tab;
              pa=ptab->pA_attr;
              while ((ptab != NULL) && (strcmp(ptab->name, a_name[0]) != 0)) {
                 ptab=ptab->next;
              }
           }
           cdptr = cdptr->next;
        }

	/* Now get the stored Animation slab that must be updated to reflect the changes */
#ifdef X11WM
	while (connect_id.drawable != vptr->connect_id.drawable)
#elif defined QTWM
	while (connect_id.cr != vptr->connect_id.cr)
#endif
              vptr = vptr->next;
        if (vptr == NULL) {
           Py_INCREF(Py_None);
           return Py_None;
	}

        /* Update the Animation slab with the modified Edit Plot changes */
        slabrank = slabRank(vptr->slab);
        dobj = slabSetKey (vptr->slab, "filename", pa->F);    /* Set the Filename */
        dobj = slabSetKey (vptr->slab, "source", pa->S);      /* Set the Source */
        dobj = slabSetKey (vptr->slab, "name", pa->N);        /* Set the Name */
        dobj = slabSetKey (vptr->slab, "title", pa->TI);      /* Set the Title */
        dobj = slabSetKey (vptr->slab, "units", pa->U);       /* Set the Units */
        dobj = slabSetKey (vptr->slab, "comment1", pa->com1); /* Set the Comment 1 */
        dobj = slabSetKey (vptr->slab, "comment2", pa->com2); /* Set the Comment 2 */
        dobj = slabSetKey (vptr->slab, "comment3", pa->com3); /* Set the Comment 3 */
        dobj = slabSetKey (vptr->slab, "comment4", pa->com4); /* Set the Comment 4 */
        dobj = slabSetDimensionKey (vptr->slab, slabrank-1, "name", pa->xn[0]); /* Set the Xname */
        dobj = slabSetDimensionKey (vptr->slab, slabrank-2, "name", pa->xn[1]); /* Set the Yname */
        dimname = slabDimensionName (vptr->slab, slabrank-1, &isLongLat);

	/* Return NULL Python Object */
        Py_INCREF (Py_None);
        return Py_None;
}

/* Change the graphic method for a display name */
static PyObject *
PyVCS_change_display_graphic_method(self,args)
  PyVCScanvas_Object *self;
  PyObject *args;
{
  void change_graphic_method();
  char *dsply_name=NULL;
  char *gtype_name=NULL;
  char *gmthd_name=NULL;
  if(PyArg_ParseTuple(args,"|sss", &dsply_name, &gtype_name, &gmthd_name)!=1){
    /* Return NULL Python Object */
    Py_INCREF(Py_None);
    return Py_None;
  }
  change_graphic_method(dsply_name,gtype_name,gmthd_name);
  PyVCS_backing_store(self,args);
  /* Return NULL Python Object */
  Py_INCREF(Py_None);
  return Py_None;
}


/* Change the VCS Canvas orientation to Landscape. */
static PyObject *
PyVCS_landscape(PyVCScanvas_Object *self, PyObject *args)
{
        int                             ier, hold_continents,clear_canvas=1;
        int				 WIDTH, HEIGHT, XPOS, YPOS, CLEAR;
        void 				display_resize_plot();
        int 				undisplay_resize_plot();
	extern struct orientation       Page;
        extern void                     set_up_canvas();
        extern int                      clear_display();
	extern int change_orientation(char *type, Gconid_X_drawable **connect_id_in, int where_from);
	extern struct default_continents Dc;
        PyObject * 			PyVCS_clear(PyVCScanvas_Object *self, PyObject *args);

        /* Check to see if vcs_legacy has been initalized */
        if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
           return NULL;
        }

        /* If the VCS Canvas is not open, then return. */
#ifdef X11WM
        if (self->connect_id.drawable == 0) {
#else
	  if (self->connect_id.cr == NULL) {
#endif
           strcpy(Page.page_orient,"landscape");
           self->orientation = 0;
           Page.sw=1;
           Py_INCREF(Py_None);
           return Py_None;
          /* PyErr_SetString(PyExc_TypeError, "Must first open VCS (i.e., x.open()).");
           return NULL;*/
        }

        if(PyArg_ParseTuple(args,"|iiiii", &WIDTH, &HEIGHT, &XPOS, &YPOS, &CLEAR)) {
           if ((CLEAR == 0)) clear_canvas = 0;
        }

        if (self->vcs_legacy_gui == 1) {  /* Check for main VCS canvas */
           PyErr_SetString(PyExc_TypeError, "Can not change page orientation for main VCS Canvas.");
           return NULL;
        }

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

        /* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);

        /* Check to clear VCS Canvas. Use the continents type of the original plot*/
        if (clear_canvas == 1)
          PyVCS_clear(self,NULL);
        else
           hold_continents = Dc.selected;

        /* Change the VCS Canvas orientation and set object flag to Landscape */
        self->orientation = 0;
        set_up_canvas(self->connect_id, "landscape", WIDTH, HEIGHT, XPOS, YPOS);
#ifdef X11WM
        XFlush( self->connect_id.display );
        XSync( self->connect_id.display, FALSE );
#elif defined QTMW
	/* nothing to do */
#else
	fprintf(stderr,"insert here your sync/flush funcs\n");
#endif
#ifdef X11WM
        if ( (self->connect_id.display != NULL) && (self->connect_id.drawable != 0) )
           XRaiseWindow(self->connect_id.display, self->connect_id.drawable);
#elif defined QTWM
        if ( self->connect_id.cr != NULL) 
	  vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#else
	fprintf(stderr,"insert here your raise func\n");
#endif
        ier = change_orientation("landscape", &self->connect_id, 3);

        /* Set up the magnification table, used for animation */
        setup_the_magnify_table();

        if (clear_canvas == 0) {
           
	  display_resize_plot( self, undisplay_resize_plot( self ) );
           Dc.selected = hold_continents; /* Restore continent's flag */
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* Change the VCS Canvas orientation to Portrait. */
static PyObject *
PyVCS_portrait(PyVCScanvas_Object *self, PyObject *args)
{
        int                              ier, hold_continents,clear_canvas=1;
        int				 WIDTH, HEIGHT, XPOS, YPOS, CLEAR;
        void 				 display_resize_plot();
        int 				 undisplay_resize_plot();
	extern struct orientation        Page;
        extern void                      set_up_canvas();
        extern int                       clear_display();
	extern int change_orientation(char *type, Gconid_X_drawable **connect_id_in, int where_from);
	extern struct default_continents Dc;
        PyObject * 			 PyVCS_clear(PyVCScanvas_Object *self, PyObject *args);

        /* Check to see if vcs_legacy has been initalized */
        fprintf(stderr,"OK we actually get here %i, %p\n",self->orientation,self);
        if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
           return NULL;
        }

        /* If the VCS Canvas is not open, then return. */
#ifdef X11WM
        if (self->connect_id.drawable == 0) {
#else
	  if (self->connect_id.cr != NULL ) {
#endif
           strcpy(Page.page_orient,"portrait");
           self->orientation = 1;
           Page.sw=1;
           Py_INCREF(Py_None);
           return Py_None;
           /*PyErr_SetString(PyExc_TypeError, "Must first open VCS (i.e., x.open()).");
           return NULL;*/
        }

        if(PyArg_ParseTuple(args,"|iiiii", &WIDTH, &HEIGHT, &XPOS, &YPOS, &CLEAR)) {
           if ((CLEAR == 0)) clear_canvas = 0;
        }

        if (self->vcs_legacy_gui == 1) {  /* Check for main VCS canvas */
           PyErr_SetString(PyExc_TypeError, "Can not change page orientation for main VCS Canvas.");
           return NULL;
        }

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

        /* Set up the VCS Canvas and XGKS workstation */
        setup_canvas_globals(self);
        
        /* Check to clear VCS Canvas. Use the continents type of the original plot*/
        if (clear_canvas == 1)
           PyVCS_clear(self,NULL);
        else
           hold_continents = Dc.selected;

        /* Change the VCS Canvas orientation and set object flag to Portrait. */
        self->orientation = 1;
        set_up_canvas(self->connect_id, "portrait", WIDTH, HEIGHT, XPOS, YPOS);
#ifdef X11WM
        XFlush( self->connect_id.display );
        XSync( self->connect_id.display, FALSE );
#elif defined QTMW
	/* nothing to do */
#else
	fprintf(stderr,"insert here your sync/flush funcs\n");
#endif
#ifdef X11WM
        if ( (self->connect_id.display != NULL) && (self->connect_id.drawable != 0) )
           XRaiseWindow(self->connect_id.display, self->connect_id.drawable);
#elif defined QTWM
        if ( self->connect_id.cr != NULL) 
	  vcs_legacy_Qt_open_window_by_id(self->connect_id.wkst_id);
#else
	fprintf(stderr,"insert here your raise func\n");
#endif
        ier = change_orientation("portrait", &self->connect_id, 3);

        /* Set up the magnification table, used for animation */
        setup_the_magnify_table();

        if (clear_canvas == 0) {
           
	  display_resize_plot( self, undisplay_resize_plot( self ) );
           Dc.selected = hold_continents; /* Restore continent's flag */
        }

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* Return VCS's orientation. Will return either "landscape" or "portrait". */
static PyObject *
PyVCS_return_orientation(PyVCScanvas_Object *self, PyObject *args)
{
    /* Bellow seems to pick up only canvas 0 */
  /*      extern struct orientation       Page;

  	return Py_BuildValue("s", Page.page_orient);
*/
if (self->orientation==0) {
    return Py_BuildValue("s","landscape");
}
else {
    return Py_BuildValue("s","portrait");
}
}

/* Update VCS's page orientation. */
static PyObject *
PyVCS_update_orientation(PyVCScanvas_Object *self, PyObject *args)
{
        char            type[10];
        extern struct 	orientation Page;
	extern int change_orientation(char *type, Gconid_X_drawable **connect_id_in, int where_from);

        /* Reset the canvas to landscape or portrait */
        if (strcmp(Page.page_orient,"landscape") == 0) {
            strcpy(type,"portrait");
            change_orientation(type,&self->connect_id, 2);
        } else if (strcmp(Page.page_orient,"portrait") == 0) {
            strcpy(type,"landscape");
            change_orientation(type,&self->connect_id, 1);
        }
          
        /* Reset the canvas flag settings back */
        if (strcmp(Page.page_orient,"landscape") == 0)
            strcpy(Page.page_orient, "portrait");
        else if (strcmp(Page.page_orient,"portrait") == 0)
            strcpy(Page.page_orient, "landscape");
            
        /* Set up the magnification table, used for animation */
        setup_the_magnify_table();

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

static PyObject *
PyVCS_addFont(PyVCScanvas_Object *self, PyObject *args)
{
	char		*fname=NULL, *fpath;
	int error=0;
	extern int AddFont();
	extern struct table_FT_VCS_FONTS TTFFONTS;
	struct table_FT_VCS_FONTS *current_font;

        /* If the GUI was not stated (i.e., cdatgui), then we need to
         * process all the X events before we move on.
         */
        if (not_using_gui)
           process_cdat_events();

        if(PyArg_ParseTuple(args, "|ss", &fpath, &fname)) {
	  error = AddFont(fpath,fname);
	}
	if (error!=0)
	  {
	    /* Return NULL Python Object */
	    Py_INCREF(Py_None);
	    return Py_None;
	  }
	else
	  {
	    current_font=&TTFFONTS;
	    while (current_font->next != NULL)
	      {
		current_font=current_font->next;
	      }
	    return Py_BuildValue("s", current_font->name);
	  }
}
extern FT_Face FT_FACE_FONTS[MAX_FONTS];
extern cairo_font_face_t *CAIRO_FONT_FACES[MAX_FONTS];

static PyObject *
PyVCS_switchFontNumbers(PyVCScanvas_Object *self, PyObject *args)
{
	int number1,number2;
	FT_Face face;
	cairo_font_face_t *cairo_face;
	extern struct table_FT_VCS_FONTS TTFFONTS;
	struct table_FT_VCS_FONTS *current_font,*current_font2;
        if(PyArg_ParseTuple(args, "|ii", &number1,&number2)) {
	  current_font=&TTFFONTS;
	  while ((current_font != NULL) && (current_font->index!=number1)) 
	    {
	      current_font=current_font->next;
	    }
	  if (current_font==NULL) {
	    PyErr_SetString(VCS_Error, "VCS first font number not found!");
	    return NULL;
	  }
	  current_font2=&TTFFONTS;
	  while ((current_font2 != NULL) && (current_font2->index!=number2)) 
	    {
	      current_font2=current_font2->next;
	    }
	  if (current_font2==NULL) {
	    PyErr_SetString(VCS_Error, "VCS second font number not found!");
	    return NULL;
	  }
	  if ((current_font2->index==1) || (current_font->index==1))
	    {
	      PyErr_SetString(VCS_Error, "Cannot switch font 1");
	      return NULL;
	    }
	  current_font->index=number2;
	  current_font2->index=number1;
	  face = FT_FACE_FONTS[number1];
	  cairo_face = CAIRO_FONT_FACES[number1];
	  FT_FACE_FONTS[number1]=FT_FACE_FONTS[number2];
	  CAIRO_FONT_FACES[number1]=CAIRO_FONT_FACES[number2];
	  FT_FACE_FONTS[number2]=face;
	  CAIRO_FONT_FACES[number2]=cairo_face;
	  number1 = current_font->loaded;
	  current_font->loaded=current_font2->loaded;
	  current_font2->loaded=number1;
	}
        Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PyVCS_copyFontNumber1to2(PyVCScanvas_Object *self, PyObject *args)
{
	int number1,number2;
	extern struct table_FT_VCS_FONTS TTFFONTS;
	struct table_FT_VCS_FONTS *source_font,*target_font;
        if(PyArg_ParseTuple(args, "|ii", &number1,&number2)) {
	  source_font=&TTFFONTS;
	  while ((source_font != NULL) && (source_font->index!=number1)) 
	    {
	      source_font=source_font->next;
	    }
	  if (source_font==NULL) {
	    PyErr_SetString(VCS_Error, "VCS source font number not found!");
	    return NULL;
	  }
	  target_font=&TTFFONTS;
	  while ((target_font != NULL) && (target_font->index!=number2)) 
	    {
	      target_font=target_font->next;
	    }
	  if (target_font==NULL) {
	    PyErr_SetString(VCS_Error, "VCS target font number not found!");
	    return NULL;
	  }
	  strcpy(target_font->path,source_font->path);
/* 	  if (target_font->index!=1) 	   */
	    {
	      strcpy(target_font->name,source_font->name);
	      FT_FACE_FONTS[target_font->index] = FT_FACE_FONTS[source_font->index];
	      CAIRO_FONT_FACES[target_font->index]=CAIRO_FONT_FACES[source_font->index];
	      target_font->loaded = source_font->loaded;
	    }
	}
	else
	  {
	    PyErr_SetString(VCS_Error, "Error you must pass 2 integers!");
	  }
	Py_INCREF(Py_None);
	return Py_None;

}

static PyObject *
PyVCS_getFontNumber(PyVCScanvas_Object *self, PyObject *args)
{
	char		*fname;
	int number;
	extern struct table_FT_VCS_FONTS TTFFONTS;
	struct table_FT_VCS_FONTS *current_font;

        if(PyArg_ParseTuple(args, "|s", &fname)) {
	  number = -1;
	  current_font=&TTFFONTS;
	  while ((current_font != NULL) && (strcmp(current_font->name,fname) != 0)) 
	    {
	      current_font=current_font->next;
	    }
	  if (current_font!=NULL) number=current_font->index;
	}
	return Py_BuildValue("i", number);
}

static PyObject *
PyVCS_getFontName(PyVCScanvas_Object *self, PyObject *args)
{
	int		number;
	extern struct table_FT_VCS_FONTS TTFFONTS;
	struct table_FT_VCS_FONTS *current_font;


        if(PyArg_ParseTuple(args, "|i", &number)) {
	  current_font=&TTFFONTS;
	  while ((current_font != NULL) && (current_font->index!=number)) 
	    {
	      current_font=current_font->next;
	    }
	  if (current_font!=NULL) return Py_BuildValue("s", current_font->name);
	}
	/* we didn't find it returning null string */
	return Py_BuildValue("s", "");
}

static PyObject *
PyVCS_gettextextent(self,args)
  PyVCScanvas_Object *self;
  PyObject *args;
{
  PyObject *listout;
  char *Tt_name,*To_name;
  struct table_text *tttab;
  struct table_chorn *pTo;
  extern struct table_text Tt_tab;
  extern struct table_chorn To_tab;
  int counter,iprim,j;
  struct points_struct		*xptr=NULL, *yptr=NULL;
  struct array_segments   	*xpts=NULL, *ypts=NULL;
  struct char_segments		*tx=NULL;
  float x1,x2,x3,x4,y1,y2,y3,y4;
  Gextent extent;
  Gpoint pxy;
  extern Gpoint proj_convert();
  extern Gpoint invert_proj_convert();
  void printextent();

  if(PyArg_ParseTuple(args, "|ss", &Tt_name, &To_name)) {
    for (tttab=&Tt_tab; tttab != NULL; tttab=tttab->next)
      if (strcmp(tttab->name,Tt_name) == 0) break;
    for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
      if (strcmp(pTo->name,To_name) == 0) break;
    if (tttab->ts == NULL) {
        Py_INCREF(Py_None);
	return Py_None;
    }
    set_viewport_and_worldcoordinate ( tttab->tvp, tttab->twc,tttab->proj );
    set_text_attr(tttab,pTo);
    xptr = tttab->tx; yptr = tttab->ty;
    xpts = xptr->ps;  ypts = yptr->ps;
    tx = tttab->ts->ss;
    listout = PyList_New(xpts->npts);
    for (iprim=0;iprim<xptr->nsegs;iprim++){ /*loop thru all text drawns...*/
      for (j=0;j<xpts->npts;j++) {
	pxy.x=xpts->pts[j];
	pxy.y=ypts->pts[j];
	pxy=proj_convert(pxy);
	extent.ll.x = extent.ul.x = 0.0;
	extent.lr.x = extent.ur.x = 0.0;
	extent.ll.y = extent.lr.y = 0.0;
	extent.ul.y = extent.ur.y = 0.0;
#ifdef CAIRODRAW
	cairogqtxx(self->wkst_id, pxy,tx->cpts, &extent);
#else
	gqtxx(self->wkst_id, pxy,tx->cpts, &extent);
#endif
	pxy = invert_proj_convert(extent.ll);
	x1 = pxy.x;
	y1=pxy.y;
	pxy = invert_proj_convert(extent.lr);
	x2 = pxy.x;
	y2=pxy.y;
	pxy = invert_proj_convert(extent.ur);
	x3 = pxy.x;
	y3=pxy.y;
	pxy = invert_proj_convert(extent.ul);
	x4 = pxy.x;
	y4=pxy.y;
	tx=tx->next;
	PyList_SetItem(listout, j, Py_BuildValue("[[d,d],[d,d],[d,d],[d,d]]", x1,y1,x2,y2,x3,y3,x4,y4));

     }
      xpts = xpts->next;
      ypts = ypts->next;
    }
    return listout;
  }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
PyVCS_startQtApp(self,args)
  PyVCScanvas_Object *self;
  PyObject *args;
{
#ifdef QTWM
  Py_BEGIN_ALLOW_THREADS
    /* PyEval_AcquireLock(); */
    /* PyEval_ReleaseLock(); */
  createVCSCanvases();
  Py_END_ALLOW_THREADS
#endif
  Py_INCREF(Py_None);
  return Py_None;
}
static PyObject *
PyVCS_getdotdirectory(self,args)
  PyVCScanvas_Object *self;
  PyObject *args;
{
    char a[1024],b[1024];
    strcpy(a,DOT_DIRECTORY);
    strcpy(b,DOT_DIRECTORY_ENV);
    return Py_BuildValue("ss",a,b);
}

static PyMethodDef PyVCScanvas_methods[] =
{
/* General functions */
  {"init", PyVCS_init, 1},
  {"startQtApp",PyVCS_startQtApp, 1},
  {"open", (PyCFunction)PyVCS_open, 1},
  {"canvasid", (PyCFunction)PyVCS_canvas_id, 1},
  {"connect_gui_and_canvas", (PyCFunction)PyVCS_connect_gui_and_canvas, 1},
  {"close", (PyCFunction)PyVCS_close, 1},
  {"destroy", (PyCFunction)PyVCScanvas_Dealloc, 1},
  {"page", (PyCFunction)PyVCS_orientation, 1},
  {"geometry", (PyCFunction)PyVCS_geometry, 1},
  {"canvasinfo", (PyCFunction)PyVCS_canvasinfo, 1},
  {"show", (PyCFunction)PyVCS_show, 1},
  {"listelements", (PyCFunction)PyVCS_listelements, 1},
  {"set", (PyCFunction)PyVCS_set, 1},
  {"grid", (PyCFunction)PyVCS_plotregion, 1},
  {"resetgrid", (PyCFunction)PyVCS_resetplotregion, 1},
  {"plot", (PyCFunction)PyVCS_plot, 1},
  {"savecontinentstype", (PyCFunction)PyVCS_savecontinentstype, 1},
  {"setcontinentstype", (PyCFunction)PyVCS_setcontinentstype, 1},
  {"getcontinentstype", (PyCFunction)PyVCS_getcontinentstype, 1},
  {"setminmax", (PyCFunction)PyVCS_setminmax, 1},
  {"setcolormap", (PyCFunction)PyVCS_setcolormap, 1},
  {"updateVCSsegments", (PyCFunction)PyVCS_updateVCSsegments, 1},
  {"setcolorcell", (PyCFunction)PyVCS_setcolorcell, 1},
  {"getcolorcell", (PyCFunction)PyVCS_getcolorcell, 1},
  {"getcolormapname", (PyCFunction)PyVCS_getcolormapname, 1},
  {"scriptrun", (PyCFunction)PyVCS_scriptrun, 1},
  {"clear", (PyCFunction)PyVCS_clear, 1},
  {"cgm", (PyCFunction)PyVCS_cgm, 1},
  {"raster", (PyCFunction)PyVCS_raster, 1},
  {"gs", (PyCFunction)PyVCS_gs, 1},
  {"gif_or_eps", (PyCFunction)PyVCS_gif_or_eps, 1},
  {"postscript_old", (PyCFunction)PyVCS_postscript_old, 1},
  {"postscript", (PyCFunction)PyVCS_postscript, 1},
  {"svg", (PyCFunction)PyVCS_svg, 1},
  {"png", (PyCFunction)PyVCS_png, 1},
  {"pdf", (PyCFunction)PyVCS_pdf, 1},
  {"printer", (PyCFunction)PyVCS_printer, 1},
  {"showbg", (PyCFunction)PyVCS_showbg, 1},
  {"setbgoutputdimensions", (PyCFunction)PyVCS_setbgoutputdimensions, 1},
  {"backing_store", (PyCFunction)PyVCS_backing_store, 1},
  /*{"refreshcanvas", (PyCFunction)PyVCS_refreshcanvas, 1},*/
  /*{"flushcanvas", (PyCFunction)PyVCS_flushcanvas, 1},*/
  {"flush", (PyCFunction)PyVCS_flush, 1},
  {"landscape", (PyCFunction)PyVCS_landscape, 1},
  {"portrait", (PyCFunction)PyVCS_portrait, 1},
  {"orientation", (PyCFunction)PyVCS_return_orientation, 1},
  {"updateorientation", (PyCFunction)PyVCS_update_orientation, 1},
  {"updatecanvas", (PyCFunction)PyVCS_updatecanvas, 1},
  {"updatecanvas_continents", (PyCFunction)PyVCS_updatecanvas_continents, 1},
  {"saveinitialfile", (PyCFunction)PyVCS_saveinitialfile,1},
  {"scriptstate", (PyCFunction)PyVCS_scriptstate,1},
  {"canvasraised", (PyCFunction)PyVCS_canvasraised,1},
  {"iscanvasdisplayed", (PyCFunction)PyVCS_iscanvasdisplayed,1},
  {"dictionarytovcs_legacylist",(PyCFunction)PyVCS_dictionarytovcs_legacylist,1},
/* X server functions */
  {"startxmainloop", (PyCFunction)PyVCS_startxmainloop,1},
  {"stopxmainloop", (PyCFunction)PyVCS_stopxmainloop,1},
  {"THREADED", (PyCFunction)PyVCS_THREADED,1},
  {"BLOCK_X_SERVER", (PyCFunction)PyVCS_BLOCK_X_SERVER,1},
  {"UNBLOCK_X_SERVER", (PyCFunction)PyVCS_UNBLOCK_X_SERVER,1},
  {"xpending", (PyCFunction)PyVCS_Xpending,1},
  {"xsync_discard", (PyCFunction)PyVCS_Xsync_discard,1},
  {"SCREEN_TEMPLATE_FLAG", (PyCFunction)PyVCS_screen_template_flag,1},
  {"SCREEN_GM_FLAG", (PyCFunction)PyVCS_screen_gm_flag,1},
  {"SCREEN_DATA_FLAG", (PyCFunction)PyVCS_screen_data_flag,1},
  {"SCREEN_CHECKMODE_DATA_FLAG", (PyCFunction)PyVCS_checkmode_data_flag,1},
  {"SCREEN_MODE", (PyCFunction)PyVCS_screen_mode,1},
/* Plot annotation functions */
  {"plot_annotation", (PyCFunction)PyVCS_plot_annotation, 1},
/* Animation functions */
  {"animate_init", (PyCFunction)PyVCS_animate_init, 1},
  {"animate_load", (PyCFunction)PyVCS_animate_load, 1},
  {"animate_info", (PyCFunction)PyVCS_animate_info, 1},
  {"animate_number_of_frames", (PyCFunction)PyVCS_animate_number_of_frames, 1},
  {"animate_frame_count", (PyCFunction)PyVCS_animate_frame_count, 1},
  {"animate_run", (PyCFunction)PyVCS_animate_run, 1},
  {"animate_stop", (PyCFunction)PyVCS_animate_stop, 1},
  {"animate_stop_create", (PyCFunction)PyVCS_animate_stop_create, 1},
  {"animate_set_min_max", (PyCFunction)PyVCS_animate_set_min_max, 1},
  {"animate_frame", (PyCFunction)PyVCS_animate_frame, 1},
  {"animate_pause", (PyCFunction)PyVCS_animate_pause, 1},
  {"animate_zoom", (PyCFunction)PyVCS_animate_zoom, 1},
  {"animate_horizontal", (PyCFunction)PyVCS_animate_horizontal, 1},
  {"animate_vertical", (PyCFunction)PyVCS_animate_vertical, 1},
  {"animate_direction", (PyCFunction)PyVCS_animate_direction, 1},
  {"animate_mode", (PyCFunction)PyVCS_animate_mode, 1},
  {"animate_close", (PyCFunction)PyVCS_animate_close, 1},
  {"creating_animation", (PyCFunction)PyVCS_creating_animation, 1},
  {"update_animation_data", (PyCFunction)PyVCS_update_animation_data, 1},
  {"return_dimension_info", (PyCFunction)PyVCS_return_dimension_information, 1},
  {"put_png_on_canvas",(PyCFunction)PyVCS_put_png,1},
/* Display plot functions */
  {"getDpmember", (PyCFunction)PyVCS_getDpmember, 1},
  {"setDpmember", (PyCFunction)PyVCS_setDpmember, 1},
  {"renameDp", (PyCFunction)PyVCS_renameDp, 1},
  {"return_display_names", (PyCFunction)PyVCS_return_display_names, 1},
  {"remove_display_name", (PyCFunction)PyVCS_remove_display_name, 1},
  {"return_display_ON_num", (PyCFunction)PyVCS_return_display_ON_num, 1},
  {"change_display_graphic_method",(PyCFunction)PyVCS_change_display_graphic_method,1},
  {"get_selected_display",(PyCFunction)PyVCS_get_selected_display_graphic_method,1},
/* Template text functions */
  {"getPomember", (PyCFunction)PyVCS_getPomember, 1},
  {"getPtmember", (PyCFunction)PyVCS_getPtmember, 1},
  {"getPfmember", (PyCFunction)PyVCS_getPfmember, 1},
  {"getPxtmember", (PyCFunction)PyVCS_getPxtmember, 1},
  {"getPytmember", (PyCFunction)PyVCS_getPytmember, 1},
  {"getPxlmember", (PyCFunction)PyVCS_getPxlmember, 1},
  {"getPylmember", (PyCFunction)PyVCS_getPylmember, 1},
  {"getPblmember", (PyCFunction)PyVCS_getPblmember, 1},
  {"getPlsmember", (PyCFunction)PyVCS_getPlsmember, 1},
  {"getPdsmember", (PyCFunction)PyVCS_getPdsmember, 1},
  {"setPomember", (PyCFunction)PyVCS_setPomember, 1},
  {"setPtmember", (PyCFunction)PyVCS_setPtmember, 1},
  {"setPfmember", (PyCFunction)PyVCS_setPfmember, 1},
  {"setPxtmember", (PyCFunction)PyVCS_setPxtmember, 1},
  {"setPytmember", (PyCFunction)PyVCS_setPytmember, 1},
  {"setPxlmember", (PyCFunction)PyVCS_setPxlmember, 1},
  {"setPylmember", (PyCFunction)PyVCS_setPylmember, 1},
  {"setPblmember", (PyCFunction)PyVCS_setPblmember, 1},
  {"setPlsmember", (PyCFunction)PyVCS_setPlsmember, 1},
  {"setPdsmember", (PyCFunction)PyVCS_setPdsmember, 1},
  {"copyP", (PyCFunction)PyVCS_copyP, 1},
  {"renameP", (PyCFunction)PyVCS_renameP, 1},
  {"removeP", (PyCFunction)PyVCS_removeP, 1},
  {"scriptP", (PyCFunction)PyVCS_scriptP, 1},
  {"syncP", (PyCFunction)PyVCS_syncP, 1},
  {"_select_one", (PyCFunction)PyVCS_select_one, 1},
  {"_unselect_one", (PyCFunction)PyVCS_unselect_one, 1},
  {"_select_all", (PyCFunction)PyVCS_select_all, 1},
  {"_unselect_all", (PyCFunction)PyVCS_unselect_all, 1},
  {"_set_normalized_flag", (PyCFunction)PyVCS_set_normalized_flag, 1},
  {"_return_normalized_flag", (PyCFunction)PyVCS_return_normalized_flag, 1},
/* Boxfill functions */
  {"getGfbmember", (PyCFunction)PyVCS_getGfbmember, 1},
  {"setGfbmember", (PyCFunction)PyVCS_setGfbmember, 1},
  {"copyGfb", (PyCFunction)PyVCS_copyGfb, 1},
  {"renameGfb", (PyCFunction)PyVCS_renameGfb, 1},
  {"removeGfb", (PyCFunction)PyVCS_removeGfb, 1},
  {"scriptGfb", (PyCFunction)PyVCS_scriptGfb, 1},
/* Isofill functions */
  {"getGfimember", (PyCFunction)PyVCS_getGfimember, 1},
  {"setGfimember", (PyCFunction)PyVCS_setGfimember, 1},
  {"copyGfi", (PyCFunction)PyVCS_copyGfi, 1},
  {"renameGfi", (PyCFunction)PyVCS_renameGfi, 1},
  {"removeGfi", (PyCFunction)PyVCS_removeGfi, 1},
  {"scriptGfi", (PyCFunction)PyVCS_scriptGfi, 1},
/* Isoline functions */
  {"getGimember", (PyCFunction)PyVCS_getGimember, 1},
  {"setGimember", (PyCFunction)PyVCS_setGimember, 1},
  {"copyGi", (PyCFunction)PyVCS_copyGi, 1},
  {"renameGi", (PyCFunction)PyVCS_renameGi, 1},
  {"removeGi", (PyCFunction)PyVCS_removeGi, 1},
  {"scriptGi", (PyCFunction)PyVCS_scriptGi, 1},
/* Outline functions */
  {"getGomember", (PyCFunction)PyVCS_getGomember, 1},
  {"setGomember", (PyCFunction)PyVCS_setGomember, 1},
  {"copyGo", (PyCFunction)PyVCS_copyGo, 1},
  {"renameGo", (PyCFunction)PyVCS_renameGo, 1},
  {"removeGo", (PyCFunction)PyVCS_removeGo, 1},
  {"scriptGo", (PyCFunction)PyVCS_scriptGo, 1},
/* Outfill functions */
  {"getGfomember", (PyCFunction)PyVCS_getGfomember, 1},
  {"setGfomember", (PyCFunction)PyVCS_setGfomember, 1},
  {"copyGfo", (PyCFunction)PyVCS_copyGfo, 1},
  {"renameGfo", (PyCFunction)PyVCS_renameGfo, 1},
  {"removeGfo", (PyCFunction)PyVCS_removeGfo, 1},
  {"scriptGfo", (PyCFunction)PyVCS_scriptGfo, 1},
/* Xyvsy functions */
  {"getGXymember", (PyCFunction)PyVCS_getGXymember, 1},
  {"setGXymember", (PyCFunction)PyVCS_setGXymember, 1},
  {"copyGXy", (PyCFunction)PyVCS_copyGXy, 1},
  {"renameGXy", (PyCFunction)PyVCS_renameGXy, 1},
  {"removeGXy", (PyCFunction)PyVCS_removeGXy, 1},
  {"scriptGXy", (PyCFunction)PyVCS_scriptGXy, 1},
/* Yxvsx functions */
  {"getGYxmember", (PyCFunction)PyVCS_getGYxmember, 1},
  {"setGYxmember", (PyCFunction)PyVCS_setGYxmember, 1},
  {"copyGYx", (PyCFunction)PyVCS_copyGYx, 1},
  {"renameGYx", (PyCFunction)PyVCS_renameGYx, 1},
  {"removeGYx", (PyCFunction)PyVCS_removeGYx, 1},
  {"scriptGYx", (PyCFunction)PyVCS_scriptGYx, 1},
/* XvsY functions */
  {"getGXYmember", (PyCFunction)PyVCS_getGXYmember, 1},
  {"setGXYmember", (PyCFunction)PyVCS_setGXYmember, 1},
  {"copyGXY", (PyCFunction)PyVCS_copyGXY, 1},
  {"renameGXY", (PyCFunction)PyVCS_renameGXY, 1},
  {"removeGXY", (PyCFunction)PyVCS_removeGXY, 1},
  {"scriptGXY", (PyCFunction)PyVCS_scriptGXY, 1},
/* Vector functions */
  {"getGvmember", (PyCFunction)PyVCS_getGvmember, 1},
  {"setGvmember", (PyCFunction)PyVCS_setGvmember, 1},
  {"copyGv", (PyCFunction)PyVCS_copyGv, 1},
  {"renameGv", (PyCFunction)PyVCS_renameGv, 1},
  {"removeGv", (PyCFunction)PyVCS_removeGv, 1},
  {"scriptGv", (PyCFunction)PyVCS_scriptGv, 1},
/* Scatter functions */
  {"getGSpmember", (PyCFunction)PyVCS_getGSpmember, 1},
  {"setGSpmember", (PyCFunction)PyVCS_setGSpmember, 1},
  {"copyGSp", (PyCFunction)PyVCS_copyGSp, 1},
  {"renameGSp", (PyCFunction)PyVCS_renameGSp, 1},
  {"removeGSp", (PyCFunction)PyVCS_removeGSp, 1},
  {"scriptGSp", (PyCFunction)PyVCS_scriptGSp, 1},
/* Continents functions */
  {"getGconmember", (PyCFunction)PyVCS_getGconmember, 1},
  {"setGconmember", (PyCFunction)PyVCS_setGconmember, 1},
  {"copyGcon", (PyCFunction)PyVCS_copyGcon, 1},
  {"renameGcon", (PyCFunction)PyVCS_renameGcon, 1},
  {"removeGcon", (PyCFunction)PyVCS_removeGcon, 1},
  {"scriptGcon", (PyCFunction)PyVCS_scriptGcon, 1},
/* Colormap functions */
  {"getCpmember", (PyCFunction)PyVCS_getCpmember, 1},
  {"setCpmember", (PyCFunction)PyVCS_setCpmember, 1},
  {"copyCp", (PyCFunction)PyVCS_copyCp, 1},
  {"renameCp", (PyCFunction)PyVCS_renameCp, 1},
  {"removeCp", (PyCFunction)PyVCS_removeCp, 1},
  {"scriptCp", (PyCFunction)PyVCS_scriptCp, 1},
/* Line functions */
  {"getTlmember", (PyCFunction)PyVCS_getTlmember, 1},
  {"setTlmember", (PyCFunction)PyVCS_setTlmember, 1},
  {"copyTl", (PyCFunction)PyVCS_copyTl, 1},
  {"renameTl", (PyCFunction)PyVCS_renameTl, 1},
  {"removeTl", (PyCFunction)PyVCS_removeTl, 1},
  {"scriptTl", (PyCFunction)PyVCS_scriptTl, 1},
/* Marker functions */
  {"getTmmember", (PyCFunction)PyVCS_getTmmember, 1},
  {"setTmmember", (PyCFunction)PyVCS_setTmmember, 1},
  {"copyTm", (PyCFunction)PyVCS_copyTm, 1},
  {"renameTm", (PyCFunction)PyVCS_renameTm, 1},
  {"removeTm", (PyCFunction)PyVCS_removeTm, 1},
  {"scriptTm", (PyCFunction)PyVCS_scriptTm, 1},
/* Fillarea functions */
  {"getTfmember", (PyCFunction)PyVCS_getTfmember, 1},
  {"setTfmember", (PyCFunction)PyVCS_setTfmember, 1},
  {"copyTf", (PyCFunction)PyVCS_copyTf, 1},
  {"renameTf", (PyCFunction)PyVCS_renameTf, 1},
  {"removeTf", (PyCFunction)PyVCS_removeTf, 1},
  {"scriptTf", (PyCFunction)PyVCS_scriptTf, 1},
/* Text Table functions */
  {"getTtmember", (PyCFunction)PyVCS_getTtmember, 1},
  {"setTtmember", (PyCFunction)PyVCS_setTtmember, 1},
  {"copyTt", (PyCFunction)PyVCS_copyTt, 1},
  {"renameTt", (PyCFunction)PyVCS_renameTt, 1},
  {"removeTt", (PyCFunction)PyVCS_removeTt, 1},
  {"scriptTt", (PyCFunction)PyVCS_scriptTt, 1},
/* Text Orientation functions */
  {"getTomember", (PyCFunction)PyVCS_getTomember, 1},
  {"setTomember", (PyCFunction)PyVCS_setTomember, 1},
  {"copyTo", (PyCFunction)PyVCS_copyTo, 1},
  {"renameTo", (PyCFunction)PyVCS_renameTo, 1},
  {"removeTo", (PyCFunction)PyVCS_removeTo, 1},
  {"scriptTo", (PyCFunction)PyVCS_scriptTo, 1},
/* Meshfill Functions */
  {"getGfmmember", (PyCFunction)PyVCS_getGfmmember, 1},
  {"setGfmmember", (PyCFunction)PyVCS_setGfmmember, 1},
  {"copyGfm", (PyCFunction)PyVCS_copyGfm, 1},
  {"renameGfm", (PyCFunction)PyVCS_renameGfm, 1},
  {"removeGfm", (PyCFunction)PyVCS_removeGfm, 1},
  {"scriptGfm", (PyCFunction)PyVCS_scriptGfm, 1},
/* Projection Functions */
  {"getProjmember", (PyCFunction)PyVCS_getProjmember, 1},
  {"setProjmember", (PyCFunction)PyVCS_setProjmember, 1},
  {"copyProj", (PyCFunction)PyVCS_copyProj, 1},
  {"renameProj", (PyCFunction)PyVCS_renameProj, 1},
  {"removeProj", (PyCFunction)PyVCS_removeProj, 1},
  {"checkProj", (PyCFunction)PyVCS_checkProj, 1},
  {"scriptProj", (PyCFunction)PyVCS_scriptProj, 1},
  /* Fonts functions */
  {"addfont", (PyCFunction)PyVCS_addFont, 1},
  {"getfontnumber", (PyCFunction)PyVCS_getFontNumber, 1},
  {"getfontname", (PyCFunction)PyVCS_getFontName, 1},
  {"switchfontnumbers", (PyCFunction)PyVCS_switchFontNumbers, 1},
  {"copyfontto", (PyCFunction)PyVCS_copyFontNumber1to2, 1},
  {"gettextextent", (PyCFunction)PyVCS_gettextextent, 1},
  {"getdotdirectory", (PyCFunction)PyVCS_getdotdirectory,1},
  {0, 0} };



static PyObject *
PyVCScanvas_getattr(self, name)
  PyVCScanvas_Object *self;
  char *name;
{
  PyObject *method;

  method = Py_FindMethod(PyVCScanvas_methods, (PyObject *)self, name);
  if (method != NULL)
    return method;
  PyErr_Clear();

  PyErr_SetString(PyExc_AttributeError, name);

  Py_INCREF (Py_None);
  return Py_None;
}

static PyTypeObject PyVCScanvas_Type = {
  PyObject_HEAD_INIT(NULL)
  0,                                      /* ob_size */
  "VCS Canvas",                           /* tp_name */
  sizeof(PyVCScanvas_Object),             /* tp_basicsize */
  0,                                      /* tp_itemsize */
  /* methods */
  (destructor)PyVCScanvas_Dealloc,        /* tp_dealloc */
  0,                                      /* tp_print */
  (getattrfunc)PyVCScanvas_getattr,       /* tp_getattr */
  (setattrfunc)0,                         /* tp_setattr */
  0,                                      /* tp_compare */
  (reprfunc) 0,                           /* tp_repr */
  0,                                      /* tp_as_number */
  0,                                      /* tp_as_sequence */
  0,                                      /* tp_as_mapping */
  0,                                      /* tp_hash */
};

void
PyInit_VCS()
{
  	PyObject *m, *d;
	int i;
        import_array();
        VCS_Error = PyVCS_Error; /* DUBOIS */

        /* Initialize type object headers */
        PyVCScanvas_Type.ob_type = &PyType_Type;

  	m = Py_InitModule("_vcs_legacy", PyVCScanvas_methods);
    
  	d = PyModule_GetDict(m);
  	PyVCS_Error = Py_BuildValue("s", "vcs_legacy.error");
  	PyDict_SetItemString(d,"error", PyVCS_Error);

 	/* Initials the default plot region */
	for (i=0; i<CU_MAX_VAR_DIMS; i++) {
	   index_s[i] = -1;
	   index_e[i] = -1;
	}
}


/* These are versions of the ones in vcs_legacy itself (mmm and immm) but
   they don't set the missing data mask, they use it.
   Assume max number of dimensions is 4.

*/
    int gimmmm(pdat,pmask,pdim,pw,nd,XC,xv,pmin,pmax,pmean)
      int *pdat;	/*  Data array (integers).			*/
      short *pmask;	/*  Data mask.					*/
      int *pdim[];	/*  Dimensions.					*/
      float *pw[];	/*  Weights to use for mean.			*/
      int nd;		/*  Number of dimensions.			*/
      float *XC[];	/*  Cycle length.				*/
      float *xv[];	/*  Dimension values.				*/
      float *pmin;	/*  Minimum data value.				*/
      float *pmax;	/*  Maximum data value.				*/
      float *pmean;	/*  Mean data value.				*/
      {
	int i,j,k,m,c;
	int di,dj,dk,dm;
	float *pwi,*pwj,*pwk,*pwm,one,data;
	float pw3,pw2,pw1,pw0;
	double weight,mean,w;

	if (nd < 1) return 0;

	weight=0.0;
	mean=0.0;
	one=1.0;
	*pmean=0.0;
	*pmin=1.e20;
	*pmax=-1.e20;
	di=dj=dk=dm=1;
	if (nd > 0) di=*pdim[0];
	if (nd > 1) dj=*pdim[1];
	if (nd > 2) dk=*pdim[2];
	if (nd > 3) dm=*pdim[3];

	for (m=0,pwm=(nd>3)?pw[3]:&one;m < dm;m++,pwm++)
	  {
	   pw3=*pwm;
	   if (nd > 3 && *XC[3] > 0.0)
	   pw3=(fabs((double)(*(xv[3]+m)-*(xv[3]))) >=
		(1.0-.01/dm)*fabs((double)*XC[3])) ? 0.0:*pwm;
	   for (k=0,pwk=(nd>2)?pw[2]:&one;k < dk;k++,pwk++)
	     {
	      pw2=*pwk;
	      if (nd > 2 && *XC[2] > 0.0)
	      pw2=(fabs((double)(*(xv[2]+k)-*(xv[2]))) >=
		   (1.0-.01/dk)*fabs((double)*XC[2])) ? 0.0:*pwk;
	      for (j=0,pwj=(nd>1)?pw[1]:&one;j < dj;j++,pwj++)
		{
	         pw1=*pwj;
	         if (nd > 1 && *XC[1] > 0.0)
	         pw1=(fabs((double)(*(xv[1]+j)-*(xv[1]))) >=
		      (1.0-.01/dj)*fabs((double)*XC[1])) ? 0.0:*pwj;
		 for (i=0,pwi=(nd>0)?pw[0]:&one;i < di;i++,pwi++,pdat++,pmask++)
		   {
		    pw0=*pwi;
		    if (*XC[0] > 0.0)
		    pw0=(fabs((double)(*(xv[0]+i)-*(xv[0]))) >=
			(1.0-.01/di)*fabs((double)*XC[0])) ? 0.0:*pwi;
		    data=*pdat;
		    if (*pmask)
		      {
		       w=pw0*pw1*pw2*pw3;
		       mean=mean+w*data;
		       weight=weight+w;
		       *pmin=(data < *pmin)? data : *pmin;
		       *pmax=(data > *pmax)? data : *pmax;
		      }
		   }
		}
	     }
	  }
	if (weight != 0.0)
	  {
	   *pmean=mean/weight;
	   c=1;
	  }
	else
	  {
	   c=0;
	  }
	return c;
      }

/*		Find the max, min, weighted mean, and set the mask
		true if the value is valid (i.e. < 0.999e20).
		Assume max number of dimensions is 4.
		Assume float values of the data.			*/

    int gmmmm(pdat,pmask,pdim,pw,nd,XC,xv,pmin,pmax,pmean)
      float *pdat;	/*  Data array (float).				*/
      short *pmask;	/*  Data mask.					*/
      int *pdim[];	/*  Dimensions.					*/
      float *pw[];	/*  Weights to use for mean.			*/
      int nd;		/*  Number of dimensions.			*/
      float *XC[];	/*  Cycle length.				*/
      float *xv[];	/*  Dimension values.				*/
      float *pmin;	/*  Minimum data value.				*/
      float *pmax;	/*  Maximum data value.				*/
      float *pmean;	/*  Mean data value.				*/
      {
	int i,j,k,m,c;
	int di,dj,dk,dm;
	float *pwi,*pwj,*pwk,*pwm,one;
	float pw3,pw2,pw1,pw0;
	double w,weight,mean;

	if (nd < 1) return 0;

	weight=0.0;
	one=1.0;
	mean=0.0;
	*pmean=0.0;
	*pmin=1.e20;
	*pmax=-1.e20;
	di=dj=dk=dm=1;
	if (nd > 0) di=*pdim[0];
	if (nd > 1) dj=*pdim[1];
	if (nd > 2) dk=*pdim[2];
	if (nd > 3) dm=*pdim[3];

	for (m=0,pwm=(nd>3)?pw[3]:&one;m < dm;m++,pwm++)
	  {
	   pw3=*pwm;
	   if (nd > 3 && *XC[3] > 0.0)
	   pw3=(fabs((double)(*(xv[3]+m)-*(xv[3]))) >=
		(1.0-.01/dm)*fabs((double)*XC[3])) ? 0.0:*pwm;
	   for (k=0,pwk=(nd>2)?pw[2]:&one;k < dk;k++,pwk++)
	     {
	      pw2=*pwk;
	      if (nd > 2 && *XC[2] > 0.0)
	      pw2=(fabs((double)(*(xv[2]+k)-*(xv[2]))) >=
		   (1.0-.01/dk)*fabs((double)*XC[2])) ? 0.0:*pwk;
	      for (j=0,pwj=(nd>1)?pw[1]:&one;j < dj;j++,pwj++)
		{
	         pw1=*pwj;
	         if (nd > 1 && *XC[1] > 0.0)
	         pw1=(fabs((double)(*(xv[1]+j)-*(xv[1]))) >=
		      (1.0-.01/dj)*fabs((double)*XC[1])) ? 0.0:*pwj;
		 for (i=0,pwi=(nd>0)?pw[0]:&one;i < di;i++,pwi++,pdat++,pmask++)
		   {
		    pw0=*pwi;
		    if (*XC[0] > 0.0)
		    pw0=(fabs((double)(*(xv[0]+i)-*(xv[0]))) >=
			(1.0-.01/di)*fabs((double)*XC[0])) ? 0.0:*pwi;
		    if (*pmask)
		      {
		       w=pw0*pw1*pw2*pw3;
		       mean=mean+w*(*pdat);
		       weight=weight+w;
		       *pmin=(*pdat < *pmin)? *pdat : *pmin;
		       *pmax=(*pdat > *pmax)? *pdat : *pmax;
		      }
		   }
		}
	     }
	  }
	if (weight != 0.0)
	  {
	   *pmean=mean/weight;
	   c=1;
	  }
	else
	  {
	   c=0;
	  }
	return c;
      }

/*     This function is for the interactive portion of this
 *     software.  It will create an attribute set for the array
 *     definition, by looking in the array attribute table for the
 *     name, and making a table entry if it doesn't exist, or
 *     returning a zero if it does.
 *
 *     If a new entry in the array table is made the script output
 *     file (if open) receives the attribute assignment.      
 */
void
put_slab_in_VCS_data_struct(PyObject* slab, char* g_name, char* s_name, int position_ct, int animate_flg,int slab_number)
{
	int             	ok_selection,itemp,tndim,isLongLat;
	int			actual_dimsize[CU_MAX_VAR_DIMS],*ip;
	long			i,j,k,nd,gndim,ier,nelems=1,hnelems=1,*lp;
	short			*sp;
	long long               *llp;
	long double             *ldp;
	unsigned long long      *ullp;
	unsigned int            *uip;
	unsigned long           *ulp;
	unsigned short          *usp;
	unsigned char           *ucp;
	float			ftemp, *fp;
	double			*dp;
	char			src[121],tit[81],uni[41],ty[9];
	char			cmt1[121],cmt2[121];
	char			cmt3[121],cmt4[121];
	char			date[20],time[20],buf[100], *cp;
        int 			slabrank;
        char                    slabtype;
        char			*slabfilename;
        char			*dimname;
        char			*dimunits;
        PyArrayObject           *slabdata; 
        PyObject                *slabmask;
        PyArrayObject           *shortmask;
        PyArrayObject           *dimvalues;
        float			*dimarray;
        PyArrayObject		*bounds;
        PyArrayObject           *weights;
        PyObject                *weightsraw; 
        int 			dimiscircular, dimlen;
	int     		start[CU_MAX_VAR_DIMS];
	int     		end[CU_MAX_VAR_DIMS];
	int     		tstart[CU_MAX_VAR_DIMS];
	int     		tend[CU_MAX_VAR_DIMS];
	CuType  		data_type;
        struct a_tab 		*ptab;
        struct a_attr 		*pa;
	extern struct a_tab 	A_tab;
	int			graphics_num_of_dims();
	void 			hyperslab_data();
	extern int      	select_A();
	char                    *varid;
 
        slabrank = slabRank(slab);
        if(PyErr_Occurred()) return;
heartbeat("slabrank=%d", slabrank);
        slabtype = slabType(slab);
        if(PyErr_Occurred()) return;
heartbeat("slabtype=%c", slabtype);
        slabfilename = slabAttribute (slab, "filename", "");

        strncpy(src, 
                slabAttribute (slab, "source", ""), 
                sizeof(src)/sizeof(char));
        strncpy(tit, 
                slabAttribute (slab, "title", ""), 
                sizeof(tit)/sizeof(char));
        strncpy(uni, 
                slabAttribute (slab, "units", ""), 
                sizeof(uni)/sizeof(char));
        strncpy(ty, 
                slabAttribute (slab, "type", ""), 
                sizeof(ty)/sizeof(char));
        slabDateAndTime( slab, position_ct );
        strncpy(date, 
                slabAttribute (slab, "date", ""), 
                sizeof(date)/sizeof(char));
        strncpy(time, 
                slabAttribute (slab, "time", ""), 
                sizeof(time)/sizeof(char));
        if (animate_flg == 1)
           sprintf(buf,"Frame %d", position_ct+1);
        else
           sprintf(buf,"");
        strncpy(cmt1, 
                slabAttribute (slab, "comment1", buf), 
                sizeof(cmt1)/sizeof(char));
        strncpy(cmt2, 
                slabAttribute (slab, "comment2", ""), 
                sizeof(cmt2)/sizeof(char));
        strncpy(cmt3,
                slabAttribute (slab, "comment3", ""), 
                sizeof(cmt3)/sizeof(char));
        strncpy(cmt4, 
                slabAttribute (slab, "comment4", ""), 
                sizeof(cmt4)/sizeof(char));
heartbeat("filename=%s", slabfilename);
heartbeat("src=%s", src);
heartbeat("tit=%s", tit);
heartbeat("uni=%s", uni);
heartbeat("ty=%s", ty);
heartbeat("date=%s", date);
heartbeat("time=%s", time);
heartbeat("cmt1=%s", cmt1);
heartbeat("cmt2=%s", cmt2);
heartbeat("cmt3=%s", cmt3);
heartbeat("cmt4=%s", cmt4);
	/* Create the VCS data structure from the slab structure by
	 * first passing down the appropriate name, source, title,
	 * units, and file name. 
         */
	ok_selection = select_A(s_name, NULL, NULL, NULL, NULL,
                                NULL, "CDAT");
	if (ok_selection == 0) {
           PyErr_SetString(VCS_Error, "vcs_legacy plot, select_A call failed.");
	   return;
        }

	/* Find the newly created VCS data structure. */
        ptab=&A_tab;
        while ((ptab != NULL) && (strcmp(ptab->name,s_name) != 0)) {
           ptab=ptab->next;
        }
	if (ptab == NULL) {
	   PyErr_SetString(VCS_Error, "VCS data name not found!");
	   return;
	}

	ptab->FROM_CDAT = 1; /* array originated from CDAT */

	pa=ptab->pA_attr;

	pa->notok=0; /* Assume the definition is A-Okay  */

	/* Set the dimsizes in a list */
        for (i=0; i<slabrank; ++i) {
          if (pa->xi[i] == NULL &&
              (pa->xi[i]=(int *)malloc(sizeof(int))) == NULL) {
              PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!");
              return;
          }
	  pa->xi[i][0] = 0;
          if (pa->xj[i] == NULL &&
              (pa->xj[i]=(int *)malloc(sizeof(int))) == NULL) {
              PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!"); 
              return;
          }
	  pa->xj[i][0] = 1;
          if (pa->XS[i] == NULL &&
              (pa->XS[i]=(int *)malloc(sizeof(int))) == NULL) {
              PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!"); 
              return;
          }
	  pa->XS[i][0] = actual_dimsize[i] = slabDimensionLength(slab, slabrank-1-i);
          if (pa->XK[i] == NULL &&
              (pa->XK[i]=(int *)malloc(2*sizeof(int))) == NULL) {
              PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!"); 
              return;
          }
	  pa->XK[i][0] = 0;
	  pa->XK[i][1] = 0;
        }
	heartbeat("%s", "Dimension sizes set");
        /* 
           Find out how many dimensions the graphics method is expecting. 
           If there are more than needed go through hyperslab to extract
        */
	
	/* set the attributes*/
	varid = slabAttribute(slab, "name", "");
	if (strcmp(varid, "")==0) varid = s_name;
	pa->F =repstr(pa->F,slabfilename);
	pa->S =repstr(pa->S, src);
	pa->N =repstr(pa->N, varid);
	pa->TI=repstr(pa->TI, tit);
	pa->U =repstr(pa->U, uni);
	pa->TY=repstr(pa->TY, ty);
	pa->CRD=repstr(pa->CRD, date);
	pa->CRT=repstr(pa->CRT, time);
	pa->s =repstr(pa->s, src);
	pa->n =repstr(pa->n, varid);
	pa->ti=repstr(pa->ti, tit);
	pa->u =repstr(pa->u, uni);
	pa->ty=repstr(pa->ty, ty);
	pa->crd=repstr(pa->crd, date);
	pa->crt=repstr(pa->crt, time);
	pa->af=repstr(pa->af,"CDAT");
	pa->aS =repstr(pa->aS, src);
	pa->com1 =repstr(pa->com1, cmt1);
	pa->com2 =repstr(pa->com2, cmt2);
	pa->com3 =repstr(pa->com3, cmt3);
	pa->com4 =repstr(pa->com4, cmt4);
	pa->aN =repstr(pa->aN, s_name);
	pa->aTI=repstr(pa->aTI, tit);
	pa->aU =repstr(pa->aU, uni);
	pa->aTY=repstr(pa->aTY, ty);
	pa->ND = slabrank;
	heartbeat("%s", "Atributes set.");

        gndim = graphics_num_of_dims(g_name, slabrank, slab_number);
	if (gndim > slabrank) {
	   sprintf(buf,"Graphics method must have data with at least %d dimensions.", gndim);
           PyErr_SetString(VCS_Error, buf);
	   return ;
        }
	if (gndim == slabrank) 
	  {
	    heartbeat("%s", "Case of gndim == slabrank");
	    for (i=0; i<slabrank; ++i) 
	      {
		dimlen = slabDimensionLength (slab, slabrank-1-i);
		heartbeat("dimension length is %d", dimlen);
		if(PyErr_Occurred()) return;
		start[i] = 0.;
		end[i] =  dimlen - 1;
		hnelems = ((long) hnelems) * ((long) dimlen);
	      }
	    nelems = hnelems;
	    if ((pa->un.data=(float *)malloc(hnelems*sizeof(float)))==NULL) 
	      {
		PyErr_SetString(VCS_Error, "Not enough memory to store plot data.");
		return;
	      }
	    if (pa->mask!=NULL) 
	      {
		free((char *) pa->mask);
		pa->mask=NULL;
	      }
	    if ((pa->mask=(short *)malloc(nelems*sizeof(short))) == NULL) 
	      {
		PyErr_SetString(VCS_Error, "vcs_legacy: Not enough memory to store mask.");
		return;
	      }
	    heartbeat("%s", "Starting mask.");
	    slabmask = slabMask(slab);
	    if(PyErr_Occurred()) return;
	    if(slabmask == Py_None) 
	      {
		heartbeat("%s", "No missing values in this array.");
		for (j=0; j<nelems; j++) 
		  {
		    pa->mask[j] = (short) 1;
		  }
		Py_DECREF(slabmask);
	      } 
	    else
	      {
		heartbeat("%s", "There are missing values in this array.");
		shortmask = (PyArrayObject*) PyArray_Cast ((PyArrayObject*) slabmask, PyArray_SHORT);
		Py_DECREF(slabmask);
		if(!shortmask) 
		  {
		    PyErr_SetString(VCS_Error, "Could not convert mask to short.");
		    return;
		  }
		for (j=0; j<nelems; j++) 
		  {
		    pa->mask[j] = ((short) 1) - ((short*)((shortmask)->data))[j];
		  }
		Py_DECREF(shortmask);
	      }
	    heartbeat("%s", "Ready to get data pointer");
	    slabdata = slabData(slab);
	    if(PyErr_Occurred()) return;
	    if (slabtype == '?')
	      { /* convert to Int 8 */
		cp = (short *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    pa->un.data[i] = (short) cp[i];
		  }
	      }  
	    else if (slabtype == 'b')
	      { /* convert to Int 8 */
		cp = (char *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    pa->un.data[i] = (char) cp[i];
		  }
	      }  
	    else if (slabtype == 'h')
	      { /* convert to Int 16 */
		sp = (short *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    pa->un.data[i] = (short) sp[i];
		  }
	      } 
	    else if (slabtype == 'i') 
	      { /* convert to Int 32 */
		ip = (int *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    pa->un.data[i] = (int) ip[i];
		  }
	      } 
	    else if (slabtype == 'l') 
	      { /* convert to long = Int 64 */
		lp = (long *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    if(pa->mask[i]) 
		      {
			pa->un.data[i] = (long) lp[i];
		      }
		    else
		      {
			pa->un.data[i] = (long) 1e20;
		      }
		  }
	      } 
	    else if (slabtype == 'f') 
	      { /*convert to double Float 64*/
		fp = (float *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    if(pa->mask[i]) 
		      {
			pa->un.data[i] = (float) fp[i];
		      }
		    else
		      {
			pa->un.data[i] = (float) 1.0e20;
		      }
		  }
	      } 
	    /* other numpy types */
	    else if (slabtype == 'q') 
	      { 
		llp = (long long *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    if(pa->mask[i]) 
		      {
			pa->un.data[i] = (long long) llp[i];
		      }
		    else
		      {
			pa->un.data[i] = (long long) 1e20;
		      }
		  }
	      } 
	    else if (slabtype == 'B') 
	      { 
		ucp = (unsigned char *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    pa->un.data[i] = (unsigned char) ucp[i];
		  }
	      } 
	    else if (slabtype == 'H') 
	      { 
		usp = (unsigned short *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    pa->un.data[i] = (unsigned short) usp[i];
		  }
	      } 
	    else if (slabtype == 'I') 
	      { 
		uip = (unsigned int *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    pa->un.data[i] = (unsigned int) uip[i];
		  }
	      } 
	    else if (slabtype == 'L') 
	      { 
		ulp = (unsigned long *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    if(pa->mask[i]) 
		      {
			pa->un.data[i] = (unsigned long) ulp[i];
		      }
		    else
		      {
			pa->un.data[i] = (unsigned long) 1e20;
		      }
		  }
	      } 
	    else if (slabtype == 'Q') 
	      { 
		ullp = (unsigned long long *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    if(pa->mask[i]) 
		      {
			pa->un.data[i] = (unsigned long long) ullp[i];
		      }
		    else
		      {
			pa->un.data[i] = (unsigned long long) 1e20;
		      }
		  }
	      } 
	    else if (slabtype == 'g') 
	      { 
		ldp = (long double *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    if(pa->mask[i]) 
		      {
			pa->un.data[i] = ( long double) ldp[i];
		      }
		    else
		      {
			pa->un.data[i] = ( long double) 1.e20;
		      }
		  }
	      } 
	    else 
	      {                  /* convert to double */
		dp = (double *) slabdata->data;
		for (i=0; i<hnelems; ++i) 
		  {
		    if(pa->mask[i]) 
		      {
			pa->un.data[i] = (double) dp[i];
		      } 
		    else
		      {
			pa->un.data[i] = (double) 1.0e20;
		      }
		  }
	      }



	    Py_DECREF(slabdata);
	    heartbeat("%s", "Data delivered.");
	  } 
	else if (gndim < slabrank) 
	  {
	    heartbeat("%s", "Case of gndim < slabrank");
	    /* Get the data type */
	    if (slabtype == 'c' ) 
	      {
		data_type = CuChar;
		strcpy(ty,"C*n");
	      } 
	    else if (slabtype == '?')  /* numpy masks */
	      {
		data_type = '1';
		strcpy(ty,"I*2");
	      } 
	    else if (slabtype == 'b')  /* used to be '1' for Numeric */
	      {
		data_type = 'b';
		strcpy(ty,"I*2");
	      } 
	    else if (slabtype == 'h')  /* used to be 's' for Numeric */
	      {
		data_type = CuShort;
		strcpy(ty,"I*2");
	      } 
	    else if (slabtype == 'i')
	      {
		data_type = CuInt;
		strcpy(ty,"I*4");
	      } 
	    else if (slabtype == 'f')
	      {
		data_type = CuFloat;
		strcpy(ty,"R*4");
	      } 
	    else if (slabtype == 'd') 
	      {
		data_type = CuDouble;
		strcpy(ty,"R*8");
	      }
	    else if (slabtype == 'l')
	      {
		data_type = CuLong;
		strcpy(ty,"I*8");
	      }
	    /* Other numpy types.... */
	    else 
	      {
		data_type = slabtype;
		strcpy(ty,"NUMPY");
	    }

	    
	    /* Set the start and end index for each dimension */
	    for (i=0; i<slabrank; i++) 
	      { /* Set the start and end indices */
/* 		printf("i,actualsize,indexe,indexs:%d,%d,%d,%d\n",i,actual_dimsize[i],index_e[slabrank - i - 1],index_s[slabrank - i - 1] ); */
		if (index_e[slabrank - i - 1] != -1) 
		  {
		    if (index_e[slabrank - i - 1] >= actual_dimsize[i]) 
		      {
			end[i] = actual_dimsize[i] - 1;
		      } 
		    else 
		      {
			end[i] = index_e[slabrank - i - 1];
		      }
		  } 
		else 
		  {
		    end[i] = actual_dimsize[i] - 1;
		  }
		if (index_s[slabrank - i - 1] != -1) 
		  {
		    if (index_s[slabrank - i - 1] > end[i]) 
		      {
			start[i] = end[i];
		      } 
		    else 
		      {
			start[i] = index_s[slabrank - i - 1];
		      }
		  } 
		else
		  {
		    start[i] = 0;
		  }
		heartbeat("start=%d\n",start[i]);
		heartbeat("end=%d\n", end[i]);
/* 		printf("After: end, start: %d,%d\n",end[i],start[i]); */
	      }
	    tndim = 0;
	    for (i=0; i<slabrank; i++) 
	      { /* Get the number of dimensions */
		/*if (index_s != index_e)*/
		if (start[i] != end[i])
		  ++tndim;
		    }
	    heartbeat("tndim = %d\n", tndim);
	    /* Make sure the graphics dimension needed */
	    if ((gndim != 0) && (tndim != gndim)) 
	      {
		for (i=gndim; i<slabrank; i++)
		  { /* are the same as the number */
		    end[i] = start[i];       /* of dimensions */
		  }
	      }
	    for(i=0; i<CU_MAX_VAR_DIMS; i++) 
	      {
		tstart[i] = start[i];
		tend[i] = end[i];
	      }
	    
	    /* Call the hyperslab_data and  and retrieve the selected data */
	    
	    slabmask = slabMask(slab);
	    heartbeat("slabmask at %x, hyperslab case.", slabmask);
	    if(PyErr_Occurred()) return;
	    
	    slabdata = slabData(slab);
	    if(PyErr_Occurred()) 
	      {
		Py_DECREF(slabmask);
		return;
	      }
	    heartbeat("Data slab at %x, hpyerslab case.", slab);
	    heartbeat("Data pointer from slab at %x, hyperslab case.", slabdata->data);
	    heartbeat("%s", "Calling hyperslab.");
	    hyperslab_data(slabrank, actual_dimsize, tstart, tend,
			   data_type, slabmask, slabdata->data, 
			   &hnelems, &pa->mask, &pa->un.data);
	    Py_DECREF(slabdata);
	    Py_DECREF(slabmask);
	    if(PyErr_Occurred()) return;
	    heartbeat("%s", "Hyperslab delivered.");
	  }
	
	/* Set the dimension size, first and last values, etc. */
	
	/* looks like nd is never initialize, i'll do it here */
	nd=0;
	for (i=0;i<4;++i) { pa->xn[i]=NULL;}
	for(i=0; i < slabrank; ++i) 
	  {
	    dimname = slabDimensionName (slab, slabrank-1-i, &isLongLat);
	    dimlen = slabDimensionLength (slab, slabrank-1-i);
	    if(PyErr_Occurred()) return;
	    heartbeat("Slab dimension name = %s", dimname);
	    dimunits = slabDimensionUnits (slab, slabrank-1-i);
	    if(PyErr_Occurred()) return;
	    heartbeat("Slab dimension units = %s", dimunits);
	    dimvalues = slabDimensionValues (slab, slabrank-1-i);
	    if(PyErr_Occurred()) return;
	    dimarray = (float*) dimvalues->data;
	    heartbeat("Slab dimension pointer is %x", dimarray);
	    heartbeat("Slab dimension value at 0 is %f", dimarray[0]);
	    heartbeat("Slab dimension value at last is %f", dimarray[dimlen-1]);
	    pa->xn[i] = repstr(pa->XN[i], dimname);
	    pa->aXN[i] = repstr(pa->aXN[i], dimname); /* question if needed */
	    if (isLongLat == 2) 
	      {
		pa->XN[i] = repstr(pa->XN[i], "longitude");
		pa->aXN[i] = repstr(pa->aXN[i], "longitude"); /* question if needed */
	      } 
	    else if (isLongLat == 1) 
	      {
		pa->XN[i] = repstr(pa->XN[i], "latitude");
		pa->aXN[i] = repstr(pa->aXN[i], "latitude"); /* question if needed */
	      } 
	    else
	      pa->XN[i] = repstr(pa->XN[i], dimname);
	    pa->XU[i] = repstr(pa->XU[i], dimunits);
	    pa->xu[i] = repstr(pa->xu[i], dimunits);
	    
	    /* Free cycle, dimension, bounds, and weights */
	    if (pa->aXC[i]!=NULL)
	      {
		free((char *) pa->aXC[i]);
		pa->aXC[i]=NULL;
	      }
	    if (pa->XV[i]!=NULL) 
	      {
		free((char *) pa->XV[i]);
		pa->XV[i]=NULL;
	      }
	    if (pa->XB[i]!=NULL) 
	      {
		free((char *) pa->XB[i]);
		pa->XB[i]=NULL;
	      }
	    if (pa->XW[i]!=NULL) 
	      {
		free((char *) pa->XW[i]);
		pa->XW[i]=NULL;
	      }
	    
	    /* Get memory for cycle, dimension, bounds, and weights */
	    dimiscircular = slabDimensionIsCircular(slab, slabrank-1-i);
	    heartbeat("dimiscircular = %d", dimiscircular);
	    if(PyErr_Occurred()) return;
	    if (dimiscircular) 
	      {
		ftemp = (float) 360.0;
	      } 
	    else 
	      {
		ftemp = (float) 0.0;
	      }
	    heartbeat("%s", "setting cycle");
	    pa->XC[i] = repflt(pa->XC[i], &ftemp);
	    if (dimiscircular) 
	      {
		if ((pa->aXF[i]=(char *)malloc(20)) == NULL) 
		  {
		    PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!"); 
		    return;
		  }
		sprintf(pa->aXF[i],"%g", dimarray[0]);
		
		if ((pa->aXL[i]=(char *)malloc(20)) == NULL) 
		  {
		    PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!"); 
		    return;
		  }
		sprintf(pa->aXL[i],"%g", dimarray[dimlen-1]);
		
		if ((pa->aXC[i]=(char *)malloc(20)) == NULL) 
		  {
		    PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!"); 
		    return;
		  }
		sprintf(pa->aXC[i],"%f", ftemp);
	      }
	    pa->XF[i] = repflt(pa->XF[i], &(dimarray[0]));
	    pa->XL[i] = repflt(pa->XL[i], &(dimarray[dimlen-1]));
	    itemp = (int)(abs(end[i] - start[i])) + 1;
	    pa->xs[i] = repint(pa->xs[i], &itemp);
	    pa->xf[i] = repflt(pa->xf[i], &(dimarray[0]));
	    pa->xl[i] = repflt(pa->xl[i], &(dimarray[dimlen -1]));
	    
	    if ((pa->XV[i]=(float *)malloc((pa->XS[i][0])*sizeof(float))) == NULL) 
	      {
		PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!"); 
		return;
	      }
	    if ((pa->xv[i]=(float *)malloc(itemp*sizeof(float))) == NULL) 
	      {
		PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!"); 
		return;
	      }
	    heartbeat("%s", "Ready to set dimension data");
	    for (k=0; k<(pa->XS[i][0]); k++)
	      pa->XV[i][k] = dimarray[k];
	    for (j=start[i],k=0; j<=end[i]; j++,k++)
	      pa->xv[i][k] = dimarray[j];
	    
	    Py_DECREF(dimvalues);
	    heartbeat("%s", "Starting bounds.");
	    if ((pa->XB[i]=(float *)malloc(((pa->XS[i][0])+2)*sizeof(float))) == NULL) 
	      {
		PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!"); 
		return;
	      }
	    if ((pa->xb[i]=(float *)malloc((itemp+2)*sizeof(float)))==NULL) 
	      {
		PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!"); 
		return;
	      }
	    bounds = slabDimensionBounds(slab, slabrank-1-i);
	    if(PyErr_Occurred()) return;
	    /*slabDimensionBounds made it type 'f', not None */
	    for (k=0; k< pa->XS[i][0]+1; k++) 
	      {
		pa->XB[i][k] = ((float*)bounds->data)[k];
	      }
	    for (j=start[i],k=0; j<=end[i]+1; j++,k++) 
	      {
		pa->xb[i][k]= ((float*)bounds->data)[j];
	      }
	    Py_DECREF(bounds);
	    
	    heartbeat("%s", "Starting weights.");
	    if ((pa->XW[i]=(float *)malloc((pa->XS[i][0])*sizeof(float))) == NULL) 
	      {
		PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!"); 
		return;
	      }
	    if ((pa->xw[i]=(float *)malloc(itemp*sizeof(float))) == NULL) 
	      {
		PyErr_SetString(VCS_Error, "vcs_legacy: out of memory!"); 
		return;
	      }
	    weightsraw = slabDimensionWeights(slab, slabrank-1-i);
	    if(PyErr_Occurred()) return;
	    if (weightsraw == Py_None) 
	      { /* currently won't happen see cu */
		for (k=0; k<pa->XS[i][0]; k++)
		  pa->XW[i][k]= (float) 1.0;
		for (j=start[i],k=0; j<=(end[i]); j++,k++)
		  pa->xw[i][k]= (float) 1.0;
	      } 
	    else
	      {
		weights = (PyArrayObject*) weightsraw;
		for (k=0; k<pa->XS[i][0]; k++)
		  pa->XW[i][k]= ((float*) weights->data)[k];
		for (j=start[i],k=0; j<=(end[i]); j++,k++)
		  pa->xw[i][k]= ((float*) weights->data)[j];
	      }
	    Py_DECREF(weightsraw);
	    
	    nelems = ((long) nelems) * ((long) *pa->xs[i]);
	    
	    if ((*pa->xs[i]) > 1) nd=i+1;
	    
	  }
	/* Now check if we passed a single number and it's meshfill*/
	if (nd==0) nd=1;
	if ((cmpncs(g_name,"Outline") == 0) || (cmpncs(g_name,"Outfill") == 0)) 
	  {
	    if (cmpncs(pa->ty,"R*4") == 0) 
	      {
		for (i=0,fp=pa->un.data,ip=pa->un.idata; i < hnelems; ip++,fp++,i++)
		  *ip=*fp+0.5;
	      } 
	    else
	      {
		for (i=0,fp=pa->un.data,ip=pa->un.idata; i < hnelems; ip++,fp++,i++)
		  *ip=*fp;
	      }
	    ier=gimmmm(pa->un.idata,pa->mask,&pa->xs[0],&pa->xw[0],nd,
		       &pa->XC[0],&pa->xv[0],&pa->min,&pa->max,&pa->mean);
	    heartbeat("i min %e\n", pa->min);
	    heartbeat("i max %e\n", pa->max);
	    heartbeat("i mean %e\n", pa->mean);
	  } 
	else
	  {
	    ier=gmmmm(pa->un.data,pa->mask,&pa->xs[0],&pa->xw[0],nd,
		      &pa->XC[0],&pa->xv[0],&pa->min,&pa->max,&pa->mean);
	  }
	heartbeat("min %e\n", pa->min);
	heartbeat("max %e\n", pa->max);
	heartbeat("mean %e\n", pa->mean);
	heartbeat("%s", "Finished putting slab.");
}

int graphics_num_of_arrays(g_name)
char * g_name;
{
	if (cmpncs(g_name,"Isoline") == 0)
	   return 1;
	else if (cmpncs(g_name,"Outline") == 0)
	   return 1;
	else if (cmpncs(g_name,"Continents") == 0)
	   return 0;
	else if (cmpncs(g_name,"Isofill") == 0)
	   return 1;
	else if (cmpncs(g_name,"Outfill") == 0)
	   return 1;
	else if (cmpncs(g_name,"Boxfill") == 0)
	   return 1;
	else if (cmpncs(g_name,"Vector") == 0)
	   return 2;
	else if (cmpncs(g_name,"Xyvsy") == 0)
	   return 1;
	else if (cmpncs(g_name,"Yxvsx") == 0)
	   return 1;
	else if (cmpncs(g_name,"XvsY") == 0)
	   return 2;
	else if (cmpncs(g_name,"Scatter") == 0)
	   return 2;
	else if (cmpncs(g_name,"Meshfill") == 0)
	   return 2;
	else
	   return 0;
}

int graphics_num_of_dims(g_name, slabrank, slab_number)
char * g_name;
int slabrank;
int slab_number;
{
	if (cmpncs(g_name,"Isoline") == 0)
	   return 2;
	else if (cmpncs(g_name,"Outline") == 0)
	   return 2;
	else if ((cmpncs(g_name,"Continents") == 0) ||
                 (cmpncs(g_name,"line") == 0) ||
                 (cmpncs(g_name,"marker") == 0) ||
                 (cmpncs(g_name,"fillarea") == 0) ||
                 (cmpncs(g_name,"text") == 0))
	   return 0;
	else if (cmpncs(g_name,"Isofill") == 0)
	   return 2;
	else if (cmpncs(g_name,"Outfill") == 0)
	   return 2;
	else if (cmpncs(g_name,"Boxfill") == 0)
	   return 2;
	else if (cmpncs(g_name,"Meshfill") == 0)
	  { if (slab_number==2) return 3;
	    else return 1;
	  }
	else if (cmpncs(g_name,"Vector") == 0)
	   return 2;
	else if (cmpncs(g_name,"Xyvsy") == 0)
	   return 1;
	else if (cmpncs(g_name,"Yxvsx") == 0)
	   return 1;
	else if (cmpncs(g_name,"XvsY") == 0)
	   return 1;
	else if (cmpncs(g_name,"Scatter") == 0) {
           if (slabrank == 1)
	      return 1;
           else
	      return 2;
        } else
	   return -1;
	return 0;
}

#define MAX_DIMENSIONS  128
void
hyperslab_data(ndim, actual_dimsize, index_s, index_e,
                   data_type, slabmask, orig_data, nelems, mask_data, slab_data)
int     ndim;                    /* Number of dimensions */
int     actual_dimsize[];        /* Original (or actual) data size */
int     index_s[];               /*increment dimension index*/
int     index_e[];               /*store the end dimension index*/
CuType  data_type;               /* Data type as returned from cdunif */
void    *orig_data;              /* Original data block */
PyObject *slabmask;              /* Original slab's mask object */
long    *nelems;                 /* Return the number of data values */
void    **slab_data;             /* Return hyperslab data */
void    **mask_data;             /* Return hyperslab mask data */
{
        int     index_start[CU_MAX_VAR_DIMS];/*store the original index values*/
        int     dimsize[CU_MAX_VAR_DIMS];             /*subset dimension size*/
        double  index_v[CU_MAX_VAR_DIMS];                  /*store index value*/
        int     index;                                  /*computed index value*/
        int     i,j,k,m,data_ct;                                /*counters*/
        char    *c_data;                            /* character data pointer */
	short	*s_data;				/* short data pointer */
	unsigned char	*uc_data;				/* unsigned shortdata pointer */
        int     *i_data;                              /* integer data pointer */
        float   *f_data;                                /* float data pointer */
        long    *l_data;                                 /* long data pointer */
/*         double  *d_data;                               /\* double data pointer *\/ */
/*         void    *v_data;                                 /\* void data pointer *\/ */
        char    *oc_data;
	short	*os_data;
	unsigned char	*ouc_data;
        int     *oi_data;
        float   *of_data;
        long    *ol_data;
        double  *od_data;
        short   *mask;
	long long *ll_data;
	unsigned short *us_data;
	unsigned int *ui_data;
	unsigned long *ul_data;
	unsigned long long *ull_data;
	long double *ld_data;

	long long *oll_data;
	unsigned short *ous_data;
	unsigned int *oui_data;
	unsigned long *oul_data;
	unsigned long long *oull_data;
	long double *old_data;


        PyArrayObject *shortmask;
        short   *slabmaskdata;

        /* Initialize the parameters */
        for (i=0; i<ndim; ++i)
           index_start[i] = index_s[i];

        for (i=0; i<ndim; ++i)
           dimsize[i] = (abs(index_e[i] - index_s[i])) + 1;

        /* Malloc memory for the new hyperslab data array and mask */
        *nelems = 1;
        for (i=0; i<ndim; ++i) {
           *nelems = *nelems*dimsize[i];
        }

      	if ((mask=(short *)malloc((*nelems)*sizeof(short))) == NULL) {
           PyErr_SetString(VCS_Error, "vcs_legacy: Not enough memory to store mask.");
           return;
        }
        for (i=0; i < *nelems; ++i) {
            mask[i] = ((short) 1);
        }

        if (data_type == CuChar) {
          oc_data=orig_data;
          if((c_data=(char *)malloc(*nelems*cutypelen(data_type)))==(void*)0) {
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
        } else if (data_type == 'b') {
	  ouc_data=orig_data;
          if((uc_data=(unsigned char *)malloc(*nelems*sizeof(unsigned char)))==(void*)0) {
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
        } else if (data_type == CuShort) {
	  os_data=orig_data;
          if((s_data=(short*)malloc(*nelems*cutypelen(data_type)))==(void*)0) {
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
        } else if (data_type == CuInt) {
          oi_data=orig_data;
          if((i_data=(int *)malloc(*nelems*cutypelen(data_type)))==(void*)0) {
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
        } else if (data_type == CuFloat) {
          of_data=orig_data;
          if((f_data=(float *)malloc(*nelems*cutypelen(data_type)))==(void*)0) {
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
        } else if (data_type == CuLong) {
          ol_data=orig_data;
          if((l_data=(long *)malloc(*nelems*cutypelen(data_type)))==(void*)0) {
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
        } else if (data_type == CuDouble) {
          od_data=orig_data;
          if((f_data=(float *)malloc(*nelems*cutypelen(data_type)))==(void*)0){
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
	/* Other numpy types */
        } else if (data_type == 'q') {
	  oll_data=orig_data;
          if((ll_data=(long long *)malloc(*nelems*sizeof(long long)))==(void*)0) {
	    PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
	    return;
          }
        } else if (data_type == 'B') {
	  ouc_data=orig_data;
          if((uc_data=(unsigned char *)malloc(*nelems*sizeof(unsigned char)))==(void*)0) {
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
        } else if (data_type == 'H') {
	  ous_data=orig_data;
          if((us_data=(unsigned short *)malloc(*nelems*sizeof(unsigned short)))==(void*)0) {
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
        } else if (data_type == 'I') {
	  oui_data=orig_data;
          if((ui_data=(unsigned int *)malloc(*nelems*sizeof(unsigned int)))==(void*)0) {
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
        } else if (data_type == 'L') {
	  oul_data=orig_data;
          if((ul_data=(unsigned long *)malloc(*nelems*sizeof(unsigned long)))==(void*)0) {
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
        } else if (data_type == 'Q') {
	  oull_data=orig_data;
          if((ull_data=(unsigned long long *)malloc(*nelems*sizeof(unsigned long long)))==(void*)0) {
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
        } else if (data_type == 'g') {
	  old_data=orig_data;
          if((ld_data=( long double *)malloc(*nelems*sizeof( long double)))==(void*)0) {
             PyErr_SetString(VCS_Error, "Unable to allocate data for hyperslab.");
                  return;
          }
	}

        /* Get the beginning start values for calculating the index */
        index_v[0] = 0;
        for (i=1; i<ndim; ++i) {
           index_v[i] = index_s[i];
           for (j=1; j<=i; j++)
              index_v[i] = index_v[i] * actual_dimsize[j-1];
        }


        /*
         * Caluculate the index and copy the data into the new data buffer.
         * Example: index = i + j*lon  +  k*lon*lat  +  l*lon*lat*lev  +  ...
         *          index = i + index_v[0] + index_v[1] + index_v[2] + ...
         *
         *      where   index_v[0] = j*lon
         *              index_v[1] = k*lon*lat
         *              index_v[2] = l*lon*lat*lev
         *
         */
         shortmask = NULL;
        if (slabmask != Py_None) {
heartbeat("slabmask not None at %x", slabmask);
            shortmask = (PyArrayObject*) PyArray_Cast ((PyArrayObject*) slabmask, PyArray_SHORT);
heartbeat("shortmask at %x", shortmask);
            if(!shortmask) {
                  return;
            }
            slabmaskdata = (short*) shortmask->data;
heartbeat("slabmaskdata set at %x", slabmaskdata);
        }
heartbeat("Setting data in hyperslab at %x.", orig_data);
        data_ct = 0;

        while (data_ct < *nelems) {
           index = 0;
           for (k=0; k<ndim; k++) {
               index = index + index_v[k];
           }

           if (slabmask == Py_None) {
              for (k=index_s[0]; k<=index_e[0]; k++) {
                  mask[data_ct + k - index_s[0]] = ((short) 1);
              }
           } else {
              for (k=index_s[0]; k<=index_e[0]; k++) {
                  mask[data_ct + k - index_s[0]] = ((short) 1) - ((short*) slabmaskdata)[(k+index)];
              }
           }
           for (k=index_s[0]; k<=index_e[0]; k++, data_ct++) {
               if (data_type == CuChar) {
		 c_data[data_ct] = (char)oc_data[(k + index)];
               } else if (data_type == 'b') {
		 uc_data[data_ct] = ouc_data[(k + index)];
               } else if (data_type == CuShort) {
		 s_data[data_ct] = os_data[(k + index)];
               } else if (data_type == CuInt) {
                  i_data[data_ct] = oi_data[(k + index)];
               } else if (data_type == CuLong) {
                  if(mask[data_ct]) {
                     l_data[data_ct] = ol_data[(k + index)];
                  } else {
                     l_data[data_ct] = (long) 1e20;
                  }
               } else if (data_type == CuFloat) {
                  if(mask[data_ct]) {
                     f_data[data_ct] = of_data[(k + index)];
                  } else {
                     f_data[data_ct] = (float) 1.0e20;
                  }
               } else if (data_type == CuDouble) {
                  if(mask[data_ct]) {
                     f_data[data_ct] = (float) od_data[(k + index)];
                  } else {
                     f_data[data_ct] = (float) 1.0e20;
                  }
		  /* Other numpy types */
               } else if (data_type == 'q') {
                  if(mask[data_ct]) {
                     ll_data[data_ct] = oll_data[(k + index)];
                  } else {
                     ll_data[data_ct] = (long long) 1e20;
                  }
               } else if (data_type == 'B') {
		 uc_data[data_ct] = ouc_data[(k + index)];
               } else if (data_type == 'H') {
		 us_data[data_ct] = ous_data[(k + index)];
               } else if (data_type == 'I') {
		 ui_data[data_ct] = oui_data[(k + index)];
               } else if (data_type == 'L') {
                  if(mask[data_ct]) {
                     ul_data[data_ct] = oul_data[(k + index)];
                  } else {
                     ul_data[data_ct] = (unsigned long) 1e20;
                  }
               } else if (data_type == 'Q') {
                  if(mask[data_ct]) {
                     ull_data[data_ct] = oull_data[(k + index)];
                  } else {
                     ull_data[data_ct] = (unsigned long long) 1e20;
                  }
               } else if (data_type == 'g') {
                  if(mask[data_ct]) {
                     ld_data[data_ct] = old_data[(k + index)];
                  } else {
                     ld_data[data_ct] = ( long double) 1e20;
                  }
	       }
	   }
           /*
            * Determine the new store index value that will be used to
            * calculate the index
            */
           for (m=1; m<ndim; m++) {
              if (++index_s[m] <= index_e[m]) {
                 index_v[m] = index_s[m];
                 for (j=1; j<=m; j++)
                    index_v[m] = index_v[m] * actual_dimsize[j-1];
                 break;
              } else {
                 index_s[m] = index_start[m];
                 index_v[m] = index_s[m];
                 for (j=1; j<=m; j++)
                    index_v[m] = index_v[m] * actual_dimsize[j-1];
              }
           }
        }

        /* Return back the subset data and mask*/
        *mask_data = mask;

        if (data_type == CuChar)
           *slab_data = c_data;
        else if (data_type == 'b') { 
           if((f_data=(float *)malloc(*nelems*cutypelen(CuFloat)))==(void*)0) {
               PyErr_SetString(VCS_Error, "Error - Cannot Malloc array for data!");
               return;
           }
           for (i=0; i<*nelems; i++)
	     {
	       f_data[i] = (float) uc_data[i];
	     }
	   free((unsigned char*) uc_data);
           *slab_data = f_data;
        } else if (data_type == CuShort) { 
           if((f_data=(float *)malloc(*nelems*cutypelen(CuFloat)))==(void*)0) {
               PyErr_SetString(VCS_Error, "Error - Cannot Malloc array for data!");
               return;
           }
           for (i=0; i<*nelems; i++)
	     {
	       f_data[i] = (float) s_data[i];
	     }
	   free((short *) s_data);
           *slab_data = f_data;
        } else if (data_type == CuInt) {
           if((f_data=(float *)malloc(*nelems*cutypelen(CuFloat)))==(void*)0) {
               PyErr_SetString (VCS_Error, "Error - Cannot Malloc array for data!");
               return;
           }
           for (i=0; i<*nelems; i++)
                f_data[i] = (float) i_data[i];
           free((char *) i_data);
           *slab_data = f_data;
        } else if (data_type == CuFloat)
           *slab_data = f_data;
        else if (data_type == CuDouble)
           *slab_data = f_data;
        else if (data_type == CuLong) {
           if((f_data=(float *)malloc(*nelems*cutypelen(CuFloat)))==(void*)0) {
               PyErr_SetString(VCS_Error, "Error - Cannot Malloc array for data!");
               return;
           }
           for (i=0; i<*nelems; i++)
                f_data[i] = (float) l_data[i];
           free((char *) l_data);
           *slab_data = f_data;
	}
		  /* Other numpy types */
	else if (data_type == 'q') {
           if((f_data=(float *)malloc(*nelems*cutypelen(CuFloat)))==(void*)0) {
               PyErr_SetString(VCS_Error, "Error - Cannot Malloc array for data!");
               return;
           }
           for (i=0; i<*nelems; i++)
                f_data[i] = (float) ll_data[i];
           free((long long *) ll_data);
           *slab_data = f_data;
	}
	else if (data_type == 'B') {
           if((f_data=(float *)malloc(*nelems*cutypelen(CuFloat)))==(void*)0) {
               PyErr_SetString(VCS_Error, "Error - Cannot Malloc array for data!");
               return;
           }
           for (i=0; i<*nelems; i++)
                f_data[i] = (float) uc_data[i];
           free((unsigned char *) uc_data);
           *slab_data = f_data;
	}
	else if (data_type == 'H') {
           if((f_data=(float *)malloc(*nelems*cutypelen(CuFloat)))==(void*)0) {
               PyErr_SetString(VCS_Error, "Error - Cannot Malloc array for data!");
               return;
           }
           for (i=0; i<*nelems; i++)
                f_data[i] = (float) us_data[i];
           free((unsigned short *) us_data);
           *slab_data = f_data;
	}
	else if (data_type == 'I') {
           if((f_data=(float *)malloc(*nelems*cutypelen(CuFloat)))==(void*)0) {
               PyErr_SetString(VCS_Error, "Error - Cannot Malloc array for data!");
               return;
           }
           for (i=0; i<*nelems; i++)
                f_data[i] = (float) ui_data[i];
           free((unsigned int *) ui_data);
           *slab_data = f_data;
	}
	else if (data_type == 'L') {
           if((f_data=(float *)malloc(*nelems*cutypelen(CuFloat)))==(void*)0) {
               PyErr_SetString(VCS_Error, "Error - Cannot Malloc array for data!");
               return;
           }
           for (i=0; i<*nelems; i++)
                f_data[i] = (float) ul_data[i];
           free((unsigned long *) ul_data);
           *slab_data = f_data;
	}
	else if (data_type == 'Q') {
           if((f_data=(float *)malloc(*nelems*cutypelen(CuFloat)))==(void*)0) {
               PyErr_SetString(VCS_Error, "Error - Cannot Malloc array for data!");
               return;
           }
           for (i=0; i<*nelems; i++)
                f_data[i] = (float) ull_data[i];
           free((unsigned long long *) ull_data);
           *slab_data = f_data;
	}
	else if (data_type == 'g') {
           if((f_data=(float *)malloc(*nelems*cutypelen(CuFloat)))==(void*)0) {
               PyErr_SetString(VCS_Error, "Error - Cannot Malloc array for data!");
               return;
           }
           for (i=0; i<*nelems; i++)
                f_data[i] = (float) ld_data[i];
           free(( long double *) ld_data);
           *slab_data = f_data;
	}
        Py_XDECREF(shortmask);

        return; 
}

/* Set the minimum and maximum values for the graphics methods */
int set_plot_minmax(self, type, graphics, plot_ct)
PyVCScanvas_Object *self;
char	*type;
char	*graphics;
int	plot_ct;
{
	graphics_method_list	*gptr, *tgptr;
	int			ierr;
	char			gname[24];
	struct gfb_tab          *gfbtab;
	struct gXy_tab          *gXytab;
        struct gYx_tab          *gYxtab;
	struct gfb_attr 	*pgfb=NULL;
	struct gXy_attr         *pgXy=NULL;
        struct gYx_attr         *pgYx=NULL;
	extern struct gfb_tab   Gfb_tab;
	extern struct gXy_tab   GXy_tab;
        extern struct gYx_tab   GYx_tab;
	extern int      	copy_Gfb_name();
	extern int      	copy_GYx_name();
	extern int      	copy_GXy_name();

	/* Check to see if the minimum and maximum values need to be set. */
	if ((self->vcs_legacy_min == 1e20) && (self->vcs_legacy_max == -1e20)) {
	  return (0);
	}

	/* Create a unique graphics method name (i.e., gname is 
         * pmetidmcpnead is deapcmditemp in reverse)
         */
	sprintf(gname, "pmetidmcpnead%d", plot_ct);

        /* Create a display name structure for linked list *
        if ((gptr=(graphics_method_list *)malloc(
                  sizeof(graphics_method_list)) == NULL)) {
             pyoutput("Error - No memory for new graphics type.", 1);
             return (0);
        } */
	gptr=(graphics_method_list *)malloc(sizeof(graphics_method_list));
        if ((gptr->g_type = (char *) malloc((
             strlen(type)+1)*sizeof(char)+1)) == NULL) {
	     pyoutput("Error - No memory for the graphics type.", 1);
	     return (0);
	} else
           strcpy(gptr->g_type, type);
        if ((gptr->g_name = (char *) malloc((
             strlen(gname)+1)*sizeof(char)+1)) == NULL) {
	     pyoutput("Error - No memory for the graphics method name.", 1);
	     return (0);
	} else
           strcpy(gptr->g_name, gname);
	gptr->next = NULL;
    
	/* Put struct in graphics method link list */
	if (self->glist == NULL)
	   self->glist = gptr;
	else {
	   tgptr = self->glist;
	   while (tgptr->next != NULL)
	       tgptr = tgptr->next;
	   tgptr->next = gptr;
	}

	/* Set the minimum, maximum, and extensions for the boxfill graphics
         * method. */
	if (strcmp(type, "Boxfill") == 0) {
	   /* Copy the graphics method to new space */
	   ierr = copy_Gfb_name(graphics, gname);
	   if (!ierr) {
	      pyoutput("Error - Cannot create new boxfill graphics method for\n        minimum and maximum values.", 1); 
	      return (0);
	   }

	   /* Find the newly created graphics method */
	   gfbtab=&Gfb_tab;
           while ((gfbtab != NULL) &&
                  (strcmp(gfbtab->name, gname) != 0))
                 gfbtab = gfbtab->next;
	   pgfb=gfbtab->pGfb_attr;

	  /* Set the minimum, maximum and underflow overflow values */
	  pgfb->lev1  = self->vcs_legacy_min;
	  pgfb->lev2  = self->vcs_legacy_max;
	  if (self->vcs_legacy_ext1 == 1)
	     pgfb->ext_1 = 121;
	  if (self->vcs_legacy_ext2 == 1)
	     pgfb->ext_2 = 121;
	} else if (strcmp(type, "Xyvsy") == 0) {
           /* Copy the graphics method to new space */
           ierr = copy_GXy_name(graphics, gname);
           if (!ierr) {
              pyoutput("Error - Cannot create new Xyvsy graphics method for\n         minimum and maximum values.", 1);
              return (0);
           }

           /* Find the newly created graphics method */
           gXytab=&GXy_tab;
           while ((gXytab != NULL) &&
                  (strcmp(gXytab->name, gname) != 0))
                 gXytab = gXytab->next;
           pgXy=gXytab->pGXy_attr;

          /* Set the minimum and maximum */
          pgXy->dsp[0]=self->vcs_legacy_min;
          pgXy->dsp[2]=self->vcs_legacy_max;
        } else if (strcmp(type, "Yxvsx") == 0) {
	   /* Copy the graphics method to new space */
	   ierr = copy_GYx_name(graphics, gname);
	   if (!ierr) {
	      pyoutput("Error - Cannot create new Yxvsx graphics method for\n        minimum and maximum values.", 1); 
	      return (0);
	   }

	   /* Find the newly created graphics method */
	   gYxtab=&GYx_tab;
           while ((gYxtab != NULL) &&
                  (strcmp(gYxtab->name, gname) != 0))
                 gYxtab = gYxtab->next;
	   pgYx=gYxtab->pGYx_attr;

	  /* Set the minimum and maximum */
	  pgYx->dsp[1]=self->vcs_legacy_min;
	  pgYx->dsp[3]=self->vcs_legacy_max;
	}

        strcpy(graphics, gname); 

	return (1);
}

#ifdef USEX11
int my_X_error_handler( display, myerr )
Display         *display;
XErrorEvent     *myerr;
{
   /* Only print when debugging....
   char msg[80];
   XGetErrorText(display, myerr->error_code, msg, 80);
   fprintf(stderr, "Error code %s\n", msg);*/
   
   return 1;
}
#endif

int initialize_X(void)
{
/*   Arg     	xargs[15];       /\* name-value pairs *\/ */
  int 		ier;
/*   int 		ier, n, rargc=0; */
/*   char 		*argv[3]; */
/*   char   		*display_name = NULL; */
  int             VIS_DEPTH; /* returned from routine visual_find */
  extern char 	active_colors[]; /*colormap name*/
  extern char 	cmp_filename[MAX_LINE_STRING]; /*set color name*/
  extern void 	normal_cmap_emulate_default();
/*   long l; */
/*   XSizeHints hints; */
  
  
  not_using_gui = 0; /* called from cdatgui */
  /* XGKS initialization - Initialize the VCS module */
  /*        if (connect_id.display == NULL) {*/
  not_using_gui = 1; /* not called from cdatgui */
  /* XGKS initialization - Open the X display */
  
  /*XtToolkitInitialize();*/
  /****************************************************************
   * This is important!                                           *
   * Initializes Xlib's support for concurrent threads.           *
   * Without this function, Xlib will give the following error:   *
   *         "Xlib: unexpected async reply"                       *
   *                                                              *
   *                                                              *
   * If Tkinter is to be used, then somewhere before this call    *
   * to "XInitThreads()" you must call "Tkinter.Tk()". I called   *
   * "Tkinter.Tk()" in the "Canvas.py" file located in the        *
   * "def __init__" routine.                                      *
   *                                                              *
   ****************************************************************/
  /* 	initialize Python Thread */
  PY_INIT_THREADS
    
#ifdef USEX11
    ier = XInitThreads();
#else
  ier = 1;
#endif
  if (ier == 0)
    PySys_WriteStdout("Warning - Your X11 system [does not] support threads. You will\neventually have an 'Xlib: unexpected async reply' .\n");

#ifdef X11WM
  display = XOpenDisplay(NULL);
  if (display == NULL)
    display = XOpenDisplay(":0.0");
  display->display_name = NULL;
#endif
  /*
    printf("*** Revision = %d\n", ProtocolRevision(display));
    printf("*** Version = %d\n", ProtocolVersion(display));
    printf("*** Server Vendor = %s\n", ServerVendor(display));
    printf("*** Vendor Release = %d\n", VendorRelease(display));
    printf("*** Connection Number = %d\n", ConnectionNumber(display));
  */
  ier = vcs_legacy_main(0,NULL);/*Initialize the VCS module*/
#ifdef USEX11
  VIS_DEPTH = visual_find();
  screen = DefaultScreen(display); /* Set to the default screen */
  normal_cmap_create();
  normal_cmap_emulate_default2();
#elif defined (QTWM)
  /* not needed in Qt */
#else
  fprintf(stderr,"insert your WM colormap setup if needed here\n");
#endif
  /*DNW 08/8/01           if (app_context == NULL) {
    XtToolkitInitialize();
    app_context = XtCreateApplicationContext();
    }
    
    display = (Display *)malloc(1000);
    
    if ( (display = XtOpenDisplay(app_context,NULL,NULL,"CDAT/VCS", NULL, 0, &rargc,NULL)) == NULL ) {
    display = (Display *)malloc(1000);
    screen = 0; * Set to the default screen *
    * Will need to setup a dummy display *
    *return 0;*
    } else
    screen = DefaultScreen(display); * Set to the default screen *
    
    ier = vcs_legacy_main(0,NULL);*Initialize the VCS module*
    if (ier == 1) {
    PyErr_SetString(PyExc_TypeError, "Error initializing VCS! The VCS Canvas object was not created.\n");
    return 0;
    }
    
    
    * Setup graphics environment *
    VIS_DEPTH = visual_find();
    normal_cmap_create();
    normal_cmap_emulate_default2();
    
    * create application shell in custom visual w/ custom colormap *
    n = 0;
    XtSetArg (xargs[n], XmNdeleteResponse, XmDO_NOTHING); n++;
    XtSetArg(xargs[n], XmNvisual, visual); n++;
    XtSetArg(xargs[n], XmNdepth, VIS_DEPTH); n++;
    XtSetArg(xargs[n], XmNcolormap, n_cmap); n++;
    *XtSetArg (xargs[n], XmNbuttonFontList, font_list_menu); n++;
    XtSetArg (xargs[n], XmNlabelFontList, font_list_menu); n++;
    XtSetArg (xargs[n], XmNtextFontList, font_list_menu); n++;*
    XtSetArg(xargs[n], XmNtitle,
    "Climate Data Analysis Tool (CDAT)"); n++;
    XtSetArg(xargs[n], XmNforeground, 1); n++;
    XtSetArg(xargs[n], XmNbackground, 0); n++;
    app_shell = XtAppCreateShell(
    "CDAT", "XCmap", applicationShellWidgetClass, display, xargs, n);
    DNW 08/8/01*/
  
  /* The below information is necessary for the VCS Canvas.
   * This information is used in the VCS modules, procCanvas.c
   * and python_misc.c.
   */
#ifdef USE11
  connect_id.display = display; /* store display for VCS Canvas */
  connect_id.drawable = (XID)NULL; /* VCS Canvas */
  connect_id.app_context = 0;/* store application context */
  connect_id.app_shell = 0; /* store first widget */
  connect_id.n_cmap = n_cmap; /* store the color map */
  connect_id.visual = visual; /* store the visual */
  connect_id.canvas_pixmap = (Pixmap)NULL;/*used as the backing store*/
  /* Create the graphics content */
/*   gc_create( connect_id ); */
#endif

  
  /* Turn off the warning messages. */
  /*DNW 08/8/01           XtAppSetWarningHandler(app_context, (XtErrorHandler) catch_warnings);
    
  DNW 08/8/01*/
  
  /* Load the colormap */
  strcpy(cmp_filename,active_colors); /* Set active colormap name */
#ifdef USEX11
  if (visual->class == PseudoColor)  /* Only do this for 8-bit PseudoColor */
    load_color_table_proc(FALSE);
  
  /*        }*/
  
  XSetErrorHandler( my_X_error_handler ); /* Redirect the X11 Errors */
#endif
  return 1;
}

/*
 * Convert screen x or y values between 0 and 1 or 0 and canvas ratio.
 */
float
cnorm(PyVCScanvas_Object *self, int x_or_y, float value)
{
#ifdef X11WM
        XWindowAttributes 		xwa;
#elif defined (QTWM)
	int qx,qy;
#endif
	extern struct orientation       Page;
        float                           canvas_ratio_l=0., canvas_ratio_p=0.;
	int w,h;
	
/*                      Get the width and height of the VCS Canvas.     */
#ifdef X11WM
        XGetWindowAttributes(self->connect_id.display, self->connect_id.drawable, &xwa);
	w = xwa.width;
	h=xwa.height;
#elif defined (QTWM)
	vcs_legacy_Qt_get_window_dimensions_by_id(self->connect_id.wkst_id,&qx,&qy,&w,&h);
#else
	fprintf(stderr,"insert your WM getgeom func here\n");
#endif
        canvas_ratio_l = (float) h / (float) w;
        canvas_ratio_p = (float) w / (float) h;

	if (strcmp(Page.page_orient,"landscape") == 0) {
	   if (x_or_y == 0) {
	      return (value/w);
	   } else {
	     return ((1.-(value/(float)h))*canvas_ratio_l);
           }
	} else { /* must be portriat */
	   if (x_or_y == 1) {
	     return ((1. - (value/(float)h)));
	   } else {
	     return ((value/(float)w)*canvas_ratio_p);
           }
	}
}

/*
 * Normalize the normalized screen x or y values between 0 and 1.
 */
float
nnorm(PyVCScanvas_Object *self, int x_or_y, float value)
{
#ifdef X11WM        
       XWindowAttributes               xwa;
#endif
        extern struct orientation       Page;
        int				orientation_flg = 0;
        float                           canvas_ratio_l=0., canvas_ratio_p=0.;
        extern float 			gnorm(int x_or_y, float value, int normalized_flg, int orientation); /* Needed to normalize x,y coordinates back to values between 0 and 1 */

/*                      Get the width and height of the VCS Canvas.     */
/*	Note: the values 0.758800507 and 0.760843 are also found in the gnorm function in python_misc.c */
#ifdef X11WM
        XGetWindowAttributes(self->connect_id.display, self->connect_id.drawable, &xwa);
        canvas_ratio_l = ((float) xwa.height / (float) xwa.width) / 0.758800507;
        canvas_ratio_p = ((float) xwa.width / (float) xwa.height) / 0.760843;
#endif

        if (strcmp(Page.page_orient,"landscape") != 0) orientation_flg = 1;
        value = gnorm( x_or_y, value, 0, orientation_flg );

        if (strcmp(Page.page_orient,"landscape") == 0) {
           if (x_or_y == 0) {
              return value;
           } else {
              return (value/canvas_ratio_l);
           }
        } else { /* must be portriat */
           if (x_or_y == 1) {
              return value;
           } else {
              return (value/canvas_ratio_p);
           }
        }
}



/***************************************************************************
        END OF FILE
****************************************************************************/




