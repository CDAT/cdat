/************************************************************************
 *                      PCMDI GRAPHICS PACKAGE                          *
 *                      Developed for LLNL-PCMDI use                    *
 *                                                                      *
 *                                                                      *
 *      Copyright (C) 1992. The Regents of the University of California.*
 *      All rights reserved.                                            *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *      Author: Dean N. Williams                                        *
 *                                                                      *
 *      Date: 03/3/93                                                   *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *      File name: animation.c                                          *
 *                                                                      *
 *                                                                      *
 *      Language: C                                                     *
 *                                                                      *
 *                                                                      *
 *      Comments:                                                       *
 *                                                                      *
 *                                                                      *
 *      Modifications:                                                  *
 *                                                                      *
 *                                                                      *
 *      Contact:                                                        *
 *                                                                      *
 *              Dean N. Williams                                        *
 *                                                                      *
 *              LLNL                                                    *
 *              PO Box 808, L-264                                       *
 *              Livermore, CA                                           *
 *              94550                                                   *
 *                                                                      *
 *              (510) 423-0145                                          *
 *                                                                      *
 *                                                                      *
 ************************************************************************
 */
/****************************************************************************
*** Includes ****************************************************************
*****************************************************************************/
#include "Python.h"
#include "workstations.h"
#include "vcs_legacy_canvas.h"
#include "animation.h"
#include "color_editor.h"
#include "array.h"
#include "color.h"        /* script colormap structures */
#include "display.h"
#include "list.h"
#include "graph.h"
#include "picture.h"
#include <pwd.h>
#include <time.h>
#ifdef X11WM
#include <X11/Xlib.h>
#endif
#include "gks.h"         /* include gks routines */
#include "xgks.h"         /* include gks routines */
#include "gksshort.h"     /* gks alias Fortran names */


typedef struct level_fill {int color; float l1,l2;} S_boxfill;

/*#define Magnify_Table_Length	20*/

/****************************************************************************
*** Prototypes **************************************************************
*****************************************************************************/

/*
struct animation_file_list {
        char                    	*filename;* Name of animation file *
	int				position;
        struct animation_file_list	*next;
};
typedef struct animation_file_list   	ANIMATIONFILELIST;
typedef ANIMATIONFILELIST            	*ANIMATIONFILELIST_LINK;
*/

/* Keep track of the canvas and animations */
struct canvas_animation_list {
	CANVASINFO_LINK          	cptr_ptr;
	ANIMATIONWINDOWLIST_LINK        aptr_ptr;
	ANIMATIONMEMORYLIST_LINK	iptr_ptr;
        struct canvas_animation_list	*next;
};
typedef struct canvas_animation_list   	CANVASANIMATIONLIST;
typedef CANVASANIMATIONLIST            	*CANVASANIMATIONLIST_LINK;

/*extern MAG_INFO magnify_table[Magnify_Table_Length];*/

extern int 		screen;  /* screen index we're running on */
#ifdef X11WM
extern GC			gc;      /* graphics context */
extern Colormap  	n_cmap;  /* virtual normal colormap */
#endif
extern Gconid_X_drawable connect_id; /* VCS canvas display and drawable id */


/****************************************************************************
*** Global Functions ********************************************************
*****************************************************************************/
void				setup_the_canvas_magnify_table();

/****************************************************************************
*** Global Variables ********************************************************
*****************************************************************************/

ANIMATIONFILELIST_LINK		head_animation_list=NULL;
ANIMATIONWINDOWLIST_LINK   	head_animation_window_list=NULL;
ANIMATIONMEMORYLIST_LINK	current_iptr=NULL;
CANVASANIMATIONLIST_LINK	head_canvasanimation_list=NULL;
extern CANVASINFO_LINK         	head_canvas_info;    /* connection ID info */
struct animation_list_list 	store_animation_list_list[num_index];
#ifdef X11WM
Cursor          		create_cursor;
XColor set_xcolors[NUM_COLORS];
extern Visual     	*visual;              /* the display's visual */
#endif
int             		CompletionType;
int				animation_flg, animation_file_ct, sel_ct=1;
int	 			animation_colormap;
int				animation_mode;
int				animation_direction;
int				animation_zoom=1;
int				animation_hori=0, animation_vert=0;
extern int			a_hori, a_vert;
int				animation_speed=0;
int				file_frame_ct;
float                           animation_min_max[3];
char                            hold_animate_display_name[20];
int				sync_animation_flg, sync_stop_flg, sync_ct=0;
char 				*store_afirst[NDS];
char 				*store_alast[NDS];
char                    	cmp_filename[MAX_LINE_STRING];
extern int                      memory_ct;
#ifndef Boolean
#include <stdbool.h>
#define Boolean bool
#define TRUE 1
#define FALSE 0
#define False 0
#define True 1
#endif
Boolean				anot_stop, quit_animation, zoom_animation=False, stop_from_tog;
Boolean				use_shared_mem;
Boolean				stored_afirst_alast=FALSE;
Boolean                		modified_first_last;
Boolean				vcs_legacy_be_quiet;
char 				ani_file[1024]="";
int				selected_dim_panel;
char           			selected_data[MAX_PATH_LEN];
extern ANIMATIONMEMORYLIST_LINK	head_animation_memory, tail_animation_memory;
struct a_tab             	*global_atab, *global_atab2, *global_atab3;
struct a_tab     		*global_atab;
extern FILE     		*fperr;
static  char            	slash[] = "/";
static  char            	ras_extension[] = ".ras"; /* raster file extension */

extern struct workstations Wkst[];

unsigned long           bg,fg,white,black,red,green,blue,cyan,magenta,yellow,orange,olive_green;

/****************************************************************************
*** animation - program entry point *****************************************
*****************************************************************************/

int animate_module(int canvas_id)
{
        ANIMATIONWINDOWLIST_LINK	tptr, aptr;
#ifdef X11WM
	Atom            		quit_ev_protocol;
#endif
	int             		i,screen_num;
	char *         			rfile_home_dir;
	void            		animate_quit_cb(int w);
        extern void            		load_color_table_proc();

        /* Check if the canvas animation has already been initialized */
        if ( head_animation_window_list != NULL) {
           aptr = head_animation_window_list;
           while (aptr != NULL) {
                if (aptr->canvas_id == canvas_id) return (1); /* already initialized */
                aptr = aptr->next;
           }
        }

	/* Check for VCS canvas. If the VCS canvas does not exit, then
	 * return.
	 */
#ifdef X11WM
	if (connect_id.display == NULL) {
#else
	  if (connect_id.cr == NULL) {
#endif
           err_warn(1,fperr,"Error - Must have a VCS Canvas.\n");
           return 0;
	  }

#ifdef X11WM
	/* Make sure there is no other window in front of the VCS Canvas */
	XMapRaised(connect_id.display, connect_id.drawable);
#elif defined QTWM
	vcs_legacy_Qt_open_window_by_id(connect_id.wkst_id);
#else
	fprintf(stderr,"insert here your WM map raise\n");
#endif

        /* this sets the color of read/write cell */
        load_color_table_proc(FALSE);       /*Load color map segment*/

        /*----------------------------------------------------------------
         * Create the ui animation window objects of the interface.
         *---------------------------------------------------------------*/
	/* Create the link list */
	if((tptr=(ANIMATIONWINDOWLIST_LINK)malloc(sizeof(ANIMATIONWINDOWLIST))) == NULL) {
             err_warn(1,fperr,"Error - Can not create 'Animation Window'!\n");
             return 0;
        }
        /* Initialize Animation */
	tptr->sel_ct = 1;    /* Initialize directory file counter */
        tptr->zoom_animation = FALSE; /* Initialize zoom to false */
	tptr->memory_ct = 0;    /* Initialize number of frames to 0 */
	tptr->animation_flg = 1;    /* Run from memory or disk flag */
	tptr->create_animation_flg = 0;    /* creating animation flag */
	tptr->anot_stop = 0;  /* Stop animation flag */
        tptr->stop_from_tog = 0;  /* Stop animation from toggle button */
	tptr->animation_colormap = 1; /* Set Toggle flag for the colormap */
	tptr->animation_mode = 1; /* Set Cycle toggle as the default mode */
	tptr->animation_direction = 1;/*Set Forward toggle default direction*/
	tptr->animation_hori = 0; /* Initial horizontal panel value */
	tptr->animation_vert = 0; /* Initial vertical panel value */
	tptr->animation_speed = 0; /* Initialize animation speed */
	tptr->animation_zoom = 1; /* Initialize animation zoom */
	tptr->file_frame_ct = 0; /* Initialize the file frame counter */
        tptr->a_hori = 0; /* Initialize the horizontal value to zero */
        tptr->a_vert = 0; /* Initialize the vertical value to zero */
	tptr->garbaged = 0; /* Initialized garbage collection flag to false */
        tptr->canvas_id = canvas_id; /* VCS Canvas ID */
	tptr->next = NULL;

        current_iptr = NULL; /* set the current image pointer to NULL */

	/* Store animation window in link list */
	if (head_animation_window_list == NULL)
           head_animation_window_list = tptr;
        else {
           aptr = head_animation_window_list;
	   while (aptr->next != NULL)
                aptr = aptr->next;
           aptr->next = tptr;
        }

        /* Get the graphics contents. */
#ifdef X11WM
        screen_num = DefaultScreen(connect_id.display);
        gc = DefaultGC(connect_id.display,screen_num);
#endif

        use_shared_mem = FALSE;/*X11 Shared memory images is not available*/

	/* Initialize the stored list table used in looping */
	for (i = 0; i < num_index; ++i)
	   store_animation_list_list[i].xs = -1;

        /* Set the Raster Output directory */
        rfile_home_dir = (char *) getenv(DOT_DIRECTORY_ENV);
        if (rfile_home_dir == NULL)
           rfile_home_dir = (char *) getenv("$HOME");

	/* Set the Dimension Panel flag */
	selected_dim_panel = 0;

        /* Set the animation min and max flag and values */
        animation_min_max[0] = 0.0;
        animation_min_max[1] = 1e20;
        animation_min_max[2] = 1e20;

	/* Set the Toggle flag for the colormap */
	animation_colormap = 1;

	/* Set the Cycle toggle as the default mode */
	animation_mode = 1;

	/* Set the Forward toggle as the default direction */
	animation_direction = 1;

        return (1);
}

void animate_quit_cb(int w)
{
        ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        CANVASINFO_LINK 		cptr=head_canvas_info;
        struct display_tab              *dtab;
	extern struct display_tab       D_tab;
	display_name_list               *cdptr, *tcdptr;
	int				i, gnarray;
	char                            a_name[6][17];
        void 				free_animation_file_list();
        void 				free_animation_memory_list();
        extern int 			loop_exit;
	extern int                      removeA();
	extern int			graphics_num_of_arrays();
	extern int                      clear_display();
	extern void      		animation_clear();
        extern void 			dispatch_the_next_event();
	extern void			free_animation_list();

	if (aptr == NULL)
	   return; /* already delected animation frame */

        /* Point to the correct Animation Window */
        while ((aptr != NULL) && (aptr->canvas_id != w))
                aptr = aptr->next;
        if (aptr == NULL) return ;

        aptr->sel_ct = 1;    /* Initialize directory file counter */
        aptr->zoom_animation = FALSE; /* Initialize zoom to false */
        aptr->memory_ct = 0;    /* Initialize number of frames to 0 */
        aptr->animation_flg = 1;    /* Run from memory or disk flag */
	aptr->create_animation_flg = 0;    /* creating animation flag */
        aptr->anot_stop = 0;  /* Stop animation flag */
        aptr->stop_from_tog = 0;  /* Stop animation from toggle button */
        aptr->animation_colormap = 1; /* Set Toggle flag for the colormap */
        aptr->animation_mode = 1; /* Set Cycle toggle as the default mode */
        aptr->animation_direction = 1;/*Set Forward toggle default direction*/
        aptr->animation_hori = 0; /* Initial horizontal panel value */
        aptr->animation_vert = 0; /* Initial vertical panel value */
        aptr->animation_speed = 0; /* Initialize animation speed */
        aptr->animation_zoom = 1; /* Initialize animation zoom */
        aptr->file_frame_ct = 0; /* Initialize the file frame counter */
        aptr->a_hori = 0; /* Initialize the horizontal value to zero */
        aptr->a_vert = 0; /* Initialize the vertical value to zero */
        aptr->garbaged = 0; /* Initialized garbage collection flag to false */

        animation_min_max[0] = 0.0;
        animation_min_max[1] = 1e20;
        animation_min_max[2] = 1e20;

        loop_exit=1;

	/* Find the specified VCS Canvas */
        if (cptr == NULL)
            return ;

        for (cptr=head_canvas_info; cptr != NULL; cptr=cptr->next) {
            if ((cptr->connect_id.animate_popup != 0) &&
                (cptr->connect_id.animate_popup ==
               aptr->canvas_id))
               break;
        }

	/* Set the global connection id */
        connect_id = cptr->connect_id;

	animation_clear();

        /* Free the animation file link list */
        free_animation_file_list(cptr);

	/* Free the memory linked list if necessary */
        free_animation_memory_list(cptr);

        /* Remove the canvas link list information used for animation */
        free_animation_list();

        /* Remove the display from the VCS picture form and
         * remove all the data from the VCS data table
         */
        cdptr = cptr->dlist;
        while (cdptr != NULL) {
           dtab=&D_tab;
           while ((dtab != NULL) &&
                 (strcmp(dtab->name, cdptr->display_name) != 0))
                 dtab = dtab->next;
           if (dtab != NULL) { /* must have been removed from animation */
             gnarray = graphics_num_of_arrays(dtab->type);
             for (i=0; i<gnarray; i++)
                 strcpy(a_name[i], dtab->a[i]);
             clear_display(cdptr->display_name);
             remove_display_name(cptr->connect_id, cdptr->display_name); /*remove display name*/
             for (i=0; i<gnarray; i++)                 /*from VCS Canvas info*/
                removeA(a_name[i]);
           }
           tcdptr = cdptr;
           cdptr = cdptr->next;
           free((char *) tcdptr->display_name);
           free((char *) tcdptr);
        }
        cptr->dlist = NULL;

}

int animate_direction_cb(w, value)
int w;
int value;
{
        ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        CANVASINFO_LINK                 cptr=head_canvas_info;
        extern int      loop_exit;

        /* Point to the correct Animation Window */
        while ((aptr!=NULL) && (aptr->canvas_id != w))
                    aptr = aptr->next;
        if (aptr == NULL) return 0;

        /* Find the specified VCS Canvas */
        for (cptr=head_canvas_info; cptr != NULL; cptr=cptr->next) {
            if ((cptr->connect_id.animate_popup != 0) &&
                (cptr->connect_id.animate_popup ==
               aptr->canvas_id))
               break;
        }

	aptr->animation_direction = value;
}

int animate_mode_cb(w, value)
int w;
int value;
{
        ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        CANVASINFO_LINK                 cptr=head_canvas_info;
        extern int      loop_exit;

        /* Point to the correct Animation Window */
        while ((aptr!=NULL) && (aptr->canvas_id != w))
                    aptr = aptr->next;
        if (aptr == NULL) return 0;

        /* Find the specified VCS Canvas */
        for (cptr=head_canvas_info; cptr != NULL; cptr=cptr->next) {
            if ((cptr->connect_id.animate_popup != 0) &&
                (cptr->connect_id.animate_popup ==
               aptr->canvas_id))
               break;
        }

	aptr->animation_mode = value;
}

int ReturnAnmationCreate_flg(w)
int w;
{
        ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        CANVASINFO_LINK                 cptr=head_canvas_info;
        extern int      loop_exit;

        /* Point to the correct Animation Window */
        while ((aptr!=NULL) && (aptr->canvas_id != w))
                    aptr = aptr->next;
        if (aptr == NULL) return 0;

        /* Find the specified VCS Canvas */
        for (cptr=head_canvas_info; cptr != NULL; cptr=cptr->next) {
            if ((cptr->connect_id.animate_popup != 0) &&
                (cptr->connect_id.animate_popup ==
               aptr->canvas_id))
               break;
        }
        /* Return the animation flag */
	return aptr->create_animation_flg;
}

int restore_variable()
{
	int		i, gdnum;
        char            var1[MAX_PATH_LEN];
        char            var2[MAX_PATH_LEN];
        char            var3[MAX_PATH_LEN];
	struct a_attr   *pA_;/* dimension attribute structure */
        int             get_variable_names_from_display();
	extern struct   a_tab *getA();
        extern int      err_warn();
	extern int      chk_mov_A();
	extern FILE     *fperr;

	if (global_atab == NULL)
	   return 1;
	get_variable_names_from_display(var1, var2, var3, &gdnum);

        pA_=global_atab->pA_attr;

	/* Get the data variable structure from the table */
	if ((stored_afirst_alast) && (global_atab != NULL)) {

	   /* Re-store the afirst and alast values */
	   for (i = gdnum; i < global_atab->pA_attr->ND; ++i) {
	       if (pA_->axf[i] != NULL)
	          free((char *) pA_->axf[i]);
	       pA_->axf[i] = NULL;
	       if (pA_->axl[i] != NULL)
	          free((char *) pA_->axl[i]);
	       pA_->axl[i] = NULL;
               if (store_afirst[i] != NULL) {
                  if ((pA_->axf[i]=(char *)malloc(strlen(store_afirst[i])+1))
                       ==NULL) {
                     err_warn(1,fperr,"Error - No memory for list name./n");
                     return 1;
                  }
               }
	       if (store_afirst[i] != NULL)
	          strcpy(pA_->axf[i], store_afirst[i]);
   
               if (store_afirst[i] != NULL) {
                  if ((pA_->axl[i]=(char *)malloc(strlen(store_alast[i])+1))
                       ==NULL) {
                     err_warn(1,fperr,"Error - No memory for list name./n");
                     return 1;
                  }
               }
	       if (store_alast[i] != NULL)
	          strcpy(pA_->axl[i], store_alast[i]);
	   }

           stored_afirst_alast = FALSE;
	} else {
	   /* Re-store the afirst and alast values */
	   for (i = gdnum; i < global_atab->pA_attr->ND; ++i) {
               if (pA_->axf[i] != NULL)
                  free((char *) pA_->axf[i]);
               pA_->axf[i] = NULL;
               if (pA_->axl[i] != NULL)
                  free((char *) pA_->axl[i]);
               pA_->axl[i] = NULL;
           }
	}

        /* Put the struct into the table */
        chk_mov_A(global_atab);
 	global_atab = getA(selected_data);

	/* Set the Dimension Panel flag */
	selected_dim_panel = 0;

	return 0;
}

void order_animation_files(ANIMATIONWINDOWLIST_LINK aptr,CANVASINFO_LINK cptr)
{
	ANIMATIONFILELIST_LINK		oaptr, tptr,hptr;
	int				i=0, ict, num, is_a_num, num_len;
	char *                          num_file;
	char				char_num[6];
	void 				free_animation_file_list();
	extern FILE     		*fperr;

/*DNW   Get the list of animation file names *
	XtVaGetValues(animation_output->animation_list2, XmNitemCount, &ict, 
                      XmNitems, &itms, NULL);
DNW*/

	/* Create the ordered animation file list */
	while (i < ict) {
           strncpy(char_num, num_file, 6); char_num[5] = '\0';
	   num_len = strlen(num_file+5);
           is_a_num = isnum(char_num); /* check for number */
           if (is_a_num) {
	      num = atoi(char_num);
	      /* Create the link list */
	      if((tptr=(ANIMATIONFILELIST_LINK)malloc(sizeof(ANIMATIONFILELIST))) == NULL) {
                  err_warn(1,fperr,
                    "Error - Can not create file order link list!\n");
	          /* Free the animation file link list */
	          free_animation_file_list(cptr);
                  return;
              }
	      /*tptr = (ANIMATIONFILELIST_LINK)malloc(sizeof(ANIMATIONFILELIST));DNW 10/19/94*/
	      tptr->filename = (char *) malloc(num_len*sizeof(char)); 
	      strcpy(tptr->filename, num_file+5);
	      tptr->position = num;
	      tptr->next = NULL;

	      if (cptr->head_animation_list==NULL) {
                  cptr->head_animation_list = tptr;
	      } else {
	        oaptr = hptr = cptr->head_animation_list;
	        while ((oaptr != NULL) && (oaptr->position < num)) {
	           hptr = oaptr;
	           oaptr = oaptr->next;
	        }
	        if (oaptr == cptr->head_animation_list) {
	           cptr->head_animation_list = tptr;
	           tptr->next = oaptr;
	        } else {
	           tptr->next = hptr->next;
	           hptr->next = tptr;
	        }
	      }
	   }
	   free((char *) num_file);
	   ++i;
	}
}

void free_animation_file_list(CANVASINFO_LINK cptr)
{
	ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        ANIMATIONFILELIST_LINK          lptr,tlptr;

        /* Point to the correct Animation Window */
/*DNW        while ((aptr != NULL) && (cptr->connect_id.animate_popup != NULL) &&
               (cptr->connect_id.animate_popup !=
               aptr->animation_output->animation_frame))
            aptr = aptr->next;
DNW*/
        while ((aptr != NULL) && (aptr->canvas_id != cptr->connect_id.animate_popup))
                aptr = aptr->next;
        if (aptr == NULL) return ;

	/* Get the first file in the link list */
	lptr = cptr->head_animation_list;
	
	while (lptr != NULL) {
	   tlptr = lptr;
	   lptr = lptr->next;

	   /* Free the string file name */
	   free((char *) tlptr->filename);

	   /* Free the link list */
	   free((char *) tlptr);
	}

	cptr->head_animation_list = NULL;

#ifdef X11WM
        XFlush(cptr->connect_id.display);
#elif defined QTWM
	/* nothing to do here*/
#else
	fprintf(stderr,"insert your WM Flush here\n");
#endif
}

void free_animation_memory_list(CANVASINFO_LINK cptr)
{
	ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        ANIMATIONMEMORYLIST_LINK        iptr,tiptr;
	char            		buf[MAX_PATH_LEN];

        /* Point to the correct Animation Window */
/*DNW        while ((aptr != NULL) && (cptr->connect_id.animate_popup != NULL) &&
               (cptr->connect_id.animate_popup !=
               aptr->animation_output->animation_frame))
            aptr = aptr->next;
DNW*/
        while ((aptr != NULL) && (aptr->canvas_id != cptr->connect_id.animate_popup))
                aptr = aptr->next;
        if (aptr == NULL) return ;

	/* Free the images in the animation link list */
	iptr = cptr->head_animation_memory;
   
	while (iptr != NULL) {
	   tiptr = iptr;
	   iptr = iptr->next;

	   /* Free the ximage */
#ifdef X11WM
           XDestroyImage(tiptr->ximage);
/* #elif defined QTWM */
/* 	   QtDestroyImage(tiptr->ximage); */
#else
	   fprintf(stderr,"insert here your WM destroy image\n");
#endif
	   /* Free the link list */
	   free((char *) tiptr);
	}
 
	cptr->head_animation_memory = NULL;
	cptr->tail_animation_memory = NULL;

	/* Set the Animation Position counter to zero */
	aptr->memory_ct = 0;
	aptr->anot_stop = 0; /* Stop the animation if it is running */
	aptr->stop_from_tog = 0;
}

void free_animation_window_list(CANVASINFO_LINK cptr)
{
	ANIMATIONWINDOWLIST_LINK        taptr,aptr=head_animation_window_list;

        /* Point to the correct Animation Window */
/*
        while ((aptr != NULL) && (cptr->connect_id.animate_popup != NULL) &&
               (cptr->connect_id.animate_popup !=
               aptr->animation_output->animation_frame)) {
	    taptr = aptr;
            aptr = aptr->next;
	}
*/

        if (aptr == NULL) return ;

	if (aptr == head_animation_window_list)
            head_animation_window_list = aptr->next;
        else
            taptr->next = aptr->next;
        free((char *) aptr);

}

void stop_all_animation(ANIMATIONWINDOWLIST_LINK aptr)
{
        void 				StopAnimation();
        CANVASANIMATIONLIST_LINK	rptr, trptr;

/*DNW
        while (aptr != NULL) {
	    StopAnimation(aptr->animation_output->animation_stop, NULL, NULL);
	    aptr=aptr->next;
	}
DNW*/

        rptr = head_canvasanimation_list;
        while (rptr != NULL) {
            trptr = rptr;
            rptr = rptr->next;
	    free((char *) trptr);
        }
	head_canvasanimation_list = NULL;
}

void StopAnimationCreate(w, client_data, call_data)
int w;
#ifdef X11WM
XtPointer client_data;
XtPointer call_data;
#else
 void *client_data;
 void *call_data;
#endif
{
        ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        extern int      loop_exit;

        /* Point to the correct Animation Window */
        while ((aptr != NULL) && (aptr->canvas_id != w))
                aptr = aptr->next;
        if (aptr == NULL) return ;

        loop_exit = 1;
}

void StopAnimation(w, client_data, call_data)
int w;
#ifdef X11W
XtPointer client_data;
XtPointer call_data;
#else
 void *client_data;
 void *call_data;
#endif
{
	ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
	CANVASINFO_LINK                 cptr=head_canvas_info;
	void				stop_all_animation();
#ifdef X11WM
        extern Pixmap			create_pixmap();
        extern Pixmap			copy_pixmap();
/* #elif defined QTWM */
/*         extern QImage			create_pixmap(); */
/*         extern QImage			copy_pixmap(); */
#endif
        /* Point to the correct Animation Window */
        while ((aptr != NULL) && (aptr->canvas_id != w))
                aptr = aptr->next;
        if (aptr == NULL) return ;

        if (sync_animation_flg) {
          sync_animation_flg = False;
	  stop_all_animation(head_animation_window_list);
          sync_animation_flg = True;
          sync_stop_flg = True;
	  return ;
	}

        /* Find the specified VCS Canvas */
        for (cptr=head_canvas_info; cptr != NULL; cptr=cptr->next) {
            if ((cptr->connect_id.animate_popup != 0) &&
                (cptr->connect_id.animate_popup ==
               aptr->canvas_id))
               break;
        }
        if (cptr == NULL) return ;

	/* Set the global connection id */
	connect_id = cptr->connect_id;

	aptr->anot_stop = 0;
	aptr->stop_from_tog = 0;

#ifdef X11WM
       if (cptr->connect_id.canvas_pixmap != (Pixmap) NULL) {
              XFreePixmap(cptr->connect_id.display, cptr->connect_id.canvas_pixmap);
              cptr->connect_id.canvas_pixmap = (Pixmap) NULL;
       }
#elif defined QTWM
       /* nothing to do here */
#else
       fprintf(stderr,"insert here your FREE Canvas_pixmap func\n");
#endif
#ifdef X11WM
       cptr->connect_id.canvas_pixmap = copy_pixmap( cptr->connect_id ); /* copy current VCS canvas to pixmap */
#elif defined QTWM
       /* nothing to do i think */
#else
       fprintf(stderr,"insert here your copy pixmap func\n");
#endif
}

void RunAnimation(w, client_data, call_data)
int w;
#ifdef X11WM
XtPointer client_data;
XtPointer call_data;
#elif defined QTWM
 void *client_data;
 void *call_data;
#endif
{
	ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
	CANVASINFO_LINK                 cptr=head_canvas_info;
	int				color_val;
#ifdef X11WM
        XWindowAttributes 		xwa;
#else
	Grectangle                      xwa;
#endif
	Boolean				memory_tog_set, rasfile_tog_set;
        extern void                     load_color_table_proc();
	void 		                order_animation_files();
	void 		                free_animation_file_list();
	void                		run_from_disk();
	void                		run_from_mem();
	extern int                     	err_warn();
	extern FILE                    	*fperr;

        /* Point to the correct Animation Window */
        while ((aptr!=NULL) && (aptr->canvas_id != w))
                    aptr = aptr->next;
        if (aptr == NULL) return ;

        /* Find the specified VCS Canvas */
        for (cptr=head_canvas_info; cptr != NULL; cptr=cptr->next) {
            if ((cptr->connect_id.animate_popup != 0) &&
                (cptr->connect_id.animate_popup ==
               aptr->canvas_id))
               break;
        }
        if (cptr == NULL) return ;

	/* Set the global connection id */
	connect_id = cptr->connect_id;

        sync_stop_flg = False;

        memory_tog_set = TRUE;
        rasfile_tog_set = FALSE;
	if ((!memory_tog_set) && (!rasfile_tog_set)) {
           err_warn(1,fperr,"Error - Must select whether to Read Images from\n        'Memory' or 'Output File'.\n");
           return;
	}

	/* Set the animation start time */
	aptr->start = time((time_t *)NULL);

	/* Set the animation frame count to 0 */
        aptr->frame_ct = 0;

	if (aptr->animation_flg == 1) {
	   run_from_mem(aptr, cptr);
	} else if (aptr->animation_flg == 2) {
	   /* Order the list of animation files */
	   order_animation_files(aptr, cptr);

	   /* Run the animation from disk */
	   run_from_disk(aptr, cptr);

	   /* Free the animation file link list */
	   free_animation_file_list(cptr);

	}

	if (quit_animation) {
           /* Set the application's color map */
           (void) load_color_table_proc(FALSE);

           /* Put back the graphics */
#ifdef X11WM
	   XClearWindow(connect_id.display,connect_id.drawable);
#elif defined QTWM
	   vcs_legacy_Qt_clear_window_by_id(connect_id.wkst_id);
#else
	   fprintf(stderr,"insert here your clear win func\n");
#endif
#ifdef X11WM
           XGetWindowAttributes(connect_id.display,
                             connect_id.drawable, &xwa);
           XCopyArea(connect_id.display, connect_id.canvas_pixmap, connect_id.drawable, gc,
                  0,0, xwa.width, xwa.height, 0, 0);
#else
	   fprintf(stderr,"insert here your copy to screen func\n");
#endif
	}
}


void run_from_mem(aptr, cptr)
ANIMATIONWINDOWLIST_LINK        aptr;
CANVASINFO_LINK          	cptr;
{
	ANIMATIONMEMORYLIST_LINK	iptr, siptr;
        CANVASANIMATIONLIST_LINK	tcaptr=head_canvasanimation_list;
        CANVASANIMATIONLIST_LINK	captr;
        CANVASINFO_LINK          	tcptr;
        static CANVASANIMATIONLIST_LINK	rptr;
#ifdef X11WM
        XWindowAttributes 		xwa;
	XImage          		*zoom_ximage=NULL;
	XEvent 				event;   /* will return our events */
	extern XImage   		*xim_create_ximage();
#else
        Grectangle 		xwa;
	void          		*zoom_ximage=NULL;
	extern void   		*xim_create_ximage();
	//	void   		*pixel=NULL;
#endif
	unsigned long   		pixel;
	char            		buf[MAX_PATH_LEN];
	int				event_loop, frame_ct, x_val=0, y_val=0;
	int				i, j, k, l, last_width, last_height;
	int				i_from, i_to, j_from, j_to, mag_loop;
	int			        sync_ct=0, running_all_sync_ani=0;
        double	                        duration;
	time_t				now;
	void				run_all_animation();
	extern void		        xim_load_private_cmap();
        extern void     		load_color_table_proc();
	extern int     			err_warn();
	extern FILE    			*fperr;
	extern void			PyVCScanvas_Dealloc();
        extern void     		dispatch_the_next_event();

	PyThreadState 			*_save;

	/* Set the application's color map */
	if (aptr->animation_colormap == 1)
           (void) load_color_table_proc(FALSE);

#ifdef X11WM
        XFlush(connect_id.display);
        XSync( connect_id.display, FALSE );
#elif defined QTWM
	/*nothing to do here */
#else
	fprintf(stderr,"insert here your sync/flush fun\n");
#endif
        dispatch_the_next_event();

#ifdef X11WM
	XClearWindow(connect_id.display, connect_id.drawable);
#elif defined QTWM
	/* 2013-03-22 C. Doutriaux Qt will be handled from Python */
	fprintf(stderr,"Don't forget to clear window first???\n");
	
	vcs_legacy_Qt_clear_window_by_id(connect_id.wkst_id);
	
#else
	fprintf(stderr,"insert here your WM clear func\n");
#endif

#ifdef X11WM
        XFlush(connect_id.display);
        XSync( connect_id.display, FALSE );
#elif defined QTWM
	/*nothing to do here */
#else
	fprintf(stderr,"insert here your sync/flush fun\n");
#endif
        dispatch_the_next_event();

	/* Loop through the raster file(s) */
	if (aptr->animation_direction == 1)
	   iptr = cptr->head_animation_memory;
	else
	   iptr = cptr->tail_animation_memory;
	if (iptr != NULL) {
/*	   err_warn(0, fperr, "Running animation from memory.\n");*/
           last_width = iptr->ras_width;
           last_height = iptr->ras_height;
	   aptr->anot_stop = TRUE;
	} else
           err_warn(1,fperr,"Error - No Images stored in Memory.\n");

	/* Set up the animation pointer the the VCS Canvas */
        if ((captr=(CANVASANIMATIONLIST_LINK)malloc(
                   sizeof(CANVASANIMATIONLIST))) == NULL) {
            err_warn(1,fperr,
              "Error - Cannot Store any more images in memory!\n");
            return;
        }
	captr->aptr_ptr = aptr;
	captr->cptr_ptr = cptr;
	captr->iptr_ptr = iptr;
        captr->next = NULL;

	if (head_canvasanimation_list == NULL) {
           head_canvasanimation_list = captr;
	} else {
          while (tcaptr->next != NULL)
              tcaptr = tcaptr->next;
          tcaptr->next = captr;
	}
	rptr = captr;

        Py_BEGIN_ALLOW_THREADS

	while ((aptr->anot_stop) && (iptr != NULL)) {
#ifdef X11WM
           XFlush( connect_id.display );
           XSync( connect_id.display, FALSE );
#elif defined QTWM
	/*nothing to do here */
#else
	fprintf(stder,"insert here your sync/flush fun\n");
#endif

#ifdef XEM
/*DNW       Check for next animation event */
	   event_loop = XPending(connect_id.display);
	   while (event_loop != 0) {
	      XNextEvent(connect_id.display, &event);
              /* Handle the X event */
              switch (event.type) {
                case ButtonPress:
              /*       printf("I have pressed a Button!\n");*/
                     break;
                case ButtonRelease:
              /*       printf("I have released a Button!\n");*/
                     break;
                default:
                     break;
              }
	      event_loop = XPending(connect_id.display);
	   }
#elif defined QTWM
	   /*nothing to do looks like */
#else
	   fprintf(stderr,"insert here your event loop animation ?\n");
#endif
/*This piece of coding will be used later to select a slice of data
DNW*/

	   if ((sync_animation_flg) && (aptr->anot_stop == 0)) goto stopa;
	   if (aptr->anot_stop == 0) goto stopa;
	   if (sync_stop_flg) goto stopa;
	   if (aptr->garbaged) goto stopa;

	   /* Make sure we are pointing to the correct image resources */
           cptr = rptr->cptr_ptr;
           aptr = rptr->aptr_ptr;
	   if (!sync_animation_flg)
	      iptr = rptr->iptr_ptr;
	   else {
	      /* Check to make sure all canvases are animating */
              if (!sync_stop_flg) {
	         iptr = cptr->head_animation_memory;
	         for (i=0; i < sync_ct; i++) {
                    iptr = iptr->next;
	            if (iptr == NULL) {
	               sync_ct=0;
	               iptr = cptr->head_animation_memory;
                       break;
                    }
	         }
              }
	   }

	   if (iptr->ximage == NULL)/*Must have exited from animate_quit_cb*/
		break;
           current_iptr = iptr; /* keep a record of the image for later */

	   /* Clear window if necessary */
	   if ((iptr->ras_width != last_width) ||
               (iptr->ras_height != last_height)) {
              last_width = iptr->ras_width;
              last_height = iptr->ras_height;
	   }

	   /* Make sure that the window size is the size needed for the animation */
#ifdef X11WM
           XResizeWindow(connect_id.display, connect_id.drawable, last_width, last_height);
#elif defined QTWM
	   /* 2013-03-22 No more drawing for now */
	   fprintf(stderr,"Resize function was here\n");
	   /*
           vcs_legacy_Qt_resize_window(connect_id.wkst_id,-1,-1,last_width, last_height);
	   */
#endif
	   /* Show the image in the canvas */
	   if (aptr->zoom_animation) { /* Zoom and Pan the Images */
	      /* Set up the magnification table, used for animation zoom */
/*	      setup_the_canvas_magnify_table(aptr, cptr, 0);
	      setup_the_canvas_magnify_table(aptr, cptr, last_width, last_height, 0);*/

	      /* If zoom_ximage is not free, then free it */
	      if (zoom_ximage != NULL)
#ifdef X11WM
                 XDestroyImage(zoom_ximage);
#elif defined QTWM
	      free(zoom_ximage);
	      zoom_ximage = NULL;
#else
	      fprintf(stderr, "insert here your zoom image destroy func\n");
#endif

#ifdef X11WM
	      /* Create the new zoom image */
              zoom_ximage=(XImage *)xim_create_ximage(cptr->connect_id.display,
                            cptr->connect_id.visual, last_width, last_height,
                            DefaultDepth(cptr->connect_id.display,
                            DefaultScreen(cptr->connect_id.display)), 1);
              if (zoom_ximage == NULL) {
                 err_warn(1,fperr,"Error - creating zoom-image.\n");
                 return ;
	      }
#elif defined QTWM
	      vcs_legacy_Qt_image_create(&zoom_ximage,last_width,last_height);
#else 
	      fprintf(stderr,"insert here your create zoom image func\n");
#endif        
	      x_val = 0;
	      mag_loop = cptr->magnify_table[aptr->animation_zoom-1].mag_x;
	      i_from = cptr->magnify_table[aptr->animation_zoom-1].dx+
                       aptr->a_hori;
	      i_to=i_from+cptr->magnify_table[aptr->animation_zoom-1].width;
	      j_from = cptr->magnify_table[aptr->animation_zoom-1].dy+
                       aptr->a_vert;
	      j_to=j_from+cptr->magnify_table[aptr->animation_zoom-1].height;
              for(i=i_from; i<i_to; i++) {
                 for(j=j_from; j<j_to; j++) {
		   if (cptr->head_animation_memory != NULL) {
#ifdef X11WM
                       pixel = XGetPixel(iptr->ximage, i,j);
#elif defined QTWM
		       vcs_legacy_Qt_image_get_pixel(iptr->ximage,last_width, last_height,i,j,&pixel);
#else
		       fprintf(stderr,"insert here your get pixel func\n");
#endif
		   }
                    else
                       break;
                    for(k=0; k<mag_loop; k++) {
                       for(l=0; l<mag_loop; l++) {
#ifdef X11WM
                          XPutPixel(zoom_ximage, (x_val+k), (y_val+l), pixel);
#elif defined QTWM
			  vcs_legacy_Qt_image_put_pixel(&zoom_ximage, last_width, last_height, (x_val+k), (y_val+l), pixel);
#else
			  fprintf(stderr,"insert here your put pixel func\n");
#endif
                       }
                    }
		    y_val = y_val + mag_loop;
                 }
		 x_val = x_val + mag_loop;
		 y_val=0;
              }
              /* View the zoomed image */
              if (cptr->head_animation_memory != NULL) {
#ifdef X11WM
                 if (iptr != NULL)
                  XPutImage(cptr->connect_id.display,cptr->connect_id.drawable,
                   gc,zoom_ximage,0,0,0,0,iptr->ras_width, iptr->ras_height);
#elif defined QTWM
		 /* 2013-03-22 C. Doutriaux, no more display from here */
		 fprintf(stderr, "used to display zoomed image here\n");
		 
		 vcs_legacy_Qt_window_put_image_by_id(cptr->connect_id.wkst_id,zoom_ximage);
		 

#else
		 fprintf(stderr,"insert your put image 1 here\n");
#endif
	      }
	   } else {
#ifdef X11WM
              if (iptr != NULL)
               XPutImage(cptr->connect_id.display,cptr->connect_id.drawable,
                gc,iptr->ximage,0, 0,0,0,iptr->ras_width, iptr->ras_height);
#elif defined QTWM
	      /* 2013-03-22 C. Doutriaux, no more display from here */
	      fprintf(stderr,"We used to plot the image from here!\n");
	      
	      vcs_legacy_Qt_window_put_image_by_id(cptr->connect_id.wkst_id,iptr->ximage);
	      
	      
#else
		 fprintf(stderr,"insert your put image 2 here\n");
#endif
	   }

	   /* Use the raster's color map 
	   if (aptr->animation_colormap == 2) {
              xim_load_private_cmap(cptr->connect_id.display, n_cmap,
                       iptr->red, iptr->green, iptr->blue, iptr->map_length, 
                       iptr->wm_offset, cptr->connect_id.drawable,
                       (XID)cptr->connect_id.drawable);
	   }*/

	   /* sleep for a while  */
	   if (animation_speed != 0)
	       usleep2(animation_speed);

	   /* Get the next image in the list */
	   siptr = iptr;
	   if (aptr->animation_direction == 1)
	      iptr = iptr->next;
	   else
	      iptr = iptr->prev;

	   /* Find the animation mode and set image pointer */
	   if (iptr == NULL) {
	      if (aptr->animation_mode == 1) { /* Cycle thru images */
                 if (aptr->animation_direction == 1)
	            iptr = cptr->head_animation_memory;
	         else
	            iptr = cptr->tail_animation_memory;
	      } else if (aptr->animation_mode == 2)/*Stop after showing all images*/
	         aptr->anot_stop = FALSE;
	      else if (aptr->animation_mode == 3) { /* Forth and Back thru images */
                 if (aptr->animation_direction == 1) {
                    aptr->animation_direction = 2;
                    iptr = siptr->prev;
                 } else {
                    aptr->animation_direction = 1;
                    iptr = siptr->next;
                 }
              }
	   }

           /* Check clock time */
           ++aptr->frame_ct;
           now = time((time_t *)NULL);

	   /* difftime returns the number of seconds between start and now
            * time expressed as a double. Check for 1 second difference.
	    */
	   duration = difftime(now, aptr->start);
	   if (duration >= 1) {
              sprintf(buf, "%d", aptr->frame_ct);
              aptr->frame_ct = 0;
	      aptr->start = time((time_t *)NULL);
	   }

           /* Alternate between the different animations */

	   /* Save the next image for next time in the loop */
	   rptr->iptr_ptr = iptr;

	   if (rptr->next == NULL) {
              rptr = head_canvasanimation_list;
	      if (sync_animation_flg)
	         ++sync_ct;
	   } else
              rptr = rptr->next;

	}
stopa:  Py_END_ALLOW_THREADS

	/* Find the correct animation to stop */
        rptr = tcaptr= head_canvasanimation_list;
        while ((rptr != NULL) && (rptr->aptr_ptr != aptr)) {
	    tcaptr = rptr;
	    rptr = rptr->next;
	}

	/* Now remove ptr from the link list */
	if ((!sync_animation_flg) && (!aptr->garbaged)) {
#ifdef X11WM
	   XClearWindow(cptr->connect_id.display,cptr->connect_id.drawable);
#elif defined QTWM
	   /* 2013-03-22 No more drawing from here */
	   fprintf(stderr,"We used to clear here again\n");
	   /*
	   vcs_legacy_Qt_clear_window_by_id(cptr->connect_id.wkst_id);
	   */
#else
	   fprintf(stderr,"insert your WM clear func here\n");
#endif
	   if (head_canvasanimation_list == NULL) return;
	   if (head_canvasanimation_list == rptr)
               head_canvasanimation_list = rptr->next;
	   else {
               if (rptr != NULL)
                 tcaptr->next = rptr->next;
           }
	} else {
	   tcptr=head_canvas_info;
	   while (tcptr != NULL) {
#ifdef X11WM
	      XClearWindow(tcptr->connect_id.display,
                           tcptr->connect_id.drawable);
#elif defined QTWM
	   /* 2013-03-22 No more drawing from here */
	   fprintf(stderr,"We used to clear from here as well\n");
	   /*
	   vcs_legacy_Qt_clear_window_by_id(tcptr->connect_id.wkst_id);
	   */
#else
	   fprintf(stderr,"insert your WM clear func here\n");
#endif
	      tcptr = tcptr->next;
	   }
	}

	/* Free canvas animation link */
	if (rptr != NULL)
	   free((char *) rptr);

	if (head_canvasanimation_list != NULL) {
	   iptr  = head_canvasanimation_list->iptr_ptr;
	   cptr  = head_canvasanimation_list->cptr_ptr;
	   aptr  = head_canvasanimation_list->aptr_ptr;
           rptr = head_canvasanimation_list;
	}

	if (aptr->stop_from_tog) {
           aptr->stop_from_tog = FALSE;
	   RunAnimation(-1, NULL, NULL);
	}

	/* If garbaged collected by Python, then we must finish up the 
	 * freeing of memory by recalling PyVCScanvas_Dealloc.
	if (aptr->garbaged)
	   PyVCScanvas_Dealloc(NULL);
	 */
}

void	run_from_disk(aptr, cptr)
ANIMATIONWINDOWLIST_LINK        aptr;
CANVASINFO_LINK                 cptr;
{
	ANIMATIONMEMORYLIST_LINK	iptr, siptr;
        CANVASANIMATIONLIST_LINK	tcaptr=head_canvasanimation_list;
        CANVASANIMATIONLIST_LINK	captr;
	ANIMATIONFILELIST_LINK          afptr;
        static CANVASANIMATIONLIST_LINK	rptr;
#ifdef X11WM
	XEvent 				report;   /* will return our events */
	XImage          		*ximage, *shmximage, *zoom_ximage;
	Window				canvas_win=connect_id.drawable;
	extern XImage			*ras_load_ximage(); 
        extern XImage                   *xim_create_ximage();
	extern XImage			*xim_create_shmximage(); 
#else
	void          		*ximage, *shmximage, *zoom_ximage;
	extern void			*ras_load_ximage(); 
        extern void                   *xim_create_ximage();
	extern void			*xim_create_shmximage(); 
#endif
        int                             xwidth, yheight;
	time_t				start, finish;
        double	                        duration;
	time_t                          now;
	int				ct=0, i, j, k, l, frame_ct, event_loop;
	int				end_file, where_from, map_length;
        int                             i_from, i_to, j_from, j_to, mag_loop, x_val=0, y_val=0;
	int				wm_offset, sleep_value;
        unsigned long                   pixel;
	u_char              		red[NUM_COLORS]; 
        u_char				green[NUM_COLORS];
        u_char			        blue[NUM_COLORS];
	Boolean				first_time = TRUE, file_ended;
	char            		buf[MAX_PATH_LEN];
    	struct rasterfile   		rh;
	extern int			xim_load_default_cmap();
	extern void			xim_load_private_cmap();
	int             		last_width, last_height;
	FILE                		*fp;
        extern void     		load_color_table_proc();
	extern int     			err_warn();
	extern FILE    			*fperr;

	/* Set the application's color map */
	if (aptr->animation_colormap == 1)
           (void) load_color_table_proc(FALSE);

	/* Get the first file in the link list */
	afptr = cptr->head_animation_list;

/* Set the position of the files to not sensitive *
        XtSetSensitive(aptr->animation_output->animation_lbl7, FALSE);
	XtSetSensitive(aptr->animation_output->animation_txt3, FALSE);
	XtSetSensitive(aptr->animation_output->animation_scl1, FALSE);
*/

	if (afptr == NULL) {
	   err_warn(0, fperr, "Error - Must first select image file(s) from\n");
	   err_warn(0, fperr, "        the '.ras Files' window.\n");
	} else
	   err_warn(0, fperr, "Running animation from disk.\n");

	/* Loop through the raster file(s) */
	aptr->anot_stop = TRUE;

	/* Set start and finish for the clock and the frame counter *
	start = time(NULL);
	aptr->frame_ct = 0;
	*/
#ifdef X11WM
	XClearWindow(cptr->connect_id.display, cptr->connect_id.drawable);
#elif defined QTWM
	vcs_legacy_Qt_clear_window_by_id(cptr->connect_id.wkst_id);
#else
	fprintf(stderr,"insert your clear func here\n");
#endif

        /* Create the new zoom image */
/*
        XtVaGetValues(cptr->connect_id.canvas_drawable, XmNwidth, &xwidth,
                      XmNheight, &yheight, NULL);
*/
#ifdef X11WM
        zoom_ximage = (XImage *)xim_create_ximage(cptr->connect_id.display,
                      cptr->connect_id.visual, xwidth, yheight, 
                      DefaultDepth(cptr->connect_id.display,
                      DefaultScreen(cptr->connect_id.display)), 1);
        if (zoom_ximage == NULL) {
           err_warn(1,fperr,"Error - creating zoom-image.\n");
           return ;
        }
#else
	fprintf(stderr,"insert here your WM create zooom img here\n");
#endif
	while ((aptr->anot_stop) && (afptr != NULL)) {
	   /* Open the raster file */
	   if ( (fp = fopen(afptr->filename, "r")) == NULL) {
	      err_warn(1,fperr,
                "Error - Cannot open the output file (%s).\n", afptr->filename);
	      return ;
	   }

	   /* Step through each raster image in the file */
	   where_from = 1;
	   file_ended = FALSE;
	   while ((aptr->anot_stop) && (!file_ended)) {
/*
	      * Check for next animation event *
	      event_loop = XtAppPending(cptr->connect_id.app_context);
	      while (event_loop != 0) {
		  XtAppNextEvent(cptr->connect_id.app_context, &report);
	          XtDispatchEvent(&report);
	          event_loop = XtAppPending(cptr->connect_id.app_context);
	      }
*/

	      /* Load the raster header infomation */
	      if ((ras_load_header(fp, &rh)) == -1)
	         file_ended = TRUE;
	
	      if (!file_ended) {
	         /* Get the raster's color map */
	         if ((rh.ras_maptype != RMT_NONE) &&
                     (ras_load_colormap(fp, &rh, &map_length,
                     red, green, blue) == -1)) {
	             err_warn(1,fperr, "Error - Cannot read colormap data.\n");
	             fclose(fp);
	             return ;
                 }

    	         /* Read in the image data */
#ifdef X11WM
    	         if((ximage=(XImage *)ras_load_ximage(cptr->connect_id.display,
                            cptr->connect_id.visual,fp,&rh)) == (XImage *)0) {
	                err_warn(1,fperr, "Error - Cannot read image data.\n");
	                fclose(fp);
	                return ;
    	         }
#else
		 fprintf(stderr,"insert your WM ras_load func here\n");
#endif

	 	 /* Use the raster's color map */
                 wm_offset = 256-map_length;
#ifdef X11WM
	         if (aptr->animation_colormap == 2) {
                    xim_load_private_cmap(cptr->connect_id.display, n_cmap,
                           red, green, blue, map_length, wm_offset,
                           (XID)cptr->connect_id.drawable, (XID)cptr->connect_id.drawable);
	         }
#else
		 fprintf(stderr,"insert here your laod private cmpa func here\n");
#endif
	         /*
                  * Add data. Use offset to avoid flashing
                  */
#ifdef X11WM

                 xim_add_offset(ximage, NULL, wm_offset);
#else
		 fprintf(stderr,"insert here your add ofsse func\n");
#endif

	         /* Clear window if necessary */
                 if (first_time) {
                    last_width = rh.ras_width;
                    last_height = rh.ras_height;
                    first_time = FALSE;
	         }
	         if ((rh.ras_width != last_width) ||
                     (rh.ras_height != last_height)) {
                    last_width = rh.ras_width;
                    last_height = rh.ras_height;
#ifdef X11WM
	            XClearWindow(cptr->connect_id.display, 
                                 cptr->connect_id.drawable);
#elif defined QTWM
		    vcs_legacy_Qt_clear_window_by_id(cptr->connect_id.wkst_id);
#else
		    fprintf(stderr,"insert your cler func here\n");
#endif
	         }

	         /* Show the image in the canvas */
                 if (aptr->zoom_animation) { /* Zoom and Pan the Images */
                    x_val = 0;
                    mag_loop=cptr->magnify_table[aptr->animation_zoom-1].mag_x;
                    i_from = cptr->magnify_table[aptr->animation_zoom-1].dx+
                             aptr->a_hori;
                    i_to   = i_from+
                             cptr->magnify_table[aptr->animation_zoom-1].width;
                    j_from = cptr->magnify_table[aptr->animation_zoom-1].dy+
                             aptr->a_vert;
                    j_to   = j_from+
                             cptr->magnify_table[aptr->animation_zoom-1].height;
                    for(i=i_from; i<i_to; i++) {
                       for(j=j_from; j<j_to; j++) {
#ifdef X11WM
                          pixel = XGetPixel(ximage, i,j);
#else
			  fprintf(stderr,"insert your get pix func here\n");
#endif
                          for(k=0; k<mag_loop; k++) {
                             for(l=0; l<mag_loop; l++) {
#ifdef X11WM
                               XPutPixel(zoom_ximage, (x_val+k), (y_val+l), pixel);
#else
			       fprintf(stderr,"insert your put pix func here\n");
#endif
                             }
                          }
                          y_val = y_val + mag_loop;
                       }
                       x_val = x_val + mag_loop;
                       y_val=0;
                    }
                    /* View the zoomed image */
#ifdef X11WM
                    XPutImage(cptr->connect_id.display,
                              cptr->connect_id.drawable, gc, zoom_ximage,
                              0, 0, 0, 0, rh.ras_width, rh.ras_height);
#else
			       fprintf(stderr,"insert your put img func here\n");
#endif
                 } else {
#ifdef X11WM
	            XPutImage(cptr->connect_id.display, 
                              cptr->connect_id.drawable, gc, ximage, 
                              0, 0, 0, 0, rh.ras_width, rh.ras_height);
#else
			       fprintf(stderr,"insert your put img func here\n");
#endif
		 }
                 /* Set the position slider and text */
		 ++ct;
	         if (ct > aptr->file_frame_ct)
		    ct = 1;
/*
                 XtVaSetValues(aptr->animation_output->animation_scl1,
                                        XmNvalue, ct, NULL);
                 sprintf(buf, "%d", ct);
                 XtVaSetValues(aptr->animation_output->animation_txt3,
                               XmNvalue, buf, NULL);
*/

	         /* sleep for a while */
/*
	         XtVaGetValues(aptr->animation_output->animation_scl2, 
		               XmNvalue, &sleep_value, NULL);
*/
	         if (sleep_value != 0)
	            sleep(sleep_value);

	         /* Destroy the image */
#ifdef X11WM
	         XDestroyImage(ximage);
#else
		 fprintf(stderr,"insert your destroy func here\n");
#endif
	         /* Get the next raster in the file */
	         if ( (end_file = feof(fp)) == EOF)
	            file_ended = TRUE;
	         else
	            where_from += rh.ras_length;
	      }

              /* Check clock time */
              ++aptr->frame_ct;
	      now = time((time_t *)NULL);
              duration = difftime(now, aptr->start);
              if (duration >= 1) {
                 sprintf(buf, "%d", aptr->frame_ct);
/*DNW
                 XtVaSetValues(aptr->animation_output->animation_frames_per_sec,
                            XmNvalue, buf, NULL);
DNW*/
                 aptr->frame_ct = 0;
                 aptr->start = time((time_t *)NULL);
              }
	   }

	   afptr = afptr->next;
	   if (afptr == NULL) {
              if (aptr->animation_mode == 1) /* Cycle thru images */
	         afptr = cptr->head_animation_list;
              else /*Stop after showing all images*/
                 aptr->anot_stop = FALSE;
           }

	   /* Close the raster file */
	   fclose(fp);
	}

/*
        XtVaSetValues(aptr->animation_output->animation_frames_per_sec,
                      XmNvalue, "0", XmNbackground, red, NULL);
*/

/* Set the position of the files to sensitive *
	XtSetSensitive(aptr->animation_output->animation_lbl7, TRUE);
	XtSetSensitive(aptr->animation_output->animation_txt3, TRUE);
	XtSetSensitive(aptr->animation_output->animation_scl1, TRUE);
*/

        if (zoom_ximage == NULL)
#ifdef X11WM
           XDestroyImage(zoom_ximage);
#else
	fprintf(stderr,"insert your destroyimg func here\n");
#endif
	if (aptr->stop_from_tog) {
           aptr->stop_from_tog = FALSE;
	   RunAnimation(-1, NULL, NULL);
	}
}

int usleep2(sleep_time)
int sleep_time;
{
        struct timespec interval, returnval;

	if (sleep_time == 0)
	   return 1;

        interval.tv_sec = sleep_time/10;       /* 1 sec. sleep cycle */
        interval.tv_nsec = (sleep_time%10)*100000000; /* 1/10 sec. sleep cycle */
        nanosleep (&interval, &returnval );

/* Doesn't work for the Mac platform. So re-wrote animation pause with nanosleep function.
	clock_t 	start_time;
	float           duration;
	start_time = clock();
	duration = (clock() - start_time) / (100000);
	while (duration  < (sleep_time)) {
	   duration = (clock() - start_time) / (100000);
	}
*/
        return 1;

}

void  ScaleZoom(w,value,cb)
int w;
int  value;
int cb;
{
        ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        CANVASINFO_LINK                 cptr=head_canvas_info;
	char    			text[5];
	void 				show_zoom_pan_area();

        /* Point to the correct Animation Window */
        if (!sync_animation_flg) {
           while ((aptr != NULL) && (aptr->canvas_id!=w))
                 aptr = aptr->next;
           if (aptr == NULL) return ;
	}

        /* Find the specified VCS Canvas */
cvsptr: for (cptr=head_canvas_info; cptr != NULL; cptr=cptr->next) {
            if ((cptr->connect_id.animate_popup != 0) &&
                (cptr->connect_id.animate_popup ==
               aptr->canvas_id))
               break;
        }
        if (cptr == NULL) return ;
     
        /* Set the global connection id */
        connect_id = cptr->connect_id;

	aptr->animation_zoom = value;

	if (aptr->animation_zoom == 1)
	   aptr->zoom_animation = FALSE;
	else {
	   /* Set up the magnification table, used for animation zoom */
	   setup_the_canvas_magnify_table(aptr, cptr, 0);

	   aptr->zoom_animation = TRUE;
	   aptr->a_hori = cptr->magnify_table[aptr->animation_zoom-1].width *
                 (aptr->animation_zoom-1);
           aptr->a_hori = ((aptr->animation_hori + 100) *
                 aptr->a_hori * 0.0050) - (aptr->a_hori*0.5);
	   aptr->a_vert = cptr->magnify_table[aptr->animation_zoom-1].height *
                 (aptr->animation_zoom-1);
           aptr->a_vert = ((aptr->animation_vert + 100) *
                 aptr->a_vert * 0.0050) - (aptr->a_vert*0.5);
	}

        /* If the animation is stopped then show zoomed area */
	if (!aptr->anot_stop)
	   show_zoom_pan_area(aptr, cptr);

	/* Get the next animation to zoom */
        if (sync_animation_flg) {
           aptr = aptr->next;
           if (aptr != NULL)
	      goto cvsptr;
	}
}

void  ScaleHori(w,value,cb)
int w;
int value;
int cb;
{
        ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        CANVASINFO_LINK                 cptr=head_canvas_info;
        char            		text[5];
	void 				show_zoom_pan_area();

        /* Point to the correct Animation Window */
        if (!sync_animation_flg) {
           while ((aptr != NULL) && (aptr->canvas_id!=w))
                 aptr = aptr->next;
           if (aptr == NULL) return ;
	}

        /* Find the specified VCS Canvas */
cvsptr: for (cptr=head_canvas_info; cptr != NULL; cptr=cptr->next) {
            if ((cptr->connect_id.animate_popup != 0) &&
                (cptr->connect_id.animate_popup ==
               aptr->canvas_id))
               break;
        }
        if (cptr == NULL) return ;
     
        /* Set the global connection id */
        connect_id = cptr->connect_id;

        aptr->animation_hori = value;

	if (aptr->animation_zoom == 1)
	   aptr->zoom_animation = FALSE;
	else {
	   /* Set up the magnification table, used for animation zoom */
	   setup_the_canvas_magnify_table(aptr, cptr, 0);

	   aptr->zoom_animation = TRUE;
	   aptr->a_hori = cptr->magnify_table[aptr->animation_zoom-1].width *
                    (aptr->animation_zoom-1);
           aptr->a_hori = ((aptr->animation_hori + 100) *
                 aptr->a_hori * 0.0050) - (aptr->a_hori*0.5);
	}

        /* If the animation is stopped then show zoomed area */
	if (!aptr->anot_stop)
	   show_zoom_pan_area(aptr, cptr);

	/* Get the next animation to move horizontally */
        if (sync_animation_flg) {
           aptr = aptr->next;
           if (aptr != NULL)
	      goto cvsptr;
	}
}

void  ScaleVert(w,value,cb)
int w;
int value;
int cb;
{
        ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        CANVASINFO_LINK                 cptr=head_canvas_info;
        char            		text[5];
	void 				show_zoom_pan_area();

        /* Point to the correct Animation Window */
        if (!sync_animation_flg) {
           while ((aptr != NULL) && (aptr->canvas_id!=w))
                 aptr = aptr->next;
           if (aptr == NULL) return ;
	}

        /* Find the specified VCS Canvas */
cvsptr: for (cptr=head_canvas_info; cptr != NULL; cptr=cptr->next) {
            if ((cptr->connect_id.animate_popup != 0) &&
                (cptr->connect_id.animate_popup ==
               aptr->canvas_id))
               break;
        }
        if (cptr == NULL) return ;
     
        /* Set the global connection id */
        connect_id = cptr->connect_id;

        aptr->animation_vert = value;

	if (aptr->animation_zoom == 1)
	   aptr->zoom_animation = FALSE;
	else {
	   /* Set up the magnification table, used for animation zoom */
	   setup_the_canvas_magnify_table(aptr, cptr, 0);

	   aptr->zoom_animation = TRUE;
	   aptr->a_vert = cptr->magnify_table[aptr->animation_zoom-1].height *
                 (aptr->animation_zoom-1);
           aptr->a_vert = ((aptr->animation_vert + 100) *
                 aptr->a_vert * 0.0050) - (aptr->a_vert*0.5);
	}

        /* If the animation is stopped then show zoomed area */
	if (!aptr->anot_stop)
	   show_zoom_pan_area(aptr, cptr);

	/* Get the next animation to move horizontally */
        if (sync_animation_flg) {
           aptr = aptr->next;
           if (aptr != NULL)
	      goto cvsptr;
	}
}

void  ScaleSpeed(w,value,cb)
int w;
int value;
int cb;
{
	ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
	char    	text[5];

        /* Point to the correct Animation Window */
        while ((aptr != NULL) && (aptr->canvas_id!=w))
                 aptr = aptr->next;
        if (aptr == NULL) return ;

        animation_speed = value;

#ifdef X11WM
        XFlush(connect_id.display);
#elif defined QTWM
	/*nothing to do here */
#else
	fprintf(stderr,"insert tyour flush func here\n");
#endif
}

void  ScalePosition(w,value,cb)
int w;
int value;
int cb;
{
	ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        ANIMATIONMEMORYLIST_LINK        iptr;
        CANVASINFO_LINK                 cptr=head_canvas_info;
#ifdef X11WM
        XWindowAttributes 		xwa;
        XImage                          *zoom_ximage;
        extern XImage                   *xim_create_ximage();
#else
	Grectangle xwa;
	void *zoom_ximage=NULL;
#endif
	int				i;
        char            		text[20];
        int                             j, k, l, x_val=0, y_val=0;
        int                             i_from, i_to, j_from, j_to, mag_loop;
        unsigned long                   pixel;
	extern void			xim_load_private_cmap();
        extern void                     load_color_table_proc();
        void                            order_animation_files();
        void                            show_file_frame();
        void                            free_animation_file_list();

        /* Point to the correct Animation Window */
        if (!sync_animation_flg) {
	   while ((aptr!=NULL) && (aptr->canvas_id != w))
                    aptr = aptr->next;
           if (aptr == NULL) return ;
	}

        /* Find the specified VCS Canvas */
cvsptr: for (cptr=head_canvas_info; cptr != NULL; cptr=cptr->next) {
            if ((cptr->connect_id.animate_popup != 0) &&
                (cptr->connect_id.animate_popup ==
               aptr->canvas_id))
               break;
        }
        if (cptr == NULL) return ;

        /* Set the global connection id */
        connect_id = cptr->connect_id;

	if (animation_flg == 2) { /*Show file image */
           order_animation_files(aptr, cptr);

           /* Show the requested frame in the file */
           show_file_frame(aptr, cptr, value);

           /* Free the animation file link list */
           free_animation_file_list(cptr);
	} else {
           /* Find the correct animation to stop */
           iptr = cptr->head_animation_memory;

	   /* Get the correct image */
	   for (i=1; i < value; ++i) {
	      iptr = iptr->next;
              if (iptr == NULL)
                 iptr = cptr->head_animation_memory;
           }

           /* Save the current frame for later */
           current_iptr = iptr;
           if (zoom_ximage!=NULL) {free(zoom_ximage);zoom_ximage=NULL;}; 
           /* Set the raster's color map */
           if (aptr->animation_colormap == 1) {
              (void) load_color_table_proc(FALSE);
           } else {
#ifdef X11WM
              xim_load_private_cmap(connect_id.display, n_cmap,
                     iptr->red, iptr->green, iptr->blue, iptr->map_length,
                     iptr->wm_offset, connect_id.drawable, connect_id.drawable);
#else
	      fprintf(stderr,"insert here your WM load priv cm\n");
#endif
           }
   
           /* Show the image in the canvas */
           if (aptr->zoom_animation) { /* Zoom and Pan the Images */
	      /* Set up magnification table, used for animation zoom */
	      setup_the_canvas_magnify_table(aptr, cptr, 0);
#ifdef X11WM
              /* Create the new zoom image */
              XGetWindowAttributes(connect_id.display,
                             connect_id.drawable, &xwa);
              zoom_ximage = (XImage *)xim_create_ximage(
                   connect_id.display, connect_id.visual, 
                   xwa.width, xwa.height, DefaultDepth(connect_id.display,
                   DefaultScreen(connect_id.display)), 1);
              if (zoom_ximage == NULL) {
                 err_warn(1,fperr,"Error - creating zoom-image.\n");
                 return ;
              }
#elif defined QTWM
	      vcs_legacy_Qt_get_window_dimensions_by_id(connect_id.wkst_id,&xwa.x,&xwa.y,&xwa.width,&xwa.height);
	      vcs_legacy_Qt_image_create(&zoom_ximage,xwa.width,xwa.height);
#else
	      fprintf(stderr,"insert your create zoom img here\n");
#endif
              x_val = 0;
              mag_loop = cptr->magnify_table[aptr->animation_zoom-1].mag_x;
              i_from = cptr->magnify_table[aptr->animation_zoom-1].dx+
                       aptr->a_hori;
              i_to   = i_from+
                       cptr->magnify_table[aptr->animation_zoom-1].width;
              j_from = cptr->magnify_table[aptr->animation_zoom-1].dy+
                       aptr->a_vert;
              j_to   = j_from+
                       cptr->magnify_table[aptr->animation_zoom-1].height;
              for(i=i_from; i<i_to; i++) {
                 for(j=j_from; j<j_to; j++) {
#ifdef X11WM
                    pixel = XGetPixel(iptr->ximage, i,j);
#elif defined QTWM
		    vcs_legacy_Qt_image_get_pixel(iptr->ximage,xwa.width, xwa.height,i,j,&pixel);
#else
		    fprintf(stderr,"insert here your WM get pix func \n");
#endif
                    for(k=0; k<mag_loop; k++) {
                       for(l=0; l<mag_loop; l++) {
#ifdef X11WM
                          XPutPixel(zoom_ximage, (x_val+k), (y_val+l), pixel);
#elif defined QTWM
			  vcs_legacy_Qt_image_put_pixel(&zoom_ximage, xwa.width, xwa.height, (x_val+k), (y_val+l), pixel);
#else
			  fprintf(stderr,"insert here your WM put pix func \n");
#endif
                       }
                    }
                    y_val = y_val + mag_loop;
                 }
                 x_val = x_val + mag_loop;
                 y_val=0;
              }
#ifdef X11WM
              /* View the zoomed image */
              if (iptr != NULL)
                 XPutImage(connect_id.display, connect_id.drawable,
                        gc, zoom_ximage, 0, 0, 0, 0, iptr->ras_width,
                        iptr->ras_height);
              if (zoom_ximage != NULL)
                 XDestroyImage(zoom_ximage);
#elif defined QTWM
              if (iptr != NULL)
		vcs_legacy_Qt_window_put_image_by_id(connect_id.wkst_id,zoom_ximage);
              //if (zoom_ximage != NULL) {free(zoom_ximage);zoom_ximage=NULL;};
#else
		    fprintf(stderr,"insert here your WM put img func \n");
#endif
           } else {
              if (iptr != NULL)
#ifdef X11WM
	        XPutImage(connect_id.display, connect_id.drawable,
			  gc,iptr->ximage,0,0,0,0,iptr->ras_width,iptr->ras_height);
#elif defined QTWM
		vcs_legacy_Qt_window_put_image_by_id(connect_id.wkst_id,iptr->ximage);
#else
	      fprintf(stderr,"insert here your WM put img func \n");
#endif
	   }
	}

	/* Get the next animation to position */
        if (sync_animation_flg) {
           aptr = aptr->next;
           if (aptr != NULL)
	      goto cvsptr;
	}
}

int load_from_disk(w, load_file_name, call_data)
int w;
char *load_file_name;
#ifdef X11WM
XtPointer call_data;
#else
void *call_data;
#endif
{
        ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        CANVASINFO_LINK                 cptr=head_canvas_info;

	ANIMATIONFILELIST_LINK		afptr;
        ANIMATIONMEMORYLIST_LINK        iptr, tiptr;
#ifdef X11WM
        XEvent                          report;   /* will return our events */
        XImage                          *shmximage;
	/*XShmSegmentInfo 		shminfo;*/
        Window                          canvas_win=connect_id.drawable;
#else
        void                          *shmximage;
#endif
        int                             j, event_loop;
        int                             end_file, where_from, map_length;
        int                             wm_offset;
	char				buf[1024];
        u_char                          red[NUM_COLORS];
        u_char                          green[NUM_COLORS];
        u_char                          blue[NUM_COLORS];
        Boolean                         file_ended;
        struct rasterfile               rh;
        extern int                      xim_load_default_cmap();
        extern void                     xim_load_private_cmap();
        int                             usleep2();
#ifdef X11WM
	extern XImage			*ras_load_ximage(); 
	extern XImage			*xim_create_shmximage(); 
#elif defined QTWM
	extern void			*ras_load_ximage(); 
	extern void			*xim_create_shmximage(); 
#endif
        FILE                            *fp;
        extern FILE                     *fperr;
        void                            free_animation_file_list();
        void 				free_animation_memory_list();
        void 				StopAnimation();

        /* Get the first file in the link list */
        /*afptr = cptr->head_animation_list;*/

        /* Stop all animation, then load new images */
	StopAnimation(w, NULL, NULL);

        /* Free the animation file link list */
	free_animation_file_list(cptr);

	/* Free the memory linked list if necessary */
	free_animation_memory_list(cptr);

        aptr->memory_ct = 0;

        /* Loop through the raster file(s) */
        aptr->anot_stop = TRUE;
        /*while ((aptr->anot_stop) && (afptr != NULL)) {*/
        while (aptr->anot_stop) {
           /* Open the raster file */
           if ( (fp = fopen(load_file_name, "r")) == NULL) {
              err_warn(1,fperr,
                "Error - Cannot open the output file (%s).\n", load_file_name);
              return 0;
           }

           /* Step through each raster image in the file */
           where_from = 1;
           file_ended = FALSE;
           while ((aptr->anot_stop) && (!file_ended)) {
/*
              * Check for next animation event *
              event_loop = XtAppPending(cptr->connect_id.app_context);
              while (event_loop != 0) {
                  XtAppNextEvent(cptr->connect_id.app_context, &report);
                  XtDispatchEvent(&report);
                  event_loop = XtAppPending(cptr->connect_id.app_context);
              }
*/

              /* Load the raster header infomation */
              if ((ras_load_header(fp, &rh)) == -1) {
                 file_ended = TRUE;
		 aptr->anot_stop = FALSE;
	      }

              if (!file_ended) {
                 /* Get the raster's color map */
                 /*if ((rh.ras_maptype != RMT_NONE) && (ras_load_colormap(fp, &rh, &map_length,*/
                 if (ras_load_colormap(fp, &rh, &map_length, red, green, blue) == -1) {
                     err_warn(1,fperr, "Error - Cannot read colormap data.\n");
                     fclose(fp);
                     return 0;
                 }

                 /* Create the raster link list */
                 if ((iptr=(ANIMATIONMEMORYLIST_LINK)malloc(sizeof(ANIMATIONMEMORYLIST))) == NULL) {
                     err_warn(1,fperr,
                     "Error - Cannot Store any more images in memory!\n");
                      return 0;
                 }
		 if(!use_shared_mem) {
#ifdef X11WM
                    if((iptr->ximage=(XImage *)
                       ras_load_ximage(cptr->connect_id.display,
                       cptr->connect_id.visual,fp,&rh)) == (XImage *)0) {
                     err_warn(1,fperr, "Error - Cannot read image data.\n");
                     fclose(fp);
                     return ;
                    }
#else
		    fprintf(stderr,"insert here your ras_load_imag call\n");
#endif
                 } else {
                   /*shmximage = (XImage *)xim_create_shmximage(cptr->connect_id.display, cptr->connect_id.visual,
                                 rh.ras_width, rh.ras_height,
                                 rh.ras_depth, &shminfo);
		   CompletionType = XShmGetEventBase(cptr->connect_id.display) + ShmCompletion;*/
                 }
                 ++aptr->memory_ct;
                 iptr->position = aptr->memory_ct;
                 iptr->ras_width = rh.ras_width;
                 iptr->ras_height = rh.ras_height;
	         iptr->map_length = map_length;
                 iptr->wm_offset = 256-map_length;
                 iptr->prev = NULL;
                 iptr->next = NULL;

                 /* Save the raster's color map */
		 for (j=0; j < NUM_COLORS; ++j) {
	             iptr->red[j] = red[j];
	             iptr->green[j] = green[j];
	             iptr->blue[j] = blue[j];
	         }
		 wm_offset = 256-map_length;
#ifdef X11WM
                 xim_load_private_cmap(cptr->connect_id.display, n_cmap,
                        red, green, blue, map_length,
		        wm_offset, cptr->connect_id.drawable, cptr->connect_id.drawable);
#else
		 fprintf(stderr,"insert here your prv cm load fiunc\n");
#endif

                 /*
                  * Add data. Use offset to avoid flashing
                  */
        	 if (use_shared_mem) {
/* This is fuzz 10/26/94 */
            	    /* Copy from ximage memory to shared memory */
            	    /*xim_copy(iptr->ximage, shmximage);*/
#ifdef X11WM
            	    xim_add_offset(shmximage, NULL, wm_offset);
#else
		    fprintf(stderr,"insert here your add_offset func\n");
#endif
            	    /*XDestroyImage(iptr->ximage);*/
            	    iptr->ximage = shmximage;
        	 } else {
#ifdef X11WM
                    xim_add_offset(iptr->ximage, NULL, wm_offset);
#else
		    fprintf(stderr,"insert here your add_offset func\n");
#endif
	         }

                 /* Place the image in the linked list */
                 if (cptr->head_animation_memory == NULL)
                    cptr->head_animation_memory=cptr->tail_animation_memory=tiptr=iptr;
                 else {
                    tiptr->next = iptr;
		    iptr->prev = tiptr;
                    cptr->tail_animation_memory = tiptr = iptr;
                 }

                 /* Get the next raster in the file */
                 if ( (end_file = feof(fp)) == EOF)
                    file_ended = TRUE;
                 else
                    where_from += rh.ras_length;
              }
           }

           /*afptr = afptr->next;*/

           /* Close the raster file */
           fclose(fp);
        }

       if (cptr->head_animation_memory != NULL) {
#ifdef X11WM
         XResizeWindow(connect_id.display, connect_id.drawable, iptr->ras_width, iptr->ras_height);
#elif defined QTWM
	 vcs_legacy_Qt_resize_window(connect_id.wkst_id);
#else
	 fprintf(stderr,"insert your rszi WM func here\n");
#endif
#ifdef X11WM
         XPutImage(cptr->connect_id.display,cptr->connect_id.drawable,
                gc,cptr->head_animation_memory->ximage,0, 0,0,0,iptr->ras_width, iptr->ras_height);
#else
	 fprintf(stderr,"insert your put img here\n");
#endif
           current_iptr = cptr->head_animation_memory; /* keep a record of the image (i.e., frame) for later */
       }
#ifdef X11WM
       XFlush( connect_id.display );
       XSync( connect_id.display, FALSE );
#elif defined QTWM
       /* nothing to do */
#else
       fprintf(stderr,"insert your WM sync/flush here\n");
#endif

	return aptr->memory_ct;
}
int get_variable_names_from_display(aptr, var1, var2, var3, gdnum)
ANIMATIONWINDOWLIST_LINK	aptr;
char	*var1;
char 	*var2;
char	*var3;
int	*gdnum;
{
        CANVASINFO_LINK 		cptr=head_canvas_info;
	display_name_list       	*dptr;
	int				j;
	Boolean				first_time=TRUE;
	Boolean				display_is_on=FALSE;
	char				svar1[MAX_PATH_LEN];
        char				svar2[MAX_PATH_LEN];
        char				svar3[MAX_PATH_LEN];
        extern struct displays  	d_type[NTYPES];
        extern struct display_tab       D_tab;
        struct display_tab      	*dtab;
	extern int              	err_warn();
	extern FILE     		*fperr;

	/* Find the specified VCS Canvas */
	if (cptr == NULL) 
            return 0;
        for (cptr=head_canvas_info; cptr != NULL; cptr=cptr->next) {
            if ((cptr->connect_id.animate_popup != 0) &&
                (cptr->connect_id.animate_popup ==
               aptr->canvas_id))
               break;
        }

	/* Initialize the variables */
	var1[0] = '\0';
	var2[0] = '\0';
	var3[0] = '\0';

	/* Loop through the display table to get the active displays */
        dptr = cptr->dlist;
        while (dptr != NULL) {
           dtab=&D_tab;
           while (dtab != NULL) {
              if (strcmp(dptr->display_name, dtab->name) == 0) {
	         if (dtab->off == 0) {
	            /* Check to see if there is something displayed */
	            display_is_on = TRUE;
   
                    /* Get the variable name(s) */
	            if (dtab->na == 1) {
	               strcpy(var1, dtab->a[0]);
	            } else if (dtab->na == 2) {
	               strcpy(var1, dtab->a[0]);
	               strcpy(var2, dtab->a[1]);
	            } else if (dtab->na == 3) {
	               strcpy(var1, dtab->a[0]);
	               strcpy(var2, dtab->a[1]);
	               strcpy(var3, dtab->a[2]);
	            }
      
	            /* Check to see if the variables names are the same as before */
	            if (first_time) {
                       if (dtab->na == 1) {
                          strcpy(svar1, var1);
	               } else if (dtab->na == 2) {
                          strcpy(svar1, var1);
                          strcpy(svar2, var2);
	               } else if (dtab->na == 3) {
                          strcpy(svar1, var1);
                          strcpy(svar2, var2);
                          strcpy(svar3, var3);
                       }
      
                       /*Get the number of dimensions needed for graphics method*/
	               j = 0;
	               while (cmpncs(d_type[j].type, dtab->type) != 0)
                            ++j;
	               *gdnum = d_type[j].ndim[0];
      
	               first_time = FALSE;
      
	            } /*else {
                       if (dtab->na == 1) {
                         if (cmpncs(var1, svar1) != 0) {
                            err_warn(1,fperr, "Error - More than one variable used in the \n        Page Description Panel.\n");
                            return 0;
                         }
	               } else if (dtab->na == 2) {
                         if (cmpncs(var1, svar1) != 0) {
                            err_warn(1,fperr, "Error - First variable used in the Page Description Panel\n        are not the same.\n");
                            return 0;
                         }
                         if (cmpncs(var2, svar2) != 0) {
                            err_warn(1,fperr, "Error - Second variable used in the Page Description Panel\n        are not the same.\n");
                            return 0;
                         }
	               } else if (dtab->na == 3) {
                         if (cmpncs(var1, svar1) != 0) {
                            err_warn(1,fperr, "Error - First variable used in the Page Description Panel\n        are not the same.\n");
                            return 0;
                         }
                         if (cmpncs(var2, svar2) != 0) {
                            err_warn(1,fperr, "Error - Second variable used in the Page Description Panel\n        are not the same.\n");
                            return 0;
                         }
                         if (cmpncs(var3, svar3) != 0) {
                            err_warn(1,fperr, "Error - Third variable used in the Page Description Panel\n        are not the same.\n");
                            return 0;
                         }
                       }
	            }*/
	         }
              }
              dtab = dtab->next;
           }
           dptr = dptr->next;
        }

	if (display_is_on) 
	   return 1;
	else {
           err_warn(1,fperr, "Error - There is nothing displayed in the VCS Canvas.\n");
	   return 0;
	}
}

void restore_display_after_animation()
{
        struct display_tab              *dtab=NULL;
        extern struct display_tab       D_tab;

        /* Get the first displayed plot and its graphics method */
        dtab=&D_tab;         /* get the display table */
        while (dtab->next != NULL) {
            if ((dtab->off == 0) && (dtab->name[0] != '\0'))
               break;
            else
               dtab = dtab->next;
        }

        /* Restore the graphics method name */
        strcpy(dtab->g_name, hold_animate_display_name);

        if (cmpncs(dtab->type, "Isoline") == 0)
           removeGi_name("animate_isoline");
        else if (cmpncs(dtab->type, "Isofill") == 0)
           removeGfi_name("animate_isofill");
        else if (cmpncs(dtab->type, "Boxfill") == 0)
           removeGfb_name("animate_boxfill");
        else if (cmpncs(dtab->type, "Vector") == 0)
           removeGv_name("animate_vector");
        else if (cmpncs(dtab->type, "Xyvsy") == 0)
           removeGXy_name("animate_Xyvsy");
        else if (cmpncs(dtab->type, "Yxvsx") == 0)
           removeGYx_name("animate_Yxvsx");
        else if (cmpncs(dtab->type, "XvsY") == 0)
           removeGXY_name("animate_XvsY");
}

int create_image_toggle_cb(w, save_file_name, call_data)
int w;
char *save_file_name;
#ifdef X11WM
XtPointer call_data;
#else
void *call_data;
#endif
{
        ANIMATIONWINDOWLIST_LINK        aptr=head_animation_window_list;
        CANVASINFO_LINK                 cptr=head_canvas_info;
        display_name_list               *dptr;
        /*XmString        label_xmstr;*/
        int             ierr, i;
        Boolean         memory_tog_set=True, rasfile_tog_set=False;
        int             create_the_animation_list();
        int             create_images_in_mem_rasfile();
/*DNW        String          raster_name, label_str;*/
#ifdef X11WM
        unsigned int    cursor_shape2 = XC_watch;
#endif
/*DNW        void            set_animation_panel_back();*/
        int             restore_variable();
        void            memory_toggle_cb();
        char            *remove_animation_white_space();
        extern int      loop_exit;
#ifdef X11WM
        Cursor          change_animation_cursors();
#endif
        extern void     dispatch_the_next_event();
        extern FILE     *fperr;
        extern int      err_warn();
        extern void     set_animation_graphics_method();

        /* Point to the correct Animation Window */
        while ((aptr!=NULL) && (aptr->canvas_id != w))
                    aptr = aptr->next;
        if (aptr == NULL) return 0;

        /* Find the specified VCS Canvas */
cvsptr: for (cptr=head_canvas_info; cptr != NULL; cptr=cptr->next) {
            if ((cptr->connect_id.animate_popup != 0) &&
                (cptr->connect_id.animate_popup ==
               aptr->canvas_id))
               break;
        }

        /* Set the global connection id */
        connect_id = cptr->connect_id;

        /* Do not animate while loading or creating raster file(s) */
        if (aptr->anot_stop == TRUE)
           return 0;
        else
           aptr->anot_stop = FALSE;

/*DNW
        XtVaGetValues(w, XmNlabelString, &label_xmstr, NULL);
        XmStringGetLtoR(label_xmstr, XmSTRING_DEFAULT_CHARSET, &label_str);

        * Check to see if the user want to exist loop *
        if (strncmp(label_str, "Stop", 4) == 0) {
           loop_exit = 1; * Stop the creation of images *
           err_warn(0,fperr,"User stopped the creation loop.\n");
           XtFree((char *) label_str);
           XmStringFree(label_xmstr);
           set_animation_graphics_method("");
           return 0;
        }
DNW*/
/*DNW
        XtFree((char *) label_str);
        XmStringFree(label_xmstr);
DNW*/

/*DNW        * Change the curser to stopwatch *
        create_cursor = change_animation_cursors(aptr, cursor_shape2);
        XFlush(connect_id.display);
DNW*/

        /* Change the label of the button */
/*DNW
        dispatch_the_next_event();
        XtVaSetValues(w,
                      XmNbackground, red,
                      RES_CONVERT( XmNlabelString, "Stop Creating: "),
                      NULL);
        dispatch_the_next_event();

        * Get the toggle setting for the memory and/or raster file *
        XtVaGetValues(animation_output->animation_tog5, XmNset,
                      &memory_tog_set, NULL);
        XtVaGetValues(animation_output->animation_tog6, XmNset,
                      &rasfile_tog_set, NULL);
DNW*/
        if ((!memory_tog_set) && (!rasfile_tog_set)) {
           err_warn(1,fperr,"Error - Must select whether to save images in\n        'Memory' and/or 'Output File'.\n");
/*DNW
           set_animation_panel_back(w);
           * Set the cursor back to normal *
           XUndefineCursor(XtDisplay(animation_output->animation_frame),
                   XtWindow(animation_output->animation_frame));
           XFreeCursor(XtDisplay(animation_output->animation_frame),
                   create_cursor);
DNW*/
           return 0;
        } else {
/*DNW
          * Get the Raster File name *
          if (rasfile_tog_set) {
             XtVaGetValues(animation_output->animation_txt2, XmNvalue,
                       &raster_name,NULL);
             raster_name = remove_animation_white_space(raster_name);
             if (raster_name[0] == '\0') {
                XtFree((char *) raster_name);
                err_warn(1,fperr,"Error - Must enter Output File name.\n");
                set_animation_panel_back(w);
                * Set the cursor back to normal *
                XUndefineCursor(XtDisplay(animation_output->animation_frame),
                        XtWindow(animation_output->animation_frame));
                XFreeCursor(XtDisplay(animation_output->animation_frame),
                        create_cursor);
                return 0;
             }
          }
DNW*/
          /* Check to see if the canvas has been updated */
          if (selected_dim_panel == 3) {
             selected_dim_panel = 0;
             if (modified_first_last)
                ierr = restore_variable();
/*DNW
             if (ierr) {
                * Set the cursor back to normal *
                XUndefineCursor(XtDisplay(animation_output->animation_frame),
                        XtWindow(animation_output->animation_frame));
                XFreeCursor(XtDisplay(animation_output->animation_frame),
                        create_cursor);
                return 0;
             }
DNW*/
          }

          /* Check to see if the animation list has been created */
          if (selected_dim_panel == 0) {
             /* Initialize the stored list table used in looping */
             for (i = 0; i < num_index; ++i)
                store_animation_list_list[i].xs = -1;

             /* Create the animation list and place in List Table */
             ierr = create_the_animation_list(aptr);
/*DNW
             if (ierr) {
                *err_warn(1,fperr,"Error - Cannot create animation list.\n");*
                set_animation_panel_back(w);
                * Set the cursor back to normal *
                XUndefineCursor(XtDisplay(animation_output->animation_frame),
                        XtWindow(animation_output->animation_frame));
                XFreeCursor(XtDisplay(animation_output->animation_frame),
                        create_cursor);
                return 0;
             }
DNW*/
             /*selected_dim_panel = 1;*/
          }

/*DNW
          if (selected_dim_panel == 1) {
             if (animation_min_max[0] != 1)
                 set_animation_min_and_max();
          }
DNW*/

          /* Create the raster images in Memory and/or to a Raster file */
	  if (save_file_name[0] != '\0') rasfile_tog_set = True;
	  aptr->create_animation_flg = 1;    /* creating animation */
          ierr = create_images_in_mem_rasfile(aptr, cptr, memory_tog_set,
                                              rasfile_tog_set, save_file_name);
	  aptr->create_animation_flg = 0;    /* done creating animation */
          if (ierr) {
             err_warn(1,fperr,"Error - Cannot create images in Memory, and/or Output File.\n");
/*DNW
             set_animation_panel_back(w);
             * Set the cursor back to normal *
             XUndefineCursor(XtDisplay(animation_output->animation_frame),
                             XtWindow(animation_output->animation_frame));
             XFreeCursor(XtDisplay(animation_output->animation_frame),
                         create_cursor);
DNW*/
             return 0;
          }
        }

/*DNW
        set_animation_panel_back(w);
DNW*/

        /* Set the Read Images from toggle to 'Memory' */
/*DNW
        memory_toggle_cb(animation_output->animation_tog1, NULL, NULL);
DNW*/


        /*DNW
         * Remove the created graphics method
         * and created graphics method
        if (animation_min_max[0] != 1)
           restore_display_after_animation();
         DNW*/

        /* Set the cursor back to normal */
/*DNW
        XUndefineCursor(XtDisplay(animation_output->animation_frame),
                        XtWindow(animation_output->animation_frame));
        XFreeCursor(XtDisplay(animation_output->animation_frame),create_cursor);
DNW*/
    return aptr->memory_ct;
}

int create_the_animation_list(aptr)
ANIMATIONWINDOWLIST_LINK	aptr;
{
        char                    var1[MAX_PATH_LEN];
        char                    var2[MAX_PATH_LEN];
        char                    var3[MAX_PATH_LEN];
        char            	list_name[MAX_PATH_LEN];
        int                     i, j=0, data_dim_num, gdnum, ierr;
        int                     get_variable_names_from_display();
        int                     using_default_graphic_method_value();
        int                     set_animation_min_and_max();
        struct a_attr           *pA_;/* dimension attribute structure */
	int              	create_animation_list();
        extern Boolean          displayed_managed;
        extern void             dim();
        extern struct a_tab     *getA();
        extern int              chk_mov_A();
	extern char 		*repstr();
        extern int              killA();
        extern FILE             *fperr;

        if (get_variable_names_from_display(aptr,var1,var2,var3,&gdnum)==1) {
           /* Get the data varaible structure */
           strcpy(selected_data, var1);
           global_atab = getA(var1);
           global_atab2 = getA(var2); /* get the second variable attributes */
           global_atab3 = getA(var3); /* get the third variable attributes */

           if (global_atab == NULL) {
                err_warn(1,fperr,"Error - Cannot create animation list. No Variable selected!\n");
                return 1;
           }

           /* Get the number of data dimensions */
           data_dim_num = global_atab->pA_attr->ND;

           /* Get the data variable structure from the table */
           pA_=global_atab->pA_attr;

	   /* Set the Dimension assigned name string */
           for (i = 0; i < global_atab->pA_attr->ND; ++i) {
	       pA_->aXN[i] = repstr(pA_->aXN[i],pA_->XN[i]);
	   }

           for (i = 0; i < global_atab->pA_attr->ND; ++i)
               if (*global_atab->pA_attr->XS[i] == 1)
                  --data_dim_num;

           /* If the variable has more than the needed graphics method
            * dimensions, then show Dimension manipulation panel. Else, 
            * give animation error.
            */
           if (data_dim_num > gdnum) {
              /* Store the afirst and alast values */
              for (i = gdnum; i < global_atab->pA_attr->ND; ++i) {
	         if (pA_->XS[i][0] > 1) {
                    if (pA_->axf[i] != NULL) {
                      if ((store_afirst[i]=(char*)malloc(strlen(pA_->axf[i])+1))
                           == NULL) {
                        err_warn(1,fperr,"Error - No memory for list name./n");
                        return 1;
                      }
                      strcpy(store_afirst[i], pA_->axf[i]);
	              stored_afirst_alast = TRUE;
                    } else
                      store_afirst[i] = NULL;
                    if (pA_->axl[i] != NULL) {
                      if ((store_alast[i]=(char *)malloc(strlen(pA_->axl[i])+1))
                           == NULL) {
                        err_warn(1,fperr,"Error - No memory for list name./n");
                        return 1;
                      }
                      strcpy(store_alast[i], pA_->axl[i]);
	              stored_afirst_alast = TRUE;
                    } else
                      store_alast[i] = NULL;
   
	            /* Create the dimension list in List table */
	            ierr=create_animation_list((i+1),pA_->XV[i],
                                               pA_->XS[i][0],list_name);

                    /* Assign the list to the dimension first and last value */
                    if (!ierr) {
                       if ((pA_->axf[i]=(char *)malloc(strlen(list_name)+4))
                            == NULL) {
                         err_warn(1,fperr,"Error - No memory for list name./n");
                         return 1;
                       }
                       if ((pA_->axl[i]=(char *)malloc(strlen(list_name)+4))
                            == NULL) {
                         err_warn(1,fperr,"Error - No memory for list name./n");
                         return 1;
                       }
	               if (gdnum == 1) {
                          if (i == 1) {
                             sprintf(pA_->axf[i], "%s[I]", list_name);
                             sprintf(pA_->axl[i], "%s[I]", list_name);
                          } else if (i == 2) {
                             sprintf(pA_->axf[i], "%s[J]", list_name);
                             sprintf(pA_->axl[i], "%s[J]", list_name);
                          } else if (i == 3) {
                             sprintf(pA_->axf[i], "%s[K]", list_name);
                             sprintf(pA_->axl[i], "%s[K]", list_name);
                          }
	               } else {
                          if (i == 2) {
                             sprintf(pA_->axf[i], "%s[I]", list_name);
                             sprintf(pA_->axl[i], "%s[I]", list_name);
                          } else if (i == 3) {
                             sprintf(pA_->axf[i], "%s[J]", list_name);
                             sprintf(pA_->axl[i], "%s[J]", list_name);
                          } else if (i == 4) {
                             sprintf(pA_->axf[i], "%s[K]", list_name);
                             sprintf(pA_->axl[i], "%s[K]", list_name);
                          }
                       }
	            } else
                      return 1;
	         }
		/* Store List table name and size for looping later */
		store_animation_list_list[j].xs = pA_->XS[i][0];
		++j;
              }
           } else {
              err_warn(1,fperr, "Error - Variable must have at least %d dimensions\n        in order to animate.\n", (gdnum+1));
	      return 1;
           }
        } else
	   return 1;

        /* Put the struct into the table if no errors */
/* This is not needed for Python
        chk_mov_A(global_atab);
        global_atab = getA(var1);
	modified_first_last = TRUE;
*/

        if (!using_default_graphic_method_value())
           animation_min_max[0] = 1.0;

/*DNW
        if (animation_min_max[0] != 1)
            set_animation_min_and_max();
DNW*/

	return 0;
}

    float mean_veloc2(float x,float y)
      {
       float vi;
       double xx,vv;
       int k;

       xx=x*x+y*y;
       vv=sqrt(xx);
       if (vv < 1.e-20) return 0.0;
       if ( vv > 1.0) {k=(int)vv; vi=k; return vi;}
       k=log10(vv);
       k--;
       vi=(int) (vv*pow( (double) 10.0, (double) -k));
       return vi*pow( (double) 10.0, (double) k);
      }

/* Create the fillarea attribute for animation */
int create_fillarea_object(fill_name, color_index)
char *fill_name;
int  color_index;
{
        struct table_fill               *pf, *ptb, *ptab;
        extern struct table_fill        Tf_tab;
        extern int                      chk_mov_Tf();

        /* Search for the fillarea object in the link list */
        for (ptb=ptab=&Tf_tab;
                ptab!=NULL && cmpncs(fill_name,ptab->name)!=0;
                        ptb=ptab,ptab=ptab->next);

        if (ptab != NULL) /* Animation structure already exist */
           return 0;

        /*
         * Set new attributes for Tf.
         * Create a new fillarea structure and copy to it.
         */
        if((pf=(struct table_fill *)malloc(sizeof(struct table_fill)))==NULL) {
          err_warn(1,fperr,
            "Error - memory for getting fillarea table (%s) not found.\n",
             fill_name);
          return 1;
        }

        if ((pf->fasi = (int *) malloc(sizeof(int))) == NULL) {
           err_warn(1,fperr,"Error - table entry memory for color values not found.\n");
           return 1;
        }
        pf->fasi[0]=1;
        pf->x=0.0;
        pf->y=0.0;
        pf->w=0.1;
        pf->h=0.1;

        /* Set the fillarea's name */
        strcpy(pf->name,fill_name);

        /* Set the interior style to solid */
        if ((pf->fais = (int *) malloc(sizeof(int))) == NULL) {
           err_warn(1,fperr,"Error - table entry memory for color values not found.\n");
           return 1;
        }
        pf->fais[0]=1;

        /* Set the color index used for the fillarea */
        if ((pf->faci = (int *) malloc(sizeof(int))) == NULL) {
           err_warn(1,fperr,"Error - table entry memory for color values not found.\n");
           return 1;
        }
        pf->faci[0] = 16+color_index;

        /* Set the new structure in the list */
        chk_mov_Tf (pf);

        return 0;
}

int setup_isoline_animation(gitab, min, max)
struct gi_tab *gitab;
float min;
float max;
{
        int                     i;
        int                     start, end, interval=10;
        char                    lab[13], lb[17], tb[17], to[17];
        float                   delta;
        struct gi_attr          *pgi;
        struct iso              *piso, *next_piso, *piso_new, *tpiso;

        /* Set the interations between the intervals. */
        start = min-5;
        end = max+6;
        delta = (fabs((float)end - (float)start)) / (float) interval;

        /* Get the Isoline structure */
        pgi = gitab->pGi_attr;
        piso = pgi->line;

        /* Save the settings */
        strcpy(lab, piso->lab);
        strcpy(lb,  piso->lb);
        strcpy(tb,  piso->tb);
        strcpy(to,  piso->to);

        /* Free the current iso link list */
        while (piso != NULL) {
              next_piso = piso->next;
              free ((char *) piso);
              piso = next_piso;
        }
        pgi->line = tpiso = NULL;

        /*
         * Generate the line objects and create the
         * new Isoline iso link list
         */
        for (i = 0; i < interval; i++) {
           /* malloc the new Isoline iso struct */
           if ((piso_new =
               (struct iso *)malloc(sizeof(struct iso)))==NULL) {
              err_warn(1,fperr,
                    "Error - No memory for new Graphics isoline id(%d)./n",
                    i);
              return 1;
           } else {
              piso_new->id = i;
              piso_new->p = 0;
              piso_new->lev = start + (i*delta);
              piso_new->incr = 0;
              piso_new->hici = 1;
              strcpy(piso_new->lab, lab);
              strcpy(piso_new->lb, lb);
              strcpy(piso_new->tb, tb);
              strcpy(piso_new->to, to);
              piso_new->next = NULL;
           }

           /* Add to the new iso range link list */
           if (tpiso == NULL)
              pgi->line = tpiso = piso_new;
           else {
              tpiso->next = piso_new;
              tpiso = piso_new;
           }
        }

        return 0;
}

int setup_isofill_animation(gfitab, min, max, n)
struct gfi_tab *gfitab;
float min;
float max;
int n;
{
        int                     i;
        int                     start, end;
        char                    fill_name[17];
        float                   delta;
        struct gfi_attr         *pgfi;
        struct fill_range       *pfiso, *next_pfiso, *pfiso_new, *tpfiso;

        /* Set the interations between the intervals. */
        start = min;
        end = max;
        delta = (fabs((float)end - (float)start)) / (float) n;

        /* Get the Isofill structure */
        pgfi = gfitab->pGfi_attr;
        pfiso = pgfi->line;

        /* Free the current fill_range link list */
        while (pfiso != NULL) {
              next_pfiso = pfiso->next;
              free ((char *) pfiso);
              pfiso = next_pfiso;
        }
        pgfi->line = tpfiso = NULL;

        /*
         * Generate the fillarea objects and create the
         * new Isofill fill_range link list
         */
        for (i = 0; i < n; i++) {
           sprintf(fill_name, "GEN_animation_%d", i);
           if (create_fillarea_object(fill_name, i) == 1)
              return 1;

           /* malloc the new Isofill line struct */
           if ((pfiso_new =
               (struct fill_range *)malloc(sizeof(struct fill_range)))==NULL) {
              err_warn(1,fperr,
                    "Error - No memory for new Graphics fill isoline id(%d)./n",
                    i);
              return 1;
           } else {
              pfiso_new->id = i;
              pfiso_new->lev1 = start + (i*delta);
              pfiso_new->lev2 = start + ((i+1)*delta);
              strcpy(pfiso_new->fill_name, fill_name);
              pfiso_new->next = NULL;
           }

           /* Add to the new fill range link list */
           if (tpfiso == NULL)
              pgfi->line = tpfiso = pfiso_new;
           else {
              tpfiso->next = pfiso_new;
              tpfiso = pfiso_new;
           }
        }

        return 0;
}

void find_good_isoline_min_max(min_val, max_val, interval_num)
float *min_val, *max_val;
int   *interval_num;
{
        int   i,n,k1,k2;
        float first=0,last=0;
        float dr=0,del=0,center=0;
        int nice15(float a,float b,float *dr,int *pw10,float *center);

        if (nice15(*max_val,*min_val,&dr,&n,&center) == 0) {
           return;
        } else {
           del=dr*pow(10.0,(double) n);
           k1=*min_val/del;
           if (k1*del > *min_val) k1--;
           k2=*max_val/del;
           if (k2*del < *max_val) k2++;
           while (k2-k1+1 < 7) {
              del=del*0.5;
              k1=*min_val/del;
              if (k1*del > *min_val) k1--;
              k2=*max_val/del;
              if (k2*del < *max_val) k2++;
           }
           while (k2-k1+1 > 15) {
              del=del*2.0;
              k1=*min_val/del;
              if (k1*del > *min_val) k1--;
              k2=*max_val/del;
              if (k2*del < *max_val) k2++;
           }
           while (k2-k1+1 < 15) {
              k1--;
              if (k2-k1+1 < 15) k2++;
           }
           for (i=k1,n=0;i <= k2;i++,n++) {
              if (n==0)
                 first = i*del;
              last = (i+1)*del;
           }
        }
        *min_val = first;
        *max_val = last;
        *interval_num = n;
}

int using_default_graphic_method_value()
{
        float                           default_val=1e20;
        struct display_tab              *dtab=NULL;
        struct gi_tab                   *gitab=NULL;
        struct gfi_tab                  *gfitab=NULL;
        struct gfb_tab                  *gfbtab=NULL;
        struct gv_tab                   *gvtab=NULL;
        struct gXy_tab                  *gXytab=NULL;
        struct gYx_tab                  *gYxtab=NULL;
        struct gXY_tab                  *gXYtab=NULL;
        struct gSp_tab                  *gSptab=NULL;

        extern struct display_tab       D_tab;
        extern struct gi_tab            Gi_tab;
        extern struct gfi_tab           Gfi_tab;
        extern struct gfb_tab           Gfb_tab;
        extern struct gv_tab            Gv_tab;
        extern struct gXy_tab           GXy_tab;
        extern struct gYx_tab           GYx_tab;
        extern struct gXY_tab           GXY_tab;
        extern struct gSp_tab           GSp_tab;

        if (animation_min_max[0] == 2)
           return 1;

        /* Get the first displayed plot and its graphics method */
        dtab=&D_tab;         /* get the display table */
        while (dtab->next != NULL) {
            if ((dtab->off == 0) && (dtab->name[0] != '\0'))
               break;
            else
               dtab = dtab->next;
        }

        if (cmpncs(dtab->type, "Isoline") == 0) {
           for (gitab=&Gi_tab;gitab != NULL;gitab=gitab->next) {
               if (cmpncs(dtab->g_name,gitab->name) == 0) {
                   break;
               }
           }
           if (gitab->pGi_attr->line->incr == default_val)
              return 1;
        } else if (cmpncs(dtab->type, "Isofill") == 0) {
           for (gfitab=&Gfi_tab;gfitab != NULL;gfitab=gfitab->next) {
               if (cmpncs(dtab->g_name,gfitab->name) == 0) {
                  break;
               }
           }
           if ((gfitab->pGfi_attr->line->lev1 == default_val) &&
              (gfitab->pGfi_attr->line->lev2 == default_val))
              return 1;
        } else if (cmpncs(dtab->type, "Boxfill") == 0) {
           for (gfbtab=&Gfb_tab;gfbtab != NULL;gfbtab=gfbtab->next) {
               if (cmpncs(dtab->g_name,gfbtab->name) == 0) {
                  break;
               }
           }
           if ((gfbtab->pGfb_attr->lev1 == default_val) &&
              (gfbtab->pGfb_attr->lev2 == default_val))
              return 1;
        } else if (cmpncs(dtab->type, "Vector") == 0) {
           for (gvtab=&Gv_tab;gvtab != NULL;gvtab=gvtab->next) {
               if (cmpncs(dtab->g_name,gvtab->name) == 0) {
                  break;
               }
           }
           if (gvtab->pGv_attr->vlen == default_val)
              return 1;
        } else if (cmpncs(dtab->type, "Xyvsy") == 0) {
           for (gXytab=&GXy_tab;gXytab != NULL;gXytab=gXytab->next) {
               if (cmpncs(dtab->g_name,gXytab->name) == 0) {
                  break;
               }
           }
           if ((gXytab->pGXy_attr->dsp[0] == default_val) &&
               (gXytab->pGXy_attr->dsp[1] == default_val) &&
               (gXytab->pGXy_attr->dsp[2] == default_val) &&
               (gXytab->pGXy_attr->dsp[3] == default_val))
              return 1;
        } else if (cmpncs(dtab->type, "Yxvsx") == 0) {
           for (gYxtab=&GYx_tab;gYxtab != NULL;gYxtab=gYxtab->next) {
               if (cmpncs(dtab->g_name,gYxtab->name) == 0) {
                  break;
               }
           }
           if ((gYxtab->pGYx_attr->dsp[0] == default_val) &&
               (gYxtab->pGYx_attr->dsp[1] == default_val) &&
               (gYxtab->pGYx_attr->dsp[2] == default_val) &&
               (gYxtab->pGYx_attr->dsp[3] == default_val))
              return 1;
        } else if (cmpncs(dtab->type, "XvsY") == 0) {
           for (gXYtab=&GXY_tab;gXYtab != NULL;gXYtab=gXYtab->next) {
               if (cmpncs(dtab->g_name,gXYtab->name) == 0) {
                  break;
               }
           }
           if ((gXYtab->pGXY_attr->dsp[0] == default_val) &&
               (gXYtab->pGXY_attr->dsp[1] == default_val) &&
               (gXYtab->pGXY_attr->dsp[2] == default_val) &&
               (gXYtab->pGXY_attr->dsp[3] == default_val))
              return 1;
        }

        return 0;
}

int create_animation_list(index_val, xv, xs, list_name)
int     index_val;
float   *xv;
int     xs;
char    *list_name;
{
        struct l_val            *tval, *hval=NULL, *pval;
        struct l_tab            *ptab;
        int                     i, ierr;
        Boolean                 do_create_list=TRUE;
        char                    val_str[MAX_PATH_LEN];
        extern struct l_tab     L_tab[2];
        extern int              err_warn();
        extern FILE             *fperr;
        extern FILE             *fpout;
        /*extern void             list_lists();*/
        extern int              chk_mov_L();
        extern int              prtL();

        /* Create the new list link list */
        for (i = 0; i < xs; ++i) {
           /* malloc the list struct, or free list if too large */
           if ((tval=(struct l_val *) malloc(sizeof(*tval))) == NULL) {
              err_warn(1,fperr,
                   "Error - memory overflow.  Can't create new list!\n");

              /* remove the partially created list */
              pval = hval;
              while (pval != NULL) {
                 tval = pval;
                 free((void *)tval->str);
                 free((void *)tval);
                 pval = pval->next;
              }

              return 1;
           }
           /* Get the value string from the list */
           sprintf(val_str, "%g", xv[i]);

           /* Set the string value for the list */
           tval->str = (char *) malloc(strlen(val_str) * sizeof(char) + 1);
           strcpy(tval->str, val_str);

           /* Set the number value for the list */
           tval->data = xv[i];

           /* Set the next list pointer to NIL */
           tval->next = NULL;

           /* Add to the list */
           if (hval != NULL) {
              pval->next = tval;
              pval = tval;
           } else
             hval = pval = tval;
        }

        /* Create new table entry */
        if ((ptab=(struct l_tab *) malloc(sizeof(L_tab))) == NULL) {
           err_warn(1,fperr,
                   "Error - memory overflow.  Can't create new list!\n");

           /* remove the created list */
           pval = hval;
           while (pval != NULL) {
              tval = pval;
              free((void *)tval->str);
              free((void *)tval);
              pval = pval->next;
           }

           return 1;
        }

        /* Fill in the structure's attributes */
        sprintf(ptab->name,"animation_%d_dim", index_val);
        strcpy(list_name, ptab->name);
        ptab->count = xs;
        ptab->val = hval;
        ptab->next = NULL;

        /* Put the new List in the List Table */
        ierr = chk_mov_L(ptab);
        /* list the lists from the script file */
        if (ierr) {
	   /*printf("do list_list() here!\n");
           list_lists();*/
        } else { /* Check for error! */
           err_warn(1,fperr,
                   "Error - memory overflow.  Can't create new list!\n");

           /* remove the created list */
           pval = ptab->val;
           while (pval != NULL) {
              tval = pval;
              free((void *)tval->str);
              free((void *) tval);
              pval = pval->next;
           }
           free((void *) ptab);

           return 1;
        }

        return 0;
}

int set_animation_min_and_max()
{
        int i,j,n;
        int num_regis=0, hold[32], hold2[32];
        float min[2],max[2], maxval=1e20;
        float vlen,vxm,vym;
        float *xv[4], *xb[4], *xw[4];
        S_boxfill regis[256];

	extern void set_animation_graphics_method();
        extern void get_min_max_from_data();

        struct display_tab              *dtab=NULL;
        struct gi_tab                   *gitab=NULL;
        struct gfi_tab                  *gfitab=NULL;
        struct gfb_tab                  *gfbtab=NULL;
        struct gv_tab                   *gvtab=NULL;
        struct gXy_tab                  *gXytab=NULL;
        struct gYx_tab                  *gYxtab=NULL;
        struct gXY_tab                  *gXYtab=NULL;
        struct gSp_tab                  *gSptab=NULL;

        extern struct display_tab       D_tab;
        extern struct gi_tab            Gi_tab;
        extern struct gfi_tab           Gfi_tab;
        extern struct gfb_tab           Gfb_tab;
        extern struct gv_tab            Gv_tab;
        extern struct gXy_tab           GXy_tab;
        extern struct gYx_tab           GYx_tab;
        extern struct gXY_tab           GXY_tab;
        extern struct gSp_tab           GSp_tab;

        /* Check to see if the user has defined the min and max */
        if (animation_min_max[0] == 2) {
           if ((animation_min_max[1] < maxval) && (animation_min_max[2] < maxval)) {
              min[0] = min[1] = animation_min_max[1];
              max[0] = max[1] = animation_min_max[2];
              goto have_min_max;
           }
        }

        for (i=0; i<2; i++) {
          min[i] = 1e20;
          max[i] = 1e20;
        }

        /* Get the animation minimun and maximum values for the first variable */
        get_min_max_from_data(0, &min[0],&max[0]);


        /* Get the animation minimun and maximum values for the second variable */
        get_min_max_from_data(1, &min[1],&max[1]);

have_min_max:

        /* Get the first displayed plot and its graphics method */
        dtab=&D_tab;         /* get the display table */
        while (dtab->next != NULL) {
            if ((dtab->off == 0) && (dtab->name[0] != '\0'))
               break;
            else
               dtab = dtab->next;
        }

        strcpy(hold_animate_display_name, dtab->g_name); /* Need to restore later */

        if (cmpncs(dtab->type, "Isoline") == 0) {
           /* Remove the animate_isoline object */
           dtab->off = 2;
           for (gitab=&Gi_tab;gitab != NULL;gitab=gitab->next) {
               if (cmpncs("animate_isoline",gitab->name) == 0) {
                   removeGi_name("animate_isoline");
                   break;
               }
           }
           dtab->off = 0;

           /* Create a new Isoline graphics method for animation */
           strcpy(dtab->g_name, "default");
           if (copy_Gi_name(dtab->g_name, "animate_isoline") == 1) {
              strcpy(dtab->g_name, "animate_isoline");
           } else
              return 1;

           /* Get the newly created Isoline object */
           for (gitab=&Gi_tab;gitab != NULL;gitab=gitab->next)
               if (cmpncs(dtab->g_name,gitab->name) == 0) break;

           /* Change the animation's min and max */
           setup_isoline_animation(gitab, min[0],max[0]);
           set_animation_graphics_method("animate_isoline");
        } else if (cmpncs(dtab->type, "Isofill") == 0) {
           /* Remove the animate_isofill object */
           dtab->off = 2;
           for (gfitab=&Gfi_tab;gfitab != NULL;gfitab=gfitab->next) {
               if (cmpncs("animate_isofill",gfitab->name) == 0) {
                  removeGfi_name("animate_isofill");
                  break;
               }
           }
           dtab->off = 0;

           /* Create a new Isofill graphics method for animation */
           strcpy(dtab->g_name, "default");
           if (copy_Gfi_name(dtab->g_name, "animate_isofill") == 1) {
              strcpy(dtab->g_name, "animate_isofill");
           } else
              return 1;

           /* Get the newly created Isofill object */
           for (gfitab=&Gfi_tab;gfitab != NULL;gfitab=gfitab->next)
               if (cmpncs(dtab->g_name,gfitab->name) == 0) break;

           /* Change the animation's min and max */
           find_good_isoline_min_max(&min[0],&max[0], &n);
           setup_isofill_animation(gfitab, min[0],max[0],n);
           set_animation_graphics_method("animate_isofill");
        } else if (cmpncs(dtab->type, "Boxfill") == 0) {
           /* Remove the animate_boxfill object */
           dtab->off = 2;
           for (gfbtab=&Gfb_tab;gfbtab != NULL;gfbtab=gfbtab->next) {
               if (cmpncs("animate_boxfill",gfbtab->name) == 0) {
                  removeGfb_name("animate_boxfill");
                  break;
               }
           }
           dtab->off = 0;

           /* Create a new Boxfill graphics method for animation */
           strcpy(dtab->g_name, "default");
           if (copy_Gfb_name(dtab->g_name, "animate_boxfill") == 1) {
              strcpy(dtab->g_name, "animate_boxfill");
           } else
              return 1;

           /* Get the newly created Boxfill object */
           for (gfbtab=&Gfb_tab;gfbtab != NULL;gfbtab=gfbtab->next)
               if (cmpncs(dtab->g_name,gfbtab->name) == 0) break;

           /* Change the animation's min and max */
           gfbtab->pGfb_attr->lev1 =  min[0];
           gfbtab->pGfb_attr->lev2 =  max[0];
           set_animation_graphics_method("animate_boxfill");
        } else if (cmpncs(dtab->type, "Vector") == 0) {
           /* Remove the animate_vector object */
           dtab->off = 2;
           for (gvtab=&Gv_tab;gvtab != NULL;gvtab=gvtab->next) {
               if (cmpncs("animate_vector",gvtab->name) == 0) {
                  removeGv_name("animate_vector");
                  break;
               }
           }
           dtab->off = 0;

           /* Create a new Vector graphics method for animation */
           strcpy(dtab->g_name, "default");
           if (copy_Gv_name(dtab->g_name, "animate_vector") == 1) {
              strcpy(dtab->g_name, "animate_vector");
           } else
              return 1;

           /* Get the newly created Vector object */
           for (gvtab=&Gv_tab;gvtab != NULL;gvtab=gvtab->next)
               if (cmpncs(dtab->g_name,gvtab->name) == 0) break;

           /* Change the animation's min and max */
           vxm=(fabs(min[0])>fabs(max[0]))?
                               fabs(min[0]):fabs(max[0]);
           vym=(fabs(min[1])>fabs(max[1]))?
                               fabs(min[1]):fabs(max[1]);
           vlen=mean_veloc2(vxm,vym);
           gvtab->pGv_attr->vlen =vlen;
           set_animation_graphics_method("animate_vector");
        } else if (cmpncs(dtab->type, "Xyvsy") == 0) {
           /* Remove the animate_Xyvsy object */
           dtab->off = 2;
           for (gXytab=&GXy_tab;gXytab != NULL;gXytab=gXytab->next) {
               if (cmpncs("animate_Xyvsy",gXytab->name) == 0) {
                  removeGXy_name("animate_Xyvsy");
                  break;
               }
           }
           dtab->off = 0;

           /* Create a new Xyvsy graphics method for animation */
           strcpy(dtab->g_name, "default");
           if (copy_GXy_name(dtab->g_name, "animate_Xyvsy") == 1) {
              strcpy(dtab->g_name, "animate_Xyvsy");
           } else
              return 1;

           /* Get the newly created Xyvsy object */
           for (gXytab=&GXy_tab;gXytab != NULL;gXytab=gXytab->next)
               if (cmpncs(dtab->g_name,gXytab->name) == 0) break;

           /* Change the animation's min and max */
           gXytab->pGXy_attr->dsp[0] = min[0];
           gXytab->pGXy_attr->dsp[2] = max[0];
           set_animation_graphics_method("animate_xyvsy");
        } else if (cmpncs(dtab->type, "Yxvsx") == 0) {
           /* Remove the animate_Yxvsx object */
           dtab->off = 2;
           for (gYxtab=&GYx_tab;gYxtab != NULL;gYxtab=gYxtab->next) {
               if (cmpncs("animate_Yxvsx",gYxtab->name) == 0) {
                  removeGYx_name("animate_Yxvsx");
                  break;
               }
           }
           dtab->off = 0;

           /* Create a new Yxvsx graphics method for animation */
           strcpy(dtab->g_name, "default");
           if (copy_GYx_name(dtab->g_name, "animate_Yxvsx") == 1) {
              strcpy(dtab->g_name, "animate_Yxvsx");
           } else
              return 1;

           /* Get the newly created Yxvsx object */
           for (gYxtab=&GYx_tab;gYxtab != NULL;gYxtab=gYxtab->next)
               if (cmpncs(dtab->g_name,gYxtab->name) == 0) break;

           /* Change the animation's min and max */
           gYxtab->pGYx_attr->dsp[1] = min[0];
           gYxtab->pGYx_attr->dsp[3] = max[0];
           set_animation_graphics_method("animate_yxvsx");
        } else if (cmpncs(dtab->type, "XvsY") == 0) {
           /* Remove the animate_XvsY object */
           dtab->off = 2;
           for (gXYtab=&GXY_tab;gXYtab != NULL;gXYtab=gXYtab->next) {
               if (cmpncs("animate_XvsY",gXYtab->name) == 0) {
                  removeGXY_name("animate_XvsY");
                  break;
               }
           }
           dtab->off = 0;

           /* Create a new XvsY graphics method for animation */
           strcpy(dtab->g_name, "default");
           if (copy_GXY_name(dtab->g_name, "animate_XvsY") == 1) {
              strcpy(dtab->g_name, "animate_XvsY");
           } else
              return 1;

           /* Get the newly created XvsY object */
           for (gXYtab=&GXY_tab;gXYtab != NULL;gXYtab=gXYtab->next)
               if (cmpncs(dtab->g_name,gXYtab->name) == 0) break;

           /* Change the animation's min and max */
           gXYtab->pGXY_attr->dsp[0] = min[0];
           gXYtab->pGXY_attr->dsp[2] = max[0];
           gXYtab->pGXY_attr->dsp[1] = min[1];
           gXYtab->pGXY_attr->dsp[3] = max[1];
           set_animation_graphics_method("animate_xvsy");
        }

        return 0;

}


int create_images_in_mem_rasfile(aptr, cptr, memory_tog_set, rasfile_tog_set, save_file_name)
ANIMATIONWINDOWLIST_LINK aptr;
CANVASINFO_LINK cptr;
Boolean		memory_tog_set;
Boolean		rasfile_tog_set;
char *		save_file_name;
{
        int             i, a[6][3], loop_pause, ierr;
/*DNW	char *		raster_name=NULL;*/
        struct a_attr   *pA_;/* dimension attribute structure */
#ifdef X11WM
        Cursor          cursor;
        unsigned int    cursor_shape2 = XC_watch;
#endif
        void 		free_animation_memory_list();
        extern int      err_warn();
	extern int	loopit_cdat();
        extern void     dispatch_the_next_event();
        extern FILE     *fperr;

        /* Get the data variable structure from the table *
        pA_=global_atab->pA_attr;
*/

        /* Free the memory linked list if necessary */
	if (memory_tog_set)
           free_animation_memory_list(cptr);

	/* Set-up the index array "a" for the function "loopit_cdat" */
        for (i = 0; i < num_index; ++i) {
	   a[i][0] = 1; /* Set the loop_from value */

           /* Set the loop_to value */
           if (store_animation_list_list[i].xs == -1)
              a[i][1] = 1;
	   else 
              a[i][1] = store_animation_list_list[i].xs;

	   a[i][2] = 1; /* Set the loop_by value */
	}

  	/* Set the loop pause value */
	loop_pause = 0;
#ifdef X11WM
        XFlush(connect_id.display);
#elif defined QTWM
	/* notihng to do */
#else
	fprintf(stderr,"insert here your flush func\n");
#endif

        /* Create the images in Memory and/or to a Raster file */
        if ((memory_tog_set) && (rasfile_tog_set)) {
/*DNW           XtVaGetValues(animation_output->animation_txt2, XmNvalue,
                       &raster_name,NULL);
DNW*/
           ierr = loopit_cdat(aptr, cptr, a[0], a[1], a[2], a[3], a[4], a[5], "\0",
               save_file_name, "\0", "\0", "\0", "\0", "Memory", loop_pause);
/*DNW	   XtFree((char *) raster_name);*/
        } else if (rasfile_tog_set) {
/*DNW           XtVaGetValues(animation_output->animation_txt2, XmNvalue,
                       &raster_name,NULL);
DNW*/
           ierr = loopit_cdat(aptr, cptr, a[0], a[1], a[2], a[3], a[4], a[5], "\0",
               save_file_name, "\0", "\0", "\0", "\0", "\0", loop_pause);
/*DNW	   XtFree((char *) raster_name);*/
	} else if (memory_tog_set) {
           ierr = loopit_cdat(aptr, cptr, a[0], a[1], a[2], a[3], a[4], a[5], "\0",
               "\0", "\0", "\0", "\0", "\0", "Memory", loop_pause);
	}
	if (!ierr)
           return 1;

#ifdef X11WM
	/* Clear the canvas of any image */
        XClearWindow(connect_id.display, connect_id.drawable);
#elif defined QTWM
	vcs_legacy_Qt_clear_window_by_id(connect_id.wkst_id);
#else
	fprintf(stderr,"insert here your clear func\n");
#endif

	/* Ring bell to let user know when done creating images */
        if ((ierr) && (!vcs_legacy_be_quiet))
#ifdef X11WM
           XBell(connect_id.display, 100);
#elif defined QTWM
	vcs_legacy_Qt_window_bell();
#else
	fprintf(stderr,"insert here your bell ring func\n");
#endif

#ifdef X11WM
        XFlush(connect_id.display);
        XSync( connect_id.display, FALSE );
#elif defined QTWM
	/* nothing to do */
#else
	fprintf(stderr,"insert here your sync/flush func\n");
#endif
        dispatch_the_next_event();

        return 0;
}

/*
 * Set up the magnification table. This is used in animation. This
 * function gets the size of the VCS Canvas, then generates the
 * appropriate magnification values to be used in the animation
 * zoom, horizontal pan, and vertical pan.
 */
void setup_the_canvas_magnify_table(aptr, cptr, initialize)
ANIMATIONWINDOWLIST_LINK        aptr;
CANVASINFO_LINK 		cptr;
int				initialize;
{
#ifdef X11WM
        XWindowAttributes 		xwa;
#else
	Grectangle xwa;
#endif
	int		i;

	if ((aptr == NULL) || (cptr && NULL))
	   return ;

	/* Get the VCS canvas width and height */
#ifdef X11WM
        XGetWindowAttributes(cptr->connect_id.display,
                             cptr->connect_id.drawable, &xwa);
#elif defined QTWM
	vcs_legacy_Qt_get_window_dimensions_by_id(cptr->connect_id.wkst_id,&xwa.x,&xwa.y,&xwa.width,&xwa.height);
#else
	fprintf(stderr,"insert here your WM getgeom\n");
#endif
	if (initialize) {
	    aptr->xdwidth  = xwa.width;
	    aptr->ydheight = xwa.height;
	    aptr->a_vert = 0;
	    aptr->a_hori = 0;
	}

	/* Create the table */
	for (i = 0; i < Magnify_Table_Length; ++i) {
	   cptr->magnify_table[i].mag_x = i + 1;
	   cptr->magnify_table[i].mag_y = i + 1;
	   cptr->magnify_table[i].width = (int)xwa.width / (i + 1);
	   cptr->magnify_table[i].height = (int)xwa.height / (i + 1);
	   cptr->magnify_table[i].dx = ((int)xwa.width * 0.5) -
                                  (cptr->magnify_table[i].width * 0.5);
	   cptr->magnify_table[i].dy = ((int)xwa.height * 0.5) -
                                  (cptr->magnify_table[i].height * 0.5);
	}
}

void show_zoom_pan_area(aptr, cptr)
ANIMATIONWINDOWLIST_LINK        aptr;
CANVASINFO_LINK                 cptr;
{
	ANIMATIONMEMORYLIST_LINK      iptr;
 	char *                        val_str;
#ifdef X11WM
        XWindowAttributes 	      xwa;
        extern XImage                 *xim_create_ximage();
        XImage                        *zoom_ximage=NULL;
#else
        Grectangle 	      xwa;
        extern void                 *xim_create_ximage();
        void                        *zoom_ximage=NULL;
#endif
        int                           i, index_num;
        int                           j, k, l, x_val=0, y_val=0;
        int                           i_from, i_to, j_from, j_to;
        int                           mag_loop;
     	unsigned long                 pixel;
        void                          load_color_table_proc();
        void                          xim_load_private_cmap();
        void                          order_animation_files();
        void                          show_file_frame();
        void                          free_animation_file_list();

        /*If the animation is stopped then show zoomed area*/
	if (!aptr->anot_stop) {
           /* Get the index value */
/*DNW           XtVaGetValues(aptr->animation_output->animation_txt3,
                         XmNvalue, &val_str, NULL);
           index_num = atoi(val_str);
DNW*/
           index_num = 1;
   
           if (aptr->animation_flg == 2) { /*Show file image */
              order_animation_files(aptr, cptr);

              /* Show the requested frame in the file */
              show_file_frame(aptr, cptr, index_num);

              /* Free the animation file link list */
              free_animation_file_list(cptr);
           } else {
	      /* Set the correct animation to stop */
/*DNW              iptr = cptr->head_animation_memory;

              * Get the correct image *
              for (i=1; i < index_num; ++i)
                 iptr = iptr->next;
DNW*/
              if (current_iptr == NULL) return;
              iptr = current_iptr;
  
              /* Set the raster's color map */
              if (aptr->animation_colormap == 1) {
                 (void) load_color_table_proc(FALSE);
              } else {
#ifdef X11WM
                 xim_load_private_cmap(connect_id.display, n_cmap,
                        iptr->red, iptr->green, iptr->blue, iptr->map_length,
                        iptr->wm_offset, connect_id.drawable, connect_id.drawable);
#else
		 fprintf(stderr,"insert here your xim lo pcmp\n");
#endif
              }
 
              /* Show the image in the canvas */
              if (aptr->zoom_animation) { /* Zoom and Pan the Images */
	         /* Set up the magnification table, used for animation zoom */
	         setup_the_canvas_magnify_table(aptr, cptr, 0);

	         /* Create the new zoom image */
#ifdef X11WM
                 XGetWindowAttributes(cptr->connect_id.display,
                             cptr->connect_id.drawable, &xwa);
                 zoom_ximage =(XImage *)xim_create_ximage(connect_id.display,
                              connect_id.visual,(int) xwa.width,
                              (int) xwa.height,
                              DefaultDepth(connect_id.display,
                              DefaultScreen(connect_id.display)), 1);
                 if (zoom_ximage == NULL) {
                    err_warn(1,fperr,"Error - creating zoom-image.\n");
                    return ;
                 }
#else
		 fprintf(stderr,"insert here your create zoom func\n");
#endif
                 x_val = 0;
                 mag_loop = cptr->magnify_table[aptr->animation_zoom-1].mag_x;
                 i_from = cptr->magnify_table[aptr->animation_zoom-1].dx+
                          aptr->a_hori;
                 i_to   = i_from+
                          cptr->magnify_table[aptr->animation_zoom-1].width;
                 j_from = cptr->magnify_table[aptr->animation_zoom-1].dy+
                          aptr->a_vert;
                 j_to   = j_from+
                          cptr->magnify_table[aptr->animation_zoom-1].height;
                 for(i=i_from; i<i_to; i++) {
                    for(j=j_from; j<j_to; j++) {
#ifdef X11WM
                       pixel = XGetPixel(iptr->ximage, i,j);
#else
		       fprintf(stderr,"inser there your get pix func\n");
#endif
                       for(k=0; k<mag_loop; k++) {
                          for(l=0; l<mag_loop; l++) {
#ifdef X11WM
                            XPutPixel(zoom_ximage, (x_val+k), (y_val+l), pixel);
#else
			    fprintf(stderr,"inser there your put pix func\n");
#endif
                          }
                       }
                       y_val = y_val + mag_loop;
                    }
                    x_val = x_val + mag_loop;
                    y_val=0;
                 }

                 /* View the zoomed image */
#ifdef X11WM
                 if (iptr != NULL)
                   XPutImage(connect_id.display, connect_id.drawable, gc,
                    zoom_ximage,0,0,0,0, iptr->ras_width, iptr->ras_height);
                 if (zoom_ximage != NULL)
                    XDestroyImage(zoom_ximage);
#else
		 fprintf(stderr,"insert here your put zm img and destroy here\n");
#endif
              } else {
#ifdef X11WM
                 if (iptr != NULL)
                  XPutImage(connect_id.display, connect_id.drawable, gc, 
                   iptr->ximage,0,0,0,0,iptr->ras_width, iptr->ras_height);
#else
		 fprintf(stderr,"insert here your put zm img and destroy here\n");
#endif
	      }
	   }

#ifdef X11WM
           XFlush(connect_id.display);
#elif defined QTWM
	   /* nothing */
#else
	   fprintf(stderr,"insert here your flush func\n");
#endif
    	}
}

void show_file_frame(aptr, cptr, index_num)
ANIMATIONWINDOWLIST_LINK        aptr;
CANVASINFO_LINK                 cptr;
int 	index_num;
{
        ANIMATIONFILELIST_LINK          afptr;
#ifdef X11WM
	XImage                          *ximage, *zoom_ximage;
        Window                          canvas_win=connect_id.drawable;
	Dimension                       xwidth, yheight;
	extern XImage                   *ras_load_ximage();
	extern XImage                   *xim_create_ximage();
#else
	void                          *ximage, *zoom_ximage;
	int                       xwidth, yheight;
	extern void                   *ras_load_ximage();
	extern void                   *xim_create_ximage();
#endif
        int                             i, j, k, l, wm_offset, map_length;
        int                             i_from, i_to, j_from, j_to, mag_loop;
	int                             x_val=0, y_val=0, frame_ct=0;
        int                             end_file, where_from;
        int                             rh_size=32, ras_size;
	Boolean				found_frame=FALSE, file_ended;
        unsigned long                   pixel;
        u_char                          red[NUM_COLORS];
        u_char                          green[NUM_COLORS];
        u_char                          blue[NUM_COLORS];
        struct rasterfile               rh;
        FILE                            *fp;
        extern FILE                     *fperr;
	extern void                     xim_load_private_cmap();

	if (aptr->file_frame_ct == 0) {
           err_warn(1,fperr,
             "Error - There are no frame(s) to display.\n");
           return ;
	}

	/* Get the correct frame */
        afptr = cptr->head_animation_list; /* Get the first file in the link list */

        /* Loop through the raster file(s) */
        while ((afptr != NULL) && (!found_frame)) {
           /* Open the raster file */
           if ( (fp = fopen(afptr->filename, "r")) == NULL) {
              err_warn(1,fperr,
                "Error - Cannot open the output file (%s).\n", afptr->filename);
              return ;
           }

           /* Step through each raster frame in the file(s) */
           where_from = 0;
           file_ended = FALSE;
           while ((!file_ended) && (!found_frame)) {
              /* Load the raster header infomation */
              if ((ras_load_header(fp, &rh)) == -1)
                 file_ended = TRUE;
              else {
                 ras_size = rh.ras_length + rh_size + rh.ras_maplength;
                 where_from += ras_size;
                 ++frame_ct;
	         if (index_num == frame_ct)
		    found_frame = TRUE;
	         else
                    fseek(fp, where_from, SEEK_SET);
              }
           }

           /* Get the next file */
           afptr = afptr->next;

           /* Close the raster file */
	   if (!found_frame)
              fclose(fp);
        }

        /* Show the frame in the VCS canvas */
	if (!file_ended) {
           /* Get the raster's color map */
           if ((rh.ras_maptype != RMT_NONE) &&
               (ras_load_colormap(fp, &rh, &map_length,
               red, green, blue) == -1)) {
               err_warn(1,fperr, "Error - Cannot read colormap data.\n");
               fclose(fp);
               return ;
           }

	   /* Read the image data */
#ifdef X11WM
           if((ximage=(XImage *)ras_load_ximage(cptr->connect_id.display,
                       cptr->connect_id.visual,fp,&rh)) == (XImage *)0) {
                  err_warn(1,fperr, "Error - Cannot read image data.\n");
                  fclose(fp);
                  return ;
           }
#else
	   fprintf(stderr,"insert here your load ras func call\n");
#endif

           /* Use the raster's color map */
           wm_offset = 256-map_length;
#ifdef X11WM
           if (animation_colormap == 2) {
              xim_load_private_cmap(cptr->connect_id.display, n_cmap,
                     red, green, blue, map_length, wm_offset,
                     (XID)cptr->connect_id.drawable, (XID)cptr->connect_id.drawable);
           }
#else
	   fprintf(stderr,"insert here your xim load pr cmap\n");
#endif

           /*
            * Add data. Use offset to avoid flashing
            */
#ifdef X11WM
           xim_add_offset(ximage, NULL, wm_offset);
#else
	   fprintf(stderr,"insert yur add offset func here\n");
#endif

           /* Show the image in the canvas */
           if (aptr->zoom_animation) { /* Zoom and Pan the Images */
              /* Create the new zoom image */
/*DNW              XtVaGetValues(cptr->connect_id.canvas_drawable, XmNwidth, &xwidth,
                            XmNheight, &yheight, NULL);
DNW*/
#ifdef X11WM
              zoom_ximage = (XImage *)xim_create_ximage
                        (cptr->connect_id.display,connect_id.visual,xwidth,
                        yheight, DefaultDepth(cptr->connect_id.display,
                        DefaultScreen(cptr->connect_id.display)), 1);
              if (zoom_ximage == NULL) {
                 err_warn(1,fperr,"Error - creating zoom-image.\n");
                 return ;
              }
#else
	      fprintf(stderr,"insert here your creat zm ing\n");
#endif
              x_val = 0;
              mag_loop = cptr->magnify_table[aptr->animation_zoom-1].mag_x;
              i_from = cptr->magnify_table[aptr->animation_zoom-1].dx+
                       aptr->a_hori;
              i_to   = i_from+cptr->magnify_table[aptr->animation_zoom-1].width;
              j_from = cptr->magnify_table[aptr->animation_zoom-1].dy+
                       aptr->a_vert;
              j_to   =j_from+cptr->magnify_table[aptr->animation_zoom-1].height;
              for(i=i_from; i<i_to; i++) {
                 for(j=j_from; j<j_to; j++) {
#ifdef X11WM
                    pixel = XGetPixel(ximage, i,j);
#else
		    fprintf(stderr,"insert here your get pix\n");
#endif
                    for(k=0; k<mag_loop; k++) {
                       for(l=0; l<mag_loop; l++) {
#ifdef X11WM
                           XPutPixel(zoom_ximage, (x_val+k), (y_val+l), pixel);
#else
			   fprintf(stderr,"insert here your pt pix\n");
#endif
                       }
                    }
                    y_val = y_val + mag_loop;
                 }
                 x_val = x_val + mag_loop;
                 y_val=0;
              }
              /* View the zoomed image */
#ifdef X11WM
              XPutImage(cptr->connect_id.display, cptr->connect_id.drawable,
                   gc, zoom_ximage, 0, 0, 0, 0, rh.ras_width, rh.ras_height);
#else
			   fprintf(stderr,"insert here your pt img\n");
#endif
           } else {
#ifdef X11WM
              XPutImage(cptr->connect_id.display, cptr->connect_id.drawable,
                   gc, ximage, 0, 0, 0, 0, rh.ras_width, rh.ras_height);
#else
			   fprintf(stderr,"insert here your pt img\n");
#endif
	   }
#ifdef X11WM
           /* Destroy the image */
           XDestroyImage(ximage);
#else
	   fprintf(stderr,"insert here your destroy img clal\n");
#endif

           /* Close the raster file */
           fclose(fp);
	}
}

/* Set the cursor to a new shape */
#ifdef X11WM
Cursor change_animation_cursors(aptr, cursor_shape)
ANIMATIONWINDOWLIST_LINK	aptr;
unsigned int    cursor_shape;
{
        Display                 *dpy;
        Cursor                  cursor;
        XColor                  fg, bg;
        int                     screen_num;

/*DNW        dpy = XtDisplay(animation_output->animation_frame); DNW*/
        screen_num = DefaultScreen(dpy);
        fg.pixel=1;
        bg.pixel=2;
        XQueryColor(dpy, DefaultColormap(dpy, screen_num), &fg);
        XQueryColor(dpy, DefaultColormap(dpy, screen_num), &bg);
/*DNW        cursor=XCreateFontCursor(XtDisplay(animation_output->animation_frame), DNW
                  cursor_shape);DNW*/
        XRecolorCursor(dpy, cursor, &fg, &bg);
/*DNW        XDefineCursor(dpy, XtWindow(animation_output->animation_frame), cursor); DNW*/
        return (cursor);
}
#endif

char * remove_animation_white_space(wstr)
char    *wstr;
{
        int     i=0, j=0;

        while (wstr[i] != '\0') {
           if ( isspace(wstr[i]) == 0 ) {
              wstr[j] = wstr[i];
              ++j;
           }
           ++i;
        }
        wstr[j] = '\0';

        return ( wstr );
}

/*
 * Get the C shell tilde (~) convention for the home directory
 */
void specified_user_home_adir(path)
char *path;
{
        int             i=0;
        char            buf[MAX_PATH_LEN];
        char            name[MAX_PATH_LEN];
        struct passwd   *passwd_struct;

        /* Get the string before the '/' */
        while ((path[i] != '\n') && (path[i] != '\0') &&
              (path[i] != '\t') && (path[i] != '/')) {
              buf[i] = path[i];
              ++i;
        }
        buf[i] = '\0';

        strcpy(name, buf+1);

        /* Get the appropriate home directory */
        setpwent(); /* Rewind the password file */
        passwd_struct = (struct passwd *) getpwent(); /* Get the pointer to the first password */
        while ( (passwd_struct != NULL) &&
                (strcmp(name, passwd_struct->pw_name) != 0) )
               passwd_struct =(struct passwd *) getpwent(); /* Get the next password pointer */

        /* Set up the path string */
        if (passwd_struct != NULL) {
            strcpy(name, passwd_struct->pw_dir);
            strcat(name, path+i);
        }
        strcpy(path, name);

        endpwent(); /* Close the password file */
}

/*
 * load_color_table_proc()
 *      Load in a new color table.
 */
void
load_color_table_proc(not_initial)
Boolean         not_initial;
{
        Gintlist                wsid;
        int                     *pid;
        Gcobundl                color;
        char                    inx_num_str[4];
        int                     i, j, c, color_index, c_index;
        int                     R,G,B;
        struct                  color_table *ptab;
        void                    set_slider();
        void                    redraw_darwingareas();
        extern struct           color_table C_tab;
        extern struct           c_val std_color[16];
        extern int              err_warn();
        extern FILE             *fperr;
        extern char *           python_setcolorcell();

        for(ptab=&C_tab;ptab != NULL && (c=strcmp(ptab->name,cmp_filename))!=0;
            ptab=ptab->next) { }
        if (c != 0) {
           err_warn(1,fperr,
             "Error - No such colormap name COLOR(%s).\n",cmp_filename);
           return;
        } else {
             for (i = 0; i <= MAX_COLOR_INDEX; ++i) {
                if (i <= INDEX_TO_END) {
                  color_index = i; /* Set the color index for Xlib */
#ifdef X11WM
                  set_xcolors[color_index].pixel = color_index;
                  set_xcolors[color_index].flags = DoRed|DoGreen|DoBlue;
                  set_xcolors[color_index].pad = 0;
                  set_xcolors[color_index].red  =(int)(ptab->cval[color_index].red  *2.550+0.5)<<8;
                  set_xcolors[color_index].green=(int)(ptab->cval[color_index].green*2.550+0.5)<<8;
                  set_xcolors[color_index].blue =(int)(ptab->cval[color_index].blue *2.550+0.5)<<8;
                  color.red=(set_xcolors[i].red>>8)/255.;
                  color.green=(set_xcolors[i].green>>8)/255.;
                  color.blue=(set_xcolors[i].blue>>8)/255.;
                  if (visual->class != PseudoColor)
                     XAllocColor(connect_id.display, n_cmap, &set_xcolors[color_index]);
#elif defined QTWM
		  /* nothing to do */
#else
		  fprintf(stderr,"insert here your wm set pix colors\n");
#endif
                } else {
                  color_index = i; /* Set the color index for Xlib */
                  j = i - (INDEX_TO_END+1);
#ifdef X11WM
                  set_xcolors[color_index].pixel = color_index;
                  set_xcolors[color_index].flags = DoRed|DoGreen|DoBlue;
                  set_xcolors[color_index].pad = 0;
                  set_xcolors[color_index].red  =(int)(std_color[j].red  *2.550+0.5)<<8;
                  set_xcolors[color_index].green=(int)(std_color[j].green*2.550+0.5)<<8;
                  set_xcolors[color_index].blue =(int)(std_color[j].blue *2.550+0.5)<<8;
                  color.red=(set_xcolors[i].red>>8)/255.;
                  color.green=(set_xcolors[i].green>>8)/255.;
                  color.blue=(set_xcolors[i].blue>>8)/255.;
                  if (visual->class != PseudoColor)
                     XAllocColor(connect_id.display, n_cmap, &set_xcolors[color_index]);
#elif defined QTWM
		  /* nothing to do */
#else
		  fprintf(stderr,"insert your set pix col func here\n");
#endif
                }
             }
 
#ifdef X11WM
             /* this sets the color of read/write cell */
             if (visual->class != PseudoColor) {
                bg=white=WhitePixel(connect_id.display, DefaultScreen(connect_id.display));
                fg=black=BlackPixel(connect_id.display, DefaultScreen(connect_id.display));
                red = set_xcolors[RED].pixel;
                green = set_xcolors[GREEN].pixel;
                blue = set_xcolors[BLUE].pixel;
                cyan = set_xcolors[CYAN].pixel;
                yellow = set_xcolors[YELLOW].pixel;
                magenta = set_xcolors[MAGENTA].pixel;
                olive_green = set_xcolors[OLIVE_GREEN].pixel;
                orange = set_xcolors[ORANGE].pixel;
             } else {
                white=WHITE;
                black=BLACK;
                red = RED;
                green=GREEN;
                blue=BLUE;
                cyan=CYAN;
                yellow=YELLOW;
                magenta = MAGENTA;
                olive_green = OLIVE_GREEN;
                orange = ORANGE;
                XStoreColors (connect_id.display, connect_id.n_cmap, set_xcolors, NUM_COLORS);
             }
#elif defined QTWM
	     /* nothing to do */
#else
	     fprintf(stderr,"insert here your store colors\n");
#endif
        }
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/
