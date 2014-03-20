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
 *      Date: 04/05/97                                                  *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *      File name: vcs_legacy_canvas.h                                         *
 *                                                                      *
 *                                                                      *
 *      Langague: ANSI C                                                *
 *                                                                      *
 *                                                                      *
 *      Comments:                                                       *
 *               This is the include file for the VCS Canvas popup      *
 *               user interface.                                        *
 *                                                                      *
 *                                                                      *
 *      Modifications:							*
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

/*
 ************************************************************************
 *                      Include Files                                   *
 ************************************************************************
 */
#ifdef USEX11
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
/*
 *      X-Library include files.
 */
#endif
#include <sys/types.h>
#include <cairo/cairo.h>

/*
 * Description of header for files containing raster images
 */
#include "rasterfile.h"

/*
 ************************************************************************
 *                      Definitions and Macros                          *
 ************************************************************************
 */
#define Magnify_Table_Length  20

/****************************************************************************
*** Prototypes **************************************************************
*****************************************************************************/
/*
 * This structure contains link list of images saved in memory.
 * Allows the user to view images in both directions.
 */
struct animation_memory_list {
#ifdef USEX11
        XImage                          *ximage;  /* Name of animation file */
#else
  void *ximage; /* place holder for whatever image we decide to use in the end */
#endif
        int                             position;
        int                             ras_width; /* Raster width */
        int                             ras_height; /* Raster height */
        int                             wm_offset; /* Colormap offset */
        int                             map_length; /* Length of Colormap*/
        u_char                          red[MAX_COLORS]; /* Red values */
        u_char                          green[MAX_COLORS]; /* Green values */
        u_char                          blue[MAX_COLORS]; /* Blue values */
        struct animation_memory_list    *prev; /* Previous Image */
        struct animation_memory_list    *next; /* Next Image */
};
typedef struct animation_memory_list    ANIMATIONMEMORYLIST;
typedef ANIMATIONMEMORYLIST             *ANIMATIONMEMORYLIST_LINK;

/* This structure contains the animation file names and their
 * animation position.
 */
struct animation_file_list {
        char                            *filename;/* Name of animation file */
        int                             position; /* file viewing prosition */
        struct animation_file_list      *next; /* next file */
};
typedef struct animation_file_list      ANIMATIONFILELIST;
typedef ANIMATIONFILELIST               *ANIMATIONFILELIST_LINK;

/* This structure contains the VCS Canvas information for panning and
 * zooming in on a canvas.
 */
typedef struct MAG_INFO {
        int             mag_x;  /* magnification in the x direction */
        int             mag_y;  /* magnification in the y direction */
        int             dx;     /* calculated x-axis location */
        int             dy;     /* calculated y-axis location */
        int             width;  /* width of the zoomed region */
        int             height; /* height of the of the zoomed region */
} MAG_INFO;
//MAG_INFO magnify_table[Magnify_Table_Length]; /* animate magnify values */



/* Structure for VCS Canvas displays */
typedef struct display_name_list {       /* Store the display names */
  char *display_name;                      /* Display name */
  struct display_name_list *next;        /* Pointer to the next structure */
} display_name_list;

/* Structure for VCS canvas objects. This is a copy of what is found in the
 * Python VCS module.
 */
struct store_canvas_info {
  Gconid_X_drawable connect_id;   /* point to the canvas connection ID */
  int wkst_id;                    /* XGKS workstation id */
  struct display_name_list *dlist;    /* List of display names */
  ANIMATIONMEMORYLIST_LINK        head_animation_memory;/* Head of animation */
  ANIMATIONMEMORYLIST_LINK        tail_animation_memory;/* Tail of animation */
  ANIMATIONFILELIST_LINK          head_animation_list;
  MAG_INFO magnify_table[Magnify_Table_Length]; /* animate magnify values */
  struct store_canvas_info   *next;
};
typedef struct store_canvas_info   CANVASINFO;
typedef CANVASINFO              *CANVASINFO_LINK;

/*******************************************************************************        END OF FILE
*******************************************************************************/
