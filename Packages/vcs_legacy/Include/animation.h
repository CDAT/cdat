/*
 ************************************************************************
 *                      PCMDI GRAPHICS PACKAGE                          *
 *                      Developed for LLNL-PCMDI use                    *
 *                                                                      *
 *                                                                      *
 *      Copyright (C) 1992. The Regents of the University of California.*
 *      All rights reserved.                                            *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *      Author: Dean N. Williams                                        *
 *                                                                      *
 *      Date: 01/3/92                                                   *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *      File name: animation.h                                          *
 *                                                                      *
 *                                                                      *
 *      Langague: C                                                     *
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
 *              Dean N. Williams    Robert L. Mobley                    *
 *                                                                      *
 *              LLNL                LLNL                                *
 *              PO Box 808, L-264   PO Box 808, L-264                   *
 *              Livermore, CA       Livermore, CA                       *
 *              94550               94550                               *
 *                                                                      *
 *              (510) 423-0145      (510) 422-7649                      *
 *                                                                      *
 *                                                                      *
 ************************************************************************
 */

/*
 ************************************************************************
 *                      Include Files                                   *
 ************************************************************************
 */
#include <stdio.h>

/*
 *      X-Library include files.
 */
#ifdef USEX11
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysym.h>
#endif

/*
 *      string.h - Include file for strings.
 *      types.h - Includes all systems types.
 *      stat.h - Include file for directory status.
 *      param.h - This file is intended to contain the basic
 *      specific details of a given architecture.
 */
#include <string.h>
#ifdef LINUX
#include <linux/stat.h>
#include <linux/param.h>
#include <linux/types.h>
#else
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/types.h>
#endif

#define num_index       6 /* Number of indices that VCS can have (i.e., I,J,K,L,M,N)

/* Keep track of animation windows as they appear on the screen */
struct animation_window_list {
        char                            animate_directory[1024];/*directory*/
        int anot_stop;                  /* Stops animation */
        int quit_animation;             /* Quit animation */
        int zoom_animation;             /* Zoom animation */
        int stop_from_tog;              /* Stops animation from tog button */
        int animation_colormap;         /* Use CDAT or raster image colormap*/
        int animation_mode;             /* Animate memory or disk images */
        int animation_direction;        /* Run animation forward or backward */
        int animation_speed; 		/* Speed of the animation */
	int file_frame_ct;		/* File frame counter */
        int animation_zoom;             /* Zoom animation panel value */
        int animation_hori, animation_vert;/*horizontal/vertical panel values*/
	int a_hori, a_vert;		/* computed horizontal/vertical values*/
        int xdwidth, ydheight;		/* stored canvas size */
        int sel_ct; 			/* count file selection */
	int animation_flg;		/* run from mem or disk */
	int create_animation_flg;	/* creating animation flag */
	int memory_ct; 			/* store number of frames */
	int frame_ct; 			/* count number of frame/sec */
	int garbaged; 			/* garbaged collected */
	time_t start;			/* animation start time */
        int canvas_id;			/* VCS Canvas ID */
        struct animation_window_list    *next;
};
typedef struct animation_window_list    ANIMATIONWINDOWLIST;
typedef ANIMATIONWINDOWLIST             *ANIMATIONWINDOWLIST_LINK;

struct animation_list_list {
        int                     xs;
};

/****************************************************************************
        END OF FILE
*****************************************************************************/
