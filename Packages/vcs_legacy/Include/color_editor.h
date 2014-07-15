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
 *      File name: color_editor.h                                       *
 *                                                                      *
 *                                                                      *
 *      Langague: C                                                     *
 *                                                                      *
 *                                                                      *
 *      Comments:                                                       *
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
#include <stdio.h>

/*
 *      string.h - Include file for strings.
 *      types.h - Includes all systems types.
 *      stat.h - Include file for directory status.
 *  	param.h - This file is intended to contain the basic
 * 	specific details of a given architecture.
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
 */
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/types.h>


#define NUM_COLORS      256
#define MAX_COLOR_INDEX 255
#define MAX_LINE_STRING 128
#define MAX_PATH_LEN    1024

#define INDEX_WIDTH     400 /* Width and height values for the color indices */
#define INDEX_HEIGHT    400
#define SPECTRUM_WIDTH   40 /* Width and height values for the color spectrum */
#define SPECTRUM_HEIGHT 400
#define VIEW_HEIGHT      60
#define VIEW_WIDTH       60
#define INDEX_TO_START    0 /* change to 0 to start at index 0 */
#define INDEX_TO_END    239 /* change to 255 to end at index 255 */

#define BLACK           241
#define WHITE           240
#define RED             242
#define GREEN           243
#define BLUE            244
#define YELLOW          245
#define CYAN            246
#define MAGENTA         247
#define ORANGE          248
#define BROWN		249
#define VIOLET		250
#define OLIVE_GREEN     251
#define GREY            252
#define LIGHT_GREEN	253
#define TEMPLATE_COLOR_FG	BLACK
#define TEMPLATE_COLOR_BG	GREEN
#define DATA_COLOR_FG		WHITE
#define DATA_COLOR_BG		RED
#define GRAPH_COLOR_FG		WHITE
#define GRAPH_COLOR_BG		BLUE
#define LIST_COLOR_FG		BLACK
#define LIST_COLOR_BG		YELLOW
#define LINE_COLOR_FG		BLACK
#define LINE_COLOR_BG		CYAN
#define TEXTT_COLOR_FG		WHITE
#define TEXTT_COLOR_BG		MAGENTA
#define TEXTO_COLOR_FG		WHITE
#define TEXTO_COLOR_BG		ORANGE
#define MARKER_COLOR_FG		WHITE
#define MARKER_COLOR_BG         BROWN
#define FILLAREA_COLOR_FG	WHITE
#define FILLAREA_COLOR_BG	OLIVE_GREEN
#define FORMAT_COLOR_FG   	BLACK
#define FORMAT_COLOR_BG   	LIGHT_GREEN

#define FOREGROUND_INDEX      241 
#define BACKGROUND_INDEX        2 
#define BOTTOMSHADOW_INDEX      3
#define TOPSHADOW_INDEX         4
#define	ICON_INDEX		5

#define FOREGROUND_TEXT		BLACK	
#define BACKGROUND_TEXT	 	254
#define FOREGROUND_LIST		BLACK
#define BACKGROUND_LIST		LIGHT_GREEN

/***************************************************************************
        END OF FILE
****************************************************************************/
