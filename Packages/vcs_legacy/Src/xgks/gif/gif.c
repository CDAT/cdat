/*
*
*  This software was developed by the Thermal Modeling and Analysis
*  Project(TMAP) of the National Oceanographic and Atmospheric
*  Administration's (NOAA) Pacific Marine Environmental Lab(PMEL),
*  hereafter referred to as NOAA/PMEL/TMAP.
*
*  Access and use of this software shall impose the following
*  obligations and understandings on the user. The user is granted the
*  right, without any fee or cost, to use, copy, modify, alter, enhance
*  and distribute this software, and any derivative works thereof, and
*  its supporting documentation for any purpose whatsoever, provided
*  that this entire notice appears in all copies of the software,
*  derivative works and supporting documentation.  Further, the user
*  agrees to credit NOAA/PMEL/TMAP in any publications that result from
*  the use of this software or in any product that includes this
*  software. The names TMAP, NOAA and/or PMEL, however, may not be used
*  in any advertising or publicity to endorse or promote any products
*  or commercial entity unless specific written permission is obtained
*  from NOAA/PMEL/TMAP. The user also understands that NOAA/PMEL/TMAP
*  is not obligated to provide the user with any support, consulting,
*  training or assistance of any kind with regard to the use, operation
*  and performance of this software nor to provide the user with any
*  updates, revisions, new versions or "bug fixes".
*
*  THIS SOFTWARE IS PROVIDED BY NOAA/PMEL/TMAP "AS IS" AND ANY EXPRESS
*  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
*  ARE DISCLAIMED. IN NO EVENT SHALL NOAA/PMEL/TMAP BE LIABLE FOR ANY SPECIAL,
*  INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
*  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
*  CONTRACT, NEGLIGENCE OR OTHER TORTUOUS ACTION, ARISING OUT OF OR IN
*  CONNECTION WITH THE ACCESS, USE OR PERFORMANCE OF THIS SOFTWARE.  
*
*/



/*
 * GIF driver for XGKS metafiles
 * Created by Joe Sirott, Pacific Marine Environmental Lab
 * $Id$
 *
 * *js* 8.97 umalloc(0) returns 0 on DEC alpha; eliminated assert(meta->style) in
 *  set_lineStyle to avoid this problem, since length can be 0 
 * *js* 8.97 added clipping 
 * *js* 6.99 If resize is called with primitives, primitives are discarded 
 * (previously, error message was issued, and no resize occurred).
 */

/*
 *		Copyright IBM Corporation 1989
 *
 *                      All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of IBM not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 *
 *
 * University of Illinois at Urbana-Champaign
 * Department of Computer Science
 * 1304 W. Springfield Ave.
 * Urbana, IL	61801
 *
 * (C) Copyright 1987, 1988 by The University of Illinois Board of Trustees.
 * All rights reserved.
 *
 * Tool: X 11 Graphical Kernel System
 * Author: Gregory Scott Rogers
 * Author: Sung Hsien Ching Kelvin
 * Author: Yu Pan
 */

/*LINTLIBRARY*/
#ifdef __CYGWIN__
#include <stdlib.h>
#endif
#include <math.h>
#include <stdlib.h>
#include <time.h>		/* for time(), localtime(), and strftime() */
#include <sys/types.h>		/* for uid_t */
#include <sys/utsname.h>	/* for uname() */
#include <unistd.h>		/* for getlogin() */
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <stdarg.h>
#include <wchar.h>
#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include "gks_implem.h"
#include "cgm/cgm.h"		/* for public, API details */
#include "cgm/cgm_implem.h"		/* for implementation details */
#include "gif.h"
#include "gd.h"
#include "gdadds.h"


#ifndef lint
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id$";
#endif

static const int DefaultSize = 600;

/* Line styles */
static const int MaxLineStyleLength = 64;
static char *LineStyles[5] = {
  "",
  "",
  "---   ",
  "------      ",
  "------      ---   " 
};

/* Error messages are switched on and off through the environment */
/* variable XGKS_LOG anded with the enum MsgLog */

typedef enum MsgLog {
  INFO = 1,
  WARN = 2,
  ERR = 4
} MsgLog;

static int LogFlag = ERR;

static void initLogFlag()
{
  char *cp = getenv("XGKS_LOG");
  if (cp){
    LogFlag = atoi(cp);
  }
}

static void msgInfo(char *format, ...)
{
  if (LogFlag & INFO){
    va_list ap;
    va_start(ap, format);
    /* print out name of function causing error */
    fprintf(stderr, "XGKS(GIF): Info: ");
    /* print out remainder of message */
    vfprintf(stderr, format, ap);
    va_end(ap);
  }
}

static void msgWarn(char *format, ...)
{
  if (LogFlag & WARN){
    va_list ap;
    va_start(ap, format);
    /* print out name of function causing error */
    fprintf(stderr, "XGKS(GIF): Warning: ");
    /* print out remainder of message */
    vfprintf(stderr, format, ap);
    va_end(ap);
  }
}

static void msgErr(char *format, ...)
{
  if (LogFlag & ERR){
    va_list ap;
    va_start(ap, format);
    /* print out name of function causing error */
    fprintf(stderr, "XGKS(GIF): Error: ");
    /* print out remainder of message */
    vfprintf(stderr, format, ap);
    va_end(ap);
  }
}

/* GIF info */

typedef struct _GIFcolor {
  float r,g,b;
  int index;
} GIFcolor;

typedef struct _GIFMetaFile {
  gdImagePtr image;		/* GIF stuff */
  int sx, sy;			/* Image size */
  int numcolors;		/* Maximum number of colors */
  GIFcolor *colors;		/* XGKS->GIF index table */
  int lineIndex;		/* Current line color index */
  int lineWidth;		/* Current line width */
  int fillIndex;		/* Current fill color index */
  int styleIndex;		/* Current line style index */
  int *style;			/* Pointer to style array */
  int fillStyleIndex;		/* Current fill style index */
  int resize;			/* False => can't be resized */
  WS_STATE_PTR ws;		/* Workstation pointer */
} GIFmetafile;

/* Hacks to keep track of which GIFMetafile to use without */
/* changing GKS data structures. Oh, for a nice Java hashtable here... */
typedef struct Dict {
  mf_cgmo *key;
  GIFmetafile *value;
} Dict;

static struct {
  int count;
  Dict dict[MAX_ACTIVE_WS];
} GIFtable;
  
static void add_meta(mf_cgmo *key, GIFmetafile *value)
{
  Dict *mfp = &GIFtable.dict[GIFtable.count++];
  assert(GIFtable.count < MAX_ACTIVE_WS);
  mfp->key = key;
  mfp->value = value;
}

static int allocate_color(GIFmetafile *meta, int idx,
			  float r, float g, float b){
  GIFcolor *color = 0;
  int index;
  int ir = r * 255, ig = g * 255, ib = b * 255;
  assert(idx < meta->numcolors);

  color = &meta->colors[idx];
  if (color->index != -1)
    gdImageColorDeallocate(meta->image, color->index);
  color->index =
    gdImageColorAllocate(meta->image, ir, ig, ib);
  color->r = r; color->g = g; color->b = b;
  return color->index;
}

static void init_colors(GIFmetafile *meta, int size)
{
  int i;
  meta->numcolors = size;
  for (i=0; i < size; ++i){
    meta->colors[i].index = -1;
  }
}

static GIFmetafile *init_meta(mf_cgmo *cgmo, int sx, int sy)
{
  GIFmetafile *newmeta = (GIFmetafile *)umalloc(sizeof(GIFmetafile));
  assert(newmeta);

  newmeta->image = gdImageCreate(sx, sy);
  newmeta->sx = sx; newmeta->sy = sy;
  newmeta->colors =
    (GIFcolor *)umalloc(sizeof(GIFcolor) * cgmo->ws->wscolour);
  newmeta->lineIndex = 1;
  newmeta->lineWidth = 1;
  newmeta->fillIndex = 1;
  newmeta->styleIndex = 1;
  newmeta->fillStyleIndex = 1;
  newmeta->style = 0;
  newmeta->resize = 1;		/* OK to resize */
  newmeta->ws = cgmo->ws;
  init_colors(newmeta, cgmo->ws->wscolour);

  /* Allocate colors -- background white, foreground black */
  allocate_color(newmeta, 0, 1, 1, 1);
  allocate_color(newmeta, 1, 0, 0, 0);

  return newmeta;
}

static GIFmetafile *create_meta(mf_cgmo *cgmo, int sx, int sy)
{
  GIFmetafile *newmeta = init_meta(cgmo, sx, sy);

  assert(newmeta->image);
  add_meta(cgmo, newmeta);
  return newmeta;
}


static void remove_meta(mf_cgmo *key)
{
  int i,j;
  for(i=0; i < GIFtable.count; ++i){
    if (key == GIFtable.dict[i].key)
      break;
  }
  assert(i < GIFtable.count);
  for(j=i+1; j < GIFtable.count; ++j){
    Dict *p = &GIFtable.dict[j], *q = p-1;
    q->key = p->key;
    q->value = p->value;
  }
  --GIFtable.count;
}

static GIFmetafile *find_meta(mf_cgmo *key)
{
  int i;
  for(i=0; i < GIFtable.count; ++i){
    if (key == GIFtable.dict[i].key)
      break;
  }
  if (i < GIFtable.count){
    return GIFtable.dict[i].value;
  } else
    return 0;
}

static void destroy_meta(mf_cgmo *cgmo, GIFmetafile *meta)
{
  remove_meta(cgmo);
  if (meta->image)
    gdImageDestroy(meta->image);
  free(meta->colors);
  free(meta->style);
  free(meta);
}

/* Copy the attributes of a GIF metafile to a newly created metafile */
/* Used for resizing */

static GIFmetafile *copy_meta(mf_cgmo *cgmo, GIFmetafile *old,
			      int sx, int sy)
{
  GIFmetafile *newmeta = init_meta(cgmo, sx, sy);
  int i;

  /* Copy various stuff -- don't copy resize! */
  newmeta->lineIndex = old->lineIndex;
  newmeta->lineWidth = old->lineWidth;
  newmeta->fillIndex = old->fillIndex;
  newmeta->styleIndex = old->styleIndex;
  newmeta->fillStyleIndex = old->fillStyleIndex;
  newmeta->style = (int *)umalloc(sizeof(int) * MaxLineStyleLength);
  memcpy(newmeta->style, old->style, sizeof(int)*MaxLineStyleLength);
  newmeta->ws = old->ws;

  /* Copy the color info */
  for (i=0; i < old->numcolors; ++i){
    GIFcolor *color = &old->colors[i];
    if (old->colors[i].index == -1)
      break;
    allocate_color(newmeta, i, color->r, color->g, color->b);
  }
  /* Destroy the old metafile */
  destroy_meta(cgmo, old);
  /* Add this one */
  add_meta(cgmo, newmeta);
  return newmeta;
}
/* Coordinate transformation from NDC->device */

static void xform(GIFmetafile *meta, float x, float y, int *xp, int *yp)
{
  static int margin = 0;
  int maxdim = meta->sx > meta->sy ? meta->sx : meta->sy;
  maxdim -= 2 * margin;
  *xp = x * maxdim + margin;
  *yp = meta->sy - y * maxdim - margin;
}



/* Set up line styles. GIF metafiles at the moment */
/* only support the required GKS types of dashed(2), dotted(3) */
/* and dash/dot (4). All others default to solid */

static void set_lineStyle(GIFmetafile *meta, Gint attr, Gasf type)
{
  if (type != xgks_state.gks_lnattr.type)
    return;
  if (type == GBUNDLED){
    attr = meta->ws->lnbundl_table[attr].type;
  }
  msgInfo("set_lineStyle: setting style to %d\n", attr);
  if (attr <= 0 || attr > 4)
    attr = 1;
  meta->styleIndex = attr;
  {
    int currColor = meta->lineIndex;
    int i, length;
    char *cp = LineStyles[attr];
    length = strlen(cp);
    assert(currColor != -1);
    if (meta->style)
      free(meta->style);
    meta->style = (int *)umalloc(sizeof(int) * MaxLineStyleLength);
    length = strlen(LineStyles[attr]);
    for (i=0; i < length; ++i){
      if (cp[i] == '-'){
	meta->style[i] = currColor;
      } else {
	meta->style[i] = gdTransparent;
      }
    }
     gdImageSetStyle(meta->image, meta->style, length); 
  } 
}

static void
set_lineWidth(GIFmetafile *meta, double size, Gint attr, Gasf type)
{
  if (type != xgks_state.gks_lnattr.width)
    return;
  if (type == GBUNDLED){
    int index = attr;
    size = meta->ws->lnbundl_table[index].width;
  }
  msgInfo("set_lineWidth: setting width to %lf\n", size);
  if (size < 1.0)
    size = 1.0;
  meta->lineWidth = size +.5;
}

static void set_fillColor(GIFmetafile *meta, Gint attr)
{
  GIFcolor *color = &meta->colors[attr];
  meta->fillIndex = color->index;
  msgInfo("set_fillColor: setting color to (%f,%f,%f)\n",
	  color->r, color->g, color->b);
}

static void set_fillStyle(GIFmetafile *meta, Gint attr)
{
  if (attr < 0 || attr > 1){
    msgWarn("set_fillStyle: Unsupported fill type %d\n", attr);
    attr = 1;
  }
  meta->fillStyleIndex = attr;
}

static void set_lineColor(GIFmetafile *meta, Gint attr, Gasf type)
{
  if (type != xgks_state.gks_lnattr.colour)
    return;
  if (type == GBUNDLED){
    attr = meta->ws->lnbundl_table[attr].colour;
  }
  {
    GIFcolor *color = &meta->colors[attr];
    msgInfo("set_lineColor: setting color to (%f,%f,%f)\n",
	    color->r, color->g, color->b);
    meta->lineIndex = color->index;
    set_lineStyle(meta, meta->styleIndex, type);
  }
}

static void unsupported(char *message, int attr)
{
  msgWarn("%s: Unsupported call -- argument %d\n", message, attr);
}

/*
 * Return a string identifying the user and installation.
 */
    static Gchar*
XgksMAuthor(void)
{
    char		*username	= getlogin();
    struct utsname	name;
    static Gchar	buffer[41];

    buffer[0]	= 0;

    if (username != NULL)
	(void) strncat(buffer, username, sizeof(buffer) - 1);

    if (uname(&name) != -1) {
	int	nchr	= strlen(buffer);

	if (nchr < sizeof(buffer) - 1) {
	    buffer[nchr++]	= '@';
	    (void) strncpy(buffer + nchr, name.nodename, 
			   sizeof(buffer) - nchr - 1);
	}
    }

    return buffer;
}


/*
 * Return a date-string.
 */
    static Gchar*
XgksMDate(void)
{
    time_t          clock = time((time_t *) NULL);
    static Gchar    date[9];

    (void) strftime(date, (size_t)sizeof(date), "%y/%m/%d", 
                    localtime(&clock));

    return date;
}


/*
 * Open an output GIF file.
 */
    int
GIFmoOpen(WS_STATE_PTR ws)
{
  /* Output file not opened or written to until flushed */

    Gint status = 1;
    initLogFlag();
    assert(ws != NULL);
    assert(ws->wscolour <= gdMaxColors);
    if ((ws->mf.cgmo = (mf_cgmo*)umalloc(sizeof(mf_cgmo))) != NULL) {
	mf_cgmo	*cgmo	= ws->mf.cgmo;
	cgmo->ws = ws;

	if (find_meta(cgmo) != 0){
	  msgWarn("GIFmoOpen:metafile already open\n");
	} else {
	  create_meta(cgmo, DefaultSize, DefaultSize);
	  
	  cgmo->ws = ws;
	  cgmo->type = MF_GIF;
	  unlink(ws->conn);
	  status = OK;
	}
    }
    return status;
}


/*
 * Close an output GIF file.
 */
    int
GIFmoClose(Metafile *mf)
{
  int status = 1;		/* return status error */
  if (mf != NULL && mf->cgmo != NULL) {
    mf_cgmo *cgmo	= mf->cgmo;
    GIFmetafile *meta = find_meta(cgmo);
    status = GIFFlush(mf, meta->ws->conn);

    if (meta != 0){
      destroy_meta(cgmo, meta);
    }

    ufree((voidp)mf->cgmo);
    mf->cgmo	= NULL;
  }

  return status;
}

int GIFFlush(Metafile *mf, char *filename)
{
  mf_cgmo *cgmo	= mf->cgmo;
  GIFmetafile *meta = find_meta(cgmo);
  int status = 1;

  /* File w/ ".gif" only are not created (Ferret hack) */
  if (strcasecmp(filename, ".gif") != 0){
    FILE *fp = fopen(filename, "w");
    if (fp != NULL && meta != 0){
      status = OK;
      gdImageGif(meta->image, fp);
      fclose(fp);
    }
  } else {
    status = OK;
  }
  return status;
}

void GIFresize(WS_STATE_PTR ws, Gpoint size)
{
  mf_cgmo *cgmo = ws->mf.cgmo;
  GIFmetafile *meta = find_meta(cgmo);
  assert(meta);
#if 0
  if (meta->resize == 0){
    msgErr("GIFresize: metafile contains primitives -- can't resize\n");
  } else {
    copy_meta(cgmo, find_meta(cgmo), size.x, size.y);
    msgInfo("GIFresize: Resizing to (%f,%f)\n", size.x, size.y);
  }
#endif
  if (meta->resize == 0){
    msgWarn("GIFresize: metafile contains primitives that will be lost when resizing\n");
  } 
  copy_meta(cgmo, find_meta(cgmo), size.x, size.y);
  msgInfo("GIFresize: Resizing to (%f,%f)\n", size.x, size.y);
}

/*
 * Set the clear flag in an output GIF file.
 */
    int
GIFclear(Metafile *mf, int num, Gclrflag flag)
{
    int		imf;
    mf_cgmo		**cgmo	= &mf->cgmo;
    for (imf = 0; imf < num; ++imf) {
	Gint	i;
	GIFmetafile *meta = find_meta(cgmo[imf]);
	GIFcolor *color;
	assert(meta);
	meta->resize = 1;	/* OK to resize */
	color = &meta->colors[0];
	gdImageBlockFill(meta->image, color->index);
    }
    return OK;
}


/*
 * Redraw all segments in an output GIF file.
 */
    int
GIFredrawAllSeg(Metafile **mf, int num)
{
				/* Noop */
    msgWarn("GIFredrawAllSeg: Don't support this feature\n");
    return OK;
}


/*
 * Set the update flag in an output GIF file.
 */
     int
GIFupdate(Metafile **mf, int num, Gregen regenflag)
{
				/* Noop */
    msgWarn("GIFupdate: Don't support this feature\n");
    return OK;
}


/*
 * Set the deferal state in an output GIF file.
 */
    int
GIFdefer(Metafile **mf, int num, Gdefmode defer_mode, Girgmode regen_mode)
{
				/* Noop */
    msgWarn("GIFdefer: Don't support this feature\n");
    return OK;
}


/*
 * Write a message to an output GIF file.
 */
    int
GIFmessage(Metafile **mf, int num, Gchar *string)
{
				/* Noop */
    msgWarn("GIFmessage: Don't support this feature\n");
    return OK;
}

/*
 * Write a graphic to output GIF files.
 *
 * This routine is suitable for
 *
 *	POLYLINE    -- code == 11
 *	POLYMARKER  -- code == 12
 *	FILLAREA    -- code == 14
 */
/* RETURN HERE ****************/

static int isColinear(gdPointPtr first, gdPointPtr second, gdPointPtr third){
  int dx1 = second->x - first->x;
  int dx2 = third->x - second->x;
  int dy1 = second->y - first->y;
  int dy2 = third->y - second->y;
  return (dy2*dx1 - dx2*dy1) == 0;
}

static void stripColinear(gdPointPtr tpts, int *num_pts){
  int total = 0;
  int i;
  gdPointPtr npts, first, second;
  if (*num_pts <= 2){
    return;
  }
  npts = (gdPointPtr)umalloc(sizeof(gdPoint) * *num_pts);
  assert(npts);
  first = tpts;
  second = tpts+1;
  for (i=2; i < *num_pts; ++i){
    gdPointPtr third = &tpts[i];
    if (isColinear(first, second, third)){
      second = third;
    } else {
      npts[total].x = first->x;
      npts[total].y = first->y;
      ++total;
      first = second;
      second = third;
    }
  }
  npts[total].x = first->x;
  npts[total].y = first->y;
  ++total;
  npts[total].x = second->x;
  npts[total].y = second->y;
  ++total;

  memcpy(tpts, npts, sizeof(gdPoint)*total);

  free(npts);
  *num_pts = total;

#ifdef DEBUG
  printf("--------------------------------------\n");
  for (i=0; i < total; ++i){
    printf("strip: (%d, %d)\n", tpts[i].x, tpts[i].y);
  }
#endif

}

    int
GIFoutputGraphic(Metafile *mf, int num, Gint code, Gint num_pt, Gpoint *pos)
{
    int		imf;
    mf_cgmo		**cgmo	= &mf->cgmo;
    for (imf = 0; imf < num; ++imf) {
	Gint	i;
	GIFmetafile *meta = find_meta(cgmo[imf]);
	assert(meta);
	assert(num_pt > 0);
	meta->resize = 0;	/* Not OK to resize */


	/*
	  Trivial accept and reject clipping of points.
	*/
	{
	  int xlo = 1, xhi = 1, yhi = 1, ylo = 1;
	  Gfloat lolimit = -1, hilimit = 2;
	  int i;
	  for (i=1; i < num_pt; ++i){
	    float x = pos[i].x; 
	    float y = pos[i].y;
	    xlo &= (x < lolimit);
	    xhi &= (x > hilimit);
	    ylo &= (y < lolimit);
	    yhi &= (y > hilimit);
	  }
	  if (xlo || xhi || ylo || yhi)
	    return OK;
	    
	  /* If any points are extreme, toss the polygon. Someday,
             there will be a real clipping algorithm */
	  for (i=1; i < num_pt; ++i){ 
	    float x = pos[i].x; 
	    float y = pos[i].y; 
	    if (x < -10. || x > 10. || y < -10 || y > 10){ 
	      msgWarn("GIFoutputGraphic: Bad NDC point %f %f\n", 
		      x, y); 
	      return OK; 
	    } 
	  }
	}
	

	switch(code){
	case GKSM_FILL_AREA:
	  {
	    gdPointPtr tpts = (gdPointPtr)umalloc(sizeof(gdPoint) * num_pt);
	    assert(tpts);
	    xform(meta, pos->x, pos->y, &tpts[0].x, &tpts[0].y);
#ifdef DEBUG
	    printf("--------------------------------------\n");
	    printf("Transform: (%f, %f) -> (%d,%d)\n",
		pos->x, pos->y, tpts[0].x, tpts[0].y);   
#endif
	    ++pos;
	    for (i=1; i < num_pt; ++i, ++pos){
	      xform(meta, pos->x, pos->y, &tpts[i].x, &tpts[i].y);
#ifdef DEBUG
	      printf("Transform: (%f, %f) -> (%d,%d)\n",
		     pos->x, pos->y, tpts[i].x, tpts[i].y);   
#endif
	    }
	    if (meta->fillStyleIndex == 0){
	      gdImagePolygon(meta->image, tpts, num_pt, meta->fillIndex);
	    } else {
				/* Eliminate colinear points (bug in gd.c) */
	      stripColinear(tpts, &num_pt);
	      gdImageFilledPolygon(meta->image, tpts, num_pt,
				   meta->fillIndex);
	    }
	    free(tpts);
	  }
	break;
	case GKSM_POLYLINE:
	  {
	    int lineIndex = meta->styleIndex == 1 ? meta->lineIndex 
	      : gdStyled; 
	    gdPointPtr tpts = (gdPointPtr)umalloc(sizeof(gdPoint) * num_pt);
	    assert(tpts);
	    for (i=0; i < num_pt; ++i, ++pos){
	      xform(meta, pos->x, pos->y, &tpts[i].x, &tpts[i].y);
	    }
	    gdImageWideLines(meta->image, tpts, num_pt,
			     lineIndex, meta->lineWidth);

	    free(tpts);
	  }
	break;
	case GKSM_POLYMARKER:
	  msgWarn("GIFoutputGraphics: polymarker unsupported\n");
	  break;
	default:
	  msgWarn("GIFoutputGraphics: Unknown code %d\n", code);
	}
    }
    return OK;
}


/*
 * Write text to an output GIF file. Unsupported
 */
    int
GIFtext(Metafile *mf, int num, Gpoint *at, Gchar *string)
{
    msgWarn("GIFtext: Don't support this feature\n");
    return OK;
}

/*
 * Write a cell array to an output GIF file.
 * Unsupported
 */

    int
GIFcellArray(Metafile *mf, int num, Gpoint *ll, Gpoint *ur, Gpoint *lr, Gint row, Gint *colour, Gipoint *dim)
{
	int     dstX, dstY, sumX, sumY, xx2, yy2, c;
    int		lx, ly, index, imf, srcW, srcH, dstW, dstH, npts;
	int     x, y;
	/* Stretch vectors; code from gdImageCopyResized */
	int *stx;
	int *sty;
	double accum;
	int tox, toy;
	int i, j, p;

    mf_cgmo		**cgmo	= &mf->cgmo;
    for (imf = 0; imf < num; ++imf) {
	Gint	i;
	GIFmetafile *meta = find_meta(cgmo[imf]);
	assert(meta);
	assert(num_pt > 0);
	meta->resize = 0;	/* Not OK to resize */


	xform(meta, ll->x, ll->y, &dstX, &yy2);
	xform(meta, ur->x, ur->y, &xx2, &dstY);

	dstW = xx2 - dstX;
	dstH = yy2 - dstY;
	srcW = dim->x;
	srcH = dim->y; 
	npts = srcW * srcH; 


	/* More code from gdImageCopyResized. 
	   We only need to use floating point to determine the correct
	   stretch vector for one line's worth. */
	stx = (int *) malloc(sizeof(int) * srcW);
	sty = (int *) malloc(sizeof(int) * srcH);
	accum = 0;
	for (i=0; (i < srcW); i++) 
	{
		int got;
		accum += (double)dstW/(double)srcW;
		got = floor(accum);
		stx[i] = got;
		accum -= got;
	}
	accum = 0;
	for (i=0; (i < srcH); i++) 
	{
		int got;
		accum += (double)dstH/(double)srcH;
		got = floor(accum);
		sty[i] = got;
		accum -= got;
	}

	index = 0;
	c = colour[index];
	toy = dstY;
	sumY = dstY;

	for (y=0; (y < srcH); y++)                      /* cells in y */
	{
		    tox = dstX;
		    sumX = dstX;
			for (x=0; (x < srcW); x++)              /* cells in y */
			{
			    toy = sumY;
				for (j=0; (j < sty[y]); j++)        /* image pixels in y */
	            {   
				    p = j;
					tox = sumX;
					for (i=0; (i < stx[x]); i++)    /* image pixels in x */
			        {
					   p = i;
					   gdImageSetPixel(meta->image, tox, toy, c);
				       tox++;
				    }
				   toy++;
				}
				index++;
				c = 1;
				if (index < npts) 
				{
				c = colour[index];
				}
				sumX = sumX + stx[x];
		   }
		   sumY = sumY + sty[y];
	}


	free(stx);
	free(sty);
    }
		
	return OK;
}


/*
 * Set the size of graphics in an output GIF file.
 */
    int
GIFsetGraphSize(Metafile *mf, int num, Gint code, double size)
{
    int imf;
    mf_cgmo		**cgmo	= &mf->cgmo;

    for (imf = 0; imf < num; ++imf){
      GIFmetafile *meta = find_meta(cgmo[imf]);
      assert(meta);
      switch(code){
      case GKSM_LINEWIDTH_SCALE_FACTOR:
	set_lineWidth(meta, size, 0, GINDIVIDUAL);
	break;
      case GKSM_CHARACTER_EXPANSION_FACTOR:
      case GKSM_MARKER_SIZE_SCALE_FACTOR:
      case GKSM_CHARACTER_SPACING:
	return OK;		/* Ignore */
      default:
	msgWarn("GIFsetGraphSize: Unknown code %d\n", code);
	return OK;
      }
    }
    return OK;
}


/*
 * Close a segment in an output GIF file.
 */
    int
GIFcloseSeg(Metafile *mf, int num)
{
				/* Noop */

    msgWarn("GIFcloseSeg: Don't support this feature\n");
    return OK;
}


/*
 * Set the graphic attributes in an output GIF file.
 */

int
GIFsetGraphAttr(Metafile *mf, int num, Gint code, Gint attr)
{
  int		imf;
  mf_cgmo		**cgmo	= &mf->cgmo;
  for (imf = 0; imf < num; ++imf) {
    GIFmetafile *meta = find_meta(cgmo[imf]);
    assert(meta);
    switch(code){
    case GKSM_POLYLINE_INDEX:
      set_lineColor(meta, attr, GBUNDLED);
      set_lineWidth(meta, 0.0, attr, GBUNDLED);
      set_lineStyle(meta, attr, GBUNDLED);
      break;
    case GKSM_POLYLINE_COLOUR_INDEX:
      set_lineColor(meta, attr, GINDIVIDUAL);
      break;
    case GKSM_LINETYPE:
      set_lineStyle(meta, attr, GINDIVIDUAL);
      break;
    case GKSM_TEXT_COLOUR_INDEX:
      unsupported("GIFsetGraphAttr : GKSM_TEXT_COLOUR_INDEX", attr);
      break;
    case GKSM_FILL_AREA_COLOUR_INDEX:
      set_fillColor(meta, attr);
      break;
    case GKSM_FILL_AREA_STYLE_INDEX:
      set_fillStyle(meta, attr);
      break;
    case GKSM_POLYMARKER_COLOUR_INDEX:
      unsupported("GIFsetGraphAttr : GKSM_POLYMARKER_COLOUR_INDEX", attr);
      break;
    case GKSM_FILL_AREA_INDEX:
      unsupported("GIFsetGraphAttr : GKSM_FILL_AREA_INDEX", attr);
      break;
    case GKSM_MARKER_TYPE:
      unsupported("GIFsetGraphAttr : GKSM_MARKER_TYPE", attr);
      break;
    case GKSM_POLYMARKER_INDEX:
      unsupported("GIFsetGraphAttr : GKSM_POLYMARKER_INDEX", attr);
      break;
    case GKSM_PICK_IDENTIFIER:
      unsupported("GIFsetGraphAttr : GKSM_PICK_IDENTIFIER", attr);
      break;
    case GKSM_TEXT_INDEX:
      unsupported("GIFsetGraphAttr : GKSM_TEXT_INDEX", attr);
      break;
    default:
      msgWarn("GIFsetGraphAttr: Unknown code %d\n", code);
    }
  }
  return OK;
}


/*
 * Set the font precision in an output GIF file.
 * Unsupported
 */
    int
GIFsetTextFP(Metafile *mf, int num, Gtxfp *txfp)
{
    msgWarn("GIFsetTextFP: Don't support this feature\n");
    return OK;
}

#define HYPOT(x,y) sqrt((double)((x)*(x) + (y)*(y)))


/*
 * Set the character up-vector and character height in an output GIF file.
 * Unsupported.
 */
    int
GIFsetCharUp(Metafile *mf, int num, Gpoint *up, Gpoint *base)
{
    msgWarn("GIFsetCharUp: Don't support this feature\n");
    return OK;
}


/*
 * Set the text-path in an output GIF file.
 * Unsupported
 */
    int
GIFsetTextPath(Metafile *mf, int num, Gtxpath path)
{
    msgWarn("GIFsetTextPath: Don't support this feature\n");
    return OK;
}


/*
 * Set the text-alignment in an output GIF file.
 * Unsupported
 */
    int
GIFsetTextAlign(Metafile *mf, int num, Gtxalign *align)
{
    msgWarn("GIFsetTextAlign: Don't support this feature\n");
    return OK;
}


/*
 * Set the interior fill-style in an output GIF file.
 * Unsupported
 */
    int
GIFsetFillStyle(Metafile *mf, int num, Gflinter style)
{
    int		imf;
    mf_cgmo		**cgmo	= &mf->cgmo;

    for (imf = 0; imf < num; ++imf) {
      GIFmetafile *meta = find_meta(cgmo[imf]);
      assert(meta);
      set_fillStyle(meta, style);
    }
    return OK;
}


/*
 * Set the pattern size in an output GIF file.
 */
    int
GIFsetPatSize(Metafile *mf, int num)
{
    msgWarn("GIFsetPatSize: Don't support this feature\n");
    return OK;
}


/*
 * Set the pattern reference-point in an output GIF file.
 */
    int
GIFsetPatRefpt(Metafile *mf, int num)
{
    msgWarn("GIFsetPatRefPt: Don't support this feature\n");
    return OK;
}


/*
 * Set the ASF in an output GIF file.
 */
    int
GIFsetAsf(Metafile *mf, int num)
{
  /* This is handled in the graphics generating code */
    return OK;
}


/*
 * Set the line and marker representation in an output GIF file.
 */
    int
GIFsetLineMarkRep(Metafile *mf, int num, Gint code, Gint idx, Gint type, double size, Gint colour)
{
    msgWarn("GIFsetLineMarkRep: Don't support this feature\n");
    return OK;
}


/*
 * Set the text representation in an output GIF file.
 */
    int
GIFsetTextRep(Metafile *mf, int num, Gint idx, Gtxbundl *rep)
{
    if (!rep)
      return 1;
    msgInfo("GIFsetTextRep: index %d font prec (%d %d) exp %.6f sp %.6f color %d\n",
	    idx, rep->fp.font, rep->fp.prec,
	    rep->ch_exp, rep->space, rep->colour);
    return OK;
}


/*
 * Set the fill representation in an output GIF file.  
 */
    int
GIFsetFillRep(Metafile *mf, int num, Gint idx, Gflbundl *rep)
{
    msgWarn("GIFsetFillRep: Don't support this feature\n");
    return OK;
}


/*
 * Set the pattern representation in an output GIF file.
 */
    int
GIFsetPatRep(Metafile *mf, int num, Gint idx, Gptbundl *rep)
{
    msgWarn("GIFsetPatRep: Don't support this feature\n");
    return OK;
}


/*
 * Set the colour representation in an output GIF file.
 */
#ifndef MAX
#define MAX(a,b)    (((a)>(b))?(a):(b))
#endif

    int
GIFsetColRep(Metafile *mf, int num, Gint idx, Gcobundl *rep)
{
    Gint status = 1;
    int		imf;
    mf_cgmo		**cgmo	= &mf->cgmo;

    for (imf = 0; imf < num; ++imf) {
      float r = rep->red, g = rep->green, b = rep->blue;
      GIFmetafile *meta = find_meta(cgmo[imf]);
      int gindex;
      assert(meta);

      /* Do the color allocation -- background color */
      /* can't be reset */
      if (idx > 0 && (gindex = allocate_color(meta, idx, r, g, b)) == -1){
	msgWarn("GIFsetColRep : Can't allocate color (%f, %f, %f)\n",
	     r, g, b);
      } else {
	msgInfo("GIFsetColRep: assigning color (%f, %f, %f) to GKS %d GIF %d\n",
		r, g, b, idx, gindex);

	status = OK;
      }
    }

    return status;
}


/*
 * Set the clipping rectangle in an output GIF file.
 */
    int
GIFsetClip(Metafile *mf, int num, Glimit *rect)
{
    mf_cgmo *cgmo	= mf->cgmo;
    GIFmetafile *meta = find_meta(cgmo);
    int x1,y1,x2,y2;
    xform(meta, rect->xmin, rect->ymin, &x1, &y1);
    xform(meta, rect->xmax, rect->ymax, &x2, &y2);
    gdSetClip(meta->image, x1,y1,x2,y2);

    return OK;
}


/*
 * Set the viewport limits in an output GIF file.
 */
    int
GIFsetLimit(Metafile *mf, int num, Gint code, Glimit *rect)
{
    msgWarn("GIFsetLimit: Don't support this feature\n");
    return OK;
}


/*
 * Rename a segment in an output GIF file.
 */
    int
GIFrenameSeg(Metafile *mf, int num, Gint old, Gint new)
{
    msgWarn("GIFrenameSeg: Don't support this feature\n");
    return OK;
}

/*
 * Set the segment transformation in an output GIF file.
 */
    int
GIFsetSegTran(Metafile *mf, int num, Gint name, Gfloat (*matrix)[])
{
    msgWarn("GIFsetSegTran: Don't support this feature\n");
    return OK;
}


/*
 * Set the segment attributes in an output GIF file.
 */
    int
GIFsetSegAttr(Metafile *mf, int num, Gint name, Gint code, Gint attr)
{
    msgWarn("GIFsetSegAttr: Don't support this feature\n");
    return OK;
}


/*
 * Set the segment visibility in an output Metafile.
 */
    int
GIFsetSegVis(Metafile *mf, int num, Gint name, Gsegvis vis)
{
    msgWarn("GIFsetSegVis: Don't support this feature\n");
    return OK;
}


/*
 * Set segment highlighting in an output GIF file.
 */
    int
GIFsetSegHilight(Metafile *mf, int num, Gint name, Gseghi hilight)
{
    msgWarn("GIFsetSegHilight: Don't support this feature\n");
    return OK;
}


/*
 * Set segment priority in an output GIF file.
 */
    int
GIFsetSegPri(Metafile *mf, int num, Gint name, double pri)
{
    msgWarn("GIFsetSegPri: Don't support this feature\n");
    return OK;
}

int
GIFsetSegDetect(Metafile *mf, int num, Gint name, Gsegdet det)
{
    msgWarn("GIFsetSegDetect: Don't support this feature\n");
    return OK;
}
