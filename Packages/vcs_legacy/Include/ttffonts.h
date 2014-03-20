/* Structures to hold/load freetype fonts for VCS */

/* Created by Charles Doutriaux on May 8th, 2007 */
#include "vcs_legacy_names_length.h"
/* #ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif */
/* #include <stdlib.h> */
/* #include <time.h>		/\* for time(), localtime(), and strftime() *\/ */
/* #include <sys/types.h>		/\* for uid_t *\/ */
/* #include <unistd.h>		/\* for getuid() & getlogin() *\/ */
/* #include <string.h> */
/* #include <math.h> */
/* #include <ctype.h> */
/* #include <assert.h> */
/* #include "gks_implem.h" */
/* #include "gksm.h" */
#include "gks.h"
/* #include "gksshort.h" */
#include <ft2build.h>
#include FT_FREETYPE_H 
/* #include FT_GLYPH_H */
/* #include FT_STROKER_H */
/* #include FT_TRIGONOMETRY_H */
/* #include FT_OUTLINE_H */

#define nmax_char 500
#define nmax_outline_pts 5000
#define MAX_FONTS 500

struct table_FT_VCS_FONTS {
  char name[VCS_MX_NM_LEN];
  char path[2000];
  int index;
  int loaded;
  struct  table_FT_VCS_FONTS *next;
};

 typedef struct outline_head_ {
  Gpoint outline_points[nmax_outline_pts];
  int orientation;
  FT_Vector delta; /* advance in same units as kerning from first character */
  Gpoint start; /* where in gks units we start drawing */
  int sscript;
 } outline_head;

typedef  struct string_def_
{
  FT_Face face;
  FT_Outline ftglyph[nmax_char];
  FT_Vector deltas[nmax_char];
  int num_char;
  int error,errors[nmax_char];
  char text[nmax_char];
  int char_level[nmax_char];
  FT_Vector total; /* total height and width (y/x) */
 } string_def;
