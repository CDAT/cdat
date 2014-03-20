#ifndef VCS_EVENTS_H
#define VCS_EVENTS_H 1
#define MAX_NAME 1024
#include "ttffonts.h"
#include "array.h"
#include "picture.h"
#include "graph.h"
#include "pyvcs_legacy.h"

/* Begin section added by Jenny */
#ifdef __cplusplus
extern "C" {
#endif
struct data_point
{
    double  x;
    double  y;
    double  value;
    double  value2;
    int	    x_index;
    int	    y_index;
    int     color;
};
enum etypes {pe_text, pe_form, 
	     pe_x_tic, pe_y_tic, 
	     pe_x_lab, pe_y_lab, 
	     pe_box, pe_line, 
	     pe_leg, pe_dsp, pe_none,
	     display_tab};

union types 
{
  struct pe_text     *pet;
  struct pe_form     *pef;
  struct pe_x_tic    *pext;
  struct pe_y_tic    *peyt;
  struct pe_x_lab    *pexl;
  struct pe_y_lab    *peyl;
  struct pe_box      *peb;
  struct pe_line     *pel;
  struct pe_leg      *peleg;
  struct pe_dsp      *pedsp;
  struct display_tab *pd;
};

struct item_list
{
    enum etypes   type;
    union types  data;
    char attr_name[MAX_NAME];
    char display_name[MAX_NAME];
    struct p_tab    *ptab;
    struct Gextent extent;
    int x_reversed;
    int y_reversed;
    char string[nmax_char];
    int sub_primitive;
    struct item_list  *next;
};

extern float cnorm(PyVCScanvas_Object	*self,int x_or_y, float value);
extern float nnorm(PyVCScanvas_Object	*self,int x_or_y, float value);
extern struct item_list *select_item(PyVCScanvas_Object *self,
			      Gpoint point,
			      char *gui_template_name,
			      char *attr_name,
			      enum etypes search_only);
extern void draw_selection_box(Gextent extentin, int shadow);
extern void draw_reshape_dots(struct item_list *item, int shadow);
extern void remove_from_list(struct  item_list   *item, struct  item_list **selected_items);
extern void update_extent(Gpoint pointA, Gpoint pointB, int action, struct  item_list **selected_items);
#ifdef X11WM
extern void set_cursor(PyVCScanvas_Object *self,
		struct Gpoint position,
                Cursor cursor[],
                int  *action,
                struct item_list *selected_items);
// extern int switch_templates( PyVCScanvas_Object *self, struct item_list *selected_items);
// extern void setup_the_magnify_table();
// extern int undisplay_resize_plot(PyVCScanvas_Object *self);
// extern void display_resize_plot(PyVCScanvas_Object *self, int off);
#endif
/* void redraw_without_lines(); */
/* void redraw_overlapping(); */
extern void get_text_fields(char		shold[29][MAX_NAME],
		     char		sname[29][MAX_NAME],
		     char		fname[29][MAX_NAME],
		     char		xtname[29][MAX_NAME],
		     char		ytname[29][MAX_NAME],
		     char		xlname[29][MAX_NAME],
		     char		ylname[29][MAX_NAME],
		     char		bname[29][MAX_NAME],
		     char		lname[29][MAX_NAME],
		     struct a_tab *atab,
		     struct p_tab *ptab);

extern void zero_priority(struct item_list **selected_items);
extern PyObject *PyVCS_updateVCSsegments(PyVCScanvas_Object *self, PyObject *args);
extern PyObject *PyVCS_backing_store(PyVCScanvas_Object *self, PyObject *args);
extern void resize_or_move(PyVCScanvas_Object *self,
		    Gpoint pointA, 
		    Gpoint pointB,
		    int action,
		    struct  item_list **selected_items,
		    int arrow_move);
extern void verify_extent(struct item_list **selected_items);
extern void PyVCS_select_all_in_range(PyVCScanvas_Object *self,
			   struct item_list **hold_selected_items,
			   Gpoint pointA, 
			   Gpoint pointB);
/* void print_extent(extent) */
/* Gextent extent; */
/* { */
/*     printf("Lower left and right: (%f,%f) (%f,%f)\n",extent.ll.x,extent.ll.y,extent.lr.x, extent.lr.y); */
/*     printf("Upper left and right: (%f,%f) (%f,%f)\n",extent.ul.x,extent.ul.y,extent.ur.x, extent.ur.y); */
/* } */


extern int within(Gpoint point, Gextent extent);
extern int within_buffer(Gpoint point, Gextent extent, float buffer);
extern void append_to_list(struct  item_list *item, struct  item_list **selected_items);
extern void delete_list(struct  item_list  **selected_items);
extern void draw_selection_box(Gextent extentin, int shadow);
extern void draw_reshape_dots(struct item_list *item, int shadow);
extern void remove_from_list(struct  item_list   *item, struct  item_list **selected_items);
extern void update_extent(Gpoint pointA, Gpoint pointB, int action, struct  item_list **selected_items);
extern void delete_list(struct  item_list  **selected_items);

enum screen_mode_types {TEDITOR, DATA, ZOOM, GMEDITOR};

/* End section added by Jenny */

/* structure for popup window text */
typedef struct window_text
{
  char text[100];
  struct  window_text *next;
} window_text;

#ifdef __cplusplus
}
#endif
#endif
