#include "vcs_legacy_events.h"
#include "display.h"
#include "cdms.h"
#include "project.h"
#include "gksshort.h"

typedef struct level_fill {int color; float l1,l2;} S_boxfill;
extern "C" int set_bfills(struct gfb_attr *pGfb,struct a_attr *pA, S_boxfill *regis,int *num);
extern "C" float plnorm(int x_or_y, float value);

extern "C" int isInsidePolygon(float X, float Y, int n,float *XS,float *YS);
extern "C" float getArrayValueAsFloat(struct a_attr *,int);
extern "C" int nice15(float a,float b,float *dr,int *pw10,float *center);

extern "C" void cdRel2Comp(cdCalenType timetype, char* relunits, double reltime, cdCompTime* comptime);
extern "C" int err_warn (int beep,FILE *fp,char *fmt,...);
extern "C" void gsetlinetype(int type);
extern "C" void gsetfillintstyle(int s);
extern "C" void gsetfillcolourind(int c);
extern "C" void gsetlinewidth(float w);
extern "C" void gsetlinecolourind(int c);
extern "C" void gpolyline(int n,Gpoint *pts);
extern "C" void gfillarea(int n,Gpoint *pts);
extern "C" void replace_boxfill_data_f(float *pdat,short *pmask,int *pdim[],int nd,float dmin,float dmax,int type,char *legend,struct fill_range *values,int *color_num,int *data_num);
extern "C" int mmmm(float *pdat,short *pmask,int *pdim[],float *pw[],int nd,float *XC[],float *xv[],float *pmin,float *pmax,float *pmean);

extern "C"     void set_viewport_and_worldcoordinate ( 
						      float vp[],/* view port.                    */
						      float wc_in[], /* world coordinates.            */
						      char proj[256]
						       );
extern "C" Gpoint proj_convert(Gpoint pxy);
extern "C" Gpoint invert_proj_convert(Gpoint pxy);
extern "C" int gctp_conv( double xin, double yin,double *xout, double *yout,int revert);
extern "C" int set_projection(char *proj,struct pe_dsp pP_dsp,float pG_dsp[],
			      float pA_dsp[]);

extern "C" int X_QUE_COUNT;
extern "C"  int BLOCK_X_SERVER;
extern "C" int     data_selected;
extern "C" int	template_select_all_flg;/* select all template objs = 1, unselect = 2, None = 0 */
extern "C" float       BUFFER;
extern "C" struct item_list *hold_selected_items;
extern "C" int SCREEN_MODE;
extern "C" int CHK_MODE;
#ifdef CAIRODRAW
extern "C" void cairogqtxx(int wkid,Gpoint pxy,char *str,Gextent *extent);
#else
extern "C" void gqtxx(int wkid,Gpoint pxy,Gchar *str,Gextent *extent);
#endif

extern "C" int format (char *s_name,char *s_units,double r,char *fmt_set,char str[257]);
extern "C" int cmpncs(char *s1,char *s2);
extern "C" int draw_selected(struct item_list *selected_items, int shadow);
extern "C" int chk_mov_To(struct table_chorn *pt);
extern "C" int vcs_legacy_canvas_update ( short use_defer_flg );

/* Python Calls */
/* Global Python theads variables and macros. Used primarily for the template editor update. */
extern "C" PyInterpreterState * mainInterpreterState = NULL;
extern "C" PyThreadState *mainThreadState = NULL;
extern "C" PyThreadState *myThreadState = NULL;

extern "C" int set_text_attr(struct table_text *pTt,struct table_chorn *pTo);
extern "C" int in_list(struct  item_list *item,struct  item_list **selected_items);


extern "C" int rotateextent(Gextent* extent)
{
  int corner;
  float tx,ty;

/*   /\* Now rotate until lower right is first *\/ */
  extern void printextentp(Gextent *extent);
/*   printextentp(extent); */
/*   printf("back from gq\n"); */
  corner=0;
  while(((extent->ll.x-extent->lr.x>=-.00001)&&(extent->ll.y-extent->ul.y>=-.00001))&&(corner<5)) {
    corner+=1;
/*     printextentp(extent); */
/*     printf("LR: %f, %f\n",extent->ll.x-extent->lr.x,extent->ll.y-extent->lr.y); */
/*     printf("UR: %f, %f\n",extent->ll.x-extent->ur.x,extent->ll.y-extent->ur.y); */
/*     printf("UL: %f, %f\n",extent->ll.x-extent->ul.x,extent->ll.y-extent->ul.y); */
/*     printf("------------- %i\n",corner); */
    tx=extent->ll.x;
    ty=extent->ll.y;
    extent->ll.x=extent->lr.x;
    extent->lr.x=extent->ur.x;
    extent->ur.x=extent->ul.x;
    extent->ul.x=tx;
    extent->ll.y=extent->lr.y;
    extent->lr.y=extent->ur.y;
    extent->ur.y=extent->ul.y;
    extent->ul.y=ty;
  }
/*   printextentp(extent); */
  return corner;
}

extern "C" void printextent(Gextent extent)
{
  printf("LL: %f, %f:\n", extent.ll.x,extent.ll.y); 
  printf("LR: %f, %f:\n", extent.lr.x,extent.lr.y); 
  printf("UR: %f, %f:\n", extent.ur.x,extent.ur.y); 
  printf("UL: %f, %f:\n", extent.ul.x,extent.ul.y); 
}
extern "C" void printextentp(Gextent *extent)
{
  printf("LL: %f, %f:\n", extent->ll.x,extent->ll.y); 
  printf("LR: %f, %f:\n", extent->lr.x,extent->lr.y); 
  printf("UR: %f, %f:\n", extent->ur.x,extent->ur.y); 
  printf("UL: %f, %f:\n", extent->ul.x,extent->ul.y); 
}


extern "C" void change_graphic_method(char *dname,char *gtype,char *gname)
{
  extern struct display_tab  D_tab;
  struct display_tab  *dtab;
  extern int              	update_ind;
  dtab = &D_tab;
  while (dtab != NULL){
    if (strcmp(dtab->name,dname)==0){
      /* we got the right display to update */
      strcpy(dtab->type,gtype);
      strcpy(dtab->g_name,gname);
      dtab->dsp_seg[3]=1; /* tells it to redraw */
      update_ind=1;
      vcs_legacy_canvas_update(1);
    }
    dtab=dtab->next;
  }

}

extern "C" int within(Gpoint point, Gextent extent)
{
  //extern int isInsidePolygon();    
    float x,y;
    float xs[5],ys[5];
    float tol = 1.e-4;
    
    x = point.x;
    y = point.y;

//     printf("looking if : %f,%f is within\n",x,y);
//     printextent(extent);
    xs[0]=extent.ll.x-tol;
    xs[1]=extent.lr.x+tol;
    xs[2]=extent.ur.x+tol;
    xs[3]=extent.ul.x-tol;
    xs[4]=xs[0];
    ys[0]=extent.ll.y-tol;
    ys[1]=extent.lr.y-tol;
    ys[2]=extent.ur.y+tol;
    ys[3]=extent.ul.y+tol;
    ys[4]=ys[0];
    if (isInsidePolygon(x,y,4,xs,ys)) return 1;
/*     /\* Check comparison to see if within the tolerance of 0.00001. This is as good as 0. *\/ */
/*     if ( fabs(point.x - extent.ll.x) <= (float)1e-4 ) cpoint.x = extent.ll.x; */
/*     if ( fabs(point.x - extent.ur.x) <= (float)1e-4 ) cpoint.x = extent.ur.x; */
/*     if ( fabs(point.y - extent.ll.y) <= (float)1e-4 ) cpoint.y = extent.ll.y; */
/*     if ( fabs(point.y - extent.ur.y) <= (float)1e-4 ) cpoint.y = extent.ur.y; */

/*     if ((extent.ll.x <= cpoint.x && cpoint.x <= extent.ur.x) || (extent.ur.x <= cpoint.x && cpoint.x <= extent.ll.x)) */
/*         if ((extent.ll.y <= cpoint.y && cpoint.y <= extent.ur.y) || (extent.ur.y <= cpoint.y && cpoint.y <= extent.ll.y)) */
/*             return 1; */
    return 0;
}

extern "C" int contained_in(Gextent extent,Gextent outer_box)
{
    if (within(extent.ll,outer_box))
        if (within(extent.lr,outer_box))
            if (within(extent.ur,outer_box))
                if (within(extent.ul,outer_box))
                    return 1;
    return 0;
}

extern "C" int within_buffer(Gpoint point, Gextent extent, float buffer)
{
  Gextent bigextent;

  bigextent=extent;
  bigextent.ll.x-=buffer;
  bigextent.ul.x-=buffer;
  bigextent.lr.x+=buffer;
  bigextent.ur.x+=buffer;
  bigextent.ll.y-=buffer;
  bigextent.lr.y-=buffer;
  bigextent.ur.y+=buffer;
  bigextent.ul.y+=buffer;
  return within(point,bigextent);
}


/* This is an embedded Python function call. It is the Python command
 * that updates the Template Editor's entry windows with the changed
 * values. In the vcs_legacy/Lib directory, see the Canvas.py and 
 * gui_template_edit.py files to get a look at how the template
 * editor is threaded.
 */
extern "C" int
update_template_gui( PyVCScanvas_Object *self)
{
        PyObject *mdict = NULL, *main = NULL, *dkeys=NULL,*dvalues=NULL, *dstring=NULL;
	PyObject *dlist=NULL, *dvalue=NULL, *dnum=NULL;
	PyObject* tattribute=NULL;
	PyObject* result=NULL;

        PyObject* testattribute;
        char *test_str=NULL;

        int i, dsize;
        int canvas_num=0;
	PY_ENTER_THREADS
        PY_GRAB_THREAD

        main = PyImport_ImportModule("__main__");
        mdict = PyModule_GetDict( main ); /* borrowed ref */
        dsize = PyDict_Size( mdict );
        dkeys = PyDict_Keys( mdict);
        dvalues = PyDict_Values( mdict );

        for (i = 0; i < dsize; i++) {
	     dlist = PyList_GetItem(dkeys, i); /* borrowed ref */
	     dvalue=PyList_GetItem(dvalues, i); /* borrowed ref */
              if (PyString_Check(dlist)) {      /* get the canvas object */
		dnum = PyObject_CallMethod(dvalue, "canvasid", (char*)0);
		canvas_num = (int) PyInt_AsLong (dnum);
		if (canvas_num == self->canvas_id) {
	           tattribute = PyObject_GetAttrString(dvalue, "canvas_template_editor");

		   /* Ok here is the code to make sure we have the same template editing */
		   
		   result = PyObject_CallMethod(tattribute, "refresh_data", (char *)0);
		}
	      }
        }

	Py_XDECREF( main );
	Py_XDECREF( dkeys );
	Py_XDECREF( dvalues );
	Py_XDECREF( dnum );
	Py_XDECREF( tattribute );
	Py_XDECREF( result );

	PY_RELEASE_THREAD
	PY_LEAVE_THREADS

	return 1;
}

/* This is an embedded Python function call. It is the Python command
 * that updates the Template Editor's entry windows with the changed
 * values. In the vcs_legacy/Lib directory, see the Canvas.py and 
 * gui_template_edit.py files to get a look at how the template
 * editor is threaded.
int draw_selected(selected_items,shadow)
struct item_list *selected_items;
int shadow;
{
    struct item_list *current = selected_items;
    Gpoint pts[5];
    while (current != NULL)
    {
        draw_selection_box(current->extent,shadow);
	if (current->type == display_tab) 
	  {
	    draw_reshape_dots(current);
	  }
        current = current->next;
    }

}
 */

/* Tell the X server (i.e., the x main loop) that
 * all left button commands will be for the Data 
 * Display.
 */
extern "C" PyObject *
PyVCS_screen_data_flag(PyVCScanvas_Object *self, PyObject *args)
{
        SCREEN_MODE = DATA;

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* Tell the X server (i.e., the x main loop) that
 * all left button commands will be for the Template 
 * Editor.
 */
extern "C" PyObject *
PyVCS_screen_template_flag(PyVCScanvas_Object *self, PyObject *args)
{
        SCREEN_MODE = TEDITOR;

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* Tell the X server (i.e., the x main loop) that
 * all  button commands will be for the GraphicMethod
 * Editor.
 */
extern "C" PyObject *
PyVCS_screen_gm_flag(PyVCScanvas_Object *self, PyObject *args)
{
        SCREEN_MODE = GMEDITOR;

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* Tell the X server (i.e., the x main loop) that
 * it is already in DATA mode so don't update the 
 * VCS Canvas.
 */
extern "C" PyObject *
PyVCS_checkmode_data_flag(PyVCScanvas_Object *self, PyObject *args)
{
        CHK_MODE = DATA;

        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

extern "C" PyObject *
PyVCS_screen_mode(PyVCScanvas_Object *self, PyObject *args)
{
        /* Return (screen mode) Python object */
        if (SCREEN_MODE == TEDITOR)
           return Py_BuildValue("s", "TEDITOR");
        else if (SCREEN_MODE == DATA)
           return Py_BuildValue("s", "DATA");
        else if (SCREEN_MODE == GMEDITOR)
           return Py_BuildValue("s", "GMEDITOR");
}

extern "C" enum etypes template_type( char *name)
{
        int i;
        char *template_text_names[] = {"file", "function", "logicalmask", "transformation", "source", "dataname", "title", "units", "crdate", "crtime", "comment1", "comment2", "comment3", "comment4", "xname", "yname", "zname", "tname", "xunits", "yunits", "zunits", "tunits"};
        char *template_format_names[] = {"xvalue", "yvalue", "zvalue", "tvalue", "mean", "min", "max"};
        char *template_xtickmarks_names[] = {"xtic1", "xtic2", "xmintic1", "xmintic2"};
        char *template_ytickmarks_names[] = {"ytic1", "ytic2", "ymintic1", "ymintic2"};
        char *template_xlabels_names[] = {"xlabel1", "xlabel2"};
        char *template_ylabels_names[] = {"ylabel1", "ylabel2"};
        char *template_boxes_names[] = {"box1", "box2", "box3", "box4"};
        char *template_lines_names[] = {"line1", "line2", "line3", "line4"};

        for (i = 0; i < 22; i++) { if (cmpncs(template_text_names[i], name) == 0) return pe_text; }
        for (i = 0; i < 7; i++)  { if (cmpncs(template_format_names[i], name) == 0) return pe_form; }
        for (i = 0; i < 4; i++)  { if (cmpncs(template_xtickmarks_names[i], name) == 0) return pe_x_tic; }
        for (i = 0; i < 4; i++)  { if (cmpncs(template_ytickmarks_names[i], name) == 0) return pe_y_tic; }
        for (i = 0; i < 2; i++)  { if (cmpncs(template_xlabels_names[i], name) == 0) return pe_x_lab; }
        for (i = 0; i < 2; i++)  { if (cmpncs(template_ylabels_names[i], name) == 0) return pe_y_lab; }
        for (i = 0; i < 4; i++)  { if (cmpncs(template_boxes_names[i], name) == 0) return pe_box; }
        for (i = 0; i < 4; i++)  { if (cmpncs(template_lines_names[i], name) == 0) return pe_line; }
        if (cmpncs("legend", name) == 0) return pe_leg;
        if (cmpncs("data", name) == 0) return pe_dsp;

        return pe_none;
}

/* This function will highlight the picture template object that is
 * toggled on from the Template Editor GUI.
 */
extern "C" PyObject *
PyVCS_select_one(PyVCScanvas_Object *self, PyObject *args)
{
        struct  item_list   	*item=NULL;
	char                    *TEMPLATE_NAME=NULL,*ATTR_NAME=NULL;
        float	                X1,X2,Y1,Y2;
        Gpoint 			pointA;
        enum etypes             search_type;

        if(PyArg_ParseTuple(args,"|ssffff", &TEMPLATE_NAME,&ATTR_NAME,&X1,&X2,&Y1,&Y2)) {
           if (TEMPLATE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide a template name.");
	         Py_INCREF ((PyObject *)Py_None); return Py_None;
           }
        }

        if ((X1 == -999) && (X2 == -999))
           pointA.x = 0.5;
        else if (X2 == -999)
            pointA.x = plnorm( 0, X1 );
        else
            pointA.x = plnorm( 0, (X1 + (X2-X1)*0.5) );

        if ((Y1 == -999) && (Y2 == -999))
           pointA.y = 0.5;
        else if (Y2 == -999)
            pointA.y = plnorm( 1, Y1);
        else
            pointA.y = plnorm( 1, (Y1 + (Y2-Y1)*0.5) );

        search_type = template_type( ATTR_NAME );

        item = select_item(self, pointA, TEMPLATE_NAME, ATTR_NAME, search_type);
        if (item != NULL) {
           append_to_list(item,&hold_selected_items);
           draw_selected(hold_selected_items,0);
           PyVCS_backing_store(self, args);
        }

	/* Return NULL Python Object */
	Py_INCREF ((PyObject *)Py_None);
  	return Py_None;
}

/* This function will unhighlight the picture template object that is
 * toggled off from the Template Editor GUI.
 */
extern "C" PyObject *
PyVCS_unselect_one(PyVCScanvas_Object *self, PyObject *args)
{
	PyObject 		*update_args;
        struct  item_list   	*item=NULL;
	char                    *TEMPLATE_NAME=NULL,*ATTR_NAME=NULL;
        float	                X1,X2,Y1,Y2;
        Gpoint 			pointA;
        enum etypes             search_type;

        if(PyArg_ParseTuple(args,"|ssffff", &TEMPLATE_NAME,&ATTR_NAME,&X1,&X2,&Y1,&Y2)) {
           if (TEMPLATE_NAME == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide a template name.");
	         Py_INCREF ((PyObject *)Py_None); return Py_None;
           }
        }

	/* Update segemate arguments for PyVCS_updateVCSsegments. Sets the mode argument to 1 */
        update_args = PyTuple_New(1);
	PyTuple_SetItem(update_args, 0, Py_BuildValue("i", 1)); /* Set the 1st and only argv to 1 */

        if ((X1 == -999) && (X2 == -999))
           pointA.x = 0.5;
        else if (X2 == -999)
            pointA.x = plnorm( 0, X1 );
        else
            pointA.x = plnorm( 0, (X1 + (X2-X1)*0.5) );

        if ((Y1 == -999) && (Y2 == -999))
           pointA.y = 0.5;
        else if (Y2 == -999)
            pointA.y = plnorm( 1, Y1);
        else
            pointA.y = plnorm( 1, (Y1 + (Y2-Y1)*0.5) );

        search_type = template_type( ATTR_NAME );

        item = select_item(self, pointA, TEMPLATE_NAME, ATTR_NAME, search_type);
        if (item != NULL) {
           remove_from_list(item,&hold_selected_items);
           draw_selected(hold_selected_items,0);
           PyVCS_backing_store(self, args);
           delete_list(&item);
        }

	/* Return NULL Python Object */
	Py_INCREF ((PyObject *)Py_None);
  	return Py_None;
}

/* This function will highlight all the picture template objects
 * that have a priority greater than one.
 */
extern "C" PyObject *
PyVCS_select_all(PyVCScanvas_Object *self, PyObject *args)
{

	PyObject 	*update_args;
        Gpoint 		pointA, pointB;

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}
        template_select_all_flg = 1; /* if set to 1, then select all */

#ifdef X11WM
        if ( (self->connect_id.display != NULL) && (self->connect_id.drawable != 0) )
           XRaiseWindow(self->connect_id.display, self->connect_id.drawable);
#endif

	/* Update segemate arguments for PyVCS_updateVCSsegments. Sets the mode argument to 1 */
        update_args = PyTuple_New(1);
	PyTuple_SetItem(update_args, 0, Py_BuildValue("i", 1)); /* Set the 1st and only argv to 1 */

        PyVCS_updateVCSsegments(self, update_args);
        delete_list(&hold_selected_items);

        /* select all the template objects on the page */
        pointA.x = 0.0; pointA.y = 0.0;
        pointB.x = 1.0; pointB.y = 1.0;
        PyVCS_select_all_in_range(self, &hold_selected_items, pointA, pointB);
        draw_selected(hold_selected_items,0);
        PyVCS_backing_store(self, args);

	/* Return NULL Python Object */
	Py_INCREF ((PyObject *)Py_None);
  	return Py_None;
}

extern "C" PyObject *
PyVCS_unselect_all(PyVCScanvas_Object *self, PyObject *args)
{
	PyObject 			*update_args;

	/* Check to see if vcs_legacy has been initalized */
	if (self == NULL) {
           PyErr_SetString(PyExc_TypeError, "Must first initialize VCS (i.e., x=vcs_legacy.init()).");
  	   return NULL;
	}
        template_select_all_flg = 2; /* if set to 2, then unselect all */

#ifdef X11WM
        if ( (self->connect_id.display != NULL) && (self->connect_id.drawable != 0) )
           XRaiseWindow(self->connect_id.display, self->connect_id.drawable);
#endif

	/* Update segemate arguments for PyVCS_updateVCSsegments. Sets the mode argument to 1 */
        update_args = PyTuple_New(1);
	PyTuple_SetItem(update_args, 0, Py_BuildValue("i", 1)); /* Set the 1st and only argv to 1 */

        /*delete_list(&hold_selected_items);*/
        PyVCS_updateVCSsegments(self, args);
        delete_list(&hold_selected_items);
        PyVCS_backing_store(self, args);

	/* Return NULL Python Object */
	Py_INCREF ((PyObject *)Py_None);
  	return Py_None;
}

extern "C" int
update_template_toggle_color( PyVCScanvas_Object *self, struct item_list *selected_items)
{
        PyObject *mdict = NULL, *main = NULL, *dkeys=NULL,*dvalues=NULL, *dstring=NULL;
        PyObject *dlist=NULL, *dvalue=NULL, *dnum=NULL;
        PyObject* tattribute=NULL;
        PyObject* result=NULL;

        int i, j, dsize;
        int canvas_num=0;

        struct item_list *current = selected_items;
	char *tname;
        char *template_attr_names[] = {"file", "function", "logicalmask", "transformation", "source", "dataname", "title", "units", "crdate", "crtime", "comment1", "comment2", "comment3", "comment4", "xname", "yname", "zname", "tname", "xunits", "yunits", "zunits", "tunits", "xvalue", "yvalue", "zvalue", "tvalue", "mean", "min", "max", "xtic1", "xtic2", "xmintic1", "xmintic2", "ytic1", "ytic2", "ymintic1", "ymintic2", "xlabel1", "xlabel2", "ylabel1", "ylabel2", "box1", "box2", "box3", "box4", "line1", "line2", "line3", "line4", "legend", "data"};

        PY_ENTER_THREADS
        PY_GRAB_THREAD

        main = PyImport_ImportModule("__main__");
        mdict = PyModule_GetDict( main ); /* borrowed ref */
        dsize = PyDict_Size( mdict );
        dkeys = PyDict_Keys( mdict);
        dvalues = PyDict_Values( mdict );

        for (i = 0; i < dsize; i++) {
             dlist = PyList_GetItem(dkeys, i); /* borrowed ref */
             dvalue=PyList_GetItem(dvalues, i); /* borrowed ref */
              if (PyString_Check(dlist)) {      /* get the canvas object */
                dnum = PyObject_CallMethod(dvalue, "canvasid", (char*)0);
                canvas_num = (int) PyInt_AsLong (dnum);
                if (canvas_num == self->canvas_id) {
                   tattribute = PyObject_GetAttrString(dvalue, "canvas_template_editor");
                   for (j = 0; j < 51; j++) {
		     result = PyObject_CallMethod(tattribute, "refresh_toggle", "si", template_attr_names[j], 0);
		     Py_XDECREF( result );
		   }
                   while (current != NULL) {
		       {

			 result = PyObject_CallMethod(tattribute, "refresh_toggle", "si", current->attr_name, 1);
			 Py_XDECREF( result );
		       }
		     current = current->next;
                   }
                }
              }
        }

        Py_XDECREF( main );
        Py_XDECREF( dkeys );
        Py_XDECREF( dvalues );
        Py_XDECREF( dnum );
        Py_XDECREF( tattribute );
        PY_RELEASE_THREAD
        PY_LEAVE_THREADS
        return 1;
}


/* Following is for updating canvas editor when switching templates */
extern "C" int
switch_templates( PyVCScanvas_Object *self, struct item_list *selected_items)
{
        PyObject *mdict = NULL, *main = NULL, *dkeys=NULL,*dvalues=NULL, *dstring=NULL;
        PyObject *dlist=NULL, *dvalue=NULL, *dnum=NULL;
        PyObject* tattribute=NULL;
        PyObject* result=NULL;
        PyObject* result2=NULL;
        PyObject* result3=NULL;
        PyObject* result4=NULL;
        PyObject* dialog=NULL;
        PyObject* py_template=NULL;
        PyObject* titlef=NULL;
        PyObject* pargs=NULL;
        PyObject* ptype=NULL;
        PyObject* pvalue=NULL;
        PyObject* pstack=NULL;

        PyObject* testattribute;
        char *test_str=NULL;

        int i, j, dsize,switched=0,save;
        int canvas_num=0;

        struct item_list *current = selected_items;
	char *tname;
	char *tname_tmp;
	char *ask_from_vcs_legacy;
	char *tname_orig;

	extern struct p_tab    Pic_tab;
	struct p_tab    *ptab;
	extern struct display_tab  D_tab;
	struct display_tab  *dtab;
	
        PY_ENTER_THREADS
        PY_GRAB_THREAD

        main = PyImport_ImportModule("__main__");
        mdict = PyModule_GetDict( main ); /* borrowed ref */
        dsize = PyDict_Size( mdict );
        dkeys = PyDict_Keys( mdict);
        dvalues = PyDict_Values( mdict );

        for (i = 0; i < dsize; i++) {
	  dlist = PyList_GetItem(dkeys, i); /* borrowed ref */
	  dvalue=PyList_GetItem(dvalues, i); /* borrowed ref */
	  if (PyString_Check(dlist)) {      /* get the canvas object */
	    dnum = PyObject_CallMethod(dvalue, "canvasid", (char*)0);
	    canvas_num = (int) PyInt_AsLong (dnum);
	    if (canvas_num == self->canvas_id) {
	      tattribute = PyObject_GetAttrString(dvalue, "canvas_template_editor");
	      result = PyObject_CallMethod(tattribute, "refresh_self_canvas", (char*)0); /* ok no idea why but nothing works w/o this... */
	      Py_XDECREF( result );
	      while (current != NULL) {
		result = PyObject_GetAttrString(tattribute,"new_template_name");
		tname = PyString_AsString(result);
		Py_XDECREF( result );
		result = PyObject_GetAttrString(tattribute,"template_name");
		tname_tmp = PyString_AsString(result);
		Py_XDECREF( result );
		if (strcmp(current->ptab->name,tname)!=0) {
		  switched =1;
		  result = PyObject_GetAttrString(tattribute,"template_orig_name");
		  tname_orig = PyString_AsString(result);
		  Py_XDECREF( result );
		  break;
		}
		current = current->next;
	      }
	    }
	  }
	  if (switched==1) break;
        }
	if (switched==1)
	  {
 	    result = PyObject_CallMethod(tattribute, "ask_save_from_vcs_legacy", (char*)0); /* ok no idea why but nothing works w/o this... */
	    ask_from_vcs_legacy = PyString_AsString(result); 
	    save = 0;
	    if (strcmp(ask_from_vcs_legacy,"Save")==0) save = 1;
	    Py_XDECREF( result );
	    result=NULL;
	    if (save==1)
	      {
		
		/* create a copy that will become the new orig */
/* 		printf("creating a new template from: %s\n",tname); */
		result = PyObject_CallMethodObjArgs(dvalue,Py_BuildValue("s","createtemplate"),Py_None,Py_BuildValue("s",tname),NULL);

		/* get the original */
/* 		printf("ok now get the original one (%s)\n",tname_orig); */
		result2 = PyObject_CallMethod(dvalue,"gettemplate","s",tname_orig);
		/* removes it */
/* 		printf("and i remove it\n"); */
		result3 = PyObject_CallMethod(dvalue,"removeobject","O",result2);
		Py_XDECREF( result2 ); /* No need to keep python object */
		Py_XDECREF( result3 ); /* No need to keep python object */

		/* rename the temp one to this guy's name */
/* 		printf("now i rename the one i just created to: %s\n",tname_orig); */
		PyObject_SetAttrString(result,"name",Py_BuildValue("s",tname_orig));
		Py_XDECREF( result ); /* No need to keep python object */
		/* Here we need to loop thru displays and set the right template for */
/* 		printf("and i'm coptying %s into tname_tmp (%s)\n",tname,tname_tmp); */
		strcpy(tname_tmp,tname);
/* 		printf("which result in tname_tmp to be: %s\n",tname_tmp); */
		/* the template we were editing earlier */
	      }
	    dtab = &D_tab;
	    while (dtab != NULL)
	      {
		/*       printf("Exploring display %s with template: %s\n",dtab->name,dtab->p_name); */
		//      if ((dtab->wkst_id == self->wkst_id) && (dtab->a[0][0] != '\0'))
/* 		printf("ok looking at display: %s, templates: %s, %s\n",dtab->name,dtab->p_name,dtab->p_orig_name); */
		if ((dtab->wkst_id == self->wkst_id))
		  {
		    if (strcmp(tname,dtab->p_name) == 0) 
		      {
/* 			printf("     it is a match with the old template: %s, %s, %s, %s, %s\n",tname,dtab->p_name,dtab->p_orig_name,tname_orig,tname_tmp); */
			strcpy(dtab->p_name,tname_tmp); /* has been replaced to orig if save */
/* 			printf("replacing this display template with: %s (but not orig)\n",tname_tmp); */
			break;
		      }		      
		  }
		dtab=dtab->next;
	      }
	    /* Create a copy that we will not keep as python */
	    result2=Py_BuildValue("s",current->ptab->name);
	    result = PyObject_CallMethodObjArgs(dvalue,Py_BuildValue("s","createtemplate"),Py_None,result2,NULL);
	    result3 = PyObject_GetAttrString(result,"name");
	    tname_tmp = PyString_AsString(result3);
	    Py_XDECREF(result);

	    /* get the python object to attach it to canvas editor */
	    py_template = PyObject_CallMethod(dvalue,"gettemplate","s",current->ptab->name);
	    PyObject_SetAttrString(tattribute,"new_template",py_template);
	    PyObject_SetAttrString(tattribute,"new_template_name",result2);
	    Py_XDECREF(result2);

	    PyObject_SetAttrString(tattribute,"template_name",result3);
	    Py_XDECREF(result3);

	    /* save original name for resetting items later */
	    dtab = &D_tab;
	    while (dtab != NULL)
	      {
		if ((dtab->wkst_id == self->wkst_id))
		  {
		    if (strcmp(current->ptab->name,dtab->p_name) == 0) 
		      {
			result3 = Py_BuildValue("s",dtab->p_orig_name);
			PyObject_SetAttrString(tattribute,"template_orig_name",result3);
			Py_XDECREF(result3);
			/* reset editor text */
			dialog = PyObject_GetAttrString(tattribute,"dialog"); /* retrieve dialog tedtor to update name */
			result4 = PyObject_CallMethod(dialog,"title","s",dtab->p_orig_name);
			Py_XDECREF( result4 );
			Py_XDECREF( dialog );
			break;
		      }		      
		  }
		dtab=dtab->next;
	      }

	    /* and finaslly let's refresh data for this guy! */
	    result = PyObject_CallMethod(tattribute, "refresh_data", (char*)0);
	    Py_XDECREF(result);

	    /* The following is done no matter what */
	    /* think of sometihng to let know we accidentally click on another template */
	    /* i guess simply delete hold_selected and set item to NULL but outside of this loop */
	  }
	
        Py_XDECREF( main );
        Py_XDECREF( dkeys );
        Py_XDECREF( dvalues );
        Py_XDECREF( dnum );
        Py_XDECREF( tattribute );
        PY_RELEASE_THREAD
        PY_LEAVE_THREADS
        return switched;
}

/* This is an embedded Python function call. It is the Python command
 * that returns the current Template Editor's name. This is needed to
 * to determine which template editor to modify.
 */
extern "C" char *
return_template_name( PyVCScanvas_Object *self)
{
        PyObject *mdict = NULL, *main = NULL, *dkeys=NULL,*dvalues=NULL, *dstring=NULL;
	PyObject *dlist=NULL, *dvalue=NULL, *dnum=NULL;
	PyObject *tattribute=NULL, *template_attr=NULL;

        char *template_str=NULL;

        int i, dsize;
        int canvas_num=0;

	PY_ENTER_THREADS
        PY_GRAB_THREAD

        main = PyImport_ImportModule("__main__");
        mdict = PyModule_GetDict( main ); /* borrowed ref */
        dsize = PyDict_Size( mdict );
        dkeys = PyDict_Keys( mdict);
        dvalues = PyDict_Values( mdict );

        for (i = 0; i < dsize; i++) {
	     dlist = PyList_GetItem(dkeys, i); /* borrowed ref */
	     dvalue=PyList_GetItem(dvalues, i); /* borrowed ref */
              if (PyString_Check(dlist)) {      /* get the canvas object */
		dnum = PyObject_CallMethod(dvalue, "canvasid", (char*)0);
		canvas_num = (int) PyInt_AsLong (dnum);
		if (canvas_num == self->canvas_id) {
	           tattribute = PyObject_GetAttrString(dvalue, "canvas_template_editor");

	           /*template_attr = PyObject_GetAttrString(tattribute, "template_name");*/
	           template_attr = PyObject_GetAttrString(tattribute, "new_template_name");
                   template_str = PyString_AsString(template_attr);
		}
	      }
        }

	Py_XDECREF( main );
	Py_XDECREF( dkeys );
	Py_XDECREF( dvalues );
	Py_XDECREF( dnum );
	Py_XDECREF( tattribute );
	Py_XDECREF( template_attr );

	PY_RELEASE_THREAD
	PY_LEAVE_THREADS

        return template_str;
}

/* Tell the X server (i.e., the x main loop) that
 * all commands in the X queue will be discarded.
 * We don't need them anyway. At least, I think we don't.
 */
extern "C" PyObject *
PyVCS_Xsync_discard(PyVCScanvas_Object *self, PyObject *args)
{
#ifdef X11WM
        if (self->connect_id.display != NULL) {
           XFlush( self->connect_id.display );
           XSync( self->connect_id.display, TRUE);
        }
#endif
        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* Tell the X server (i.e., the x main loop) that 
 * a VCS Command is requested and it needs to wait
 * or stop processing until the command is completed.
 */
extern "C" PyObject *
PyVCS_BLOCK_X_SERVER(PyVCScanvas_Object *self, PyObject *args)
{
        /* Note: No need to call the X routine:
        *        XLockDisplay( connect_id.display );
        *        because the flag below will block the
        *        X display for me.
        */
        BLOCK_X_SERVER += 1;
        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}
/* Tell the X server (i.e., the X main loop) that
 * the VCS command is finished.
 */
extern "C" PyObject *
PyVCS_UNBLOCK_X_SERVER(PyVCScanvas_Object *self, PyObject *args)
{
        /* Note: No need to call the X routine:
        *        XUnlockDisplay( connect_id.display );
        *        because the flag below will unblock the 
        *        X display for me.
        */
        BLOCK_X_SERVER -= 1;
        /* Return NULL Python Object */
        Py_INCREF(Py_None);
        return Py_None;
}

/* Return the number of X queued events, That is, if the
 * X server is in the middle of something then wait until
 * it is finished before processing the VCS command.
 */
extern "C" PyObject *
PyVCS_Xpending(PyVCScanvas_Object *self, PyObject *args)
{
        int return_value = 0;

        if (X_QUE_COUNT > 0)
           return_value = 1;

        return Py_BuildValue("i", return_value);
}


/*
 * Handle the VCS Canvas X events. This function must be threaded to
 * update all X commands that occur in the VCS Canvas. For example,
 * when the VCS Canvas is brought to the front (i.e., exposed), then
 * the Canvas will be redrawn showing the previously hidden area. Therefore,
 * handling its own backing store.
 */

extern "C" void item_type(struct  item_list *item)
{
  if (item== NULL) return;
  printf("item type: ");
  switch(item->type){
  case(pe_text):
    printf("pe_text\n");
    break;
  case(pe_form):
    printf("pe_form\n");
    break;
  case(pe_x_tic):
    printf("pe_xtic\n");
    break;
  case(pe_y_tic):
    printf("pe_y_tic\n");
    break;
  case(pe_x_lab):
    printf("pe_x_lab\n");
    break;
  case(pe_y_lab):
    printf("pe_y_lab\n");
    break;
  case(pe_box):
    printf("pe_box\n");
    break;
  case(pe_line):
    printf("pe_line\n");
    break;
  case(pe_leg):
    printf("pe_leg\n");
    break;
  case(pe_dsp):
    printf("pe_dsp\n");
    break;
  case(pe_none):
    printf("pe_none\n");
    break;
  case(display_tab):
    printf("display_tab\n");
    break;
  default:
    printf("well something else believe it or not!\n");
    break;
  }
  if (item->next != NULL) {
    printf("and next is: \n");
    item_type(item->next);
  }
}
/* Change the graphic method for a display name */
extern "C" PyObject *PyVCS_get_selected_display_graphic_method(PyVCScanvas_Object *self,PyObject *args)
{
  extern struct item_list *hold_selected_items;
  if ( (SCREEN_MODE!=GMEDITOR) || (hold_selected_items==NULL)){
    Py_INCREF(Py_None);
    return Py_None;
  }
  
  /* Return NULL Python Object */
  return Py_BuildValue("s",hold_selected_items->data.pd->name);
}


/* Other misc */
extern "C" int draw_selected(struct item_list *selected_items, int shadow)
{
    struct item_list *current = selected_items;
    while (current != NULL)
    {
      if (current->type == display_tab)
	{
	  if (shadow!=1) draw_selection_box(current->extent,shadow);
	  if (shadow!=2) draw_reshape_dots(current,shadow);
	}
      else
	{
 	  draw_selection_box(current->extent,shadow);
	}
      current = current->next;
    }
}

/* int unselect_item(extent) */
/* Gextent extent;   */
/* { */
/*     Gpoint pts[5]; */
/*     /\* Box is drawn from lower left corner counter-clockwise *\/ */

/*     pts[0].x = pts[3].x = pts[4].x = extent.ll.x; */
/*     pts[1].x = pts[2].x = extent.lr.x; */
/*     pts[0].y = pts[1].y = pts[4].y = extent.ll.y; */
/*     pts[2].y = pts[3].y = extent.ul.y; */
/* gsfais(1);      /\* Set fill area to be solid *\/ */
/*     gsfaci(0);      /\* Set fill color to be white *\/ */
/*     gfa(4,pts);     /\* Define fill area *\/ */
/*     gsmk(1);        /\* Select marker style (dot) *\/ */
/*     gsmksc(3.5);    /\* Select marker size *\/ */
/*     gspmci(0);      /\* Select marker color *\/ */
/*     gpm(4,pts);     /\* Draw markers *\/ */
/* } */


extern "C" void rotate_anchors(Gpoint *pts,int n,float angle)
{
  float dx,dy,dxp,dyp;
  int i;
  for (i=1;i<n;i++) {
    dxp = pts[i].x-pts[0].x;
    dyp = pts[i].y-pts[0].y;
    dx = dxp*cos(angle)-dyp*sin(angle);
    dy = dxp*sin(angle)+dyp*cos(angle);
    pts[i].x=pts[0].x+dx;
    pts[i].y=pts[0].y+dy;
  }
}

extern "C" void marker(Gpoint point, int color, int type, float angle)
{
  Gpoint pts[5];
  double buffer=.0035;
  int i;
  
  /* Box is drawn from lower left corner counter-clockwise */
  pts[0].x = pts[3].x = pts[4].x = point.x-buffer;
  pts[1].x = pts[2].x = point.x+buffer;
  pts[0].y = pts[1].y = pts[4].y = point.y-buffer;
  pts[2].y = pts[3].y = point.y+buffer;
  gsfais(1);          /* Set fill area style (1 = solid) */
  gsfaci(color);      /* Set fill area color */
  if (type==0) pts[2]=pts[3]; /*makes it a triangle */
  else if (type==1) pts[3]=pts[0]; /*makes it a triangle */
  else if (type==2) {pts[0]=pts[1]; pts[4]=pts[1];} /*makes it a triangle */
  else if (type==3) pts[1]=pts[2]; /*makes it a triangle */
  else if ((type==4)||(type==5)) /*extent horizontal */
    {
      pts[0].x-=buffer/3.;
      pts[1].x+=buffer/3.;
      pts[2].x+=buffer/3.;
      pts[3].x-=buffer/3.;
      pts[4].x-=buffer/3.;
      
      pts[0].y+=buffer/2.;
      pts[1].y+=buffer/2;
      pts[2].y-=buffer/2.;
      pts[3].y-=buffer/2;
      pts[4].y+=buffer/2;
    }
  else if ((type==6)||(type==7)) /*extent vertical */
    {
      pts[0].y-=buffer/3.;
      pts[1].y-=buffer/3.;
      pts[2].y+=buffer/3.;
      pts[3].y+=buffer/3.;
      pts[4].y-=buffer/3.;
      
      pts[0].x+=buffer/2.;
      pts[1].x-=buffer/2;
      pts[2].x-=buffer/2.;
      pts[3].x+=buffer/2;
      pts[4].x+=buffer/2;
    }
  else if (type==8)
    {
      /* nothing to do it is a normal square!*/
    }
  else if (type==9) /*bigger square */
    {
      pts[0].x-=buffer/2.;
      pts[0].y-=buffer/2.;
      pts[1].x+=buffer/2.;
      pts[1].y-=buffer/2.;
      pts[2].x+=buffer/2.;
      pts[2].y+=buffer/2.;
      pts[3].x-=buffer/2.;
      pts[3].y+=buffer/2.;
      pts[4].x-=buffer/2.;
      pts[4].y-=buffer/2.;	
    }
  rotate_anchors(&pts[0],4,angle);
      /* nothing to do it is a normal square!*/
  if (color!=-1) /* -1 is empty box */
    gfa(5,pts);         /* Draw fill area */
  gsplci(241);        /* Set line color */
  gpl(5,pts);         /* Draw line */
}

extern "C" void draw_reshape_dots(struct item_list *item, int shadow)
{
  extern struct table_line        Tl_tab;
  struct table_line               *tltab;
  extern struct table_fill        Tf_tab;
  struct table_fill               *tftab;
  extern struct table_mark        Tm_tab;
  struct table_mark               *tmtab;
  struct points_struct		*xptr=NULL, *yptr=NULL;
  struct array_segments   	*xpts=NULL, *ypts=NULL;
  int iprim,j,color,counter;
  Gpoint pxy[1000]; /* limit of 1000 points ok ?*/
  char                            proj[256];

  counter = 0;
  if (strcmp(item->data.pd->type,"fillarea")==0){
    tftab = &Tf_tab;
    while (tftab != NULL) {
      if (cmpncs(item->data.pd->g_name, tftab->name) == 0) break;
      tftab = tftab->next;
    }
    if (tftab->priority == 0) return; /* do nothing */
    strcpy(proj,tftab->proj);
    set_viewport_and_worldcoordinate ( tftab->fvp, tftab->fwc,proj );
    xptr = tftab->fx; yptr = tftab->fy;
    xpts = xptr->ps;  ypts = yptr->ps;
    for (iprim=0; iprim<xptr->nsegs; iprim++) {
      for (j=0;j<xpts->npts;j++) {
	pxy[counter].x=xpts->pts[j];
	pxy[counter].y=ypts->pts[j];
	pxy[counter]=proj_convert(pxy[counter]);
	if ((int)tftab->faci[iprim]==244)
	  {color=243;}
	else
	  {color=244;}
	marker(pxy[counter],color,8,0.);
	counter++;
      }
      if (shadow==1)
	{
	  pxy[counter]=pxy[0];
	  gsplci(241);    /* Select line color (black = 241)*/
	  gsln(2);        /* Select line type (dashed = 2)*/
	  gpl(counter+1,pxy);     /* Draw box*/
	}
      xpts = xpts->next;
      ypts = ypts->next;
      counter=0;
    }
  }
  if (strcmp(item->data.pd->type,"line")==0){
    tltab = &Tl_tab;
    while (tltab != NULL) {
      if (cmpncs(item->data.pd->g_name, tltab->name) == 0) break;
      tltab = tltab->next;
    }
    if (tltab->priority == 0) return; /* do nothing */
    strcpy(proj,tltab->proj);
    set_viewport_and_worldcoordinate ( tltab->lvp, tltab->lwc,proj );
    xptr = tltab->lx; yptr = tltab->ly;
    xpts = xptr->ps;  ypts = yptr->ps;
    for (iprim=0; iprim<xptr->nsegs; iprim++) {
      for (j=0;j<xpts->npts;j++) {
	pxy[counter].x=xpts->pts[j];
	pxy[counter].y=ypts->pts[j];
	pxy[counter]=proj_convert(pxy[counter]);
	if ((int)tltab->lci[iprim]==244)
	  {color=243;}
	else
	  {color=244;}
	marker(pxy[counter],color,8,0.);
	counter++;
      }
      xpts = xpts->next;
      ypts = ypts->next;
      if (shadow==1)
	{
	  gsplci(241);    /* Select line color (black = 241)*/
	  gsln(2);        /* Select line type (dashed = 2)*/
	  gpl(counter,pxy);     /* Draw box*/
	}
      counter=0;
    }
  }
  if (strcmp(item->data.pd->type,"marker")==0){
    tmtab = &Tm_tab;
    while (tmtab != NULL) {
      if (cmpncs(item->data.pd->g_name, tmtab->name) == 0) break;
      tmtab = tmtab->next;
    }
    if (tmtab->priority == 0) return; /* do nothing */
    strcpy(proj,tmtab->proj);
    set_viewport_and_worldcoordinate ( tmtab->mvp, tmtab->mwc,proj );
    xptr = tmtab->mx; yptr = tmtab->my;
    xpts = xptr->ps;  ypts = yptr->ps;
    for (iprim=0; iprim<xptr->nsegs; iprim++) {
      for (j=0;j<xpts->npts;j++) {
	pxy[counter].x=xpts->pts[j];
	pxy[counter].y=ypts->pts[j];
	pxy[counter]=proj_convert(pxy[counter]);
	color=-1; /*means not filled */
	marker(pxy[counter],color,9,0.);
	counter++;
      }
      xpts = xpts->next;
      ypts = ypts->next;
      if (shadow==1)
	{
	  gsplci(241);    /* Select line color (black = 241)*/
	  gsln(2);        /* Select line type (dashed = 2)*/
	  gpl(counter,pxy);     /* Draw box*/
	}
      counter=0;

    }
  }
}


extern "C" void draw_selection_box(Gextent extentin, int shadow)
{
    Gpoint pts[9];
    int i;
    int color=242;
    float angle;
    Gextent extent;

    extent.ll.x=extentin.ll.x;
    extent.lr.x=extentin.lr.x;
    extent.ur.x=extentin.ur.x;
    extent.ul.x=extentin.ul.x;
    extent.ll.y=extentin.ll.y;
    extent.lr.y=extentin.lr.y;
    extent.ur.y=extentin.ur.y;
    extent.ul.y=extentin.ul.y;
    
    i=rotateextent(&extent);


/*     extern void printextent(); */
/*     printf("in draw box\n"); */
/*     printextent(extent); */
    /* Box is drawn from lower left corner counter-clockwise */
    /* Points after that are middle points */
/*     pts[0].x = pts[3].x = pts[4].x = pts[7].x = extent.ll.x; */
/*     pts[1].x = pts[2].x = pts[8].x =            extent.lr.x; */
/*     pts[0].y = pts[1].y = pts[4].y = pts[6].y = extent.ll.y; */
/*     pts[2].y = pts[3].y = pts[5].y =            extent.ul.y; */
/*     pts[5].x = pts[6].x =                       (extent.ll.x+extent.lr.x)/2.; */
/*     pts[7].y = pts[8].y =                       (extent.ll.y+extent.ul.y)/2.; */

    pts[0].x=extent.ll.x;/*LL corner*/
    pts[1].x=extent.lr.x;/*LR corner*/
    pts[2].x=extent.ur.x;/*UR corner*/ 
    pts[3].x=extent.ul.x;/*UL corner*/
    pts[4].x=extent.ll.x;/*LL corner*/
    pts[5].x=(extent.ul.x+extent.ur.x)/2.; /* top side */
    pts[6].x=(extent.ll.x+extent.lr.x)/2.; /* bottom side */
    pts[7].x=(extent.ll.x+extent.ul.x)/2.; /* left side */
    pts[8].x=(extent.lr.x+extent.ur.x)/2.; /* right side */

    pts[0].y=extent.ll.y;/*LL corner*/
    pts[1].y=extent.lr.y;/*LR corner*/
    pts[2].y=extent.ur.y;/*UR corner*/ 
    pts[3].y=extent.ul.y;/*UL corner*/
    pts[4].y=extent.ll.y;/*LL corner*/
    pts[5].y=(extent.ul.y+extent.ur.y)/2.; /* top side */
    pts[6].y=(extent.ll.y+extent.lr.y)/2.; /* bottom side */
    pts[7].y=(extent.ll.y+extent.ul.y)/2.; /* left side */
    pts[8].y=(extent.lr.y+extent.ur.y)/2.; /* right side */
    switch(shadow){
    case(0):
      if ((extent.lr.x-extent.ll.x)!=0) {
	angle = atan((extent.lr.y-extent.ll.y)/(extent.lr.x-extent.ll.x));
      }
      else {angle=1.5712;}
      marker(pts[0],color,0,angle); /*LL corner*/ 
      marker(pts[1],color,1,angle); /*LR corner*/ 
      marker(pts[2],color,2,angle); /*UR corner*/ 
      marker(pts[3],color,3,angle); /*UL corner*/ 
      marker(pts[5],color,4,angle); /* Top side*/ 
      marker(pts[6],color,5,angle); /* Bottom side*/ 
      marker(pts[7],color,6,angle); /* Left side*/ 
      marker(pts[8],color,7,angle); /* Right side*/ 
      break;
    case(1):
      gsplci(241);    /* Select line color (black = 241)*/
      gsln(2);        /* Select line type (dashed = 2)*/
      gpl(5,pts);     /* Draw box*/
      break;
    case(2):
      gsplci(241);    /* Select line color (black = 241)*/
      gsln(2);        /* Select line type (dashed = 2)*/
      gpl(5,pts);     /* Draw box*/
      break;
    default:
      gsplci(shadow); /*color */
      gsln(2); /* style 2: dashed */
      gslwsc(5.); /* width */
      gpl(5,pts);
      break;
    }
/*     Old code for drawing red dots in corners  */
/*     gsmk(1);        /\* Select marker style (dot) *\/ */
/*     gsmksc(3.5);    /\* Select marker size *\/ */
/*     gspmci(242);    /\* Select marker color *\/ */
/*     gpm(4,pts);     /\* Draw markers *\/ */
 
}

/* Find and return the shortest distance to the border
 * of the extent rectangle */
extern "C" float distance(Gpoint position, Gextent extent)
{
    float a,b;
    a = fmin(extent.ul.y-position.y,position.y-extent.ll.y);
    b = fmin(extent.ur.x-position.x,position.x-extent.ul.x);
    return fmin(a,b);
}

extern "C" int in_range(Gpoint position,Gextent extent)
{
    if (position.x >= extent.ll.x && position.x <= extent.lr.x &&
            position.y >= extent.ll.y && position.y <= extent.ul.y)
        return 1;
    else
        return 0;
}

/* Check and see if the current item is already in the selected list */
extern "C" void append_to_list(struct  item_list *item, struct  item_list **selected_items)
{
    struct item_list *current, *items;
    items = item;
    if (*selected_items == NULL) {
        *selected_items = (struct item_list *)item;
    } else
    {
        current = (struct item_list *)*selected_items;
/*         Get to the end of the list */
        while (current->next != NULL)
        {
            if (&current->data == &item->data)
                return;
            current = current->next;
        }

        /* Add the objects that are not already in the list */
        while (items != NULL)
        {
            /*if (!in_list(items,&selected_items))*/
            if (!in_list(items,&current))
            {
                current->next = items;
                current = current->next;
            }
            items = items->next;
        }
        current->next = NULL;
    }
}

/* Recursive part of function */
extern "C" void delete_node(struct item_list *node)
{
    if (node->next != NULL)
    {
        delete_node(node->next);
    }
    free(node);
    node=NULL;
}

extern "C" void zero_priority(struct item_list **selected_items)
{
    struct item_list *current = *selected_items;
    while (current != NULL)
    {
        switch (current->type)
        {
            case (pe_text):
                current->data.pet->p = 0;
                break;
            case (pe_form):
                current->data.pef->p = 0;
                break;
            case (pe_x_tic):
                current->data.pext->p = 0;
                break;
            case (pe_y_tic):
                current->data.peyt->p = 0;
                break;
            case (pe_x_lab):
                current->data.pexl->p = 0;
                break;
            case (pe_y_lab):
                current->data.peyl->p = 0;
                break;
            case (pe_box):
                current->data.peb->p = 0;
                break;
            case (pe_line):
                current->data.pel->p = 0;
                break;
            case (pe_leg):
                current->data.peleg->p = 0;
                break;
            case (pe_dsp):
                current->data.pedsp->p = 0;
                break;
        }
        current = current->next;
    }
}

/* Delete list by freeing all elements */
extern "C" void delete_list(struct  item_list  **selected_items) 
{
    if (*selected_items != NULL)
        delete_node(*selected_items);
    *selected_items = NULL;
}

/* Remove an item from list */
extern "C" void remove_from_list(struct  item_list   *item, struct  item_list **selected_items)
{
    struct item_list *current, *prev;
    current = prev = (struct item_list *)*selected_items;
    if (current == NULL) return;
    /* Case 1: 1st element is to be removed  */
    if (current->data.pet == item->data.pet)
    {
        /* There is more than 1 element in list */
        if (current->next != NULL)
            *selected_items = current->next;
        /* There is only 1 element in list */
        else
            *selected_items = NULL;
    }
    /* Case 2: Element to be removed is not the first element in the list */
    else
        while (current != NULL)
            if (current->data.pet == item->data.pet) {
                prev->next = current->next;
                break;
            }
            else {
                prev = current;
                current =  current->next;
            }
    free(current);
}

extern "C" void update_corner(Gpoint *corner, float dx,float dy,float angle, int x_or_y)
 /* 0 is x 1 is y */
{
  float dxp,dyp;
  dxp = dx*cos(angle)+dy*sin(angle);
  dyp = -dx*sin(angle)+dy*cos(angle);
  if (x_or_y==0) { /* x mvt */
    corner->x+=dxp*cos(angle);
    corner->y+=dxp*sin(angle);
  }
  else {
    corner->x-=dyp*sin(angle);
    corner->y+=dyp*cos(angle);
  }
}
/* */
extern "C" void update_extent(Gpoint pointA, Gpoint pointB, int action, struct  item_list **selected_items)
{
    struct item_list *current = *selected_items;
    float x_movement, y_movement;
    int j,iprim,counter,allin;
    extern struct table_fill        Tf_tab;
    struct table_fill               *tftab;
    extern struct table_line        Tl_tab;
    struct table_line               *tltab;
    extern struct table_mark        Tm_tab;
    struct table_mark               *tmtab;
    struct points_struct		*xptr=NULL, *yptr=NULL;
    struct array_segments   	*xpts=NULL, *ypts=NULL;
    char                            proj[256];
    Gpoint pxy,pxy2;
    Gextent cextent;
    float angle;
    int corner;
  PY_ENTER_THREADS
    PY_GRAB_THREAD

    x_movement = pointB.x - pointA.x;
    y_movement = pointB.y - pointA.y;
    while (current != NULL)
    {
  cextent.ll.x=current->extent.ll.x;
  cextent.lr.x=current->extent.lr.x;
  cextent.ur.x=current->extent.ur.x;
  cextent.ul.x=current->extent.ul.x;
  cextent.ll.y=current->extent.ll.y;
  cextent.lr.y=current->extent.lr.y;
  cextent.ur.y=current->extent.ur.y;
  cextent.ul.y=current->extent.ul.y;
  corner=rotateextent(&cextent);

      if ((cextent.lr.x-cextent.ll.x)!=0) {
	angle = atan((cextent.lr.y-cextent.ll.y) /(cextent.lr.x-cextent.ll.x));
      }
      else {angle=1.5712;}
      switch (action)
        {
	case (0):   /* Move entire object */
	  cextent.ll.x += x_movement;
	  cextent.ul.x += x_movement;
	  cextent.lr.x += x_movement;
	  cextent.ur.x += x_movement;
	  cextent.ll.y += y_movement;
	  cextent.lr.y += y_movement;
	  cextent.ul.y += y_movement;
	  cextent.ur.y += y_movement;
	  break;
	case (1):   /* Resize from upper left hand corner */
	  cextent.ul.x += x_movement;
	  cextent.ul.y += y_movement;
	  update_corner(&(cextent.ur),x_movement,y_movement,angle,1);
	  update_corner(&(cextent.ll),x_movement,y_movement,angle,0);
	  break;
	case (2):   /* Resize from upper right hand corner */
	  cextent.ur.x += x_movement;
	  cextent.ur.y += y_movement;
	  update_corner(&(cextent.ul),x_movement,y_movement,angle,1);
	  update_corner(&(cextent.lr),x_movement,y_movement,angle,0);
	  break;
	case (3):   /* Resize from lower right hand corner */
	  cextent.lr.x += x_movement;
	  cextent.lr.y += y_movement;
	  update_corner(&(cextent.ll),x_movement,y_movement,angle,1);
	  update_corner(&(cextent.ur),x_movement,y_movement,angle,0);
	  break;
	case (4):   /* Resize from lower left hand corner */
	  cextent.ll.x += x_movement;
	  cextent.ll.y += y_movement;
	  update_corner(&(cextent.lr),x_movement,y_movement,angle,1);
	  update_corner(&(cextent.ul),x_movement,y_movement,angle,0);
	  break;
	case (5):   /* Resize from top */
	  update_corner(&(cextent.ur),x_movement,y_movement,angle,1);
	  update_corner(&(cextent.ul),x_movement,y_movement,angle,1);
	  break;
	case (6):   /* Resize from right */
 	  update_corner(&(cextent.ur),x_movement,y_movement,angle,0);
	  update_corner(&(cextent.lr),x_movement,y_movement,angle,0);
	  break;
	case (7):   /* Resize from bottom */
 	  update_corner(&(cextent.lr),x_movement,y_movement,angle,1);
	  update_corner(&(cextent.ll),x_movement,y_movement,angle,1);
	  break;
	case (8):   /* Resize from left */
 	  update_corner(&(cextent.ul),x_movement,y_movement,angle,0);
	  update_corner(&(cextent.ll),x_movement,y_movement,angle,0);
	  break;
        }
  current->extent.ll.x=cextent.ll.x;
  current->extent.lr.x=cextent.lr.x;
  current->extent.ur.x=cextent.ur.x;
  current->extent.ul.x=cextent.ul.x;
  current->extent.ll.y=cextent.ll.y;
  current->extent.lr.y=cextent.lr.y;
  current->extent.ur.y=cextent.ur.y;
  current->extent.ul.y=cextent.ul.y;

	if (action>100) /* ok we are trying to modify a primitive */
	  {
	    if (strcmp(current->data.pd->type,"fillarea")==0)
	      {
		tftab = &Tf_tab;
		while (tftab != NULL) {
		  if (cmpncs(current->data.pd->g_name, tftab->name) == 0) break;
		  tftab = tftab->next;
		}
		if (tftab->priority == 0) break; /* do nothing */
		strcpy(proj,tftab->proj);
		set_viewport_and_worldcoordinate ( tftab->fvp, tftab->fwc,proj );
		xptr = tftab->fx; yptr = tftab->fy;
	      }
	    if (strcmp(current->data.pd->type,"line")==0)
	      {
		tltab = &Tl_tab;
		while (tltab != NULL) {
		  if (cmpncs(current->data.pd->g_name, tltab->name) == 0) break;
		  tltab = tltab->next;
		}
		if (tltab->priority == 0) break; /* do nothing */
		strcpy(proj,tltab->proj);
		set_viewport_and_worldcoordinate ( tltab->lvp, tltab->lwc,proj );
		xptr = tltab->lx; yptr = tltab->ly;
	      }
	    if (strcmp(current->data.pd->type,"marker")==0)
	      {
		tmtab = &Tm_tab;
		while (tmtab != NULL) {
		  if (cmpncs(current->data.pd->g_name, tmtab->name) == 0) break;
		  tmtab = tmtab->next;
		}
		if (tmtab->priority == 0) break; /* do nothing */
		strcpy(proj,tmtab->proj);
		set_viewport_and_worldcoordinate ( tmtab->mvp, tmtab->mwc,proj );
		xptr = tmtab->mx; yptr = tmtab->my;
	      }
	    xpts = xptr->ps;  ypts = yptr->ps;
	    counter=0;
	    pxy=pointB;
	    for (iprim=0; iprim<xptr->nsegs; iprim++) {
	      if (iprim==current->sub_primitive)
		{ /* ok we are going to touch this shape, need to recompute the extent */
		  current->extent.ll.x=current->extent.ll.y=current->extent.lr.y=current->extent.ul.x=1.E20;
		  current->extent.ur.x=current->extent.ur.y=current->extent.lr.x=current->extent.ul.y=-1.E20;
		
		  for (j=0;j<xpts->npts;j++) {
		    counter+=1;
		    /* we only do this if we're dealing with the point we're dragging */
		    /* and if this extent is the matching subprimitive number */
		    if (counter==(action-100))
		      { /* only change something if it is the right point! */
			pxy2 = invert_proj_convert(pxy);
			/*pxy=proj_convert(pxy); */
			/* need to do unproj part! and then set back to pts */
			if ((pxy2.x!=1.e20) || (pxy2.y!=1.e20))
			  {
			    xpts->pts[j]=pxy2.x;
			    ypts->pts[j]=pxy2.y;
			  }
			pxy2=pointB;
		      }
		    else
		      {
			pxy2.x=xpts->pts[j];
			pxy2.y=ypts->pts[j];
			pxy2=proj_convert(pxy2);
		      }
		    if (pxy2.x<current->extent.ll.x) current->extent.ll.x=pxy2.x;
		    if (pxy2.y<current->extent.ll.y) current->extent.ll.y=pxy2.y;
		    if (pxy2.x>current->extent.lr.x) current->extent.lr.x=pxy2.x;
		    if (pxy2.y<current->extent.lr.y) current->extent.lr.y=pxy2.y;
		    if (pxy2.x>current->extent.ur.x) current->extent.ur.x=pxy2.x;
		    if (pxy2.y>current->extent.ur.y) current->extent.ur.y=pxy2.y;
		    if (pxy2.x<current->extent.ul.x) current->extent.ul.x=pxy2.x;
		    if (pxy2.y>current->extent.ul.y) current->extent.ul.y=pxy2.y;
		  }
		}
	      else
		{ /* we need to increment counter */
		  counter+=xpts->npts;
		}
	      /* ok since we touched it we need to recopmute the extent? */
	      xpts = xpts->next;
	      ypts = ypts->next;
	    }
	  }
        current = current->next;
    }
  PY_RELEASE_THREAD
    PY_LEAVE_THREADS
}
extern "C" Gpoint change_font_size_editor_mode(struct item_list *current,float dx,float dy,int action)
{
  Gpoint point,points[2];
  struct table_chorn *pTo,*myto;
  extern struct table_chorn To_tab;
  struct table_text *pTt;
  extern struct table_text Tt_tab;
  float new_size;
  float dx0,dx1,dx2,dy0,dy1,dy2,tmp;
  char to[VCS_MX_NM_LEN],tb[VCS_MX_NM_LEN];
  Gextent extent;
  int delta,cont,first_pass,myaction,prev_delta=0;
  float increase;

  point=current->extent.ul;
  if (action<=0) return point;
  switch (current->type)
    {
    case(pe_text):
      strcpy(to,current->data.pet->to);
      strcpy(tb,current->data.pet->tb);
      break;
    case(pe_form):
      strcpy(to,current->data.pef->to);
      strcpy(tb,current->data.pef->tb);
      break;
    }
/*   printf("dealing with %s, %s\n",to,tb); */
  /* look for table orientation */

  for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
    if (strcmp(pTo->name,to) == 0) break;
  for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
    if (strcmp(pTt->name,tb) == 0) break;
  if (pTo==NULL)
    {
      return point ;
    }
/*   printf("ok we are altering: %s\n",current->attr_name); */
  /* goes to the end */
  myto = NULL;
  myto = (struct table_chorn *)malloc(sizeof(struct table_chorn));
  myto->chh = pTo->chh;
  increase = pTo->chh/5.-pTo->chh/17.;
  myto->chua = pTo->chua;
  myto->chpath=pTo->chpath;
  myto->chalh=pTo->chalh;
  myto->chalv=pTo->chalv;
  myto->next=NULL;
  if (strncmp(to,"uniq_",5)!=0)
    {
/*       printf("ok creating a new name for this guy here\n"); */
      while (pTo!=NULL)
	{
	  sprintf(to,"uniq_%i",rand());
	  for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
	    if (strcmp(pTo->name,to) == 0) break;
	}
    }
  strcpy(myto->name,to);
/*   printf("object to name: %s\n",myto->name); */

  switch (current->type)
    {
    case(pe_text):
      strcpy(current->data.pet->to,myto->name);
      break;
    case(pe_form):
      strcpy(current->data.pef->to,myto->name);
      break;
    }
/*   printf("Action %i on object intialized to:\n",action); */
/*   printf("height : %f\n",myto->chh); */
/*   printf("angle  : %f\n",myto->chua); */
/*   printf("path   : %c\n",myto->chpath); */
/*   printf("horiz  : %c\n",myto->chalh); */
/*   printf("vert   : %c\n",myto->chalv); */
  points[0].x=0.;
  points[0].y=0.;
  points[1].x=dx;
  points[1].y=dy;
  rotate_anchors(&points[0],2,myto->chua/180.*3.14159);
  dx=points[1].x;
  dy=points[1].y;
  points[0].x=0.;
  points[0].y=0.;
  points[1].x=current->extent.ur.x-current->extent.ll.x;;
  points[1].y=current->extent.ur.y-current->extent.ll.y;
  //rotate_anchors(&points,2,myto->chua/180.*3.14159);
  dy0 = fabs(points[1].y);
  if ((action==1) || (action==2) || (action==5)) {dy1 = dy0+dy;} /* moving from top dy = increase */
  if ((action==3) || (action==4) || (action==7)) {dy1 = dy0-dy;} /* moving from bottom dy = increase */
  dx0 = fabs(points[1].x);
  if ((action==2) || (action==3) || (action==6)) {dx1 = dx0+dx;} /* moving from the right dx = increase */
  if ((action==1) || (action==4) || (action==8)) {dx1 = dx0-dx;} /* moving from left dx = decrease */
  myaction=action;

  /* corrects if you did not select a corner */
  switch (myaction)
    {
    case(5): /* drag from top */
      myaction=2;
      dx1=dx0;
      break;
    case(6): /* drag from right side */
      myaction=2;
      dy1=dy0;
      break;
    case(7): /* drag from bottom */
      myaction=3;
      dx1=dx0;
      break;
    case(8): /* drag from left side */
      myaction=4;
      dy1=dy0;
      break;
    }

  if (dy1<0)
    { /* we went over in vertical direction */
/*       printf("flipping vertical ---------------------- %i, %f\n",myaction,dy1); */
      switch (myaction)
	{
	case(1):
	  myaction=4;
	  break;
	case(2):
	  myaction=3;
	  break;
	case(3):
	  myaction=2;
	  break;
	case(4):
	  myaction=1;
	  break;
	}
      dy1=-dy1;
    }
/*   printf("after flipping vertical ---------------------- %i\n",myaction); */
  if (dx1<0)
    { /* we went over in vertical direction */
/*       printf("flipping horiz  ---------------------- %i\n",myaction); */
      switch (myaction)
	{
	case(1):
	  myaction=2;
	  break;
	case(2):
	  myaction=1;
	  break;
	case(3):
	  myaction=4;
	  break;
	case(4):
	  myaction=3;
	  break;
	}
      dx1=-dx1;
    }
  /*   printf("after flipping horiz ---------------------- %i\n",myaction); */
  cont=1;
  first_pass=1;
  /* ok here we need to put code for angle */
  /* indeed we want to take into account rotation! */
  if ((-45>myto->chua) && (myto->chua>-135)){
    tmp=dx0;
    dx0=dy0;
    dy0=dx0;
    switch (myaction){
    case(1): /*TL*/
      myto->chalv='b';
      myto->chalh='l';
      break;
    case(2): /*TR*/
      myto->chalv='t';
      myto->chalh='l';
      break;
    case(3):/*BR*/
      myto->chalv='t';
      myto->chalh='r';
      break; 
    case(4):/*BL*/
      myto->chalv='b';
      myto->chalh='r';
      break;
    }
  }
  else if ((myto->chua<-135) || (myto->chua>135)){
    switch (myaction){
    case(1): /*TL*/
      myto->chalv='t';
      myto->chalh='l';
      break;
    case(2): /*TR*/
      myto->chalv='t';
      myto->chalh='r';
      break;
    case(3):/*BR*/
      myto->chalv='b';
      myto->chalh='r';
      break; 
    case(4):/*BL*/
      myto->chalv='b';
      myto->chalh='l';
      break;
    }
  }
  else if ((myto->chua>45)){
    tmp=dx0;
    dx0=dy0;
    dy0=dx0;
    switch (myaction){
    case(1): /*TL*/
      myto->chalv='t';
      myto->chalh='r';
      break;
    case(2): /*TR*/
      myto->chalv='b';
      myto->chalh='r';
      break;
    case(3):/*BR*/
      myto->chalv='b';
      myto->chalh='l';
      break; 
    case(4):/*BL*/
      myto->chalv='t';
      myto->chalh='l';
      break;
    }
  }
  else {
    switch (myaction){
    case(1): 
      myto->chalv='b';
      myto->chalh='r';
      break;
    case(2): 
      myto->chalv='b';
      myto->chalh='l';
      break;
    case(3):
      myto->chalv='t';
      myto->chalh='l';
      break;
    case(4): 
      myto->chalv='t';
      myto->chalh='r';
      break;
    }
  }
  switch (myaction) /* fixing on opposite corner of the one clicked */
    {
      case(3): /* top left */
	point=current->extent.ul;
	break;
      case(4): /* top right */
	point=current->extent.ur;
	break;
      case(1): /* bottom right */
	point=current->extent.lr;
	break;
      case(2): /* bottom left */
	point=current->extent.ll;
	break;
    }
/*   printf("We had  dy ,dx  at: %f,%f\n",dy,dx); */
/*   printf("We had  dy0,dx0 at: %f,%f\n",dy0,dx0); */
/*   printf("We had  dy1,dx1 at: %f,%f\n",dy1,dx1); */
  while (cont==1)
    {
/*       printf("first pass and cont:%i,%i\n",first_pass,cont); */
      set_text_attr(pTt,myto);     
#ifdef CAIRODRAW 
      cairogqtxx(cont,point,&current->string[0],&extent);
#else
      gqtxx(cont,point,&current->string[0],&extent);
#endif
      points[1].x=extent.ur.x-extent.ll.x;;
      points[1].y=extent.ur.y-extent.ll.y;
      rotate_anchors(&points[0],2,myto->chua/180.*3.14159);
      dy2 = fabs(points[1].y);
      dx2 = fabs(points[1].x);
      if (dy2==0)
	{
/* 	  printf("aborting dy2=0 string: %s\n",current->string); */
	  break;
	}
      if (dy2>dy0) 
	{
	  delta=-1;
	}
      else
	{
	  delta=1;
	}
      if (prev_delta==0) prev_delta=delta; /*initialize*/
      if (prev_delta!=delta) increase=.75*increase;
      if (increase<.0001) increase=.0001;

/*       printf("dx0, dy0 : %f, %f\n",dx0,dy0); */
/*       printf("dx2, dy2 : %f, %f\n",dx2,dy2); */
/*       printf("dx , dy, action  : %f, %f, %i\n",dx,dy,action); */
      if (first_pass==1) /* first we grow on ys to make fit perfectly */
	{
	  if (fabs(dy2-dy0)>0.001) /* not quite there let's keep going */
	    {
	      myto->chh=myto->chh+increase*delta;
	    }
	  else
	    {
	      first_pass=0;
	    }
	 }
      else /* ok the ys have been fit correctly now need to shrink xs to match as well */
	{
	  if (dx2>dx0) /* well ys fit but not x didn't want to go so much */
	    {
	      myto->chh=myto->chh-increase;	      
	    }	
	  else
	    {
	      cont=0; /* x and y now fit hurrah! */
	    }
	  if (myto->chh<0.001) cont=0; /* stop the loop it's shrinking! */
	}
    }
/*   printf("We have dy2,dx2 at: %f,%f\n",dy2,dx2); */
  chk_mov_To(myto);
  current->extent=extent;
/*   printf("alignement vh: %c,%c\n",myto->chalv,myto->chalh); */
  return point;
}

extern "C" void resize_or_move(PyVCScanvas_Object *self,
		    Gpoint pointA, 
		    Gpoint pointB,
		    int action,
		    struct  item_list   **selected_items,
		    int arrow_move)
{
    struct item_list *current = (struct item_list *)*selected_items;
    float x_movement, y_movement,dx,dy;
    float x_movement2, y_movement2;
    Gpoint point;
    int j,iprim,counter;
    extern struct table_fill        Tf_tab;
    struct table_fill               *tftab;
    extern struct table_line        Tl_tab;
    struct table_line               *tltab;
    extern struct table_mark        Tm_tab;
    struct table_mark               *tmtab;
    struct points_struct		*xptr=NULL, *yptr=NULL;
    struct array_segments   	*xpts=NULL, *ypts=NULL;
    char                            proj[256];
    Gpoint pxy;
    int found;
    Gextent myextent;
/*     void printextent(); */
    int within_buffer(Gpoint point, Gextent extent, float buffer);
    extern int update_ind;

  PY_ENTER_THREADS
    PY_GRAB_THREAD

    x_movement = nnorm(self, 0, (pointB.x - pointA.x));
    y_movement = nnorm(self, 1, (pointB.y - pointA.y));
    x_movement2=pointB.x-pointA.x;
    y_movement2=pointB.y-pointA.y;
    while (current != NULL)
    {
        switch (current->type)
        {
            case (pe_text):
	      point = change_font_size_editor_mode(current,
					   x_movement,
					   y_movement,
					   action
					   );
	      if (action>0)
		{
		  current->data.pet->x = nnorm(self,0,point.x);
		  current->data.pet->y = nnorm(self,1,point.y);
		}
	      else
		{
		  current->data.pet->x += x_movement;
		  current->data.pet->y += y_movement;
		}

	      break;
            case (pe_form):
	      point = change_font_size_editor_mode(current,
					   x_movement,
					   y_movement,
					   action
					   );
	      if (action>0)
		{
		  current->data.pef->x = nnorm(self,0,point.x);
		  current->data.pef->y = nnorm(self,1,point.y);
		}
	      else
		{
		  current->data.pef->x += x_movement;
		  current->data.pef->y += y_movement;
		}
                break;
            case (pe_x_tic):
                current->data.pext->y1 += y_movement;
                current->data.pext->y2 += y_movement;
                break;
            case (pe_y_tic):
                current->data.peyt->x1 += x_movement;
                current->data.peyt->x2 += x_movement;
                break;
            case (pe_x_lab):
                current->data.pexl->y += y_movement;
                break;
            case (pe_y_lab):
                current->data.peyl->x += x_movement;
                break;
            case (pe_box):
                switch (action)
                {
		case (0):   /* Move entire object as is */
                        if (current->data.peb->x1 < current->data.peb->x2) {
                            current->data.peb->x1 = nnorm(self, 0, current->extent.ll.x);
                            current->data.peb->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            current->data.peb->x2 = nnorm(self, 0, current->extent.ll.x);
                            current->data.peb->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        if (current->data.peb->y1 < current->data.peb->y2) {
                            current->data.peb->y1 = nnorm(self, 1, current->extent.ll.y);
                            current->data.peb->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            current->data.peb->y2 = nnorm(self, 1, current->extent.ll.y);
                            current->data.peb->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        break;
		case (1):   /* Resize from the top left corner */
                        if (current->data.peb->y1 < current->data.peb->y2) { 
                            if (current->y_reversed == 0)
                                current->data.peb->y2 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.peb->y2 = nnorm(self, 1, current->extent.ll.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.peb->y1 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.peb->y1 = nnorm(self, 1, current->extent.ll.y);
                        }
                        if (current->data.peb->x1 < current->data.peb->x2) { 
                            if (current->x_reversed == 0)
                                current->data.peb->x1 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.peb->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.peb->x2 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.peb->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        break;
		case (2):   /* Resize from the top right corner */
                        if (current->data.peb->y1 < current->data.peb->y2) { 
                            if (current->y_reversed == 0)
                                current->data.peb->y2 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.peb->y2 = nnorm(self, 1, current->extent.ll.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.peb->y1 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.peb->y1 = nnorm(self, 1, current->extent.ll.y);
                        }
                        if (current->data.peb->x1 < current->data.peb->x2) { 
                            if (current->x_reversed == 0)
                                current->data.peb->x2 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.peb->x2 = nnorm(self, 0, current->extent.ll.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.peb->x1 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.peb->x1 = nnorm(self, 0, current->extent.ll.x);
                        }
                        break;
		case (3):   /* Resize from the bottom right corner */
                        if (current->data.peb->y1 < current->data.peb->y2) { 
                            if (current->y_reversed == 0)
                                current->data.peb->y1 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.peb->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.peb->y2 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.peb->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        if (current->data.peb->x1 < current->data.peb->x2) { 
                            if (current->x_reversed == 0)
                                current->data.peb->x2 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.peb->x2 = nnorm(self, 0, current->extent.ll.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.peb->x1 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.peb->x1 = nnorm(self, 0, current->extent.ll.x);
                        }
                        break;
		case (4):   /* Resize from the bottom left corner */
                        if (current->data.peb->y1 < current->data.peb->y2) { 
                            if (current->y_reversed == 0)
                                current->data.peb->y1 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.peb->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.peb->y2 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.peb->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        if (current->data.peb->x1 < current->data.peb->x2) { 
                            if (current->x_reversed == 0)
                                current->data.peb->x1 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.peb->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.peb->x2 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.peb->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        break;
                    case (5):   /* Resize from the top */
                        if (current->data.peb->y1 < current->data.peb->y2) { 
                            if (current->y_reversed == 0)
                                current->data.peb->y2 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.peb->y2 = nnorm(self, 1, current->extent.ll.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.peb->y1 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.peb->y1 = nnorm(self, 1, current->extent.ll.y);
                        }
                        break;
                    case (6):   /* Resize from the right */
                        if (current->data.peb->x1 < current->data.peb->x2) { 
                            if (current->x_reversed == 0)
                                current->data.peb->x2 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.peb->x2 = nnorm(self, 0, current->extent.ll.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.peb->x1 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.peb->x1 = nnorm(self, 0, current->extent.ll.x);
                        }
                        break;
                    case (7):   /* Resize from the bottom */
                        if (current->data.peb->y1 < current->data.peb->y2) { 
                            if (current->y_reversed == 0)
                                current->data.peb->y1 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.peb->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.peb->y2 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.peb->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        break;
                    case (8):   /* Resize from the left */
                        if (current->data.peb->x1 < current->data.peb->x2) { 
                            if (current->x_reversed == 0)
                                current->data.peb->x1 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.peb->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.peb->x2 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.peb->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        break;
                }
                break;
            case (pe_line):
                switch (action)
                {
                    case (0):   /* Move entire object as is */
                        if (current->data.pel->x1 < current->data.pel->x2) {
                            current->data.pel->x1 = nnorm(self, 0, current->extent.ll.x);
                            current->data.pel->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            current->data.pel->x2 = nnorm(self, 0, current->extent.ll.x);
                            current->data.pel->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        if (current->data.pel->y1 < current->data.pel->y2) {
                            current->data.pel->y1 = nnorm(self, 1, current->extent.ll.y);
                            current->data.pel->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            current->data.pel->y2 = nnorm(self, 1, current->extent.ll.y);
                            current->data.pel->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        break;
                    case (1):   /* Resize from the top left corner */
                        if (current->data.pel->y1 < current->data.pel->y2) { 
                            if (current->y_reversed == 0)
                                current->data.pel->y2 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.pel->y2 = nnorm(self, 1, current->extent.ll.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.pel->y1 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.pel->y1 = nnorm(self, 1, current->extent.ll.y);
                        }
                        if (current->data.pel->x1 < current->data.pel->x2) { 
                            if (current->x_reversed == 0)
                                current->data.pel->x1 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.pel->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.pel->x2 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.pel->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        break;
                    case (2):   /* Resize from the top right corner */
                        if (current->data.pel->y1 < current->data.pel->y2) { 
                            if (current->y_reversed == 0)
                                current->data.pel->y2 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.pel->y2 = nnorm(self, 1, current->extent.ll.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.pel->y1 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.pel->y1 = nnorm(self, 1, current->extent.ll.y);
                        }
                        if (current->data.pel->x1 < current->data.pel->x2) { 
                            if (current->x_reversed == 0)
                                current->data.pel->x2 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.pel->x2 = nnorm(self, 0, current->extent.ll.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.pel->x1 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.pel->x1 = nnorm(self, 0, current->extent.ll.x);
                        }
                        break;
                    case (3):   /* Resize from the bottom right corner */
                        if (current->data.pel->y1 < current->data.pel->y2) { 
                            if (current->y_reversed == 0)
                                current->data.pel->y1 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.pel->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.pel->y2 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.pel->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        if (current->data.pel->x1 < current->data.pel->x2) { 
                            if (current->x_reversed == 0)
                                current->data.pel->x2 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.pel->x2 = nnorm(self, 0, current->extent.ll.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.pel->x1 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.pel->x1 = nnorm(self, 0, current->extent.ll.x);
                        }
                        break;
		case (4):   /* Resize from the bottom left corner */
                        if (current->data.pel->y1 < current->data.pel->y2) { 
                            if (current->y_reversed == 0)
                                current->data.pel->y1 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.pel->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.pel->y2 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.pel->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        if (current->data.pel->x1 < current->data.pel->x2) { 
                            if (current->x_reversed == 0)
                                current->data.pel->x1 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.pel->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.pel->x2 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.pel->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        break;
                    case (5):   /* Resize from the top */
                        if (current->data.pel->y1 < current->data.pel->y2) { 
                            if (current->y_reversed == 0)
                                current->data.pel->y2 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.pel->y2 = nnorm(self, 1, current->extent.ll.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.pel->y1 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.pel->y1 = nnorm(self, 1, current->extent.ll.y);
                        }
                        break;
                    case (6):   /* Resize from the right */
                        if (current->data.pel->x1 < current->data.pel->x2) { 
                            if (current->x_reversed == 0)
                                current->data.pel->x2 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.pel->x2 = nnorm(self, 0, current->extent.ll.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.pel->x1 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.pel->x1 = nnorm(self, 0, current->extent.ll.x);
                        }
                        break;
		case (7):   /* Resize from the bottom */
                        if (current->data.pel->y1 < current->data.pel->y2) { 
                            if (current->y_reversed == 0)
                                current->data.pel->y1 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.pel->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.pel->y2 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.pel->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        break;
                    case (8):   /* Resize from the left */
                        if (current->data.pel->x1 < current->data.pel->x2) { 
                            if (current->x_reversed == 0)
                                current->data.pel->x1 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.pel->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.pel->x2 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.pel->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        break;
                }
                break;
            case (pe_leg):
                switch (action)
                {
                    case (0):   /* Move entire object as is */
                        if (current->data.peleg->x1 < current->data.peleg->x2) {
                            current->data.peleg->x1 = nnorm(self, 0, current->extent.ll.x);
                            current->data.peleg->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            current->data.peleg->x2 = nnorm(self, 0, current->extent.ll.x);
                            current->data.peleg->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        if (current->data.peleg->y1 < current->data.peleg->y2) {
                            current->data.peleg->y1 = nnorm(self, 1, current->extent.ll.y);
                            current->data.peleg->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            current->data.peleg->y2 = nnorm(self, 1, current->extent.ll.y);
                            current->data.peleg->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        break;
                    case (1):   /* Resize from the top left corner */
                        if (current->data.peleg->y1 < current->data.peleg->y2) { 
                            if (current->y_reversed == 0)
                                current->data.peleg->y2 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.peleg->y2 = nnorm(self, 1, current->extent.ll.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.peleg->y1 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.peleg->y1 = nnorm(self, 1, current->extent.ll.y);
                        }
                        if (current->data.peleg->x1 < current->data.peleg->x2) { 
                            if (current->x_reversed == 0)
                                current->data.peleg->x1 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.peleg->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.peleg->x2 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.peleg->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        break;
                    case (2):   /* Resize from the top right corner */
                        if (current->data.peleg->y1 < current->data.peleg->y2) { 
                            if (current->y_reversed == 0)
                                current->data.peleg->y2 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.peleg->y2 = nnorm(self, 1, current->extent.ll.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.peleg->y1 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.peleg->y1 = nnorm(self, 1, current->extent.ll.y);
                        }
                        if (current->data.peleg->x1 < current->data.peleg->x2) { 
                            if (current->x_reversed == 0)
                                current->data.peleg->x2 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.peleg->x2 = nnorm(self, 0, current->extent.ll.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.peleg->x1 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.peleg->x1 = nnorm(self, 0, current->extent.ll.x);
                        }
                        break;
                    case (3):   /* Resize from the bottom right corner */
                        if (current->data.peleg->y1 < current->data.peleg->y2) { 
                            if (current->y_reversed == 0)
                                current->data.peleg->y1 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.peleg->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.peleg->y2 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.peleg->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        if (current->data.peleg->x1 < current->data.peleg->x2) { 
                            if (current->x_reversed == 0)
                                current->data.peleg->x2 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.peleg->x2 = nnorm(self, 0, current->extent.ll.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.peleg->x1 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.peleg->x1 = nnorm(self, 0, current->extent.ll.x);
                        }
                        break;
                    case (4):   /* Resize from the bottom left corner */
                        if (current->data.peleg->y1 < current->data.peleg->y2) { 
                            if (current->y_reversed == 0)
                                current->data.peleg->y1 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.peleg->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.peleg->y2 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.peleg->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        if (current->data.peleg->x1 < current->data.peleg->x2) { 
                            if (current->x_reversed == 0)
                                current->data.peleg->x1 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.peleg->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.peleg->x2 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.peleg->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        break;
                    case (5):   /* Resize from the top */
                        if (current->data.peleg->y1 < current->data.peleg->y2) { 
                            if (current->y_reversed == 0)
                                current->data.peleg->y2 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.peleg->y2 = nnorm(self, 1, current->extent.ll.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.peleg->y1 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.peleg->y1 = nnorm(self, 1, current->extent.ll.y);
                        }
                        break;
                    case (6):   /* Resize from the right */
                        if (current->data.peleg->x1 < current->data.peleg->x2) { 
                            if (current->x_reversed == 0)
                                current->data.peleg->x2 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.peleg->x2 = nnorm(self, 0, current->extent.ll.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.peleg->x1 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.peleg->x1 = nnorm(self, 0, current->extent.ll.x);
                        }
                        break;
                    case (7):   /* Resize from the bottom */
                        if (current->data.peleg->y1 < current->data.peleg->y2) { 
                            if (current->y_reversed == 0)
                                current->data.peleg->y1 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.peleg->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.peleg->y2 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.peleg->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        break;
                    case (8):   /* Resize from the left */
                        if (current->data.peleg->x1 < current->data.peleg->x2) { 
                            if (current->x_reversed == 0)
                                current->data.peleg->x1 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.peleg->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.peleg->x2 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.peleg->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        break;
                }
                break;
            case (pe_dsp):
                switch (action)
                {
                    case (0):   /* Move entire object as is */
                        if (current->data.pedsp->x1 < current->data.pedsp->x2) {
                            current->data.pedsp->x1 = nnorm(self, 0, current->extent.ll.x);
                            current->data.pedsp->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            current->data.pedsp->x2 = nnorm(self, 0, current->extent.ll.x);
                            current->data.pedsp->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        if (current->data.pedsp->y1 < current->data.pedsp->y2) {
                            current->data.pedsp->y1 = nnorm(self, 1, current->extent.ll.y);
                            current->data.pedsp->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            current->data.pedsp->y2 = nnorm(self, 1, current->extent.ll.y);
                            current->data.pedsp->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        break;
                    case (1):   /* Resize from the top left corner */
                        if (current->data.pedsp->y1 < current->data.pedsp->y2) { 
                            if (current->y_reversed == 0)
                                current->data.pedsp->y2 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.pedsp->y2 = nnorm(self, 1, current->extent.ll.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.pedsp->y1 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.pedsp->y1 = nnorm(self, 1, current->extent.ll.y);
                        }
                        if (current->data.pedsp->x1 < current->data.pedsp->x2) { 
                            if (current->x_reversed == 0)
                                current->data.pedsp->x1 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.pedsp->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.pedsp->x2 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.pedsp->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        break;
                    case (2):   /* Resize from the top right corner */
                        if (current->data.pedsp->y1 < current->data.pedsp->y2) { 
                            if (current->y_reversed == 0)
                                current->data.pedsp->y2 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.pedsp->y2 = nnorm(self, 1, current->extent.ll.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.pedsp->y1 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.pedsp->y1 = nnorm(self, 1, current->extent.ll.y);
                        }
                        if (current->data.pedsp->x1 < current->data.pedsp->x2) { 
                            if (current->x_reversed == 0)
                                current->data.pedsp->x2 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.pedsp->x2 = nnorm(self, 0, current->extent.ll.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.pedsp->x1 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.pedsp->x1 = nnorm(self, 0, current->extent.ll.x);
                        }
                        break;
                    case (3):   /* Resize from the bottom right corner */
                        if (current->data.pedsp->y1 < current->data.pedsp->y2) { 
                            if (current->y_reversed == 0)
                                current->data.pedsp->y1 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.pedsp->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.pedsp->y2 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.pedsp->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        if (current->data.pedsp->x1 < current->data.pedsp->x2) { 
                            if (current->x_reversed == 0)
                                current->data.pedsp->x2 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.pedsp->x2 = nnorm(self, 0, current->extent.ll.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.pedsp->x1 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.pedsp->x1 = nnorm(self, 0, current->extent.ll.x);
                        }
                        break;
                    case (4):   /* Resize from the bottom left corner */
                        if (current->data.pedsp->y1 < current->data.pedsp->y2) { 
                            if (current->y_reversed == 0)
                                current->data.pedsp->y1 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.pedsp->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.pedsp->y2 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.pedsp->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        if (current->data.pedsp->x1 < current->data.pedsp->x2) { 
                            if (current->x_reversed == 0)
                                current->data.pedsp->x1 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.pedsp->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.pedsp->x2 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.pedsp->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        break;
                    case (5):   /* Resize from the top */
                        if (current->data.pedsp->y1 < current->data.pedsp->y2) { 
                            if (current->y_reversed == 0)
                                current->data.pedsp->y2 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.pedsp->y2 = nnorm(self, 1, current->extent.ll.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.pedsp->y1 = nnorm(self, 1, current->extent.ul.y);
                            else
                                current->data.pedsp->y1 = nnorm(self, 1, current->extent.ll.y);
                        }
                        break;
                    case (6):   /* Resize from the right */
                        if (current->data.pedsp->x1 < current->data.pedsp->x2) { 
                            if (current->x_reversed == 0)
                                current->data.pedsp->x2 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.pedsp->x2 = nnorm(self, 0, current->extent.ll.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.pedsp->x1 = nnorm(self, 0, current->extent.lr.x);
                            else
                                current->data.pedsp->x1 = nnorm(self, 0, current->extent.ll.x);
                        }
                        break;
                    case (7):   /* Resize from the bottom */
                        if (current->data.pedsp->y1 < current->data.pedsp->y2) { 
                            if (current->y_reversed == 0)
                                current->data.pedsp->y1 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.pedsp->y1 = nnorm(self, 1, current->extent.ul.y);
                        }
                        else {
                            if (current->y_reversed == 0)
                                current->data.pedsp->y2 = nnorm(self, 1, current->extent.ll.y);
                            else
                                current->data.pedsp->y2 = nnorm(self, 1, current->extent.ul.y);
                        }
                        break;
                    case (8):   /* Resize from the left */
                        if (current->data.pedsp->x1 < current->data.pedsp->x2) { 
                            if (current->x_reversed == 0)
                                current->data.pedsp->x1 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.pedsp->x1 = nnorm(self, 0, current->extent.lr.x);
                        }
                        else {
                            if (current->x_reversed == 0)
                                current->data.pedsp->x2 = nnorm(self, 0, current->extent.ll.x);
                            else
                                current->data.pedsp->x2 = nnorm(self, 0, current->extent.lr.x);
                        }
                        break;
                }
                break;
        }
	if ((current->type==display_tab) && (action<100))
	  { /* we have primitive and we are resizing it */
	    /* (reshaping done on update_extent) */
	    if (strcmp(current->data.pd->type,"fillarea")==0)
	      {
		tftab = &Tf_tab;
		while (tftab != NULL) {
		  if (cmpncs(current->data.pd->g_name, tftab->name) == 0) break;
		  tftab = tftab->next;
		}
		if (tftab->priority == 0) break; /* do nothing */
		strcpy(proj,tftab->proj);
		set_viewport_and_worldcoordinate ( tftab->fvp, tftab->fwc,proj );
		xptr = tftab->fx; yptr = tftab->fy;
	      }
	    if (strcmp(current->data.pd->type,"line")==0)
	      {
		tltab = &Tl_tab;
		while (tltab != NULL) {
		  if (cmpncs(current->data.pd->g_name, tltab->name) == 0) break;
		  tltab = tltab->next;
		}
		if (tltab->priority == 0) break; /* do nothing */
		strcpy(proj,tltab->proj);
		set_viewport_and_worldcoordinate ( tltab->lvp, tltab->lwc,proj );
		xptr = tltab->lx; yptr = tltab->ly;
	      }
	    if (strcmp(current->data.pd->type,"marker")==0)
	      {
		tmtab = &Tm_tab;
		while (tmtab != NULL) {
		  if (cmpncs(current->data.pd->g_name, tmtab->name) == 0) break;
		  tmtab = tmtab->next;
		}
		if (tmtab->priority == 0) break; /* do nothing */
		strcpy(proj,tmtab->proj);
		set_viewport_and_worldcoordinate ( tmtab->mvp, tmtab->mwc,proj );
		xptr = tmtab->mx; yptr = tmtab->my;
	      }
	    dx=(current->extent.lr.x-current->extent.ll.x);
	    dy=(current->extent.ur.y-current->extent.lr.y);
	    xpts = xptr->ps;  ypts = yptr->ps;
/* 	    printf("coming in extent is:\n"); */
/* 	    printextent(current->extent); */
/* 	    printf("xmvt, ymvt are: %f,%f\n",x_movement,y_movement); */
	    myextent=current->extent;
/* 	   printf("or : %f, %f\n",x_movement2,y_movement2); */
	    switch (action){
	    case (0):
	      myextent.ul.x-=x_movement2;
	      myextent.ll.x-=x_movement2;
	      myextent.ul.y-=y_movement2;
	      myextent.ur.y-=y_movement2;
	      myextent.ur.x-=x_movement2;
	      myextent.lr.x-=x_movement2;
	      myextent.ll.y-=y_movement2;
	      myextent.lr.y-=y_movement2;
	      break;
	    case (1):   /* Resize from the top left corner */
	      myextent.ul.x-=x_movement2;
	      myextent.ll.x-=x_movement2;
	      myextent.ul.y-=y_movement2;
	      myextent.ur.y-=y_movement2;
	      break;
	    case (2):   /* Resize from the top right corner */
	      myextent.ur.x-=x_movement2;
	      myextent.lr.x-=x_movement2;
	      myextent.ul.y-=y_movement2;
	      myextent.ur.y-=y_movement2;
	      break;
	    case (3):   /* Resize from the bottom right corner */
	      myextent.ur.x-=x_movement2;
	      myextent.lr.x-=x_movement2;
	      myextent.ll.y-=y_movement2;
	      myextent.lr.y-=y_movement2;
	      break;
	    case (4):   /* Resize from the bottom left corner */
	      myextent.ul.x-=x_movement2;
	      myextent.ll.x-=x_movement2;
	      myextent.ll.y-=y_movement2;
	      myextent.lr.y-=y_movement2;
	      break;
	    case (5):   /* Resize from the top */
	      myextent.ul.y-=y_movement2;
	      myextent.ur.y-=y_movement2;
	      break;
	    case (6):   /* Resize from the right */
	      myextent.ur.x-=x_movement2;
	      myextent.lr.x-=x_movement2;
	      break;
	    case (7):   /* Resize from the bottom */
	      myextent.ll.y-=y_movement2;
	      myextent.lr.y-=y_movement2;
	      break;
	    case (8):   /* Resize from the left */
	      myextent.ul.x-=x_movement2;
	      myextent.ll.x-=x_movement2;
	      break;
	    }
/* 	    printf("computed original extent"); */
/* 	    printextent(myextent); */
/* 	    printextent(current->extent); */
	    for (iprim=0; iprim<xptr->nsegs; iprim++) {
/* 	      found=0; */
/* 	      for (j=0;j<xpts->npts;j++) { */
/* 		pxy.x=xpts->pts[j]; */
/* 		pxy.y=ypts->pts[j]; */
/* 		/\* goes to screen coordinates *\/ */
/* 		pxy=proj_convert(pxy); */
/* /\* 		printf("testing: %f,%f ",pxy.x,pxy.y); *\/ */
/* 		if (within_buffer(pxy,myextent,BUFFER)) { */
/* 		  found++; */
/* /\* 		  printf("good\n"); *\/ */
/* 		} */
/* /\* 		else printf("nope\n"); *\/ */
/* 	      } */
/* 	      printf("found: %i\n",found); */
	      if (iprim==current->sub_primitive)
		{
		  /* ok we are resizing this shape? */
		  for (j=0;j<xpts->npts;j++) {
		    pxy.x=xpts->pts[j];
		    pxy.y=ypts->pts[j];
		    /* goes to screen coordinates */
		    pxy=proj_convert(pxy);
		    switch (action){
		    case (0):
		      pxy.x+=x_movement2; 
		      pxy.y+=y_movement2;
		      break;
		    case (1):   /* Resize from the top left corner */
		      pxy.x=current->extent.lr.x-(current->extent.lr.x-pxy.x)/(dx+x_movement2)*dx;
		      pxy.y=(pxy.y-current->extent.ll.y)/(dy-y_movement2)*dy+current->extent.ll.y;
		      break;
		    case (2):   /* Resize from the top right corner */
		      pxy.x=(pxy.x-current->extent.ll.x)/(dx-x_movement2)*dx+current->extent.ll.x;
		      pxy.y=(pxy.y-current->extent.ll.y)/(dy-y_movement2)*dy+current->extent.ll.y;
		      break;
		    case (3):   /* Resize from the bottom right corner */
		      pxy.x=(pxy.x-current->extent.ll.x)/(dx-x_movement2)*dx+current->extent.ll.x;
		      pxy.y=current->extent.ur.y-(current->extent.ur.y-pxy.y)/(dy+y_movement2)*dy;
		      break;
		    case (4):   /* Resize from the bottom left corner */
		      pxy.x=current->extent.lr.x-(current->extent.lr.x-pxy.x)/(dx+x_movement2)*dx;
		      pxy.y=current->extent.ur.y-(current->extent.ur.y-pxy.y)/(dy+y_movement2)*dy;
		      break;
		    case (5):   /* Resize from the top */
/* 		      printf("before: y: %f, dy: %f, ymv: %f,ll: %f\n",pxy.y,dy,y_movement2,current->extent.ll.y); */
		      pxy.y=(pxy.y-current->extent.ll.y)/(dy-y_movement2)*dy+current->extent.ll.y;
/* 		      printf("after : y: %f\n",pxy.y); */
		      break;
		    case (6):   /* Resize from the right */
		      pxy.x=(pxy.x-current->extent.ll.x)/(dx-x_movement2)*dx+current->extent.ll.x;
		      break;
		    case (7):   /* Resize from the bottom */
		      pxy.y=current->extent.ur.y-(current->extent.ur.y-pxy.y)/(dy+y_movement2)*dy;
		      break;
		    case (8):   /* Resize from the left */
		      pxy.x=current->extent.lr.x-(current->extent.lr.x-pxy.x)/(dx+x_movement2)*dx;
		      break;
		    }
		    /* goes back to world coordiantes */
		    pxy=invert_proj_convert(pxy);
		    xpts->pts[j]=pxy.x;
		    ypts->pts[j]=pxy.y;
		  }
		}
	      xpts = xpts->next;
	      ypts = ypts->next;
	    }
	  }
        current = current->next;
    }
#ifndef TRUE
#define TRUE 1
#endif
    update_ind =TRUE;
    vcs_legacy_canvas_update(1);
  PY_RELEASE_THREAD
    PY_LEAVE_THREADS
}


void
get_text_fields(char		shold[29][MAX_NAME],
		char		sname[29][MAX_NAME],
		char		fname[29][MAX_NAME],
		char		xtname[29][MAX_NAME],
		char		ytname[29][MAX_NAME],
		char		xlname[29][MAX_NAME],
		char		ylname[29][MAX_NAME],
		char		bname[29][MAX_NAME],
		char		lname[29][MAX_NAME],
		struct a_tab *atab,
		struct p_tab *ptab)
{
    int i = 0;
    char month[13][10];
    cdCompTime  comptime;
    struct a_attr   *pA;

    strcpy(month[1],"January");
    strcpy(month[2],"February");
    strcpy(month[3],"March");
    strcpy(month[4],"April");
    strcpy(month[5],"May");
    strcpy(month[6],"June");
    strcpy(month[7],"July");
    strcpy(month[8],"August");
    strcpy(month[9],"September");
    strcpy(month[10],"October");
    strcpy(month[11],"November");
    strcpy(month[12],"December");

    pA = atab->pA_attr;
    /* Setup array shold to contain the labels of all text objects */
    for (i=0; i<29; i++) strcpy(shold[i], " "); /* Initialize shold[] */

    /* Text objects */
	if (atab->pA_attr->F != NULL)       strcpy(shold[0], atab->pA_attr->F);
        if (atab->pA_attr->f != NULL)       strcpy(shold[1], atab->pA_attr->f);
	if (atab->pA_attr->lmask != NULL)   strcpy(shold[2], atab->pA_attr->lmask);
        if (atab->pA_attr->trnf != NULL)    strcpy(shold[3], atab->pA_attr->trnf);
	if (atab->pA_attr->s != NULL)       strcpy(shold[4], atab->pA_attr->s);
	if (atab->pA_attr->n != NULL)       strcpy(shold[5], atab->pA_attr->n);
	if (atab->pA_attr->ti != NULL)      strcpy(shold[6], atab->pA_attr->ti);
	if (atab->pA_attr->u != NULL)       strcpy(shold[7], atab->pA_attr->u);
	if (atab->pA_attr->crd != NULL)     strcpy(shold[8], atab->pA_attr->crd);
	if (atab->pA_attr->crt != NULL)     strcpy(shold[9], atab->pA_attr->crt);
	if (atab->pA_attr->com1 != NULL)    strcpy(shold[10], atab->pA_attr->com1);
	if (atab->pA_attr->com2 != NULL)    strcpy(shold[11], atab->pA_attr->com2);
	if (atab->pA_attr->com3 != NULL)    strcpy(shold[12], atab->pA_attr->com3);
	if (atab->pA_attr->com4 != NULL)    strcpy(shold[13], atab->pA_attr->com4);
	if (atab->pA_attr->xn[0] != NULL)   strcpy(shold[14], atab->pA_attr->xn[0]);
	if (atab->pA_attr->xn[1] != NULL)   strcpy(shold[15], atab->pA_attr->xn[1]);
	if (atab->pA_attr->xn[2] != NULL)   strcpy(shold[16], atab->pA_attr->xn[2]);
	if (atab->pA_attr->xn[3] != NULL)   strcpy(shold[17], atab->pA_attr->xn[3]);
	if (atab->pA_attr->xu[0] != NULL)   strcpy(shold[18], atab->pA_attr->xu[0]);
	if (atab->pA_attr->xu[1] != NULL)   strcpy(shold[19], atab->pA_attr->xu[1]);
	if (atab->pA_attr->xu[2] != NULL)   strcpy(shold[20], atab->pA_attr->xu[2]);
	if (atab->pA_attr->xu[3] != NULL)   strcpy(shold[21], atab->pA_attr->xu[3]);

    /* Format objects */
    for (i = 0; i < 4; i++)
      {
        if ((pA->xn[i] != NULL) && ( cmpncs(pA->xn[i],"")!=0))
        {
/* 	  printf("xs %s\n",pA->xs[i]); */
/* 	  printf("xvfmt %s\n",ptab->xv.fmt); */
	  if (cmpncs("time",pA->xn[i]) != 0)
	    format(pA->xn[i],pA->xu[i],*pA->xv[i],ptab->xv.fmt,shold[22+i]);
	  else
            {
	      comptime.month = comptime.year = comptime.day = comptime.hour = 0;
	      cdRel2Comp(cd360, pA->xu[i], (double)*pA->xv[i] , &comptime);
	      if (comptime.month !=0)
		sprintf(shold[22+i], "%s/%d", month[comptime.month], comptime.year);
	      else
		format(pA->xn[i],pA->xu[i],*pA->xv[i],ptab->xv.fmt,shold[22+i]);
            }
        }
    }
    format("Mean",pA->u, pA->mean,ptab->mean.fmt,shold[26]);
    format("Max",pA->u, pA->max,ptab->max.fmt,shold[27]);
    format("Min",pA->u, pA->min,ptab->min.fmt,shold[28]);

    /* Save the text attribute names */
    strcpy(sname[0], "file");
    strcpy(sname[1], "function");
    strcpy(sname[2], "logicalmask");
    strcpy(sname[3], "transformation");
    strcpy(sname[4], "source");
    strcpy(sname[5], "dataname");
    strcpy(sname[6], "title");
    strcpy(sname[7], "units");
    strcpy(sname[8], "crdate");
    strcpy(sname[9], "crtime");
    strcpy(sname[10], "comment1");
    strcpy(sname[11], "comment2");
    strcpy(sname[12], "comment3");
    strcpy(sname[13], "comment4");
    strcpy(sname[14], "xname");
    strcpy(sname[15], "yname");
    strcpy(sname[16], "zname");
    strcpy(sname[17], "tname");
    strcpy(sname[18], "xunits");
    strcpy(sname[19], "yunits");
    strcpy(sname[20], "zunits");
    strcpy(sname[21], "tunits");

    /* Save the format attribute names */
    strcpy(fname[0], "xvalue");
    strcpy(fname[1], "yvalue");
    strcpy(fname[2], "zvalue");
    strcpy(fname[3], "tvalue");
    strcpy(fname[4], "mean");
    strcpy(fname[5], "max");
    strcpy(fname[6], "min");

    /* Save the X-Tick attribute names */
    strcpy(xtname[0], "xtic1");
    strcpy(xtname[1], "xtic2");
    strcpy(xtname[2], "xmintic1");
    strcpy(xtname[3], "xmintic2");

    /* Save the Y-Tick attribute names */
    strcpy(ytname[0], "ytic1");
    strcpy(ytname[1], "ytic2");
    strcpy(ytname[2], "ymintic1");
    strcpy(ytname[3], "ymintic2");

    /* Save the X-Label attribute names */
    strcpy(xlname[0], "xlabel1");
    strcpy(xlname[1], "xlabel2");

    /* Save the Y-Label attribute names */
    strcpy(ylname[0], "ylabel1");
    strcpy(ylname[1], "ylabel2");

    /* Save the Box attribute names */
    strcpy(bname[0], "box1");
    strcpy(bname[1], "box2");
    strcpy(bname[2], "box3");
    strcpy(bname[3], "box4");

    /* Save the line attribute names */
    strcpy(lname[0], "line1");
    strcpy(lname[1], "line2");
    strcpy(lname[2], "line3");
    strcpy(lname[3], "line4");

}



/* Check and see if the current item is already in the selected list */
extern "C" int in_list(struct  item_list *item,struct  item_list **selected_items)
{
    /*struct item_list    *current = (struct item_list *)*selected_items;*/
    struct item_list    *current;

    if (*selected_items == NULL) {
       return 0;
    }

    current = (struct item_list *)*selected_items;
    while (current != NULL)
    {
        if ((current->data).pet == (item->data).pet) {
            return 1;
        } else
            current = current->next;
    }
    return 0;
}
               
  extern "C" struct table_mark        Tm_tab;
  extern "C" struct a_tab    A_tab;
  extern "C" struct p_tab    Pic_tab;
  extern "C" struct display_tab  D_tab;
  extern "C" struct table_text Tt_tab;
  extern "C" struct table_chorn To_tab;
  extern "C" struct table_fill        Tf_tab;
  extern "C" struct table_line        Tl_tab;

/* If user clicked on a template object, return an item_list 
 * pointer to that object */
extern "C" struct item_list *select_item(PyVCScanvas_Object *self,
			      Gpoint point,
			      char *gui_template_name,
			      char *attr_name,
			      enum etypes search_only)
{
  int 		i, found = 0, priority=0,iprim;
  float       temp_closest,closest=100, fudge = 1.0e-5;
  float	val1, val2, value1, value2;
  struct table_text *pTt;
  struct table_chorn *pTo;
  struct a_tab    *atab;
  struct p_tab    *ptab;
  struct display_tab  *dtab;
  struct table_fill               *tftab;
  struct table_line               *tltab;
  struct table_mark               *tmtab;
  Gpoint      pxy;
  Gextent	 	extent;
  char		shold[29][MAX_NAME], sname[29][MAX_NAME], fname[29][MAX_NAME];
  char		xtname[29][MAX_NAME], ytname[29][MAX_NAME], xlname[29][MAX_NAME];
  char		ylname[29][MAX_NAME], bname[29][MAX_NAME], lname[29][MAX_NAME];
  struct pe_text 	*pet;
  struct pe_form 	*pef;
  struct pe_x_tic *pext;
  struct pe_y_tic *peyt;
  struct pe_x_lab *pexl;
  struct pe_y_lab *peyl;
  struct pe_box 	*peb;
  struct pe_line 	*pel;
  struct pe_leg 	*peleg;
  struct pe_dsp 	*pedsp;
  char                            proj[256];
  struct item_list *items[51],*prim_items,*prim_item,*prim_item2;
  struct item_list *selected=NULL, *temp_selected=NULL;
  int counter=0,position=0, temp_position=0,gui_flg=0,j;
  char * template_name = NULL;
  char   toname[1000], ttname[1000];
  struct table_text               *tttab;
  struct table_chorn              *totab;
  char *tpt,*tpo;
  struct points_struct		*xptr=NULL, *yptr=NULL;
  struct array_segments   	*xpts=NULL, *ypts=NULL;
  struct char_segments		*tx=NULL;
/*   void printextent(); */

  if (SCREEN_MODE == TEDITOR) {
    if (gui_template_name == NULL){
      template_name = return_template_name( self );
/*       if ((template_name = (char *) malloc(sizeof(char)+1)) == NULL) { */
/* 	PyErr_SetString(PyExc_TypeError, "No memory for the template name."); */
/* 	return NULL; */
/*       } */
/*       template_name = strcpy(template_name,"\0"); */
    }
    else {
      if ((template_name = (char *) malloc((strlen(gui_template_name)+1)*sizeof(char)+1)) == NULL) {
	PyErr_SetString(PyExc_TypeError, "No memory for the template name.");
	return NULL;
      }
      strcpy(template_name, gui_template_name);
      gui_flg = 1;
    }
  }
  else if (SCREEN_MODE == GMEDITOR) {
    dtab = &D_tab;
    while (dtab != NULL) {
      if ((dtab->wkst_id == self->wkst_id)){
	for (ptab = &Pic_tab; ptab != NULL; ptab = ptab->next)
	  if (strcmp(ptab->name,dtab->p_name) == 0) break;
	if (ptab!=NULL){
/* 	  printf("comparing with template: %s\n",dtab->p_name); */
	  /* ok we got the template ofthat display */
	  /* we need to see if the point is with this extent */
	  value1 = plnorm(0, (ptab->dsp.x1 - (float) fmod(ptab->dsp.x1, fudge)));
	  value2 = plnorm(0, (ptab->dsp.x2 - (float) fmod(ptab->dsp.x2, fudge)));
	  extent.ll.x = extent.ul.x = value1;
	  extent.lr.x = extent.ur.x = value2;
	  value1 = plnorm(1, (ptab->dsp.y1 - (float) fmod(ptab->dsp.y1, fudge)));
	  value2 = plnorm(1, (ptab->dsp.y2 - (float) fmod(ptab->dsp.y2, fudge)));
	  extent.ll.y = extent.lr.y = value1;
	  extent.ur.y = extent.ul.y = value2;
	  if (within(point,extent)){
	    selected = (struct item_list *)malloc(sizeof(struct item_list));
	    selected->extent=extent;
	    selected->type = display_tab;
	    selected->data.pd = dtab;
	    strcpy(selected->attr_name,dtab->name);
	    strcpy(selected->display_name, dtab->name );
	    selected->next=NULL;
	    return selected;
	  }
	}
      }
      dtab=dtab->next;
    }
    return selected;
  }
/*   printf("template name is: %s\n",template_name); */
/*   dtab = &D_tab; */
/*   while (dtab != NULL) */
/*     {printf("Dtab->name: %s\n",dtab->name); */
/*     dtab=dtab->next; */
/*     } */
  dtab = &D_tab;
  while (dtab != NULL)
    {
//       printf("Exploring display %s with template: %s\n",dtab->name,dtab->p_name);
      //      if ((dtab->wkst_id == self->wkst_id) && (dtab->a[0][0] != '\0'))
      if ((dtab->wkst_id == self->wkst_id))
	{
	  for (ptab = &Pic_tab; ptab != NULL; ptab = ptab->next)
	      if (strcmp(ptab->name,dtab->p_name) == 0) break;
// 	  printf("exploring template: %s, which i compare to %s\n",dtab->p_name,ptab->name);
	  if (strcmp(dtab->a[0],"\0")!=0)
	    {  /* Check for correct template */
// 	      printf("operator, i'm in!\n");
	      for (atab = &A_tab; atab != NULL; atab = atab->next){
// 		printf("template_name: %s, dtab->p_name: %s, atab->name: %s, dtab->a[0]: %s ---\n",ptab->name, dtab->p_name, atab->name, dtab->a[0]);
		/*		if (((SCREEN_MODE == DATA) || (strcmp(template_name,dtab->p_name) == 0)) && (strcmp(atab->name,dtab->a[0]) == 0)) break; */
		if (strcmp(atab->name,dtab->a[0]) == 0){
		    break;
		}
	      }
	      if (atab==NULL) break;
	      for (i=0; i < 51; i++)
		items[i] = NULL;
	      temp_selected=NULL;
	      get_text_fields(shold,sname,fname,xtname,ytname,xlname,ylname,bname,lname,atab,ptab);
	      counter = 0;
	      /* Get extent of all text objects and see if the mouse
	       * click occured on or within those boundaries */
	      if ((search_only == pe_text) || (search_only == pe_none)) {
		for (pet=&(ptab->F), i=0;i < 22;i++,pet++,counter++) {
		  if ( ((attr_name == NULL) || (strcmp(attr_name, sname[i]) == 0)) && (pet->p > 0)) {
		    for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
		      if (strcmp(pTt->name,pet->tb) == 0) break;
		    for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
		      if (strcmp(pTo->name,pet->to) == 0) break;
		    set_text_attr(pTt,pTo);
		    pxy.x = plnorm(0, (pet->x - (float) fmod(pet->x, fudge)));
		    pxy.y = plnorm(1, (pet->y - (float) fmod(pet->y, fudge)));
		    extent.ll.x = extent.ul.x = 0.0;
		    extent.lr.x = extent.ur.x = 0.0;
		    extent.ll.y = extent.lr.y = 0.0;
		    extent.ul.y = extent.ur.y = 0.0;
// 		    printf("cairogq\n");
#ifdef CAIRODRAW 
		    cairogqtxx(self->wkst_id, pxy, shold[i], &extent);
#else
		    gqtxx(self->wkst_id, pxy, shold[i], &extent);
#endif
// 		    printf("back\n");
/* 		    if (extent.ll.x == extent.lr.x) extent.lr.x = extent.ur.x; */
/* 		    if (extent.ll.y == extent.ul.y) extent.ul.y = extent.ur.y; */
		    items[counter] = (struct item_list *)malloc(sizeof(struct item_list));
		    items[counter]->next = NULL;
		    strcpy( items[counter]->attr_name, sname[i] );
		    strcpy( items[counter]->display_name, dtab->name );
// 		    printf("setting type to pe_text\n");
		    items[counter]->type = pe_text;
		    items[counter]->data.pet = pet;
		    items[counter]->ptab = ptab;
		    items[counter]->extent.ll.x = extent.ll.x;
		    items[counter]->extent.lr.x = extent.lr.x;
		    items[counter]->extent.ll.y = extent.ll.y;
		    items[counter]->extent.ul.y = extent.ul.y;
		    items[counter]->extent.ul.x = extent.ul.x;
		    items[counter]->extent.ur.x = extent.ur.x;
		    items[counter]->extent.lr.y = extent.lr.y;
		    items[counter]->extent.ur.y = extent.ur.y;
		    strcpy(items[counter]->string,shold[i]);
		    if (within(point,items[counter]->extent))
		      {
			temp_closest = distance(point,extent);
			if (temp_closest < closest || pet->p > priority)
			  {
			    closest = temp_closest;
			    priority = pet->p;
			    temp_selected = items[counter];
			    temp_position = counter;
			  }
		      }
		  }
		}
	      }
	      /* Get extent of all format objects and see if the mouse
	       * click occured on or within those boundaries */
	      if ((search_only == pe_form) || (search_only == pe_none)) {
		for (pef=&(ptab->xv),i=22;i < 29;i++,pef++,counter++) {
		  if ( ((attr_name == NULL) || (strcmp(attr_name, fname[i-22]) == 0)) && (pef->p > 0)) {
		    for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
		      if (strcmp(pTt->name,pef->tb) == 0) break;
		    for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
		      if (strcmp(pTo->name,pef->to) == 0) break;
		    set_text_attr(pTt,pTo);
		    pxy.x = plnorm(0, (pef->x - (float) fmod(pef->x, fudge)));
		    pxy.y = plnorm(1, (pef->y - (float) fmod(pef->y, fudge)));
		    extent.ll.x = extent.ul.x = 0.0;
		    extent.lr.x = extent.ur.x = 0.0;
		    extent.ll.y = extent.lr.y = 0.0;
		    extent.ul.y = extent.ur.y = 0.0;
#ifdef CAIRODRAW
		    cairogqtxx(self->wkst_id, pxy, shold[i], &extent);
#else
		    gqtxx(self->wkst_id, pxy, shold[i], &extent);
#endif
		    items[counter] = (struct item_list *)malloc(sizeof(struct item_list));
/* 		    printf("setting type to pe_form\n"); */
		    items[counter]->type = pe_form;
		    items[counter]->data.pef = pef;
		    items[counter]->ptab = ptab;
		    items[counter]->next = NULL;
		    strcpy( items[counter]->attr_name, fname[i-22] );
		    strcpy( items[counter]->display_name, dtab->name );
		    items[counter]->extent.ll.x = items[counter]->extent.ul.x = extent.ll.x;
		    items[counter]->extent.lr.x = items[counter]->extent.ur.x = extent.lr.x;
		    items[counter]->extent.ll.y = items[counter]->extent.lr.y = extent.ll.y;
		    items[counter]->extent.ul.y = items[counter]->extent.ur.y = extent.ul.y;
		    strcpy(items[counter]->string,shold[i]);
		    if (within(point,extent))
		      {
			temp_closest = distance(point,extent);
			if (temp_closest < closest || pef->p > priority)
			  {
			    closest = temp_closest;
			    priority = pef->p;
			    temp_selected = items[counter];
			    temp_position = counter;
			  }
		      }
		  }
		}
	      }
	      /* Get extent of all x-tic mark objects and see if the mouse
	       * click occured on or within those boundaries */
	      if ((search_only == pe_x_tic) || (search_only == pe_none)) {
		for (pext=&(ptab->xt1),i=0;i < 4;i++,pext++,counter++) {
		  if ( ((attr_name == NULL) || (strcmp(attr_name, xtname[i]) == 0)) && (pext->p > 0)) {
		    extent.ll.x = extent.ul.x = ptab->dsp.x1;
		    extent.lr.x = extent.ur.x = ptab->dsp.x2;
		    extent.ll.y = extent.lr.y = plnorm(1,(pext->y1 - (float) fmod(pext->y1, fudge)));
		    extent.ul.y = extent.ur.y = plnorm(1,(pext->y2 - (float) fmod(pext->y2, fudge)));
		    items[counter] = (struct item_list *)malloc(sizeof(struct item_list));
/* 		    printf("setting type to pe_x_tic\n"); */
		    items[counter]->type = pe_x_tic;
		    items[counter]->data.pext = pext;
		    items[counter]->ptab = ptab;
		    items[counter]->next = NULL;
		    strcpy( items[counter]->attr_name, xtname[i] );
		    strcpy( items[counter]->display_name, dtab->name );
		    items[counter]->extent.ll.x = items[counter]->extent.ul.x = extent.ll.x;
		    items[counter]->extent.lr.x = items[counter]->extent.ur.x = extent.lr.x;
		    items[counter]->extent.ll.y = items[counter]->extent.lr.y = extent.ll.y;
		    items[counter]->extent.ul.y = items[counter]->extent.ur.y = extent.ul.y;
		    if (within(point,items[counter]->extent))
		      {
			temp_closest = distance(point,extent);
			if (temp_closest < closest || pext->p > priority)
			  {
			    closest = temp_closest;
			    priority = pext->p;
			    temp_selected = items[counter];
			    temp_position = counter;
			  }
		      }
		  }
		}
	      }
	      
	      /* Get extent of all y-tic mark objects and see if the mouse
                * click occured on or within those boundaries */
	      if ((search_only == pe_y_tic) || (search_only == pe_none)) {
		for (peyt=&(ptab->yt1),i=0;i < 4;i++,peyt++,counter++) {
		  if ( ((attr_name == NULL) || (strcmp(attr_name, ytname[i]) == 0)) && (peyt->p > 0)) {
		    extent.ll.x = extent.ul.x = plnorm(0,(peyt->x1 - (float) fmod(peyt->x1, fudge)));
		    extent.lr.x = extent.ur.x = plnorm(0,(peyt->x2 - (float) fmod(peyt->x2, fudge)));
		    extent.ll.y = extent.lr.y = ptab->dsp.y1;
		    extent.ul.y = extent.ur.y = ptab->dsp.y2;
		    items[counter] = (struct item_list *)malloc(sizeof(struct item_list));
		    items[counter]->next = NULL;
		    strcpy( items[counter]->display_name, dtab->name );
		    strcpy( items[counter]->attr_name, ytname[i] );
// 		    printf("setting type to pe_y_tic\n");
		    items[counter]->type = pe_y_tic;
		    items[counter]->data.peyt = peyt;
		    items[counter]->ptab = ptab;
		    items[counter]->extent.ll.x = items[counter]->extent.ul.x = extent.ll.x;
		    items[counter]->extent.lr.x = items[counter]->extent.ur.x = extent.lr.x;
		    items[counter]->extent.ll.y = items[counter]->extent.lr.y = extent.ll.y;
		    items[counter]->extent.ul.y = items[counter]->extent.ur.y = extent.ul.y;
		    if (within(point,items[counter]->extent))
		      {
			found = 1;
			temp_closest = distance(point,extent);
			if (temp_closest < closest || peyt->p > priority)
			  {
			    closest = temp_closest;
			    priority = peyt->p;
			    temp_selected = items[counter];
			    temp_position = counter;
			  }
		      }
		  }
		}
	      }
        
	      /* Get extent of all x-label objects and see if the mouse
	       * click occured on or within those boundaries */
	      if ((search_only == pe_x_lab) || (search_only == pe_none)) {
	        for (pexl=&(ptab->xl1),i=0;i < 2;i++,pexl++,counter++) {
		  if ( ((attr_name == NULL) || (strcmp(attr_name, xlname[i]) == 0)) && (pexl->p > 0)) {
		    for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
		      if (strcmp(pTt->name,pexl->tb) == 0) break;
		    for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
		      if (strcmp(pTo->name,pexl->to) == 0) break;
		    set_text_attr(pTt,pTo);
		    pxy.x = ptab->dsp.x1;
		    pxy.y = plnorm(1, (pexl->y - (float) fmod(pexl->y, fudge)));
		    extent.ll.x = extent.ul.x = 0.0;
		    extent.lr.x = extent.ur.x = 0.0;
		    extent.ll.y = extent.lr.y = 0.0;
		    extent.ul.y = extent.ur.y = 0.0;
#ifdef CAIRODRAW
		    cairogqtxx(self->wkst_id, pxy, "120", &extent);
#else
		    gqtxx(self->wkst_id, pxy, "120", &extent);
#endif
		    extent.lr.x = extent.ur.x = ptab->dsp.x2;
		    items[counter] = (struct item_list *)malloc(sizeof(struct item_list));
		    items[counter]->next = NULL;
		    strcpy( items[counter]->attr_name, xlname[i] );
		    strcpy( items[counter]->display_name, dtab->name );
// 		    printf("setting type to pe_x_lab\n");
		    items[counter]->type = pe_x_lab;
		    items[counter]->data.pexl = pexl;
		    items[counter]->ptab = ptab;
		    items[counter]->extent.ll.x = items[counter]->extent.ul.x = extent.ll.x;
		    items[counter]->extent.lr.x = items[counter]->extent.ur.x = extent.lr.x;
		    items[counter]->extent.ll.y = items[counter]->extent.lr.y = extent.ll.y;
		    items[counter]->extent.ul.y = items[counter]->extent.ur.y = extent.ul.y;
		    if (within(point,items[counter]->extent))
		      {
			temp_closest = distance(point,extent);
			if (temp_closest < closest || pexl->p > priority)
			  {
			    closest = temp_closest;
			    priority = pexl->p;
			    temp_selected = items[counter];
			    temp_position = counter;
			  }
		      }
		  }
		}
	      }
        
	      /* Get extent of all y-label objects and see if the mouse
	       * click occured on or within those boundaries */
	      if ((search_only == pe_y_lab) || (search_only == pe_none)) {
		for (peyl=&(ptab->yl1),i=0;i < 2;i++,peyl++,counter++) {
		  if ( ((attr_name == NULL) || (strcmp(attr_name, ylname[i]) == 0)) && (peyl->p > 0)) {
		    for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
		      if (strcmp(pTt->name,peyl->tb) == 0) break;
		    for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
		      if (strcmp(pTo->name,peyl->to) == 0) break;
		    set_text_attr(pTt,pTo);
		    pxy.x = plnorm(0, (peyl->x - (float) fmod(peyl->x, fudge)));
		    pxy.y = ptab->dsp.y1;
		    extent.ll.x = extent.ul.x = 0.0;
		    extent.lr.x = extent.ur.x = 0.0;
		    extent.ll.y = extent.lr.y = 0.0;
		    extent.ul.y = extent.ur.y = 0.0;
#ifdef CAIRODRAW
		    cairogqtxx(self->wkst_id, pxy, "120", &extent);
#else
		    gqtxx(self->wkst_id, pxy, "120", &extent);
#endif
		    extent.ul.y = extent.ur.y = ptab->dsp.y2;
		    items[counter] = (struct item_list *)malloc(sizeof(struct item_list));
		    items[counter]->next = NULL;
		    strcpy( items[counter]->attr_name, ylname[i] );
		    strcpy( items[counter]->display_name, dtab->name );
// 		    printf("setting type to pe_ylab\n");
		    items[counter]->type = pe_y_lab;
		    items[counter]->data.peyl = peyl;
		    items[counter]->ptab = ptab;
		    items[counter]->extent.ll.x = items[counter]->extent.ul.x = extent.ll.x;
		    items[counter]->extent.lr.x = items[counter]->extent.ur.x = extent.lr.x;
		    items[counter]->extent.ll.y = items[counter]->extent.lr.y = extent.ll.y;
		    items[counter]->extent.ul.y = items[counter]->extent.ur.y = extent.ul.y;
		    if (within(point,extent))
		      {
			found = 1;
			temp_closest = distance(point,extent);
			if (temp_closest < closest || peyl->p > priority)
			  {
			    closest = temp_closest;
			    priority = peyl->p;
			    temp_selected = items[counter];
			    temp_position = counter;
			  }
		      }
		  }
		}
	      }
	      
	      /* Get extent of all box objects and see if the mouse
	       * click occured on or within those boundaries */
	      if ((search_only == pe_box) || (search_only == pe_none)) {
		for (peb=&(ptab->b1),i=0;i < 4;i++,peb++,counter++) {
		  if ( ((attr_name == NULL) || (strcmp(attr_name, bname[i]) == 0)) && (peb->p > 0)) {
		    items[counter] = (struct item_list *)malloc(sizeof(struct item_list));
		    items[counter]->x_reversed = items[counter]->y_reversed = 0;
		    value1 = plnorm(0, (peb->x1 - (float) fmod(peb->x1, fudge)));
		    value2 = plnorm(0, (peb->x2 - (float) fmod(peb->x2, fudge)));
		    if (peb->x1 < peb->x2) {
		      items[counter]->extent.ll.x = items[counter]->extent.ul.x = value1;
		      items[counter]->extent.lr.x = items[counter]->extent.ur.x = value2;
		    }
		    else {
		      items[counter]->extent.ll.x = items[counter]->extent.ul.x = value2;
		      items[counter]->extent.lr.x = items[counter]->extent.ur.x = value1;
		    }
		    value1 = plnorm(1, (peb->y1 - (float) fmod(peb->y1, fudge)));
		    value2 = plnorm(1, (peb->y2 - (float) fmod(peb->y2, fudge)));
		    if (peb->y1 < peb->y2) {
		      items[counter]->extent.ll.y = items[counter]->extent.lr.y = value1;
		      items[counter]->extent.ul.y = items[counter]->extent.ur.y = value2;
		    }
		    else {
		      items[counter]->extent.ll.y = items[counter]->extent.lr.y = value2;
		      items[counter]->extent.ul.y = items[counter]->extent.ur.y = value1;
		    }
		    items[counter]->next = NULL;
		    strcpy( items[counter]->attr_name, bname[i] );
		    strcpy( items[counter]->display_name, dtab->name );
// 		    printf("setting type to pe_box\n"); 
		    items[counter]->type = pe_box;
		    items[counter]->data.peb= peb;
		    items[counter]->ptab = ptab;
		    if (within(point,items[counter]->extent))
		      {
			temp_closest = distance(point,items[counter]->extent);
			if (temp_closest < closest || peb->p > priority)
			  {
			    closest = temp_closest;
			    priority = peb->p;
			    temp_selected = items[counter];
			    temp_position = counter;
			  }
		      }
		  }
		}
	      }
	      
	      /* Get extent of all line objects and see if the mouse
	       * click occured on or within those boundaries. Since in 
	       * some cases it is impossible to actually click on the 
	       * line value, I've added a slight buffer. */
	      if ((search_only == pe_line) || (search_only == pe_none)) {
		for (pel=&(ptab->l1),i=0;i < 4;i++,pel++,counter++) {
		  if ( ((attr_name == NULL) || (strcmp(attr_name, lname[i]) == 0)) && (pel->p > 0)) {
		    items[counter] = (struct item_list *)malloc(sizeof(struct item_list));
		    items[counter]->x_reversed = items[counter]->y_reversed = 0;
		    if (pel->x1 < pel->x2) {
		      val1 = pel->x1-BUFFER; val2 = pel->x2+BUFFER;
		      value1 = plnorm(0, (val1 - (float) fmod(val1, fudge)));
		      value2 = plnorm(0, (val2 - (float) fmod(val2, fudge)));
		      items[counter]->extent.ll.x = items[counter]->extent.ul.x = value1;
		      items[counter]->extent.lr.x = items[counter]->extent.ur.x = value2;
		    }
		    else {
		      val1 = pel->x1+BUFFER; val2 = pel->x2-BUFFER;
		      value1 = plnorm(0, (val1 - (float) fmod(val1, fudge)));
		      value2 = plnorm(0, (val2 - (float) fmod(val2, fudge)));
		      items[counter]->extent.ll.x = items[counter]->extent.ul.x = value2;
		      items[counter]->extent.lr.x = items[counter]->extent.ur.x = value1;
		    }
		    if (pel->y1 < pel->y2) {
		      val1 = pel->y1-BUFFER; val2 = pel->y2+BUFFER;
		      value1 = plnorm(1, (val1 - (float) fmod(val1, fudge)));
		      value2 = plnorm(1, (val2 - (float) fmod(val2, fudge)));
		      items[counter]->extent.ll.y = items[counter]->extent.lr.y = value1;
		      items[counter]->extent.ul.y = items[counter]->extent.ur.y = value2;
		    }
		    else {
		      val1 = pel->y1+BUFFER; val2 = pel->y2-BUFFER;
		      value1 = plnorm(1, (val1 - (float) fmod(val1, fudge)));
		      value2 = plnorm(1, (val2 - (float) fmod(val2, fudge)));
		      items[counter]->extent.ll.y = items[counter]->extent.lr.y = value2;
		      items[counter]->extent.ul.y = items[counter]->extent.ur.y = value1;
		    }
		    items[counter]->next = NULL;
		    strcpy( items[counter]->attr_name, lname[i] );
		    strcpy( items[counter]->display_name, dtab->name );
// 		    printf("setting type to pe_line\n"); 
		    items[counter]->type = pe_line;
		    items[counter]->data.pel = pel;
		    items[counter]->ptab = ptab;
		    if (within(point,items[counter]->extent))
		      {
			temp_closest = distance(point,items[counter]->extent);
			if (temp_closest < closest || pel->p > priority)
			  {
			    closest = temp_closest;
			    priority = pel->p;
			    temp_selected = items[counter];
			    temp_position = counter;
			  }
		      }
		  }
		}
	      }
	      
	      /* Check and see if the legend was selected */
	      if ((search_only == pe_leg) || (search_only == pe_none)) {
		peleg = &(ptab->leg);
		if (peleg->p > 0) {
		  items[counter] = (struct item_list *)malloc(sizeof(struct item_list));
		  items[counter]->x_reversed = items[counter]->y_reversed = 0;
		  value1 = plnorm(0, (peleg->x1 - (float) fmod(peleg->x1, fudge)));
		  value2 = plnorm(0, (peleg->x2 - (float) fmod(peleg->x2, fudge)));
		  if (peleg->x1 < peleg->x2) {
		    items[counter]->extent.ll.x = items[counter]->extent.ul.x = value1;
		    items[counter]->extent.lr.x = items[counter]->extent.ur.x = value2;
		  }
		  else {
		    items[counter]->extent.ll.x = items[counter]->extent.ul.x = value2;
		    items[counter]->extent.lr.x = items[counter]->extent.ur.x = value1;
		  }
		  value1 = plnorm(1, (peleg->y1 - (float) fmod(peleg->y1, fudge)));
		  value2 = plnorm(1, (peleg->y2 - (float) fmod(peleg->y2, fudge)));
		  if (peleg->y1 < peleg->y2) {
		    items[counter]->extent.ll.y = items[counter]->extent.lr.y = value1;
		    items[counter]->extent.ul.y = items[counter]->extent.ur.y = value2;
		  }
		  else {
		    items[counter]->extent.ll.y = items[counter]->extent.lr.y = value2;
		    items[counter]->extent.ul.y = items[counter]->extent.ur.y = value1;
		  }
		  items[counter]->next = NULL;
		  strcpy( items[counter]->attr_name, "legend" );
		  strcpy( items[counter]->display_name, dtab->name );
// 		  printf("setting type to pe_leg\n");
		  items[counter]->type = pe_leg;
		  items[counter]->data.peleg = peleg;
		  items[counter]->ptab = ptab;
		  if (within(point,items[counter]->extent))
                    {
		      temp_closest = distance(point,items[counter]->extent);
		      if (temp_closest < closest || peleg->p > priority)
			{
			  closest = temp_closest;
			  priority = peleg->p;
			  temp_selected = items[counter];
			  temp_position = counter;
			}
                    }
		}
		counter++;
	      }
	      
	      /* Check and see if the data was selected */
// 	      printf("going to the data part test\n");
	      if ((search_only == pe_dsp) || (search_only == pe_none)) {
		pedsp = &(ptab->dsp);
		if (pedsp->p > 0) {
		  items[counter] = (struct item_list *)malloc(sizeof(struct item_list));
		  items[counter]->x_reversed = items[counter]->y_reversed = 0;
		  value1 = plnorm(0, (pedsp->x1 - (float) fmod(pedsp->x1, fudge)));
		  value2 = plnorm(0, (pedsp->x2 - (float) fmod(pedsp->x2, fudge)));
// 		  printf("plnormed: %f, %f\n",pedsp->x1,pedsp->x2);
		  if (pedsp->x1 < pedsp->x2) {
		    items[counter]->extent.ll.x = items[counter]->extent.ul.x = value1;
		    items[counter]->extent.lr.x = items[counter]->extent.ur.x = value2;
		  }
		  else {
		    items[counter]->extent.ll.x = items[counter]->extent.ul.x = value2;
		    items[counter]->extent.lr.x = items[counter]->extent.ur.x = value1;
		  }
		  value1 = plnorm(1, (pedsp->y1 - (float) fmod(pedsp->y1, fudge)));
		  value2 = plnorm(1, (pedsp->y2 - (float) fmod(pedsp->y2, fudge)));
// 		  printf("plnormed: %f, %f\n",pedsp->y1,pedsp->y2);
		  if (pedsp->y1 < pedsp->y2) {
		    items[counter]->extent.ll.y = items[counter]->extent.lr.y = value1;
		    items[counter]->extent.ul.y = items[counter]->extent.ur.y = value2;
		  }
		  else {
		    items[counter]->extent.ll.y = items[counter]->extent.lr.y = value2;
		    items[counter]->extent.ul.y = items[counter]->extent.ur.y = value1;
		  }
		  items[counter]->next = NULL;
		  strcpy( items[counter]->attr_name, "data" );
		  strcpy( items[counter]->display_name, dtab->name );
// 		  printf("setting type to pe_dsp\n"); 
		  items[counter]->type = pe_dsp;
		  items[counter]->data.pedsp = pedsp;
		  items[counter]->ptab = ptab;
		  if (within(point,items[counter]->extent))
                    {
		      temp_closest = distance(point,items[counter]->extent);
		      if (temp_closest <= closest || pedsp->p >= priority)
			{
			  closest = temp_closest;
			  priority = pedsp->p;
			  temp_selected = items[counter];
			  temp_position = counter;
			}
                    }
		}
		counter++;
	      }

	      /* If we found an object that is closer than the
	       * previously selected item, remove items from list */
	      if (selected != NULL && temp_selected != NULL)
		delete_list(&selected);
	      if (selected == NULL && temp_selected != NULL) {
		selected = temp_selected;
		if ( (temp_selected->type == pe_dsp) && (search_only == pe_none) ) {
		  items[50] = NULL;
		  if (items[41] != NULL)
		    {
		      temp_selected->next = items[41];
		      items[41] = NULL;
		      temp_selected = temp_selected->next;
		    }
		  for (i = 14; i <= 17; i++)
		    {
		      if (items[i] != NULL)
			{
			  temp_selected->next = items[i];
			  items[i] = NULL;
			  temp_selected = temp_selected->next;
			}
		    }
		  for (i = 29; i <= 40; i++)
		    {
		      if (items[i] != NULL)
			{
			  temp_selected->next = items[i];
			  items[i] = NULL;
			  temp_selected = temp_selected->next;
			}
		    }
		} else {
		  for (i = 0; i < 51; i++) {
		    if (&items[i]->data == &selected->data)
		      items[i] = NULL;
		  }
		}
	      }
	      for (i = 0; i < 51; i++) {
		if (items[i] != NULL) {
		  free(items[i]);
		  items[i] = NULL;
		}
	      }
	    }
	}
      dtab = dtab->next;
      /* Find the correct display for data. Break out of loop when found. */
      if ((SCREEN_MODE == DATA) && (selected != NULL)) {
// 	printf("and doing a break\n");
	break;
      } 
    }
  temp_selected=NULL;
  /* C. Doutriaux addition trying to catch primitives */
  /* text ones in the first time */
  if (SCREEN_MODE==TEDITOR)
    {
      dtab = &D_tab;
      while (dtab != NULL) {
	if (dtab->wkst_id == self->wkst_id) {
	  /* text object ? */
	  if (strcmp(dtab->type,"text")==0){
/* 	    printf("in text\n"); */
	    /* the text table & orientation name and structs */
	    strcpy(ttname, dtab->g_name);
	    tpt = strtok(ttname, ":::");
	    strcpy(toname, dtab->g_name);
	    tpo = strstr(toname, ":::")+3;
	
	    tttab = &Tt_tab;
	    while (tttab != NULL) {
	      if (cmpncs(tpt, tttab->name) == 0) break;
	      tttab = tttab->next;
	    }
	    if (tttab->ts == NULL) break;    /* do nothing */
	    if (tttab->priority == 0) break; /* do nothing */
	    strcpy(proj,tttab->proj);
	    set_viewport_and_worldcoordinate ( tttab->tvp, tttab->twc,proj );
	    totab = &To_tab;
	    while (totab != NULL) { 
	      if (cmpncs(tpo, totab->name) == 0) break;
	      totab = totab->next;
	    }
	    set_text_attr(tttab,totab);
	    xptr = tttab->tx; yptr = tttab->ty;
	    xpts = xptr->ps;  ypts = yptr->ps;
	    tx = tttab->ts->ss;
	    prim_items=NULL;
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
		if (extent.ll.x == extent.lr.x) extent.lr.x = extent.ur.x;
		if (extent.ll.y == extent.ul.y) extent.ul.y = extent.ur.y;
		prim_item = (struct item_list *)malloc(sizeof(struct item_list));
		prim_item->sub_primitive=iprim;
		prim_item->next = NULL;
		strcpy( prim_item->attr_name, dtab->name );
		/* 	    printf("setting type to display_tab in text\n"); */
		prim_item->type = display_tab;
		prim_item->data.pd = dtab;
		prim_item->ptab = NULL; /* no template here */
/* 		prim_item->extent.ll.x = prim_item->extent.ul.x = extent.ll.x; */
/* 		prim_item->extent.lr.x = prim_item->extent.ur.x = extent.lr.x; */
/* 		prim_item->extent.ll.y = prim_item->extent.lr.y = extent.ll.y; */
/* 		prim_item->extent.ul.y = prim_item->extent.ur.y = extent.ul.y; */
		prim_item->extent.ll.x = extent.ll.x;
		prim_item->extent.lr.x = extent.lr.x;
		prim_item->extent.ll.y = extent.ll.y;
		prim_item->extent.ul.y = extent.ul.y;
		prim_item->extent.ul.x = extent.ul.x;
		prim_item->extent.ur.x = extent.ur.x;
		prim_item->extent.lr.y = extent.lr.y;
		prim_item->extent.ur.y = extent.ur.y;
		strcpy(prim_item->string,(const char *)tx->cpts);
		if (prim_items==NULL)
		  prim_items=prim_item;
		else
		  {
		    prim_item2=prim_items;
		    while(prim_item2->next!=NULL) prim_item2=prim_item2->next;
		    prim_item2->next=prim_item;
		  }
		tx=tx->next;
		if (within(point,prim_item->extent)) {
		  temp_closest = distance(point,extent);
		  if (temp_closest < closest || tttab->priority > priority)
		    {
		      closest = temp_closest;
		      priority = tttab->priority;
		      temp_selected = prim_items;
		    }
		}
	      }
	      xpts = xpts->next;
	      ypts = ypts->next;
	    }
	  }
	  if (strcmp(dtab->type,"fillarea")==0){
	    prim_items=NULL;
	    tftab = &Tf_tab;
	    while (tftab != NULL) {
	      if (cmpncs(dtab->g_name, tftab->name) == 0) break;
	      tftab = tftab->next;
	    }
	    if (tftab->priority == 0) break; /* do nothing */
	    strcpy(proj,tftab->proj);
	    set_viewport_and_worldcoordinate ( tftab->fvp, tftab->fwc,proj );
	    xptr = tftab->fx; yptr = tftab->fy;
	    xpts = xptr->ps;  ypts = yptr->ps;
	    prim_items=NULL;
	    for (iprim=0; iprim<xptr->nsegs; iprim++) {
	      prim_item = (struct item_list *)malloc(sizeof(struct item_list));
	      prim_item->next = NULL;
	      prim_item->sub_primitive=iprim;
	      strcpy( prim_item->attr_name, dtab->name );
	      strcpy( prim_item->string, "" );
	      /* 	  printf("setting type to display_tab in fillarea\n"); */
	      prim_item->type = display_tab;
	      prim_item->data.pd = dtab;
	      prim_item->ptab = NULL; /* no template here */
	      extent.ll.x=extent.ll.y=extent.lr.y=extent.ul.x=1.E20;
	      extent.ur.x=extent.ur.y=extent.lr.x=extent.ul.y=-1.E20;
	      for (j=0;j<xpts->npts;j++) {
		pxy.x=xpts->pts[j];
		pxy.y=ypts->pts[j];
		pxy=proj_convert(pxy);
		if (pxy.x<extent.ll.x) extent.ll.x=pxy.x;
		if (pxy.y<extent.ll.y) extent.ll.y=pxy.y;
		if (pxy.x>extent.lr.x) extent.lr.x=pxy.x;
		if (pxy.y<extent.lr.y) extent.lr.y=pxy.y;
		if (pxy.x>extent.ur.x) extent.ur.x=pxy.x;
		if (pxy.y>extent.ur.y) extent.ur.y=pxy.y;
		if (pxy.x<extent.ul.x) extent.ul.x=pxy.x;
		if (pxy.y>extent.ul.y) extent.ul.y=pxy.y;
	      }
	      xpts = xpts->next;
	      ypts = ypts->next;
	      prim_item->extent.ll.x = prim_item->extent.ul.x = extent.ll.x;
	      prim_item->extent.lr.x = prim_item->extent.ur.x = extent.lr.x;
	      prim_item->extent.ll.y = prim_item->extent.lr.y = extent.ll.y;
	      prim_item->extent.ul.y = prim_item->extent.ur.y = extent.ul.y;
	      if (prim_items==NULL)
		prim_items=prim_item;
	      else
		{
		  prim_item2=prim_items;
		  while(prim_item2->next!=NULL) prim_item2=prim_item2->next;
		  prim_item2->next=prim_item;
		}
	      if (within(point,prim_item->extent)) {
		temp_closest = distance(point,extent);
		if (temp_closest < closest || tftab->priority > priority)
		  {
		    closest = temp_closest;
		    priority = tftab->priority;
		    temp_selected = prim_items;
		  }
	      }
	    }
	  }
	  /*lines*/
	  if (strcmp(dtab->type,"line")==0){
	    prim_items=NULL;
	    tltab = &Tl_tab;
	    while (tltab != NULL) {
	      if (cmpncs(dtab->g_name, tltab->name) == 0) break;
	      tltab = tltab->next;
	    }
	    if (tltab->priority == 0) break; /* do nothing */
	    strcpy(proj,tltab->proj);
	    set_viewport_and_worldcoordinate ( tltab->lvp, tltab->lwc,proj );
	    xptr = tltab->lx; yptr = tltab->ly;
	    xpts = xptr->ps;  ypts = yptr->ps;
	    prim_items=NULL;
	    for (iprim=0; iprim<xptr->nsegs; iprim++) {
	      prim_item = (struct item_list *)malloc(sizeof(struct item_list));
	      prim_item->next = NULL;
	      prim_item->sub_primitive=iprim;
	      strcpy( prim_item->attr_name, dtab->name );
	      /* 	  printf("setting type to display_tab in line\n"); */
	      prim_item->type = display_tab;
	      prim_item->data.pd = dtab;
	      prim_item->ptab = NULL; /* no template here */
	      extent.ll.x=extent.ll.y=extent.lr.y=extent.ul.x=1.E20;
	      extent.ur.x=extent.ur.y=extent.lr.x=extent.ul.y=-1.E20;
	      for (j=0;j<xpts->npts;j++) {
		pxy.x=xpts->pts[j];
		pxy.y=ypts->pts[j];
		pxy=proj_convert(pxy);
		if (pxy.x<extent.ll.x) extent.ll.x=pxy.x;
		if (pxy.y<extent.ll.y) extent.ll.y=pxy.y;
		if (pxy.x>extent.lr.x) extent.lr.x=pxy.x;
		if (pxy.y<extent.lr.y) extent.lr.y=pxy.y;
		if (pxy.x>extent.ur.x) extent.ur.x=pxy.x;
		if (pxy.y>extent.ur.y) extent.ur.y=pxy.y;
		if (pxy.x<extent.ul.x) extent.ul.x=pxy.x;
		if (pxy.y>extent.ul.y) extent.ul.y=pxy.y;
	      }
	      xpts = xpts->next;
	      ypts = ypts->next;
	      prim_item->extent.ll.x = prim_item->extent.ul.x = extent.ll.x;
	      prim_item->extent.lr.x = prim_item->extent.ur.x = extent.lr.x;
	      prim_item->extent.ll.y = prim_item->extent.lr.y = extent.ll.y;
	      prim_item->extent.ul.y = prim_item->extent.ur.y = extent.ul.y;
	      if (prim_items==NULL)
		prim_items=prim_item;
	      else
		{
		  prim_item2=prim_items;
		  while(prim_item2->next!=NULL) prim_item2=prim_item2->next;
		  prim_item2->next=prim_item;
		}
	      if (within(point,prim_item->extent)) {
		temp_closest = distance(point,extent);
		if (temp_closest < closest || tltab->priority > priority)
		  {
		    closest = temp_closest;
		    priority = tltab->priority;
		    temp_selected = prim_items;
		  }
	      }
	    }
	  }
	  /*markers*/
	  if (strcmp(dtab->type,"marker")==0){
	    /* 	printf("in marker\n"); */
	    prim_items=NULL;
	    tmtab = &Tm_tab;
	    while (tmtab != NULL) {
	      if (cmpncs(dtab->g_name, tmtab->name) == 0) break;
	      tmtab = tmtab->next;
	    }
	    if (tmtab->priority == 0) break; /* do nothing */
	    strcpy(proj,tmtab->proj);
	    set_viewport_and_worldcoordinate ( tmtab->mvp, tmtab->mwc,proj );
	    xptr = tmtab->mx; yptr = tmtab->my;
	    xpts = xptr->ps;  ypts = yptr->ps;
	    prim_items=NULL;
	    for (iprim=0; iprim<xptr->nsegs; iprim++) {
	      prim_item = (struct item_list *)malloc(sizeof(struct item_list));
	      prim_item->next = NULL;
	      prim_item->sub_primitive=iprim;
	      strcpy( prim_item->attr_name, dtab->name );
	      prim_item->type = display_tab;
	      prim_item->data.pd = dtab;
	      prim_item->ptab = NULL; /* no template here */
	      extent.ll.x=extent.ll.y=extent.lr.y=extent.ul.x=1.E20;
	      extent.ur.x=extent.ur.y=extent.lr.x=extent.ul.y=-1.E20;
	      for (j=0;j<xpts->npts;j++) {
		pxy.x=xpts->pts[j];
		pxy.y=ypts->pts[j];
		pxy=proj_convert(pxy);
		if (pxy.x<extent.ll.x) extent.ll.x=pxy.x;
		if (pxy.y<extent.ll.y) extent.ll.y=pxy.y;
		if (pxy.x>extent.lr.x) extent.lr.x=pxy.x;
		if (pxy.y<extent.lr.y) extent.lr.y=pxy.y;
		if (pxy.x>extent.ur.x) extent.ur.x=pxy.x;
		if (pxy.y>extent.ur.y) extent.ur.y=pxy.y;
		if (pxy.x<extent.ul.x) extent.ul.x=pxy.x;
		if (pxy.y>extent.ul.y) extent.ul.y=pxy.y;
	      }
	      xpts = xpts->next;
	      ypts = ypts->next;
	      prim_item->extent.ll.x = prim_item->extent.ul.x = extent.ll.x;
	      prim_item->extent.lr.x = prim_item->extent.ur.x = extent.lr.x;
	      prim_item->extent.ll.y = prim_item->extent.lr.y = extent.ll.y;
	      prim_item->extent.ul.y = prim_item->extent.ur.y = extent.ul.y;
	      if (prim_items==NULL)
		prim_items=prim_item;
	      else
		{
		  prim_item2=prim_items;
		  while(prim_item2->next!=NULL) prim_item2=prim_item2->next;
		  prim_item2->next=prim_item;
		}
	      if (within(point,prim_item->extent)) {
		temp_closest = distance(point,extent);
		if (temp_closest < closest || tmtab->priority > priority)
		  {
		    closest = temp_closest;
		    priority = tmtab->priority;
		    temp_selected = prim_items;
		  }
	      }
	    }
	  }
      
      
	}
	/* add code to clean unused prim_items */
	dtab = dtab->next;
      }
    }
/*   /\* If we found an object that is closer than the */
/*    * previously selected item, remove items from list *\/ */
  if (selected != NULL && temp_selected != NULL)
    delete_list(&selected);
  if (selected == NULL && temp_selected != NULL) {
    selected = temp_selected;
  }
  
  if (gui_flg) {free((char *) template_name); template_name=NULL;}
  return selected;
}

/* This function will select all the picture template objects on the 
 * VCS Canvas (provided that they have a priority greater than zero 
 * (0).
 */
void
PyVCS_select_all_in_range( PyVCScanvas_Object *self,
			   struct item_list **hold_selected_items,
			   Gpoint pointA, 
			   Gpoint pointB)
{
    struct item_list    *item;
	int 		i, found = 0, priority=0;
    float       temp_closest,closest=100, fudge = 1.0e-5;
    float   val1, val2, value1, value2;
    struct table_text *pTt;
    struct table_chorn *pTo;
    extern struct a_tab    A_tab;
    extern struct p_tab    Pic_tab;
    extern struct display_tab  D_tab;
    struct a_tab    *atab;
    struct p_tab    *ptab, *temp_ptab;
    struct display_tab  *dtab;
    extern struct table_text Tt_tab;
    extern struct table_chorn To_tab;
    Gpoint      pxy;
    Gextent	 	outer_box,extent;
    char		shold[29][MAX_NAME], sname[29][MAX_NAME], fname[29][MAX_NAME];
    char		xtname[29][MAX_NAME], ytname[29][MAX_NAME], xlname[29][MAX_NAME];
    char		ylname[29][MAX_NAME], bname[29][MAX_NAME], lname[29][MAX_NAME];
    char 		label[256];
    struct pe_text 	*pet;
    struct pe_form 	*pef;
    struct pe_x_tic *pext;
    struct pe_y_tic *peyt;
    struct pe_x_lab *pexl;
    struct pe_y_lab *peyl;
    struct pe_box 	*peb;
    struct pe_line 	*pel;
    struct pe_leg 	*peleg;
    struct pe_dsp 	*pedsp;
    struct item_list *selected_items=NULL;

    verify_extent(&selected_items);

    /* Setup coordinates of selected box vertices */
    if (pointA.x < pointB.x)
    {
        if (pointA.y < pointB.y)
        {
            outer_box.ll.x = outer_box.ul.x = pointA.x;
            outer_box.lr.x = outer_box.ur.x = pointB.x;
            outer_box.ll.y = outer_box.lr.y = pointA.y;
            outer_box.ul.y = outer_box.ur.y = pointB.y;
        }
        else
        {
            outer_box.ll.x = outer_box.ul.x = pointA.x;
            outer_box.lr.x = outer_box.ur.x = pointB.x;
            outer_box.ll.y = outer_box.lr.y = pointB.y;
            outer_box.ul.y = outer_box.ur.y = pointA.y;
        }
    }
    else
    {
        if (pointA.y < pointB.y)
        {
            outer_box.ll.x = outer_box.ul.x = pointB.x;
            outer_box.lr.x = outer_box.ur.x = pointA.x;
            outer_box.ll.y = outer_box.lr.y = pointA.y;
            outer_box.ul.y = outer_box.ur.y = pointB.y;
        }
        else
        {
            outer_box.ll.x = outer_box.ul.x = pointB.x;
            outer_box.lr.x = outer_box.ur.x = pointA.x;
            outer_box.ll.y = outer_box.lr.y = pointB.y;
            outer_box.ul.y = outer_box.ur.y = pointA.y;
        }
    }

    dtab = &D_tab;
    while (dtab != NULL)
    {
        if (dtab->wkst_id == self->wkst_id)
        {
            for (ptab = &Pic_tab; ptab != NULL; ptab = ptab->next)
                if (strcmp(ptab->name,dtab->p_name) == 0) break;
            for (atab = &A_tab; atab != NULL; atab = atab->next)
                if (strcmp(atab->name,dtab->a[0]) == 0) break;
            get_text_fields(shold,sname,fname,xtname,ytname,xlname,ylname,bname,lname,atab,ptab);

            /* Get extent of all text objects and see if the mouse
            * click occured on or within those boundaries */
            for (pet=&(ptab->F), i=0;i < 22;i++,pet++)
            {
                if ( (i != 2) && (pet->p > 0) )
                {
                    for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
                        if (strcmp(pTt->name,pet->tb) == 0) break;
                    for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
                        if (strcmp(pTo->name,pet->to) == 0) break;
                    set_text_attr(pTt,pTo);
                    pxy.x = plnorm(0, (pet->x - (float) fmod(pet->x, fudge)));
                    pxy.y = plnorm(1, (pet->y - (float) fmod(pet->y, fudge)));
                    extent.ll.x = extent.ul.x = 0.0;
                    extent.lr.x = extent.ur.x = 0.0;
                    extent.ll.y = extent.lr.y = 0.0;
                    extent.ul.y = extent.ur.y = 0.0;
#ifdef CAIRODRAW
                    cairogqtxx(self->wkst_id, pxy, shold[i], &extent);
#else
                    gqtxx(self->wkst_id, pxy, shold[i], &extent);
#endif
                    if (extent.ll.x == extent.lr.x) extent.lr.x = extent.ur.x;
                    if (extent.ll.y == extent.ul.y) extent.ul.y = extent.ur.y;
                    if (contained_in(extent,outer_box))
                    {
                        found = 1;
                        item = (struct item_list *)malloc(sizeof(struct item_list));
                        item->x_reversed = item->y_reversed = 0;
                        item->next = NULL;
                        strcpy( item->attr_name, sname[i] );
                        priority = pet->p;
                        item->type = pe_text;
                        item->data.pet = pet;
			item->extent.ll.x = extent.ll.x;
			item->extent.lr.x = extent.lr.x;
			item->extent.ll.y = extent.ll.y;
			item->extent.ul.y = extent.ul.y;
			item->extent.ul.x = extent.ul.x;
			item->extent.ur.x = extent.ur.x;
			item->extent.lr.y = extent.lr.y;
			item->extent.ur.y = extent.ur.y;
			strcpy(item->string,shold[i]);
                        if (in_list(item,&selected_items))
                            free(item);
                        else
                            append_to_list(item,&selected_items);
	                }
                }
            }
    
            /* Get extent of all format objects and see if the mouse
            * click occured on or within those boundaries */
	        for (pef=&(ptab->xv),i=22;i < 29;i++,pef++)
            {
                if (pef->p > 0)
                {
                    for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
                        if (strcmp(pTt->name,pef->tb) == 0) break;
                    for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
                        if (strcmp(pTo->name,pef->to) == 0) break;
                    set_text_attr(pTt,pTo);
                    pxy.x = plnorm(0, (pef->x - (float) fmod(pef->x, fudge)));
                    pxy.y = plnorm(1, (pef->y - (float) fmod(pef->y, fudge)));
                    extent.ll.x = extent.ul.x = 0.0;
                    extent.lr.x = extent.ur.x = 0.0;
                    extent.ll.y = extent.lr.y = 0.0;
                    extent.ul.y = extent.ur.y = 0.0;
#ifdef CAIRODRAW
                    cairogqtxx(self->wkst_id, pxy, shold[i], &extent);
#else
                    gqtxx(self->wkst_id, pxy, shold[i], &extent);
#endif
                    if (contained_in(extent,outer_box))
                    {
                        found = 1;
                        item = (struct item_list *)malloc(sizeof(struct item_list));
                        item->x_reversed = item->y_reversed = 0;
                        item->next = NULL;
                        strcpy( item->attr_name, fname[i-22] );
                        priority = pef->p;
                        item->type = pe_text;
                        item->data.pef = pef;
			item->extent.ll.x = extent.ll.x;
			item->extent.lr.x = extent.lr.x;
			item->extent.ll.y = extent.ll.y;
			item->extent.ul.y = extent.ul.y;
			item->extent.ul.x = extent.ul.x;
			item->extent.ur.x = extent.ur.x;
			item->extent.lr.y = extent.lr.y;
			item->extent.ur.y = extent.ur.y;
			strcpy(item->string,shold[i]);
                        if (in_list(item,&selected_items))
                            free(item);
                        else
                            append_to_list(item,&selected_items);
	                }
                }
            }
    
            /* Get extent of all x-tic mark objects and see if the mouse
            * click occured on or within those boundaries */
	        for (pext=&(ptab->xt1),i=0;i < 4;i++,pext++)
            {
                if (pext->p > 0)
                {
                    extent.ll.x = extent.ul.x = ptab->dsp.x1;
                    extent.lr.x = extent.ur.x = ptab->dsp.x2;
                    extent.ll.y = extent.lr.y = plnorm(1,(pext->y1 - (float) fmod(pext->y1, fudge)));
                    extent.ul.y = extent.ur.y = plnorm(1,(pext->y2 - (float) fmod(pext->y2, fudge)));
                    if (contained_in(extent,outer_box))
                    {
                        found = 1;
                        data_selected = 1;
                        item = (struct item_list *)malloc(sizeof(struct item_list));
                        item->x_reversed = item->y_reversed = 0;
                        item->next = NULL;
                        strcpy( item->attr_name, xtname[i] );
                        priority = pext->p;
                        item->type = pe_x_tic;
                        item->data.pext = pext;
                        item->extent.ll.x = item->extent.ul.x = extent.ll.x;
                        item->extent.lr.x = item->extent.ur.x = extent.lr.x;
                        item->extent.ll.y = item->extent.lr.y = extent.ll.y;
                        item->extent.ul.y = item->extent.ur.y = extent.ul.y;
                        if (in_list(item,&selected_items))
                            free(item);
                        else
                            append_to_list(item,&selected_items);
	                }
                }
            }
    
            /* Get extent of all y-tic mark objects and see if the mouse
            * click occured on or within those boundaries */
	        for (peyt=&(ptab->yt1),i=0;i < 4;i++,peyt++)
            {
                if (peyt->p > 0)
                {
                    extent.ll.x = extent.ul.x = plnorm(0,(peyt->x1 - (float) fmod(peyt->x1, fudge)));
                    extent.lr.x = extent.ur.x = plnorm(0,(peyt->x2 - (float) fmod(peyt->x2, fudge)));
                    extent.ll.y = extent.lr.y = ptab->dsp.y1;
                    extent.ul.y = extent.ur.y = ptab->dsp.y2;
                    if (contained_in(extent,outer_box))
                    {
                        found = 1;
                        item = (struct item_list *)malloc(sizeof(struct item_list));
                        item->x_reversed = item->y_reversed = 0;
                        item->next = NULL;
                        strcpy( item->attr_name, ytname[i] );
                        priority = peyt->p;
                        item->type = pe_y_tic;
                        item->data.peyt = peyt;
                        item->extent.ll.x = item->extent.ul.x = extent.ll.x;
                        item->extent.lr.x = item->extent.ur.x = extent.lr.x;
                        item->extent.ll.y = item->extent.lr.y = extent.ll.y;
                        item->extent.ul.y = item->extent.ur.y = extent.ul.y;
                        if (in_list(item,&selected_items))
                            free(item);
                        else
                            append_to_list(item,&selected_items);
	            }
                }
            }
    
            /* Get extent of all x-label objects and see if the mouse
            * click occured on or within those boundaries */
	        for (pexl=&(ptab->xl1),i=0;i < 2;i++,pexl++)
            {
                if (pexl->p > 0)
                {
                    for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
                        if (strcmp(pTt->name,pexl->tb) == 0) break;
                    for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
                        if (strcmp(pTo->name,pexl->to) == 0) break;
                    set_text_attr(pTt,pTo);
                    pxy.x = ptab->dsp.x1;
                    pxy.y = plnorm(1, (pexl->y - (float) fmod(pexl->y, fudge)));
                    extent.ll.x = extent.ul.x = 0.0;
                    extent.lr.x = extent.ur.x = 0.0;
                    extent.ll.y = extent.lr.y = 0.0;
                    extent.ul.y = extent.ur.y = 0.0;
#ifdef CAIRODRAW
                    cairogqtxx(self->wkst_id, pxy, "120", &extent);
#else
                    gqtxx(self->wkst_id, pxy, "120", &extent);
#endif
                    extent.lr.x = extent.ur.x = ptab->dsp.x2;
                    if (contained_in(extent,outer_box))
                    {
                        found = 1;
                        item = (struct item_list *)malloc(sizeof(struct item_list));
                        item->x_reversed = item->y_reversed = 0;
                        item->next = NULL;
                        strcpy( item->attr_name, xlname[i] );
                        priority = pexl->p;
                        item->type = pe_x_lab;
                        item->data.pexl = pexl;
 			item->extent.ll.x = extent.ll.x;
			item->extent.lr.x = extent.lr.x;
			item->extent.ll.y = extent.ll.y;
			item->extent.ul.y = extent.ul.y;
			item->extent.ul.x = extent.ul.x;
			item->extent.ur.x = extent.ur.x;
			item->extent.lr.y = extent.lr.y;
			item->extent.ur.y = extent.ur.y;
                       if (in_list(item,&selected_items))
                            free(item);
                        else
                            append_to_list(item,&selected_items);
                    }
                }
            }
    
            /* Get extent of all y-label objects and see if the mouse
            * click occured on or within those boundaries */
	        for (peyl=&(ptab->yl1),i=0;i < 2;i++,peyl++)
            {
                if (peyl->p > 0)
                {
                    for (pTt=&Tt_tab; pTt != NULL; pTt=pTt->next)
                        if (strcmp(pTt->name,peyl->tb) == 0) break;
                    for (pTo=&To_tab; pTo != NULL; pTo=pTo->next)
                        if (strcmp(pTo->name,peyl->to) == 0) break;
                    set_text_attr(pTt,pTo);
                    pxy.x = plnorm(0, (peyl->x - (float) fmod(peyl->x, fudge)));
                    pxy.y = ptab->dsp.y1;
                    extent.ll.x = extent.ul.x = 0.0;
                    extent.lr.x = extent.ur.x = 0.0;
                    extent.ll.y = extent.lr.y = 0.0;
                    extent.ul.y = extent.ur.y = 0.0;
#ifdef CAIRODRAW
                    cairogqtxx(self->wkst_id, pxy, "120", &extent);
#else
                    gqtxx(self->wkst_id, pxy, "120", &extent);
#endif
                    extent.ul.y = extent.ur.y = ptab->dsp.y2;
                    if (contained_in(extent,outer_box))
                    {
                        found = 1;
                        item = (struct item_list *)malloc(sizeof(struct item_list));
                        item->x_reversed = item->y_reversed = 0;
                        item->next = NULL;
                        strcpy( item->attr_name, ylname[i] );
                        priority = peyl->p;
                        item->type = pe_y_lab;
                        item->data.peyl = peyl;
 			item->extent.ll.x = extent.ll.x;
			item->extent.lr.x = extent.lr.x;
			item->extent.ll.y = extent.ll.y;
			item->extent.ul.y = extent.ul.y;
			item->extent.ul.x = extent.ul.x;
			item->extent.ur.x = extent.ur.x;
			item->extent.lr.y = extent.lr.y;
			item->extent.ur.y = extent.ur.y;
                        if (in_list(item,&selected_items))
                            free(item);
                        else
                            append_to_list(item,&selected_items);
                    }
                }
            }

    
            /* Get extent of all box objects and see if the mouse
            * click occured on or within those boundaries */
	    for (peb=&(ptab->b1),i=0;i < 4;i++,peb++)
            {
                if (peb->p > 0)
                {
                    item = (struct item_list *)malloc(sizeof(struct item_list));
                    item->x_reversed = item->y_reversed = 0;
                    value1 = plnorm(0, (peb->x1 - (float) fmod(peb->x1, fudge)));
                    value2 = plnorm(0, (peb->x2 - (float) fmod(peb->x2, fudge)));
                    if (peb->x1 < peb->x2) {
                        item->extent.ll.x = item->extent.ul.x = value1;
                        item->extent.lr.x = item->extent.ur.x = value2;
                    }
                    else {
                        item->extent.ll.x = item->extent.ul.x = value2;
                        item->extent.lr.x = item->extent.ur.x = value1;
                    }
                    value1 = plnorm(1, (peb->y1 - (float) fmod(peb->y1, fudge)));
                    value2 = plnorm(1, (peb->y2 - (float) fmod(peb->y2, fudge)));
                    if (peb->y1 < peb->y2) {
                        item->extent.ll.y = item->extent.lr.y = value1;
                        item->extent.ul.y = item->extent.ur.y = value2;
                    }
                    else {
                        item->extent.ll.y = item->extent.lr.y = value2;
                        item->extent.ul.y = item->extent.ur.y = value1;
                    }
                    if (contained_in(item->extent,outer_box))
                    {
                        found = 1;
                        item->next = NULL;
                        strcpy( item->attr_name, bname[i] );
                        priority = peb->p;
                        item->type = pe_box;
                        item->data.peb = peb;
                        if (in_list(item,&selected_items))
                            free(item);
                        else
                            append_to_list(item,&selected_items);
                    }
                    else
                        free(item);
                }
            }
    
            /* Get extent of all line objects and see if the mouse
            * click occured on or within those boundaries. Since in 
            * some cases it is impossible to actually click on the 
            * line value, I've added a slight buffer. */
	    for (pel=&(ptab->l1),i=0;i < 4;i++,pel++)
            {
                if (pel->p > 0)
                {
                    item = (struct item_list *)malloc(sizeof(struct item_list));
                    item->x_reversed = item->y_reversed = 0;
                    if (pel->x1 < pel->x2) {
                        val1 = pel->x1-BUFFER; val2 = pel->x2+BUFFER;
                        value1 = plnorm(0, (val1 - (float) fmod(val1, fudge)));
                        value2 = plnorm(0, (val2 - (float) fmod(val2, fudge)));
                        item->extent.ll.x = item->extent.ul.x = value1;
                        item->extent.lr.x = item->extent.ur.x = value2;
                    }
                    else {
                        val1 = pel->x1+BUFFER; val2 = pel->x2-BUFFER;
                        value1 = plnorm(0, (val1 - (float) fmod(val1, fudge)));
                        value2 = plnorm(0, (val2 - (float) fmod(val2, fudge)));
                        item->extent.ll.x = item->extent.ul.x = value2;
                        item->extent.lr.x = item->extent.ur.x = value1;
                    }
                    if (pel->y1 < pel->y2) {
                        val1 = pel->y1-BUFFER; val2 = pel->y2+BUFFER;
                        value1 = plnorm(1, (val1 - (float) fmod(val1, fudge)));
                        value2 = plnorm(1, (val2 - (float) fmod(val2, fudge)));
                        item->extent.ll.y = item->extent.lr.y = value1;
                        item->extent.ul.y = item->extent.ur.y = value2;
                    }
                    else {
                        val1 = pel->y1+BUFFER; val2 = pel->y2-BUFFER;
                        value1 = plnorm(1, (val1 - (float) fmod(val1, fudge)));
                        value2 = plnorm(1, (val2 - (float) fmod(val2, fudge)));
                        item->extent.ll.y = item->extent.lr.y = value2;
                        item->extent.ul.y = item->extent.ur.y = value1;
                    }
                    if (contained_in(extent,outer_box))
                    {
                        found = 1;
                        item->next = NULL;
                        strcpy( item->attr_name, lname[i] );
                        priority = pel->p;
                        item->type = pe_line;
                        item->data.pel = pel;
                        if (in_list(item,&selected_items))
                            free(item);
                        else
                            append_to_list(item,&selected_items);
                    }
                    else
                        free(item);
                }
            }

            /* Check and see if the legend was selected */
            peleg = &(ptab->leg);
            if (peleg->p > 0)
            {
                item = (struct item_list *)malloc(sizeof(struct item_list));
                item->x_reversed = item->y_reversed = 0;
                value1 = plnorm(0, (peleg->x1 - (float) fmod(peleg->x1, fudge)));
                value2 = plnorm(0, (peleg->x2 - (float) fmod(peleg->x2, fudge)));
                if (peleg->x1 < peleg->x2) {
                    item->extent.ll.x = item->extent.ul.x = value1;
                    item->extent.lr.x = item->extent.ur.x = value2;
                }
                else {
                    item->extent.ll.x = item->extent.ul.x = value2;
                    item->extent.lr.x = item->extent.ur.x = value1;
                }
                value1 = plnorm(1, (peleg->y1 - (float) fmod(peleg->y1, fudge)));
                value2 = plnorm(1, (peleg->y2 - (float) fmod(peleg->y2, fudge)));
                if (peleg->y1 < peleg->y2) {
                    item->extent.ll.y = item->extent.lr.y = value1;
                    item->extent.ul.y = item->extent.ur.y = value2;
                }
                else {
                    item->extent.ll.y = item->extent.lr.y = value2;
                    item->extent.ul.y = item->extent.ur.y = value1;
                }
                if (contained_in(item->extent,outer_box))
                {
                        found = 1;
                        item->next = NULL;
                        strcpy( item->attr_name, "legend" );
                        priority = peleg->p;
                        item->type = pe_leg;
                        item->data.peleg = peleg;
                        if (in_list(item,&selected_items))
                            free(item);
                        else
                            append_to_list(item,&selected_items);
                }
                else
                    free(item);
            }
    
            /* Check and see if the data was selected */
            pedsp = &(ptab->dsp);
            if (pedsp->p > 0)
            {
                item = (struct item_list *)malloc(sizeof(struct item_list));
                item->x_reversed = item->y_reversed = 0;
                value1 = plnorm(0, (pedsp->x1 - (float) fmod(pedsp->x1, fudge)));
                value2 = plnorm(0, (pedsp->x2 - (float) fmod(pedsp->x2, fudge)));
                if (pedsp->x1 < pedsp->x2) {
                    item->extent.ll.x = item->extent.ul.x = value1;
                    item->extent.lr.x = item->extent.ur.x = value2;
                }
                else {
                    item->extent.ll.x = item->extent.ul.x = value2;
                    item->extent.lr.x = item->extent.ur.x = value1;
                }
                    value1 = plnorm(1, (pedsp->y1 - (float) fmod(pedsp->y1, fudge)));
                    value2 = plnorm(1, (pedsp->y2 - (float) fmod(pedsp->y2, fudge)));
                if (pedsp->y1 < pedsp->y2) {
                    item->extent.ll.y = item->extent.lr.y = value1;
                    item->extent.ul.y = item->extent.ur.y = value2;
                }
                else {
                    item->extent.ll.y = item->extent.lr.y = value2;
                    item->extent.ul.y = item->extent.ur.y = value1;
                }
                if (contained_in(extent,outer_box))
                {
                        found = 1;
                        item->next = NULL;
                        strcpy( item->attr_name, "data" );
                        priority = pedsp->p;
                        item->type = pe_dsp;
                        item->data.pedsp = pedsp;
                        if (in_list(item,&selected_items))
                            free(item);
                        else
                            append_to_list(item,&selected_items);
                }
                else
                    free(item);
            }
        }
        dtab = dtab->next;
    }
    draw_selected(selected_items,0);
    *hold_selected_items = selected_items;
}

void swap(float *a,float *b)
{
    float temp;
    temp = *a;
    *a = *b;
    *b = temp;
}

void verify_extent(struct item_list **selected_items)
{
    struct item_list *current = (struct item_list *)*selected_items;
    while (current != NULL)
    {
        if (current->extent.ll.x > current->extent.lr.x)
        {
            current->x_reversed = 1;
            swap(&current->extent.ll.x, &current->extent.lr.x);
            swap(&current->extent.ul.x, &current->extent.ur.x);
        }
        else 
            current->x_reversed = 0;
        if (current->extent.ll.y > current->extent.ul.y)
        {
            current->y_reversed = 1;
            swap(&current->extent.ll.y, &current->extent.ul.y);
            swap(&current->extent.lr.y, &current->extent.ur.y);
        }
        else 
            current->y_reversed = 0;
        current = current->next;
    }
}



extern "C" struct fill_range *generate_auto_fill_range(float min,float max)
{
  extern struct table_fill Tf_tab;
  struct fill_range *p1,*pl1,*plg;
  struct table_fill *pt,*ptt;
  float dr,del,center;
  int n,erret,k1,k2,i,tmp;
  char tmpchar[256];
  extern FILE                      *fperr;
  plg=NULL;
  for (pt=&Tf_tab;
       pt!=NULL&&strcmp(pt->name,"isof1")!=0;
       pt=pt->next);
  if (pt == NULL) pt=&Tf_tab;
  ptt=pt;
  if (nice15(max,min,&dr,&n,&center) == 0)
    {
      PyErr_SetString(PyExc_TypeError, "Error - can not compute the meshfill ranges\n");
      k1=k2=1;
      del=max;
      erret++;
    }
  else
    {
      del=dr*pow(10.0,(double) n);
      k1=min/del;
      if (k1*del > min) k1--;
      k2=max/del;
      if (k2*del < max) k2++;
      while (k2-k1+1 < 7) 
	{
	  del=del*0.5;
	  k1=min/del;
	  if (k1*del > min) k1--;
	  k2=max/del;
	  if (k2*del < max) k2++;
	}
      while (k2-k1+1 > 15)
	{
	  del=del*2.0;
	  k1=min/del;
	  if (k1*del > min) k1--;
	  k2=max/del;
	  if (k2*del < max) k2++;		       
	}
      while (k2-k1+1 < 15)
	{
	  k1--;
	  if (k2-k1+1 < 15) k2++;
	}
      tmp=0;
      for (i=k1;i <= k2;i++)
	{
	  /* 	      printf("k1,k2,i %d,%d,%d\n",k1,k2,i); */
	  if((p1=(struct fill_range *)
	      malloc(sizeof(struct fill_range))) == NULL)
	    {
	      err_warn(1,fperr,
		       "Error - no memory to compute meshfill ranges\n");
	      for (pl1=plg;pl1 != NULL;pl1=pl1->next)
		free((char *)pl1);
	      plg=NULL;
	      erret++;
	    }
	  if (plg == NULL) 
	    {
	      pl1=plg=p1;
	      pl1->lev1=i*del;
	    }
	  else 
	    {
	      p1->lev1=pl1->lev2;
	      pl1->next=p1; 
	      pl1=p1;
	    }
	  pl1->id=i-k1+1;
	  pl1->lev2=(i+1)*del;
	  
	  sprintf(tmpchar,"AuTo_%d",tmp);
	  
	  for (ptt=&Tf_tab;
	       ptt!=NULL&&strcmp(ptt->name,tmpchar)!=0;
	       ptt=ptt->next);
/* 	  printf("Ok we're dealing with %s:\n",ptt->name); */
	  if (ptt->faci!=NULL) free(ptt->faci);
	  if (ptt->fais!=NULL) free(ptt->fais);
	  if (ptt->fasi!=NULL) free(ptt->fasi);
	  if ((ptt->faci=(int *) malloc(sizeof(int))) == NULL)
	    {
	      err_warn(1,fperr,
		       "Error - no memory to compute meshfill ranges (%s)\n");
	      for (pl1=plg;pl1 != NULL;pl1=pl1->next)
		free((char *)pl1);
	      plg=NULL;
	      erret++;
	    }
	  if ((ptt->fasi=(int *) malloc(sizeof(int))) == NULL)
	    {
	      err_warn(1,fperr,
		       "Error - no memory to compute meshfill ranges (%s)\n");
	      for (pl1=plg;pl1 != NULL;pl1=pl1->next)
		free((char *)pl1);
	      plg=NULL;
	      erret++;
	    }
	  if ((ptt->fais=(int *) malloc(sizeof(int))) == NULL)
	    {
	      err_warn(1,fperr,
		       "Error - no memory to compute meshfill ranges (%s)\n");
	      for (pl1=plg;pl1 != NULL;pl1=pl1->next)
		free((char *)pl1);
	      plg=NULL;
	      erret++;
	    }
	  *ptt->faci=(int)((tmp)*(239.-16.)/14.+16);
	  *ptt->fasi=1;
	  *ptt->fais=1;
	  tmp=tmp+1;
/* 	  printf("name of fill_table: %s, color %d\n",ptt->name,*ptt->faci);; */
	  strcpy(pl1->fill_name,ptt->name);
	  pl1->next=NULL;
	}
    }
  return plg;
}


int find_color_from_fill_range(struct fill_range *line, float value,float min,float max,int missing)

{   
  struct table_fill *pf;
  struct fill_range *po;
  extern struct table_fill Tf_tab;
  struct fill_range *pl,*plg;
  float tmp,tmp2;

  pl=plg=line;
  if (pl == NULL || (pl->lev1 >= 0.99e20 && pl->lev2 >= 0.99e20 &&
		     pl->next == NULL) )
    plg=generate_auto_fill_range(min,max);
  if (value<1.E20)
    {
      for (po=plg; po!=NULL; po=po->next)
	{
	  tmp=po->lev1;
	  tmp2=po->lev2;
/* 	  printf("%s, %f, %f\n",po->fill_name,tmp,tmp2); */
	  if (tmp>.99E20 && tmp2>.99E20) tmp=-.99E20;
	  if ((tmp<=value) && (tmp2>=value))
	    {
	      /* Set up the fill area definitions. */
	      
	      for (pf=&Tf_tab;pf!=NULL;pf=pf->next) {
/* 		printf("%s,%d,%d,%d,%s\n",pf->name,pf->faci[0],pf->fais[0],pf->fasi[0],po->fill_name); */
		if (strcmp(pf->name,po->fill_name) == 0)
		  {
		    return pf->faci[0];
		  }
	      }
	    }
	}
    }
  else
    {
      return missing;
    }
  return -999;
}



int locator(Gpoint point,struct p_tab *pPin,struct display_tab *dtab,struct data_point *info)
{
  extern struct project_attr      p_PRJ;
  /*   extern int                      set_projection(char *,struct pe_dsp, float *,float *); */
  /*   extern int                      gctp_conv(double, double,double *, double *,int); */
  double                          xx,yy,tmp1,tmp2;
  int                             ierr=1;
  struct project_attr             *pj;

  /* Ok now figuring the actual location */
  pj=&p_PRJ;
  xx=(double)((point.x-pj->cX)/pj->sX);
  yy=(double)((point.y-pj->cY)/pj->sY);
  if (pj->proj_type>0)
    {
      ierr=gctp_conv(xx,yy,&tmp1,&tmp2,1);
      info->x=tmp1;
      info->y=tmp2;
      /* if lat > 90 in abs , we must have clicked somewhere outside the world */
      if (fabs(tmp2)>90.)
	{
	  info->x=-999;
	  info->y=-999;
	}
    }
  else if (pj->proj_type<0)
    {
      /* don't know how to inverse molleweide/polar/robinson from Dean */
      info->x=-999;
      info->y=-999;
    }
  else
    {
      info->x=xx;
      info->y=yy;
    }
/*   printf("locator: %f,%f\n",info->x,info->y); */
  return ierr;
}


extern "C" int get_data_coords(PyVCScanvas_Object *self,Gpoint point,struct item_list *item,struct data_point *info)
{
  extern struct gfm_tab            Gfm_tab;
  extern struct gfi_tab            Gfi_tab;
  extern struct gfb_tab            Gfb_tab;
  extern struct gfo_tab            Gfo_tab;
  extern struct gi_tab            Gi_tab;
  extern struct go_tab            Go_tab;
  extern struct gSp_tab            GSp_tab;
  extern struct gv_tab            Gv_tab;
  extern struct gYx_tab            GYx_tab;
  extern struct gXy_tab            GXy_tab;
  extern struct gXY_tab            GXY_tab;
  extern struct p_tab              Pic_tab;
  extern struct display_tab    	   D_tab;
  extern struct a_tab     	   A_tab;

  extern struct project_attr       p_PRJ;
  extern FILE                      *fperr;
  extern struct table_fill Tf_tab;
  
  struct display_tab     	   *dtab=NULL;
  struct a_tab            	   *atab=NULL,*pb,*pB;
  struct a_attr           	   *pa;
  struct a_attr           	   *pa2;
  struct p_tab                     *pP,*pP0;
  struct a_attr                    *pmesh,*pdata;
  struct gfm_tab                   *pgfm;
  struct gfm_attr                  *pGfm;
  struct gfi_tab                   *pgfi;
  struct gfi_attr                  *pGfi;
  struct gfb_tab                   *pgfb;
  struct gfb_attr                  *pGfb;
  struct gfo_tab                   *pgfo;
  struct gfo_attr                  *pGfo;
  struct gi_tab                   *pgi;
  struct gi_attr                  *pGi;
  struct go_tab                   *pgo;
  struct go_attr                  *pGo;
  struct gSp_tab                   *pgSp;
  struct gSp_attr                  *pGSp;
  struct gv_tab                   *pgv;
  struct gv_attr                  *pGv;
  struct gXy_tab                   *pgXy;
  struct gXy_attr                  *pGXy;
  struct gYx_tab                   *pgYx;
  struct gYx_attr                  *pGYx;
  struct gXY_tab                   *pgXY;
  struct gXY_attr                  *pGXY;

  canvas_display_list              *dptr;

  float				   xs1, xs2, ys1, ys2;
  float				   Xmin,Xmax,Ymin,Ymax;
  float				   cvalue,ftmp,xoff,yoff;
  float                            *meshX=NULL,*meshY=NULL;
  float                            x,y,xwrap,ywrap,dsp[NDS];
  int                              n1,n2,k1,j,iw,jw,ierr;
  int				   i,k,l, xindex, yindex, size=1;
  int                              nxwrap,nywrap,ncells;

  S_boxfill                        regis[256],save_regis[256];
  float  save_lev_1, save_lev_2;
  int save_num_regis=0, save_color_1, save_color_2;
  struct fill_range *pfiso;
  int num_regis, data_regis;
  struct table_fill               *ptb, *ftab;
  short *save_mask;
  float *save_data;
  float save_min, save_max, save_mean;

  struct pe_dsp pP_dsp;

/*   int nicedf(float a,float b,float *dr,int *pw10,float *center); */
/*   float dr,dx,x1,x2,center; */
/*   int pw10; */

  xs1=item->data.pedsp->x1;
  xs2=item->data.pedsp->x2;
  ys1=item->data.pedsp->y1;
  ys2=item->data.pedsp->y2;
/* We used to initialize info object here but now it's done outside*/
/*   info->x = -999; */
/*   info->y = -999; */
/*   info->value=-999; */
/*   info->value2=-999; */
/*   info->x_index = -999; */
/*   info->y_index = -999; */
/*   info->color=-999; */
  extern int templateRatio(struct p_tab *pp, int wkst_id);
  extern int copyP_attr(struct p_tab *gtab,struct p_tab *ptab);
  xindex=-999;
  yindex=-999;
  if (self->dlist != NULL) {
    dptr=self->dlist;
    dtab=&D_tab;
    while ((dtab != NULL) &&
	   (strcmp(dtab->p_name, item->ptab->name) != 0))
      dtab = dtab->next;
    if (dtab == NULL) return 0; /* did not find the correct display */


    pP0=&Pic_tab;
    while (pP0 != NULL )
      {
	if (strcmp(pP0->name,dtab->p_name) == 0) break;
	pP0=pP0->next;
      }
    if (pP0 == NULL) return 0; /*did not find template */



      /*              Create a new table structure and copy to it its new locations base on the 
                      VCS Canvases page orientation (i.e., landscape or portrait).                */

      if((pP=(struct p_tab *)malloc(sizeof(struct p_tab)))==NULL) {
        err_warn(1,fperr,
                 "Error - memory for getting picture template( gm_template_hold ) not found.\n");
        return 1;
      }

      strcpy( pP->name,"gm_template_hd" );
      copyP_attr(pP0,pP);
      templateRatio(pP,dtab->wkst_id-7);
    

    atab=&A_tab;
    while ((atab != NULL) && (strcmp(atab->name, dtab->a[0]) != 0)) {
      atab = atab->next;
    }
    if (atab == NULL) return 0; /* did not find the correct slab array */
    pa=atab->pA_attr;

    /* Finds the second array if exists */
    atab=&A_tab;
    while ((atab != NULL) && (strcmp(atab->name, dtab->a[1]) != 0)) {
      atab = atab->next;
    }
    pa2=NULL;
    if (atab != NULL) 
      {
	pa2=atab->pA_attr; /* did not find the second slab array */
      }
    /* Find the x and y coordinate value given the projection, coordinate min and max and the
     * graphics method.
     */

    dsp[0]=dtab->dsp_used[0];
    dsp[1]=dtab->dsp_used[1];
    dsp[2]=dtab->dsp_used[2];
    dsp[3]=dtab->dsp_used[3];

    pP_dsp.p = pP->dsp.p;
    pP_dsp.x1 = plnorm(0, pP->dsp.x1); pP_dsp.x2 = plnorm(0, pP->dsp.x2);
    pP_dsp.y1 = plnorm(1, pP->dsp.y1); pP_dsp.y2 = plnorm(1, pP->dsp.y2);

    if (cmpncs(dtab->type,"boxfill") == 0) {
      for (pgfb=&Gfb_tab; strcmp(dtab->g_name,pgfb->name) != 0; pgfb=pgfb->next) {}
      pGfb=pgfb->pGfb_attr;
      set_projection(pGfb->proj,pP_dsp,pGfb->dsp,dsp);
    } else if (cmpncs(dtab->type,"isofill") == 0) {
      for (pgfi=&Gfi_tab; strcmp(dtab->g_name,pgfi->name) != 0; pgfi=pgfi->next) {}
      pGfi=pgfi->pGfi_attr;
      set_projection(pGfi->proj,pP_dsp,pGfi->dsp,dsp);
    } else if (cmpncs(dtab->type,"isoline") == 0) {
      for (pgi=&Gi_tab; strcmp(dtab->g_name,pgi->name) != 0; pgi=pgi->next) {}
      pGi=pgi->pGi_attr;
      set_projection(pGi->proj,pP_dsp,pGi->dsp,dsp);
    } else if (cmpncs(dtab->type,"outline") == 0) {
      for (pgo=&Go_tab; strcmp(dtab->g_name,pgo->name) != 0; pgo=pgo->next) {}
      pGo=pgo->pGo_attr;
      set_projection(pGo->proj,pP_dsp,pGo->dsp,dsp);
    } else if (cmpncs(dtab->type,"outfill") == 0) {
      for (pgfo=&Gfo_tab; strcmp(dtab->g_name,pgfo->name) != 0; pgfo=pgfo->next) {}
      pGfo=pgfo->pGfo_attr;
      set_projection(pGfo->proj,pP_dsp,pGfo->dsp,dsp);
    } else if (cmpncs(dtab->type,"meshfill") == 0) {
      for (pgfm=&Gfm_tab; strcmp(dtab->g_name,pgfm->name) != 0; pgfm=pgfm->next) {}
      pGfm=pgfm->pGfm_attr;
      set_projection(pGfm->proj,pP_dsp,pGfm->dsp,dsp);
    } else if (cmpncs(dtab->type,"vector") == 0) {
      for (pgv=&Gv_tab; strcmp(dtab->g_name,pgv->name) != 0; pgv=pgv->next) {}
      pGv=pgv->pGv_attr;
      set_projection(pGv->proj,pP_dsp,pGv->dsp,dsp);
    } else if (cmpncs(dtab->type,"xyvsy") == 0) {
      for (pgXy=&GXy_tab; strcmp(dtab->g_name,pgXy->name) != 0; pgXy=pgXy->next) {}
      pGXy=pgXy->pGXy_attr;
      set_projection(pGXy->proj,pP_dsp,pGXy->dsp,dsp);
    } else if (cmpncs(dtab->type,"yxvsx") == 0) {
      for (pgYx=&GYx_tab; strcmp(dtab->g_name,pgYx->name) != 0; pgYx=pgYx->next) {}
      pGYx=pgYx->pGYx_attr;
      set_projection(pGYx->proj,pP_dsp,pGYx->dsp,dsp);
    } else if (cmpncs(dtab->type,"xvsy") == 0) {
      for (pgXY=&GXY_tab; strcmp(dtab->g_name,pgXY->name) != 0; pgXY=pgXY->next) {}
      pGXY=pgXY->pGXY_attr;
      set_projection(pGXY->proj,pP_dsp,pGXY->dsp,dsp);
    } else if (cmpncs(dtab->type,"scatter") == 0) {
      for (pgSp=&GSp_tab; strcmp(dtab->g_name,pgSp->name) != 0; pgSp=pgSp->next) {}
      pGSp=pgSp->pGSp_attr;
      set_projection(pGSp->proj,pP_dsp,pGSp->dsp,dsp);
    }
      else {
/* 	printf("type: %s is not pickable\n"); */
    }
    ierr=locator(point,pP,dtab,info);

    /* Figuring out the projection used , call the locator, and figures out the index*/
    if (cmpncs(dtab->type,"meshfill") == 0)
      {
	pgfm=&Gfm_tab;
	while (pgfm != NULL)
	  {
	    if (strcmp(dtab->g_name,pgfm->name) == 0) break;
	    pgfm=pgfm->next;
	  }

	pGfm=pgfm->pGfm_attr;

	/*     Find the mesh array.   	*/

	pB=&A_tab;
	while (pB != NULL)
	  {
	    if (strlen(pB->name)>(size_t)0 && pB->pA_attr->notok==0 
		&& strcmp(pB->name,dtab->a[0]) == 0) break;
	    pB=pB->next;
	  }

	pdata=pB->pA_attr;

	pb=&A_tab;
	while (pb != NULL)
	  {
	    if (strlen(pb->name)>(size_t)0 && pb->pA_attr->notok==0
		&& strcmp(pb->name,dtab->a[1]) == 0) break;
	    pb=pb->next;
	  }
	pmesh=pb->pA_attr;

	n1=pdata->XS[0][0]; /* number of cell to plot */
	n2=pmesh->XS[0][0]; /* number of vertices for each cell */

	/* Malloc size for meshX/meshY*/
	if ((meshX=(float *)malloc(sizeof(float)*n2)) == NULL) PyErr_SetString(PyExc_TypeError, "Error -no memory to store X mesh values\n");
	if ((meshY=(float *)malloc(sizeof(float)*n2)) == NULL) PyErr_SetString(PyExc_TypeError, "Error -no memory to store Y mesh values\n");

	/* Ok now figures out the index and value */
	xwrap=pGfm->xwrap;
	ywrap=pGfm->ywrap;

	info->x_index=-999;
	info->y_index=-999;
	info->value=(float)-999.;

	/* Now loop through the mesh */
	for (i=0;i<n1;i++)
	  {
	    ftmp=(float)getArrayValueAsFloat(pmesh,i*pmesh->XS[0][0]*pmesh->XS[1][0]);
	    /* 	   checks there's an actual mesh point there! */
	    if ( ftmp<1.E20 )
	      {
		/*       printf("in there !\n"); */
		xoff=0.;
		yoff=0.;
		nxwrap=1;
		nywrap=1;
		ncells=0;
		for (j=0;j<n2;j++)
		  {
		    k1=i*pmesh->XS[0][0]*pmesh->XS[1][0]+j;
		    y=(float)getArrayValueAsFloat(pmesh,k1);
		    if (y<1.E20) ncells=ncells+1;
		  }
		if ((xwrap!=0.)||(ywrap!=0.))
		  {
		    Xmin=  1.E23;
		    Xmax= -1.E23;
		    Ymin=  1.E23;
		    Ymax= -1.E23;
		    /* first determine the max and min X and Y of the polygon (for wrapping) */
		    for (j=0;j<ncells;j++)
		      {
			k1=i*pmesh->XS[0][0]*pmesh->XS[1][0]+j;
			y=(float)getArrayValueAsFloat(pmesh,k1);
			x=(float)getArrayValueAsFloat(pmesh,k1+pmesh->XS[0][0]);
			if (x>Xmax && x<1.E20) Xmax=x;
			if (x<Xmin && x<1.E20) Xmin=x;
			if (y>Ymax && y<1.E20) Ymax=y;
			if (y<Ymin && y<1.E20) Ymin=y;
		      }
		    /* Now set the x/y offset in order to be left/lower and X1/Y1 */
		    /* and count how many wrap to do in each direction  */
/* 		    printf("Xmin, Xmax, Ymin, Ymax : %f,%f,%f,%f, dsp_used 0,1,2,3: %f,%f,%f,%f\n",Xmin, Xmax, Ymin, Ymax,dtab->dsp_used[0],dtab->dsp_used[1],dtab->dsp_used[2],dtab->dsp_used[3]); */
		    if (xwrap!=0.)
		      {
			while (Xmax>dtab->dsp_used[0]) { Xmin=Xmin-xwrap;Xmax=Xmax-xwrap;xoff=xoff-xwrap;}
			while((Xmin+xwrap)<=dtab->dsp_used[2]) {Xmin=Xmin+xwrap;nxwrap=nxwrap+1;}
		      }
		    if (ywrap!=0.)
		      {
			while (Ymax>dtab->dsp_used[1]) { Ymin=Ymin-ywrap;Ymax=Ymax-ywrap;yoff=yoff-ywrap;}
			while((Ymin+ywrap)<=dtab->dsp_used[3]) {Ymin=Ymin+ywrap;nywrap=nywrap+1;}
		      }
		  }
/* 		printf("WRAPS ARE: %d,%d, %f, %f\n",nxwrap,nywrap, xoff, yoff); */
		/* 	    Now do the wraping thing */
		for (iw=0;iw<nywrap;iw++) /*y wrap */
		  {
		    for (jw=0;jw<nxwrap;jw++)
		      {
			for (j=0;j<ncells;j++)
			  {
			    /* construct the array of the corners */
			    k1=i*pmesh->XS[0][0]*pmesh->XS[1][0]+j;
			    meshY[j]=(float)getArrayValueAsFloat(pmesh,k1)+yoff;
			    meshX[j]=(float)getArrayValueAsFloat(pmesh,k1+pmesh->XS[0][0])+xoff;
			  }
			/* Is it in the domain ? */
			x=(float)(info->x);
			y=(float)(info->y);
			/* 		    printf("%d:looking for %f,%f\n%f/%f------------%f/%f\n",i,x,y,meshX[0],meshY[0],meshX[1],meshY[1]); */
			/* 		    printf("%f/%f------------%f/%f\n",meshX[3],meshY[3],meshX[2],meshY[2]); */
			/* 		    printf("ncells:%d\n",ncells); */
			k1=(int)isInsidePolygon(x,y,ncells,meshX,meshY);
			if (k1==1)
			  {
			    info->x_index=i;
			    /*			info->y_index=i;*/
			    info->value=(float)getArrayValueAsFloat(pa,i);
/* 			    printf("Found it at: %i, ixwrap, iywrap are: %i, %i, value is: %f\n",i,iw,jw,info->value); */
			  }
			xoff=xoff+xwrap;
		      }
		    yoff=yoff+ywrap;
		  }
	      }
	  }
	if (meshX !=NULL) free(meshX);
	if (meshY !=NULL) free(meshY);
      }
    else if ((cmpncs(dtab->type,"xvsy")==0) || (cmpncs(dtab->type,"scatter")==0) )
      {
/* 	for (k=0; k<(pa2->xs[0][0]); k++) */
/* 	  if ( ((info->y >= pa2->xb[0][k]) && (info->y <= pa2->xb[0][k+1])) || */
/* 	       ((info->y <= pa2->xb[0][k]) && (info->y >= pa2->xb[0][k+1])) ) */
/* 	    { */
/* 	      for (i=0; i<(pa->xs[0][0]); i++) */
/* 		if ( ((info->x >= pa->xb[0][i]) && (info->x <= pa->xb[0][i+1])) || */
/* 		     ((info->x <= pa->xb[0][i]) && (info->x >= pa->xb[0][i+1])) ) */
/* 		  { */
/* 		    info->y_index = yindex = k; */
/* 		    info->x_index = xindex = i; */
/* 		    break; */
/* 		  } */
/* 	      if (info->y_index!=-999) break; */
/* 	    } */
      }
    else
      {
	for (i=0; i<pa->ND; ++i)
	  {
	    if ((i==0) && cmpncs(dtab->type,"xyvsy") != 0)
	      {
		cvalue = info->x;
	      }
	    else if ((i == 1) || cmpncs(dtab->type,"xyvsy") == 0)
	      {
		cvalue = info->y;
	      }
	    for (k=0; k<(pa->xs[i][0]); k++) 
	      {
		/* 		printf("k,pa->xb[i][k], value, pa->xb[i][k+1] : %d, %f, %f, %f\n",k,pa->xb[i][k], cvalue, pa->xb[i][k+1] ); */ 
		if ( ((cvalue >= pa->xb[i][k]) && (cvalue <= pa->xb[i][k+1])) ||
		     ((cvalue <= pa->xb[i][k]) && (cvalue >= pa->xb[i][k+1])) )
		  {
		    if ((i==0) && cmpncs(dtab->type,"xyvsy") != 0)
		      {
			info->x_index = xindex = k;
		      } 
		    else if ((i == 1) || cmpncs(dtab->type,"xyvsy") == 0)
		      {
			info->y = cvalue;
			info->y_index = yindex = k;
		      }
		    break;
		  }
	      }
	  }
      }
  }
  
  if (cmpncs(dtab->type,"yxvsx") == 0) 
    {
      info->y_index = -999;
      info->value = pa->un.data[xindex];
    }
  else if (cmpncs(dtab->type,"xyvsy") == 0)
    {
      info->x_index = -999;
      info->value = pa->un.data[yindex];
    }
  else if ((cmpncs(dtab->type,"xvsy") == 0) || (cmpncs(dtab->type,"scatter") == 0))
    {
      if ((info->x_index!=-999) && (info->y_index!=-999))
	{
	  info->value  = pa->un.data[xindex];
	  info->value2 = pa2->un.data[yindex];
	}
    }
  else if (cmpncs(dtab->type,"vector")==0)
    {
      if ((xindex!=-999) && (yindex!=-999)) info->value = pa->un.data[xindex + (yindex*pa->xs[0][0])];
      if ((xindex!=-999) && (yindex!=-999)) info->value2 = pa2->un.data[xindex + (yindex*pa->xs[0][0])];
    }
  else if (cmpncs(dtab->type,"meshfill") != 0)
    {
      if ((xindex!=-999) && (yindex!=-999)) 
	{
	  if ((cmpncs(dtab->type,"outfill")==0) ||(cmpncs(dtab->type,"outline")==0))
	    info->value = (float)pa->un.idata[xindex + (yindex*pa->xs[0][0])];
	  else
	    info->value = (float)pa->un.data[xindex + (yindex*pa->xs[0][0])];
	}
    }

  /* Color */
  
  if (cmpncs(dtab->type,"meshfill") == 0)
    {

      info->color = find_color_from_fill_range(pGfm->line,info->value,pdata->min,pdata->max,pGfm->missing);
      /* Now the part to figure out the color */
      
      /* 	pj=&p_PRJ; */
      /* 	xx=(double)((point.x-pj->cX)/pj->sX); */
      /* 	yy=(double)((point.y-pj->cY)/pj->sY); */
      /* 	point.x=(float)xx; */
      /* 	point.y=(float)yy; */
      /* test with XGKS */
      /* 	printf("OK before query the loc is: %f,%f\n",point.x,point.y); */
      /* 	n=ginqpixel(self->wkst_id,&point,&erret); */
      /* 	printf("OK after query the colour/error are: %d,%d\n",n,erret); */
      /* Now sets the levels if needed and assume reference at 0.0.	*/
    }
  else if (cmpncs(dtab->type,"boxfill") == 0)
    {
/*       printf("Color for boxfill....\n"); */
      pgfb=&Gfb_tab;
      while (pgfb != NULL)
	{
	  if (strcmp(dtab->g_name,pgfb->name) == 0) break;
	  pgfb=pgfb->next;
	}
      pGfb=pgfb->pGfb_attr;
      
      pB=&A_tab;
      while (pa != NULL)
	{
	  if (strlen(pB->name)>(size_t)0 && pB->pA_attr->notok==0 
	      && strcmp(pB->name,dtab->a[0]) == 0) break;
	  pB=pB->next;
	}
      
      pdata=pB->pA_attr;

      i=1;
      if (pGfb->boxfill_type == 3)
	{ /* Compute the custom of the data */
	  save_lev_1 = pGfb->lev1;
	  save_lev_2 = pGfb->lev2;
	  pGfb->lev1 = pGfb->lev2 = 1.e+20;
	  if (pGfb->boxfill_type == 3) { /* Store originial color legend values for custom */
	    save_color_1 = pGfb->color_1;
	    save_color_2 = pGfb->color_2;
	    for (num_regis=0, pfiso=pGfb->line; pfiso!=NULL; num_regis++, pfiso=pfiso->next);
	    pGfb->color_1 = 16; /* Set the min color for the regis */
	    pGfb->color_2 = 16+num_regis-1; /* Set the max color for the regis */
	  }
	  set_bfills(pGfb,pdata,save_regis,&j);/* save the original legend color range */
	  if (j == 1)
	    set_bfills(pGfb,pdata,regis,&num_regis);/* save the original legend color range */
	  if (j != 0) { /* Don't change the colors for 'linear' */
	    for (i=0, pfiso=pGfb->line; pfiso!=NULL; i++, pfiso=pfiso->next) {
              regis[i].l1 = pfiso->lev1; regis[i].l2 = pfiso->lev2;
	      for (ptb=ftab=&Tf_tab;
		   ftab!=NULL && cmpncs(pfiso->fill_name,ftab->name)!=0;
		   ptb=ftab,ftab=ftab->next);
	      regis[i].color = *ftab->faci;
	    }
	  }
	}
      else if (pGfb->boxfill_type == 2)
	{ /* Compute the log10 of the data */
          save_min = pa->min; save_max = pa->max, save_mean = pa->mean;
          for (l = 0; l < pa->ND; l++) size *= *pa->xs[l];
          if ((save_data=(float *)malloc(size*sizeof(float)))==NULL) {
              err_warn(1,fperr, "Not enough memory to store plot data.\n");
              return 0;
          }
          if ((save_mask=(short *)malloc(size*sizeof(short)))==NULL) {
              err_warn(1,fperr, "Not enough memory to store plot data.\n");
              return 0;
          }
          for (l=0; l<size; ++l) {
              save_data[l] = pa->un.data[l];
              save_mask[l] = pa->mask[l];
          }

          replace_boxfill_data_f(pa->un.data,pa->mask,&pa->xs[0],pa->ND,pa->min,pa->max,
				 pGfb->boxfill_type,pGfb->legend, pGfb->line,&num_regis,&data_regis);
          mmmm(pa->un.data,pa->mask,&pa->xs[0],&pa->xw[0],2,
                        &pa->XC[0],&pa->xv[0],&pa->min,&pa->max,&pa->mean);

	  set_bfills(pGfb,pa,regis,&i);

          pa->min = save_min; pa->max = save_max; pa->mean = save_mean;
          free((char *) pa->un.data);
          free((char *) pa->mask);
          pa->un.data = save_data;
          pa->mask = save_mask;

          info->value = log10( info->value );
        }
      else
	{
	  set_bfills(pGfb,pdata,regis,&i);
	}
      info->color=pGfb->missing;
      for (j=0;j<i;j++)
	{
/* 	  printf("%f, %f, %f ---- %d\n",regis[j].l1,info->value,regis[j].l2,regis[j].color); */
	  if ((regis[j].l1<=info->value) && (regis[j].l2>=info->value))
	    {
	      info->color=regis[j].color;
	    }
	}

      if (pGfb->boxfill_type == 3) { /* Restore the originial color legend values for custom */
	num_regis=save_num_regis;
	pGfb->color_1 = save_color_1;
	pGfb->color_2 = save_color_2;
	for (k=0; k < save_num_regis; k++ ) {
	  regis[k].l1    = save_regis[k].l1;
	  regis[k].l2    = save_regis[k].l2;
	}
      }
      /* Restore the originial levels values for log10 or custom */
      if (pGfb->boxfill_type == 3) { 
	pGfb->lev1 = save_lev_1;
	pGfb->lev2 = save_lev_2;
	for (j=0; j < save_num_regis; j++ ) regis[j].color = save_regis[j].color;
      } 
      if (num_regis == 0) set_bfills(pGfb,pdata,regis,&num_regis);
   }
  else if (cmpncs(dtab->type,"isofill") == 0)
    {
/*		If no isofill ranges specified then compute an interval
		and assume reference at 0.0.				*/
      pgfi=&Gfi_tab;
      while (pgfi != NULL)
	{
	  if (strcmp(dtab->g_name,pgfi->name) == 0) break;
	  pgfi=pgfi->next;
	}
      pGfi=pgfi->pGfi_attr;
      
      pB=&A_tab;
      while (pa != NULL)
	{
	  if (strlen(pB->name)>(size_t)0 && pB->pA_attr->notok==0 
	      && strcmp(pB->name,dtab->a[0]) == 0) break;
	  pB=pB->next;
	}
      
      pdata=pB->pA_attr;

      info->color = find_color_from_fill_range(pGfi->line,info->value,pdata->min,pdata->max,pGfi->missing);
      
    }

  extern int killP(struct p_tab *p);
      killP( pP ); /* Remove the newly created picture template. */
};
