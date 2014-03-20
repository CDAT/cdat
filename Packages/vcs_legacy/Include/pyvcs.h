#ifndef PYVCS_H
#define PYVCS_H 1
#include "workstations.h"
#include "Python.h"

/* Structure for VCS canvas objects */
typedef struct {                	/* File */
  PyObject_HEAD
  int wkst_id;				/* Canvas identification number */
  int canvas_id;			/* VCS Canvas ID number */
  int orientation;			/* Orientation flag */
  int virgin;				/* VCS Canvas initially opened */
  int virgin_animation;			/* Canvas animate initially opened */
  Gconid_X_drawable connect_id;         /* VCS canvas drawable ID */
  char *template_name;			/* Template attribute name */
  char *graphics_name;			/* Graphics method attribute name */
  char graphics_type[20];		/* Type of graphics method */
  double vcs_legacy_min;			/* VCS plot minimum value */
  double vcs_legacy_max;			/* VCS plot maximum value */
  int vcs_legacy_ext1;				/* Show on plot the underflow arrow */
  int vcs_legacy_ext2;				/* Show on plot the overflow arrow */
  int vcs_legacy_gui;		 		/* Tell if old VCS (2.7) GUI is used */
  int stopxmainloop;			/* X server flag, stop X main loop */
  int havexmainloop;			/* X server flag, doing X main loop */
  int number_of_frames;	 		/* Number of animation frames */
  int frame_count;	 		/* Position number of animation frame */
  int savecontinents;                   /* Hold the continents type for resize */
  char *background;	 		/* Indicating plot generation in backgroud mode */
  int gui;                              /* Indicate if the VCS Canvas is in GUI mode */
  double orig_ratio;                    /* indicates ratio at creation time A4/us letter other, etc... */
#ifdef X11WM
  XID gui_drawable;                     /* X window drawable from the Tk interface */
#endif
  struct graphics_method_list *glist;   /* List of graphics methods names */
  struct canvas_display_list *dlist;    /* List of display names */
} PyVCScanvas_Object;

/* Structure for VCS Canvas displays */
typedef struct canvas_display_list {       /* Store the display names */
  char *display_name;                      /* Display name */
  struct canvas_display_list *next;        /* Pointer to the next structure */
} canvas_display_list;


/*
	PyEval_InitThreads(); \
	mainThreadState = PyThreadState_Get(); \
	PyEval_ReleaseLock(); \
	*/
	/*PyThreadState_Delete(myThreadState); \*/
#define PY_INIT_THREADS { \
	Py_Initialize(); \
	mainThreadState = PyThreadState_Get(); \
}
#define PY_ENTER_THREADS { \
	PyEval_AcquireLock(); \
	mainInterpreterState = mainThreadState->interp; \
	myThreadState = PyThreadState_New(mainInterpreterState); \
	PyEval_ReleaseLock(); \
}
#define PY_LEAVE_THREADS { \
	PyEval_AcquireLock(); \
	PyThreadState_Swap(NULL); \
	PyThreadState_Clear(myThreadState); \
	PyEval_ReleaseLock(); \
}
#define PY_GRAB_THREAD { \
        PyEval_AcquireLock(); \
        myThreadState = PyThreadState_New(mainThreadState->interp); \
        PyEval_ReleaseLock(); \
        PyEval_AcquireLock(); \
        PyThreadState_Swap(myThreadState); \
}
#define PY_RELEASE_THREAD { \
        PyEval_ReleaseLock(); \
        PyEval_AcquireLock(); \
        PyThreadState_Swap(NULL); \
        PyThreadState_Clear(myThreadState); \
        PyThreadState_Delete(myThreadState); \
        PyEval_ReleaseLock(); \
}
#endif
