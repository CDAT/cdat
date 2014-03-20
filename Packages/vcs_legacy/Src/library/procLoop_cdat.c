#define STRMAX 256

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "display.h"
#if defined(INTERFACE) || defined(PYTHON)
#include "workstations.h"
#include "vcs_legacy_canvas.h"
#include "animation.h" 
#include "cdunif.h"

#include "Python.h"
#include "import.h"
#include "graminit.h"
#include "pythonrun.h"
#include "sysmodule.h"
#include "numpy/arrayobject.h"
#include "abstract.h"


#endif
#if defined(PYTHON)
    extern struct workstations Wkst[];
    extern Gconid_X_drawable connect_id;
    extern CANVASINFO_LINK    head_canvas_info; /* connection ID info */

    int     index_s[CU_MAX_VAR_DIMS];
    int     index_e[CU_MAX_VAR_DIMS];
#endif

    extern FILE *fpin,*fpout,*fperr;

    extern struct display_tab D_tab;
    extern struct a_tab A_tab;

    extern int update_ind;

    extern int I;
    extern int J;
    extern int K;
    extern int L;
    extern int M;
    extern int N;

    extern int loop_pause;
    extern int loop_exit;

    void dispatch_the_next_event();

    extern int Inactive;

/*		Loop through index ranges in CDAT.			*/

    int loopit_cdat(aptr, cptr,a,b,c,d,e,f,psf,rasf,netcdff,hdff,drsf,aname,msf,iv)
        CANVASINFO_LINK    cptr; /* connection ID info */
        ANIMATIONWINDOWLIST_LINK aptr;/*point to the correct animation window*/
	int a[],b[],c[],d[],e[],f[];
	char *psf,*rasf,*netcdff,*hdff,*drsf,*aname,*msf;
	unsigned int iv;

      {
        struct display_tab *pd;
        extern struct display_tab D_tab;
	int i,j,k,l,m,n,p,nd,gnd,ct;
	int Na,Ma,La,Ka,Ja,Ia;
	int error;
	char buf[120];
	FILE *fpsave;
	void get_new_display();
	int get_number_of_dimensions();
        int index_ss[CU_MAX_VAR_DIMS], index_ee[CU_MAX_VAR_DIMS];
	extern int store_image_in_memory();

	i=I; j=J; k=K; l=L; m=M; n=N;
	if (a[0] <= 0 && a[1] <= 0) a[0]=a[1]=I;
	if (b[0] <= 0 && b[1] <= 0) b[0]=b[1]=J;
	if (c[0] <= 0 && c[1] <= 0) c[0]=c[1]=K;
	if (d[0] <= 0 && d[1] <= 0) d[0]=d[1]=L;
	if (e[0] <= 0 && e[1] <= 0) e[0]=e[1]=M;
	if (f[0] <= 0 && f[1] <= 0) f[0]=f[1]=N;
	if (a[0] > 0 && a[1] > 0)
	  {
	   if (a[2] == 0) a[2]=1;
	   if ((a[1]-a[0])*a[2] < 0) a[2]=-a[2];
	  }
	else
	  {
	   err_warn(1,fperr,"Error - error in (I) loop syntax.\n");
	   return 0;
	  }
	if (b[0] > 0 && b[1] > 0)
	  {
	   if (b[2] == 0) b[2]=1;
	   if ((b[1]-b[0])*b[2] < 0) b[2]=-b[2];
	  }
	else
	  {
	   err_warn(1,fperr,"Error - error in (J) loop syntax.\n");
	   return 0;
	  }
	if (c[0] > 0 && c[1] > 0)
	  {
	   if (c[2] == 0) c[2]=1;
	   if ((c[1]-c[0])*c[2] < 0) c[2]=-c[2];
	  }
	else
	  {
	   err_warn(1,fperr,"Error - error in (K) loop syntax.\n");
	   return 0;
	  }
	if (d[0] > 0 && d[1] > 0)
	  {
	   if (d[2] == 0) d[2]=1;
	   if ((d[1]-d[0])*d[2] < 0) d[2]=-d[2];
	  }
	else
	  {
	   err_warn(1,fperr,"Error - error in (L) loop syntax.\n");
	   return 0;
	  }
	if (e[0] > 0 && e[1] > 0)
	  {
	   if (e[2] == 0) e[2]=1;
	   if ((e[1]-e[0])*e[2] < 0) e[2]=-e[2];
	  }
	else
	  {
	   err_warn(1,fperr,"Error - error in (M) loop syntax.\n");
	   return 0;
	  }
	if (f[0] > 0 && f[1] > 0)
	  {
	   if (f[2] == 0) f[2]=1;
	   if ((f[1]-f[0])*f[2] < 0) f[2]=-f[2];
	  }
	else
	  {
	   err_warn(1,fperr,"Error - in (N) loop syntax.\n");
	   return 0;
	  }
/*		Turn off the output script when looping.		*/

	fpsave=fpout;
	fpout=NULL;

#if defined(PYTHON)
	for (p = 0; p < CU_MAX_VAR_DIMS; p++) {
           index_ss[p] = index_s[p];
           index_ee[p] = index_e[p];
	}

	/* get the number of dimensions for the data and the grapics method */
	error = get_number_of_dimensions(cptr, &nd, &gnd);

	if (error) {
          err_warn(1,fperr,
          "Error getting the number of dimensions for the data and graphics method.\n");
          return 0;
        }

	/* set the animation loop */
	for (p=gnd, ct=1; p < nd; p++, ct++) {
            if (ct == 1) {
               if (index_s[p] != -1) a[0] = index_s[p];
	       if (index_e[p] != -1) a[1] = index_e[p];
	    } else if (ct == 2) {
               if (index_s[p] != -1) b[0] = index_s[p];
	       if (index_e[p] != -1) b[1] = index_e[p];
	    } else if (ct == 3) {
               if (index_s[p] != -1) c[0] = index_s[p];
	       if (index_e[p] != -1) c[1] = index_e[p];
	    } else if (ct == 4) {
               if (index_s[p] != -1) d[0] = index_s[p];
	       if (index_e[p] != -1) d[1] = index_e[p];
	    } else if (ct == 5) {
               if (index_s[p] != -1) e[0] = index_s[p];
	       if (index_e[p] != -1) e[1] = index_e[p];
	    } else if (ct == 6) {
               if (index_s[p] != -1) f[0] = index_s[p];
	       if (index_e[p] != -1) f[1] = index_e[p];
            }
	}
#endif

        loop_exit = 0;
	I=J=K=L=M=N=-1000;
	for (Na=f[0];(Na-f[1])*(Na-f[0])<=0;Na+=f[2])
	 for (Ma=e[0];(Ma-e[1])*(Ma-e[0])<=0;Ma+=e[2])
	  for (La=d[0];(La-d[1])*(La-d[0])<=0;La+=d[2])
	   for (Ka=c[0];(Ka-c[1])*(Ka-c[0])<=0;Ka+=c[2])
	    for (Ja=b[0];(Ja-b[1])*(Ja-b[0])<=0;Ja+=b[2])
	     for (Ia=a[0];(Ia-a[1])*(Ia-a[0])<=0;Ia+=a[2])
		{
		 if (check_index(Ia,Ja,Ka,La,Ma,Na) != 0)
		   {
		    update_ind=1;
		    /*vcs_legacy_canvas_update(0);*/
		    get_new_display(cptr, nd, gnd, Ia,Ja,Ka,La,Ma,Na);
		    if (psf[0] != '\0') cgmeta(psf,1);
		    if (rasf[0] != '\0') raster_dump(rasf,1);
		    if (netcdff[0] != '\0') storenetCDF(aname,netcdff,1);
#ifdef HDF
		    if (hdff[0] != '\0') storeHDF(aname,hdff,1);
#endif
#ifdef DRS
		    if (drsf[0] != '\0') storeDRS(aname,drsf,1);
#endif
		    if (msf[0] != '\0') {
                       error=store_image_in_memory(cptr, ++aptr->memory_ct);
		       if (!error) return 0;
                    }
		    dispatch_the_next_event();
		    if (loop_exit) goto exit_loop;
		    if (iv > 0) sleep(iv);
		   }
		 else
		   {
		    err_warn(1,fperr,
			"Error - no indices changed or they're not used"
				" (%d %d %d %d %d %d).\n",I,J,K,L,M,N);
		    return 0;
		   }
	        }
#if defined(INTERFACE) || defined(PYTHON)
exit_loop:	loop_exit = 0;
#endif
/*			Restore script output.				*/

	fpout=fpsave;
	if (!Inactive && fpout != NULL)
		 prtLoop(fpout,a,b,c,d,e,f,psf,rasf,netcdff,hdff,drsf,aname,iv);
	if (check_index(i,j,k,l,m,n) != 0) update_ind=1;

#if defined(PYTHON)
	for (p = 0; p < CU_MAX_VAR_DIMS; p++) {
           index_s[p] = index_ss[p];
           index_e[p] = index_ee[p];
	}
#endif
	return 1;

      }

#ifdef PYTHON

void dispatch_the_next_event()
{
        int                     event_loop;
#ifdef USEX11
        XEvent                  report;         /* will return our events */

        /* Check for next index event */
        event_loop = XPending(connect_id.display);
        while (event_loop != 0) {
                XNextEvent(connect_id.display, &report);
                /*XFilterEvent(&report, connect_id.drawable);*/
                event_loop = XPending(connect_id.display);
        }
#endif
}


void get_new_display(cptr,nd, gnd, Ia,Ja,Ka,La,Ma,Na)
CANVASINFO_LINK    cptr; /* connection ID info */
int nd, gnd;
int Ia,Ja,Ka,La,Ma,Na;
{
	struct a_tab  *ptab;
	struct display_tab *pd;
	char	buf[1000];
	int p,ct;
	void loop_clear();
	extern int clearCanvas();
	extern struct a_tab A_tab;
	extern struct display_tab D_tab;
	extern int run_cmd();
	extern PyObject *PyVCS_plot();

	/* Search the displays to determine whether it is being displayed. */
        for (pd=&D_tab;pd != NULL;pd=pd->next) {
           if (pd->type[0] != '\0' || pd->g_name[0] != '\0' ||
                pd->p_name[0] != '\0' || pd->off != 1)
              break;
	}

	/* set the plot index */
	/*index_s[0] = index_e[0] = (Ia-1);*/
	for (p=gnd, ct=1; p < nd; p++, ct++) {
            if (ct == 1)
	       index_s[nd-p-1] = index_e[nd-p-1] = (Ia-1);
	    else if (ct == 2)
	       index_s[nd-p-1] = index_e[nd-p-1] = (Ja-1);
	    else if (ct == 3)
	       index_s[nd-p-1] = index_e[nd-p-1] = (Ka-1);
	    else if (ct == 4)
	       index_s[nd-p-1] = index_e[nd-p-1] = (La-1);
	    else if (ct == 5)
	       index_s[nd-p-1] = index_e[nd-p-1] = (Ma-1);
	    else if (ct == 6)
	       index_s[nd-p-1] = index_e[nd-p-1] = (Na-1);
	}

        /* Find the data in the VCS data table */
        ptab=&A_tab;
        while ((ptab != NULL) && (strcmp(ptab->name,pd->a[0]) != 0)) {
           ptab=ptab->next;
        }
 
        /* check if data exist */
        if (ptab == NULL)
	   return ;

	/* call the Python routine to plot the new index */
	/*clearCanvas(); * clear the VCS Canvas */
	loop_clear(cptr);
	PyVCS_plot(NULL, NULL); /* plot the next frame */
}

int get_number_of_dimensions(cptr, nd, gnd)
CANVASINFO_LINK    cptr; /* connection ID info */
int *nd;
int *gnd; 
{
        struct a_tab  *ptab;
        struct display_tab *pd;
        extern struct a_tab A_tab;
        extern struct display_tab D_tab;
	extern int graphics_num_of_dims();

        /* Search the displays to determine whether it is being displayed. */
        for (pd=&D_tab;pd != NULL;pd=pd->next) {
	   if (strcmp(cptr->dlist->display_name, pd->name) == 0)
              break;
        }

	if (pd == NULL)
	   return 1;

	*gnd = graphics_num_of_dims(pd->type);

	/* Find the data in the VCS data table */
	ptab=&A_tab;
        while ((ptab != NULL) && (strcmp(ptab->name,pd->a[0]) != 0)) {
           ptab=ptab->next;
        }

	if (ptab == NULL)
	   return 1;

	*nd = ptab->pA_attr->ND;

	return 0;
}

void loop_clear(cptr)
CANVASINFO_LINK    cptr; /* connection ID info */
{
        struct display_tab      *dtab;
        extern struct display_tab D_tab;
        display_name_list       *cdptr, *tcdptr;
        int                     i, gnarray;
        int                     graphics_num_of_arrays();
        char                    a_name[6][17];
	char		 	dname[1024];
        extern int              clear_display();
#ifdef USEX11
        extern Pixmap           copy_pixmap();
#endif
        extern int              clearCanvas();
	extern void             remove_display_name();
        extern int              removeA();

        /* Remove the display from the VCS picture form and
         * remove all the data from the VCS data table
         */
        cdptr = cptr->dlist;
        while (cdptr != NULL) {
           dtab=&D_tab;
           while ( strcmp(dtab->name, cdptr->display_name) != 0)
                 dtab = dtab->next;
           gnarray = graphics_num_of_arrays(dtab->type);
           for (i=0; i<gnarray; i++)
               strcpy(a_name[i], dtab->a[i]);
	   strcpy(dname, cdptr->display_name);
           clear_display(dname);
           for (i=0; i<gnarray; i++)       
              removeA(a_name[i]);

	   /* remove display and display name from link list */
           tcdptr = cdptr;
           cdptr = cdptr->next;
           free((char *) tcdptr->display_name);
	   free((char *) tcdptr);
        }
	cptr->dlist = NULL;

        /*
         * Make sure the VCS canvas is up and running
         * before copying the blank canvas to the pixmap backingstore.
         */
#ifdef USEX11
        if (cptr->connect_id.drawable != 0) {
#else
	  if (cptr->connect_id.cr != NULL ) {
#endif
        
#if USEQT
      extern void vcs_legacy_Qt_clear_window_by_id_without_repaint(int);
      vcs_legacy_Qt_clear_window_by_id_without_repaint(cptr->connect_id.wkst_id);
#else
      clearCanvas(cptr->connect_id); /* blank the VCS Canvas */
#endif

#ifdef USEX11
          if (cptr->connect_id.canvas_pixmap != (Pixmap) NULL)
              XFreePixmap(cptr->connect_id.display, cptr->connect_id.canvas_pixmap);
           cptr->connect_id.canvas_pixmap = copy_pixmap( cptr->connect_id );
#elif defined USEQT
	   /* Nothing to do for QT */
#else
	   fprintf(stderr,"humm... maybe we need some code for procLoop_cdat\n");
#endif
        }
}

#endif
