#include "gks.h"
#include "gksshort.h"
#ifdef USEX11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif
#include "workstations.h"

	extern struct workstations Wkst[];

/*		Get the workstation number for the canvas if it's
		open, zero if it isn't.				*/

    int check_canvas_defer ()
      {
	int i;
        Gintlist wsid;
        int *pid;
	int wks;

/*			Get the "cgm" workstation id.	*/

	for(i=0;(wks=Wkst[i].id) > 0 && 
			         cmpncs(Wkst[i].type,"X_WIN") != 0; i++);
        gqopwk(&wsid);
        for (i=0,pid=wsid.integers;i<wsid.number;pid++,i++)
	  {
           if (*pid == wks)
	     {
	      free((char *)wsid.integers);
	      return wks;
	     }
	  }
	if (wsid.number > 0 && wsid.integers != NULL) 
				free((char *)wsid.integers);
	return 0;
	
      }
