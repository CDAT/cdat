#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "list.h"
#include "workstations.h"

#define STRMAX 256

    extern struct a_tab A_tab;
    extern struct l_tab L_tab[2];
    extern char A_strg[38][12];
    extern char A_intg[5][NDS][8];
    extern char A_sflt[5][NDS][8];
    extern char A_vflt[6][NDS][8];
    extern int I,J,K,L,M,N;
    extern Gconid_X_drawable connect_id;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

    int procCOMM (str,tok)
      char *str;
      int *tok;

/*	This function reads and interprets a command in a script.	*/

      {

	int c,tokm, tmp=-99, canvas_ct=1;
        char strm[1024];

	tokm=*tok;

/*		Is it an INDEX command.				*/

	if 	(cmpncs(str,"INDEX") == 0)
	        c=procInd(str,&tokm);

/*		Is it a CGM command?			*/

	else if (cmpncs(str,"CGM") == 0)
		c=procCGM(str,&tokm);

/*		Is it a RASTER command?				*/

	else if (cmpncs(str,"RASTER") == 0)
		c=procRas(str,&tokm);

/*		Is it a netCDF command?				*/

	else if (cmpncs(str,"netCDF") == 0)
		c=procnetCDF(str,&tokm);

/*		Is it a HDF command?				*/

#ifdef HDF
	else if (cmpncs(str,"HDF") == 0)
		c=procHDF(str,&tokm);
#endif

/*		Is it a DRS command?				*/

#ifdef DRS
	else if (cmpncs(str,"DRS") == 0)
		c=procDRS(str,&tokm);
#endif

/*		Is it a PAGE command?				*/

	else if (cmpncs(str,"PAGE") == 0)
		c=procPage(str,&tokm);

/*		Is it a CANVAS command?				*/

	else if (cmpncs(str,"CANVAS") == 0) {
#ifdef USEX11
                connect_id.display = NULL;
#else
		fprintf(stderr,"PLEASE SETUP YOUR CONNECT_ID HERE\n");
#endif
		c=procCanvas("open",&connect_id, canvas_ct, &tmp);
                ++canvas_ct;
                c=getsttk(strm,&tokm);

/*		Is it a COLOR command?				*/

	} else if (cmpncs(str,"COLOR") == 0)
		c=procColor(str,&tokm);

/*		Is it a CLEAR command?				*/

	else if (cmpncs(str,"CLEAR") == 0)
		c=procClear(str,&tokm);

/*		Is it a RENAME command?				*/

	else if (cmpncs(str,"RENAME") == 0)
		c=procRen(str,&tokm);

/*		Is it a COPY command?				*/

	else if (cmpncs(str,"COPY") == 0)
		c=procCop(str,&tokm);

/*		Is it a REMOVE command?				*/

	else if (cmpncs(str,"REMOVE") == 0)
		c=procRem(str,&tokm);

/*		Is it a DUMP command?				*/

	else if (cmpncs(str,"DUMP") == 0)
	        c=procDump(str,&tokm);

/*		Is it a RUN command.				*/

	else if (cmpncs(str,"RUN") == 0)
	        c=procRun(str,&tokm);

/*		Is it a LOOP command.				*/

	else if (cmpncs(str,"LOOP") == 0)
	        c=procLoop(str,&tokm);

/*		Is it a SLEEP command.				*/

	else if (cmpncs(str,"SLEEP") == 0)
	        c=procSleep(str,&tokm);

/*		Is it an OVERLAY_CONTINENTS command.		*/

	else if (cmpncs(str,"OVERLAY_CONTINENTS") == 0)
	        c=procOvly(str,&tokm);

/*		Is it a HINTS command.				*/

	else if (cmpncs(str,"HINTS") == 0)
	        c=procHints(str,&tokm);

/*		Is it a CONTROL command.				*/

	else if (cmpncs(str,"CONTROL") == 0)
	        c=procControl(str,&tokm);


/*		If none of the above					*/

	else
	  {
	   err_warn(1,fperr,"Error - COMMAND (%s%c) not found.\n",str,*tok);
	   return 0;
	  }
	return c;
      }
