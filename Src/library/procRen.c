#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "graph.h"
#include "display.h"

#define STRMAX 256
#define TRUE 1
#define FALSE 0

    extern FILE *fpin,*fpout,*fperr;

/*	Process a rename of a table element for any table.
	The string defining the function must be in str[] and the following
	token must be in tok.						*/

    int procRen(str,tok)

      char str[257];
      int *tok;

      {
	int i,c,d;
	int tokm,tokf;
	char strm[36];
	char strf[36];
	char strp[10];

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }
/*		Check the names have a prefix followed by underline
		"Tt_" prior to the names.				*/

	c=getsttk(strm,&tokm);
	d=getsttk(strf,&tokf);
	for (i=0;i<9 && strm[i] !='_' && strm[i]==strf[i];strp[i]=strm[i],i++);

	if (c==0 || d==0 || tokf!=')' || tokm!=','||
			 i==0 || i==10 || strm[i]!='_' || strf[i]!='_' )
	  {
	   err_warn(1,fperr,"Error - syntax for (%s%c%s%c%s%c).\n",str,*tok,
				strm,tokm,strf,tokf);
	   return 0;
	  }
	strp[i]='\0';
	c=0;
/*			Array.				*/

	if (cmpncs(strp,"A") == 0) c=renameA_name(&strm[2],&strf[2]);
/*			Picture.				*/

	else if (cmpncs(strp,"P") == 0) c=renameP_name(&strm[2],&strf[2]);

/*			Graphic Isolines.				*/

	else if (cmpncs(strp,"Gi") == 0) c=renameGi_name(&strm[3],&strf[3]);

/*			Graphics Outlines.				*/

	else if (cmpncs(strp,"Go") == 0) c=renameGo_name(&strm[3],&strf[3]);

/*			Continents.					*/

	else if (cmpncs(strp,"Gcon") == 0) c=renameGcon_name(&strm[5],&strf[5]);

/*			Graphic Isofill.				*/

	else if (cmpncs(strp,"Gfi") == 0) c=renameGfi_name(&strm[4],&strf[4]);

/*			Graphic Outfill.				*/

	else if (cmpncs(strp,"Gfo") == 0) c=renameGfo_name(&strm[4],&strf[4]);

/*			Graphic Image.					*/

	else if (cmpncs(strp,"Gfb") == 0) c=renameGfb_name(&strm[4],&strf[4]);

/*			Graphic Vector.					*/

	else if (cmpncs(strp,"Gv") == 0) c=renameGv_name(&strm[3],&strf[3]);

/*			Graphic X(y) vs y.				*/

	else if (cmpncs(strp,"GXy") == 0) c=renameGXy_name(&strm[4],&strf[4]);

/*			Graphic Y(x) vs x.				*/

	else if (cmpncs(strp,"GYx") == 0) c=renameGYx_name(&strm[4],&strf[4]);

/*			Graphic X(t) vs Y(t).				*/

	else if (cmpncs(strp,"GXY") == 0) c=renameGXY_name(&strm[4],&strf[4]);

/*			Graphic ScatterPlot.				*/

	else if (cmpncs(strp,"GSp") == 0) c=renameGSp_name(&strm[4],&strf[4]);

/*			Lists.						*/

	else if (cmpncs(strp,"L") == 0) c=renameL_name(&strm[2],&strf[2]);

/*			Basic Fill.					*/

	else if (cmpncs(strp,"Tf") == 0) c=renameTf_name(&strm[3],&strf[3]);

/*			Basic Lines.					*/

	else if (cmpncs(strp,"Tl") == 0) c=renameTl_name(&strm[3],&strf[3]);

/*			Basic Text orientation.				*/

	else if (cmpncs(strp,"To") == 0) c=renameTo_name(&strm[3],&strf[3]);

/*			Basic Text.					*/

	else if (cmpncs(strp,"Tt") == 0) c=renameTt_name(&strm[3],&strf[3]);

/*			Basic Format.					*/

	else if (cmpncs(strp,"Th") == 0) c=renameTh_name(&strm[3],&strf[3]);

/*			Basic Mark.					*/

	else if (cmpncs(strp,"Tm") == 0) c=renameTm_name(&strm[3],&strf[3]);

	else
	  {
	   err_warn(1,fperr,
			"Error - RENAME(%s,%s) name not found.\n",strm,strf);
	   return 0;
	  }
	return c;
      }
