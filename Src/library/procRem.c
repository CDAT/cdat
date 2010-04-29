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

/*	Process a remove of a table element for any table.
	The string defining the function must be in str[] and the following
	token must be in tok.						*/

    int procRem(str,tok)

      char str[257];
      int *tok;

      {
	int i,j,c;
	int tokm;
	char strm[36];
	char strp[10];

/*		The token following a name must be left parenthesis.	*/

	if (*tok != '(')
	  {
	   err_warn(1,fperr,"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }
/*		Check the names have a prefix followed by underline like
		"Pfx_" prior to the name.				*/

	j=0;
	while ( (c=getp(fpin,fpout)) != ')' && c != EOF)
	  {
	   if (c > ' ' && j < 16) strm[j++]=c;
	  }
	tokm=c;
	strm[j]='\0';

	for (i=0;i<9 && strm[i] !='_';strp[i]=strm[i],i++);

	if (c==0 || tokm!=')'||
			 i==0 || i==10 || strm[i]!='_')
	  {
	   err_warn(1,fperr,"Error - syntax for (%s%c%s%c).\n",str,*tok,
				strm,tokm);
	   return 0;
	  }
	strp[i]='\0';
/*			Array.						*/

	if (cmpncs(strp,"A") == 0) c=removeA(&strm[2]);

/*			Picture.					*/

	else if (cmpncs(strp,"P") == 0) c=removeP_name(&strm[2]);

/*			Color.						*/

	else if (cmpncs(strp,"C") == 0) c=removeC(&strm[2]);

/*			Graphics Isolines.				*/

	else if (cmpncs(strp,"Gi") == 0) c=removeGi_name(&strm[3]);

/*			Graphics Outlines.				*/

	else if (cmpncs(strp,"Go") == 0) c=removeGo_name(&strm[3]);

/*			Graphics Isofill.				*/

	else if (cmpncs(strp,"Gfi") == 0) c=removeGfi_name(&strm[4]);

/*			Graphics Outfill.				*/

	else if (cmpncs(strp,"Gfo") == 0) c=removeGfo_name(&strm[4]);

/*			Continents.					*/

	else if (cmpncs(strp,"Gcon") == 0) c=removeGcon_name(&strm[5]);

/*			Graphics Image.					*/

	else if (cmpncs(strp,"Gfb") == 0) c=removeGfb_name(&strm[4]);

/*			Graphics Vector.				*/

	else if (cmpncs(strp,"Gv") == 0) c=removeGv_name(&strm[3]);

/*			Graphics X(y) vs y.				*/

	else if (cmpncs(strp,"GXy") == 0) c=removeGXy_name(&strm[4]);

/*			Graphics Y(x) vs x.				*/

	else if (cmpncs(strp,"GYx") == 0) c=removeGYx_name(&strm[4]);

/*			Graphics X(t) vs Y(t).				*/

	else if (cmpncs(strp,"GXY") == 0) c=removeGXY_name(&strm[4]);

/*			Graphics ScatterPlot.				*/

	else if (cmpncs(strp,"GSp") == 0) c=removeGSp_name(&strm[4]);

/*			Lists.					*/

	else if (cmpncs(strp,"L") == 0) c=removeL_name(&strm[2]);

/*			Basic Fill.					*/

	else if (cmpncs(strp,"Tf") == 0) c=removeTf_name(&strm[3]);

/*			Basic Lines.					*/

	else if (cmpncs(strp,"Tl") == 0) c=removeTl_name(&strm[3]);

/*			Basic Text orientation.				*/

	else if (cmpncs(strp,"To") == 0) c=removeTo_name(&strm[3]);

/*			Basic Text.					*/

	else if (cmpncs(strp,"Tt") == 0) c=removeTt_name(&strm[3]);

/*			Basic Format.					*/

	else if (cmpncs(strp,"Th") == 0) c=removeTh_name(&strm[3]);

/*			Basic Mark.					*/

	else if (cmpncs(strp,"Tm") == 0) c=removeTm_name(&strm[3]);


	else
	  {
	   err_warn(1,fperr,"Error - REMOVE(%s) name not found.\n",strm);
	   return 0;
	  }
	return c;
      }
