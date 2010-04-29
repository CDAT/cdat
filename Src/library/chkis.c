/*		Check whether the array data is used for a display.
		If so, a display update is needed.			*/
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "list.h"
#include "display.h"
#include "graph.h"

#define MAXELEM 120
#define STRMAX 256
#define TRUE 1
#define FALSE 0

    extern struct a_tab A_tab;
    extern struct display_tab D_tab;
    extern struct gi_tab Gi_tab;
    extern struct go_tab Go_tab;
    extern struct gfi_tab Gfi_tab;
    extern struct gfo_tab Gfo_tab;
    extern struct gcon_tab Gcon_tab;

    extern int update_ind;

    extern int Inactive;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */

/*		Check whether a change to an array attribute set
		will affect any display.
		Check also covers calculated variables.
		Return true if it might affect a display and false
		otherwise.						*/

    int chkisA_in_D(name)
      char *name;
      {
       int i;

       struct display_tab *pd;
       struct a_tab *p;
       struct a_attr *pa;

/*			Check the "on" displays.			*/
       pd=&D_tab;
       while (pd != NULL)
	 {
	  if (pd->off == 0)
	    for (i=0;i < pd->na && pd->a[i][0] != '\0';i++)
	       if (strcmp(pd->a[i],name) == 0)
		  return 1;
	  pd=pd->next;
         }
/*			Check if it's used in computations and
			the computations displayed.			*/
       p=&A_tab;
       while (p != NULL && (pa=p->pA_attr) != NULL)
	 {
	  if (pa->notok != 0 && pa->f != NULL && findnam(name,pa->f))
		chkisA_in_D(p->name);
	  p=p->next;
	 }
       return 0;
      }
/*		Check whether a list is used in an array attribute set
		and if so, is the array attriubte set being used in an
		active display.
		Return true if it might affect a display and false
		otherwise.						*/

    int chkisL_in_A(char *str)

      {
	int i;
	int change;
	char lstr[257];
	struct a_tab *pA;
	struct a_attr *pa;
	char **pcc;
	char *pc;

       change=0;
       pA=&A_tab;
       strcpy(lstr,"L_");
       strcat(lstr,str);
       while (pA != NULL && pA->pA_attr != NULL)
	 {
	  pa=pA->pA_attr;
	  if (pa->notok == 0)
	    {
	     for (pcc=&pa->aF;change == 0 && pcc <= &pa->axu[NDS];pcc++)
	       {
	        pc=*pcc;
	        if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
		   change=1;
	       }
	     for (i=0;change == 0 && i < NDS;i++)
	       {
	        if ((pa->axv[i] != NULL && cmpnbl(lstr,pa->axv[i]) == 0) ||
		    (pa->axb[i] != NULL && cmpnbl(lstr,pa->axb[i]) == 0) ||
	            (pa->axw[i] != NULL && cmpnbl(lstr,pa->axw[i]) == 0) ||
		    (pa->axf[i] != NULL && cmpnbl(lstr,pa->axf[i]) == 0) ||
	            (pa->axl[i] != NULL && cmpnbl(lstr,pa->axl[i]) == 0)  )
	           change=1;
	        if ((pa->axv[i] != NULL && cmpnbl(str,pa->axv[i]) == 0) ||
		    (pa->axb[i] != NULL && cmpnbl(str,pa->axb[i]) == 0) ||
	            (pa->axw[i] != NULL && cmpnbl(str,pa->axw[i]) == 0) ||
		    (pa->axf[i] != NULL && cmpnbl(str,pa->axf[i]) == 0) ||
	            (pa->axl[i] != NULL && cmpnbl(str,pa->axl[i]) == 0)  )
	           change=1;
	       }
	     for (i=0;change == 0 && i < NDS;i++)
	       {
	        if ((pa->aXV[i] != NULL && cmpnbl(lstr,pa->aXV[i]) == 0) ||
		    (pa->aXB[i] != NULL && cmpnbl(lstr,pa->aXB[i]) == 0) ||
	            (pa->aXW[i] != NULL && cmpnbl(lstr,pa->aXW[i]) == 0) ||
		    (pa->aXF[i] != NULL && cmpnbl(lstr,pa->aXF[i]) == 0) ||
	            (pa->aXL[i] != NULL && cmpnbl(lstr,pa->aXL[i]) == 0)  )
	           change=1;
	        if ((pa->aXV[i] != NULL && cmpnbl(str,pa->aXV[i]) == 0) ||
		    (pa->aXB[i] != NULL && cmpnbl(str,pa->aXB[i]) == 0) ||
	            (pa->aXW[i] != NULL && cmpnbl(str,pa->aXW[i]) == 0) ||
		    (pa->aXF[i] != NULL && cmpnbl(str,pa->aXF[i]) == 0) ||
	            (pa->aXL[i] != NULL && cmpnbl(str,pa->aXL[i]) == 0)  )
	           change=1;
	       }
	    }
	  if (change > 0)
	    {
/*		Check displays for use of the array.			*/
	     if (chkisA_in_D(pA->name)) return TRUE;
	    }
	  pA=pA->next;
	 }
	return 0;
      }

/*		Check graphics attribute sets for use of a list.
		Return TRUE if it is used and the graphics 
		attribute set is used in a display.			*/

    int chkisL_in_G(char *str)

      {
	char lstr[257];
	struct a_tab *pA;

	struct gi_tab *pGi;
	struct gi_attr *pgi;
	struct gfi_tab *pGfi;
	struct gfi_attr *pgfi;
	struct go_tab *pGo;
	struct go_attr *pgo;
	struct gfo_tab *pGfo;
	struct gfo_attr *pgfo;
	struct gcon_tab *pGcon;
	struct gcon_attr *pgcon;
	char *pc;

       strcpy(lstr,"L_");
       strcat(lstr,str);

       pGi=&Gi_tab;
       while (pGi != NULL && (pgi=pGi->pGi_attr) != NULL)
	 {
	  for (pc=pgi->xtl1; pc < &pgi->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		if (chkisG_in_D("isoline",pGi->name)) return 1;
	       }
	    }
/*		Check displays for use of the graphic display attributes.*/
	  pGi=pGi->next;
	 }
       pGfi=&Gfi_tab;
       while (pGfi != NULL && (pgfi=pGfi->pGfi_attr) != NULL)
	 {
	  for (pc=pgfi->xtl1; pc < &pgfi->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		if (chkisG_in_D("isofill",pGfi->name)) return 1;
	       }
	    }
/*		Check displays for use of the graphic display attributes.*/
	  pGfi=pGfi->next;
	 }
       pGo=&Go_tab;
       while (pGo != NULL && (pgo=pGo->pGo_attr) != NULL)
	 {
	  for (pc=pgo->xtl1; pc < &pgo->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		if (chkisG_in_D("outline",pGo->name)) return 1;
	       }
	    }
/*		Check displays for use of the graphic display attributes.*/
	  pGo=pGo->next;
	 }
       pGfo=&Gfo_tab;
       while (pGfo != NULL && (pgfo=pGfo->pGfo_attr) != NULL)
	 {
	  for (pc=pgfo->xtl1; pc < &pgfo->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		if (chkisG_in_D("outfill",pGfo->name)) return 1;
	       }
	    }
/*		Check displays for use of the graphic display attributes.*/
	  pGfo=pGfo->next;
	 }
       pGcon=&Gcon_tab;
       while (pGcon != NULL && (pgcon=pGcon->pGcon_attr) != NULL)
	 {
	  for (pc=pgcon->xtl1; pc < &pgcon->ymt2[16];pc+=17)
	    {
	     if (pc != NULL && (cmpnbl(lstr,pc) == 0 || cmpnbl(str,pc) == 0))
	       {
		if (chkisG_in_D("continents",pGcon->name)) return 1;
	       }
	    }
/*		Check displays for use of the graphic display attributes.*/
	  pGcon=pGcon->next;
	 }
       return 0;
     }

/*		Check whether a graphics attribute set is used in an
		active display.
		Return true if it might affect a display and false
		otherwise.						*/

   int chkisG_in_D(char *type,char *name)

     {
       struct display_tab *pd;

       pd=&D_tab;
       while (pd != NULL)
	 {
	  if (cmpncs(type,pd->type)==0 && cmpncs(pd->g_name,name)==0 &&
		pd->off != 0)
		return 1;
	  pd=pd->next;
         }
       return 0;
      }
