#define STRMAX 256

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "picture.h"
#include "graph.h"
#include "display.h"

    extern FILE *fpin,*fpout,*fperr;

    extern int update_ind;

    extern char DGi_attr[5][10];

    extern struct display_tab D_tab;
    extern struct p_tab Pic_tab;
    extern struct a_tab A_tab;
    extern struct gi_tab Gi_tab;
    extern struct displays d_type[NTYPES];


/*	Process an ISOLINE display command.				*/

    int procDGi(str,tok)

      char str[257];
      int *tok;

      {
	int tokm,tokn;
	int i,j,k,c;

	char strm[STRMAX+1],strn[STRMAX+1];
	float v;
	struct p_tab *ptab, *pp;
	struct a_tab *pa;
	struct gi_tab *pg;

	char name[17];
	char priority[17];
	char g_name[1024];
	char p_name[17];
	char a[6][17];

	char *pc;


	if (*tok != '(')
	  {
	   flushp(fpout);
	   err_warn(1,fperr,
		"Error (procDGi) - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }

	name[1]='\0';
	priority[1]='\0';
	g_name[1]='\0';
	p_name[1]='\0';
	for (i=0; i < 6; i++) a[i][0]='\0';

	while ( (c=getsttk(strm,&tokm)) != EOF && tokm != EOF)
	  {
	   if (tokm != '=')
	     {
	      flushp(fpout);
	      err_warn(1,fperr,
		"Error (procDGi) - not a proper token (%s%c%s%c).\n",
			str,*tok,strm,tokm);
	      return 0;
	     }
	   for (i=0; i < 5 && cmpncs(strm,DGi_attr[i]) != 0; i++);
	   if (i == 5)
	     {
	      flushp(fpout);
	      err_warn(1,fperr,
		"Error - name not found (%s%c%s%c).\n",str,*tok,strm,tokm);
	      return 0;
	     }

	   if ( (c=getsttk(strn,&tokn)) == EOF || tokn == EOF)
	     {
	      flushp(fpout);
	      err_warn(1,fperr,
		"Error - EOF found (%s%c%s%c).\n",str,*tok,strm,tokm);
	      return 0;
	     }
	   if (i == 0) {
              strncpy(name,strn,17); name[16] = '\0';
	   }

	   else if (i == 1)
	     {
	      if (!isnum(strn) )
		{
		 flushp(fpout);
		 err_warn(1,fperr,
		   "Error - value should be a number (%s%c%s%c%s).\n",
			str,*tok,strm,tokm,strm);
		 return 0;
		}
	      strncpy(priority,strn,17); priority[16] = '\0';
	     }
	   else if (i == 2)
	     {
	      
	      strncpy(g_name,strn,1024); g_name[1023] = '\0';
	      for (pg=&Gi_tab;pg!=NULL&&cmpnbl(pg->name,g_name)!=0;pg=pg->next);
	      if (pg == NULL)
	        {
	         flushp(fpout);
	         err_warn(1,fperr,
		   "Error - isoline name doesn't exist (%s%c%s=%s).\n",
			str,*tok,DGi_attr[2],g_name);
	         return 0;
	        }
	     }
	   else if (i == 3)
	     {
	      strncpy(p_name,strn,17); p_name[16] = '\0';
	      for (pp=&Pic_tab;pp!=NULL&&cmpnbl(pp->name,p_name)!=0;pp=pp->next);
	      if (pp == NULL)
	        {
	         flushp(fpout);
	         err_warn(1,fperr,
		   "Error - picture element name doesn't exist (%s%c%s=%s).\n",
			str,*tok,DGi_attr[3],p_name);
	         return 0;
	        }
	     }

	   else if (i == 4)
	     {
	      if (tokn == '(')
		{
		 k=0;
		 while ( (c=getsttk(strn,&tokn)) > 0)
		   {
		    if (k < 6)
		      {
		       strncpy(a[k++],strn,17); a[k++][16] = '\0';
	               for (pa=&A_tab;
				pa!=NULL&&cmpnbl(pa->name,a[0])!=0;
				pa=pa->next);
		       if (pa == NULL)
			 {
			  flushp(fpout);
			  err_warn(1,fperr,
			     "Error - array name doesn't exist (%s%c%s=%s).\n",
			     str,*tok,DGi_attr[4],a[0]);
			  return 0;
			 }
		      }
		    else
		      {
		       flushp(fpout);
		       err_warn(1,fperr,
			 "Error - too many arrays in a display command\n");
		      }
		    if (tokn == ')') {getsttk(strn,&tokn); break;}
	            if (c == EOF || tokn == EOF)
		      {
		       flushp(fpout);
		       err_warn(1,fperr,
			"Error - EOF found (%s%c%s%c).\n",str,*tok,strm,tokm);
		       ungetp(c,fpin);
		       return EOF;
		      }
		   }
	        }
	      else
		{
	         strncpy(a[0],strn,17); a[0][16] = '\0';
	         for (pa=&A_tab;pa!=NULL&&cmpnbl(pa->name,a[0])!=0;pa=pa->next);
	         if (pa == NULL)
	           {
	            flushp(fpout);
	            err_warn(1,fperr,
		      "Error - array name doesn't exist (%s%c%s=%s).\n",
			str,*tok,DGi_attr[4],a[0]);
	            return 0;
	           }
		}
	     }

	   if (tokn == ')') break;
	   if (tokn != ',')
	     {
	      flushp(fpout);
	      err_warn(1,fperr,
		"Error (procDGi) - not a proper token (%s%c%s%c%s%c).\n",
			str,*tok,strm,tokm,strn,tokn);
	      return 0;
	     }
	  } /* end of while != EOF					*/

/*		When no name was given set it to "0".			*/

        if (strprt(name)<=0) {name[0]='0'; name[1]='\0';}

	if (c == EOF || tokm == EOF) {tokm=EOF; ungetp(tokm,fpin);}
	c=d_update("ISOLINE",0,name,priority,"-2",g_name,p_name,a);
	if (tokm == EOF) c=EOF;
	return c;
      }
