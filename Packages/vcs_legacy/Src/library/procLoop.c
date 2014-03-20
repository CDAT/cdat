#define STRMAX 256

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "display.h"


    int memory_ct=0;

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

    extern void dispatch_the_next_event();

    extern int Inactive;

/*	Process a loop on index (I,J,K,L,M,N) command.			*/

    int procLoop(str,tok)

      char str[];
      int *tok;

      {
	int i,j;
	int tokm;
	int a[6][3],b;
	int c,d,r;
	char name[STRMAX+1],memfile[STRMAX+1];
	char psfile[STRMAX+1],rasfile[STRMAX+1];
	char netcdffile[STRMAX+1], hdffile[STRMAX+1], drsfile[STRMAX+1];
	char strm[STRMAX+1];
	char aname[STRMAX+1];
	float v;
	unsigned int iv;

	if (*tok!='(')
	  {
	   err_warn(1,fperr,
		"Error (Loop) - improper token (%s%c).\n",str,*tok);
	   return 0;
	  }
	for (i=0;i<6;i++)
	  for (j=0;j<3;j++)
	    a[i][j]=0;

	psfile[0]=rasfile[0]=netcdffile[0]=hdffile[0]=drsfile[0]=aname[0]=memfile[0]='\0';
	iv=0;

	c=getsttk(strm,&tokm);
	while ( tokm == '(' )
	  {
/*			"I(" or "cgm(" or "raster(" or "netCDF" or "DRS("  */

	   if (c > 0)
	     {
	      for (d=0;d<20 && strm[d] > ' ';d++) name[d]=strm[d];
	      name[d]='\0';
	      if (cmpncs(name,"cgm")==0)
		{
	         c=getsttk(psfile,&tokm);
		 if (c > 0) xtend(psfile,".cgm");
		}
	      else if (cmpncs(name,"raster")==0)
		{
	         c=getsttk(rasfile,&tokm);
		 if (c > 0) xtend(rasfile,".ras");
		}
	      else if (cmpncs(name,"sleep")==0)
		{
	         if ((c=getsttk(strm,&tokm)) > 0 && isnum(strm))
		   {
		    sscanf(strm,"%f",&v);
		    if (v < 1.0) v=1.0;
		    if (v > 100.) v=100.;
		    iv=v;
		   }
		}
	      else if (cmpncs(name,"netCDF")==0)
		{
	         c=getsttk(netcdffile,&tokm);
		 if (c > 0) xtend(netcdffile,".nc");
		}
              else if (cmpncs(name,"HDF")==0)
                {
                 c=getsttk(hdffile,&tokm);
                 if (c > 0) xtend(hdffile,".hdf");
                }
	      else if (cmpncs(name,"DRS")==0)
		{
		 if ( (c=getsttk(aname,&tokm)) == 0 || tokm != ',')
		   {
		    err_warn(1,fperr,"Error - Loop syntax error.\n");
		    return 0;
		   }
	         c=getsttk(drsfile,&tokm);
		 if (c > 0) xtend(drsfile,".dic");
		}
	      else if (name[0]=='I' || name[0]=='J' || name[0]=='K' ||
		       name[0]=='L' || name[0]=='M' || name[0]=='N')
		{
		 b=name[0]-'I';  /*	compute index for saving in a	*/
		 a[b][0]=a[b][1]=a[b][2]=0;
		 c=getsttk(strm,&tokm);
		 if (isnum(strm) && tokm == ',')
		   {
		    for(r=0;strm[r]!='\0';r++) a[b][0]=10*a[b][0]+(strm[r]-'0');
		    c=getsttk(strm,&tokm);
		    if (isnum(strm))
		      {
		       for(r=0;strm[r]!='\0';r++)
					 a[b][1]=10*a[b][1]+(strm[r]-'0');
		       if (tokm == ',')
			 {
			  c=getsttk(strm,&tokm);
			  if (isnum(strm))
			   for(r=0;strm[r]!='\0';r++)
					a[b][2]=10*a[b][2]+(strm[r]-'0');
			 }
		      }
		   }
		 if (tokm != ')' || a[b][0] <= 0 || a[b][1] <= 0)
		   {
		    err_warn(1,fperr,
			"Error - Loop(%c(%d,%d is in error.\n",
							name[0],a[0],a[1]);
		    return 0;
		   }
		}
	      else
		{
		 err_warn(1,fperr,
			"Error - Loop internal name not recognized.\n");
		 return 0;
		}
	      if (tokm != ')')
		{
		 err_warn(1,fperr,"Error - Loop syntax error.\n");
		 return 0;
		}
	     }
	   else
	     {
	      err_warn(1,fperr,"Error - Loop syntax error.\n");
	      return 0;
	     }
	   if ((c=getsttk(strm,&tokm)) == 0 && tokm == ',')
	     {
	      c=getsttk(strm,&tokm);
	     }
	  }
	if (tokm != ')')
	  {
	   err_warn(1,fperr,"Error - Loop syntax error.\n");
	   return 0;	   
	  }
	loopit (a[0],a[1],a[2],a[3],a[4],a[5],psfile,rasfile,
                netcdffile, hdffile, drsfile,aname,memfile,iv);
	return 1;
      }

/*		Loop through index ranges.				*/

    int loopit (a,b,c,d,e,f,psf,rasf,netcdff,hdff,drsf,aname,msf,iv)
	int a[],b[],c[],d[],e[],f[];
	char *psf,*rasf,*netcdff,*hdff,*drsf,*aname,*msf;
	unsigned int iv;

      {
	int i,j,k,l,m,n;
	int Na,Ma,La,Ka,Ja,Ia;
	int error;
	FILE *fpsave;
	extern int store_image_in_memory_vcs_legacy();

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
		    vcs_legacy_canvas_update(0);
		    if (psf[0] != '\0') cgmeta(psf,1);
		    if (rasf[0] != '\0') raster_dump(rasf,1);
		    if (netcdff[0] != '\0') storenetCDF(aname,netcdff,1);
#ifdef HDF
		    if (hdff[0] != '\0') storeHDF(aname,hdff,1);
#endif
#ifdef DRS
		    if (drsf[0] != '\0') storeDRS(aname,drsf,1);
#endif
		    if (msf[0] != '\0')error=store_image_in_memory_vcs_legacy(++memory_ct);
		    if (!error) return 0;

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

exit_loop:	loop_exit = 0;

/*			Restore script output.				*/

	fpout=fpsave;
	if (!Inactive && fpout != NULL)
		 prtLoop(fpout,a,b,c,d,e,f,psf,rasf,netcdff,hdff,drsf,aname,iv);
	if (check_index(i,j,k,l,m,n) != 0) update_ind=1;
	return 1;

      }
/*		Print loop command.				*/

    int prtLoop (fp,a,b,c,d,e,f,psf,rasf,netcdff,hdff,drsf,aname,iv)
      FILE *fp;
      int a[],b[],c[],d[],e[],f[];
      char *psf,*rasf,*netcdff,*hdff,*drsf,*aname;
      unsigned int iv;
      {
       int i;

       i=0;
       fprintf (fp,"Loop(");
       if (a[0] != I || a[1] != I)
	 {
	  fprintf (fp,"I(%d,%d,%d)",a[0],a[1],a[2]);
	  i=1;
	 }
       if (b[0] != J || b[1] != J)
	 {
	  if (i > 0) fprintf (fp,",");
	  fprintf (fp,"J(%d,%d,%d)",b[0],b[1],f[2]);
	  i=1;
	 }
       if (c[0] != K || c[1] != K)
	 {
	  if (i > 0) fprintf (fp,",");
	  fprintf (fp,"K(%d,%d,%d)",c[0],c[1],c[2]);
	  i=1;
	 }
       if (d[0] != L || d[1] != L)
	 {
	  if (i > 0) fprintf (fp,",");
	  fprintf (fp,"L(%d,%d,%d)",d[0],d[1],d[2]);
	  i=1;
	 }
       if (e[0] != M || e[1] != M)
	 {
	  if (i > 0) fprintf (fp,",");
	  fprintf (fp,"M(%d,%d,%d)",e[0],e[1],e[2]);
	  i=1;
	 }
       if (f[0] != N || f[1] != N)
	 {
	  if (i > 0) fprintf (fp,",");
	  fprintf (fp,"N(%d,%d,%d)",f[0],f[1],f[2]);
	  i=1;
	 }
       if (psf[0] != '\0')
	 {
	  if (i > 0) fprintf (fp,",");
	  fprintf (fp,"cgm(%s)",psf);
	  i=1;
	 }
       if (rasf[0] != '\0')
	 {
	  if (i > 0) fprintf (fp,",");
	  fprintf (fp,"raster(%s)",rasf);
	  i=1;
	 }
       if (netcdff[0] != '\0')
         {
          if (i > 0) fprintf (fp,",");
          fprintf (fp,"netCDF(%s,%s)",aname,netcdff);
          i=1;
         }
       if (hdff[0] != '\0')
         {
          if (i > 0) fprintf (fp,",");
          fprintf (fp,"HDF(%s,%s)",aname,hdff);
          i=1;
         }
       if (drsf[0] != '\0')
	 {
	  if (i > 0) fprintf (fp,",");
	  fprintf (fp,"DRS(%s,%s)",aname,drsf);
	  i=1;
	 }
       if (iv != 0)
	 {
	  if (i > 0) fprintf (fp,",");
	  fprintf (fp,"SLEEP(%d)",iv);
	  i=1;
	 }
       fprintf (fp,")\n");
       return 1;
      }
/*		Change index values interactively.			*/

    int change_indices(int Ia,int Ja,int Ka,int La,int Ma,int Na,
					char *psf,char *rasf,char *netcdff,
					char *hdff, char *drsf,char *aname)
      {
	if (check_index(Ia,Ja,Ka,La,Ma,Na) != 0)
	  {
	   update_ind=1;
	   vcs_legacy_canvas_update(0);
	   if (fpout != NULL && !Inactive) prtInd(fpout);
	   if (psf[0] != '\0')
	     {
	      cgmeta(psf,1);
	      if (fpout != NULL && !Inactive)
				 fprintf(fpout,"cgm(%s)\n",psf);
	     }
	   if (rasf[0] != '\0')
	     {
	      raster_dump(rasf,1);
	      if (fpout != NULL && !Inactive)
				 fprintf(fpout,"raster(%s)\n",rasf);
	     }
           if (netcdff[0] != '\0')
             {
              storenetCDF(aname,netcdff,1);
              if (fpout != NULL && !Inactive)
                         fprintf(fpout,"netCDF(%s,%s)\n",aname,netcdff);
             }
           if (hdff[0] != '\0')
             {
#ifdef HDF
              storeHDF(aname,hdff,1);
              if (fpout != NULL && !Inactive)
                         fprintf(fpout,"HDF(%s,%s)\n",aname,hdff);
#endif
             }
	   if (drsf[0] != '\0')
	     {
#ifdef DRS
	      storeDRS(aname,drsf,1);
	      if (fpout != NULL && !Inactive)
			 fprintf(fpout,"DRS(%s,%s)\n",aname,drsf);
#endif
	     }
	  }
        return 1;
      }
