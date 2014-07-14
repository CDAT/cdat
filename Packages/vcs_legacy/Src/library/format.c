#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "picture.h"
#include "cddrs.h"

    extern FILE *fpin, *fpout, *fperr;	/* i/o, and error files for scripts */

    extern struct table_form Th_tab;
    char blnk[2]={" "};

/*    struct form
      {
       char s_name[17];
       char s_units[41];
       char format[121];
       struct form *next;
      };
    struct formit
      {
       char name[17];
       struct form *variety;
       struct table_form *next;
      };

    struct form Fstd9={"mean","*","%n %g",NULL};
    struct form Fstd8={"min","*","%n %g",&Fstd9};
    struct form Fstd7={"max","*","%n %g",&Fstd8};
    struct form Fstd6={"time","month","%t[Mon/c+1979]",&Fstd7};
    struct form Fstd5={"time","hour","%t[h d/m/c+1979]",&Fstd6};
    struct form Fstd4={"time","season","%t[Sea/c+1979]",&Fstd5};
    struct form Fstd3={"time","S","%t[Season/c+1979]",&Fstd4};
    struct form Fstd2={"time","day","%t[Month/c+1979]",&Fstd3};
    struct form Fstd1={"month","*","%t[Month/c+1979]",&Fstd2};
    struct form Fstd={"time","second","%n %t[h:m:s]",&Fstd1};
    struct table_form Th_tab={"default",&Fstd,NULL};				
							*/

/*		Format data into a string.  The format should contain
		%n, %u, and %g or %t followed a set of parenthesized
		units definitions, as [h:m:s] or [d/M/y] or
		perhaps [d/M/c+1979].

		Where:	1. s - second
			2. m - minute
			3. h - hour
			4. day - day
			5. M - month number
			   mon - three character month designator (no capital)
			   Mon - three character month designator (1st capital)
			   MON - three character month designator (all capital)
			   month - full month name (no capital)
			   Month - full month name (1st capital)
			   MONTH - full month name (all capital)
			6. S - three character season designator
			       (i.e. 1 - DJF, 2 - MAM, 3 - JJA, 4 - SON)
			   Sea - three character/month season designator
			         (i.e. Dec-Jan-Feb)
			   SEA - three character season designator in caps
			         (i.e. DEC-JAN-FEB)
			   season - three month of the season in full
				    (i.e. december-january-february)
			   Season - three months of the season capitalized in
				    full (i.e. December-January-February)
			   SEASON - three month of the season in full caps
				    (i.e. DECEMBER-JANUARY-FEBRUARY)
			7. y - year last two digits (i.e. no century)
			   c - year all four digits

		These format designators will be used only when s_name
		and s_units (below) are "time" and "month" or "hour" etc..
									*/


/*		Format a string for display using any of a variety of
		formats depending upon the name and units of the
		variable.						*/

    int format (s_name,s_units,r,fmt_set,str)
      char *s_name,*s_units;	/* Input name and units strings.	*/
      char str[257];		/* Output string (should be 256 bytes).	*/
      char *fmt_set;		/* Name of the set of formats to use.	*/
      double r;			/* Real number to be formatted.		*/

      {
       int i,j,k,K,m,n;
       struct table_form *now;
       struct form *f;
       char *fs;
       char fg[25];
       double dt[7];
       int dm[7],km[7];
       char cm[20][10];
       char trail[20][10];
       char month[12][10];
       char season[4][3][10];

	strcpy(month[0],"January");
	strcpy(month[1],"February");
	strcpy(month[2],"March");
	strcpy(month[3],"April");
	strcpy(month[4],"May");
	strcpy(month[5],"June");
	strcpy(month[6],"July");
	strcpy(month[7],"August");
	strcpy(month[8],"September");
	strcpy(month[9],"October");
	strcpy(month[10],"November");
	strcpy(month[11],"December");
	strcpy(season[0][0],"December");
	strcpy(season[0][1],"January");
	strcpy(season[0][2],"February");
	strcpy(season[1][0],"March");
	strcpy(season[1][1],"April");
	strcpy(season[1][2],"May");
	strcpy(season[2][0],"June");
	strcpy(season[2][1],"July");
	strcpy(season[2][2],"August");
	strcpy(season[3][0],"September");
	strcpy(season[3][1],"October");
	strcpy(season[3][2],"November");

       if (s_name == NULL) s_name=&blnk[0];
       if (s_units == NULL) s_units=&blnk[0];

       for (i=0;i<256;i++) str[i]='\0';
       now=&Th_tab;
       while (now != NULL)
	 {
	  if (strcmp(now->name,fmt_set) == 0) break;
	  now=now->next;
	 }
       if (now == NULL) now=&Th_tab;

       f=now->variety;
/*			Search for a format that has the correct
			name and units.					*/
       while (f != NULL)
	 {
	  if (doexist(f->s_name,s_name) && doexist(f->s_units,s_units))
		break;
	  f=f->next;
	 }
       if (f == NULL)
	 {
/*			Search for a format that has the correct name
			and a generic units specifier.			*/
	  f=now->variety;
	  while (f != NULL)
	    {
	      if (f->s_units[0] == '*' && doexist(f->s_name,s_name)) break;
	      f=f->next;
	     }
	 }
/*			If none exists use a completely generic format.	*/
       if (f == NULL)
	 {
	  sprintf(str,"%s %g %s",s_name,r,s_units);
	  return 1;
	 }
/*			Now break down the format and put the string
			together.					*/

       i=0;
       fs=f->format;
       while (i < 256 && *fs != '\0')
	 {
	  if (*fs != '%') str[i++]=*fs++;
	  else
	    {
	     fs++;
	     if (*fs == 'n')
	       {
		fs++;
		for (j=0;i<256 && j<CW_MAX_NAME && *s_name != '\0';j++)
		  str[i++]=*s_name++;
	       }
	     else if (*fs == 'u')
	       {
	        fs++;
		for (j=0;i<256 && j<40 && *s_units != '\0';j++)
		  str[i++]=*s_units++;
	       }
	     else if (*fs == 'g')
	       {
		fs++;
		if ( (n=codeg(r,fg,24)) > 0 && (n+i) < 256)
		   strncpy(&str[i],fg,n); str[256] = '\0';
		i+=n;
	       }
	     else if (*fs == 't')
	       {
		fs++;
		k=0;
		while (*fs != '[' && *fs != '\0')
		  {if (*fs == ' ') str[i++]=*fs; fs++;}
		if (*fs == '[')
		  {
		   for (j=0;j < 20;j++) 
		     for (n=0;n < 10;n++) cm[j][n]=trail[j][n]='\0';
		   fs++;
		   while (*fs != ']' && *fs != '\0')
		     {
		      if (isalpha(*fs))
			{
			 for (j=0;j<9 && isalpha(*fs);j++)
			    {cm[k][j]=*fs; fs++;}
			 while (isalpha(*fs)) fs++;
			 cm[k][j]='\0';
			 for (j=0;j<9&&!isalpha(*fs)&&*fs!=']'&&*fs!='\0';j++)
				 {trail[k][j]=*fs; fs++;}
			 while (!isalpha(*fs)&&*fs!=']'&&*fs!='\0') fs++;
			 trail[k][j]='\0';
			 k++;
			}
		      else fs++;
		     }
		   if (*fs == ']') fs++;
		   K=0;
		   if      (doexist("sec",s_units)) K=1;
		   else if (doexist("min",s_units)) K=2;
		   else if (doexist("hour",s_units)) K=3;
		   else if (doexist("day",s_units)) K=4;
		   else if (doexist("mon",s_units)) K=5;
		   else if (doexist("seas",s_units)) K=6;
		   else if (doexist("year",s_units)) K=7;

		   if (k > 0 && K > 0)
		     {
		      for (j=0;j<7;j++) {km[j]=0; dm[j]=0;}

		      for (j=0;j<k;j++)
			{
			 if (cm[j][0] == 's')
			   {
			    if (cm[j][1] == '\0') {dm[j]=1; km[0]=1;}
			    else {dm[j]=6; km[5]=1;}
			   }
			 else if (cm[j][0] == 'S') {dm[j]=6; km[5]=1;}
			 else if (cm[j][0] == 'm')
			   {
			    if (cm[j][1] == '\0') {dm[j]=2; km[1]=1;}
			    else {dm[j]=5; km[4]=1;}
			   }
			 else if (cm[j][0] == 'M') {dm[j]=5; km[4]=1;}
			 else if (cm[j][0] == 'h') {dm[j]=3; km[2]=1;}
			 else if (cm[j][0] == 'd') {dm[j]=4; km[3]=1;}
			 else if (cm[j][0] == 'y') {dm[j]=7; km[6]=1;}
			 else if (cm[j][0] == 'c') {dm[j]=7; km[6]=1;}
			}
/*		      if (time_convert(r,K,dm,km,cm,trail,dt))		*/
		      if (time_convert(r,K,dm,km,trail,dt))
			{
			 for (j=0;j<k && dm[j]>0;j++)
			   {
			    if (dm[j] <= 4)
			      {
			       if ((n=codeg((float)dt[dm[j]-1],fg,24))>0 &&
								 (n+i)<256)
				 {strcpy(&str[i],fg); i+=(n-1);}
			       if ((n=strlen(trail[j])) < 256-i && n > 0)
				 {strcpy(&str[i],trail[j]); i+=n;}
			      }
			    else if (dm[j] == 5)
			      {
			       if (strcmp(cm[j],"M") == 0)
				 {
			          if ((n=codeg((float)dt[dm[j]-1],fg,24))>0
					&&(n+i)<256)
				    {strcpy(&str[i],fg); i+=(n-1);}
				 }
			       else if (strcmp(cm[j],"mon") == 0)
				 {
				  if (i+3 < 256)
				     for(m=dt[dm[j]-1],n=0;n<3;n++)
					 str[i++]=tolower(month[m-1][n]);
				   str[i]='\0';
				 }
			       else if (strcmp(cm[j],"month") == 0)
				 {
				  if (i+9 < 256)
				     for(m=dt[dm[j]-1],n=0;
					 (str[i]=tolower(month[m-1][n]))!='\0';
						i++,n++);
				 }
			       else if (strcmp(cm[j],"Mon") == 0)
				 {
				  if (i+3 < 256)
				     for(m=dt[dm[j]-1],n=0;n<3;n++)
					 str[i++]=month[m-1][n];
				   str[i]='\0';
				 }
			       else if (strcmp(cm[j],"Month") == 0)
				 {
				  if (i+9 < 256)
				     for(m=dt[dm[j]-1],n=0;
					 (str[i]=month[m-1][n]) != '\0';
						i++,n++);
				 }
			       else if (strcmp(cm[j],"MON") == 0)
				 {
				  if (i+3 < 256)
				     for(m=dt[dm[j]-1],n=0;n<3;n++)
					 str[i++]=toupper(month[m-1][n]);
				   str[i]='\0';
				 }
			       else if (strcmp(cm[j],"MONTH") == 0)
				 {
				  if (i+9 < 256)
				     for(m=dt[dm[j]-1],n=0;
					 (str[i]=toupper(month[m-1][n]))!='\0';
						i++,n++);
				 }
			       if ((n=strlen(trail[j])) < 256-i && n > 0)
				  {strcpy(&str[i],trail[j]); i+=n; str[i]='\0';}
			      }
			    else if (dm[j] == 6)
			      {
			       if (strcmp(cm[j],"S") == 0)
				 {
				  if (i+3 < 256)
				    {
				     m=dt[dm[j]-1];
				     str[i++]=tolower(season[m-1][0][0]);
				     str[i++]=tolower(season[m-1][1][0]);
				     str[i++]=tolower(season[m-1][2][0]);
				     str[i]='\0';
				    }
				 }
			       else if (strcmp(cm[j],"season") == 0)
				 {
				  if (i+29 < 256)
				    {
				     m=dt[dm[j]-1];
				     for(n=0;
				      (str[i]=tolower(season[m-1][0][n]))!='\0';
						i++,n++);
				     str[i++]='-';
				     for(n=0;
				      (str[i]=tolower(season[m-1][1][n]))!='\0';
						i++,n++);
				     str[i++]='-';
				     for(n=0;
				      (str[i]=tolower(season[m-1][2][n]))!='\0';
						i++,n++);
				    }
				 }
			       else if (strcmp(cm[j],"Season") == 0)
				 {
				  if (i+29 < 256)
				    {
				     m=dt[dm[j]-1];
				     for(n=0;
				       (str[i]=season[m-1][0][n])!='\0';
						i++,n++);
				     str[i++]='-';
				     for(n=0;
				       (str[i]=season[m-1][1][n])!='\0';
						i++,n++);
				     str[i++]='-';
				     for(n=0;
				       (str[i]=season[m-1][2][n])!='\0';
						i++,n++);
				    }
				 }
			       else if (strcmp(cm[j],"SEASON") == 0)
				 {
				  if (i+29 < 256)
				    {
				     m=dt[dm[j]-1];
				     for(n=0;
				      (str[i]=toupper(season[m-1][0][n]))!='\0';
						i++,n++);
				     str[i++]='-';
				     for(n=0;
				      (str[i]=toupper(season[m-1][1][n]))!='\0';
						i++,n++);
				     str[i++]='-';
				     for(n=0;
				      (str[i]=toupper(season[m-1][2][n]))!='\0';
						i++,n++);
				    }
				 }
			       else if (strcmp(cm[j],"SEA") == 0)
				 {
				  if (i+11 < 256)
				    {
				     m=dt[dm[j]-1];
				     for(n=0;n<3;n++)
					 str[i++]=toupper(season[m-1][0][n]);
				     str[i++]='-';
				     for(n=0;n<3;n++)
					 str[i++]=toupper(season[m-1][1][n]);
				     str[i++]='-';
				     for(n=0;n<3;n++)
					 str[i++]=toupper(season[m-1][2][n]);
				     str[i]='\0';
				    }
				 }
			       else if (strcmp(cm[j],"Sea") == 0)
				 {
				  if (i+11 < 256)
				    {
				     m=dt[dm[j]-1];
				     for(n=0;n<3;n++)
					 str[i++]=season[m-1][0][n];
				     str[i++]='-';
				     for(n=0;n<3;n++)
					 str[i++]=season[m-1][1][n];
				     str[i++]='-';
				     for(n=0;n<3;n++)
					 str[i++]=season[m-1][2][n];
				     str[i]='\0';
				    }
				 }
			       if ((n=strlen(trail[j])) < 256-i && n > 0)
				  {strcpy(&str[i],trail[j]); i+=n; str[i]='\0';}
			      }
			    else if (dm[j] == 7)
			      {
			       if (tolower(cm[j][0]) == 'y')
				 {
			          m=dt[dm[j]-1];  m=m%100;
				  if (i+2 < 256)
				    {sprintf(&str[i],"%2d",m); i+=2;}
				 }
			       else if (tolower(cm[j][0]) == 'c')
				 {
			          m=dt[dm[j]-1]; 
				  if (i+4 < 256)
				    {sprintf(&str[i],"%4d",m); i+=4;}
				 }
			       if ((n=strlen(trail[j])) < 256-i && n > 0)
				  {strcpy(&str[i],trail[j]); i+=n;}
			      }
			   }




			}
		      else
			{
		         if ( (n=codeg(r,fg,24)) > 0 && (n+i) < 256)
		            strncpy(&str[i],fg,n); str[256] = '\0';
		         i+=n;
			 break;
			}
		     }
		   else
		     {
		      if ( (n=codeg(r,fg,24)) > 0 && (n+i) < 256)
		         strncpy(&str[i],fg,n); str[256] = '\0';
		      i+=n;
		      break;
		     }
		  }
		else
		  {
		   if ( (n=codeg(r,fg,24)) > 0 && (n+i) < 256)
		      strncpy(&str[i],fg,n); str[256] = '\0';
		   i+=n;
		   break;
		  }
	       }
	    }
	 }
       return 1;
      }

/* 		Convert time to time date as requested in a format.	*/

/*    int time_convert(r,K,dm,km,cm,tr,dt)				*/
      int time_convert(r,K,dm,km,tr,dt)
      double r;
      int K;
      int dm[7];
      int km[7];
/*      char cm[7][10];							*/
      char tr[7][10];
      double dt[7];
      {
       int i,j,k,N;


/*			Add in increments from the trailing string.	*/

       N=20;
       for (i=0; i < 7; i++) dt[i]=0;
       for (i=0; i < 7; i++)
	 {
	  if (tr[i][0] == '-' || tr[i][0] == '+')
	    {
	     for (j=1; j < 10 && isdigit(tr[i][j]);j++)
		dt[dm[i]-1]=10.*dt[dm[i]-1]+(tr[i][j]-'0');
	     if (tr[i][0] == '-') dt[dm[i]-1]=-dt[dm[i]-1];
	     for (k=0;j<10;k++,j++) tr[i][k]=tr[i][j];
	     tr[i][k]='\0';
	    }
	  if (dm[i] > 0) N=(dm[i]<N)?dm[i]:N;
	 }


/*			Put the value in, and cascade upward	*/

       dt[K-1]=dt[K-1]+r;

       k=dt[0]/60.0;
       dt[0]=dt[0]-k*60;
       dt[1]=dt[1]+k;
       k=dt[1]/60.0;
       dt[1]=dt[1]-k*60;
       dt[2]=dt[2]+k;
       k=dt[2]/24.0;
       dt[2]=dt[2]-k*24;
       dt[3]=dt[3]+k;

       if (N == 4)
	 {
	  dt[3]=dt[3]+dt[0]/21600.+dt[1]/1440.+dt[2]/24.;
	  dt[0]=dt[1]=dt[2]=0.0;
	 }
       else if (N == 3)
         {
	  dt[2]=dt[2]+dt[0]/3600.+dt[1]/60.;
	  dt[0]=dt[1]=0.0;
	 }
       else if (N == 2)
	 {
	  dt[1]=dt[1]+dt[0]/60.;
	  dt[0]=0.0;
	 }
       if (km[4] != 0)
	    calendar(&dt[3],&dt[4],&dt[6]);	/* adjust day,mon,yr	*/
       if (km[5] != 0)
	 {
	  if (K != 6) return 0;
	  dt[6]=dt[6]+(dt[5]-1)/4;
	  k=dt[5];
	  dt[5]=(k-1)%4+1;
	 }
       return 1;
      }

/*		Adjust calendar day, month, year.			*/

    int calendar(day,mon,yr)
      double *day,*mon,*yr;
      {
       int m,y,d;
       double fd,fm,fy;

       int month[12];

	month[0]=31;
	month[1]=28;
	month[2]=31;
	month[3]=30;
	month[4]=31;
	month[5]=30;
	month[6]=31;
	month[7]=31;
	month[8]=30;
	month[9]=31;
	month[10]=30;
	month[11]=31;

       d=*day;
       fd=*day-d;
       m=*mon;
       fm=*mon-m;
       y=*yr;
       fy=*yr-y;
       if (m <= 0) m=1;
       y=y+(m-1)/12;
       m=(m-1)%12+1;
       month[1]=(y%4 == 0 && (y%100 != 0 || y%1000 == 0))?29:28;
       while (d > month[m-1])
	 {
	  d=d-month[m-1];
	  m+=1;
	  if (m > 12)
	    {
	     y=y+(m-1)/12;
	     m=(m-1)%12+1;
	     month[1]=(y%4 == 0 && (y%100 != 0 || y%1000 == 0))?29:28;
	    }
	 }
       *mon=m+fm;
       *day=d+fd;
       *yr=y+fy;

       return 1;
      }
