/* #ifdef PYTHON */
/* #include "Python.h" */
/* #endif */

#include <stdio.h>
#include <stdarg.h>
#ifdef USEX11
#include <X11/Xlib.h>
#endif

/*     extern Display *display;                /\* display reference *\/ */
    extern int Inactive;
    extern int Batch_mode;


/*		Display error and warning messages in the error file
		and on the text screen.					*/

    int err_warn (int beep,FILE *fp,char *fmt,...)
      {
	int i;
	char s[277];
	va_list ap;
	char *p, *sval;
	int ival;
	double dval;

	for (i=0;i<257;i++) s[i]='\0';
	va_start(ap,fmt);
	for (i=0,p=fmt; *p != '\0' && i < 256;p++)
	  {
	   if (*p == '%')
	     {
	      switch (*++p)
	        {
		 case 's':
		   if ( (sval=va_arg(ap,char *)) == NULL)
		     {
		      strcpy(&s[i],"NULL");
		      i+=4;
		     }
		   else
		      for(;i<256 && *sval!='\0';sval++) s[i++]=*sval;
		   break;
		 case 'c':
		   s[i++]=va_arg(ap,int);
		   break;
		 case 'd':
		   ival=va_arg(ap,int);
		   sprintf(&s[i],"%d",ival);
		   while (s[++i]!='\0');
		   break;
		 case 'g':
		   dval=va_arg(ap,double);
		   sprintf(&s[i],"%g",dval);
		   while (s[++i]!='\0');
		   break;
		 default:
		   s[i++]=*p;
		   break;
		}
	     }
	   else s[i++]=*p;
	  }
	if (fp != NULL) fprintf (fp,"%s\n",s);
	va_end(ap);
#ifdef PYTHON
           /* Redirect stderr for Python */
/*          PySys_WriteStderr( s ); */
/*           if (beep) XBell (display, 100); */
#endif
        return 1;
      }
