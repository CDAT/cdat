#include <stdarg.h>
#include <stdio.h>

/*		Get a path name error and warning messages in the error file
		and on the text screen.					*/

    int get_a_path (char *p)
      {
	int i,j,k;

#ifdef hpux
	getcwd(p);
#else
	getwd(p);
#endif
	k=strlen(p);
	if (k > 8 && strncmp(p,"/tmp_mnt",8) == 0)
	  {
	   for (j=0,i=8;i < k;i++,j++) p[j]=p[i];
	   p[j]='\0';
	  }
	return 1;
      }
