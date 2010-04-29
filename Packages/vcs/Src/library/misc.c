#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <sys/param.h>
#include "picture.h"

#define STRMAX 256
#define MAXPATHLN    1024

#define apps    1024  	/* Set the maximum number of array values 	*/

    struct array_points_struct /* Contain the pointers of floats	*/
      {
       int size;        /* Maximum size of the array.   		*/
       int actual_size; /* Actual size of the array.    		*/
       float *pa;       /* Array of points.             		*/
      } array_points_struct;

    extern FILE *fpin, *fpout, *fperr;/* input, output, and error for scripts */


    int get_more_array_memory( struct array_points_struct *aptrs )
    {
        float *new_pa;

        aptrs->size *= 2;
        if ((new_pa = (float *) malloc( aptrs->size * sizeof(float))) == NULL) {
             err_warn(1,fperr, "Error - table entry memory for points not found.\n");
              return 0;
        }
        memcpy( new_pa, aptrs->pa, (aptrs->actual_size*sizeof(float)) );
        free((char *) aptrs->pa);
        aptrs->pa = new_pa;

        return 1;
    }

    int get_points(struct points_struct **rpts, int *stop)
    {
        int c,tokm, ltpn_ct;
        struct points_struct *pts=NULL;
        struct array_segments *hptr, *lptr, *tptr;
        char strm[STRMAX+1];
        struct points_struct ptrs;
        struct array_points_struct aptrs;

        /* Set up the pointer struct that will point the list of segments */
        if ((pts = (struct points_struct *) malloc( sizeof(struct points_struct))) == NULL) {
             err_warn(1,fperr, "Error - table entry memory for points not found.\n");
              return 0;
        }
        pts->nsegs = 0;
        pts->ps = hptr = NULL;

        /* Set up the array pointer */
        aptrs.size = apps;
        aptrs.actual_size = 0;
        if ((aptrs.pa = (float *) malloc( aptrs.size * sizeof(float))) == NULL) {
             err_warn(1,fperr, "Error - table entry memory for points not found.\n");
             return 0;
        }

	/* Read all the values */
        ltpn_ct = 1;
        while (ltpn_ct != 0) {
            c=getsttk(strm,&tokm);
            if (tokm == 40) {    		/* look for the open paren */
               ++ltpn_ct;
               ++pts->nsegs;
               if ((tptr=(struct array_segments *) malloc(sizeof(struct array_segments))) == NULL) {
                   err_warn(1,fperr, "Error - table entry memory for points not found.\n");
                   return 0;
               }
               tptr->npts = 0; tptr->pts = NULL; tptr->next = NULL;
               if (hptr == NULL) 
                  pts->ps = hptr = lptr = tptr;
               else {
                  lptr->next = tptr;
                  lptr = tptr;
               }
            } else if (tokm == 41) { 		/* look for the closed paren */
               --ltpn_ct;
               if (c != 0) {			/* get the end number */
                  if (aptrs.actual_size == aptrs.size)
                      if (get_more_array_memory( &aptrs ) == 0) return 0;
                  sscanf( strm, "%f", &aptrs.pa[aptrs.actual_size] );
                  ++aptrs.actual_size;

                  /* copy array to pointer array */
                  if ((lptr->pts = (float *) malloc (aptrs.actual_size * sizeof(float))) == NULL) {
                      err_warn(1,fperr, "Error - table entry memory for points not found.\n");
                      return 0;
                  }
                  memcpy( lptr->pts, aptrs.pa, (aptrs.actual_size*sizeof(float)) );
                  lptr->npts = aptrs.actual_size;

                  aptrs.actual_size = 0;
               }
            } else if (c != 0) {     		/* get the number */
               if (aptrs.actual_size == aptrs.size)
                   if (get_more_array_memory( &aptrs ) == 0) return 0;

               sscanf( strm, "%f", &aptrs.pa[aptrs.actual_size] );
               ++aptrs.actual_size;
            }
        }

        if (aptrs.pa != NULL)
	   free((char *) aptrs.pa);

        *rpts = pts; /* return the struct */

        c=getsttk(strm,&tokm); /* get the next character for the calling routine */
        if (tokm == 41) *stop = 1;

        return 1;
    }

    int print_points(FILE *fp, struct array_segments *aptr)
    {
        int i;

        while (aptr != NULL) {
           fprintf (fp,"(");
           for (i=0; i<(aptr->npts-1); i++) {
               fprintf (fp,"%g,", aptr->pts[i]);
           }
           fprintf (fp,"%g)", aptr->pts[i]);
           aptr = aptr->next;
           if (aptr != NULL)
              fprintf (fp,",\n     ");
           else
              fprintf (fp,")");
        }

        return 1;
    }

    void free_points( struct points_struct **rpts )
    {
        int 			i;
        struct points_struct    *kpts=NULL;
        struct array_segments	*pts, *tpts;

        if (*rpts != NULL)  { /* Free existing structure */
           kpts = *rpts;
           pts = kpts->ps;
           for (i=0; i<kpts->nsegs; i++) {
              tpts = pts;
              pts = pts->next;
              free((char *) tpts->pts); tpts->pts = NULL;
              free((char *) tpts); tpts = NULL;
              if (i == 0)
                 kpts->ps = NULL;
           }
           free((char *) kpts); *rpts = NULL;
        }
    }

    int copy_points( struct points_struct **rpts, struct points_struct *cpts )
    {
        int                     i,j;
        struct points_struct    *kpts=NULL;
        struct array_segments   *ptr, *tpts, *ctpts;

        if (cpts == NULL) {
           *rpts = NULL;
           return 1;
        }

        if ((kpts =
           (struct points_struct *) malloc( sizeof(struct points_struct))) == NULL) {
	   err_warn(1,fperr,"Error - table entry memory for points not found.\n");
           return 0;
        }
        kpts->nsegs = cpts->nsegs;
        kpts->ps = NULL;

       ctpts = cpts->ps;

       for (i=0; i< cpts->nsegs; i++) {
          if ((ptr = (struct array_segments *) malloc(sizeof(
           struct array_segments))) == NULL) {
	   err_warn(1,fperr,"Error - table entry memory for points not found.\n");
           return 0;
          }

          ptr->npts = ctpts->npts;
          if ((ptr->pts = (float *) malloc( ptr->npts * sizeof(float))) == NULL) {
	    err_warn(1,fperr,"Error - table entry memory for points not found.\n");
            return 0;
          }
          for (j = 0 ; j < ptr->npts; j++)
            ptr->pts[j] = ctpts->pts[j];
          ptr->next = NULL;

          if (kpts->ps == NULL)
            tpts = kpts->ps = ptr;
          else {
            tpts->next = ptr;
            tpts = ptr;
          } 

          ctpts = ctpts->next;
       } 

       *rpts = kpts;
       return 1;
    }

    int compare_points( struct points_struct *rpts, struct points_struct *cpts )
    {
        int                     i,j;
        struct array_segments   *rtpts, *ctpts;

        if (cpts == NULL){
	  if (rpts == NULL) {
           return 1;
	  }
	  else {
	    return 0;
	  }
        }
        if (rpts == NULL){
	    return 0;
        }

/* 	printf("cpts->nsegs!=rpts->nsegs %d,%d\n",cpts->nsegs!=rpts->nsegs); */
	if (cpts->nsegs!=rpts->nsegs) return 0;

	ctpts = cpts->ps;
	rtpts = rpts->ps;

       for (i=0; i< cpts->nsegs; i++) {

/* 	printf("ctpts->n!=rtpts->n %d,%d\n",ctpts->npts!=rtpts->npts); */
	 if (ctpts->npts!=rtpts->npts) return 0;

          for (j = 0 ; j < ctpts->npts; j++)
	    {
/* 	      printf("points i,a,b: %d,%f,%f\n",j,rtpts->pts[j],ctpts->pts[j]); */
	      if (rtpts->pts[j] != ctpts->pts[j]) return 0;
	    }
          ctpts = ctpts->next;
          rtpts = rtpts->next;
       } 

       return 1;
    }


    int get_strings(struct strings_struct **rpts, int *stop)
    {
        int c,tokm, ltpn_ct;
        struct strings_struct *pts=NULL;
        struct char_segments *hptr, *lptr, *tptr;
        char strm[STRMAX+1];
        struct strings_struct ptrs;

        /* Set up the pointer struct that will point the list of segments */
        if ((pts = (struct strings_struct *) malloc( sizeof(struct strings_struct))) == NULL) {
             err_warn(1,fperr, "Error - table entry memory for strings not found.\n");
              return 0;
        }
        pts->nsegs = 0;
        pts->ss = hptr = NULL;

        /* Read all the values */
        ltpn_ct = 1;
        while (ltpn_ct != 0) {
            c=getsttk(strm,&tokm);
            if (tokm == 40) {                   /* look for the open paren */
               ++ltpn_ct;
               ++pts->nsegs;
               if ((tptr=(struct char_segments *) malloc(sizeof(struct char_segments))) == NULL) {
                   err_warn(1,fperr, "Error - table entry memory for strings not found. \n");
                   return 0;
               }
               tptr->npts = 0; tptr->cpts = NULL; tptr->next = NULL;
               if (hptr == NULL)
                  pts->ss = hptr = lptr = tptr;
               else {
                  lptr->next = tptr;
                  lptr = tptr;
               }
            } else if (tokm == 41) {            /* look for the closed paren */
               --ltpn_ct;
               if (c != 0) {                    /* get the end number */
                   lptr->npts = c;
                   if ((lptr->cpts = (char *) malloc (c* sizeof(char)+1)) == NULL) {
                      err_warn(1,fperr, "Error - table entry memory for strings not found.\n");
                      return 0;
                  }
                  memcpy( lptr->cpts, strm, (c*sizeof(char)+1) );
               }
            }
        }

        *rpts = pts; /* return the struct */

        if (tokm == 41) *stop = 1;

        return 1;
    }

    int print_strings(FILE *fp, struct char_segments *aptr)
    {
        int i;

        while (aptr != NULL) {
           fprintf (fp,"(");
           fprintf (fp,"%s)", aptr->cpts);
           aptr = aptr->next;
           if (aptr != NULL)
              fprintf (fp,",\n     ");
           else
              fprintf (fp,")");
        }

        return 1;
    }

    void free_strings( struct strings_struct **rpts )
    {
        int                     i;
        struct strings_struct    *kpts=NULL;
        struct char_segments   *pts, *tpts;

        if (*rpts != NULL)  { /* Free existing structure */
           kpts = *rpts;
           pts = kpts->ss;
           for (i=0; i<kpts->nsegs; i++) {
              tpts = pts;
              pts = pts->next;
              free((char *) tpts->cpts); tpts->cpts = NULL;
              free((char *) tpts); tpts = NULL;
              if (i == 0)
                 kpts->ss = NULL;
           }
           free((char *) kpts); *rpts = NULL;
        }
    }

    int copy_strings( struct strings_struct **rpts, struct strings_struct *cpts )
    {
        int                     i,j;
        struct strings_struct   *kpts=NULL;
        struct char_segments   	*ptr, *tpts, *ctpts;

        if (cpts == NULL) {
           *rpts = NULL;
           return 1;
        }

        if ((kpts =
           (struct strings_struct *) malloc(sizeof(struct strings_struct))) == NULL) {
           err_warn(1,fperr,"Error - table entry memory for strings not found.\n");
           return 0;
        }
        kpts->nsegs = cpts->nsegs;
        kpts->ss = NULL;

       ctpts = cpts->ss;

       for (i=0; i< cpts->nsegs; i++) {
          if ((ptr = (struct char_segments *) malloc(sizeof(
           struct char_segments))) == NULL) {
           err_warn(1,fperr,"Error - table entry memory for strings not found.\n");
           return 0;
          }

          ptr->npts = ctpts->npts;
          if ((ptr->cpts = (char *) malloc( ptr->npts * sizeof(char)+1)) == NULL) {
            err_warn(1,fperr,"Error - table entry memory for strings not found.\n");
            return 0;
          }
          strcpy(ptr->cpts, ctpts->cpts);
          ptr->next = NULL;

          if (kpts->ss == NULL)
            tpts = kpts->ss = ptr;
          else {
            tpts->next = ptr;
            tpts = ptr;
          }

          ctpts = ctpts->next;
       }

       *rpts = kpts;
       return 1;
    }

    int compare_strings( struct strings_struct *rpts, struct strings_struct *cpts )
    {
        int                     i,j;
        struct char_segments   	*rtpts, *ctpts;

        if (cpts == NULL){
	  if (rpts == NULL) {
           return 1;
	  }
	  else {
	    return 0;
	  }
        }
        if (rpts == NULL){
	    return 0;
        }

	if (cpts->nsegs!=rpts->nsegs) return 0;
       ctpts = cpts->ss;
       rtpts = rpts->ss;


       for (i=0; i< cpts->nsegs; i++) {

          if (rtpts->npts != ctpts->npts) return 0;

/* 	  printf("i,strings: %d,%s,%s\n",i,rtpts->cpts, ctpts->cpts); */
          if (strcmp(rtpts->cpts, ctpts->cpts)!=0) return 0;

          ctpts = ctpts->next;
          rtpts = rtpts->next;
       }

       return 1;
    }


    int get_int_size_and_set_value(int **pts, int npts, int value)
    {
        if (*pts != NULL) free((char *) *pts);
        if ((*pts=(int *)malloc(npts*sizeof(int)))==NULL)
          {
           err_warn(1,fperr,"Error - memory overflow for integer.\n");
           return 0;
          }
        *pts[0] = value;
       return 1;
    }

    int get_float_size_and_set_value(float **pts, int npts, float *value)
    {
        if (*pts != NULL) free((char *) *pts);
        if ((*pts=(float *)malloc(npts*sizeof(float)))==NULL)
          {
           err_warn(1,fperr,"Error - memory overflow for float.\n");
           return 0;
          }
        *pts[0] = *value;
       return 1;
    }

    int print_ints(FILE *fp, int *iptr, int nptr)
    {
        int i;

        for (i=0; i<nptr-1; i++)
            fprintf (fp,"%d,", iptr[i]);
        fprintf (fp,"%d),", iptr[i]);

        return 1;
    }

    int print_floats(FILE *fp, float *fptr, int nptr)
    {
        int i;

        for (i=0; i<nptr-1; i++)
            fprintf (fp,"%g,", fptr[i]);
        fprintf (fp,"%g),", fptr[i]);

        return 1;
    }

    int copy_int_array(int **ptr1, int **ptr2, int *nptr1, int nptr2, int deflt )
    {
        int nptrs;

        if (nptr2 == 0) nptrs = 1;
        else nptrs = nptr2;

        if ((*ptr1=(int *)malloc(nptrs*sizeof(int)))==NULL)
          {
           err_warn(1,fperr,"Error - memory overflow for integer.\n");
           return 0;
          }

        if (nptr2 != 0) memcpy( *ptr1, *ptr2, (nptrs*sizeof(int)) );
        else *ptr1[0] = deflt;

        *nptr1 = nptrs;

        return 1;
    }

    int copy_float_array(float **ptr1, float **ptr2, int *nptr1,int nptr2,float *deflt)
    {
        int nptrs;

        if (nptr2 == 0) nptrs = 1;
        else nptrs = nptr2;

        if ((*ptr1=(float *)malloc(nptrs*sizeof(float)))==NULL)
          {
           err_warn(1,fperr,"Error - memory overflow for float.\n");
           return 0;
          }

        if (nptr2 != 0) memcpy( *ptr1, *ptr2, (nptrs*sizeof(float)) );
        else *ptr1[0] = *deflt;

        *nptr1 = nptrs;

        return 1;
    }

/* Commented out by C.Doutriaux, didn't work if spaces before "-" */

/*     int isnum (str) */
/*       char str[]; */
/*       { */
/* 	int i,c,cm; */
/* 	cm=0; */
/* 	i=0; */
/* 	while (str[i] == ' ') i++; */
/* 	for (; isdigit(c=str[i]) || c == '.' || c == 'E' || c == 'e' || */
/* 		  (c == '+' && cm == 'e') || (c == '-' && cm == 'e') || */
/* 		  (i==0 && c == '-') || (i==0 && c== '+'); i++) */
/* 	   if ((cm=c) == 'E') cm='e'; */

/* 	if (i == 0 || cm == 0 || cm == 'e') return 0; */
/* 	while (str[i] == ' ') i++; */
/* 	if (str[i] != '\0' && str[i] != ',') return 0; */
/* 	return 1; */
/*       } */

    int isnum(char *str)
      {
	int i,j,c,cm;
	cm=0;
	i=0;
	j=0;
	while (str[i] == ' ') i++;
	for (; isdigit(c=str[i]) || c == '.' || c == 'E' || c == 'e' ||
		  (c == '+' && cm == 'e') || (c == '-' && cm == 'e') ||
		  (j==0 && c == '-') || (j==0 && c== '+'); i++,j++)
	   if ((cm=c) == 'E') cm='e';

	if (j == 0 || cm == 0 || cm == 'e') return 0;
	while (str[i] == ' ') i++;
	if (str[i] != '\0' && str[i] != ',') return 0;
	return 1;
      }

/*		Tokens are defined to be "'()*+,-:<=>"
		This check only works for the ASCII character set.	*/
    int istoken (int c)
/*	Test whether it is a token or not and return T (1) or F (0).	*/
      {
	if ((c>'!' && c<'?') && !(c>='#'&&c<='&') && !isdigit(c) && c!=';' &&
		c!='.' && c!='/') return 1;
	return 0;
      }

    int doexist (char *str1,char *str2)
/*			Check whether str1 exists within str2.
			The comparison is only on non-blank characters and
			it is case insensitive.  If str1 or str2 is empty
			or all blank return false (0).			*/
      {
	int c,d,i,j,k,n,l1,l2;
	char *pc1,*pc2,*pc,*pd;
	if ((str1==NULL) || str2==NULL) return 0;
	if ((l1=strlen(str1))==0 || (l2=strlen(str2))==0)
	  return 0;
	for (i=0,j=0,pc1=str1,pc2=str2; i<l1; i++,pc1++)
	  {
	   if( (c=tolower(*pc1)) > ' ')
	     {
	      for (;j<l2;j++,pc2++)
		{
		 if ((d=tolower(*pc2)) == (c=tolower(*pc1)))
		   {
		    for (n=j+1,pd=pc2+1,k=i+1,pc=pc1+1;k < l1;k++,pc++,n++,pd++)
		      {
		       if ( (c=tolower(*pc)) > ' ')
			 {
			  for (;n < l2 && (d=tolower(*pd)) <= ' ';n++) pd++;
			  if (c != d) break;
			 }
		      };
		    if (c == d) return 1;
		   }
		};
	      return 0;
	     }
	  };
	return 0;
      }


/*		Compare only printing characters (case sensitive)
		 in two strings.

		Return =0 - if they are identical.
		Return >0 - if string 2 is larger.
		Return <0 - if string 1 is larger.			*/

int cmpnbl(char *s1,char *s2)
      {
       for ( ;*s1 != '\0';s1++)
	 {
	  if (*s1 > ' ' && *s1 <= '~')
	    {
	     while ((*s2 <= ' ' || *s2 > '~') && *s2 != '\0') s2++;
	     if (*s1 != *s2) return *s2-*s1;
	     s2++;
            }
	 }
       while ((*s2 <= ' ' || *s2 > '~') && *s2 != '\0') s2++;
       return *s2-*s1;
      }


/*		Compare only printing characters (non-case sensitive)
		 in two strings.

		Return =0 - if they are identical.
		Return >0 - if string 2 is larger.
		Return <0 - if string 1 is larger.			*/

int cmpncs(char *s1,char *s2)
      {
       int c1,c2;

       if (s1 == NULL || s2 == NULL) return 1;

       for ( ;*s1 != '\0';s1++)
	 {
	  if (*s1 > ' ' && *s1 <= '~')
	    {
	     while ((*s2 <= ' ' || *s2 > '~') && *s2 != '\0') s2++;
	     c1=*s1;
	     c2=*s2;
	     if (tolower(c1) != tolower(c2)) return c2-c1;
	     s2++;
            }
	 }
       while ((*s2 <= ' ' || *s2 > '~') && *s2 != '\0') s2++;
       return *s2-*s1;
      }


/*		Set a '\0' at the end of the printable characters.
		ASCII character set is assumed.				*/

int trimbl (char *str,int len)
      {
	int i,j;

	i=j=0;
	while (str[i] != '\0' && i < len)
	  {
	   if (str[i] > ' ' && str[i] <= '~') j=i;
	   else str[i]=' ';
	   i++;
	  }
	if (len > j+1) str[j+1]='\0';
	return 1;
      }

/*		Replace string (s2) with string (s1) in dynamic memory
		after removing trailing blanks.  Free and/or allocate
		space if necessary.  Return s2.				*/

char *repstr (char *s2,char *s1)
      {
	int i;
	char blank[2];

	if (s1 == NULL)
	  {
	   if (s2 != NULL) free(s2);
	   s2=NULL;
	   return s2;
	  }
	strcpy(blank," ");
	i=strlen(s1);
	if (s2 != NULL) free(s2);
	if (i == 0) {s1=blank; i=1;}
	if ((s2=malloc(i+1))==NULL)
	  {
	   err_warn(1,fperr,"Error - memory overflow.\n");
	   return NULL;
	  }
	strcpy(s2,s1);
	trimbl(s2,i+1);
	return s2;
      }

/*              Replace float (f2) with float (f1) in dynamic memory.
                Free and/or allocate space if necessary.  Return f2.
                   				                         */

float *repflt (float *f2,float *f1)
      {
        if (f2 != NULL)
           free(f2);

        if (f1 == NULL)
          {
           f2=NULL;
           return f2;
          }
        if ((f2=(float *)malloc(sizeof(float)))==NULL)
          {
           err_warn(1,fperr,"Error - memory overflow for float.\n");
           return NULL;
          }

	*f2 = *f1;

        return f2;
      }

/*              Replace int (i2) with int (i1) in dynamic memory.
                Free and/or allocate space if necessary.  Return i2.
                                                                         */

int *repint (int *i2,int *i1)
      {
        if (i2 != NULL)
           free(i2);

        if (i1 == NULL)
          {
           i2=NULL;
           return i2;
          }
        if ((i2=(int *)malloc(sizeof(int)))==NULL)
          {
           err_warn(1,fperr,"Error - memory overflow for int.\n");
           return NULL;
          }

        *i2 = *i1;

        return i2;
      }

    int getsttk(char *str,int *tok)

      /*char str[STRMAX+1]; DNW - 1/25/00 * The string returns here */
/*       char *str; /\* The string returns here *\/ */
/*       int *tok;		/\* The token returns here *\/ */

/*	This function supplies a string and token and returns string length.
	The string will not contain more than one blank, which will either
	follow any other non-printing characters or be the solo character.
	Control characters (those below blank in the ASCII order) are discarded
        by GETP, i.e. carriage return (CR), line feed (LF).  Only one of a
	sequence of multiple blanks is returned, the others are discarded.
	Some examples:

		Script excerpt within []	string		token	return
		-------------------------------	-------		-------	-------
		[ssssss       =]		[ssssss ]	[=]	6
		[sssCR,]			[sss]		[,]	3
		[sss   sss]			[sss]		[NULL]	3
		    (NOTE: the next getsttk will begin at the second string)
 
	When an end-of-file is reached, either the token will contain the
	EOF (when a string precedes the EOF), or EOF will be the return (when
	no string is available).  The following are examples:

		Script excerpt within []	string		token	return
		-------------------------------	-------		-------	-------
		[ssssss)EOF]			[ssssss]	[)]	6
				(and then)	[]		[EOF]	EOF

		[sssss EOF]			[sssss ]	[EOF]	6
				(and then)	[]		[EOF]	EOF

	Strings longer than STRMAX are truncated.	

	Tokens are defined to be ""'()*+,-:<=> EOF .

	When a plus or a minus (+ or -) precedes a string it is not considered
	to be a token, and is included in the string.  For instance:

		Script excerpt within []	string		token	return
		-------------------------------	-------		-------	-------
		[ssssss=-sss,+sss)]		[ssssss]	[=]	6
				(and then)	[-sss]		[,]	4
				(and then)	[+sss]		[)]	3

									*/

      {
	int i,c;

	i=0; str[0]='\0'; *tok=0;

	while ( !istoken(c=getp(fpin,fpout)) || (i==0 && (c=='-' || c=='+')))
	  {
	   if (c == EOF)
	     {*tok=EOF;
	      str[i]='\0';
	      if (i>0) return i;
	      else return EOF;
	     }
	   if (c != ' ' && i < STRMAX) str[i++]=c;
	  };
	str[i]='\0';
	*tok=c;
	return i;
      }

/*		Count printing characters in a string.			*/

    int strprt(char *str)
      {
       int n;

       n=0;
       while (*str != '\0')
	 {
	  if (*str > ' ' && *str <= '~') n++;
	  str++;
	 }
       return n;
      }

/*		Find a "nice" interval to span a range.			*/
/*		With 8 to 15 intervals.					*/

    int nicedf(float a,float b,float *dr,int *pw10,float *center)
      {
	double dc,dl;
	double df,di;
	double amx,amn;
	int i,nd,n[7],dd;
	float x[7];

	x[0]=1.;
	x[1]=2.;
	x[2]=3.;
	x[3]=5.;
	x[4]=6.;
	x[5]=7.;
	x[6]=10.;

	*dr=0.0;
	*pw10=0;
	*center=0.0;
	amx=(a>b)?a:b;
	amn=(a<b)?a:b;
	dc=amx-amn;
	if (dc < 1.e-20 || amx >= 1.e20 || amn >=1.e20) /* DNW - 10/18/00 changed */
	  {                                             /* from "dc < 1.e-10" */
	   *dr=0.0;
	   *pw10=0;
	   return 0;
	  }
	dl=log10(dc);
	if (dl > 1.0) nd=-dl+1.0;
	else	      nd=-dl+1.99990;
	*pw10=-nd;
        df=dc*pow(10.0,(double) nd);
	di=(int)(df+.9999);
	for (i=0;i<7;i++)
	      n[i]=di/x[i];
	if ((float)df == (float)di)
	  {
	   for (i=0;i<7;i++)
	     {
	      if ( (n[i]*x[i]-di) == 0.0 && n[i] >=5 && n[i] < 16)
	        {
	         *dr=x[i];
		 df=amn*pow(10.0,(double)nd);
	         dd=(df-(int)df)/0.000000000001;
		 if (dd == 0) *center=(float)amn;
	         return 1;
	        }
	     }
	  }
	*pw10=-nd;
	if (n[6] >= 5) 	    *dr=x[6];
	if (n[5] >= 5)      *dr=x[5];
	if (n[4] >= 5)      *dr=x[4];
	else if (n[3] >= 5) *dr=x[3];
	else if (n[2] < 16) *dr=x[2];
	else if (n[1] < 16) *dr=x[1];
	else if (n[0] < 16) *dr=x[0];
	else		    *dr=x[3];
	return 1;
      }

/*		Find a "nice" interval to span a range.			*/
/*		With 8 to 15 intervals.					*/

    int nice15(float a,float b,float *dr,int *pw10,float *center)
      {
	double dc,dl,del;
	double df,di;
	double amx,amn;
	int i,nd,n,nmax,k1,k2;
/*	float x[7];
*/

        *dr=0.0;
        *pw10=0;
	*center=0.0;
	amx=(a>b)?a:b;
	amn=(a<b)?a:b;
	dc=amx-amn;
	if (dc < 1.e-20 || amx >= 1.e20 || amn >=1.e20) /* DNW - 10/18/00 changed */
	  {                                             /* from "dc < 1.e-10" */
	   *dr=0.0;
	   *pw10=0;
	   return 0;
	  }
	dl=log10(dc);
	if (dl > 1.0) nd=-dl+1.0;
	else	      nd=-dl+1.99990;
	*pw10=-nd;
	df=dc*pow(10.0,(double) nd);
	nmax=-10;
	for (i=0;i<10;i++)
	  {
	   n=(df+i+0.99999)/(i+1);
	   if (n <= 15)
	     {
	      del=(i+1)*pow(10.0,(double) -nd);
	      k1=amn/del;
	      if (k1*del > amn) k1--;
	      k2=amx/del;
	      if (k2*del < amx) k2++;
	      if (k2-k1+1 <= 15) break;
	     }
	  }
	if (i == 10) return 0;

	*dr=i+1;
	
/*	for (i=0;i<7;i++)
	  {
	      n[i]=di/x[i];
	  }
	if ((float)df == (float)di)
	  {
	   for (i=0;i<7;i++)
	     {
	      if ( (n[i]*x[i]-di) == 0.0 && n[i] < 16)
	        {
	         *dr=x[i];
*/
/*		 df=amn*pow(10.0,(double)nd);
		 if ( ((int) df) == df)  *center=amn;		*/
/*
	         return 1;
	        }
	     }
	  }
	*pw10=-nd;
	if (n[6] >= 10)	    *dr=x[6];
	if (n[5] >= 10)     *dr=x[5];
	if (n[4] >= 10)     *dr=x[4];
	else if (n[3] >= 10)*dr=x[3];
	else if (n[2] < 16) *dr=x[2];
	else if (n[1] < 16) *dr=x[1];
	else if (n[0] < 16) *dr=x[0];
	else		    *dr=x[3];
*/
	return 1;
      }

    int findnam (str1,str2)
/*			Check whether str1 exists within str2.  Preceeding
			and following character must be non-printing or token.
			If str1 or str2 is empty or str1 contains non-printing
			characters or tokens return false (0).		*/
      char *str1,*str2;
      {
	int i,j,l1,l2;
	char s1[18],s2[1024];
	char *pc1;

	if ((l1=strlen(str1))==0 || (l2=strlen(str2))==0 || l1>17||l2>1024 ||
		l1 > l2)
	return 0;
/*			Remove blank and non-print characters from (str1) 
			and (str2) must not contain a token.	*/
	for (i=j=0;i<18 && str1[i] != '\0';i++)
	  {
	   if (str1[i] > ' ') s1[j++]=str1[i];
	   if (istoken(str1[i])) return 0;
	  }
	s1[j]='\0';
	l1=j;
	if (j <= 0) return 0;

	for (i=j=0;i<1024 && str2[i] != '\0';i++)
	  {
	   if (str2[i] > ' ') s2[j++]=str2[i];
	  }
	s2[j]='\0';
	l2=j;
	if (j < l1) return 0;


	for (j=0; j <= l2-l1; j++)
	  {
	   if (s1[0] == s2[j])
	     {
	      if (j == 0 || istoken(s2[j-1]) )
		{
	         for (i=0;i<l1;i++)
		   {
		    if (s1[i] != s2[i+j]) break;
		   }
/*			If it agrees and it's a name return true.	*/

		 if (i==l1 && (i+j==l2 || istoken(s2[i+j]))) return 1;
		}
	     }
	  }
	return 0;
      }

/*		Check for an extension and if it doesn't exist (as: .ext) then
		add it.							*/

    int xtend (char *str,char *xten)
      {
       int k,m;

       k=strlen(str);
       m=strlen(xten);
       if (k==0 || m==0) return 0;
       if (k <= m || strcmp(xten,&str[k-m]) != 0) strcat(str,xten);
       return 1;
      }

/*		Get a path name.					*/

#include <unistd.h>
    int get_a_path (char *p)
      {
	int i,j,k;

	p = (char *) getcwd((char *)p, MAXPATHLN);
	k=strlen(p);
	if (k > 8 && strncmp(p,"/tmp_mnt",8) == 0)
	  {
	   for (j=0,i=8;i < k;i++,j++) p[j]=p[i];
	   p[j]='\0';
	  }
	return 1;
      }

#include "gks.h"
#include "gksshort.h"
/*		Set segment priority.					*/

    int gsetsegpri (int segno,double prty)
      {
	Gsegattr seg_attr=
	   {0, 1.0,0.0,0.0, 0.0,1.0,0.0, GVISIBLE,GNORMAL,0.0,GUNDETECTABLE};

	seg_attr.seg=segno;

/*	gqsga(seg_attr);		*/
	seg_attr.pri=prty;
	gssga(segno,&seg_attr);

	return 1;
      }

/************************************************************************
 *  End of File                                                         *
 ************************************************************************/
