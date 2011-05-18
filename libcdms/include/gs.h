/*  Copyright (C) 1988-2010 by Brian Doty and the 
    Institute of Global Environment and Society (IGES).  
    See file COPYRIGHT for more information.   */

#define RSIZ 3600  /* increased from 600 in version 1.9 */

/* One of these gets allocated for each record in the file */

/* Record types:
                  1 - statement
                  2 - assignment
                  3 - while
                  4 - endwhile
                  5 - continue
                  6 - break
                  7 - if
                  8 - else
                  9 - endif
                  10 - return
                  11 - function  */

struct gsrecd {
  struct gsrecd *forw;  /* Link list pointer */
  struct gsrecd *refer; /* Position of end of code block */
  struct gsfdef *pfdf;  /* Pointer to file def for this record */
  char *pos;            /* Start of record */
  char *epos;           /* Position of start of expression, if any */
  gaint num;              /* Record number in file */
  gaint type;             /* Record type */
};

/* Following structure describes a file that has been read in
   to become part of the running script.  */

struct gsfdef {
  struct gsfdef *forw;  /* Link list pointer */
  struct gsrecd *precd; /* Record descriptor for start of this file */
  char *name;           /* Text name of the file  */
  char *file;           /* The contents of the file */
};

/* Following structure is a member of a link list providing the
   current value of a variable.    */

struct gsvar {
  struct gsvar *forw;      /* Forward pointer             */
  char name[16];           /* Variable name               */
  char *strng;             /* Value of variable           */
};

/* Following structure is a member of a link list pointing to
   all the functions contained within a file.  */

struct gsfnc {
  struct gsfnc *forw;      /* Forward pointer */
  struct gsrecd *recd;     /* Record block for function   */
  char name[16];           /* Name of function            */
};

/* Following structure hold information on open files
   accessed via the read/write/close user callable functions */

struct gsiob {
  struct gsiob *forw;      /* Forward pointer  */
  FILE *file;              /* File pointer     */
  char *name;              /* File name        */
  gaint flag;                /* Status flag: 1-read 2-write  */
};

/* Following structure holds global pointers needed by all the
   gs routines, and anchors most global memory allocations */

struct gscmn {
  struct gsfdef *ffdef;    /* Head of input file link list */
  struct gsfdef *lfdef;    /* Last in chain of input files */
  struct gsrecd *frecd;    /* Head of record descriptor link list */   
  struct gsrecd *lrecd;    /* Last in record list list */
  struct gsvar *fvar;      /* Head of variable linklist   */
  struct gsfnc *ffnc;      /* Head of function list       */
  struct gsiob *iob;       /* Head of file I/O list       */
  struct gsvar *gvar;      /* Head of global var list     */
  struct gsvar *farg;      /* Pointer to function arglist */
  char *fname;             /* Pointer to user-entered file name   */
  char *fprefix;           /* File name prefix for loading functions */
  char *ppath;             /* Private path for gsf loads */
  char *rres;              /* Pointer to function result  */
  char *gsfnm;             /* Most recent file name read in */
  gaint gsfflg;              /* Dynamic load script functions from files */
  gaint rc;                  /* Exit value                  */
};

/* Operator codes:  ordered by precedence

                    1: |   logical or
                    2: &   logical and
                    3: =   equality
                    4: !=  not equal
                    5: >   greater than
                    6: >=  greater than or equal
                    7: <   less than
                    8: <=  less than or equal
                    9: %   concatenation
                   10: +   addition
                   11: -   subtraction
                   12: *   multiplication
                   13: /   division
                   14: !   unary not
                   15: -   unary minus                        */

char *opchars[13] = {"!=",">=","<=","|","&","=",">","<","%",
       "+","-","*","/"};
gaint opvals[13] = {4,6,8,1,2,3,5,7,9,10,11,12,13};
gaint optyps[15] = {0,0,1,1,1,1,1,1,0,2,2,2,2,0,2};
gaint opmins[7] = {14,12,10,9,3,2,1};
gaint opmaxs[7] = {15,13,11,9,8,2,1};

/* Stack to evaluate the expression.  The stack consists of an
   doubly linked list of structures.                              */

struct stck {
  struct stck *pforw;               /* Forward Pointer  */
  struct stck *pback;               /* Backwards Pointer */
  gaint type;        /* Entry type: 0=oprnd,1=oprtr,2='(',3=')'        */
  union tobj {
    gaint op;                         /* Operator */
    char *strng;                    /* Operand  */
  } obj;
};

/* Function prototypes */

void gsfree (struct gscmn *);
struct gsrecd *gsrtyp (char **, gaint *, gaint *);
gaint gsblck (struct gsrecd *, struct gscmn *);
struct gsrecd *gsbkst (struct gsrecd *, struct gsrecd *,
     struct gsrecd *, gaint *);
struct gsrecd *gsbkdo (struct gsrecd *, struct gsrecd *,
     struct gsrecd *, gaint *);
struct gsrecd *gsbkif (struct gsrecd *, struct gsrecd *,
     struct gsrecd *, gaint *);
gaint gsrunf (struct gsrecd *, struct gscmn *);
void gsfrev (struct gsvar *);
struct gsrecd *gsruns (struct gsrecd *, struct gscmn *, gaint *);
struct gsrecd *gsrund (struct gsrecd *, struct gscmn *, gaint *);
struct gsrecd *gsruni (struct gsrecd *, struct gscmn *, gaint *);
gaint gsstmn (struct gsrecd *, struct gscmn *);
gaint gsassn (struct gsrecd *, struct gscmn *);
char *gsexpr (char *, struct gscmn *);
struct stck *gseval (struct stck *);
char *gscnst (char **);
char *gsgopd (char **, struct gscmn *);
char *gsfunc (char *, char *, struct gscmn *);
char *gsfvar (char *, struct gscmn *);
gaint gsrvar (struct gscmn *, char *, char *);
void stkdmp (struct stck *);
void gsnum (char *, gaint *, gaint *, gadouble *);
struct stck *gsoper (struct stck *);

/* Functions for searching and reading script files */

gaint gsgsfrd (struct gscmn *, gaint, char *);
FILE *gsonam (struct gscmn *, struct gsfdef *);
FILE *gsogsf(struct gscmn *, struct gsfdef *, char *);
char *gsstcp (char *);
gaint gsdelim (char);
char *gsstad (char *, char *);

/* script functions */

gaint gsfsub (struct gscmn *);
gaint gsfwrd (struct gscmn *);
gaint gsflin (struct gscmn *);
gaint gsfpwd (struct gscmn *);
gaint gsfrd (struct gscmn *);
gaint gsfwt (struct gscmn *);
gaint gsfcl (struct gscmn *);
gaint gsfval (struct gscmn *);
gaint gsfsln (struct gscmn *);
gaint gsflog (struct gscmn *, gaint);

gaint gsstmt (struct gsrecd *, struct gscmn *); 
gaint gsfallw (struct gscmn *);
gaint gsfpath (struct gscmn *);
gaint gsfmath (struct gscmn *, gaint);
