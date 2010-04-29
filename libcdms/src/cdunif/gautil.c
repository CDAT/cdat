/*  GrADS (Grid Analysis and Display System) Version 0.9.

    Copyright (c) 1988, 1989, 1990 by Brian Doty (Center for Ocean-
    Land-Atmosphere Interactions, Department of Meteorology,
    University of Maryland, College Park, MD  20742).  Permission is
    granted to any individual or institution to use, copy, or
    redistribute this software so long as it is not sold and all
    copyright notices are retained.                              */

#include <stdio.h>
#include <math.h>
#ifndef __APPLE__
#include <malloc.h>
#endif
#include <limits.h>


#include "grads.h"
#include "gx.h"
#if READLINE == 1
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include "readline.h"
#include "history.h"
#endif

/*mf 961205 --- expose Mike Fiorino's global struct to these routines for 365 day calandars mf*/
/*-------- declare here vice grads.c because this is a common routine with LATS  ------*/
struct gamfcmn mfcmn;
/*mf 961205 --- expose Mike Fiorino's global struct to these routines for 365 day calandars mf*/

static char pout[256];   /* Build Error msgs here */

/* Retrieves the next command from the user.  Leading blanks
   are stripped.  The number of characters entered before the
   CR is returned.                                                    */

int nxtcmd (char *cmd, char *prompt) {
int i,past,cnt;
static char *ch;

  printf ("%s ",prompt);
  past = 0;
  cnt = 0;
  while (1) {
    *cmd = getchar();
    if (*cmd == EOF) return (-1);
    if (*cmd == '\n') {
      cmd++;
      *cmd = '\0';
      return (cnt);
    }
    if (past || *cmd != ' ') {
      cmd++; cnt++; past = 1;
    }
  }
}

/* Retrieves the next command from the user.  Leading blanks
   are stripped.  The number of characters entered before the
   CR is returned.                                                    */

#if READLINE == 1
int nxrdln (char *cmd, char *prompt) {
int i,past,cnt;
static char *ch;

ch=readline(prompt);
for(i=0;i<strlen(ch);i++) *(cmd+i)=*(ch+i);
*(cmd+strlen(ch))='\0';
add_history(ch);
free(ch);
return(strlen(cmd)+1);

}
#endif

/* Date/Time manipulation routines.  Note that these routines
   are not particularly efficient, thus Date/Time conversions
   should be kept to a minimum.                                      */

static int mosiz[13] = {0,31,28,31,30,31,30,31,31,30,31,30,31};
static int momn[13] = {0,44640,40320,44640,43200,44640,43200,
                        44640,44640,43200,44640,43200,44640};
static int mnacum[13] = {0,0,44640,84960,129600,172800,217440,
                        260640,305280,349920,393120,437760,480960};
static int mnacul[13] = {0,0,44640,86400,131040,174240,218880,
                        262080,306720,351360,394560,439200,482400};

/* Add an offset to a time.  Output to dto.                          */

void timadd (struct dt *dtim, struct dt *dto) {
int i;
int cont;

  /* First add months and years.  Normalize as needed.               */

  dto->mo += dtim->mo;
  dto->yr += dtim->yr;

  while (dto->mo>12) {
    dto->mo -= 12;
    dto->yr++;
  }

  /* Add minutes, hours, and days directly.  Then normalize
     to days, then normalize extra days to months/years.             */

  dto->mn += dtim->mn;
  dto->hr += dtim->hr;
  dto->dy += dtim->dy;

  if (dto->mn > 59) {
    i = dto->mn / 60;
    dto->hr += i;
    dto->mn = dto->mn - (i*60);
  }
  if (dto->hr > 23) {
    i = dto->hr / 24;
    dto->dy += i;
    dto->hr = dto->hr - (i*24);
  }

  cont = 1;
  while (dto->dy > mosiz[dto->mo] && cont) {
    if (dto->mo==2 && qleap(dto->yr)) {
      if (dto->dy == 29) cont=0;
      else {
        dto->dy -= 29;
        dto->mo++;
      }
    } else {
      dto->dy -= mosiz[dto->mo];
      dto->mo++;
    }
    while (dto->mo > 12) {dto->mo-=12; dto->yr++;}
  }
}

/* Subtract an offset from a time.  Subtract minutes/hours/days
   first so that we will exactly reverse the operation of timadd     */

void timsub (struct dt *dtim, struct dt *dto) {
int s1,s2;

  /* Subtract minutes, hour, and days directly.  Then normalize
     to days, then normalize deficient days from months/years.       */

  dto->mn = dtim->mn - dto->mn;
  dto->hr = dtim->hr - dto->hr;
  dto->dy = dtim->dy - dto->dy;
  s1 = dto->mo; s2 = dto->yr;
  dto->mo = dtim->mo;
  dto->yr = dtim->yr;

  while (dto->mn < 0) {dto->mn+=60; dto->hr--;}
  while (dto->hr < 0) {dto->hr+=24; dto->dy--;}

  while (dto->dy < 1) {
    dto->mo--;
    if (dto->mo < 1) {dto->mo=12; dto->yr--;}
    if (dto->mo==2 && qleap(dto->yr)) dto->dy += 29;
    else dto->dy += mosiz[dto->mo];
  }

  /* Now subtract months and years.  Normalize as needed.            */

  dto->mo = dto->mo - s1;
  dto->yr = dto->yr - s2;

  while (dto->mo < 1) {dto->mo+=12; dto->yr--;}

  /* Adjust for leaps */

  if (dto->mo==2 && dto->dy==29 && !qleap(dto->yr)) {
    dto->mo=3; dto->dy=1;
  }
}

/* Convert from Absolute time (year/month/day/etc.) to grid
   coordinate.                                                       */

float t2gr (float *vals, struct dt *dtim) {
struct dt stim;
int eyear,mins;
float val,*moincr,*mnincr;
float rdiff;

  /* Get constants associated with this conversion                   */

  stim.yr = (int)(*vals+0.1);
  stim.mo = (int)(*(vals+1)+0.1);
  stim.dy = (int)(*(vals+2)+0.1);
  stim.hr = (int)(*(vals+3)+0.1);
  stim.mn = (int)(*(vals+4)+0.1);

  moincr = vals+5;
  mnincr = vals+6;

  /* If the increment for this conversion is days, hours, or minutes,
     then we do our calculations in minutes.  If the increment is
     months or years, we do our calculations in months.              */

  if (*mnincr>0.1) {
    mins = timdif(&stim,dtim);
    rdiff = (float)mins;
    val = rdiff/(*mnincr);
    val += 1.0;
    return (val);
  } else {
    eyear = stim.yr;
    if (stim.yr > dtim->yr) eyear = dtim->yr;
    rdiff = (((dtim->yr - eyear)*12) + dtim->mo) -
            (((stim.yr - eyear)*12) + stim.mo);
    stim.yr = dtim->yr;
    stim.mo = dtim->mo;
    mins = timdif(&stim,dtim);
    if (mins>0) {
      if (dtim->mo==2 && qleap(dtim->yr) ) {
        rdiff = rdiff + (((float)mins)/41760.0);
      } else {
        rdiff = rdiff + (((float)mins)/((float)momn[dtim->mo]));
      }
    }
    val = rdiff/(*moincr);
    val += 1.0;
    return (val);
  }
}

/* Convert from a t grid coordinate to an absolute time.           */

void gr2t (float *vals, float gr, struct dt *dtim) {
struct dt stim;
float *moincr,*mnincr;
float v;

  /* Get constants associated with this conversion                   */

  stim.yr = (int)(*vals+0.1);
  stim.mo = (int)(*(vals+1)+0.1);
  stim.dy = (int)(*(vals+2)+0.1);
  stim.hr = (int)(*(vals+3)+0.1);
  stim.mn = (int)(*(vals+4)+0.1);

  moincr = vals+5;
  mnincr = vals+6;

  /* Initialize output time                                          */

  dtim->yr = 0;
  dtim->mo = 0;
  dtim->dy = 0;
  dtim->hr = 0;
  dtim->mn = 0;




  /* Do conversion if increment is in minutes.                       */

  if (*mnincr>0.1) {
    v = *mnincr * (gr-1.0);
    if (v>0.0) v = v + 0.5;   /* round */
    else v = v - 0.5;
    dtim->mn = (int)v;
    if (dtim->mn<0) {
      dtim->mn = -1 * dtim->mn;
      timsub (&stim,dtim);
    } else {
      timadd (&stim,dtim);
    }
    return;

  /* Do conversion if increment is in months.  Same as for minutes,
     except special handling is required for partial months.         */

  } else {
    v = *moincr * (gr-1.0);
    if (v<0.0) dtim->mo = (int)(v-0.9999); /* round (sort of)       */
    else dtim->mo = (int)(v+0.0001);
    v = v - (float)dtim->mo;                /* Get fractional month  */
    if (dtim->mo<0) {
      dtim->mo = -1 * dtim->mo;
      timsub (&stim,dtim);
    } else timadd (&stim,dtim);
    if (v<0.0001) return;         /* if fraction small, return       */

    if (dtim->mo==2 && qleap(dtim->yr) ) {
      v = v * 41760.0;
    } else {
      v = v * (float)momn[dtim->mo];
    }
    stim = *dtim;
    dtim->yr = 0;
    dtim->mo = 0;
    dtim->dy = 0;
    dtim->hr = 0;
    dtim->mn = (int)(v+0.5);
    timadd (&stim,dtim);
    return;
  }
}

/* Calculate the difference between two times and return the
   difference in minutes.   The calculation is time2 - time1, so
   if time2 is earlier than time1, the result is negative.           */

int timdif (struct dt *dtim1, struct dt *dtim2) {
int min1,min2,yr;
struct dt *temp;
int swap,mo1,mo2;

  swap = 0;
  if (dtim1->yr > dtim2->yr) {
    temp = dtim1;
    dtim1 = dtim2;
    dtim2 = temp;
    swap = 1;
  }

  min1 = 0;
  min2 = 0;

  yr = dtim1->yr;
  while (yr < dtim2->yr) {
    if (qleap(yr)) min2 += 527040L;
    else min2 += 525600L;
    yr++;
  }

  mo1 = dtim1->mo;
  mo2 = dtim2->mo;
  if (qleap(dtim1->yr)) {
    min1 = min1+mnacul[mo1]+(dtim1->dy*1440L)+(dtim1->hr*60L)+dtim1->mn;
  } else {
    min1 = min1+mnacum[mo1]+(dtim1->dy*1440L)+(dtim1->hr*60L)+dtim1->mn;
  }
  if (qleap(dtim2->yr)) {
    min2 = min2+mnacul[mo2]+(dtim2->dy*1440L)+(dtim2->hr*60L)+dtim2->mn;
  } else {
    min2 = min2+mnacum[mo2]+(dtim2->dy*1440L)+(dtim2->hr*60L)+dtim2->mn;
  }
  if (swap) return (min1-min2);
  else return (min2-min1);
}

/* Test for leap year.  Rules are:

      Divisible by 4, it is a leap year, unless....
      Divisible by 100, it is not a leap year, unless...
      Divisible by 400, it is a leap year.                           */

int qleap (int year)  {
int i,y;

/*mf - disable if 365 day calendar mf*/

 if(mfcmn.cal365) return(0);

  y = year;

  i = y / 4;
  i = (i*4) - y;
  if (i!=0) return (0);

  i = y / 100;
  i = (i*100) - y;
  if (i!=0) return (1);

  i = y / 400;
  i = (i*400) - y;
  if (i!=0) return (0);

  return (1);


}

char *mons[12] = {"jan","feb","mar","apr","may","jun","jul","aug",
                  "sep","oct","nov","dec"};

/* Parse an absolute date/time value.  Format is:

   12:00z 1jan 1989 (jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)

   Must have Z or Month abbrev, or value is invalid.  'def' contains
   higher order missing values (usually from tmin in pst).  Lower order
   values are defaulted to be: dy = 1, hr = 0, mn = 0.              */

char *adtprs (char *ch, struct dt *def, struct dt *dtim) {
int val,flag,i;
char *pos;
char monam[5];

  pos = ch;

  dtim->mn = 0;
  dtim->hr = 0;
  dtim->dy = 1;

  if (*ch>='0' && *ch<='9') {
  flag = 0;
    ch = intprs (ch,&val);
    if (*ch == ':' || *ch == 'z') {
      if (val>23) {
        gaprnt (0,"Syntax Error:  Invalid Date/Time value.\n");
        sprintf (pout,"  Hour = %i -- greater than 23\n",val);
        gaprnt (0,pout);
        return (NULL);
      }
      dtim->hr = val;
      if (*ch == ':') {
        ch++;
        if (*ch>='0' && *ch<='9') {
          ch = intprs (ch,&val);
          if (val>59) {
            gaprnt (0,"Syntax Error:  Invalid Date/Time value.\n");
            sprintf (pout,"  Minute = %i -- greater than 59\n",val);
            gaprnt (0,pout);
            return (NULL);
          }
          if (*ch!='z') {
            gaprnt (0,"Syntax Error:  Invalid Date/Time value.\n");
            gaprnt (0,"  'z' delimiter is missing \n");
            return (NULL);
          }
          dtim->mn = val;
          ch++;
          if (*ch>='0' && *ch<='9') ch = intprs (ch,&val);
          else val = def->dy;
        } else {
          gaprnt (0,"Syntax Error:  Invalid Date/Time value.\n");
          gaprnt (0,"  Missing minute value \n");
          return (NULL);
        }
      } else {
        ch++;
        if (*ch>='0' && *ch<='9') ch = intprs (ch,&val);
        else val = def->dy;
      }
    } else flag = 2;
    dtim->dy = val;
  } else flag = 1;

  monam[0] = *ch;
  monam[1] = *(ch+1);
  monam[2] = *(ch+2);
  monam[3] = '\0';

  i = 0;
  while (i<12 && !cmpwrd(monam,mons[i]) ) i++;
  i++;

  if (i==13) {
    if (flag==1) {
      gaprnt (0,"Syntax Error:  Invalid Date/Time value.\n");
      gaprnt (0,"  Expected month abbreviation, none found\n");
      return (NULL);
    }
    if (flag==2) {
      gaprnt (0,"Syntax Error:  Invalid Date/Time value.\n");
      gaprnt (0,"  Missing month abbreviation or 'z' delimiter\n");
      return (NULL);
    }
    dtim->mo = def->mo;
    dtim->yr = def->yr;
  } else {
    dtim->mo = i;
    ch+=3;
    if (*ch>='0' && *ch<='9') {

/*mf 971020 --- use fullyear only if year 1 = 0001 mf*/

      if(*(ch+2)>='0' && *(ch+2)<='9' && mfcmn.fullyear < 0) {
	mfcmn.fullyear=1;
      } else if(mfcmn.fullyear <0) {
	mfcmn.fullyear=0;
      }
      ch = intprs (ch,&val);

    } else {
      val = def->yr;
    }

/*mf ---  turn off setting of < 100 years to 1900 or 2000  ---mf*/

    if(mfcmn.fullyear == 0) {
      if (val<50) val+=2000;
      else if (val<100) val+=1900;
    }

    dtim->yr = val;
  }

  i = mosiz[dtim->mo];
  if (dtim->mo==2 && qleap(dtim->yr)) i = 29;
  if (dtim->dy > i) {
    gaprnt (0,"Syntax Error:  Invalid Date/Time value.\n");
    sprintf (pout,"  Day = %i -- greater than %i \n",dtim->dy,i);
    gaprnt (0,pout);
    return (NULL);
  }
  return (ch);
}

/* Parse a relative date/time (offset).  Format is:

   nn (yr/mo/dy/hr/mn)

   Examples:  5mo
              1dy12hr
              etc.

   Missing values are filled in with 0s.                             */

char *rdtprs (char *ch, struct dt *dtim) {
int flag,val;
char *pos;
char id[3];

  pos = ch;

  dtim->yr = 0;
  dtim->mo = 0;
  dtim->dy = 0;
  dtim->hr = 0;
  dtim->mn = 0;

  flag = 1;

  while (*ch>='0' && *ch<='9') {
    flag = 0;
    ch = intprs(ch,&val);
    id[0] = *ch; id[1] = *(ch+1); id[2] = '\0';
    if (cmpwrd("yr",id)) dtim->yr = val;
    else if (cmpwrd("mo",id)) dtim->mo = val;
    else if (cmpwrd("dy",id)) dtim->dy = val;
    else if (cmpwrd("hr",id)) dtim->hr = val;
    else if (cmpwrd("mn",id)) dtim->mn = val;
    else {
      gaprnt (0,"Syntax Error:  Invalid Date/Time offset.\n");
      sprintf (pout,"  Expecting yr/mo/dy/hr/mn, found %s\n",id);
      gaprnt (0,pout);
      return (NULL);
    }
    ch+=2;
  }
  if (flag) {
    gaprnt (0,"Syntax Error:  Invalid Date/Time offset.\n");
    gaprnt (0,"  No offset value given\n");
    return (NULL);
  }
  return (ch);
}
static char num[10]={'0','1','2','3','4','5','6','7','8','9'};

int gaedit (float val, char *chars, int icode) {
int i,j,k,sign;
char ch[20], *cc;
float vvv;
int w1,w2;

  vvv=icode;
  vvv=pow(10.0,vvv);
  vvv=vvv*val;

  sign=0;
  if (vvv<0.0) {sign=1; vvv=vvv*(-1.0);}

  i = 1;
  ch[19] = '\0';
  cc = &ch[18];
  w2 = vvv+0.5;
  while (w2>0) {
    w1 = w2/10;
    w1 = w1*10;
    w1 = w2-w1;
    *cc = num[w1];
    cc--; i++;
    icode--;
    k=0;
    if (icode==0) {
      *cc='.';
      cc--;
      i++;
      k=1;
    }
    w2 = w2/10;
  }
  if (k) {i++; *cc='0'; cc--;}
  if (i==1) {i++; *cc='0';}
  else {
    if (sign) {*cc='-'; i++;}
    else *cc++;
  }
  for (j=0;j<i;j++) {
    *chars = *cc;
    cc++; chars++;
  }
  return (i-1);
}

/* Compares two strings.  A match occurs if the leading
   blank-delimited words in the two strings match.  CR and NULL also
   serve as delimiters.                                               */

int cmpwrd (char *ch1, char *ch2) {

  while (*ch1==' '||*ch1=='\t') ch1++;  /* Advance past leading blanks.     */
  while (*ch2==' '||*ch2=='\t') ch2++;

  while (*ch1 == *ch2) {
    if (*ch1==' '||*ch1=='\t'||*ch1=='\0'||*ch1=='\n' ) return (1);
    ch1++; ch2++;
  }

  if ( (*ch1==' '||*ch1=='\t'||*ch1=='\0'||*ch1=='\n') &&
       (*ch2==' '||*ch2=='\t'||*ch2=='\0'||*ch2=='\n') ) return (1);
  return (0);
}

/* Moves a pointer to the start of the next blank-delimited word
   in a string.  If not found, NULL is returned.                      */

char * nxtwrd (char *ch) {

  while (*ch!=' '&&*ch!='\t') {                     /* Skip 1st word  */
    if (*ch == '\0' || *ch == '\n') return (NULL);
    ch++;
  }
  while (*ch==' '||*ch=='\t') ch++;                 /* Find next word */
  if (*ch == '\0' || *ch == '\n') return (NULL);
  return (ch);
}

/* Figures out how many digits are after the decimal point  */

int gxdeci (float val) {
int code,ival;

  code = 0;
  val = fabs(val);
  ival = val;
  val = val - (float)ival;
  while (code<4&&val>0.01) {
    val=val*10.0;
    ival=val;
    val=val-(float)ival;
    code++;
  }
  return (code);
}

/* Linear conversion routine for dimension conversions.               */

float liconv (float *vals, float v) {
  return ( (*vals * v) + *(vals+1) );
}

/* Non-linear scaling routine for discrete levels.  Linear interp
   between levels.  Scaling beyond upper and lower bounds is
   linear based on the last and first grid spacing, respectively.
   In each case a pointer to a list of values is provided.  The
   list contains in its first element the number of values
   in the list.    */

/* Convert a grid value to a world coordinate value.
   This operation needs to be efficient, since it gets done
   very often.  */

float gr2lev (float *vals, float gr) {
int i;
float x;
  if (gr<1.0) return ( *(vals+1) + (1.0-gr)*(*(vals+1)-*(vals+2)) );
  if (gr>*vals) {
    i = (int)(*vals+0.1);
    return ( *(vals+i) + (gr-*vals)*(*(vals+i)-*(vals+i-1)) );
  }
  i = (int)gr;
  return (*(vals+i)+((gr-(float)i)*(*(vals+i+1)-*(vals+i))));
}

/* Convert from world coordinate value to grid value.  This operation
   is not set up to be efficient, under the assumption that it won't
   get done all that often.  */

float lev2gr (float *vals, float lev) {
int i,num;
float gr;
  num = (int)(*vals+0.1);
  for (i=1; i<num; i++) {
    if ( (lev >= *(vals+i) && lev <= *(vals+i+1)) ||
         (lev <= *(vals+i) && lev >= *(vals+i+1)) ) {
      gr = (float)i + (lev - *(vals+i))/(*(vals+i+1) - *(vals+i));
      return (gr);
    }
  }
  if (*(vals+1)<*(vals+num)) {
    if (lev<*(vals+1)) {
      gr = 1.0 + ((lev-*(vals+1))/(*(vals+2)-*(vals+1)));
      return (gr);
    }
    gr = (float)i + ((lev-*(vals+i))/(*(vals+i)-*(vals+i-1)));
    return (gr);
  } else {
    if (lev>*(vals+1)) {
      gr = 1.0 + ((lev-*(vals+1))/(*(vals+2)-*(vals+1)));
      return (gr);
    }
    gr = (float)i + ((lev-*(vals+i))/(*(vals+i)-*(vals+i-1)));
    return (gr);
  }
}

/* Parses a number in a character string.
   This routine will detect numbers of the form:
       nnnn
       -nnnn

   Args:    ch     - pointer to the number, in character form.
            val    - integer value returned
            return value  - address of 1st character past the
                            number parsed.  NULL if no number found
                            at pointer ch or if the number is an
                            invalid format.
             */

char *intprs (char *ch, int *val) {

int nflag,flag;

  nflag = 0;
  if (*ch=='-') { nflag = 1; ch++; }
  else if (*ch=='+') ch++;

  *val = 0;
  flag = 1;

  while (*ch>='0' && *ch<='9') {
    *val = *val*10 + (int)(*ch-'0');
    flag = 0;
    ch++;
  }

  if (flag) return (NULL);

  if (nflag) *val = -1 * *val;
  return (ch);
}

/* Parses a number in a character string.
   This routine will detect numbers of the form:
       nnnn
       nnnn.fff
       nnnn.fffExxx

   Args:    ch     - pointer to the number, in character form.
            val    - floating point value returned
            return value  - address of 1st character past the
                            number parsed.  NULL if no number found
                            at pointer ch or if the number is an
                            invalid format.
             */

char *valprs (char *ch, float *val) {

int nflag,dflag,eflag,enflag,flag,cont;
int pflag,epflag,evflag;
float exp,dfp;
int zip;

  flag=0;
  nflag=0;dflag=0;eflag=0;enflag=0;
  pflag=0;epflag=0;evflag=0;
  *val=0.0;exp=0.0;dfp=0.1;
  zip='0';

  cont=1;
  while (cont) {

    if (*ch>='0' && *ch<='9') {
      if (!flag) flag=1;
      if (eflag) {evflag=1; exp=(exp*10)+(*ch-zip);}
      else if (dflag) { *val = *val+((*ch-zip)*dfp); dfp=dfp/10.0; }
      else *val = (*val*10.0)+(*ch-zip);

    } else if (*ch=='-') {
      if (eflag&&!evflag) {
        if (enflag) {cont=0; flag=0;}
        enflag=1;
      }
      else if (!flag) {
        if (nflag) {cont=0; flag=0;}
        nflag=1;
      } else cont=0;

    } else if (*ch=='+') {
      if (eflag&&!evflag) {
        if (epflag) {cont=0; flag=0;}
        epflag=1;
      }
      else if (!flag) {
        if (pflag) {cont=0; flag=0;}
        pflag=1;
      } else cont=0;

    } else if (*ch=='.') {
      if (dflag||eflag) {cont=0;}
      else dflag=1;

    } else if (*ch=='e') {
      if (eflag) {
        cont=0;
        if (!evflag) flag=0;
      }
      else if (flag) {eflag=1; dflag=0;}
      else cont=0;

    } else cont=0;

    if (cont) ch++;
  }

  if (flag) {
    if (nflag) *val = *val*(-1.0);
    if (eflag) {
      if (enflag) exp = exp*(-1.0);
      *val = *val*(pow(10.0,exp));
    }
    return (ch);
  } else return (NULL);

}


/* dimprs parses a dimension 'expression', ie, where the user
   specifies an absolute or relative dimension value.
   The format is:

   dim op val

   where:  dim = x,y,z,t,lat,lon,lev,time
           op  = +, -, or =
           val = dimension value

   Examples are:
           t=1
           lev=500
           time=00:00z01jan1989

   The coordinates are evaluated with respect to the coordinate
   transformations of the file descriptor passed.  Grid
   coordinates are returned.  Relative offsets are evaluated
   from the values in the status block.

   In addition, r=radius is also supported.  The dimension value
   returned is the radius, and the dimension number returned
   is 9.  This is only valid for stn type files.

   wflag is set to zero if the dimension expression was
   grid coordinates; 1 if it was world coordinates.
                                                                */

char *dimprs (char *ch, struct gastat *pst, struct gafile *pfi,
              int *dim, float *d, int type, int *wflag) {
char *pos, cc, cc2, cd, *frst;
char name[5];
int dval, flag,i,op;
struct dt dtim;
float (*conv) (float *, float);
float *cvals,v;

  frst = ch;
  i = 0;
  while (*ch>='a' && *ch<='z' && i<6) {
    name[i] = *ch;
    ch++; i++;
  }
  name[i] = '\0';
  if (i>4) {
    gaprnt (0,"Syntax Error:  Invalid dimension expression\n");
    sprintf (pout,"  Expecting X/Y/Z/T/LAT/LON/LEV/TIME, found %s\n",
          name);
    gaprnt (0,pout);
    return (NULL);
  }

  if (*ch == '=') op = 0;
  else if (*ch == '+') op = 1;
  else if (*ch == '-') op = 2;
  else {
    gaprnt (0,"Syntax Error:  Invalid dimension expression\n");
    sprintf (pout,"  Expecting +/-/= operator, found %c\n",*ch);
    gaprnt (0,pout);
    return (NULL);
  }

  ch++;
  if (cmpwrd("time",name)) {
    if (op==0) {
      if ( (pos=adtprs(ch,&(pst->tmin),&dtim)) == NULL ) {
        gaprnt (0,"  Invalid dimension expression\n");
        return (NULL);
      }
    } else {
      if ( (pos=rdtprs(ch,&dtim)) == NULL ) {
        return (NULL);
      }
    }
  } else {
    if ( (pos=valprs(ch,&v)) == NULL ) {
      gaprnt (0,"Syntax Error:  Invalid dimension expression\n");
      gaprnt (0,"  Dimension value missing or invalid\n");
      return (NULL);
    }
  }
  ch = pos;

  /* We now have all the info we need about this dimension
     expression.  We can now evaluate it.                          */

  if (cmpwrd("x",name)) *dim = 0;
  else if (cmpwrd("y",name)) *dim = 1;
  else if (cmpwrd("z",name)) *dim = 2;
  else if (cmpwrd("t",name)) *dim = 3;
  else if (cmpwrd("lon",name)) *dim = 4;
  else if (cmpwrd("lat",name)) *dim = 5;
  else if (cmpwrd("lev",name)) *dim = 6;
  else if (cmpwrd("time",name)) *dim = 7;
  else if (type==0 && cmpwrd("r",name)) *dim = 9;
  else {
    gaprnt (0,"Syntax Error:  Invalid dimension expression\n");
    sprintf (pout,"  Expecting X/Y/Z/T/LAT/LON/LEV/TIME, found %s\n",
          name);
    gaprnt (0,pout);
    return (NULL);
  }

  *wflag = 0;
  if (*dim == 9) {
    *d = v;
    return (ch);
  }

  if (*dim < 4) {
    if (op==0) {
      *d = v + pfi->dimoff[*dim];
      return (ch);
    } else {
      if (*dim==pst->idim || *dim==pst->jdim) {
        gaprnt (0,"Syntax Error:  Invalid dimension expression\n");
        gaprnt (0,"  Cannot use an offset value with a varying ");
        gaprnt (0,"dimension\n");
        sprintf (pout,"  Varying dimension = %i \n",*dim);
        gaprnt (0,pout);
        return (NULL);
      }
      if (*dim < 3) {
        if (pfi->type==1) {
          conv = pfi->ab2gr[*dim];
          cvals = pfi->abvals[*dim];
          *d = conv(cvals,pst->dmin[*dim]);
        } else {
          *d = pst->dmin[*dim];
        }
      } else {
        *d = t2gr(pfi->abvals[3],&(pst->tmin));
      }
      if (op==1) *d = *d + v;
      if (op==2) *d = *d - v;
      return (ch);
    }
  } else {
    *dim = *dim - 4;
    *wflag = 1;
    if (op>0) {
      if (*dim==pst->idim || *dim==pst->jdim) {
        gaprnt (0,"Syntax Error:  Invalid dimension expression\n");
        gaprnt (0,"  Cannot use an offset value with a varying ");
        gaprnt (0,"dimension\n");
        sprintf (pout,"  Varying dimension = %i \n",*dim);
        gaprnt (0,pout);
        return (NULL);
      }
      if (*dim==3) {
        if (op==1) timadd (&(pst->tmin),&dtim);
        if (op==2) timsub (&(pst->tmin),&dtim);
      } else {
        if (op==1) v = pst->dmin[*dim] + v;
        if (op==2) v = pst->dmin[*dim] - v;
      }
    }
    if (*dim < 3) {
      if (pfi->type==1 || pfi->type==4) {
        conv = pfi->ab2gr[*dim];
        cvals = pfi->abvals[*dim];
        *d = conv(cvals,v);
      } else {
        *d = v;
      }
    } else {
      *d = t2gr(pfi->abvals[3],&dtim);
    }
    return (ch);
  }
}

/*mf version
  convert all upper case alphabetic characters to lower case.
  The GrADS system is case insensitive, and assumes lower case
  internally in most cases.                                          */

void lowcas (char *ch) {
int i;
int qflag=0;

  while (*ch!='\0' && *ch!='\n') {
    i = *ch;
/*
  mf - 940415 : do not turn to lower case if in "'s
*/
    if(*ch == '\"' && qflag == 0 ) {
      qflag=1;
      } else if(*ch == '\"' && qflag == 1 ) {
	qflag=0;
      }
    if (i>64 && i<91 && qflag==0) {
      i+=32;
      *ch = i;
    } else if(i == 95) {
      *ch=i;
    }
    ch++;
  }
}


/* convert all upper case alphabetic characters to lower case.
   The GrADS system is case insensitive, and assumes lower case
   internally in most cases.                                          */
/*----------original version
void lowcas (char *ch) {
int i;

  while (*ch!='\0' && *ch!='\n') {
    i = *ch;
    if (i>64 && i<91) {
      i+=32;
      *ch = i;
    }
    ch++;
  }
}
------------original version */

/* convert to upper case */

void uppcas (char *ch) {
int i;

  while (*ch!='\0' && *ch!='\n') {
    i = *ch;
    if (i>96 && i<123) {
      i -= 32;
      *ch = i;
    }
    ch++;
  }
}

/* Copies a string of a specified length, or when \0 or \n is hit.
   Trailing blanks are removed, and the output string is terminated
   with '\0'.                                                         */

void getstr (char *ch1, char *ch2, int len) {
char *ch;

  ch = ch1;
  while (len>0 && *ch2!='\n' && *ch2!='\0') {
    *ch1 = *ch2;
    len--;
    ch1++;  ch2++;
  }
  ch1--;
  while (ch1>=ch && *ch1==' ') ch1--;
  ch1++;
  *ch1 = '\0';
}

/* Copies a word of a specified length, or when \0 or \n or ' ' is
   encountered.  The word is terminated with '\0'.                    */

void getwrd (char *ch1, char *ch2, int len) {
char *ch;

  ch = ch1;
  while (len>0 && *ch2!='\n' && *ch2!='\0' && *ch2!=' ' ) {
    *ch1 = *ch2;
    len--;
    ch1++;  ch2++;
  }
  *ch1 = '\0';
}

/* Get minimum and maximum grid value.  Set rmin and rmax in the
   grid descriptor.                                                  */

void gamnmx (struct gagrid *pgr) {
int i,size;
float *r;
int cnt;
  size = pgr->isiz * pgr->jsiz;
  if (size==1) return;
  pgr->rmin= 9.99E35;
  pgr->rmax= -9.99E35;
  r = pgr->grid;
  cnt=0;
  for (i=0;i<size;i++) {
    if (*r != pgr->undef) {
      cnt++;
      if (pgr->rmin>*r) pgr->rmin = *r;
      if (pgr->rmax<*r) pgr->rmax = *r;
    }
    r++;
  }
  if (cnt==0 || pgr->rmin==9.99e35 || pgr->rmax==-9.99e35) {
    pgr->rmin = pgr->undef;
    pgr->rmax = pgr->undef;
  }
}

/*mf version  Remove blanks from a string */
int garemb (char *ch) {
char *cc;
int cnt;
int qflag=0;

  cc = ch;
  cnt = 0;

  while ( *ch!='\n' && *ch!='\0' ) {

/*
  mf - 940415 : do not remove blanks if string in "'s
*/
    if(*ch == '\"' && qflag == 0 ) {
      qflag=1;
      } else if(*ch == '\"' && qflag == 1 ) {
	qflag=0;
      }

    if ( ( (*ch!=' ') || qflag ) && (*ch!='\"') ) {
      *cc = *ch;
      cc++; cnt++;
    }
    ch++;
  }
  *cc = '\0';
  return (cnt);
}

/*---- original ------

int garemb (char *ch) {
char *cc;
int cnt;

  cc = ch;
  cnt = 0;
  while ( *ch!='\n' && *ch!='\0' ) {
    if (*ch!=' ') {
      *cc = *ch;
      cc++; cnt++;
    }
    ch++;
  }
  *cc = '\0';
  return (cnt);
}
*/

static float glts15[40] = {
       -86.60,-82.19,-77.76,-73.32,-68.88,-64.43,-59.99,
       -55.55,-51.11,-46.66,-42.22,-37.77,-33.33,-28.89,
       -24.44,-20.00,-15.55,-11.11, -6.67, -2.22,  2.22,
         6.67, 11.11, 15.55, 20.00, 24.44, 28.89, 33.33,
        37.77, 42.22, 46.66, 51.11, 55.55, 59.99, 64.43,
        68.88, 73.32, 77.76, 82.19, 86.60};
static float glts20[52] = {
       -87.38,-83.98,-80.56,-77.13,-73.71,-70.28,-66.85,
       -63.42,-59.99,-56.57,-53.14,-49.71,-46.28,-42.85,
       -39.43,-36.00,-32.57,-29.14,-25.71,-22.28,-18.86,
       -15.43,-12.00, -8.57, -5.14, -1.71,  1.71,  5.14,
         8.57, 12.00, 15.43, 18.86, 22.28, 25.71, 29.14,
        32.57, 36.00, 39.43, 42.85, 46.28, 49.71, 53.14,
        56.57, 59.99, 63.42, 66.85, 70.28, 73.71, 77.13,
        80.56, 83.98, 87.38};
static float glts30[80] = {
       -88.29, -86.07, -83.84, -81.61, -79.37, -77.14, -74.90,
       -72.67, -70.43, -68.20, -65.96, -63.72, -61.49, -59.25,
       -57.02, -54.78, -52.55, -50.31, -48.07, -45.84, -43.60,
       -41.37, -39.13, -36.89, -34.66, -32.42, -30.19, -27.95,
       -25.71, -23.48, -21.24, -19.01, -16.77, -14.53, -12.30,
       -10.06,  -7.83,  -5.59,  -3.35,  -1.12,   1.12,   3.35,
         5.59,   7.83,  10.06,  12.30,  14.53,  16.77,  19.01,
        21.24,  23.48,  25.71,  27.95,  30.19,  32.42,  34.66,
        36.89,  39.13,  41.37,  43.60,  45.84,  48.07,  50.31,
        52.55,  54.78,  57.02,  59.25,  61.49,  63.72,  65.96,
        68.20,  70.43,  72.67,  74.90,  77.14,  79.37,  81.61,
        83.84,  86.07,  88.29};

static float glats[102] = {
 -88.66,-86.91,-85.16,-83.41,-81.65,-79.90,-78.14,-76.39,-74.63,
 -72.88,-71.12,-69.36,-67.61,-65.85,-64.10,-62.34,-60.58,-58.83,
 -57.07,-55.32,-53.56,-51.80,-50.05,-48.29,-46.54,-44.78,-43.02,
 -41.27,-39.51,-37.76,-36.00,-34.24,-32.49,-30.73,-28.98,-27.22,
 -25.46,-23.71,-21.95,-20.19,-18.44,-16.68,-14.93,-13.17,-11.41,
  -9.66, -7.90, -6.15, -4.39, -2.63, -0.88,  0.88,  2.63,  4.39,
   6.15,  7.90,  9.66, 11.41, 13.17, 14.93, 16.68, 18.44, 20.19,
  21.95, 23.71, 25.46, 27.22, 28.98, 30.73, 32.49, 34.24, 36.00,
  37.76, 39.51, 41.27, 43.02, 44.78, 46.54, 48.29, 50.05, 51.80,
  53.56, 55.32, 57.07, 58.83, 60.58, 62.34, 64.10, 65.85, 67.61,
  69.36, 71.12, 72.88, 74.63, 76.39, 78.14, 79.90, 81.65, 83.41,
  85.16, 86.91, 88.66 };

static float m32lts[32] = {-20.453, -18.01, -15.763, -13.738,
  -11.95, -10.405, -9.097, -8.010, -7.120, -6.392, -5.253, -4.25,
  -3.25, -2.25, -1.25, -0.25, 0.25, 1.25, 2.25, 3.25, 4.25, 5.253,
  6.392, 7.12, 8.01, 9.097, 10.405, 11.95, 13.738, 15.763, 18.01,
  20.453};

/* Given the starting point and the length, return the MOM32 lats */

float *gamo32 (int istrt, int num) {
int size;
float *vals;

  istrt--;
  if (istrt+num > 32) {
    gaprnt (0,"Open Error: Invalid MOM32 scaling.\n");
    gaprnt (0,"  Maximum 32 latitudes exceeded \n");
    return (NULL);
  }
  size = (num+3) * sizeof(float);
  vals = (float *)malloc(size);
  if (vals==NULL) {
    gaprnt (0,"Memory Allocation Error: MOM32 Grid Scaling\n");
    return (NULL);
  }
  *vals = (float)num;
  for (size=0; size<num; size++) *(vals+size+1) = m32lts[size+istrt];
  *(vals+num+1) = -999.9;
  return (vals);
}


/* Given the starting point and the length, return the gaussian lats */

float *gagaus (int istrt, int num) {
int size;
float *vals;

  istrt--;
  if (istrt+num > 102) {
    gaprnt (0,"Open Error: Invalid GAUSR40 scaling.\n");
    gaprnt (0,"  Maximum 102 latitudes exceeded \n");
    return (NULL);
  }
  size = (num+3) * sizeof(float);
  vals = (float *)malloc(size);
  if (vals==NULL) {
    gaprnt (0,"Memory Allocation Error: Gaussian Grid Scaling\n");
    return (NULL);
  }
  *vals = (float)num;
  for (size=0; size<num; size++) *(vals+size+1) = glats[size+istrt];
  *(vals+num+1) = -999.9;
  return (vals);
}

/* Given the starting point and the length, return the gaussian lats
  for R30 grids */

float *gags30 (int istrt, int num) {
int size;
float *vals;

  istrt--;
  if (istrt+num > 80) {
    gaprnt (0,"Open Error: Invalid GAUSR30 scaling.\n");
    gaprnt (0,"  Maximum 80 latitudes exceeded \n");
    return (NULL);
  }
  size = (num+3) * sizeof(float);
  vals = (float *)malloc(size);
  if (vals==NULL) {
    gaprnt (0,"Memory Allocation Error: Gaussian Grid Scaling\n");
    return (NULL);
  }
  *vals = (float)num;
  for (size=0; size<num; size++) *(vals+size+1) = glts30[size+istrt];
  *(vals+num+1) = -999.9;
  return (vals);
}

/* Given the starting point and the length, return the gaussian lats
  for R20 grids */

float *gags20 (int istrt, int num) {
int size;
float *vals;

  istrt--;
  if (istrt+num > 52) {
    gaprnt (0,"Open Error: Invalid GAUSR20 scaling.\n");
    gaprnt (0,"  Maximum 52 latitudes exceeded \n");
    return (NULL);
  }
  size = (num+3) * sizeof(float);
  vals = (float *)malloc(size);
  if (vals==NULL) {
    gaprnt (0,"Memory Allocation Error: Gaussian Grid Scaling\n");
    return (NULL);
  }
  *vals = (float)num;
  for (size=0; size<num; size++) *(vals+size+1) = glts20[size+istrt];
  *(vals+num+1) = -999.9;
  return (vals);
}

/* Given the starting point and the length, return the gaussian lats
  for R15 grids */

float *gags15 (int istrt, int num) {
int size;
float *vals;

  istrt--;
  if (istrt+num > 40) {
    gaprnt (0,"Open Error: Invalid GAUSR15 scaling.\n");
    gaprnt (0,"  Maximum 40 latitudes exceeded \n");
    return (NULL);
  }
  size = (num+3) * sizeof(float);
  vals = (float *)malloc(size);
  if (vals==NULL) {
    gaprnt (0,"Memory Allocation Error: Gaussian Grid Scaling\n");
    return (NULL);
  }
  *vals = (float)num;
  for (size=0; size<num; size++) *(vals+size+1) = glts15[size+istrt];
  *(vals+num+1) = -999.9;
  return (vals);
}

char *monc[12] = {"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG",
                  "SEP","OCT","NOV","DEC"};

int gat2ch (struct dt *dtim, int tinc, char *ch) {
int mn1,mn2,hr1,hr2,dy1,dy2,len,mnth;

  mnth = dtim->mo - 1L;
  mn1 = dtim->mn/10L;
  mn2 = dtim->mn - (mn1*10);
  hr1 = dtim->hr/10L;
  hr2 = dtim->hr - (hr1*10);
  dy1 = dtim->dy/10L;
  dy2 = dtim->dy - (dy1*10);
  if (tinc==1) {
    sprintf(ch,"%li",dtim->yr);
  }
  else if (tinc==2) {
    if (dtim->yr==9999L) {
      sprintf(ch,"%s",monc[mnth]);
    } else {
      sprintf(ch,"%s%li",monc[mnth],dtim->yr);
    }
  }
  else if (tinc==3) {
    sprintf(ch,"%i%i%s%li",dy1,dy2,monc[mnth],dtim->yr);
  }
  else if (tinc==4) {
    sprintf(ch,"%i%iZ%i%i%s%li",hr1,hr2,dy1,dy2,
          monc[mnth],dtim->yr);
  }
  else if (tinc==5) {
    sprintf(ch,"%i%i:%i%iZ%i%i%s%li",hr1,hr2,mn1,mn2,dy1,dy2,
          monc[mnth],dtim->yr);
  }
  else sprintf(ch,"???");
  len=0;
  while (ch[len]) len++;
  return (len);
}

/* Compare two strings given the length.  */
/* Return 0 if the string match, otherwise 1.  */

int cmpch (char *str1, char *str2, int len) {

  while (len>0) {
    len--;
    if (*(str1+len) != *(str2+len)) return (1);
  }
  return (0);
}

/* Free anything hung off a gastat block */

void gafree (struct gastat *pst) {

  if (pst->type == 1) {
    gagfre (pst->result.pgr);
    pst->result.pgr=NULL;
  } else {
    gasfre (pst->result.stn);
    pst->result.stn=NULL;
  }
}

void gagfre (struct gagrid *pgr) {
  if (pgr==NULL) return;
  if (pgr->alocf) {
    free(pgr->ivals);
    free(pgr->jvals);
  }
  if (pgr->idim>-1 && (pgr->isiz*pgr->jsiz)>1) free (pgr->grid);
  free (pgr);
}

void gasfre (struct gastn *stn) {
int i;
  if (stn==NULL) return;
  for (i=0; i<BLKNUM; i++) {
    if (stn->blks[i] != NULL) free (stn->blks[i]);
  }
  free (stn->tvals);
  free (stn);
}


/* Expand file names prefixed with '^' from data descriptor
   files */

void fnmexp (char *out, char *in1, char *in2) {
char *pos, *ch, envv[20], *envr;
int i,j;

  if (*in1=='$') {
    in1++;
    i = 0;
    while (*in1!='/' && *in1!='\0' && i<16) {
      envv[i] = *in1;
      i++; in1++;
    }
    envv[i] = '\0';
    envr = gxgsym(envv);
    if (envr) {
      i = 0; j = 0;
      while (*(envr+j)) {
        *(out+i) = *(envr+j);
        i++; j++;
      }
      while (*in1!='\0' && *in1!=' ' && *in1!='\n') {
        *(out+i) = *in1;
        i++; in1++;
      }
    }
    return;
  }
  ch = in2;
  pos=NULL;
  while (*ch!='\0' && *ch!=' ' && *ch!='\n') {
    if (*ch=='/') pos=ch;
    ch++;
  }
  if (pos) pos++;
  while (pos!=NULL && in2<pos) {
    *out = *in2;
    out++; in2++;
  }
  in1++;
  while (*in1!='\0' && *in1!=' ' && *in1!='\n') {
    *out = *in1;
    out++; in1++;
  }
  *out = '\0';
}

/* Given a file name template and a dt structure, fill in to get the
   file name */

char *gafndt (char *fn, struct dt *dtim, struct dt *dtimi, float *vals) {
struct dt stim;
int len,iv,tdif;
char *fnout, *in, *out;

  len = 0;
  while (*(fn+len)) len++;
  len+=3;
  fnout = (char *)malloc(len);
  if (fnout==NULL) return (NULL);

  in = fn;
  out = fnout;

  while (*in) {
    if (*in=='%' && *(in+1)=='i') {
      if (*(in+2)=='y' && *(in+3)=='2') {
        iv = dtimi->yr/100;
        iv = dtimi->yr - iv*100;
        sprintf (out,"%02i",iv);
        out+=2;  in+=4;
      } else if (*(in+2)=='y' && *(in+3)=='4') {
        sprintf (out,"%04i",dtimi->yr);
        out+=4;  in+=4;
      } else if (*(in+2)=='m' && *(in+3)=='1') {
          sprintf (out,"%i",dtimi->mo);
        while (*out) out++;
        in+=4;
      } else if (*(in+2)=='m' && *(in+3)=='2') {
        sprintf (out,"%02i",dtimi->mo);
        out+=2;  in+=4;
      } else if (*(in+2)=='m' && *(in+3)=='h') {
        if (dtimi->dy < 16) *out='a';
        else *out = 'b';
        out+=1;  in+=4;
      } else if (*(in+2)=='m' && *(in+3)=='H') {
        if (dtimi->dy < 16) *out='A';
        else *out = 'B';
        out+=1;  in+=4;
      } else if (*(in+2)=='m' && *(in+3)=='c') {
        *out = *(mons[dtimi->mo-1]);
        *(out+1) = *(mons[dtimi->mo-1]+1);
        *(out+2) = *(mons[dtimi->mo-1]+2);
        out+=3;  in+=4;
      } else if (*(in+2)=='d' && *(in+3)=='1') {
        sprintf (out,"%i",dtimi->dy);
        while (*out) out++;
        in+=4;
      } else if (*(in+2)=='d' && *(in+3)=='2') {
        sprintf (out,"%02i",dtimi->dy);
        out+=2;  in+=4;
      } else if (*(in+2)=='h' && *(in+3)=='1') {
        sprintf (out,"%i",dtimi->hr);
        while (*out) out++;
        in+=4;
      } else if (*(in+2)=='h' && *(in+3)=='2') {
        sprintf (out,"%02i",dtimi->hr);
        out+=2;  in+=4;
/*mf added 95082800 mf*/
      } else if (*(in+2)=='h' && *(in+3)=='3') {
        sprintf (out,"%03i",dtimi->hr);
        out+=3;  in+=4;
      } else if (*(in+2)=='n' && *(in+3)=='2') {
        sprintf (out,"%02i",dtimi->mn);
        out+=2;  in+=4;
      } else {
        *out = *in;
        in++; out++;
      }
    } else if (*in=='%' && *(in+1)=='y' && *(in+2)=='2') {
      iv = dtim->yr/100;
      iv = dtim->yr - iv*100;
      sprintf (out,"%02i",iv);
      out+=2;  in+=3;
    } else if (*in=='%' && *(in+1)=='y' && *(in+2)=='4') {
      sprintf (out,"%04i",dtim->yr);
      out+=4;  in+=3;
    } else if (*in=='%' && *(in+1)=='m' && *(in+2)=='1') {
      sprintf (out,"%i",dtim->mo);
      while (*out) out++;
      in+=3;
    } else if (*in=='%' && *(in+1)=='m' && *(in+2)=='2') {
      sprintf (out,"%02i",dtim->mo);
      out+=2;  in+=3;
    } else if (*in=='%' && *(in+1)=='m' && *(in+2)=='h') {
      if (dtim->dy < 16) *out='a';
      else *out = 'b';
      out+=1;  in+=3;
    } else if (*in=='%' && *(in+1)=='m' && *(in+2)=='H') {
      if (dtim->dy < 16) *out='A';
      else *out = 'B';
      out+=1;  in+=3;
    } else if (*in=='%' && *(in+1)=='m' && *(in+2)=='c') {
      *out = *(mons[dtim->mo-1]);
      *(out+1) = *(mons[dtim->mo-1]+1);
      *(out+2) = *(mons[dtim->mo-1]+2);
      out+=3;  in+=3;
    } else if (*in=='%' && *(in+1)=='d' && *(in+2)=='1') {
      sprintf (out,"%i",dtim->dy);
      while (*out) out++;
      in+=3;
    } else if (*in=='%' && *(in+1)=='d' && *(in+2)=='2') {
      sprintf (out,"%02i",dtim->dy);
      out+=2;  in+=3;
    } else if (*in=='%' && *(in+1)=='h' && *(in+2)=='1') {
      sprintf (out,"%i",dtim->hr);
      while (*out) out++;
      in+=3;
    } else if (*in=='%' && *(in+1)=='h' && *(in+2)=='2') {
      sprintf (out,"%02i",dtim->hr);
      out+=2;  in+=3;
/*mf added 95082800 mf*/
    } else if (*in=='%' && *(in+1)=='h' && *(in+2)=='3') {
      sprintf (out,"%03i",dtim->hr);
      out+=3;  in+=3;
    } else if (*in=='%' && *(in+1)=='n' && *(in+2)=='2') {
      sprintf (out,"%02i",dtim->mn);
      out+=2;  in+=3;
    } else if (*in=='%' && *(in+1)=='f' && *(in+2)=='2') {
      stim.yr = (int)(*vals+0.1);
      stim.mo = (int)(*(vals+1)+0.1);
      stim.dy = (int)(*(vals+2)+0.1);
      stim.hr = (int)(*(vals+3)+0.1);
      stim.mn = (int)(*(vals+4)+0.1);
      tdif = timdif(&stim,dtim);
      tdif = (tdif+30)/60;
      if (tdif<99) sprintf (out,"%02i",tdif);
      else sprintf (out,"%i",tdif);
      while (*out) out++;
      in+=3;
/*mf added 95082800 mf*/
    } else if (*in=='%' && *(in+1)=='f' && *(in+2)=='3') {
      stim.yr = (int)(*vals+0.1);
      stim.mo = (int)(*(vals+1)+0.1);
      stim.dy = (int)(*(vals+2)+0.1);
      stim.hr = (int)(*(vals+3)+0.1);
      stim.mn = (int)(*(vals+4)+0.1);
      tdif = timdif(&stim,dtim);
      tdif = (tdif+30)/60;
      if (tdif<999) sprintf (out,"%03i",tdif);
      else sprintf (out,"%i",tdif);
      while (*out) out++;
      in+=3;
    } else {
      *out = *in;
      in++; out++;
    }
  }
  *out = '\0';
  return (fnout);
}

/* Byte swap requested number of 4 byte elements */

void gabswp (float *r, int cnt) {
int i;
char *ch1,*ch2,*ch3,*ch4,cc1,cc2;

  ch1 = (char *)r;
  ch2 = ch1+1;
  ch3 = ch2+1;
  ch4 = ch3+1;
  for (i=0; i<cnt; i++) {
    cc1 = *ch1;
    cc2 = *ch2;
    *ch1 = *ch4;
    *ch2 = *ch3;
    *ch3 = cc2;
    *ch4 = cc1;
    ch1+=4; ch2+=4; ch3+=4; ch4+=4;
  }
}

/* Byte swap a report header from a station data file */

void gahswp (struct rpthdr *hdr) {
  gabswp((float *)(&(hdr->lat)),5);
}

/* Return day of week for date/time  0=sunday, 6=saturday */

int dayweek (struct dt *dtime) {
struct dt anch;
int i,j;
  if (dtime->yr<1950 || dtime->yr>2020) return(7);
  anch.yr = 1950;
  anch.mo = 1;
  anch.dy = 1;
  anch.hr = 0;
  anch.mn = 0;
  i = timdif(&anch,dtime);
  i = i/1440;
  j = i/7;
  i = i - j*7;
  return(i);
}

/*
 * convert an IMB float to single precision number v1.0
 *
 *                      Wesley Ebisuzaki
 */

float ibm2flt(unsigned char *ibm) {

	int positive, power;
	unsigned int abspower;
	long int mant;
	double value, exp;

	positive = (ibm[0] & 0x80) == 0;
	mant = (ibm[1] << 16) + (ibm[2] << 8) + ibm[3];
	power = (int) (ibm[0] & 0x7f) - 64;
	abspower = power > 0 ? power : -power;


	/* calc exp */
	exp = 16.0;
	value = 1.0;
	while (abspower) {
		if (abspower & 1) {
			value *= exp;
		}
		exp = exp * exp;
		abspower >>= 1;
	}

	if (power < 0) value = 1.0 / value;
	value = value * mant / 16777216.0;
	if (positive == 0) value = -value;
	return (float)value;
}

/*
 * convert a float to an IBM single precision number v1.0
 *
 *                      Wesley Ebisuzaki
 *
 * doesn't handle subnormal numbers
 */

int flt2ibm(float x, unsigned char *ibm) {

	int sign, exp, i;
	double mant;

	if (x == 0.0) {
		ibm[0] = ibm[1] = ibm[2] = ibm[3] = 0;
		return 0;
	}

	/* sign bit */
	if (x < 0.0) {
		sign = 128;
		x = -x;
	}
	else sign = 0;

	mant = frexp((double) x, &exp);

	/* round up by adding 2**-24 */
	/* mant = mant + 1.0/16777216.0; */

	if (mant >= 1.0) {
		mant = 0.5;
		exp++;
	}
	while (exp & 3) {
		mant *= 0.5;
		exp++;
	}
	
	exp = exp/4 + 64;

	if (exp < 0) {
		fprintf(stderr,"underflow in flt2ibm\n");
		ibm[0] = ibm[1] = ibm[2] = ibm[3] = 0;
		return 0;
	}
	if (exp > 127) {
		fprintf(stderr,"overflow in flt2ibm\n");
		ibm[0] = sign | 127;
		ibm[1] = ibm[2] = ibm[3] = 255;
		return -1;
	}

	/* normal number */

	ibm[0] = sign | exp;

	mant = mant * 256.0;
	i = floor(mant);
	mant = mant - i;
	ibm[1] = i;

	mant = mant * 256.0;
	i = floor(mant);
	mant = mant - i;
	ibm[2] = i;

	ibm[3] = floor(mant*256.0);

	return 0;
}

/* wesley ebisuzaki v0.1
 *
 * takes 4 byte character string (single precision ieee big-endian)
 * and returns a float
 *
 * doesn't handle NAN, infinity and any other funny stuff in ieee
 *
 * ansi C
 */

float ieee2flt(unsigned char *ieee) {
	double fmant;
	int exp;

        if (ieee[0] == 0 && ieee[1] == 0 && ieee[2] == 0 && ieee[3] == 0)
	   return (float) 0.0;

	exp = ((ieee[0] & 127) << 1) + (ieee[1] >> 7);
	fmant = (double) ((int) ieee[3] + (int) (ieee[2] << 8) +
              (int) ((ieee[1] | 128) << 16));
	if (ieee[0] & 128) fmant = -fmant;
	return (float) (ldexp(fmant, (int) (exp - 128 - 22)));
}


/*
 * convert a float to an ieee single precision number v1.1
 * (big endian)
 *                      Wesley Ebisuzaki
 *
 * bugs: doesn't handle subnormal numbers
 * bugs: assumes length of integer >= 25 bits
 */

int flt2ieee(float x, unsigned char *ieee) {

	int sign, exp;
        unsigned int umant;
	double mant;

	if (x == 0.0) {
		ieee[0] = ieee[1] = ieee[2] = ieee[3] = 0;
		return 0;
	}

	/* sign bit */
	if (x < 0.0) {
		sign = 128;
		x = -x;
	}
	else sign = 0;
	mant = frexp((double) x, &exp);

        /* 2^24 = 16777216 */

	umant = mant * 16777216 + 0.5;
	if (umant >= 16777216) {
            umant = umant / 2;
            exp++;
        }
        /* bit 24 should be a 1 .. not used in ieee format */

	exp = exp - 1 + 127;

	if (exp < 0) {
		/* signed zero */
		ieee[0] = sign;
		ieee[1] = ieee[2] = ieee[3] = 0;
		return 0;
	}
	if (exp > 255) {
		/* signed infinity */
		ieee[0] = sign + 127;
		ieee[1] = 128;
                ieee[2] = ieee[3] = 0;
                return 0;
	}
	/* normal number */

	ieee[0] = sign + (exp >> 1);

        ieee[3] = umant & 255;
        ieee[2] = (umant >> 8) & 255;
        ieee[1] = ((exp & 1) << 7) + ((umant >> 16) & 127);
	return 0;
}

int be_int2int(unsigned char *be_int) {

	int sign;
	unsigned long n;

	sign = (be_int[0] & 128);
	n = (unsigned int) ((be_int[0] & 127) << 24) +
	    (unsigned int)  (be_int[1] << 16) +
	    (unsigned int)  (be_int[2] <<  8) +
	    (unsigned int)   be_int[3];

	/* positive number */

	if (sign == 0) {
	    return (n <= INT_MAX) ? (int) n : (int) INT_MAX;
	}

	/* negative number */
	n = (n ^ 0x7fffffff);

	if (n <= (unsigned) -(INT_MIN+1)) {
	    return -1 - (int) n;
	}
	return INT_MIN;
}
