#include <stdio.h>
#include <string.h>
#include "cmor.h"
#include <udunits2.h>
#include <stdlib.h>
#include "cdmsint.h"
#include <math.h>

int cuErrOpts =CU_VERBOSE;

int cmor_calendar_c2i(char *calendar, cdCalenType *ical) {
  cmor_add_traceback("cmor_calendar_c2i");
  cmor_is_setup();
  if (strcmp(calendar,"gregorian")==0)*ical = cdMixed;
  else if (strcmp(calendar,"standard")==0) *ical = cdMixed;
  else if (strcmp(calendar,"proleptic_gregorian")==0) *ical = cdStandard;
  else if (strcmp(calendar,"noleap")==0) *ical = cdNoLeap;
  else if (strcmp(calendar,"365_day")==0) *ical = cdNoLeap;
  else if (strcmp(calendar,"360_day")==0) *ical = cd360;
  else if (strcmp(calendar,"julian")==0) *ical = cdJulian;
  else if (strcmp(calendar,"none")==0) *ical = cdClim;
  else {
    cmor_pop_traceback();
    return 1;
  }
  cmor_pop_traceback();
  return 0;
}

double cmor_convert_interval_to_seconds( double interv, char *inunits) {
  /* this converts times values from some units to some others */

  int i,oui,n;
  char msg[CMOR_MAX_STRING];
  char ctmp[CMOR_MAX_STRING];
  char sshort[6];
  extern ut_system *ut_read;
  ut_unit *user_units=NULL, *cmor_units=NULL;
  cv_converter *ut_cmor_converter=NULL;
  double tmp;

  cmor_add_traceback("cmor_convert_interval_to_seconds");

  strcpy(msg,"seconds");

  ut_trim(msg,UT_ASCII);
  cmor_units = ut_parse(ut_read, msg,UT_ASCII);

  sshort[5]='\0';
  /* first we need to figure out the out units */
  /* step 1 look for the since keyword */
  n=strlen(inunits);
  oui=-1;
  for (i=0;i<n;i++) {
    strncpy(sshort,&inunits[i],5);
    if (strcmp(sshort,"since")==0) {oui=i;break;}
  }
  if (oui==-1) {
    snprintf(msg,CMOR_MAX_STRING,"Time units conversion, output units must contain the 'since' word, you defined: %s",inunits);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }

  strncpy(msg,inunits,oui-1);
  msg[oui-1]='\0';

  /* ok at this point we need to convert this in some base units: seconds */
  ut_trim(msg,UT_ASCII);

  user_units = ut_parse(ut_read, msg, UT_ASCII);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"In udunuits parsing user units: %s",msg);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  if (ut_are_convertible(cmor_units,user_units)==0 ) {
    snprintf(ctmp,CMOR_MAX_STRING,"axis interval units (%s) are incompatible with seconds",msg);
    cmor_handle_error(ctmp,CMOR_CRITICAL);
    cmor_pop_traceback();
    return 1;
  }
  ut_cmor_converter=ut_get_converter(user_units,cmor_units);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"In udunuits getting converter");
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  tmp = cv_convert_double(ut_cmor_converter,interv);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"In udunuits converting");
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  cv_free(ut_cmor_converter);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing converter");
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  ut_free(user_units);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units");
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  ut_free(cmor_units);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units");
    cmor_handle_error(msg,CMOR_CRITICAL);
  }

  cmor_pop_traceback();
  return tmp;
}
int cmor_convert_time_units( char *inunits, char *outunits, char *loutunits) {
  /* this converts times values from some units to some others */

  int i,oui,iui,n;
  char msg[CMOR_MAX_STRING];
  char sshort[6];
  int hasqm;

  cmor_add_traceback("cmor_convert_time_units");
  cmor_is_setup();

  sshort[5]='\0';
  /* first we need to figure out the out units */
  /* step 1 look for the since keyword */
  n=strlen(outunits);
  oui=-1;
  for (i=0;i<n;i++) {
    strncpy(sshort,&outunits[i],5);
    if (strcmp(sshort,"since")==0) {oui=i;break;}
  }
  if (oui==-1) {
    snprintf(msg,CMOR_MAX_STRING,"Time units conversion, output units must contain the 'since' word");
    cmor_handle_error(msg,CMOR_CRITICAL);
  }

  /* ok now check if output units have a "?" */
  hasqm =-1;
  for (i=oui+5;i<n;i++)  if (outunits[i]=='?') {hasqm=i;break;}

  /* here we check for the since in user units */
  n=strlen(inunits);
  iui=-1;
  for (i=0;i<n;i++) {
    strncpy(sshort,&inunits[i],5);
    if (strcmp(sshort,"since")==0) {iui=i;break;}
  }
  if (iui==-1) {
    snprintf(msg,CMOR_MAX_STRING,"Time units conversion, input units must contain the 'since' word");
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  /* now construct the output units in case of a ? */
  if (hasqm!=-1) { /* there is a ? */
    strncpy(loutunits,outunits,oui);
    loutunits[oui]='\0';
    strcpy(msg,&inunits[iui]);
    strncat(loutunits,msg,CMOR_MAX_STRING-strlen(loutunits));
  }
  else { /* fully defined output units */
    strncpy(loutunits,outunits,CMOR_MAX_STRING);
  }
  cmor_pop_traceback();
  return 0;
}

int cmor_convert_time_values( void *values_in, char type, int nvalues, double *values_out, char *inunits, char *outunits, char *calin, char *calout) {
  /* this converts times values from some units to some others */

  int i;
  char msg[CMOR_MAX_STRING];
  char loutunits[CMOR_MAX_STRING];
  double dtmp;
  cdCalenType icali, icalo;
  cdCompTime comptime;

  cmor_add_traceback("cmor_convert_time_values");
  cmor_is_setup();


  if (cmor_calendar_c2i(calin,&icali)!=0) {
    snprintf(msg,CMOR_MAX_STRING,"Cannot convert times for calendar: %s",calin);
    cmor_handle_error(msg,CMOR_NORMAL);
    cmor_pop_traceback();
    return 1;
  }
  if (cmor_calendar_c2i(calout,&icalo)!=0) {
    snprintf(msg,CMOR_MAX_STRING,"Cannot convert times for calendar: %s",calout);
    cmor_handle_error(msg,CMOR_NORMAL);
    cmor_pop_traceback();
    return 1;
  }

  i = cmor_convert_time_units( inunits, outunits, &loutunits[0]);
  for (i=0;i<nvalues;i++) {
     if (type=='d') dtmp = (double)((double *)values_in)[i];
     else if (type=='f') dtmp = (double)((float *)values_in)[i];
     else if (type=='l') dtmp = (double)((long *)values_in)[i];
     else if (type=='i') dtmp = (double)((int *)values_in)[i];
     else {
       snprintf(msg,CMOR_MAX_STRING,"cannot convert time value from '%c' type",type);
       cmor_handle_error(msg,CMOR_CRITICAL);
     }
    /* ok makes a comptime out of input */
    cdRel2Comp(icali,&inunits[0],dtmp,&comptime);
    /* ok now converts that back to a rel units with outunits */
    cdComp2Rel(icalo,comptime,loutunits,&dtmp);
    values_out[i]=dtmp;
  }
  cmor_pop_traceback();
  return 0;
}

int cmor_set_axis_attribute(int id, char *attribute_name, char type, void *value)
{
  extern cmor_axis_t cmor_axes[];
  char msg[CMOR_MAX_STRING];
  int i,index;

  cmor_add_traceback("cmor_set_axis_attribute");
  cmor_is_setup();

  index=-1;
  cmor_trim_string(attribute_name,msg);
  for (i=0;i<cmor_axes[id].nattributes;i++) {
    if (strcmp(cmor_axes[id].attributes[i],msg)==0) {index=i;break;} /* we found it */
  }
  if (index==-1) {index=cmor_axes[id].nattributes; cmor_axes[id].nattributes+=1;}
  strncpy(cmor_axes[id].attributes[index],msg,CMOR_MAX_STRING); /*stores the name */
  cmor_axes[id].attributes_type[index]=type;
  if (type=='c') {if (strlen(value)>0) strncpytrim(cmor_axes[id].attributes_values_char[index],value,CMOR_MAX_STRING);}
  else if (type=='f')  cmor_axes[id].attributes_values_num[index] = (double)*(float*)value;
  else if (type=='i')  cmor_axes[id].attributes_values_num[index] = (double)*(int*)value;
  else if (type=='d') cmor_axes[id].attributes_values_num[index] = (double)*(double*)value;
  else if (type=='l') cmor_axes[id].attributes_values_num[index] = (double)*(long*)value;
  else {
    snprintf(msg,CMOR_MAX_STRING,"unknown type %c allowed types are c,i,l,f,d, for attribute %s of axis %s (table: %s)",type,attribute_name,cmor_axes[id].id,cmor_tables[cmor_axes[id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_NORMAL);
    cmor_pop_traceback();
    return 1;
  }
  cmor_pop_traceback();
  return 0;
}
int cmor_get_axis_attribute(int id, char *attribute_name, char type, void *value)
{
  extern cmor_axis_t cmor_axes[];
  char msg[CMOR_MAX_STRING];
  int i,index;
  cmor_add_traceback("cmor_get_axis_attribute");
  cmor_is_setup();
  index=-1;
  for (i=0;i<cmor_axes[id].nattributes;i++) {
    if (strcmp(cmor_axes[id].attributes[i],attribute_name)==0) {index=i;break;} /* we found it */
  }
  if (index==-1) {
    snprintf(msg,CMOR_MAX_STRING,"Attribute %s could not be found for axis %i (%s, table: %s)",attribute_name,id,cmor_axes[id].id,cmor_tables[cmor_axes[id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_NORMAL);
    cmor_pop_traceback();
    return 1;
  }
  type = cmor_axes[id].attributes_type[i];
  if (type=='c')  strcpy(value,cmor_axes[id].attributes_values_char[index]);
  else if (type=='f')  value = (float *)&cmor_axes[id].attributes_values_num[index];
  else if (type=='i')  value = (int *)&cmor_axes[id].attributes_values_num[index];
  else if (type=='d') value = (double *)&cmor_axes[id].attributes_values_num[index];
  else if (type=='l') value = (long *)&cmor_axes[id].attributes_values_num[index];
  cmor_pop_traceback();
  return 0;
}
int cmor_has_axis_attribute(int id, char *attribute_name)
{
  extern cmor_axis_t cmor_axes[];
  int i,index;
  cmor_add_traceback("cmor_has_axis_attribute");
  cmor_is_setup();
  index=-1;
  for (i=0;i<cmor_axes[id].nattributes;i++) {
    if (strcmp(cmor_axes[id].attributes[i],attribute_name)==0) {index=i;break;} /* we found it */
  }
  if (index==-1) {
    cmor_pop_traceback();
    return 1;
  }
  cmor_pop_traceback();
  return 0;
}

int cmor_check_values_inside_bounds(double *values,double *bounds, int length, char *name) {
  int i;
  char msg[CMOR_MAX_STRING];
  cmor_add_traceback("cmor_check_values_inside_bounds");
  for (i=0;i<length;i++) {
    /* printf("check within: %i: %lf < %lf < %lf ???\n",i,bounds[2*i],values[i],bounds[2*i+1]); */
    if ( ((bounds[2*i]<values[i]) && (bounds[2*i+1]<values[i])) || ((bounds[2*i]>values[i]) && (bounds[2*i+1]>values[i])) ) {
      snprintf(msg,CMOR_MAX_STRING,"axis %s has values not within bounds at indice: %i: %lf not within: %lf, %lf",name,i,values[i],bounds[2*i],bounds[2*i+1]);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
  }
  cmor_pop_traceback();
  return 0;
}

int cmor_check_monotonic(double *values,int length, char *name,int isbounds, int axis_id) {
  int i,treatlon=0,j=0;
  char msg[CMOR_MAX_STRING];
  char msg2[CMOR_MAX_STRING];
  cmor_axis_def_t *refaxis;
  int mono;
  int nloop;
  double *values2,tmp;
  /*for (i=0;i<length;i++) printf("in monotonic: %i, %lf, %i\n",i,values[i],isbounds);*/
  cmor_add_traceback("cmor_check_monotonic");

  refaxis = &cmor_tables[cmor_axes[axis_id].ref_table_id].axes[cmor_axes[axis_id].ref_axis_id];
  if ((refaxis->axis=='X')&& (strncmp(refaxis->units,"degrees",7)==0)) {
    treatlon=1;
  }
  /* ok ensure that values are monotonic */
  if (isbounds==1) {
    for (i=0;i<length/2-2;i++) {
      if (((values[2*i]-values[2*i+2])/(values[2*i+2]-values[2*i+4]))<0.) { 
	if ((refaxis->axis=='X') && (strncmp(refaxis->units,"degrees",7)==0)) {
	  treatlon=1;
	}
	else {
	  snprintf(msg,CMOR_MAX_STRING,"axis %s (table: %s) has non monotonic bounds values : %lf, %lf, %lf",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,values[2*i],values[2*i+2],values[2*i+4]);
	cmor_handle_error(msg,CMOR_CRITICAL);
	}
      }
    }
    /* printf("In is isbounds treatlon is: %i\n",treatlon); */
    if (treatlon) {/* ok we are dealing with a longitude need to figure out the offset.... */
      /* for (i=0;i<length;i++) printf("in monotonic: %i, %lf\n",i,values[i]); */
     /* The VERY first thing is to make sure we are modulo 360 */
      values2 = (double *) malloc(sizeof(double)*length);
      for (i=0;i<length;i++) {
	values2[i] = fmod(values[i], 360.);
      }

      /* for(i=0;i<length-1;i+=2) { */
      /* 	fprintf(stderr,"moduloed: %i: %lf - %lf\n",i,values2[i],values2[i+1]); */
      /* } */

      /* Now keep looping until we do not have up and downs */
      mono = -1;
      nloop=0;
      while (mono == -1 ) {
	mono=1;
	tmp=0;
	for (i=0;i<length-4;i++) {
	  tmp =  (values2[i]-values2[i+2])*(values2[i+2]-values2[i+4]);
	  if (tmp<0) break;
	}
	if (tmp<0) { /* ok we flip floppped */
	  tmp=values2[0];
	  for (i=0;i<length-1;i++) {
	    values2[i]=values2[i+1];
	  }
	  values2[i]=tmp;
	  mono=-1;
	  nloop+=1;
	  if (nloop==length) {
	    sprintf(msg,"longitude axis bounds are not monotonic, axis %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
	    cmor_handle_error(msg,CMOR_CRITICAL);
	}
	}
      }
      /* for(i=0;i<length-1;i+=2) { */
      /* 	fprintf(stderr,"ordered: %i: %lf - %lf\n",i,values2[i],values2[i+1]); */
      /* } */

      if (length>2) {
	if (values2[0]<values2[2]) {
	  /* first keep adding 360 until each value is greater than the previous */
	  for (i=0;i<length-2;i++) {
	    while (values[i+2]<values[i]) {
	      values[i+2]+=360.;
	    }
	  }
	}
	else {
	  /* first keep adding 360 until each value is greater than the previous */
	  for (i=0;i<length-2;i++) {
	    while (values[i+2]>values[i]) {
	      values[i+2]-=360.;
	    }
	  }
	}
      }
      free(values2);
    
      /* stored_direction*/
      /* printf("------length: %i, storeddir: %c, vlue-1, vlaue0: %lf and %lf\n",length,refaxis->stored_direction,values[length-1],values[0]); */

     if ((length>1) && (((refaxis->stored_direction=='i') && (values[length-1]<values[0])) || ((refaxis->stored_direction=='d') && (values[0]<values[length-1])))) { /* need to flip that axis */
	if (cmor_axes[axis_id].revert==1) {
	  snprintf(msg,CMOR_MAX_STRING, "bounds of axis %s (table: %s) need to be flipped but axis values did not need to. This is inconsistent",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	for (i=0;i<length/2;i++) {
	  tmp = values[i];
	  values[i] = values[length-1-i];
	  values[length-1-i] = tmp;
	}
      }

      /* for(i=0;i<length-1;i+=2) { */
      /* 	fprintf(stderr,"finally: %i: %lf - %lf\n",i,values[i],values[i+1]); */
      /* } */

      /* ok make sure we have data spanning only 1 modulo */
      if (abs(values[length-1]-values[0])>360.) {
	snprintf(msg,CMOR_MAX_STRING,"axis %s has bounds values spanning more 360 degrees %lf, %lf",name,values[0],values[length-1]);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      /* ok now check the monotonic again */
      for (i=0;i<length/2-2;i++) {
	if (((values[2*i]-values[2*i+2])/(values[2*i+2]-values[2*i+4]))<0.) { 
	  snprintf(msg,CMOR_MAX_STRING,"axis %s (table: %s), has really non monotonic bounds values : %lf, %lf, %lf",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,values[i],values[i+2],values[i+4]);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
      }
      /* First of all need to check if bounds needs to be flipped */
      j=1;
      for (i=0;i<length-2;i+=2) {
	if ((values[i]<values[i+1])&&(values[i]>values[i+2])) {
	  sprintf(msg,"Axis: '%s' (table: %s), your bounds direction seems to be decreasing, but within cell %i they are stored increasingly: you have [%lf, %lf], but the next set is: [%lf, %lf]",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,i,values[i],values[i+1],values[i+2],values[i+3]);
	  cmor_handle_error(msg,CMOR_WARNING);
	  j++;
	}
	if ((values[i]>values[i+1]) && (values[i]<values[i+2])) {
	  sprintf(msg,"Axis: '%s' (table: %s), your bounds direction seems to be increasing, but within cell %i they are stored decreasingly: you have [%lf, %lf], but the next set is: [%lf, %lf]",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,i,values[i],values[i+1],values[i+2],values[i+3]);
	  cmor_handle_error(msg,CMOR_WARNING);
	  j++;
	}
      }
      if (j==length/2) {
	for(i=0;i<length;i+=2) {
	  tmp=values[i];
	  values[i]=values[i+1];
	  values[i+1]=tmp;
	}
      }
      else if (j!=1) {
	sprintf(msg,"Some but not all of your longitude bounds need to be flipped, see warnings ot see which ones, axis: %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
    }
    
    
     /* do not do the following in case of climatological stuff.... */
    if (refaxis->climatology==0) {
      for (i=0;i<length-2;i++) {
	/* also check that bounds do not overlap */
	if (((values[i]<values[i+1]) && (values[i+2]<values[i+1])) || ((values[i]>values[i+1]) && (values[i+2]>values[i+1]))) {
	  snprintf(msg,CMOR_MAX_STRING,"axis %s (table: %s) has overlapping bounds values : %lf, %lf, %lf at index: %i",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,values[i],values[i+1],values[i+2],i);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
      }
      for (i=0;i<length-2;i=i+2) {
	if (values[i+1]!=values[i+2]) {
	  snprintf(msg,CMOR_MAX_STRING,"axis %s (table: %s) has bounds values that leave gaps (index %i): %lf, %lf, %lf",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,i,values[i],values[i+1],values[i+2]);
	  cmor_handle_error(msg,CMOR_WARNING);
	}
      }
    }
  }
  else {
    for (i=0;i<length-2;i++) {
      if (((values[i]-values[i+1])/(values[i+1]-values[i+2]))<0.) {
	if (refaxis->axis == 'X') { 
	  treatlon = 1;
	  break;
	}
	else {
	  snprintf(msg,CMOR_MAX_STRING,"axis %s (table: %s) has non monotonic values : %lf, %lf and  %lf",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,values[i],values[i+1],values[i+2]);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
      }
    }

    if (treatlon) {/* ok we are dealing with a longitude need to figure out the offset.... */

      /* The VERY first thing is to make sure we are modulo 360 */
      values2 = (double *) malloc(sizeof(double)*length);
      for (i=0;i<length;i++) {
	values2[i] = fmod(values[i], 360.);
      }

      /* Now keep looping until we do not have up and downs */
      mono = -1;
      nloop=0;
      while (mono == -1 ) {
	mono=1;
	tmp=0;
	for (i=0;i<length-2;i++) {
	  tmp =  (values2[i]-values2[i+1])*(values2[i+1]-values2[i+2]);
	  if (tmp<0) break;
	}
	if (tmp<0) { /* ok we flip floppped */
	  tmp=values2[0];
	  for (i=0;i<length-1;i++) {
	    values2[i]=values2[i+1];
	  }
	  values2[i]=tmp;
	  mono=-1;
	  nloop+=1;
	  if (nloop==length) {
	    sprintf(msg,"longitude axis is not monotonic (axis: %s, table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
	    cmor_handle_error(msg,CMOR_CRITICAL);
	}
	}
      }


      if (length>1) {
	if (values2[0]<values2[1]) {
	  /* first keep adding 360 until each value is greater than the previous */
	  for (i=0;i<length-1;i++) {
	    while (values[i+1]<values[i]) {
	      values[i+1]+=360.;
	    }
	  }
	}
	else {
	  /* first keep removing 360 until each value is lower than the previous */
	  for (i=0;i<length-1;i++) {
	    while (values[i+1]>values[i]) {
	      values[i+1]-=360.;
	    }
	  }
	}
      }
      free(values2);

      /* stored_direction*/
      /* printf("length: %i, storeddir: %c, vlue-1, vlaue0: %lf and %lf\n",length,refaxis->stored_direction,values[length-1],values[0]); */
      if ((length>1) && (((refaxis->stored_direction=='i') && (values[length-1]<values[0])) || ((refaxis->stored_direction=='d') && (values[0]<values[length-1])))) { /* need to flip that axis */
	if ((isbounds==1)  && (cmor_axes[axis_id].revert==1)) {
	  snprintf(msg,CMOR_MAX_STRING, "bounds of axis %s (table: %s), need to be flipped but axis values did not need to. This is inconsistent",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	cmor_axes[axis_id].revert=-1;
	for (i=0;i<length/2;i++) {
	  tmp = values[i];
	  values[i] = values[length-1-i];
	  values[length-1-i] = tmp;
	}
      }


     /* ok make sure we have data spanning only 1 modulo */
      if (abs(values[length-1]-values[0])>360.) {
	snprintf(msg,CMOR_MAX_STRING,"axis %s (table: %s) has values spanning more 360 degrees %lf, %lf",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,values[0],values[length-1]);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      /* ok now check the monotonic again */
      for (i=0;i<length-2;i++) {
	if (((values[i]-values[i+1])/(values[i+1]-values[i+2]))<0.) {
	  snprintf(msg,CMOR_MAX_STRING,"axis %s (table: %s) has non monotonic values : %lf, %lf and  %lf",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,values[i],values[i+1],values[i+2]);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
      }
    }
  }
  /* here we check if interval is about right */
  if ( (refaxis->axis=='T')) {
    /* do not do the following in case of climatological stuff.... */
    if (refaxis->climatology==0) {
      /* just keep the begining of units out no need to know the since part */
      j=0;
      while (refaxis->units[j]==' ') j++;
      i=0;
      while ((refaxis->units[i+j]!=' ') && (refaxis->units[i+j]!='\0')) {
	msg2[i]=refaxis->units[i+j];
	i++;
      }
      msg2[i]='\0';
      snprintf(msg,CMOR_MAX_STRING,"%lf %s",cmor_tables[cmor_axes[axis_id].ref_table_id].interval,msg2);
      /* printf("calling chck interv: %i\n",length); */
      /* for(i=0;i<length;i++) printf("i:%i ,val: %lf\n",i,values[i]); */
      /* ok skip this for non standard cal */
      
      i = cmor_check_interval(axis_id,msg,&values[0],length,isbounds);
    }
  }
  cmor_pop_traceback();
  return treatlon;
}


int cmor_treat_axis_values(int axis_id, double *values, int length, int n_requested, char *units, char *name, int isbounds){
  extern ut_system *ut_read;
  ut_unit *user_units, *cmor_units;
  cv_converter *ut_cmor_converter;
  char local_unit[CMOR_MAX_STRING];
  int i,j,treatlon=0;
  double tmp;
  char msg[CMOR_MAX_STRING];
  cdCalenType acal;
  double *tmplon;

  cmor_axis_def_t *refaxis;
  cmor_axis_t *axis;
  int found = 0;
  double eps,eps2;

  cmor_add_traceback("cmor_treat_axis_values");
  cmor_is_setup();

  axis = &cmor_axes[axis_id];
  refaxis = &cmor_tables[axis->ref_table_id].axes[axis->ref_axis_id];

  /* for (i=0;i<length;i++) printf("isbounds: %i, i: %i, value: %lf\n",isbounds,i,values[i]); */

  if (refaxis->axis=='T') {
    /*ok this part will try to convert time values to the right units */
    cmor_get_cur_dataset_attribute("calendar",&msg[0]);
    if (cmor_calendar_c2i(msg,&acal)!=0) {
      snprintf(msg,CMOR_MAX_STRING,"non-standard calendar... hum we will try to accomodate this later\n");
      cmor_handle_error(msg,CMOR_NORMAL);
      cmor_pop_traceback();
      return 1;
    }
/*     for(i=0;i<length;i++) printf("coming in values are: %i, %lf\n",i,values[i]); */
    cmor_convert_time_values(&values[0],'d',length,&values[0],units,refaxis->units,msg,msg);
/*     for(i=0;i<length;i++) printf("converted values are: %i, %lf\n",i,values[i]); */
  }
  else { /*ok using udunits to convert */
    strcpy(local_unit,units);
    ut_trim(local_unit,UT_ASCII);
    user_units = ut_parse(ut_read, local_unit, UT_ASCII);
    if (ut_get_status() != UT_SUCCESS ) {
      snprintf(msg,CMOR_MAX_STRING,"In udunits analyzing units from user (%s), axis %s (table: %s)",local_unit,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
      cmor_pop_traceback();
      return 1;
    }
    if (strcmp(refaxis->units,"?")==0) strcpy(local_unit,units);
    else strcpy(local_unit,refaxis->units);
    ut_trim(local_unit,UT_ASCII);
    cmor_units = ut_parse(ut_read, local_unit,UT_ASCII);
    if (ut_get_status() != UT_SUCCESS ) {
      snprintf(msg,CMOR_MAX_STRING,"In udunits analyzing table defined units (%s) for axis: %s (table: %s)",local_unit,refaxis->id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id );
      cmor_handle_error(msg,CMOR_CRITICAL);
      cmor_pop_traceback();
      return 1;
    }
    if (ut_are_convertible(cmor_units,user_units)==0 ) {
      snprintf(msg,CMOR_MAX_STRING,"axis %i (%s, table: %s): cmor and user units are incompatible: %s and %s",axis_id,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,refaxis->units,units);
      cmor_handle_error(msg,CMOR_CRITICAL);
      cmor_pop_traceback();
      return 1;
    }
    ut_cmor_converter=ut_get_converter(user_units,cmor_units);
    if (ut_get_status() != UT_SUCCESS ) {
      snprintf(msg,CMOR_MAX_STRING,"In udunits getting converter, for axis %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id );
      cmor_handle_error(msg,CMOR_CRITICAL);
      cmor_pop_traceback();
      return 1;
    }
    cv_convert_doubles(ut_cmor_converter,values,length,values);
    /*    cv_convert_doubles(ut_cmor_converter,&cmor_axes[cmor_naxes].values[0],length,&cmor_axes[cmor_naxes].values[0]); */
    if (ut_get_status() != UT_SUCCESS ) {
      snprintf(msg,CMOR_MAX_STRING,"In udunits converting values, for axis %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id );
      cmor_handle_error(msg,CMOR_CRITICAL);
      cmor_pop_traceback();
      return 1;
    }
    cv_free(ut_cmor_converter);
    if (ut_get_status() != UT_SUCCESS) {
      snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing converter, for axis %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
    
    ut_free(cmor_units);
    if (ut_get_status() != UT_SUCCESS) {
      snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units, for axis %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
    ut_free(user_units);
    if (ut_get_status() != UT_SUCCESS) {
      snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units, for axis %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
  }


  /* test for requested */
  /* ok is there some requested values ? */
  if (n_requested!=0) {
    for (j=0;j<n_requested;j++) {
      found=0;
      /* First test is it within .001 of req_values */
      eps = (double) fabs(1.e-3*refaxis->tolerance*refaxis->requested[j]);
/*       printf("epsilon  is: %lf, tol: %lf\n",eps,refaxis->tolerance); */
      if (j>0) {
	eps2 = (double)fabs(refaxis->requested[j]-refaxis->requested[j-1])*refaxis->tolerance;
/* 	printf("epsilon2 is: %lf\n",eps2); */
	if (eps2<eps) eps=eps2;
      }
      for (i=0;i<length;i++) {
	/* 	  printf("value %d is: %lf\n",i,values[i]); */
	if ((double)fabs(values[i]-refaxis->requested[j])<=eps) {
	  found = 1;
	  break;
	}
      }
      if (found==0) {
	snprintf(msg,CMOR_MAX_STRING,"requested value %f for axis %s (table: %s) was not found",refaxis->requested[j],name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
    }
  }
  

  /* stored_direction*/
  if ((length>1) && (((refaxis->stored_direction=='i') && (values[length-1]<values[0])) || ((refaxis->stored_direction=='d') && (values[0]<values[length-1])))) { /* need to flip that axis */
    if ((isbounds==1)  && (axis->revert==1)) {
      if (length>2) {
      snprintf(msg,CMOR_MAX_STRING, "bounds of axis %s (table: %s) need to be flipped but axis values did not need to. This is inconsistent",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
      }
    }
    axis->revert=-1;
    for (i=0;i<length/2;i++) {
      tmp = values[i];
      values[i] = values[length-1-i];
      values[length-1-i] = tmp;
    }
  }
  if (isbounds==1) {
    /* ok at that point we need to see if the bounds are stored in the correct first bound/second bounds order */
    if (refaxis->axis!='X') {
      if (refaxis->stored_direction=='i') { /* ok values are going incresingly */
	for (i=0;i<length;i=i+2) { 
	  if (values[i]>values[i+1]) { /* ok bounds are not stored first bound lower that second one */
	    tmp = values[i];
	    values[i] = values[i+1];
	    values[i+1]=tmp;
	  }
	}
      }
      else {
	for (i=0;i<length;i=i+2) { 
	  if (values[i]<values[i+1]) { /* ok bounds are not stored first bound greater that second one */
	    tmp = values[i];
	    values[i] = values[i+1];
	    values[i+1]=tmp;
	  }
	}
      }
    }
    /* need to check for requested bounds */
    if (refaxis->n_requested_bounds!=0) {
      /* ok let's loop thru it but basically we need to loop every over ones */
      for (j=0;j<refaxis->n_requested_bounds;j++) {
	found=0;
	/* First test is it within .001 of req_values */
	eps = (double) fabs(1.e-3*refaxis->tolerance*refaxis->requested_bounds[j]);
	if ((j % 2) == 0) {
	  eps2 = (double)fabs(refaxis->requested_bounds[j]-refaxis->requested_bounds[j+1])*refaxis->tolerance;
	}
	else {
	  eps2 = (double)fabs(refaxis->requested_bounds[j]-refaxis->requested_bounds[j-1])*refaxis->tolerance;
	}
/* 	printf("j, eps 2 is:%i, %lf\n",j,eps2); */
	if (eps2<eps) eps=eps2;
	if ((j%2)==0) {
	  for (i=0;i<length;i=i+2) { /* it is a req beg bounds need to test against beg ones only */
/* 	    printf("0req: %lf, val: %lf, eps: %lf, diff: %lf\n",refaxis->requested_bounds[j],values[i],eps,fabs(values[i]-refaxis->requested_bounds[j])); */
	    if ((double)fabs(values[i]-refaxis->requested_bounds[j])<=eps) {
	      found = 1;
	      break;
	    }
	  }
	}
	else {
	  for (i=1;i<length;i=i+2) { /* it is a second bounds, need to test against second nes only */
/* 	    printf("1req: %lf, val: %lf, eps: %lf, diff: %lf\n",refaxis->requested_bounds[j],values[i],eps,fabs(values[i]-refaxis->requested_bounds[j])); */
	    if ((double)fabs(values[i]-refaxis->requested_bounds[j])<=eps) {
	      found = 1;
	      break;
	    }
	  }
	}
	if (found==0) {
	  snprintf(msg,CMOR_MAX_STRING,"requested value %f for axis %s (table: %s) was not found",refaxis->requested_bounds[j],name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
      }
    }
    
  }

  /* is there any offsetting to be done ? */
  if ((axis->offset!=0) && (isbounds==1)) {
    tmplon = malloc(2*axis->offset*sizeof(double));
    for (i=0;i<2*axis->offset;i++) {
      tmplon[i]=values[i];
    }
    for (i=2*axis->offset;i<length;i++) {
      values[i-2*axis->offset]=values[i];
    }
    for (i=0;i<2*axis->offset;i++) {
      values[i+length-2*axis->offset]=tmplon[i];
    }
    free(tmplon);
  }
 
  i = cmor_check_monotonic(&values[0],length,name,isbounds,axis_id);


  /* now check for valid_min/max things */
  if (isbounds==0) {
    if (refaxis->valid_min!=1.e20) for (i=0;i<length;i++) if (values[i]<refaxis->valid_min) { 
      if (refaxis->axis=='X') {
	treatlon = 1;
      }
      else {
	snprintf(msg,CMOR_MAX_STRING,"axis %s (table: %s), detected value at: %f when valid_min is %f\n",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,values[i],refaxis->valid_min); 
	cmor_handle_error(msg,CMOR_NORMAL);
	cmor_pop_traceback();
	return 1;
      }
    }
    if (treatlon==1) {
      for (i=0;i<length;i++) { /* ok lets add 360. until it's all good */
	while (values[i]<refaxis->valid_min) values[i]+=360.;
      }
      /* ok now need to determine the offset */
      for (i=0;i<length-1;i++) {
	if (values[i]>values[i+1]) {
	  axis->offset = i+1; 
	  break;
	}
      }
    }
    treatlon=0;
    if (refaxis->valid_max!=1.e20) for (i=0;i<length;i++) if (values[i]>refaxis->valid_max) {
      if (refaxis->axis=='X') {
    	treatlon = 1;
      }
      else {
    	snprintf(msg,CMOR_MAX_STRING,"axis %s (table: %s) , detected value at: %f when valid_max is %f\n",name,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,values[i],refaxis->valid_max);
    	cmor_handle_error(msg,CMOR_NORMAL);
    	cmor_pop_traceback();
    	return 1;
      }
    }
    if (treatlon==1) {
      for (i=0;i<length;i++) { /* ok lets add 360. until it's all good */
    	while (values[i]>refaxis->valid_max) values[i]-=360.;
      }
      /* ok now need to determine the offset */
      for (i=0;i<length-1;i++) {
    	if (values[i]>values[i+1]) {
    	  axis->offset = i+1;
    	  break;
    	}
      }
    }
    /* ok now need to move the offset thing */
    if (axis->offset!=0) {
      if (isbounds==0) {
/* 	printf("ok unoffseted values are (axes): (offset is: %i) \n",axis->offset); */
/* 	for (i=0;i<length;i++) printf("%i : %f\n",i,values[i]); */
	tmplon = malloc(axis->offset*sizeof(double));
	for (i=0;i<axis->offset;i++) {
	  tmplon[i]=values[i];
	}
	for (i=axis->offset;i<length;i++) {
	  values[i-axis->offset]=values[i];
	}
	for (i=0;i<axis->offset;i++) {
	  values[i+length-axis->offset]=tmplon[i];
	}
	free(tmplon);
/* 	printf("ok offseted values are (axes): \n"); */
/* 	for (i=0;i<length;i++) printf("%i : %f\n",i,values[i]); */
      }
      else {
	tmplon = malloc(2*axis->offset*sizeof(double));
	for (i=0;i<2*axis->offset;i++) {
	  tmplon[i]=values[i];
	}
	for (i=2*axis->offset;i<length;i++) {
	  values[i-2*axis->offset]=values[i];
	}
	for (i=0;i<2*axis->offset;i++) {
	  values[i+length-2*axis->offset]=tmplon[i];
	}
	free(tmplon);
      }
    }
    i = cmor_check_monotonic(&values[0],length,name,isbounds,axis_id);

  }
  cmor_pop_traceback();
  return 0;
}

int cmor_check_interval(int axis_id, char *interval, double *values, int nvalues, int isbounds) 
{
  char ctmp[CMOR_MAX_STRING];
  char ctmp2[CMOR_MAX_STRING];
  char msg[CMOR_MAX_STRING];
  int i,j,n,nval;
  double interv,diff,diff2,tmp;
  extern ut_system *ut_read;
  ut_unit *user_units=NULL, *cmor_units=NULL;
  cv_converter *ut_cmor_converter=NULL;
  double *tmp_values=NULL;
  cmor_axis_def_t *refaxis;
  cmor_axis_t *axis;
  cdCalenType icali;
  cdCompTime comptime;

  cmor_add_traceback("cmor_check_interval"); 
  axis = &cmor_axes[axis_id];
  refaxis = &cmor_tables[axis->ref_table_id].axes[axis->ref_axis_id];

  if (isbounds==0) {
    nval = nvalues;
    tmp_values=values;
  }
  else {
    nval = nvalues/2+1;
    tmp_values = malloc(sizeof(double)*nval);
    if (tmp_values==NULL) {
      snprintf(ctmp,CMOR_MAX_STRING,"Error allocating memory for %i values in check_interval (%s), axis: %s (table: %s)",nval,interval,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
      cmor_handle_error(ctmp,CMOR_CRITICAL);
    }
    for (i=0;i<nval-1;i++) tmp_values[i] = values[i*2];
    tmp_values[nval-1] = values[nvalues-1];
  }
  /* first of all need to figure out the numeric and unit part of the interval */
  cmor_trim_string(interval,ctmp);
  n = strlen(ctmp);
  for (i=0;i<n;i++) {
    if (ctmp[i]==' ') {
      ctmp2[i]='\0';
      break;
    }
    ctmp2[i]=ctmp[i];
  }
  interv = atof(ctmp2);
  for(j=0;j<n-i;j++) {
    ctmp2[j]=ctmp[j+i+1];
  }
  ctmp2[j]='\0';
  /* ok at this point we need to convert this in some base units: seconds */
  sprintf(msg,"seconds");
  ut_trim(msg,UT_ASCII);
  cmor_units = ut_parse(ut_read, msg,UT_ASCII);
  user_units = ut_parse(ut_read, ctmp2, UT_ASCII);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"In udunuits parsing user units: %s, axis: %s (table: %s)",ctmp2,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  if (ut_are_convertible(cmor_units,user_units)==0 ) {
    snprintf(ctmp,CMOR_MAX_STRING,"axis interval units (%s) are incompatible with seconds, axis: %s (table: %s)",ctmp2,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
    cmor_handle_error(ctmp,CMOR_CRITICAL);
    cmor_pop_traceback();
    return 1;
  }
  ut_cmor_converter=ut_get_converter(user_units,cmor_units);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"In udunuits getting converter, axis: %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  tmp = cv_convert_double(ut_cmor_converter,interv);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"In udunuits converting, axis: %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  interv = tmp;
  cv_free(ut_cmor_converter);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing converter, axis: %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  ut_free(user_units);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units, axis: %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  
  /* at this point we need to figure out the axis units interval */
  strcpy(ctmp,refaxis->units);
  n = strlen(ctmp);
  for (i=0;i<n;i++) {
    if (ctmp[i]==' ') {
      ctmp2[i]='\0';
      break;
    }
    ctmp2[i]=ctmp[i];
  }
  user_units = ut_parse(ut_read, ctmp2, UT_ASCII);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"In udunits parsing user units: %s, axis: %s (table: %s)",ctmp2,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  if (ut_are_convertible(cmor_units,user_units)==0 ) {
    snprintf(ctmp,CMOR_MAX_STRING,"axis interval units (%s) are incompatible with seconds, axis: %s (table: %s)",ctmp2,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
    cmor_handle_error(ctmp,CMOR_CRITICAL);
    cmor_pop_traceback();
    return 1;
  }
  ut_cmor_converter=ut_get_converter(user_units,cmor_units);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"Udunits: Error getting converter from %s to %s, axis: %s (table: %s)",ctmp2,msg,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  tmp=0.;
  for (i=0;i<nval-1;i++) {
    diff = tmp_values[i+1]-tmp_values[i]; /* still in user units */
    /* now converts to seconds */
    tmp = cv_convert_double(ut_cmor_converter,diff);
    if (ut_get_status() != UT_SUCCESS) {
      snprintf(msg,CMOR_MAX_STRING,"In udunits converting, axis: %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
    diff2 = tmp;
    tmp = (double)fabs(diff2-interv);
    tmp = tmp/interv;
    if (tmp>cmor_tables[cmor_axes[axis_id].ref_table_id].interval_error) { /* more than 20% diff issues an error */
      if (isbounds==1) {
	snprintf(ctmp,CMOR_MAX_STRING,"approximate time axis interval is defined as %f seconds (%s), for value %i we got a difference (based on bounds) of %f seconds, (%f %s), which is %f %% , seems too big, check your values", interv, interval, i+1, diff2,diff, ctmp2,tmp*100.);
      }
      else {
	if (isbounds==1) {
	  snprintf(ctmp,CMOR_MAX_STRING,"approximate time axis interval is defined as %f seconds (%s), for value %i we got a difference (based on bounds) of %f seconds (%f %s), which is %f %% , seems too big, check your values", interv, interval, i+1, diff2, diff, ctmp2, tmp*100.);
	}
	else {
	  snprintf(ctmp,CMOR_MAX_STRING,"approximate time axis interval is defined as %f seconds (%s), for value %i we got a difference of %f seconds (%f %s), which is %f %% , seems too big, check your values", interv, interval, i+1, diff2, diff, ctmp2, tmp*100.);
	}
      }
      cmor_handle_error(ctmp,CMOR_CRITICAL);
    }
    else if (tmp>cmor_tables[cmor_axes[axis_id].ref_table_id].interval_warning) { /* more than 10% diff issues a warning */
      snprintf(ctmp,CMOR_MAX_STRING,"approximate time axis interval is defined as %f seconds (%s), for value %i we got a difference of %f seconds (%f %s), which is %f %% , seems too big, check your values", interv, interval, i+1, diff2, diff, ctmp2,tmp*100.);
      cmor_handle_error(ctmp,CMOR_WARNING);
    }
  }

  /* ok here we test for bounds being at begining and end of the month */
  if ((isbounds==1) && (fabs(interv-2592000.)/2592000. < .1)) {
    cmor_get_cur_dataset_attribute("calendar",ctmp);
    if (cmor_calendar_c2i(ctmp,&icali)!=0) {
      snprintf(ctmp,CMOR_MAX_STRING,"Cannot convert times for calendar: %s",ctmp);
      cmor_handle_error(ctmp,CMOR_NORMAL);
      cmor_pop_traceback();
      return 1;
    }
    for (i=0;i<nvalues;i++) {
      cmor_convert_time_units(axis->iunits,cmor_tables[axis->ref_table_id].axes[axis->ref_axis_id].units,ctmp);
      cdRel2Comp(icali,ctmp,values[i],&comptime);
      if (comptime.day!=1) {
	snprintf(ctmp,CMOR_MAX_STRING,"Bounds value %ld-%d-%d is not beg or end of month and you seem to be writing monthly data, please check",comptime.year,comptime.month,comptime.day);
	cmor_handle_error(ctmp,CMOR_WARNING);
      }
    }
  }
  cv_free(ut_cmor_converter);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing converter, axis: %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  
  ut_free(cmor_units);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units, axis: %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  ut_free(user_units);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units, axis: %s (table: %s)",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  if (isbounds == 1) free(tmp_values);
  cmor_pop_traceback();
  return 0;
}

int cmor_axis(int *axis_id, char *name,char *units, int length,void *coord_vals, char type, void *cell_bounds,int cell_bounds_ndim,char *interval) 
{
  extern int cmor_naxes;
  extern int CMOR_TABLE;

  int i,iref,j,ierr,k,l;
  cmor_axis_def_t refaxis;
  char msg[CMOR_MAX_STRING];
  char ctmp[CMOR_MAX_STRING];
  
  cmor_add_traceback("cmor_axis");
  cmor_is_setup();

  if (CMOR_TABLE==-1) {
    cmor_handle_error("You did not define a table yet!",CMOR_CRITICAL);
  }

  if (cmor_naxes==CMOR_MAX_AXES-1) { 
    cmor_handle_error("Too many axes defined",CMOR_CRITICAL);
    cmor_pop_traceback();
    return 1;
  }

  /* printf("cell bnds ndims: %i\n",cell_bounds_ndim); */
  /* if (cell_bounds_ndim==1) { */
  /*   for (i=0;i<length;i++) { */
  /*     printf("%lf < %lf < %lf\n",(double)((double *)cell_bounds)[i],(double)((double *)coord_vals)[i],(double)((double *)cell_bounds)[i+1]); */
  /*   } */
  /* } */
   /* for (i=0;i<length;i++) {printf("ok in cmor_axsi we have: %i, %lf, %lf, %lf\n",i,(double)((double *)coord_vals)[i],(double)((double *)cell_bounds)[2*i],(double)((double *)cell_bounds)[2*i+1]);} */

  cmor_naxes+=1;
  strncpytrim(cmor_axes[cmor_naxes].id,name,CMOR_MAX_STRING);
  /* ok now look which axis is corresponding in table if not found then error */
  iref=-1;
  cmor_trim_string(name,msg);
  for (i=0;i<=cmor_tables[CMOR_TABLE].naxes;i++) if (strcmp(cmor_tables[CMOR_TABLE].axes[i].id,msg)==0) { iref=i;break;}
  if (iref==-1) {
    snprintf(ctmp,CMOR_MAX_STRING,"Could not find a matching axis for name: '%s'\n",msg);
    cmor_handle_error(ctmp,CMOR_CRITICAL);
  }
  /*printf("ok your axis is actually axis %i in table %i\n",iref,CMOR_TABLE);*/
  refaxis=cmor_tables[CMOR_TABLE].axes[iref];
  cmor_axes[cmor_naxes].ref_table_id=CMOR_TABLE;
  cmor_axes[cmor_naxes].ref_axis_id=iref;

  cmor_axes[cmor_naxes].length=length;
  cmor_axes[cmor_naxes].type=type;
  cmor_axes[cmor_naxes].store_in_netcdf=1;

  /* attributes settings */
  if (refaxis.axis=='T') {
    cmor_get_cur_dataset_attribute("calendar",ctmp);
    cmor_set_axis_attribute(cmor_naxes,"calendar",'c',ctmp);
    cmor_convert_time_units(units,refaxis.units,&ctmp[0]);
    strncpytrim(cmor_axes[cmor_naxes].iunits,units,CMOR_MAX_STRING);
  }
  else strcpy(ctmp,refaxis.units);
  ierr = cmor_set_axis_attribute(cmor_naxes,"units",'c',ctmp);
  ctmp[0]=refaxis.axis; 
  ctmp[1]='\0';
  cmor_axes[cmor_naxes].axis=refaxis.axis;
/*   printf("axis %s ref axis convert: %s\n",cmor_axes[cmor_naxes].id,refaxis.convert_to); */
  if (refaxis.axis!='\0') ierr = cmor_set_axis_attribute(cmor_naxes,"axis",'c',ctmp);
  if (refaxis.positive!='\0') {
    if (refaxis.positive=='u') cmor_set_axis_attribute(cmor_naxes,"positive",'c',"up");
    else if (refaxis.positive=='d') cmor_set_axis_attribute(cmor_naxes,"positive",'c',"down");
  }
  if (refaxis.long_name[0]!='\0') ierr = cmor_set_axis_attribute(cmor_naxes,"long_name",'c',refaxis.long_name);
  if (refaxis.standard_name[0]!='\0') ierr = cmor_set_axis_attribute(cmor_naxes,"standard_name",'c',refaxis.standard_name);
  if (refaxis.formula[0]!='\0') ierr = cmor_set_axis_attribute(cmor_naxes,"formula",'c',refaxis.formula);
  if (refaxis.z_factors[0]!='\0') ierr = cmor_set_axis_attribute(cmor_naxes,"z_factors",'c',refaxis.z_factors);
  if ((refaxis.z_bounds_factors[0]!='\0') && (cell_bounds!=NULL)) ierr = cmor_set_axis_attribute(cmor_naxes,"z_bounds_factors",'c',refaxis.z_bounds_factors);

/*   printf("comparing %s\n",cmor_axes[cmor_naxes].id); */
  if (strcmp(cmor_axes[cmor_naxes].id,"standard_sigma")==0) cmor_axes[cmor_naxes].hybrid_in=3;
  if (strcmp(cmor_axes[cmor_naxes].id,"standard_hybrid_sigma")==0) cmor_axes[cmor_naxes].hybrid_in=1;
  if (strcmp(cmor_axes[cmor_naxes].id,"alternate_hybrid_sigma")==0) cmor_axes[cmor_naxes].hybrid_in=2;
/*   printf("we got hybrid in setto : %i\n",cmor_axes[cmor_naxes].hybrid_in); */
  if (refaxis.convert_to[0]!='\0') {
    if (strcmp(refaxis.convert_to,name)!=0) {
      if (strcmp(refaxis.convert_to,"standard_hybrid_sigma")==0) {
	/* remembers what the original type was */
	i = cmor_axes[cmor_naxes].hybrid_in;
	if ((strcmp(name,"standard_hybrid_sigma")!=0) && (strcmp(name,"alternate_hybrid_sigma")!=0) && (strcmp(name,"standard_sigma")!=0)) {
	  snprintf(msg,CMOR_MAX_STRING,"axis: %s (table: %s) converting to \"standard_hybrid_sigma\" from unknown type: %s",cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id,name);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	/* printf("yep we are copnverting to: %s\n",refaxis.convert_to); */
	ierr = cmor_axis(axis_id,refaxis.convert_to,units,length,coord_vals,type,cell_bounds,cell_bounds_ndim,interval); 
	cmor_axes[cmor_naxes].hybrid_in=i; 
	cmor_axes[cmor_naxes].hybrid_out=1;
	*axis_id = cmor_naxes;
	cmor_pop_traceback();
	return 0;
      }
      else {
	snprintf(msg,CMOR_MAX_STRING,"axis: %s (table: %s) is declared to be converted to unknown type: %s",cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id,refaxis.convert_to);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
    }
  }
  else {
    cmor_axes[cmor_naxes].hybrid_out=cmor_axes[cmor_naxes].hybrid_in;
  }

  /* interval attribute */
  if ((interval!=NULL) && (interval[0]!='\0')) {
    ierr = cmor_set_axis_attribute(cmor_naxes,"interval",'c',interval);
  }
  
  /* test whether bounds are requested or not */
  if ((cell_bounds==NULL) && (refaxis.must_have_bounds==1)) {
    if (refaxis.axis!='T') {
      snprintf(msg,CMOR_MAX_STRING,"axis: %s (table: %s) must have bounds, you did not pass any when creating it via cmor_axis",cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
    else if (coord_vals!=NULL) { /* we passed time values that means we do not intend to pass them later */
      snprintf(msg,CMOR_MAX_STRING,"axis: %s (table: %s) must have bounds, you did not pass any when creating it via cmor_axis",cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
  }
  if (coord_vals!=NULL) { /* user passed coords need to convert this guys */
    if (type!='c') {
      cmor_axes[cmor_naxes].values=malloc(length*sizeof(double));
      if ( cmor_axes[cmor_naxes].values == NULL ) {
	snprintf(msg,CMOR_MAX_STRING,"cannot allocate memory for %i double elts for axis %s (table: %s)",length,cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      if (type=='f') for (i=0;i<length;i++) cmor_axes[cmor_naxes].values[i] = (double)((float *)coord_vals)[i];
      else if (type=='l') for (i=0;i<length;i++) cmor_axes[cmor_naxes].values[i] = (double)((long *)coord_vals)[i];
      else if (type=='i') for (i=0;i<length;i++) cmor_axes[cmor_naxes].values[i] = (double)((int *)coord_vals)[i];
      else if (type=='d') for (i=0;i<length;i++) cmor_axes[cmor_naxes].values[i] = (double)((double *)coord_vals)[i];

/*       for(i=0;i<length;i++) printf("ok we received: %i:%lf\n",i,cmor_axes[cmor_naxes].values[i]); */
    }
    else {
      /* before we do anything let check that output type is also character */
      if (type!=refaxis.type) {
	snprintf(msg,CMOR_MAX_STRING,"You are trying to define axis %s (table: %s) as character when it should be of type: %c\n",cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id,refaxis.type);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      cmor_axes[cmor_naxes].cvalues=malloc(length*sizeof(char *));
      if (cmor_axes[cmor_naxes].cvalues == NULL)  {
	snprintf(msg,CMOR_MAX_STRING,"cannot allocate memory for %i char elts for axis %s (table: %s)",length,cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      if (cell_bounds_ndim==0) k=CMOR_MAX_STRING;
      else k=cell_bounds_ndim;
      for (i=0;i<length;i++) {
	j = strlen(&((char *)coord_vals)[i*k]);
	if (j>k) j=k;
	cmor_axes[cmor_naxes].cvalues[i]=malloc((j+1)*sizeof(char));
	if (cmor_axes[cmor_naxes].cvalues[i] == NULL )   {
	  snprintf(msg,CMOR_MAX_STRING,"cannot allocate memory for %i char elts for axis %s (table: %s)",j+1,cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	strncpy(cmor_axes[cmor_naxes].cvalues[i],&((char *)coord_vals)[i*k],j);
	cmor_axes[cmor_naxes].cvalues[i][j]='\0';
      }
      /* we don't want to do the bounds stuff */
      /* but we need to look at the requested thing! */
      if (refaxis.crequested!=NULL) {
	i=0;
	j=strlen(&refaxis.crequested[0]);
	for (i=0;i<j;i++) {
	  k=0;
	  while ((refaxis.crequested[i]!=' ') && (refaxis.crequested[i]!='\0')) {ctmp[k]=refaxis.crequested[i];i++;k++;}
	  ctmp[k]='\0';
	  k=-1;
	  for (l=0;l<length;l++) {
/* 	    printf("comparing: %s and %s\n",ctmp,cmor_axes[cmor_naxes].cvalues[l]); */
	    if (strncmp(ctmp,cmor_axes[cmor_naxes].cvalues[l],strlen(ctmp))==0) k=l;
	  }
	  if (k==-1) {
	    snprintf(msg,CMOR_MAX_STRING,"Requested region for axis '%s' (table: %s) is not passed: '%s'",cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id,ctmp);
	    cmor_handle_error(msg,CMOR_CRITICAL);
	  }
	}
      }
      if (refaxis.out_name[0]!='\0') strcpy(cmor_axes[cmor_naxes].id,refaxis.out_name);
      *axis_id=cmor_naxes;
      cmor_pop_traceback();
      return 0;
    }

    /* printf("Ok is this the problematic place?\n"); */
    ierr = cmor_treat_axis_values(cmor_naxes, &cmor_axes[cmor_naxes].values[0],length,refaxis.n_requested, units, name, 0 );
    /* printf("nope\n"); */
    /* puts bounds on 2d array */
    if ((cell_bounds!=NULL) && (cell_bounds_ndim!=0)) {
      cmor_axes[cmor_naxes].bounds=malloc(2*length*sizeof(double));
      if (cmor_axes[cmor_naxes].bounds == NULL)   {
	snprintf(msg,CMOR_MAX_STRING,"cannot allocate memory for %i bounds elts for axis %s (table: %s)",2*length,cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      if (cell_bounds_ndim == 2) {
	if (type=='f') for (i=0;i<length;i++) { cmor_axes[cmor_naxes].bounds[2*i]=(double)((float *)cell_bounds)[2*i];cmor_axes[cmor_naxes].bounds[2*i+1]=(double)((float *)cell_bounds)[2*i+1]; }
	else if (type=='d') for (i=0;i<length;i++) { cmor_axes[cmor_naxes].bounds[2*i]=(double)((double *)cell_bounds)[2*i];cmor_axes[cmor_naxes].bounds[2*i+1]=(double)((double *)cell_bounds)[2*i+1]; }
	else if (type=='l') for (i=0;i<length;i++) { cmor_axes[cmor_naxes].bounds[2*i]=(double)((long *)cell_bounds)[2*i];cmor_axes[cmor_naxes].bounds[2*i+1]=(double)((long *)cell_bounds)[2*i+1]; }
	else if (type=='i') for (i=0;i<length;i++) { cmor_axes[cmor_naxes].bounds[2*i]=(double)((int *)cell_bounds)[2*i];cmor_axes[cmor_naxes].bounds[2*i+1]=(double)((int *)cell_bounds)[2*i+1]; }
	else { /* ??? charcter axis code here */ snprintf(msg,CMOR_MAX_STRING,"CMOR cannot handle axes of type %c please change type, axis: %s (table: %s)",type,cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id); cmor_handle_error(msg,CMOR_CRITICAL);}
      }
      else if (cell_bounds_ndim == 1) {
	if (type=='f') for (i=0;i<length;i++) { cmor_axes[cmor_naxes].bounds[2*i]=(double)((float *)cell_bounds)[i]; cmor_axes[cmor_naxes].bounds[2*i+1]=(double)((float *)cell_bounds)[i+1]; }
	else if (type=='d') for (i=0;i<length;i++) {cmor_axes[cmor_naxes].bounds[2*i]=(double)((double *)cell_bounds)[i]; cmor_axes[cmor_naxes].bounds[2*i+1]=(double)((double *)cell_bounds)[i+1]; }
	else if (type=='i') for (i=0;i<length;i++) { cmor_axes[cmor_naxes].bounds[2*i]=(double)((int *)cell_bounds)[i]; cmor_axes[cmor_naxes].bounds[2*i+1]=(double)((int *)cell_bounds)[i+1]; }
	else if (type=='l') for (i=0;i<length;i++) { cmor_axes[cmor_naxes].bounds[2*i]=(double)((long *)cell_bounds)[i]; cmor_axes[cmor_naxes].bounds[2*i+1]=(double)((long *)cell_bounds)[i+1]; }
	else { /* ??? charcter axis code here */ snprintf(msg,CMOR_MAX_STRING,"CMOR cannot handle axes of type %c please change type, axis: %s (table: %s)",type,cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id); cmor_handle_error(msg,CMOR_CRITICAL);}
      }
      /* for (i=0;i<length;i++) printf("bounds: %i -> %lf,%lf\n",i,cmor_axes[cmor_naxes].bounds[2*i],cmor_axes[cmor_naxes].bounds[2*i+1]); */
      ierr = cmor_treat_axis_values(cmor_naxes, &cmor_axes[cmor_naxes].bounds[0],2*length,0,units, name, 1 );
      /* printf("nope again\n"); */
      /* At this point we are checking that the axis values are within bounds */
      /* for (i=0;i<length;i++) printf("check bounds: %i -> %lf,%lf,%lf\n",i,cmor_axes[cmor_naxes].bounds[2*i],cmor_axes[cmor_naxes].values[i],cmor_axes[cmor_naxes].bounds[2*i+1]); */
      ierr = cmor_check_values_inside_bounds(&cmor_axes[cmor_naxes].values[0],&cmor_axes[cmor_naxes].bounds[0], length, name);
      if ((refaxis.axis=='T')&&(refaxis.climatology==0)) {
	/* ok now we need to overwrite the time values with mid point */
	for (i=0;i<length;i++) cmor_axes[cmor_naxes].values[i]=(cmor_axes[cmor_naxes].bounds[2*i]+cmor_axes[cmor_naxes].bounds[2*i+1])/2.;
      }
    }
  }
  else {
    if ((refaxis.axis!='T') && (refaxis.index_only=='n')) {
      snprintf(msg,CMOR_MAX_STRING,"function called for axis '%s' (table: %s) w/o any values",cmor_axes[cmor_naxes].id,cmor_tables[CMOR_TABLE].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
    }
    if (refaxis.index_only!='n') {
      /* it is an index no need to store values */
      cmor_axes[cmor_naxes].store_in_netcdf=0;
    }
  }

  if (refaxis.out_name[0]!='\0') strcpy(cmor_axes[cmor_naxes].id,refaxis.out_name);
  *axis_id=cmor_naxes;

  cmor_pop_traceback();
  return 0;
};

void cmor_init_axis_def(cmor_axis_def_t *axis, int table_id)
{
  cmor_is_setup();
  axis->table_id=table_id;
  axis->climatology=0;
  axis->standard_name[0]='\0';
  axis->units[0]='\0';
  axis->axis='\0';
  axis->positive='\0';
  axis->long_name[0]='\0';
  axis->out_name[0]='\0';
  axis->type='d';
  axis->stored_direction='i';
  axis->valid_min=1.e20; /* means no check */
  axis->valid_max=1.e20;
  if (axis->requested!=NULL) free(axis->requested);
  axis->requested=NULL;
  if (axis->requested_bounds!=NULL) free(axis->requested_bounds);
  axis->requested_bounds=NULL;
  axis->tolerance=1.e-3;
  axis->value=1.e20;
  axis->cvalue[0]='\0';
  axis->bounds_value[0]=1.e20;
  axis->bounds_value[1]=1.e20;
  axis->convert_to[0]='\0';
  axis->formula[0]='\0';
  axis->z_factors[0]='\0';
  axis->z_bounds_factors[0]='\0';
  if (axis->crequested!=NULL) free(axis->requested);
  axis->crequested=NULL;
  axis->cname[0]='\0';
  if (axis->requested_bounds!=NULL) free(axis->requested_bounds);
  axis->requested=NULL;
  axis->n_requested=0;
  axis->n_requested_bounds=0;
  axis->index_only='n';
  axis->must_have_bounds=0;
  axis->must_call_cmor_grid=0;
}

int cmor_set_axis_def_att(cmor_axis_def_t *axis,char att[CMOR_MAX_STRING],char val[CMOR_MAX_STRING] ){
  int i,n,j;
  char dim[CMOR_MAX_STRING];
  char msg[CMOR_MAX_STRING];
  double vals[CMOR_MAX_ELEMENTS];
  double *tmp=NULL;
  
  cmor_add_traceback("cmor_set_axis_def_att");
  cmor_is_setup();
/*   printf("setting %s to %s on axis %s\n",att,val,axis->id); */
  if (strcmp(att,"required")==0) {
    strcpy(axis->required,att);
  }
  else if (strcmp(att,"id")==0) {
    strcpy(axis->id,val);
  }
  else if (strcmp(att,"climatology")==0) {
    if (strcmp(val,"yes")==0) axis->climatology=1;
  }
  else if (strcmp(att,"out_name")==0) {
    strcpy(axis->out_name,val);
  }
  else if (strcmp(att,"standard_name")==0) {
    strcpy(axis->standard_name,val);
  }
  else if (strcmp(att,"long_name")==0) {
    strcpy(axis->long_name,val);
  }
  else if (strcmp(att,"convert_to")==0) {
    strcpy(axis->convert_to,val);
  }
  else if (strcmp(att,"formula")==0) {
    strcpy(axis->formula,val);
  }
  else if (strcmp(att,"z_factors")==0) {
    strcpy(axis->z_factors,val);
  }
  else if (strcmp(att,"z_bounds_factors")==0) {
    strcpy(axis->z_bounds_factors,val);
  }
  else if (strcmp(att,"units")==0) {
    strncpy(axis->units,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"stored_direction")==0) {
    if (val[0]=='i') axis->stored_direction='i';
    else if (val[0]=='d') axis->stored_direction='d';
  }
  else if (strcmp(att,"positive")==0) {
    axis->positive=val[0];
  }
  else if (strcmp(att,"axis")==0) {
    axis->axis=val[0];
  }
  else if (strcmp(att,"index_only")==0) {
    axis->index_only=val[0];
  }
  else if (strcmp(att,"must_have_bounds")==0) {
    if (strcmp(val,"yes")==0) axis->must_have_bounds=1;
  }
  else if (strcmp(att,"must_call_cmor_grid")==0) {
    if (strcmp(val,"yes")==0) axis->must_call_cmor_grid=1;
  }
  else if (strcmp(att,"type")==0) {
    if (strcmp(val,"real")==0) axis->type='f';
    else if (strcmp(val,"double")==0) axis->type='d';
    else if (strcmp(val,"integer")==0) axis->type='i';
    else if (strcmp(val,"long")==0) axis->type='l';
    else if (strcmp(val,"character")==0) axis->type='c';
  }
  else if (strcmp(att,"valid_min")==0) {
    axis->valid_min=atof(val);
  }
  else if (strcmp(att,"valid_max")==0) {
    axis->valid_max=atof(val);
  }
  else if (strcmp(att,"tolerance")==0) {
    axis->tolerance = atof(val);
  }
  else if (strcmp(att,"tol_on_requests")==0) {
    axis->tolerance = atof(val);
  }
  else if (strcmp(att,"value")==0) {
    strncpytrim(axis->cvalue,val,CMOR_MAX_STRING);
    axis->value = atof(val);
  }
  else if (strcmp(att,"bounds_values")==0) {
    sscanf(val,"%lf %lf",&axis->bounds_value[0],&axis->bounds_value[1]);
  }
  else if (strcmp(att,"coords_attrib")==0) {
    strncpytrim(axis->cname,val,CMOR_MAX_STRING);
  }
  else if ((strcmp(att,"bounds_requested")==0) || (strcmp(att,"requested_bounds")==0)) { /* requested values for axis */
    dim[0]='\0';
    n=0;
    for(i=0;i<strlen(val);i++) {
      j=i;
      while(((val[i]!=' ')&&(val[i]!='\0')) && (i<strlen(val))) {
	dim[i-j]=val[i];
	i++;
      }
      dim[i-j]='\0';
      sscanf(dim,"%lf",&vals[n]);
      while((val[i]==' ') && (val[i]!='\0')) {
	i++;
      }
      i--;
      n++;
    }
    if (axis->n_requested_bounds!=0) { /* ok we already had some read in need to memorize it */
      if (axis->requested_bounds == NULL)  {
	snprintf(msg,CMOR_MAX_STRING,"axis (%s, table: %s): looks like we already read %d requested bounds but they are not stored in the internal tables, maybe some bad cleanup",axis->id, cmor_tables[axis->table_id].table_id,axis->n_requested_bounds);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      tmp=malloc(axis->n_requested_bounds*sizeof(double));
      if (tmp == NULL)  {
	snprintf(msg,CMOR_MAX_STRING,"axis (%s, table: %s): cannot allocate memory for %i requested bounds elts for axis %s",axis->id, cmor_tables[axis->table_id].table_id,axis->n_requested,axis->id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      for (i=0;i<axis->n_requested_bounds;i++) {
	tmp[i]=axis->requested_bounds[i];
      }
      free(axis->requested_bounds);
      axis->n_requested_bounds+=n;
    }
    else {
      axis->n_requested_bounds=n;
    }
    axis->requested_bounds=malloc(axis->n_requested_bounds*sizeof(double));
    if (axis->requested_bounds == NULL)  {
      snprintf(msg,CMOR_MAX_STRING,"axis (%s, table: %s): cannot allocate memory for %i requested bounds elts for axis %s",axis->id, cmor_tables[axis->table_id].table_id,axis->n_requested,axis->id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
    for (i=0;i<n;i++) {  axis->requested_bounds[axis->n_requested_bounds-n+i]=vals[i];}
    if (tmp!=NULL) { /* ok we had previously read in data need to add this */
      for (i=0;i<axis->n_requested_bounds-n;i++) {
	axis->requested_bounds[i]=tmp[i];
      }
      free(tmp); /* we don't need this any longer */
      tmp=NULL;
    }
  }
  else if (strcmp(att,"requested")==0) { /* requested values for axis */
    if (axis->type!='c') {
      dim[0]='\0';
      n=0;
      for(i=0;i<strlen(val);i++) {
	j=i;
	while(((val[i]!=' ')&&(val[i]!='\0')) && (i<strlen(val))) {
	  dim[i-j]=val[i];
	  i++;
	}
	dim[i-j]='\0';
	sscanf(dim,"%lf",&vals[n]);
	while((val[i]==' ') && (val[i]!='\0')) {
	  i++;
	}
	i--;
	n++;
      }
      if (axis->n_requested!=0) { /* ok we already had some read in need to memorize it */
	if (axis->requested == NULL)  {
	  snprintf(msg,CMOR_MAX_STRING,"axis (%s, table: %s): looks like we already read %d requested values but they are not stored in the internal tables, maybe some bad cleanup",axis->id, cmor_tables[axis->table_id].table_id,axis->n_requested);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	tmp=malloc(axis->n_requested*sizeof(double));
	if (tmp == NULL)  {
	  snprintf(msg,CMOR_MAX_STRING,"axis (%s, table: %s): cannot allocate memory for %i requested elts for axis %s",axis->id, cmor_tables[axis->table_id].table_id,axis->n_requested,axis->id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	for (i=0;i<axis->n_requested;i++) {
	  tmp[i]=axis->requested[i];
	}
	free(axis->requested);
	axis->n_requested+=n;
      }
      else {
	axis->n_requested=n;
      }
      axis->requested=malloc(axis->n_requested*sizeof(double));
      if (axis->requested == NULL)  {
	snprintf(msg,CMOR_MAX_STRING,"axis (%s, table: %s): cannot allocate memory for %i requested elts for axis %s",axis->id, cmor_tables[axis->table_id].table_id,axis->n_requested,axis->id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      for (i=0;i<n;i++) { /* printf("requested: %i : %f\n",i,vals[i]); */ axis->requested[i]=vals[i];}
      if (tmp!=NULL) { /* ok we had previously read in data need to add this */
	for (i=0;i<axis->n_requested-n;i++) {
	  axis->requested[i+n]=tmp[i];
	}
	free(tmp); /* we don't need this any longer */
	tmp=NULL;
      }
    }
    else {
      axis->n_requested=1;
      axis->crequested=malloc((strlen(val)+1)*sizeof(char));
      if (axis->crequested == NULL ) {
	snprintf(msg,CMOR_MAX_STRING,"axis (%s, table: %s): cannot allocate memory for %ld requested char elts for axis %s",axis->id, cmor_tables[axis->table_id].table_id,(long int)strlen(val)+1,axis->id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      strcpy(axis->crequested,val); 
    }
  }
  else {
    snprintf(msg,CMOR_MAX_STRING,"Unknown attribute >>>%s<<< for axis section (%s, table: %s), value: %s",att,axis->id, cmor_tables[axis->table_id].table_id,val);
    cmor_handle_error(msg,CMOR_WARNING);
  }
  cmor_pop_traceback();
  return 0;
}

