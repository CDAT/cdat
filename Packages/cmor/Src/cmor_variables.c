#include <time.h>
#include <stdio.h>
#include <string.h>
#include "cmor.h"
#include <netcdf.h>
#include <udunits2.h>
#include <stdlib.h>
#include <math.h>

int cmor_is_required_variable_attribute(cmor_var_def_t var, char *attribute_name) 
{
  
  char astr[CMOR_MAX_STRING];
  int i,j;
  if (var.required[0]=='\0') {
    return 1;
  }

  i=0;
  astr[0]='\0';
  j=0;
  while(var.required[i]!='\0') {
    while((var.required[i]!=' ') && (var.required[i]!='\0')) {
      astr[j]=var.required[i];
      i+=1;
      j+=1;
    }
    astr[j]='\0';
    if (strncmp(astr,attribute_name,CMOR_MAX_STRING)==0) return 0;
    j=0;
    astr[0]='\0';
    while(var.required[i]==' ') i+=1;
 }
  return 1;
}

int cmor_has_required_variable_attributes(int var_id) 
{
  extern cmor_var_t cmor_vars[];
  char astr[CMOR_MAX_STRING];
  char msg[CMOR_MAX_STRING];
  int i,j;
  cmor_var_def_t var;
  cmor_add_traceback("cmor_has_required_variable_attributes");
  var = cmor_tables[cmor_vars[var_id].ref_table_id].vars[cmor_vars[var_id].ref_var_id];
  
  if (var.required[0]=='\0') {
    cmor_pop_traceback();
    return 0;
  }
  i=0;
  astr[0]='\0';
  j=0;
  while(var.required[i]!='\0') {
    while((var.required[i]!=' ') && (var.required[i]!='\0')) {
      astr[j]=var.required[i];
      i+=1;
      j+=1;
    }
    astr[j]='\0';
    if (cmor_has_variable_attribute(var_id,astr)!=0) {
      snprintf(msg,CMOR_MAX_STRING,"variable %s (table %s) does not have required attribute: %s",cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id,astr);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
    j=0;
    astr[0]='\0';
    while(var.required[i]==' ') i+=1;
  }
  cmor_pop_traceback();
  return 0;

  
}
int cmor_set_variable_attribute_internal(int id, char *attribute_name, char type, void *value)
{
  extern cmor_var_t cmor_vars[];
  int i,index;
  char msg[CMOR_MAX_STRING];
  cmor_add_traceback("cmor_set_variable_attribute_internal");

  cmor_is_setup();
  index=-1;
  cmor_trim_string(attribute_name,msg);
  for (i=0;i<cmor_vars[id].nattributes;i++) {
    if (strcmp(cmor_vars[id].attributes[i],msg)==0) {index=i;break;} /* we found it */
  }
  if (index==-1) {index=cmor_vars[id].nattributes; cmor_vars[id].nattributes+=1;}
  strncpy(cmor_vars[id].attributes[index],msg,CMOR_MAX_STRING); /*stores the name */
  cmor_vars[id].attributes_type[index]=type;
  if (type=='c')  {
    if (strlen(value)>0) {
      strncpytrim(cmor_vars[id].attributes_values_char[index],value,CMOR_MAX_STRING);
    }
    else {
      strcpy(cmor_vars[id].attributes[index],"");
    }
  }
  else if (type=='f')  cmor_vars[id].attributes_values_num[index] = (double)*(float*)value;
  else if (type=='i')  cmor_vars[id].attributes_values_num[index] = (double)*(int*)value;
  else if (type=='d') cmor_vars[id].attributes_values_num[index] = (double)*(double*)value;
  else if (type=='l') cmor_vars[id].attributes_values_num[index] = (double)*(long*)value;
  else {
    snprintf(msg,CMOR_MAX_STRING,"unknown type %c for attribute %s of variable %s (table %s),allowed types are c,i,l,f,d",type,attribute_name,cmor_vars[id].id,cmor_tables[cmor_vars[id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_NORMAL);
    cmor_pop_traceback();
    return 1;
  }
  cmor_pop_traceback();
  return 0;
}
int cmor_set_variable_attribute(int id, char *attribute_name, char type, void *value)
{
  char msg[CMOR_MAX_STRING];
  cmor_add_traceback("cmor_set_variable_attribute");

  /* First of all we need to see if it is not one of the args you can set by calling cmor_variable */
  if (
      (strcmp(attribute_name,"units")==0) ||
      (strcmp(attribute_name,"missing_values")==0) ||
      (strcmp(attribute_name,"_FillValue")==0) ||
      (strcmp(attribute_name,"standard_name")==0) ||
      (strcmp(attribute_name,"long_name")==0) ||
      (strcmp(attribute_name,"flag_values")==0) ||
      (strcmp(attribute_name,"flag_meaning")==0) ||
      (strcmp(attribute_name,"comment")==0) ||
      (strcmp(attribute_name,"history")==0) ||
      (strcmp(attribute_name,"original_name")==0) ||
      (strcmp(attribute_name,"original_units")==0) ||
      (strcmp(attribute_name,"positive")==0) ||
      (strcmp(attribute_name,"cell_methods")==0)
      )
    {
      snprintf(msg,CMOR_MAX_STRING,"variable attribute %s (vor variable %s, table %s) must be set via a call to cmor_variable or it is automaticaly set via the tables",attribute_name,cmor_vars[id].id,cmor_tables[cmor_vars[id].ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_NORMAL);
      cmor_pop_traceback();
      return 1;
    }
  /* Before setting the attribute we need to see if the variable has been initialized */
  if (cmor_vars[id].initialized!=-1) {
    snprintf(msg,CMOR_MAX_STRING,"attribute %s on variable %s (table %s) will probably not be set as the variable has already been created into the output NetCDF file, please place this call BEFORE any cal to cmor_write",attribute_name,cmor_vars[id].id,cmor_tables[cmor_vars[id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_NORMAL);
    cmor_pop_traceback();
    return 1;
  }
  cmor_pop_traceback();
  return cmor_set_variable_attribute_internal(id,attribute_name,type,value);
}

int cmor_get_variable_attribute(int id, char *attribute_name, void *value)
{
  extern cmor_var_t cmor_vars[];
  int i,index;
  char msg[CMOR_MAX_STRING];
  char type;
  cmor_add_traceback("cmor_get_variable_attribute");
  cmor_is_setup();
  index=-1;
  for (i=0;i<cmor_vars[id].nattributes;i++) {
    if (strcmp(cmor_vars[id].attributes[i],attribute_name)==0) {index=i;break;} /* we found it */
  }
  if (index==-1) {
    snprintf(msg,CMOR_MAX_STRING,"Attribute %s could not be found for variable %i (%s, table: %s)",attribute_name,id,cmor_vars[id].id,cmor_tables[cmor_vars[id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_NORMAL);
    cmor_pop_traceback();
    return 1;
  }
  type = cmor_vars[id].attributes_type[i];
  if (type=='c')  strncpy(value,cmor_vars[id].attributes_values_char[index],CMOR_MAX_STRING);
  else if (type=='f') value = (float *)&cmor_vars[id].attributes_values_num[index];
  else if (type=='i')  value = (int *)&cmor_vars[id].attributes_values_num[index];
  else if (type=='l') value = (long *)&cmor_vars[id].attributes_values_num[index];
  else  value = (double *)&cmor_vars[id].attributes_values_num[index];
  cmor_pop_traceback();
  return 0;
}
int cmor_has_variable_attribute(int id, char *attribute_name)
{
  extern cmor_var_t cmor_vars[];
  int i,index;
  char type;
  char msg[CMOR_MAX_STRING];
  cmor_add_traceback("cmor_has_variable_attribute");
  cmor_is_setup();
  index=-1;
  for (i=0;i<cmor_vars[id].nattributes;i++) {
    if (strcmp(cmor_vars[id].attributes[i],attribute_name)==0) {index=i;break;} /* we found it */
  }
  if ((index==-1) || strlen(attribute_name)==0)  {
   cmor_pop_traceback();
    return 1;
  }
  i=0;
  /* if it is empty we assume not defined */
  cmor_get_variable_attribute_type(id,attribute_name,&type);
  if (type=='c') {
    cmor_get_variable_attribute(id,attribute_name,msg);
    if (strlen(msg)==0) {
      /* empty string attribute has been deleted */
      i=1;
    }
  }
  cmor_pop_traceback();
  return i;
}

int cmor_get_variable_attribute_names(int id, int *nattributes,char attributes_names[][CMOR_MAX_STRING])
{
  extern cmor_var_t cmor_vars[];
  int i;
  cmor_add_traceback("cmor_get_variable_attribute_names");
  cmor_is_setup();
  *nattributes = cmor_vars[id].nattributes;
  for (i=0;i<cmor_vars[id].nattributes;i++) {
    strncpy(attributes_names[i],cmor_vars[id].attributes[i],CMOR_MAX_STRING);
  }
  cmor_pop_traceback();
  return 0;
}

int cmor_get_variable_attribute_type(int id, char *attribute_name, char *type)
{
  extern cmor_var_t cmor_vars[];
  int i,index;
  char msg[CMOR_MAX_STRING];
  cmor_add_traceback("cmor_get_variable_attribute_type");
  cmor_is_setup();
  index=-1;
  for (i=0;i<cmor_vars[id].nattributes;i++) {
    if (strcmp(cmor_vars[id].attributes[i],attribute_name)==0) {index=i;break;} /* we found it */
  }
  if (index==-1) {
    snprintf(msg,CMOR_MAX_STRING,"Attribute %s could not be found for variable %i (%s, table: %s)",attribute_name,id,cmor_vars[id].id,cmor_tables[cmor_vars[id].ref_table_id].table_id);
    cmor_handle_error(msg,CMOR_NORMAL);
    cmor_pop_traceback();
    return 1;
  }
  *type = cmor_vars[id].attributes_type[i];
  cmor_pop_traceback();
  return 0;
}

int cmor_zfactor (int *zvar_id,int axis_id, char *name, char *units, int ndims, int axes_ids[], char type, void *values, void *bounds)
{
  extern int cmor_nvars;
  extern cmor_var_t cmor_vars[];

  int i,j,k;
  int n,gid;
  int var_id;

  char msg[CMOR_MAX_STRING];
  extern ut_system *ut_read;
  ut_unit *user_units, *cmor_units;
  cv_converter *ut_cmor_converter;
  char local_unit[CMOR_MAX_STRING];
  double tmp;

  cmor_add_traceback("cmor_zfactor");
  cmor_is_setup();

  /* first check if we need to convert values */
  /* printf("in zf:%s, %s, %i, %i\n",name,cmor_axes[axis_id].id,ndims,axes_ids[0]); */
  /* printf("in zf:%s, %s, %i, %i\n",name,cmor_axes[axis_id].id,cmor_axes[axis_id].hybrid_out,cmor_axes[axis_id].hybrid_in); */
  if (cmor_axes[axis_id].hybrid_out==cmor_axes[axis_id].hybrid_in) { /* no it's a normal hybrid, no conv */
    /*printf("this cmor var: %s, %s, %i, %i, %c\n",name,units,ndims,axes_ids[0],type);*/
    i = cmor_variable(&var_id,name,units,ndims,axes_ids,type,NULL,NULL,NULL,NULL,NULL,NULL);
    cmor_vars[var_id].needsinit=0;
    cmor_vars[var_id].zaxis=axis_id;
    if (values != NULL) {
      n=1;
      for (i=0;i<ndims;i++) {
	if (axes_ids[i]>-1) {
	  n*=cmor_axes[axes_ids[i]].length;
	}
	else {
	  /* ok irregular grid */
	  gid = -axes_ids[i]-CMOR_MAX_GRIDS;
	  for (j=0;j<cmor_grids[gid].ndims;j++) {
	    n*=cmor_axes[cmor_grids[gid].axes_ids[j]].length;
	  }
	}
      }
      cmor_vars[var_id].values = malloc(n*sizeof(double));
      if (cmor_vars[var_id].values == NULL) {
	snprintf(msg,CMOR_MAX_STRING,"cmor_zfactor: zaxis %s, cannot allocate memory for %i double elts %s var '%s' (table: %s)",cmor_axes[axis_id].id,n,cmor_vars[var_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      for (i=0;i<n;i++) {
	if (type=='d')  cmor_vars[var_id].values[i] = (double)((double *)values)[i];
	else if (type=='f')  cmor_vars[var_id].values[i] = (double)((float *)values)[i];
	else if (type=='l')  cmor_vars[var_id].values[i] = (double)((long *)values)[i];
	else if (type=='i')  cmor_vars[var_id].values[i] = (double)((int *)values)[i];
      }

      /* ok we may need to convert to some decent untis */
      strncpy(local_unit,cmor_vars[var_id].ounits,CMOR_MAX_STRING);
      cmor_units = ut_parse(ut_read, local_unit,UT_ASCII);
      if (ut_get_status() != UT_SUCCESS) {
	snprintf(msg,CMOR_MAX_STRING,"Udunits: Error parsing units: %s, zaxis: %s, variable %s (table: %s)",local_unit,cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      strncpy(local_unit,units,CMOR_MAX_STRING);
      ut_trim(local_unit,UT_ASCII);
      user_units = ut_parse(ut_read, local_unit, UT_ASCII);
      if (ut_get_status() != UT_SUCCESS) {
	snprintf(msg,CMOR_MAX_STRING,"Udunits: Error parsing units: %s, zaxis %s, variable %s (table: %s)",local_unit,cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      ut_cmor_converter=ut_get_converter(user_units,cmor_units);
      if (ut_get_status() != UT_SUCCESS) {
	snprintf(msg,CMOR_MAX_STRING,"Udunits: Error getting converter from %s to %s, zaxis: %s, variable %s (table: %s)",units,cmor_vars[var_id].ounits,cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      cv_convert_doubles(ut_cmor_converter,&cmor_vars[var_id].values[0],n,&cmor_vars[var_id].values[0]);
      if (ut_get_status() != UT_SUCCESS) {
	snprintf(msg,CMOR_MAX_STRING,"Udunits: Error with converter (from %s to %s), zaxis: %s, variable %s (table: %s)",units,cmor_vars[var_id].ounits,cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      cv_free(ut_cmor_converter);
      if (ut_get_status() != UT_SUCCESS) {
	snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing converter, zaxis %s, variable %s (table: %s)",cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      ut_free(cmor_units);
      if (ut_get_status() != UT_SUCCESS) {
	snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units %s, zaxis %s, variable %s (table: %s)",cmor_vars[var_id].ounits,cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      ut_free(user_units);
      if (ut_get_status() != UT_SUCCESS) {
	snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units %s, zaxis %s,variable %s (table: %s)",units,cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }

      cmor_vars[var_id].itype='d';
      *zvar_id= var_id; 
    }
    else {
      /* Ok let's check to make sure it has a time axis! */
      int k=0; 
      for (i=0;i<ndims;i++) {
	if (axes_ids[i]>-1) {
	  if (cmor_axes[axes_ids[i]].axis=='T') {
	    k=1;
	    break;
	  }
	}
	else {
	  /* ok irregular grid */
	  gid = -axes_ids[i]-CMOR_MAX_GRIDS;
	  for (j=0;j<cmor_grids[gid].ndims;j++) {
	    if (cmor_axes[cmor_grids[gid].axes_ids[j]].axis=='T') {
	      k=1;
	      break;
	    }
	  }
	}
      }
      if (k==0) {
	snprintf(msg,CMOR_MAX_STRING,"zfactor: axis %s, variable %s (table %s), is not time dependent and you did not provide any values",cmor_axes[axis_id].id,name,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      *zvar_id = var_id;
    }
    if (bounds != NULL) {
      if (ndims!=1) {
	snprintf(msg,CMOR_MAX_STRING,"zfactor axis %s, variable %s (table: %s): you passed bounds values but you also declared %i dimensions, we will ignore you bounds",cmor_axes[axis_id].id,name,cmor_tables[cmor_vars[var_id].ref_table_id].table_id,ndims);
	cmor_handle_error(msg,CMOR_WARNING);
      }
      else {
	strncpy(msg,name,CMOR_MAX_STRING);
	strncat(msg,"_bnds",CMOR_MAX_STRING-strlen(msg));
	i = cmor_variable(&var_id,msg,units,ndims,axes_ids,'d',NULL,NULL,NULL,NULL,NULL,NULL);
	cmor_vars[var_id].zaxis=axis_id;
	cmor_vars[var_id].needsinit=0;
/* 	printf("setting Zaxis to:%i for var: %i (%s) \n",axis_id,var_id,cmor_vars[var_id].id); */
	n = cmor_axes[axes_ids[0]].length;
	cmor_vars[var_id].values = malloc(2*n*sizeof(double));
	if ( cmor_vars[var_id].values == NULL)  {
	  snprintf(msg,CMOR_MAX_STRING,"cmor_zfactor: zaxis %s, cannot allocate memory for %i double bounds elts %s var '%s' (table: %s)",cmor_axes[axis_id].id,2*n,cmor_vars[var_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	cmor_vars[var_id].isbounds=1;
	for (i=0;i<n;i++) {
	  if (type=='d')  { cmor_vars[var_id].values[2*i] = (double)((double *)bounds)[i];cmor_vars[var_id].values[2*i+1] = (double)((double *)bounds)[i+1];}
	  else if (type=='f')  { cmor_vars[var_id].values[2*i] = (double)((float *)bounds)[i];cmor_vars[var_id].values[2*i+1] = (double)((float *)bounds)[i+1];}
	  else if (type=='l')  { cmor_vars[var_id].values[2*i] = (double)((long *)bounds)[i];cmor_vars[var_id].values[2*i+1] = (double)((long *)bounds)[i+1];}
	  else if (type=='i')  { cmor_vars[var_id].values[2*i] = (double)((int *)bounds)[i];cmor_vars[var_id].values[2*i+1] = (double)((int *)bounds)[i+1];}
	}
	/* ok we may need to convert to some decent untis */
	strncpy(local_unit,cmor_vars[var_id].ounits,CMOR_MAX_STRING);
	cmor_units = ut_parse(ut_read, local_unit,UT_ASCII);
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error parsing units: %s, for zaxis %s, variable %s (table: %s)",local_unit,cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	strncpy(local_unit,units,CMOR_MAX_STRING);
	ut_trim(local_unit,UT_ASCII);
	user_units = ut_parse(ut_read, local_unit, UT_ASCII);
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error parsing units: %s, zaxis %s, variable %s (table: %s)",local_unit,cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	ut_cmor_converter=ut_get_converter(user_units,cmor_units);
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error getting converter from %s to %s, zaxis %s, variable %s (table: %s)",units,cmor_vars[var_id].ounits,cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	cv_convert_doubles(ut_cmor_converter,&cmor_vars[var_id].values[0],2*n,&cmor_vars[var_id].values[0]);
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error converting units from %s to %s, zaxis %s, variable %s (table: %s)",units,cmor_vars[var_id].ounits,cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	cv_free(ut_cmor_converter);
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing converter, zaxis %s, variable %s (table: %s)",cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}

	ut_free(cmor_units);
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing cmor units %s, zaxis %s, variable %s (table: %s)",cmor_vars[var_id].ounits,cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	ut_free(user_units);
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units %s, zaxis %s, variable %s (table: %s)",units,cmor_axes[axis_id].id,cmor_vars[var_id].id,cmor_tables[cmor_vars[var_id].ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}

      }
    }
  }
  else {
    /* first prepare the conversion thing */
    
    /* stores the original hybrid_in to put back later */
    i = cmor_axes[axis_id].hybrid_in;
    cmor_axes[axis_id].hybrid_in = cmor_axes[axis_id].hybrid_out;
/*     printf("input type was: %i\n",i); */
    /* ok we now have 3 possible case */
    if (i==1) {
      n = cmor_zfactor(zvar_id,axis_id,name,units,ndims,axes_ids,type,values,bounds);
    }
    else if (i == 2) { /* case alternate hybrid sigma */
      if (strcmp(name,"ap")==0) {
	/* creates the p0 */
	tmp = (double)1.e5;
	j = cmor_zfactor(zvar_id,axis_id,"p0","Pa",0,NULL,'d',&tmp,NULL);
	/* creates the "a" */
	n = cmor_zfactor(zvar_id,axis_id,"a","",ndims,axes_ids,type,values,bounds); /* ok redefined it as a "a" factor */
	/* ok we need to change the values now */
	/* first convert p0 to user units */
	cmor_units = ut_parse(ut_read, "Pa",UT_ASCII);
	strncpy(local_unit,units,CMOR_MAX_STRING);
	ut_trim(local_unit,UT_ASCII);
	user_units = ut_parse(ut_read, local_unit, UT_ASCII);
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error parsing user units: %s, zaxis %s (table: %s), when creating zfactor: %s",local_unit,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,name);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	if (ut_are_convertible(cmor_units,user_units)==0 ) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunuits: Pa and user units (%s) are incompatible, zaxis %s (table: %s), when creating zfactor: %s",units,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,name);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	  cmor_pop_traceback();
	  return 1;
	}
	ut_cmor_converter=ut_get_converter(cmor_units,user_units);
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error getting converter from Pa to %s,variable %s (table %s), when creating zfactor: %s",units,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,name);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	tmp = (double) 1.e5;
	tmp = cv_convert_double(ut_cmor_converter,tmp);
	/* free units thing */
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error converting units from Pa to %s, zaxis %s (table: %s), when creating zfactor: %s",local_unit,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,name);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	cv_free(ut_cmor_converter);
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing converter, zaxis %s (table: %s), when creating zfactor: %s",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,name);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}

	ut_free(cmor_units);
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units Pa, zaxis: %s (table: %s), when creating zfactor: %s",cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,name);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	ut_free(user_units);
	if (ut_get_status() != UT_SUCCESS) {
	  snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units %s, zaxis %s (table: %s), when creating zfactor: %s",local_unit,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,name);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}

	if (values!=NULL) {
	  n = cmor_axes[axes_ids[0]].length;
	  for (j=0;j<n;j++)  cmor_vars[*zvar_id].values[j] /= tmp;
	}
	if (bounds!=NULL) {
	  k=-1;
	  for (n=0;n<=cmor_nvars;n++) if ((strcmp(cmor_vars[n].id,"a_bnds")==0) && (cmor_vars[n].zaxis==axis_id)) {k=n;break;}
	  n = cmor_axes[axes_ids[0]].length;
	  for (j=0;j<2*n;j++) cmor_vars[k].values[j] /= tmp;
	}
      }
      else n = cmor_zfactor(zvar_id,axis_id,name,units,ndims,axes_ids,type,values,bounds); 
    }
    else if (i==3) { /* atmosphere_sigma_coordinates case */
      if (strcmp(name,"sigma")==0) {
	/*ok first we need to make sure we crated the ptop */
	j=-1;
	for (n=0;n<=cmor_nvars;n++) if ((strcmp(cmor_vars[n].id,"ptop")==0) && (cmor_vars[n].zaxis==axis_id)) {j=n;break;}
	if (j==-1) { /* we did not find the ztop! */
	  snprintf(msg,CMOR_MAX_STRING,"zfactor variable \"ptop\" for zfactor axis: %i (%s, table: %s), is not defined when creating zfactor %s, please define ptop first",axis_id,cmor_axes[axis_id].id,cmor_tables[cmor_axes[axis_id].ref_table_id].table_id,name);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	tmp = (double) 1.e5;
	/* creates the p0 */
	n = cmor_zfactor(zvar_id,axis_id,"p0","Pa",0,NULL,'d',&tmp,NULL);
	tmp = cmor_vars[j].values[0]; /* stores ptop (in Pa) */
	/* creates the "a" */
	n = cmor_zfactor(zvar_id,axis_id,"a","",ndims,axes_ids,type,values,bounds); /* ok redefined it as a "a" factor */
	if (values!=NULL) {
	  n = cmor_axes[axes_ids[0]].length;
	  for (j=0;j<n;j++) cmor_vars[*zvar_id].values[j] = (1.-cmor_vars[*zvar_id].values[j])*tmp/1.e5;
	}
	if (bounds!=NULL) {
	  k=-1;
	  for (n=0;n<=cmor_nvars;n++) if ((strcmp(cmor_vars[n].id,"a_bnds")==0) && (cmor_vars[n].zaxis==axis_id)) {k=n;break;}
	  n = cmor_axes[axes_ids[0]].length;
	  for (j=0;j<2*n;j++) cmor_vars[k].values[j] /= tmp;
	}
	/* creates the "b" */
	n = cmor_zfactor(zvar_id,axis_id,"b","",ndims,axes_ids,type,values,bounds); /* ok redefined it as a "a" factor */
      }
      else n = cmor_zfactor(zvar_id,axis_id,name,units,ndims,axes_ids,type,values,bounds); 
    }

    /* put back input type */
    cmor_axes[axis_id].hybrid_in = i;

  }
  cmor_pop_traceback();
  return 0;
}

int cmor_update_history(int var_id,char *add){
  struct tm *ptr;
  time_t lt;
  char date[CMOR_MAX_STRING];
  char tmp[CMOR_MAX_STRING];
  char tmp2[CMOR_MAX_STRING];

  /* first figures out time */
  lt = time(NULL);
  ptr = gmtime(&lt);
  snprintf(date,CMOR_MAX_STRING,"%.4i-%.2i-%.2iT%.2i:%.2i:%.2iZ",ptr->tm_year+1900,ptr->tm_mon+1,ptr->tm_mday,ptr->tm_hour,ptr->tm_min,ptr->tm_sec);
  if (cmor_has_variable_attribute(var_id,"history")==0) {
    cmor_get_variable_attribute(var_id,"history",&tmp[0]);
  }
  else {
    tmp[0]='\0';
  }
  snprintf(tmp2,CMOR_MAX_STRING,"%s %s altered by CMOR: %s.",tmp,date,add);
  cmor_set_variable_attribute_internal(var_id,"history",'c',tmp2);
  cmor_pop_traceback();
  return 0;
}

int cmor_history_contains(int var_id,char *add){
  char tmp[CMOR_MAX_STRING];

  if (cmor_has_variable_attribute(var_id,"history")==0) {
    cmor_get_variable_attribute(var_id,"history",&tmp[0]);
    if (cmor_stringinstring(tmp, add)) {
      return 1;
    }
  }
  return 0;
}

int cmor_variable(int *var_id, char *name, char *units, int ndims, int axes_ids[], char type, void *missing, double *tolerance, char *positive, char*original_name, char *history, char *comment) 
{
  extern int cmor_nvars,cmor_naxes;
  extern int CMOR_TABLE;
  extern cmor_var_t cmor_vars[];
  int i,iref,j,k,ierr,l;
  char msg[CMOR_MAX_STRING];
  char ctmp[CMOR_MAX_STRING];
  cmor_var_def_t refvar;
  int laxes_ids[CMOR_MAX_DIMENSIONS];
  int grid_id=1000;
  int lndims,olndims;
  float afloat;
  int aint;
  long along;
  int did_grid_reorder=0;
  int vrid;

  cmor_add_traceback("cmor_variable");
  cmor_is_setup();

  if (CMOR_TABLE==-1) {
    cmor_handle_error("You did not define a table yet!",CMOR_CRITICAL);
  }

  if (cmor_nvars==CMOR_MAX_VARIABLES-1) { 
    cmor_handle_error("Too many variables defined",CMOR_CRITICAL);
    cmor_pop_traceback();
    return 1;
  }



  /* ok now look which variable is corresponding in table if not found then error */
  iref=-1;
  cmor_trim_string(name,ctmp);
  for (i=0;i<cmor_tables[CMOR_TABLE].nvars+1;i++) {
    if (strcmp(cmor_tables[CMOR_TABLE].vars[i].id,ctmp)==0) { iref=i;break;}
  }
  if (iref==-1) {
    snprintf(msg,CMOR_MAX_STRING,"Could not find a matching variable for name: '%s'",ctmp);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  /*printf("ok your variable is actually variable %i in table %i\n",iref,CMOR_TABLE);*/
  refvar=cmor_tables[CMOR_TABLE].vars[iref];
  for (i=0;i<CMOR_MAX_VARIABLES;i++) {
    if (cmor_vars[i].self==-1) {
      vrid=i;
      break;
    }
  }

  if (vrid>cmor_nvars) cmor_nvars=vrid;
  //vrid = cmor_nvars;

  cmor_vars[vrid].ref_table_id=CMOR_TABLE;
  cmor_vars[vrid].ref_var_id=iref;

  /* init some things */
  strcpy(cmor_vars[vrid].suffix,"");
  strcpy(cmor_vars[vrid].base_path,"");
  strcpy(cmor_vars[vrid].current_path,"");
  cmor_vars[vrid].suffix_has_date=0;

  /*output missing value */
  cmor_vars[vrid].omissing = (double) cmor_tables[CMOR_TABLE].missing_value;

  /* copying over values from ref var */
  cmor_vars[vrid].valid_min = refvar.valid_min;
  cmor_vars[vrid].valid_max = refvar.valid_max;
  cmor_vars[vrid].ok_min_mean_abs = refvar.ok_min_mean_abs;
  cmor_vars[vrid].ok_max_mean_abs = refvar.ok_max_mean_abs;
  cmor_vars[vrid].shuffle = refvar.shuffle;
  cmor_vars[vrid].deflate = refvar.deflate;
  cmor_vars[vrid].deflate_level = refvar.deflate_level;
  /*printf("going in vrid is: %i\n",vrid);*/
  if (refvar.out_name[0]=='\0') {
    strncpy(cmor_vars[vrid].id,name,CMOR_MAX_STRING);
  }
  else {
    strncpy(cmor_vars[vrid].id,refvar.out_name,CMOR_MAX_STRING);
  }

  cmor_set_variable_attribute_internal(vrid,"standard_name",'c',refvar.standard_name);
  cmor_set_variable_attribute_internal(vrid,"long_name",'c',refvar.long_name);
  if ((refvar.flag_values!=NULL) && (refvar.flag_values[0]!='\0')) {
    cmor_set_variable_attribute_internal(vrid,"flag_values",'c',refvar.flag_values);
  }
  if ((refvar.flag_meanings!=NULL) && (refvar.flag_meanings[0]!='\0')) {
    cmor_set_variable_attribute_internal(vrid,"flag_meanings",'c',refvar.flag_meanings);
  }
/*   cmor_set_variable_attribute_internal(vrid,"realm",'c',refvar.realm); */
  cmor_set_variable_attribute_internal(vrid,"comment",'c',refvar.comment);
  if (strcmp(refvar.units,"?")==0) {
    strncpy(cmor_vars[vrid].ounits,units,CMOR_MAX_STRING);
  }
  else {
    strncpy(cmor_vars[vrid].ounits,refvar.units,CMOR_MAX_STRING);
  }
  if (refvar.type!='c' ) {
    cmor_set_variable_attribute_internal(vrid,"units",'c',cmor_vars[vrid].ounits);
  }
  strncpy(cmor_vars[vrid].iunits,units,CMOR_MAX_STRING);
  if ((original_name!=NULL) && (original_name[0]!='\0')) cmor_set_variable_attribute_internal(vrid,"original_name",'c',original_name);
  if ((history!=NULL) && (history[0]!='\0')) cmor_set_variable_attribute_internal(vrid,"history",'c',history);
  if ((comment!=NULL) && (comment[0]!='\0')) {
    if (cmor_has_variable_attribute(vrid,"comment")==0) {
      
      strncpy(msg,comment,CMOR_MAX_STRING);
      strncat(msg,", ",CMOR_MAX_STRING-strlen(msg));
      strncat(msg,cmor_tables[cmor_vars[vrid].ref_table_id].project_id,CMOR_MAX_STRING-strlen(msg));
      strncat(msg,"_table_comment: ",CMOR_MAX_STRING-strlen(msg));
      strncat(msg,refvar.comment,CMOR_MAX_STRING-strlen(msg));
    }
    else {
      strncpy(msg,comment,CMOR_MAX_STRING);
    }
    cmor_set_variable_attribute_internal(vrid,"comment",'c',msg);
  }
  if (strcmp(units,refvar.units)!=0) {
    cmor_set_variable_attribute_internal(vrid,"original_units",'c',units);
    snprintf(msg,CMOR_MAX_STRING,"Converted units from '%s' to '%s'",units,refvar.units);
    cmor_update_history(vrid,msg);
  }
  cmor_set_variable_attribute_internal(vrid,"cell_methods",'c',refvar.cell_methods);
  cmor_set_variable_attribute_internal(vrid,"cell_measures",'c',refvar.cell_measures);
  /*if ((refvar.positive!='\0') && (positive!=NULL) && (positive[0]!=refvar.positive)) cmor_vars[vrid].sign=-1;*/
  if ((positive!=NULL) && (positive[0]!='\0')) {
    if ((positive[0]!='d') && positive[0]!='u') {
      snprintf(msg,CMOR_MAX_STRING,"variable '%s' (table %s): unknown value for positive : %s (only first character is considered, which was: %c)",cmor_vars[vrid].id,cmor_tables[cmor_vars[vrid].ref_table_id].table_id,positive,positive[0]);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
    if (refvar.positive=='u') {
      if (cmor_is_required_variable_attribute(refvar,"positive")==0) cmor_set_variable_attribute_internal(vrid,"positive",'c',"up");
      if (positive[0]!='u') {
	cmor_vars[vrid].sign=-1;
	cmor_update_history(vrid,"Changed sign");
      }
    }
    else if (refvar.positive=='d') {
      if (cmor_is_required_variable_attribute(refvar,"positive")==0) cmor_set_variable_attribute_internal(vrid,"positive",'c',"down");
      if (positive[0]!='d') {
	cmor_vars[vrid].sign=-1;
	cmor_update_history(vrid,"Changed sign");
      }
    }
    else { 
      snprintf(msg,CMOR_MAX_STRING,"variable '%s' (table %s) you passed positive value:%s, but table does not mention it, will be ignored, if you really want this in your variable output use cmor_set_variable_attribute_internal function",cmor_vars[vrid].id,cmor_tables[cmor_vars[vrid].ref_table_id].table_id,positive); 
      cmor_handle_error(msg,CMOR_WARNING);
    }
  }
  else {
    if (cmor_is_required_variable_attribute(refvar,"positive")==0) {
      snprintf(msg,CMOR_MAX_STRING,"you need to provide the 'positive' argument for variable: %s (table %s)",cmor_vars[vrid].id,cmor_tables[cmor_vars[vrid].ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
    if (refvar.positive!='\0') {
      if (refvar.positive=='u') {
	if (cmor_is_required_variable_attribute(refvar,"positive")==0) cmor_set_variable_attribute_internal(vrid,"positive",'c',"up");
	snprintf(msg,CMOR_MAX_STRING,"you did not provide the 'positive' argument for variable: %s (table %s)",cmor_vars[vrid].id,cmor_tables[cmor_vars[vrid].ref_table_id].table_id);
      }
      else if (refvar.positive=='d') {
	if (cmor_is_required_variable_attribute(refvar,"positive")==0) cmor_set_variable_attribute_internal(vrid,"positive",'c',"down");
	snprintf(msg,CMOR_MAX_STRING,"you did not provide the 'positive' argument for variable: %s (table %s)",cmor_vars[vrid].id,cmor_tables[cmor_vars[vrid].ref_table_id].table_id);
      }
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
  }
  /* before anything we copy axes_ids into laxes_ids */
  for (i=0;i<ndims;i++) {
    laxes_ids[i] = axes_ids[i];
  }
  /* Now figure out if the variable ask for an axis that is actually calling for a grid to be defined */
  k=0;
  for (i=0;i<refvar.ndims;i++) {
    /* printf("%s, %i\n",refvar.id,i,refvar.dimensions[i]); */
    for (j=0;j<cmor_tables[cmor_vars[vrid].ref_table_id].naxes;j++) {
      /* printf("comparing: %s vs %s (%i/%i)\n",cmor_tables[cmor_vars[vrid].ref_table_id].axes[refvar.dimensions[i]].id,cmor_tables[cmor_vars[vrid].ref_table_id].axes[j].id,i,j); */
      if (strcmp(cmor_tables[cmor_vars[vrid].ref_table_id].axes[refvar.dimensions[i]].id,cmor_tables[cmor_vars[vrid].ref_table_id].axes[j].id)==0){
	/* printf("must have grid: %i\n",cmor_tables[cmor_vars[vrid].ref_table_id].axes[refvar.dimensions[i]].must_call_cmor_grid); */
	if (cmor_tables[cmor_vars[vrid].ref_table_id].axes[refvar.dimensions[i]].must_call_cmor_grid==1) k=1;
	break;
      }
    }
  }

  if (k==1) { /* ok we MUST HAVE called cmor_grid to generate this variable let's make sure */
    j=0;
    for (i=0;i<ndims;i++) {
      if (laxes_ids[i]<-CMOR_MAX_GRIDS+1) { /* grid definition */
	grid_id = -laxes_ids[i]-CMOR_MAX_GRIDS;
	if (grid_id>cmor_ngrids) continue;
	j=1;
      }
    }
    if (j==0) {
      sprintf(msg,"Variable %s (table %s) must be defined using a grid (a call to cmor_grid)",cmor_vars[vrid].id,cmor_tables[cmor_vars[vrid].ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
  }

  lndims=ndims;
  /* printf("ok ndims is actually: %i\n",ndims); */
  aint=0; /* just to know if we deal with  a grid */
  /* ok we need to replace grids definitions with the grid axes */
  for (i=0;i<ndims;i++) {
    if (laxes_ids[i]>cmor_naxes) {
      sprintf(msg,"For variable %s (table %s) you requested axis_id (%i) that has not been defined yet",cmor_vars[vrid].id,cmor_tables[cmor_vars[vrid].ref_table_id].table_id,laxes_ids[i]);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
    if (laxes_ids[i]<-CMOR_MAX_GRIDS+1) { /* grid definition */
      grid_id = -laxes_ids[i]-CMOR_MAX_GRIDS;
      if (grid_id>cmor_ngrids) {
	sprintf(msg,"For variable %s (table: %s) you requested grid_id (%i) that has not been defined yet",cmor_vars[vrid].id,cmor_tables[cmor_vars[vrid].ref_table_id].table_id,laxes_ids[i]);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      /* here we need to know if the refvar has been defined with lat/lon or in the grid space */
      k=0;
      for (j=0;j<refvar.ndims;j++) {
	if (strcmp(cmor_tables[refvar.table_id].axes[refvar.dimensions[j]].id,"longitude")==0) k++;
	if (strcmp(cmor_tables[refvar.table_id].axes[refvar.dimensions[j]].id,"latitude")==0) k++;
	if (refvar.dimensions[j]==-CMOR_MAX_GRIDS) k++;
      }
      if (k==2) { 
	aint = cmor_grids[grid_id].ndims-2; /* basically replaces the lat/lon with the number of dims in our grid */
      }
      cmor_vars[vrid].grid_id = grid_id;
      k = cmor_grids[grid_id].ndims-1;
      /* printf("ok k offsetting is: %i\n",k); */
      /* first move everything to the right */
      for (j=lndims-1;j>=i;j--) laxes_ids[j+k] = laxes_ids[j];
      /* ok now we need to insert the grid dimensions */
      lndims +=k;
      for (j=0;j<cmor_grids[grid_id].ndims;j++) {
	/* printf("adding axis: %i at pos: %i\n",cmor_grids[grid_id].original_axes_ids[j],i+j); */
	laxes_ids[i+j] = cmor_grids[grid_id].original_axes_ids[j];
      }
    }
  }
  /* printf("&&&&&&&&&&&&&&&&&&&&&&&&&&&&& refvar (%s), has: %i dimensions! aint: %i, lndims: %i\n",refvar.id,refvar.ndims,aint,lndims); */
  /* for(i=0;i<lndims;i++) fprintf(stderr,"after the grid id section: %i, id: %i, name: %s\n",i,laxes_ids[i],cmor_axes[laxes_ids[i]].id); */
  olndims = lndims;
  if (refvar.ndims+aint!=lndims) {
    lndims=0;
    /* ok before we panic we check if there is a "dummy" dim */
    j=refvar.ndims-olndims+aint;
    /* printf("at the start: refvar: %i, ndims: %i, aint: %i, lndims: %i, olndims: %i, j:%i\n",refvar.ndims,ndims,aint,lndims,olndims,j); */
    for (i=0;i<refvar.ndims;i++) {
      /* printf("ok none matchng # of dims, i: %d, id: %s, value: %lf, lndims is: %d\n",i,cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].id,cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].value,olndims); */
      if (cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].value!=1.e20) {
	/* ok it could be a dummy but we need to check if the user already defined this dummy dimension or not */
	l=-1;
	for (k=0;k<olndims;k++) {
	  if (cmor_has_axis_attribute(laxes_ids[k],"standard_name")==0) {
	      cmor_get_axis_attribute(laxes_ids[k],"standard_name",'c',&msg);
	    }
	    else {
	      strcpy(msg,"nope");
	    }
	  /* printf("k: %d, axes_id: %d, stdnm: %s, ref stdnm: %s\n",k,laxes_ids[k],msg,cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].standard_name); */
	  if (strcmp(msg,cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].standard_name)==0) {
	    /* ok user did define this one on its own */
	    l=k;
	    break;
	  }
	  /* printf("And now l is: %i\n",l); */
	}
	if (l==-1) { /* ok it is a singleton dimension */
	  j-=1;
	  /* ok then we create a dummy axis that we will add at the end of the axes */
	  if (cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].bounds_value[0]!=1.e20) {
	    ierr = cmor_axis(&k,cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].id,cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].units,1,&cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].value,'d',&cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].bounds_value[0],2,"");
	  }
	  else {
	    ierr = cmor_axis(&k,cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].id,cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].units,1,&cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].value,'d',NULL,0,"");
	  }
	  /* printf("messing up laxes: %i, replacing from %i to %i\n",olndims,laxes_ids[olndims],k); */
	  laxes_ids[olndims]=k;
	  lndims+=1;
	}
	/* printf("after l is :%i, j is: %i\n",l,j); */
      }
    }

    if (j!=0) {
      snprintf(msg,CMOR_MAX_STRING,"You are defining variable '%s' (table %s)  with %i dimensions, when it should have %i",name,cmor_tables[cmor_vars[vrid].ref_table_id].table_id,ndims,refvar.ndims);
      cmor_handle_error(msg,CMOR_CRITICAL);
      cmor_pop_traceback();
      return 1;
    }
    else {
      lndims += ndims;
      for (j=0;j<ndims;j++) {
	/* printf("ok laxes_ids is: %i\n",axes_ids[j]); */
	if (axes_ids[j]<-CMOR_MAX_GRIDS+1) { /* grid definition */
	  lndims+=cmor_grids[grid_id].ndims-1;
	}
      }
    }
  }
  /* At that point we need to check that the dims we passed match what's in the refvar */
  k=-1;
  for (i=0;i<lndims;i++) {
    for (j=0;j<refvar.ndims;j++) {
/*       printf("i,j: %i, %i, laxes_ids[i]: %i\n",i,j,laxes_ids[i]); */
      if ((strcmp(cmor_tables[cmor_axes[laxes_ids[i]].ref_table_id].axes[cmor_axes[laxes_ids[i]].ref_axis_id].id,cmor_tables[CMOR_TABLE].axes[refvar.dimensions[j]].id)==0) ||((cmor_tables[cmor_axes[laxes_ids[i]].ref_table_id].axes[cmor_axes[laxes_ids[i]].ref_axis_id].axis=='Z') && (refvar.dimensions[j]==-2))){ 
/* || ((cmor_tables[cmor_axes[laxes_ids[i]].ref_table_id].axes[cmor_axes[laxes_ids[i]].ref_axis_id].axis==cmor_tables[CMOR_TABLE].axes[refvar.dimensions[j]].axis) && cmor_tables[cmor_axes[laxes_ids[i]].ref_table_id].axes[cmor_axes[laxes_ids[i]].ref_axis_id].axis!='\0'))  { */
	k++;
      }
    }
    if (k!=i) {
      /* ok we didn't find it, but there is still a slight chance it is a grid axis! */
      for (j=0;j<cmor_grids[grid_id].ndims;j++) {
	if (laxes_ids[i]==cmor_grids[grid_id].original_axes_ids[j]) k++;
      }
    }
    if (k!=i) {
      snprintf(msg,CMOR_MAX_STRING,"You defined variable '%s' (table %s) with axis id '%s' which is not part of this variable according to your table, it says: ( ",refvar.id,cmor_tables[cmor_vars[vrid].ref_table_id].table_id,cmor_tables[cmor_axes[laxes_ids[i]].ref_table_id].axes[cmor_axes[laxes_ids[i]].ref_axis_id].id);
      for (i=0;i<refvar.ndims;i++) {
	strcat(msg,cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].id);
	strcat(msg," ");
      }
      strcat(msg,")");
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
  }
  k=0;
  /* ok now loop thru axes */    
  /* printf("lndims is: %i\n",lndims); */
  for (i=0;i<lndims;i++) { 
    /* printf("i and k: %i, %i, %i \n",i,k,laxes_ids[i]); */
    if (laxes_ids[i]>cmor_naxes) {
      snprintf(msg,CMOR_MAX_STRING,"Axis %i not defined",axes_ids[i]);
      cmor_handle_error(msg,CMOR_CRITICAL);
      cmor_pop_traceback();
      return 1;
    }
    if (cmor_axes[laxes_ids[i]].ref_table_id!=CMOR_TABLE && cmor_axes[laxes_ids[i]].isgridaxis!=1) {
      snprintf(msg,CMOR_MAX_STRING,"While creating variable %s, you are passing axis %i (named %s) which has been defined using table %i (%s) but the current table is %i (%s) (and isgridaxis says: %i)",cmor_vars[vrid].id,laxes_ids[i],cmor_axes[laxes_ids[i]].id,cmor_axes[laxes_ids[i]].ref_table_id,cmor_tables[cmor_axes[laxes_ids[i]].ref_table_id].table_id,CMOR_TABLE,cmor_tables[CMOR_TABLE].table_id,cmor_axes[laxes_ids[i]].isgridaxis);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
    /* printf("ok: %s \n" , cmor_tables[cmor_axes[laxes_ids[i]].ref_table_id].axes[cmor_axes[laxes_ids[i]].ref_axis_id].id); */
    /* printf("ok: %lf \n" , cmor_tables[cmor_axes[laxes_ids[i]].ref_table_id].axes[cmor_axes[laxes_ids[i]].ref_axis_id].value); */
    if (cmor_tables[cmor_axes[laxes_ids[i]].ref_table_id].axes[cmor_axes[laxes_ids[i]].ref_axis_id].value != 1.e20) {
      /*singleton dim */
      snprintf(msg,CMOR_MAX_STRING,"Treated scalar dimension: '%s'",cmor_axes[laxes_ids[i]].id);
      cmor_update_history(vrid,msg);
      cmor_vars[vrid].singleton_ids[i-k]=laxes_ids[i];
      if (cmor_has_variable_attribute(vrid,"coordinates")==0) {
	cmor_get_variable_attribute(vrid,"coordinates",&msg[0]);
      }
      else {
	strncpy(msg,"",CMOR_MAX_STRING);
      }
      if (cmor_tables[cmor_axes[laxes_ids[i]].ref_table_id].axes[cmor_axes[laxes_ids[i]].ref_axis_id].out_name[0]=='\0') {
	snprintf(ctmp,CMOR_MAX_STRING,"%s %s",msg,cmor_tables[cmor_axes[laxes_ids[i]].ref_table_id].axes[cmor_axes[laxes_ids[i]].ref_axis_id].id);
      }
      else {
	snprintf(ctmp,CMOR_MAX_STRING,"%s %s",msg,cmor_tables[cmor_axes[laxes_ids[i]].ref_table_id].axes[cmor_axes[laxes_ids[i]].ref_axis_id].out_name);
      }
      cmor_set_variable_attribute_internal(vrid,"coordinates",'c',&ctmp[0]);
    }
    else {
/*       printf("in else\n"); */
      cmor_vars[vrid].original_order[k]=laxes_ids[i];
      k++;
    }
  }
  /* Now figures out the real order... */
  k=0;

  /* for (i=0;i<lndims;i++) { */
  /*   printf("OK IN CMOR VAR (%s),ORIGINAL ORDER FOR %i is: %i\n",cmor_vars[vrid].id,i,cmor_vars[vrid].original_order[i]); */
  /* } */
  /* printf("at this point we say var has %i dims\n",cmor_vars[vrid].ndims); */
  /* for (i=0;i<lndims;i++) { */
  /*   printf("axis id/name: %i/%s\n",laxes_ids[i],cmor_axes[laxes_ids[i]].id); */
  /* } */

  for (i=0;i<lndims;i++) { 
    /* printf("dim %i\n",i); */
    if (((strcmp(cmor_tables[refvar.table_id].axes[refvar.dimensions[i]].id,"latitude")==0) ||
	 (strcmp(cmor_tables[refvar.table_id].axes[refvar.dimensions[i]].id,"longitude")==0) ) &&
	(grid_id!=1000) ) {
      /* printf("case 1\n"); */
      /* ok we are  dealing with a "grid" type of data */
      if (did_grid_reorder !=0 ) continue;
      for (j=0;j<cmor_grids[grid_id].ndims;j++) {
	cmor_vars[vrid].axes_ids[k]=cmor_grids[grid_id].axes_ids[j];
	k++;
      }
      did_grid_reorder = 1;
    }
    else if ((refvar.dimensions[i]==-2) || (cmor_tables[CMOR_TABLE].axes[refvar.dimensions[i]].value == 1.e20)) { /* not a singleton dim */
      /* printf("case 2\n"); */
      iref=-1;
      for (j=0;j<lndims;j++) {
	/* printf("\tj:%i, refvar table: %i, axis table: %i, refvardim: %i, axisrefax: %i\n",j,refvar.table_id,cmor_axes[laxes_ids[j]].ref_table_id,refvar.dimensions[i],cmor_axes[laxes_ids[j]].ref_axis_id); */
	if ((refvar.table_id==cmor_axes[laxes_ids[j]].ref_table_id) && (refvar.dimensions[i]==cmor_axes[laxes_ids[j]].ref_axis_id)) {
	  cmor_vars[vrid].axes_ids[k]=laxes_ids[j];
	}
	/* -2 means it is a zaxis */
	if (refvar.dimensions[i]==-2) {
	  /* printf("i: %i, j: %i, axid: %i, laxesid: %i, axes_ids: %i\n",i,j,cmor_vars[vrid].axes_ids[i],laxes_ids[j],axes_ids[j]); */
	  if (cmor_axes[laxes_ids[j]].axis=='Z') cmor_vars[vrid].axes_ids[i]=laxes_ids[j];
	}
      }
      k++;
    }
    else if (refvar.dimensions[i]==-CMOR_MAX_GRIDS) {
      /* printf("case 3\n"); */
      /* ok this is either a lat/lon */
      for(j=0;j<ndims;j++) if (axes_ids[j]<-CMOR_MAX_GRIDS+1) break;
      l=j;
      for (j=0;j<cmor_grids[grid_id].ndims;j++) {
	cmor_vars[vrid].axes_ids[k]=cmor_grids[grid_id].axes_ids[j];
	k++;
	i++;
      }
      i--; /* one too many i adds */
    }
  }

  /* printf("OK WE ARE SAYING THAT THIS VARIABLE HAS %i DIMENSIONS\n",k); */
  cmor_vars[vrid].ndims=k;
  cmor_vars[vrid].itype=type;
  k=0;

  for (i=0;i<cmor_vars[vrid].ndims;i++) if (cmor_vars[vrid].original_order[i]!=cmor_vars[vrid].axes_ids[i]) k=-1;
  if (k==-1) {
    /* printf("reordered: "); */
    strncpy(msg,"Reordered dimensions, original order:",CMOR_MAX_STRING);
    for (i=0;i<cmor_vars[vrid].ndims;i++) {
      snprintf(ctmp,CMOR_MAX_STRING," %s",cmor_axes[cmor_vars[vrid].original_order[i]].id);
      /* printf(" %s",cmor_axes[cmor_vars[vrid].original_order[i]].id); */
      strncat(msg,ctmp,CMOR_MAX_STRING-strlen(ctmp));
    }
    /* printf("\n"); */
    cmor_update_history(vrid,msg);
  }
  /* printf("Original/Final order: "); */
  /* for (i=0;i<cmor_vars[vrid].ndims;i++) { */
  /*   printf(" %s/%s",cmor_axes[cmor_vars[vrid].original_order[i]].id,cmor_axes[cmor_vars[vrid].axes_ids[i]].id); */
  /* } */
  /* printf("\n"); */
  if (refvar.type=='\0') cmor_vars[vrid].type='f';
  else cmor_vars[vrid].type=refvar.type;
  if (missing != NULL) {
    cmor_vars[vrid].nomissing = 0;/*printf("ok missing was not NULL pointer\n");}*/
    if (type=='i') cmor_vars[vrid].missing = (double)*(int *)missing;
    if (type=='l') cmor_vars[vrid].missing = (double)*(long *)missing;
    if (type=='f') cmor_vars[vrid].missing = (double)*(float *)missing;
    if (type=='d') cmor_vars[vrid].missing = (double)*(double *)missing;
    if (cmor_vars[vrid].missing!=cmor_vars[vrid].omissing) {
      snprintf(msg,CMOR_MAX_STRING,"replaced missing value flag (%g) with standard missing value (%g)",cmor_vars[vrid].missing,cmor_vars[vrid].omissing);
      cmor_update_history(vrid,msg);
    }
    if (refvar.type=='d') {
      cmor_set_variable_attribute_internal(vrid,"missing_value",'d',&cmor_vars[vrid].omissing);
      cmor_set_variable_attribute_internal(vrid,"_FillValue",'d',&cmor_vars[vrid].omissing);
    }
    else if (refvar.type=='f') {
      afloat = (float)cmor_vars[vrid].omissing;
      cmor_set_variable_attribute_internal(vrid,"missing_value",'f',&afloat);
      cmor_set_variable_attribute_internal(vrid,"_FillValue",'f',&afloat);
    }
    else if (refvar.type=='l') {
      along = (long)cmor_vars[vrid].omissing;
      cmor_set_variable_attribute_internal(vrid,"missing_value",'l',&along);
      cmor_set_variable_attribute_internal(vrid,"_FillValue",'l',&along);
    }
    else if (refvar.type=='i') {
      aint = (int)cmor_vars[vrid].omissing;
      cmor_set_variable_attribute_internal(vrid,"missing_value",'i',&aint);
      cmor_set_variable_attribute_internal(vrid,"_FillValue",'i',&aint);
    }
  }

  /*printf("assiging %i to na\n",vrid);*/
  cmor_vars[vrid].tolerance = 1.e-4;
  cmor_vars[vrid].self=vrid;
  if (tolerance!=NULL) cmor_vars[vrid].tolerance = (double)*(double *)tolerance;
  *var_id=vrid;
  cmor_pop_traceback();
  return 0;
};

void cmor_init_var_def(cmor_var_def_t *var, int table_id)
{
  int n;
  cmor_is_setup();
  var->table_id=table_id;
  var->standard_name[0]='\0';
  var->units[0]='\0';
  var->cell_methods[0]='\0';
  var->cell_measures[0]='\0';
  var->positive='\0';
  var->long_name[0]='\0';
  var->comment[0]='\0';
  var->realm[0]='\0';
  var->frequency[0]='\0';
  var->out_name[0]='\0';
  var->ndims=0;
  var->flag_values[0]='\0';
  var->flag_meanings[0]='\0';
  for(n=0;n<CMOR_MAX_DIMENSIONS;n++) var->dimensions[n]=-1;
  var->type='f';
  var->valid_min=1.e20; /* means no check */
  var->valid_max=1.e20;
  var->ok_min_mean_abs=1.e20;
  var->ok_max_mean_abs=1.e20;
  var->shuffle=0;
  var->deflate=1;
  var->deflate_level=1;
}

int cmor_set_var_def_att(cmor_var_def_t *var,char att[CMOR_MAX_STRING],char val[CMOR_MAX_STRING] ){
  int i,n,j,n0,k;
  char dim[CMOR_MAX_STRING];
  char msg[CMOR_MAX_STRING];

  cmor_add_traceback("cmor_set_var_def_att");
  cmor_is_setup();
  if (strcmp(att,"required")==0) {
    strncpy(var->required,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"id")==0) {
    strncpy(var->id,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"standard_name")==0) {
    strncpy(var->standard_name,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"long_name")==0) {
    strncpy(var->long_name,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"comment")==0) {
    strncpy(var->comment,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"dimensions")==0) {
    n0=strlen(val);
    for (i=0;i<n0;i++) {
      j=0;
      while((i<n0)&&((val[i]!=' ')&&val[i]!='\0')) { dim[j]=val[i] ; j++ ; i++ ;}
      dim[j]='\0';
      if (var->ndims > CMOR_MAX_DIMENSIONS) {
	snprintf(msg,CMOR_MAX_STRING,"Too many dimensions (%i) defined for variable (%s), max is: %i",var->ndims,var->id,CMOR_MAX_DIMENSIONS);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      /* check that the dimension as been defined in the table */
      n=-1; /* not found yet */
      for (j=0;j<=cmor_tables[var->table_id].naxes;j++) {
	if (strcmp(dim,cmor_tables[var->table_id].axes[j].id)==0) {n=j;break;}
      }
      if (n==-1) {
	j = strcmp("zlevel",dim);
        j *= strcmp("alevel",dim);
        j *= strcmp("olevel",dim);
	for (k=0;k<CMOR_MAX_ELEMENTS;k++) {
	  if (cmor_tables[var->table_id].generic_levels[k][0]=='\0') break;
	  j*=strcmp(dim,cmor_tables[var->table_id].generic_levels[k]);
	}

	if (j==0) {
	  /* printf("ignoring zlevel for now\n");*/
	  var->dimensions[var->ndims]=-2;
	}
	else {
	  if ((strcmp(dim,"longitude")!=0) && strcmp(dim,"latitude")!=0) { /* do not raise a warning if the axis is "longitude" / "latitude" it is probably a grid variable */
	    snprintf(msg,CMOR_MAX_STRING,"Reading table %s: axis name: '%s' for variable: '%s' is not defined in table. Table defines dimensions: '%s' for this variable",cmor_tables[var->table_id].table_id,dim,var->id,val);cmor_handle_error(msg,CMOR_CRITICAL);
	  }
	  else {
	    var->dimensions[var->ndims]=-CMOR_MAX_GRIDS;
	  }
	}
      }
      else {
	var->dimensions[var->ndims]=n;
      }
      var->ndims++;
    }
    /* revert to put in C order */
    for (i=0;i<var->ndims/2;i++) {
      n=var->dimensions[i];
      var->dimensions[i]=var->dimensions[var->ndims-1-i];
      var->dimensions[var->ndims-1-i]=n;
    }
    /*for (i=0;i<var->ndims;i++) printf("%s dimension %i: %s\n",var->id,i,cmor_tables[var->table_id].axes[var->dimensions[i]].id);*/
  }
  else if (strcmp(att,"units")==0) {
    strncpy(var->units,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"cell_methods")==0) {
    strncpy(var->cell_methods,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"ext_cell_measures")==0) {
    strncpy(var->cell_measures,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"cell_measures")==0) {
    strncpy(var->cell_measures,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"positive")==0) {
    var->positive=val[0];
  }
  else if (strcmp(att,"type")==0) {
    if (strcmp(val,"real")==0) var->type='f';
    else if (strcmp(val,"double")==0) var->type='d';
    else if (strcmp(val,"integer")==0) var->type='i';
    else if (strcmp(val,"long")==0) var->type='l';
  }
  else if (strcmp(att,"valid_min")==0) {
    var->valid_min = atof(val);
  }
  else if (strcmp(att,"valid_max")==0) {
    var->valid_max = atof(val);
  }
  else if (strcmp(att,"ok_min_mean_abs")==0) {
    var->ok_min_mean_abs=atof(val);
  }
  else if (strcmp(att,"ok_max_mean_abs")==0) {
    var->ok_max_mean_abs = atof(val);
  }
  else if (strcmp(att,"shuffle")==0) {
    var->shuffle = atoi(val);
    if (atoi(val)!=0) {
      if (USE_NETCDF_4 == 0) {
	sprintf(msg,"Reading a table (%s) that calls for NetCDF4 features, you are using NetCDF3 library",cmor_tables[var->table_id].table_id);
	cmor_handle_error(msg,CMOR_WARNING);
      }
      else if ( (CMOR_NETCDF_MODE == CMOR_APPEND_3) ||
		(CMOR_NETCDF_MODE == CMOR_REPLACE_3) ||
		(CMOR_NETCDF_MODE == CMOR_PRESERVE_3)) {
	sprintf(msg,"Reading a table (%s) that calls for NetCDF4 features, you asked for NetCDF3 features",cmor_tables[var->table_id].table_id);
	cmor_handle_error(msg,CMOR_WARNING);
      }
    }
  }
  else if (strcmp(att,"deflate")==0) {
    var->deflate = atoi(val);
    if (atoi(val)!=0) {
      if (USE_NETCDF_4 == 0) {
	sprintf(msg,"Reading a table (%s) that calls for NetCDF4 features, you are using NetCDF3 library",cmor_tables[var->table_id].table_id);
	cmor_handle_error(msg,CMOR_WARNING);
      }
      else if ( (CMOR_NETCDF_MODE == CMOR_APPEND_3) ||
		(CMOR_NETCDF_MODE == CMOR_REPLACE_3) ||
		(CMOR_NETCDF_MODE == CMOR_PRESERVE_3)) {
	sprintf(msg,"Reading a table (%s) that calls for NetCDF4 features, you asked for NetCDF3 features",cmor_tables[var->table_id].table_id);
	cmor_handle_error(msg,CMOR_WARNING);
      }
    }
  }
  else if (strcmp(att,"deflate_level")==0) {
    var->deflate_level = atoi(val);
  }
  else if (strcmp(att,"modeling_realm")==0) {
    strncpy(var->realm,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"frequency")==0) {
    strncpy(var->frequency,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"flag_values")==0) {
    strncpy(var->flag_values,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"flag_meanings")==0) {
    strncpy(var->flag_meanings,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"out_name")==0) {
    strncpy(var->out_name,val,CMOR_MAX_STRING);
  }
  else {
    snprintf(msg,CMOR_MAX_STRING,"Table %s, unknown variable attribute: >>>>%s<<<< value: (%s)",cmor_tables[var->table_id].table_id,att,val);
    cmor_handle_error(msg,CMOR_WARNING);
  }
  cmor_pop_traceback();
  return 0;
}

int cmor_get_variable_time_length(int *var_id, int *length) {
  cmor_var_t avar;
  int i;

  *length=0;
  avar = cmor_vars[*var_id];
  for (i=0;i<avar.ndims;i++) {
    if (cmor_axes[avar.original_order[i]].axis=='T') *length=cmor_axes[avar.original_order[i]].length;
  }

/*   if (*length == -1) { */
/*     snprintf(msg,CMOR_MAX_STRING,"trying to retrieve length of time axis for variable %s but this variable does not have a time axis",avar.id); */
/*     cmor_handle_error(msg,CMOR_CRITICAL); */
/*   } */
  return 0;
}
    

int cmor_get_original_shape(int *var_id, int *shape_array, int *rank, int blank_time) {
  int i;
  cmor_var_t avar;
  char msg[CMOR_MAX_STRING];
  cmor_add_traceback("cmor_get_original_shape");
  avar = cmor_vars[*var_id];
  for(i=0;i<*rank;i++) shape_array[i]=-1; /* init array */

  if (*rank<avar.ndims) {
    snprintf(msg,CMOR_MAX_STRING,"trying to retrieve shape of variable %s (table: %s) into a %id array but this variable is %id",avar.id,cmor_tables[avar.ref_table_id].table_id,*rank,avar.ndims);
	  cmor_handle_error(msg,CMOR_CRITICAL);
  }
  for (i=0;i<avar.ndims;i++) {
    if ((blank_time==1) && (cmor_axes[avar.original_order[i]].axis=='T')) {
      shape_array[i]=0;
    }
    else {
      shape_array[i]=cmor_axes[avar.original_order[i]].length;
    }
  }
  
  /*   for(i=0;i<avar.ndims;i++) printf("var order: %i, %i\n",i,cmor_axes[avar.original_order[i]].length); */
  /*   for(i=0;i<*rank;i++) printf("returning: %i, %i\n",i,shape_array[i]); */
  cmor_pop_traceback();
  return 0;
}

int cmor_write_var_to_file(int ncid,cmor_var_t *avar,void *data,char itype, int ntimes_passed, double *time_vals, double *time_bounds){
  size_t counts[CMOR_MAX_DIMENSIONS];
  size_t counts2[CMOR_MAX_DIMENSIONS];
  int counter[CMOR_MAX_DIMENSIONS];
  int counter_orig[CMOR_MAX_DIMENSIONS];
  int counter_orig2[CMOR_MAX_DIMENSIONS];
  int counter2[CMOR_MAX_DIMENSIONS];
  size_t starts[CMOR_MAX_DIMENSIONS];
  int nelements,loc,add,nelts;
  double *data_tmp=NULL, tmp=0., tmp2,amean;
  int *idata_tmp=NULL;
  long *ldata_tmp=NULL;
  float *fdata_tmp=NULL;
  char mtype;
  int i,j,ierr=0,dounits=1;
  char msg[CMOR_MAX_STRING];
  char msg2[CMOR_MAX_STRING];
  double *tmp_vals;
  ut_unit *user_units=NULL, *cmor_units=NULL;
  cv_converter *ut_cmor_converter=NULL;
  char local_unit[CMOR_MAX_STRING];
  int n_lower_min=0,n_greater_max=0;
  double emax,emin,first_time;
  char msg_min[CMOR_MAX_STRING];
  char msg_max[CMOR_MAX_STRING];
  extern ut_system *ut_read;   
  int tmpindex=0;

  cmor_add_traceback("cmor_write_var_to_file");
  cmor_is_setup();

  emax = 0.;
  emin = 0.;
/*   type = avar->itype; /\* stores input type for variable *\/ */

  if (strcmp(avar->ounits,avar->iunits)==0) dounits=0;
  mtype = avar->type;
  /* This counts how many elements there is in each dimension and the total number of elements written at this time */
  /* This needs to be passed to NetCDF */
  /* do we have times ? */
  if (ntimes_passed!=0) {
    counts[0]=ntimes_passed;
    if (cmor_axes[avar->axes_ids[0]].axis!='T') {     
      snprintf(msg,CMOR_MAX_STRING,"you are passing %i time steps for a static (no time dimension) variable (%s, table: %s), please pass 0 (zero) as the number of times", ntimes_passed, avar->id, cmor_tables[avar->ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
  }
  else {
    /* need to determine if it is a static variable */
    if (avar->ndims>0) counts[0] = cmor_axes[avar->axes_ids[0]].length;
    else counts[0]=1;
  }
  nelements = counts[0];
  for (i=1;i<avar->ndims;i++) {
    counts[i]=cmor_axes[avar->axes_ids[i]].length;
    nelements = nelements*counts[i];
  }
  if (avar->isbounds==1) nelements*=2;
/*   printf("we detected: %i elts over %i dims, var: %s\n",nelements,avar->ndims,avar->id); */
  /* This section counts how many elements are needed before you increase the index in each dimension */
  counter[avar->ndims]=1; /* dummy */
  counter_orig[avar->ndims]=1; /*dummy */

/*   for (i=0;i<avar->ndims;i++) { */
/*     printf("dimension: %i, alength: %i, id: %s, counts:%i\n",i,cmor_axes[avar->axes_ids[i]].length,cmor_axes[avar->axes_ids[i]].id,counts[i]); */
/*     printf("dimension: %i, olength: %i, id: %s, counts:%i\n",i,cmor_axes[avar->original_order[i]].length,cmor_axes[avar->original_order[i]].id,counts[i]); */
/*   } */
  for (i=avar->ndims-1;i>=0;i--) {
    /* we need to do this for the order in which we will write and the order the user defined its variable */
    if (cmor_axes[avar->axes_ids[i]].axis!='T') counter[i]=cmor_axes[avar->axes_ids[i]].length*counter[i+1];
    else counter[i]=counts[0]*counter[i+1];
    if (cmor_axes[avar->original_order[i]].axis!='T') counter_orig[i]=cmor_axes[avar->original_order[i]].length*counter_orig[i+1];
    else counter_orig[i]=counts[0]*counter_orig[i+1];
  }
/*   for (i=0;i<avar->ndims;i++) { */
/*     printf("dimension: %i, counter_orig:%i\n",i,counter_orig[i]); */
/*   } */
  /* Now we need to map, i.e going ahead by 2 eleemnts of final array eq going ahaead of n elements originally */
  for (i=0;i<avar->ndims;i++) {
    for (j=0;j<avar->ndims;j++) {
      if (avar->axes_ids[i]==avar->original_order[j]) counter_orig2[i]=counter_orig[j+1];
    }
  }
/*   for (i=0;i<avar->ndims;i++) {printf("i: %d, id: %s, length: %d, counter: %d, counter_orig: %d, counter_orig2: %d\n",i,cmor_axes[avar->axes_ids[i]].id,cmor_axes[avar->axes_ids[i]].length,counter[i],counter_orig[i],counter_orig2[i]);} */


  /* Allocates the memory to store data to be written after reordering and scaling/offsetting */
  /* needs to figure out if we need to touch the variable... */
  if (mtype=='i') {
    idata_tmp = malloc(sizeof(int)*nelements);
    if (idata_tmp == NULL )  {
      snprintf(msg,CMOR_MAX_STRING,"cannot allocate memory for %i int tmp elts var '%s' (table: %s)",nelements,avar->id,cmor_tables[avar->ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
  }
  else if (mtype=='l') {
    ldata_tmp = malloc(sizeof(long)*nelements);
    if (ldata_tmp == NULL)   {
      snprintf(msg,CMOR_MAX_STRING,"cannot allocate memory for %i long tmp elts var '%s' (table: %s)",nelements,avar->id,cmor_tables[avar->ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
  }
  else if (mtype=='d') {
    data_tmp = malloc(sizeof(double)*nelements);
    if (data_tmp == NULL)   {
      snprintf(msg,CMOR_MAX_STRING,"cannot allocate memory for %i double tmp elts var '%s' (table: %s)",nelements,avar->id,cmor_tables[avar->ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
  }
  else {
    fdata_tmp = malloc(sizeof(float)*nelements);
    if (fdata_tmp == NULL)   {
      snprintf(msg,CMOR_MAX_STRING,"cannot allocate memory for %i float tmp elts var '%s' (table: %s)",nelements,avar->id,cmor_tables[avar->ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
  }
  
  /* Reorder data, applies scaling, etc... */
  if (dounits==1) {
    strncpy(local_unit,avar->ounits,CMOR_MAX_STRING);
    ut_trim(local_unit,UT_ASCII);
    cmor_units = ut_parse(ut_read, local_unit,UT_ASCII);
    if (ut_get_status() != UT_SUCCESS ) {
      snprintf(msg,CMOR_MAX_STRING,"in udunits analyzing units from cmor table (%s) for variable %s (table: %s)",local_unit,avar->id,cmor_tables[avar->ref_table_id].table_id );
      cmor_handle_error(msg,CMOR_CRITICAL);
      cmor_pop_traceback();
      return 1;
    }
    strncpy(local_unit,avar->iunits,CMOR_MAX_STRING);
    ut_trim(local_unit,UT_ASCII);
    user_units = ut_parse(ut_read, local_unit, UT_ASCII);
    if (ut_get_status() != UT_SUCCESS ) {
      snprintf(msg,CMOR_MAX_STRING,"in udunits analyzing units from user (%s) for variable %s (table: %s)",local_unit,avar->id,cmor_tables[avar->ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
      cmor_pop_traceback();
      return 1;
    }
    if (ut_are_convertible(cmor_units,user_units)==0 ) {
      snprintf(msg,CMOR_MAX_STRING,"variable: %s, cmor and user units are incompatible: %s and %s for variable %s (table: %s)",avar->id,avar->ounits,avar->iunits,avar->id,cmor_tables[avar->ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
      cmor_pop_traceback();
      return 1;
    }
    ut_cmor_converter=ut_get_converter(user_units,cmor_units);
    if (ut_get_status() != UT_SUCCESS ) {
      snprintf(msg,CMOR_MAX_STRING," in udunits, getting converter for variable %s (table: %s)",avar->id,cmor_tables[avar->ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
      cmor_pop_traceback();
      return 1;
    }
  }
  amean = 0.;
  nelts = 0;
  for (i=0;i<nelements;i++) {
    loc = i;
/*     printf("i: %i (",i); */
    /* first figures out the coeff in final order */
    /* puts the result in counter2 */
    for (j=0;j<avar->ndims;j++) {
      counter2[j] = (int)loc/(int)counter[j+1];
      /* this is the reverse part */
      /* doing it this way to avoid if test */
      loc = loc - counter2[j]*counter[j+1];
/*       printf(" %i: %i,",j,counter2[j]); */
    }
/*     printf(")\n"); */
    /* now figures out what these indices meant in the original order */
    loc = 0;
    for (j=0;j<avar->ndims;j++) {
      /* revert and offsetting done automatically */
      /* next 2 commented out lines are for no reverse no modulo */
/*       add = counter2[j]; */
/*       loc = loc + add*counter_orig2[j]; */
      if (cmor_axes[avar->axes_ids[j]].axis!='T') {
/* 	printf("J: %i, counter2[j]: %i, counter_orig2[j]: %i , avar->axes_ids[j]: %i, id: %s, revert: %i, len: %i\n",j,counter2[j],counter_orig2[j],avar->axes_ids[j],cmor_axes[avar->axes_ids[j]].id,cmor_axes[avar->axes_ids[j]].revert,cmor_axes[avar->axes_ids[j]].length); */
	add = counter2[j]*cmor_axes[avar->axes_ids[j]].revert + (cmor_axes[avar->axes_ids[j]].length-1)*(1-cmor_axes[avar->axes_ids[j]].revert)/2;
	loc = loc + (int)fmod(add+cmor_axes[avar->axes_ids[j]].offset,cmor_axes[avar->axes_ids[j]].length)*counter_orig2[j];
      }
      else {
/* 	printf("time one\n"); */
/* 	printf("J: %i, counter2[j]: %i, counter_orig2[j]: %i , avar->axes_ids[j]: %i, id: %s, revert: %i, len: %i\n",j,counter2[j],counter_orig2[j],avar->axes_ids[j],cmor_axes[avar->axes_ids[j]].id,cmor_axes[avar->axes_ids[j]].revert,counts[0]); */
	add = counter2[j]*cmor_axes[avar->axes_ids[j]].revert + (counts[0]-1)*(1-cmor_axes[avar->axes_ids[j]].revert)/2;
	loc = loc + (int)fmod(add+cmor_axes[avar->axes_ids[j]].offset,counts[0])*counter_orig2[j];
      }
/*       printf("add,loc are: %i, %i\n",add,loc); */
    }

    /* Copy from user's data into our data */
/*     printf("i,loc: %d, %d\n",i,loc); */

    if (itype=='d') tmp = (double)((double *)data)[loc];
    else if (itype=='f') tmp = (double)((float *)data)[loc];
    else if (itype=='i') tmp = (double)((int *)data)[loc];
    else if (itype=='l') tmp = (double)((long *)data)[loc];
    if (avar->isbounds) {
      /* ok here's the code for filipping the code if necessary */
      if (cmor_axes[avar->axes_ids[0]].revert==-1) {
	loc = nelements - i -1;
      }
      else {
	loc=i;
      }
      tmp = (double)((double *)data)[loc];
    }
/*     printf("data read as: i: %lf\n",(double)((int *)data)[loc]); */
/*     printf("data read as: l: %lf\n",(double)((long *)data)[loc]); */
/*     printf("data read as: f: %lf\n",(double)((float *)data)[loc]); */
/*     printf("data read as: d: %lf\n",(double)((double *)data)[loc]); */
/*     printf("type is: %c, tmp is: %lf\n",itype,tmp); */
/*     printf("missing is: %lf\n",avar->missing); */
    tmp2 = (double)fabs(tmp-avar->missing);
    if ((avar->nomissing==0) && (tmp2<=avar->tolerance*(double)fabs(tmp))) {
      tmp = avar->omissing;
    }
    else {
      if (dounits==1) {
	tmp = cv_convert_double(ut_cmor_converter,tmp);
	if (ut_get_status() != UT_SUCCESS ) {
	  snprintf(msg,CMOR_MAX_STRING,"in udunits, converting values from %s to %s for variable %s (table: %s)",avar->iunits, avar->ounits, avar->id,cmor_tables[avar->ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	  cmor_pop_traceback();
	  return 1;
	}
      }
      tmp = tmp*avar->sign; /* do we need to change the sign ? */
      amean += fabs(tmp);
      nelts+=1;
      if ((avar->valid_min!=(float)1.e20) && (tmp<avar->valid_min)) {
	n_lower_min+=1;
	if ((n_lower_min==1)|| (tmp<emin)) { /*minimum val */
	  emin = tmp;
	  snprintf(msg_min,CMOR_MAX_STRING, "Invalid value(s) detected for variable '%s' (table: %s): %%i values were lower than minimum valid value (%.4g). Minimum encountered bad value (%.5g) was at (axis: index/value):" , avar->id, cmor_tables[avar->ref_table_id].table_id, avar->valid_min,tmp);
	  for (j=0;j<avar->ndims;j++) {
	    if (cmor_axes[avar->axes_ids[j]].values!=NULL) {
	      snprintf(msg2, CMOR_MAX_STRING, " %s: %i/%.5g" , cmor_axes[avar->axes_ids[j]].id,counter2[j],cmor_axes[avar->axes_ids[j]].values[counter2[j]]);
	    }
	    else {
	      snprintf(msg2, CMOR_MAX_STRING, " %s: %i/%.5g" , cmor_axes[avar->axes_ids[j]].id,counter2[j],time_vals[counter2[j]]);
	    }
	    strncat(msg_min,msg2,CMOR_MAX_STRING-strlen(msg));
	  }
	}
      }
      if ((avar->valid_max!=(float)1.e20) && (tmp>avar->valid_max)) {
	n_greater_max+=1;
	if ((n_greater_max==1)|| (tmp>emax)) {
	  emax=tmp;
	  snprintf(msg_max,CMOR_MAX_STRING, "Invalid value(s) detected for variable '%s' (table: %s): %%i values were greater than maximum valid value (%.4g).Maximum encountered bad value (%.5g) was at (axis: index/value):" , avar->id, cmor_tables[avar->ref_table_id].table_id, avar->valid_max,tmp);
	  for (j=0;j<avar->ndims;j++) {
	    if (cmor_axes[avar->axes_ids[j]].values!=NULL) {
	      snprintf(msg2, CMOR_MAX_STRING, " %s: %i/%.5g" , cmor_axes[avar->axes_ids[j]].id,counter2[j],cmor_axes[avar->axes_ids[j]].values[counter2[j]]);
	    }
	    else {
	      snprintf(msg2, CMOR_MAX_STRING, " %s: %i/%.5g" , cmor_axes[avar->axes_ids[j]].id,counter2[j],time_vals[counter2[j]]);
	    }
	    strncat(msg_max,msg2,CMOR_MAX_STRING-strlen(msg));
	  }
	}
      }
    }

    /*printf("mtype: %c\n",mtype);*/
    if (mtype=='i') idata_tmp[i]=(int)tmp;
    else if (mtype=='l') ldata_tmp[i]=(long)tmp;
    else if (mtype=='f') fdata_tmp[i]=(float)tmp;
    else if (mtype=='d') data_tmp[i]=(double)tmp;
  }
  if (n_lower_min!=0) {
    snprintf(msg,CMOR_MAX_STRING,msg_min,n_lower_min);
    cmor_handle_error(msg,CMOR_WARNING);
  }
  if (n_greater_max!=0) {
    snprintf(msg,CMOR_MAX_STRING,msg_max,n_greater_max);
    cmor_handle_error(msg,CMOR_WARNING);
  }
  if (avar->ok_min_mean_abs!=(float)1.e20) {
    if (amean/nelts<.1*avar->ok_min_mean_abs) {
      snprintf(msg,CMOR_MAX_STRING, "Invalid Absolute Mean for variable '%s' (table: %s) (%.5g) is lower by more than an order of magnitude than minimum allowed: %.4g" , avar->id, cmor_tables[avar->ref_table_id].table_id, amean/nelts, avar->ok_min_mean_abs);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
    if (amean/nelts<avar->ok_min_mean_abs) {
      snprintf(msg,CMOR_MAX_STRING, "Invalid Absolute Mean for variable '%s' (table: %s) (%.5g) is lower than minimum allowed: %.4g" , avar->id, cmor_tables[avar->ref_table_id].table_id, amean/nelts, avar->ok_min_mean_abs);
      cmor_handle_error(msg,CMOR_WARNING);
    }
  }
  if (avar->ok_max_mean_abs!=(float)1.e20) {
    if  (amean/nelts>10.*avar->ok_max_mean_abs) {
      snprintf(msg,CMOR_MAX_STRING, "Invalid Absolute Mean for variable '%s' (table: %s) (%.5g) is greater by more than an order of magnitude than maximum allowed: %.4g" , avar->id, cmor_tables[avar->ref_table_id].table_id, amean/nelts, avar->ok_max_mean_abs);
    cmor_handle_error(msg,CMOR_CRITICAL);
    }
    if  (amean/nelts>avar->ok_max_mean_abs) {
      snprintf(msg,CMOR_MAX_STRING, "Invalid Absolute Mean for variable '%s' (table: %s) (%.5g) is greater than maximum allowed: %.4g" , avar->id, cmor_tables[avar->ref_table_id].table_id, amean/nelts, avar->ok_max_mean_abs);
    cmor_handle_error(msg,CMOR_WARNING);
    }
  }
  if (dounits==1) {
    cv_free(ut_cmor_converter);
    if (ut_get_status() != UT_SUCCESS) {
      snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing converter, variable %s (table: %s)", avar->id, cmor_tables[avar->ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
    
    ut_free(cmor_units);
    if (ut_get_status() != UT_SUCCESS) {
      snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units, variable %s (table: %s)", avar->id, cmor_tables[avar->ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
    ut_free(user_units);
    if (ut_get_status() != UT_SUCCESS) {
      snprintf(msg,CMOR_MAX_STRING,"Udunits: Error freeing units, variable %s (table: %s)", avar->id, cmor_tables[avar->ref_table_id].table_id);
      cmor_handle_error(msg,CMOR_CRITICAL);
    }
  }
  /* Initialize the start index in each dimensions */
  for (i=0;i<avar->ndims;i++) starts[i]=0;
  starts[0]=avar->ntimes_written;

  /* Write the times passed by user */
  if (ntimes_passed!=0){
    if (time_vals!=NULL) {
      if (cmor_axes[avar->axes_ids[0]].values!=NULL) {
	snprintf(msg,CMOR_MAX_STRING,"variable '%s' (table %s) you are passing time values but you already defined them via cmor_axis, this is not allowed",avar->id, cmor_tables[avar->ref_table_id].table_id);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      
      if (time_bounds!=NULL) {
	counts2[0]=counts[0];
	counts2[1]=2;
	starts[1]=0;
	cmor_get_axis_attribute(avar->axes_ids[0],"units",'c',&msg);
	cmor_get_cur_dataset_attribute("calendar",msg2);
	
	
	
	/* 	else { */
	/* 	  /\* checking manually that time is within bounds *\/ */
	/* 	  if ((time_vals[0]<time_bounds[0]) || (time_vals[0]>time_bounds[1])) { */
	/* 	    snprintf(msg,CMOR_MAX_STRING, "Time point: %lf is not within the bounds you passed: [%lf, %lf]\n",time_vals[0],time_bounds[0],time_bounds[1]); */
	/* 	    cmor_handle_error(msg,CMOR_CRITICAL); */
	/* 	  } */
	/* 	} */
	tmp_vals = malloc((ntimes_passed+1)*2*sizeof(double));
	if (tmp_vals == NULL) {
	  snprintf(msg,CMOR_MAX_STRING,"cannot malloc %i tmp bounds time vals for variable '%s' (table: %s)",ntimes_passed*2,avar->id,cmor_tables[avar->ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
        if (avar->ntimes_written>0) {
            if ((avar->last_time!=-999.)&&(avar->last_bound!=1.e20)) {
                tmpindex = 1;
                tmp_vals[0]=avar->last_time;
            }
            else {
                tmpindex=0;
            }
        }
        else {
            tmpindex = 0;
        }
	ierr = cmor_convert_time_values(time_vals,'d',ntimes_passed,&tmp_vals[tmpindex],cmor_axes[avar->axes_ids[0]].iunits,msg,msg2,msg2);
	ierr = cmor_check_monotonic(&tmp_vals[0],ntimes_passed+tmpindex,"time",0,avar->axes_ids[0]);
        if (avar->ntimes_written>0) {
            if ((avar->last_time!=-999.)&&(avar->last_bound!=1.e20)) {
                tmp_vals[0] = 2*avar->last_time-avar->last_bound;
                tmp_vals[1] = avar->last_bound;
            }
        }
	ierr = cmor_convert_time_values(time_bounds,'d',ntimes_passed*2,&tmp_vals[2*tmpindex],cmor_axes[avar->axes_ids[0]].iunits,msg,msg2,msg2);
	ierr = cmor_check_monotonic(&tmp_vals[0],(ntimes_passed+tmpindex)*2,"time",1,avar->axes_ids[0]);
	ierr = cmor_check_values_inside_bounds(&time_vals[0],&time_bounds[0], ntimes_passed, "time");
	ierr = nc_put_vara_double(ncid,avar->time_bnds_nc_id,starts,counts2,&tmp_vals[2*tmpindex]);
	if (ierr != NC_NOERR) {snprintf(msg,CMOR_MAX_STRING,"NetCDF error (%i) writing time bounds for variable '%s', already written in file: %i",ierr,avar->id,avar->ntimes_written);cmor_handle_error(msg,CMOR_CRITICAL);}
	/* /\* ok first time around the we need to store bounds *\/ */
	if (avar->ntimes_written == 0) {
	  /* ok first time we're putting data  in */
	  avar->first_bound = tmp_vals[0];
	}
        else {
            /* ok let's put the bounds back on "normal" (start at 0) indices */
            for (i=0;i<2*ntimes_passed;i++) {
                tmp_vals[i]=tmp_vals[i+2];
            }
        }
        avar->last_bound = tmp_vals[ntimes_passed*2-1];
	
	/* ok since we have bounds we need to set time in the middle */
	/* but only do this in case of none climato */
	if (cmor_tables[cmor_axes[avar->axes_ids[0]].ref_table_id].axes[cmor_axes[avar->axes_ids[0]].ref_axis_id].climatology==0) {
	  for (i=0;i<ntimes_passed;i++) {
	    tmp_vals[i]=(tmp_vals[2*i]+tmp_vals[2*i+1])/2.;
	  }
	  first_time = tmp_vals[0]; /*store for later */
	}
	else { /* we need to put into tmp_vals the right things */
	  ierr = cmor_convert_time_values(time_vals,'d',ntimes_passed,&tmp_vals[0],cmor_axes[avar->axes_ids[0]].iunits,msg,msg2,msg2);
	  first_time = tmp_vals[0] ; /*store for later */
	}
	
	
	ierr = nc_put_vara_double(ncid,avar->time_nc_id,starts,counts,&tmp_vals[0]);
	if (ierr != NC_NOERR) {snprintf(msg,CMOR_MAX_STRING,"NetCDF error (%i: %s) writing time values for variable '%s' (%s)",ierr,nc_strerror(ierr),avar->id,cmor_tables[avar->ref_table_id].table_id);cmor_handle_error(msg,CMOR_CRITICAL);}
	
	if (cmor_tables[cmor_axes[avar->axes_ids[0]].ref_table_id].axes[cmor_axes[avar->axes_ids[0]].ref_axis_id].climatology==0) {
	  /* all good in that case */
	}
	else {
	  //tmp_vals[0] = tmp_vals[ntimes_passed*2-2];
	  tmp_vals[ntimes_passed-1]=tmp_vals[ntimes_passed*2-1];
	}	/* ok now we need to store first and last stuff */
	if (avar->ntimes_written == 0) {
	  /* ok first time we're putting data  in */
	  avar->first_time = first_time;
	}
	else {
	  if (tmp_vals[0]<avar->last_time) {
	    snprintf(msg,CMOR_MAX_STRING, "Time point: %lf ( %lf in output units) is not monotonic last time was: %lf (in output units), variable %s (table: %s)",time_vals[0],tmp_vals[0],avar->last_time,avar->id,cmor_tables[avar->ref_table_id].table_id);
	    cmor_handle_error(msg,CMOR_CRITICAL);
	  }
	}
	/* 	printf("setting last time to: %lf\n",tmp_vals[ntimes_passed-1]); */
	avar->last_time = tmp_vals[ntimes_passed-1];
	
	free(tmp_vals);
      }
      else {
	/* checks wether you need bounds or not */
	if (cmor_tables[cmor_axes[avar->axes_ids[0]].ref_table_id].axes[cmor_axes[avar->axes_ids[0]].ref_axis_id].must_have_bounds==1) {
	  snprintf(msg,CMOR_MAX_STRING,"time axis must have bounds, please pass them to cmor_write along with time values, variable %s, table %s",avar->id,cmor_tables[avar->ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	avar->first_bound=1.e20;
	avar->last_bound=1.e20;
	cmor_get_axis_attribute(avar->axes_ids[0],"units",'c',&msg);
	cmor_get_cur_dataset_attribute("calendar",msg2);
	tmp_vals = malloc(ntimes_passed*sizeof(double));
	if (tmp_vals == NULL)  {
	  snprintf(msg,CMOR_MAX_STRING,"cannot malloc %i time vals for variable '%s' (table: %s)",ntimes_passed,avar->id,cmor_tables[avar->ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	ierr = cmor_convert_time_values(time_vals,'d',ntimes_passed,&tmp_vals[0],cmor_axes[avar->axes_ids[0]].iunits,msg,msg2,msg2);
	ierr = nc_put_vara_double(ncid,avar->time_nc_id,starts,counts,tmp_vals);
	/* 	printf("setting last time to (4): %lf\n",tmp_vals[ntimes_passed-1]); */
	if (avar->ntimes_written == 0) {
	  /* ok first time we're putting data  in */
	  avar->first_time = tmp_vals[0];
	}
	avar->last_time = tmp_vals[ntimes_passed-1];
	/* 	printf("setting last time to: %f\n",avar->last_time); */
	free(tmp_vals);
	if (ierr != NC_NOERR) {snprintf(msg,CMOR_MAX_STRING,"NetCDF error (%i: %s) writing times for variable '%s' (table: %s), already written in file: %i",ierr,nc_strerror(ierr),avar->id,cmor_tables[avar->ref_table_id].table_id,avar->ntimes_written);cmor_handle_error(msg,CMOR_CRITICAL);}
      }
    }
    else { /* ok we did not pass time values therefore it means they were defined via the axis */
      if (cmor_axes[avar->axes_ids[0]].values==NULL) {
	snprintf(msg,CMOR_MAX_STRING,"variable '%s' (table: %s) you are passing %i times but no values and you did not define them via cmor_axis",avar->id,cmor_tables[avar->ref_table_id].table_id,ntimes_passed);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      if (cmor_axes[avar->axes_ids[0]].bounds!=NULL) {
	/* ok at that stage the recentering must already be done so we just need to write the bounds */
	counts2[0]=counts[0];
	counts2[1]=2;
	starts[1]=0;
	ierr = nc_put_vara_double(ncid,avar->time_bnds_nc_id,starts,counts2,&cmor_axes[avar->axes_ids[0]].bounds[starts[0]*2]);
	if (ierr != NC_NOERR) {snprintf(msg,CMOR_MAX_STRING,"NCError (%i: %s) writting time bounds values for variable '%s' (table: %s)",ierr,nc_strerror(ierr),avar->id,cmor_tables[avar->ref_table_id].table_id);cmor_handle_error(msg,CMOR_CRITICAL);}
	/* ok we need to store first and last bounds */
	if (avar->ntimes_written==0) {
	  avar->first_bound=cmor_axes[avar->axes_ids[0]].bounds[starts[0]*2];
	}
	avar->last_bound=cmor_axes[avar->axes_ids[0]].bounds[(starts[0]+counts[0])*2-1];
      }
      else {
	/* checks wether you need bounds or not */
	if (cmor_tables[cmor_axes[avar->axes_ids[0]].ref_table_id].axes[cmor_axes[avar->axes_ids[0]].ref_axis_id].must_have_bounds==1) {
	  snprintf(msg,CMOR_MAX_STRING,"time axis must have bounds, you defined it w/o any for variable %s (table: %s)",avar->id,cmor_tables[avar->ref_table_id].table_id);
	  cmor_handle_error(msg,CMOR_CRITICAL);
	}
	avar->first_bound=1.e20;
	avar->last_bound=1.e20;
      }
      ierr = nc_put_vara_double(ncid,avar->time_nc_id,starts,counts,&cmor_axes[avar->axes_ids[0]].values[starts[0]]);
      if (ierr != NC_NOERR) {snprintf(msg,CMOR_MAX_STRING,"NCError (%i: %s) writting time values for variable '%s' (table: %s)",ierr,nc_strerror(ierr),avar->id,cmor_tables[avar->ref_table_id].table_id);cmor_handle_error(msg,CMOR_CRITICAL);}
      /* ok now we need to store first and last stuff */
      if (avar->ntimes_written==0) {
	avar->first_time = cmor_axes[avar->axes_ids[0]].values[starts[0]];
      }
      /*       printf("setting last time (2) to: %lf\n",cmor_axes[avar->axes_ids[0]].values[starts[0]+counts[0]-1]); */
      avar->last_time = cmor_axes[avar->axes_ids[0]].values[starts[0]+counts[0]-1];
    }
  }
  else { /* ok we did not pass time values therefore it means they were defined via the axis */
    ierr = -1;
    /* look for time dimension */
    for (i = 0;i<avar->ndims;i++) {
      if (cmor_axes[avar->axes_ids[0]].axis=='T') {
	ierr = i;
	break;
      }
    }
    if (ierr!=-1) {
      if (cmor_axes[avar->axes_ids[ierr]].values==NULL) {
	snprintf(msg,CMOR_MAX_STRING,"variable '%s' (table: %s) you are passing %i times but no values and you did not define them via cmor_axis",avar->id,cmor_tables[avar->ref_table_id].table_id,ntimes_passed);
	cmor_handle_error(msg,CMOR_CRITICAL);
      }
      avar->first_bound=1.e20;
      avar->last_bound=1.e20;
      if (cmor_axes[avar->axes_ids[ierr]].bounds!=NULL) {
	/* ok at that stage the recentering must already be done so we just need to write the bounds */
	counts2[0]=counts[0];
	counts2[1]=2;
	starts[0]=0;
	starts[1]=0;
	ierr = nc_put_vara_double(ncid,avar->time_bnds_nc_id,starts,counts2,&cmor_axes[avar->axes_ids[0]].bounds[starts[0]*2]);
	if (ierr != NC_NOERR) {snprintf(msg,CMOR_MAX_STRING,"NCError (%i: %s) writting time bounds values for variable '%s' (table: %s)",ierr,nc_strerror(ierr),avar->id,cmor_tables[avar->ref_table_id].table_id);cmor_handle_error(msg,CMOR_CRITICAL);}
	avar->first_bound=cmor_axes[avar->axes_ids[0]].bounds[0];
	avar->last_bound=cmor_axes[avar->axes_ids[0]].bounds[counts[0]*2-1];
      }
      ierr = nc_put_vara_double(ncid,avar->time_nc_id,starts,counts,&cmor_axes[avar->axes_ids[0]].values[starts[0]]);
      if (ierr != NC_NOERR) {snprintf(msg,CMOR_MAX_STRING,"NCError (%i: %s) writting time values for variable '%s' (table: %s)",ierr,nc_strerror(ierr),avar->id,cmor_tables[avar->ref_table_id].table_id);cmor_handle_error(msg,CMOR_CRITICAL);}
      /* ok now we need to store first and last stuff */
      avar->first_time = cmor_axes[avar->axes_ids[0]].values[0];
/*       printf("setting last time (3) to: %lf\n",cmor_axes[avar->axes_ids[0]].values[starts[0]+counts[0]-1]); */
      avar->last_time = cmor_axes[avar->axes_ids[0]].values[starts[0]+counts[0]-1];
    }
  }
  
  if (avar->isbounds) {counts[avar->ndims]=2;starts[avar->ndims]=0;}
  /* printf("writing: %s with: \n",avar->id); */
  /* for (i=0;i<avar->ndims;i++) { */
  /*   printf("dim: %i, starts: %i, counts: %i\n",i,starts[i],counts[i]); */
  /* } */
  if (mtype=='d') ierr = nc_put_vara_double(ncid,avar->nc_var_id,starts,counts,data_tmp);
  else if (mtype=='f') ierr = nc_put_vara_float(ncid,avar->nc_var_id,starts,counts,fdata_tmp);
  else if (mtype=='l') ierr = nc_put_vara_long(ncid,avar->nc_var_id,starts,counts,ldata_tmp);
  else if (mtype=='i') ierr = nc_put_vara_int(ncid,avar->nc_var_id,starts,counts,idata_tmp);

  if (ierr != NC_NOERR) {snprintf(msg,CMOR_MAX_STRING,"NetCDF Error (%i: %s), writing variable '%s' (table %s) to file",ierr,nc_strerror(ierr),avar->id,cmor_tables[avar->ref_table_id].table_id);cmor_handle_error(msg,CMOR_CRITICAL);}
  avar->ntimes_written+=ntimes_passed;
  if (mtype=='d') free(data_tmp);
  else if (mtype=='f') free(fdata_tmp);
  else if (mtype=='l') free(ldata_tmp);
  else if (mtype=='i') free(idata_tmp);
  cmor_pop_traceback();
  return 0;
}
