#include <stdio.h>
#include <string.h>
#include "cmor.h"
#include "cmor_func_def.h"
#include <netcdf.h>
#include <udunits2.h>
#include <stdlib.h>
#include "cmor_locale.h"

int wfgetc(FILE *afile) {
  int i = fgetc(afile);
  while (i=='\r') {
    i=fgetc(afile);
  }
  return i;
}

void cmor_init_table(cmor_table_t *table, int id)
{
  int i;
  cmor_add_traceback("cmor_init_table");
  cmor_is_setup();
  /* init the table */
  table->id=id;
  table->nvars=-1;
  table->naxes=-1;
  table->nexps=-1;
  table->nmappings=-1;
  table->cf_version=1.4;
  table->cmor_version=2.0;
  table->project_id[0]='\0';
  table->table_id[0]='\0';
  strcpy(table->realm,"REALM");
  table->date[0]='\0';
  table->missing_value=1.e20;
  table->interval=0.;
  table->interval_warning=.1;
  table->interval_error=.2;
  table->URL[0]='\0';
  strcpy(table->product,"output");
  table->path[0]='\0';
  table->required_gbl_att[0]='\0';
  table->frequency[0]='\0';
  table->nforcings=0;
  for (i=0;i<CMOR_MAX_ELEMENTS;i++) {
    table->expt_ids[i][0]='\0';
    table->sht_expt_ids[i][0]='\0';
    table->generic_levels[i][0]='\0';
  }

  cmor_pop_traceback();
}

int cmor_set_dataset_att(cmor_table_t *table, char att[CMOR_MAX_STRING],char val[CMOR_MAX_STRING] ){
  int n,i,j;
  float d,d2;
  char value[CMOR_MAX_STRING];
  char value2[CMOR_MAX_STRING];
  extern int cmor_ntables;

  cmor_add_traceback("cmor_set_dataset_att");
  cmor_is_setup();

  strncpy(value,val,CMOR_MAX_STRING);

  if (strcmp(att,"cmor_version")==0) {
    d2 = CMOR_VERSION_MAJOR;
    d = CMOR_VERSION_MINOR;
    while(d>1.) d/=10.;
    d2+=d;
    sscanf(value,"%f",&d);
    if (d>d2) {
      snprintf(value2,CMOR_MAX_STRING,"Table %s is defined for cmor_version %f, this library verson is: %i.%i.%i, %f",table->table_id,d,CMOR_VERSION_MAJOR,CMOR_VERSION_MINOR,CMOR_VERSION_PATCH,d2);
      cmor_handle_error(value2,CMOR_CRITICAL);
      cmor_ntables--;
      cmor_pop_traceback();
      return 1;
    }
    table->cmor_version=d;
  }
  else if (strcmp(att,"generic_levels")==0) {
    n=0;
    i=0;
    while(i<strlen(value)) {
      while(value[i]==' ') i++;
      j=0;
      while (i<strlen(value) && value[i]!=' ') {
	table->generic_levels[n][j]=value[i];
	j++;
	i++;
      }
      table->generic_levels[n][j]='\0';
      n+=1;
    }
  }
  else if (strcmp(att,"cf_version")==0) {
    d = atof(value);
    table->cf_version=d;
  }
  else if (strcmp(att,"required_global_attributes")==0) {
    strncpy(table->required_gbl_att,val,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"project_id")==0) {
    strncpy(table->project_id,value,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"modeling_realm")==0) {
    strncpy(table->realm,value,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"table_date")==0) {
    strncpy(table->date,value,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"baseURL")==0) {
    strncpy(table->URL,value,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"forcings")==0) {
    cmor_convert_string_to_list(value,'c',(void **)&table->forcings,&table->nforcings);
  }
  else if (strcmp(att,"product")==0) {
    strncpy(table->product,value,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"frequency")==0) {
    strncpy(table->frequency,value,CMOR_MAX_STRING);
  }
  else if (strcmp(att,"table_id")==0) {
    for (n=0;n==cmor_ntables;n++) {
     if (strcmp(cmor_tables[n].table_id,value)==0) {
        snprintf(value2,CMOR_MAX_STRING,"Table %s is already defined",table->table_id);
	cmor_handle_error(value2,CMOR_CRITICAL);
	cmor_ntables--;
	cmor_pop_traceback();
	return 1;
      }
    }
    n = strlen(value);
    for (i=n-1;i>0;i--) {
      if (value[i]==' ') break;
    }
    if (value[i]==' ') i++;

    for (j=i;j<n;j++) value2[j-i]=value[j];
    value2[n-i]='\0';

    strcpy(table->table_id,value2);
  }
  else if (strcmp(att,"expt_id_ok")==0) {
    table->nexps++;
    if (table->nexps>CMOR_MAX_ELEMENTS) {
      snprintf(value2,CMOR_MAX_STRING,"Table %s: Too many experiments defined",table->table_id);
      cmor_handle_error(value2,CMOR_CRITICAL);
      cmor_ntables--;
      cmor_pop_traceback();
      return 1;
    }
    if (value[0]=='\'') for (n=0;n<strlen(value)-1;n++) value[n]=value[n+1] ; /* removes leading "'" */
    n=strlen(value);
    if (value[n-2]=='\'') value[n-2]='\0'; /*removes trailing "'" */
    /* ok here we look for a ' which means there is a short name associated with it */
    n=-1;
    for(j=0;j<strlen(value);j++) {
      if (value[j]=='\'') {
	n=j;
	break;
      }
    }
    if (n==-1) {
      strncpy(table->expt_ids[table->nexps],value,CMOR_MAX_STRING);
      strcpy(table->sht_expt_ids[table->nexps],"");
    }
    else {
      /* ok looks like we have a short name let clook for the next ' */
      i=-1;
      for (j=n+1;j<strlen(value);j++) {
	if (value[j]=='\'') i=j;
      }
      if (i==-1) {/* ok we must have a ' in our exp_id_ok */
	strncpy(table->expt_ids[table->nexps],value,CMOR_MAX_STRING);
	strcpy(table->sht_expt_ids[table->nexps],"");
      }
      else {
	for (j=i+1;j<strlen(value);j++) {
	  value2[j-i-1]=value[j];
	  value2[j-i]='\0';
	}
	strncpy(table->sht_expt_ids[table->nexps],value2,CMOR_MAX_STRING);
	value[n]='\0';
	strncpy(table->expt_ids[table->nexps],value,CMOR_MAX_STRING);
      }
    }
  }
  else if (strcmp(att,"approx_interval")==0) {
    sscanf(value,"%lf",&table->interval);
  }
  else if (strcmp(att,"approx_interval_error")==0) {
    sscanf(value,"%f",&table->interval_error);
  }
  else if (strcmp(att,"approx_interval_warning")==0) {
    sscanf(value,"%f",&table->interval_warning);
  }
  else if (strcmp(att,"missing_value")==0) {
    sscanf(value,"%f",&table->missing_value);
  }
  else if (strcmp(att,"magic_number")==0) {
    /* Never actually implemented supposed to control table has not been altered */
  }
  else {
    snprintf(value,CMOR_MAX_STRING,"table: %s, unknown keyword for dataset: %s (%s)",table->table_id,att,value);
    cmor_handle_error(value,CMOR_WARNING);
  }
  cmor_pop_traceback();
  return 0;
}

int cmor_set_table(int table) {
  extern int CMOR_TABLE;
  char msg[CMOR_MAX_STRING];
  cmor_add_traceback("cmor_set_table");
  cmor_is_setup();
  if (table>cmor_ntables) {
    snprintf(msg,CMOR_MAX_STRING,"Invalid table number: %i",table);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  if (cmor_tables[table].table_id=='\0') {
    snprintf(msg,CMOR_MAX_STRING,"Invalid table: %i , not loaded yet!",table);
    cmor_handle_error(msg,CMOR_CRITICAL);
  }
  CMOR_TABLE = table;
  cmor_pop_traceback();
  return 0;
}

int cmor_load_table(char table[CMOR_MAX_STRING], int *table_id) {
  FILE *table_file;
  char word[CMOR_MAX_STRING],word2[CMOR_MAX_STRING];
  int i,n;
  int do_var=0,do_axis=0,do_dataset=1,do_mapping=0;
  extern int CMOR_TABLE,cmor_ntables;
  extern char cmor_input_path[CMOR_MAX_STRING] ;
  char msg[CMOR_MAX_STRING];

  cmor_add_traceback("cmor_load_table");
  cmor_is_setup();
  
/*   printf("loading table: %s\n",table); */
  for (i=0;i<cmor_ntables+1;i++) {
    if (strcmp(cmor_tables[i].path,table)==0) {
      CMOR_TABLE = i;
      *table_id = i;
/*       printf("table %s was already loaded, no need to do that again\n",table); */
      cmor_pop_traceback();
      return 0;
    }
  }
  cmor_ntables+=1;
  cmor_init_table(&cmor_tables[cmor_ntables],cmor_ntables);
  table_file=fopen(table,"r");
  if (table_file == NULL) {
    if (table[0]!='/') {
      snprintf(word,CMOR_MAX_STRING,"%s/%s",cmor_input_path,table);
      table_file=fopen(word,"r");
    }
    if (table_file == NULL) {
      snprintf(word,CMOR_MAX_STRING,"%s/share/%s",CMOR_PREFIX,table);
      table_file=fopen(word,"r");
    }
    if (table_file == NULL ) {
      snprintf(word,CMOR_MAX_STRING,"Could not find table: %s",table);
      cmor_handle_error(word,CMOR_NORMAL);
      cmor_ntables-=1;
      cmor_pop_traceback();
      return 1;
    }
  }

  /* ok now we need to store the md5 */
  cmor_md5(table_file,cmor_tables[cmor_ntables].md5);
  i = wfgetc(table_file);
  while (i != EOF ) {
    /* skip blanks and returns */
    while((i=='\n') || i==' ' || i=='\t') i = wfgetc(table_file);
    /* skip comment lines */
    /*printf("looking at first line charcter:--%c--\n",i);*/
    while (i=='!') {
      i = wfgetc(table_file);
      if (i==EOF) break;
      /*printf("ok it is comment line, second char is --%c--\n");*/
      if (i=='=') { /* we found the head of a definition section */
	while(i!='\n') i = wfgetc(table_file);
	i = wfgetc(table_file);
	if (i==EOF) break;
	/* now read the word from the definition */
	n=0;
	while(i!=':') {
	  word[n] = i;
	  i = wfgetc(table_file);
	  n+=1;
	}
	word[n]='\0';
	/* now figures out the name of the entry */
	i = wfgetc(table_file);
	while (i==' ' || i=='\t') i = wfgetc(table_file);
	n=0;
	while((i!='\n') && i!=' ' && i!='\t') {
	  word2[n]=i;
	  i = wfgetc(table_file);
	  n+=1;
	}
	word2[n]='\0';
	/* finishes the line */
	while (i!='\n') i = wfgetc(table_file);
	i = wfgetc(table_file);
	/*skip the next line */
	while (i!='\n') i = wfgetc(table_file);
	i = wfgetc(table_file);
	/*printf("entry %s, name: %s\n",word,word2);*/
	/* Now let's see what we found */
	if (strcmp(word,"axis_entry")==0) {
	  do_dataset=0;
	  do_var=0;
	  do_axis=1;
	  do_mapping=0;
	  cmor_tables[cmor_ntables].naxes++;
	  if (cmor_tables[cmor_ntables].naxes>=CMOR_MAX_ELEMENTS) {
	    snprintf(msg,CMOR_MAX_STRING,"Too many axes defined for table: %s",cmor_tables[cmor_ntables].table_id);
	    cmor_handle_error(msg,CMOR_CRITICAL);
	    cmor_ntables--;
	    cmor_pop_traceback();
	    return 1;
	  }
	  /* init the axis def */
/* 	  printf("initializing axis: %s\n",word2); */
	  cmor_init_axis_def(&cmor_tables[cmor_ntables].axes[cmor_tables[cmor_ntables].naxes],cmor_ntables);
	  cmor_set_axis_def_att(&cmor_tables[cmor_ntables].axes[cmor_tables[cmor_ntables].naxes],"id",word2);
	}
	else if (strcmp(word,"variable_entry")==0) {
	  do_dataset=0;
	  do_var=1;
	  do_axis=0;
	  do_mapping=0;
	  cmor_tables[cmor_ntables].nvars++;
	  if (cmor_tables[cmor_ntables].nvars>=CMOR_MAX_ELEMENTS) {
	    snprintf(msg,CMOR_MAX_STRING,"Too many variables defined for table: %s",cmor_tables[cmor_ntables].table_id);
	    cmor_handle_error(msg,CMOR_CRITICAL);
	    cmor_ntables--;
	    cmor_pop_traceback();
	    return 1;
	  }
	  /* init the variable def */
	  cmor_init_var_def(&cmor_tables[cmor_ntables].vars[cmor_tables[cmor_ntables].nvars],cmor_ntables);
	  cmor_set_var_def_att(&cmor_tables[cmor_ntables].vars[cmor_tables[cmor_ntables].nvars],"id",word2);

	}
	else if (strcmp(word,"mapping_entry")==0) {
	  do_dataset=0;
	  do_var=0;
	  do_axis=0;
	  do_mapping=1;
	  cmor_tables[cmor_ntables].nmappings++;
	  if (cmor_tables[cmor_ntables].nmappings>=CMOR_MAX_ELEMENTS) {
	    snprintf(msg,CMOR_MAX_STRING,"Too many mappings defined for table: %s",cmor_tables[cmor_ntables].table_id);
	    cmor_handle_error(msg,CMOR_CRITICAL);
	    cmor_ntables--;
	    cmor_pop_traceback();
	    return 1;
	  }
	  for(n=0;n<cmor_tables[cmor_ntables].nmappings-1;n++) if (strcmp(cmor_tables[cmor_ntables].mappings[cmor_tables[cmor_ntables].nmappings].id,cmor_tables[cmor_ntables].mappings[n].id)==0) {
	      snprintf(msg,CMOR_MAX_STRING,"mapping: %s already defined within this table (%s)",cmor_tables[cmor_ntables].mappings[n].id,cmor_tables[cmor_ntables].table_id);
	      cmor_handle_error(msg,CMOR_CRITICAL);
	    };

	  /* init the variable def */
	  cmor_init_grid_mapping(&cmor_tables[cmor_ntables].mappings[cmor_tables[cmor_ntables].nmappings],word2);
	}
	else { /* nothing knwon we will not be setting any attributes! */
	  snprintf(msg,CMOR_MAX_STRING,"unknown section: %s, for table: %s",word,cmor_tables[cmor_ntables].table_id);
	  cmor_handle_error(msg,CMOR_WARNING);
	  do_dataset=0;
	  do_var=0;
	  do_axis=0;
	  do_mapping=0;
	}
	/*printf("ok now i is: --%c--\n",i);*/
    }
      else { /* just a commented out line, let's skip it */
	/*printf("all right skiping line\n");*/
	while(i!='\n') i = wfgetc(table_file);
	i = wfgetc(table_file);
	while((i==' ') || (i=='\n') || (i=='\t')) i = wfgetc(table_file);
	/*printf("ok now i is: --%c--\n",i);*/
      }
    }
    if (i==EOF) break;
    /* ok here we must have a word then, let's read it and it's value */
    n=0;
    while(i!=':'){
      word[n]=i;
      i = wfgetc(table_file);
      n+=1;
    }
    i = wfgetc(table_file);
    word[n]='\0';
    while (i==' ' || i=='\t') i = wfgetc(table_file);
    n=0;
    while((i!='\n') && (i!='!')) {
      word2[n]=i;
      i = wfgetc(table_file);
      n+=1;
    }
    word2[n]='\0';
    n=strlen(word2);
    for (n=strlen(word2)-1;n>-1;n--) {
      if (word2[n]==' ' || word2[n]=='\t' ) word2[n]='\0';
      else break;
    }
    /* finishes the line */
    while (i!='\n' && i!=EOF) i = wfgetc(table_file);
    /*printf("got entry: %s with value %s, var: %i, axis: %i, dat: %i\n",word,word2,do_var,do_axis,do_dataset);*/
    /* First check for table/dataset mode values */
    if (do_dataset==1) {
      if (cmor_set_dataset_att(&cmor_tables[cmor_ntables],word,word2)==1) {
	cmor_pop_traceback();
	return 1; /* sometihng bad might happen */
      }
    }
    else if (do_var==1) {
      cmor_set_var_def_att(&cmor_tables[cmor_ntables].vars[cmor_tables[cmor_ntables].nvars],word,word2);
    }
    else if (do_axis ==1){
      cmor_set_axis_def_att(&cmor_tables[cmor_ntables].axes[cmor_tables[cmor_ntables].naxes],word,word2);
    }
    else if (do_mapping==1) {
      cmor_set_mapping_attribute(&cmor_tables[cmor_ntables].mappings[cmor_tables[cmor_ntables].nmappings],word,word2);
    }
    else {
      snprintf(msg,CMOR_MAX_STRING,"attribute for unknown section: %s,%s (table: %s)",word,word2,cmor_tables[cmor_ntables].table_id);
      cmor_handle_error(msg,CMOR_WARNING);
      /*printf("attribute for unknown section\n");*/
    }
  }
  fclose(table_file);
  *table_id = cmor_ntables;
  strcpy(cmor_tables[cmor_ntables].path,table);
  CMOR_TABLE = cmor_ntables;
  cmor_pop_traceback();
  return 0;
}
