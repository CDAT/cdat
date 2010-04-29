#ifdef HAVE_QL
#include "cdmsint_new.h"
#include "cdunifint.h"
#include <stdlib.h>
#include <string.h>


/*********************************************************************
 * Function to process ':attribute ...' card for ds, dim, var structs.

 * Note: A specialized sub-function of 'Alter' ds instruction.
 *********************************************************************/


void alt_addatt( cdms_card *line,  /* struct metafile line */
                 cdHd      *hd,    /* ds, dim, var struct */
                 int       idx,    /* input symbol index */
                 int       flg )   /* (0 no) prefix att name flag */
{
  int       i, j, k;
  char      aa[120];
  void      *v;
  cdAtt_new *att;


 /*---------------------------------
  * Prefix name with "psql_"
  *---------------------------------*/


  j = idx;
  k = line->idx_sym[j];

  if( flg == 0 )
     aa[0] = '\0';
  else
     strcpy( aa, "psql_" );

  strcat( aa, &line->asc_line[k] );


 /*---------------------------------
  * If attribute already defined delete it.
  *---------------------------------*/


  if( line->cls_sym[j] == 'a' )
  {
     att = scn_lnk_list( aa, id_cdAtt, hd );

     if( att != NULL )
     {
        delete_struct( (cdHd *) att );
           err_r( );
     }
  }


 /*---------------------------------
  * Create attribute under struct.
  *---------------------------------*/


  att       = cre_struct( id_cdAtt, hd );
  att->name = cr_asc_cpy( aa );


 /*---------------------------------
  * Set attribute value from alter-card.
  *---------------------------------*/


  j++;

  if( line->cls_sym[j] == ';' )
  {
    /*******nothing after name, so copy name as attribute value*******/
     att->length   = strlen( att->name );
     att->datatype = cdChar;
     att->values   = cr_asc_cpy( att->name );
  }
  else
  {
     if( line->cls_sym[j] == '=' )
        j++;

     v = meta_after_eq( j, line, &att->datatype, &att->length );
            err_r( );

     att->values = v;
  }

  return;
}


/*********************************************************************
 * Function to process ':dimension ...' within ds, dim, var. alter 

 * Note: A specialized sub-function of 'Alter' ds instruction.
 *********************************************************************/


void alt_adddim( cdms_card *line,  /* struct metafile line */
                 cdHd      *hd,    /* dataset struct */
                 int       flg )   /* 1 update, 0 check */
{
  int       i, j, k, l, n;
  cdCheck   **cklst;
  cdDim_new *dim;


 /*---------------------------------
  * Check if dimension already defined.
  *---------------------------------*/


  j = 2;
  if( line->cls_sym[j] == 'i' )
     j++;

  if( line->cls_sym[j] == 'a' )
  {
     k = line->idx_sym[j];

     dim = scn_lnk_list( &line->asc_line[k], id_cdDim, hd );

     if( dim != NULL && flg == 0 )
        return;
  }


 /*---------------------------------
  * Create dimension struct.
  *---------------------------------*/


  if( dim == NULL )
  {
     dim       = cre_struct( id_cdDim, hd );
     dim->name = cr_asc_cpy( &line->asc_line[k] );
  }


 /*---------------------------------
  * Set dimension struct from alter-card.
  *---------------------------------*/


  if( line->cls_sym[j+1] == ':' && line->cls_sym[j+2] == 'a' )
  {
     l   = line->idx_sym[j+2];
     ck_nam_fnd( id_cdDim, &line->asc_line[k],
                 &line->asc_line[l], &n, &cklst );
        err_r( );
  }
  else
  {
     ck_nam_fnd( id_cdDim, &line->asc_line[k], NULL, &n, &cklst );
        err_r( );
  }

  if( n == 1 )
  {
     alt_one_dim( dim, cklst[0] );
        err_r( );
  }
  else if( n > 1 )
  {
     sprintf( PSQL_MSG, "%s dimension has %d alter sections\n",
              dim->name, n );
     wrt_msg( );
  }

  free( cklst );
  return;
}


/*********************************************************************
 * Function to process ':variable ...' within ds, dim, var. alter

 * Note: A specialized sub-function of 'Alter' ds instruction.
 *********************************************************************/


void alt_addvar( cdms_card *line,  /* struct metafile line */
                 cdHd      *hd,    /* dataset struct */
                 int       flg )   /* 1 update, 0 check */
{
  int       i, j, k, l, n;
  cdCheck   **cklst;
  cdVar_new *var;


 /*---------------------------------
  * Check if variable already defined.
  *---------------------------------*/


  j = 2;
  if( line->cls_sym[j] == 'i' )
     j++;

  if( line->cls_sym[j] == 'a' )
  {
     k = line->idx_sym[j];

     var = scn_lnk_list( &line->asc_line[k], id_cdVar, hd );

     if( var != NULL && flg == 0 )
        return;
  }


 /*---------------------------------
  * Create variable struct.
  *---------------------------------*/


  if( var == NULL )
  {
     var       = cre_struct( id_cdVar, hd );
     var->name = cr_asc_cpy( &line->asc_line[k] );
  }


 /*---------------------------------
  * Set variable struct from alter-card.
  *---------------------------------*/


  if( line->cls_sym[j+1] == ':' && line->cls_sym[j+2] == 'a' )
  {
     l   = line->idx_sym[j+2];
     ck_nam_fnd( id_cdVar, &line->asc_line[k],
                 &line->asc_line[l], &n, &cklst );
        err_r( );
  }
  else
  {
     ck_nam_fnd( id_cdVar, &line->asc_line[k], NULL, &n, &cklst );
        err_r( );
  }


  if( n == 1 )
  {
     alt_one_var( var, cklst[0] );
        err_r( );
  }
  else if( n > 1 )
  {
     sprintf( PSQL_MSG, "%s variable has %d alter sections\n",
              var->name, n );
     wrt_msg( );
  }

  free( cklst );
  return;
}


/*********************************************************************
 * Function to replace patterns in alterfile cards.

 * NOTE: Patterns given as first symbols on psql 'alter' command.
 * NOTE: Alterfile card patterns are *arg1*, *arg2*, ...
 *********************************************************************/


void alt_arg_pat( char *out,  /* pattern replaced card */
                  char *in )  /* input original alterfile card */
{
  char   *aa, *bb, *cc, *dd, incpy[240], msg[240];
  time_t t;


 /*---------------------------------
  * Check input for '*arg' patterns.
  *---------------------------------*/


  if( strlen( in ) < 8 || strstr( in, "*arg" ) == NULL )
  {
     strcpy( out, in );
     return;
  }

  strcpy( incpy, in );
  aa     = incpy;
  msg[0] = '\0';


 /*---------------------------------
  * Scan across input searching for '*arg' patterns.
  *---------------------------------*/


  while( strlen( aa ) > 0 )
  {
    /*---------------------------------
     * Search for '*arg' pattern.
     *---------------------------------*/
     bb = strstr( aa, "*arg" );

     if( bb == NULL )
        break;


    /*---------------------------------
     * '*arg0*' pattern -- current time as ascii string.

     * Note: ctime on linux puts in a '\n'.
     *---------------------------------*/
     if( bb[4] == '0' && bb[5] == '*' )
     {
        bb[0] = '\0';
        strcat( msg, aa );

        t  = time( NULL );
        cc = ctime( &t );

        dd = strchr( cc, '\n' );
        if( dd != NULL )
           dd[0] = '\0';

        strcat( msg, cc );
        free( cc );

        aa = &bb[6];
     }


    /*---------------------------------
     * '*arg1*' pattern -- from 'alter' command.
     *---------------------------------*/
     else if( bb[4] == '1' && bb[5] == '*' &&
         DB_ROOT_ADR->f->ALT_PAT != NULL )
     {
        bb[0] = '\0';
        strcat( msg, aa );

        strcat( msg, DB_ROOT_ADR->f->ALT_PAT );

        aa = &bb[6];
     }


    /*---------------------------------
     * '*arg..' pattern -- no pattern replacement required.
     *---------------------------------*/
     else
     {
        bb[3] = '\0';
        strcat( msg, aa );
        bb[3] = 'g';
        aa = &bb[3];
     }
  }


 /*---------------------------------
  * Set output.
  *---------------------------------*/


  strcat( msg, aa );
  strcpy( out, msg );
  return;
}


/*********************************************************************
 * Function to process ':attconcat ...' card for ds, dim, var structs.

 * Note: A specialized sub-function of 'Alter' ds instruction.
 *********************************************************************/


void alt_att_concat( cdms_card *line,  /* struct metafile line */
                     cdHd      *hd,    /* ds, dim, var struct */
                     int       idx )   /* input symbol index */
{
  int       i, j, k;
  char      *aa, *bb;
  cdAtt_new *att;


 /*---------------------------------
  * Verify attribute defined and is type cdChar.
  *---------------------------------*/


  att = NULL;
  j   = idx;

  if( line->cls_sym[j] == 'a' )
  {
     k = line->idx_sym[j];

     att = scn_lnk_list( &line->asc_line[k], id_cdAtt, hd );
  }

  if( att == NULL || att->datatype != cdChar )
     return;


 /*---------------------------------
  * Get string to append.
  *---------------------------------*/


  j++;
  if( line->cls_sym[j] == '=' )
     j++;

  k  = att->length;

  aa = (char *) meta_after_eq( j, line, &att->datatype, &att->length );
         err_r( );


 /*---------------------------------
  * Append string to attribute value.
  *---------------------------------*/


  k  += strlen( aa ) + 1;

  bb = (char *) malloc( k + 1 );

  sprintf( bb, "%s\n%s", (char *) att->values, aa );

  free( aa );
  free( att->values );

  att->length = k;
  att->values = bb;

  return;
}


/*********************************************************************
 * Function to process ':delatt ...' card for ds, dim, var structs.

 * Note: A specialized sub-function of 'Alter' ds instruction.
 *********************************************************************/


void alt_delatt( cdms_card *line,  /* struct metafile line */
                 cdHd      *hd )   /* input symbol index */
{
  int       i, j, k;
  void      **v;
  cdAtt_new *att, *atta;


 /*---------------------------------
  * Get input struct's linked list of attributes.
  *---------------------------------*/


  v = fnd_lnk_list( id_cdAtt, hd );

  if( v == NULL || *v == NULL )
     return;

  att = (cdAtt_new *) *v;


 /*---------------------------------
  * Delete all attributes attached to input struct.
  * Note: need 'file','path' for reading dim.,var. data arrays.
  *---------------------------------*/


  if( line->num_sym == 2 || line->cls_sym[2] == ';' )
  {
     while( att )
     {
        atta = att->next;

        if( !strcmp( att->name, "psql_file" ) ||
            !strcmp( att->name, "psql_path" ) )
           ;
        else
        {
           delete_struct( (cdHd *) att );
              err_r( );
        }

        att = atta;
     }

     return;
  }


 /*---------------------------------
  * Delete list of attributes attached to input struct.
  *---------------------------------*/


  for( i=2; i < line->num_sym; i++ )
  {
     if( line->cls_sym[i] == 'a' )
     {
        k = line->idx_sym[i];

        att = scn_lnk_list( &line->asc_line[k], id_cdAtt, hd );

        if( att != NULL )
        {
           delete_struct( (cdHd *) att );
              err_r( );
        }
     }
  }

  return;
}


/*********************************************************************
 * Function: execute a alter-file 'delvar', 'deldim' attribute.
 *********************************************************************/


void alt_del_node( cdAtt_new *att )  /* attribute struct */
{
  int       j;
  char      **vlst, aa[120];
  cdAtt_new *atta;
  cdDim_new *dim;
  cdVar_new *var;
  cdHd      *hd;

  if( att == NULL )
     return;

  hd = (cdHd *) att->above;


 /*---------------------------------
  * Prepare to execute 'deldim' 'alterfile' line.

  * Note: new dimension name is prefixed with 'psql_'
  *    Original name stored in new 'psql_name' attribute.
  *---------------------------------*/


  if( !strcmp( att->name, "psql_deldim" ) )
  {
     if( att->datatype == cdChar )
     {
        dim = nam_fnd( (char *) att->values, id_cdDim, hd );

        if( dim != NULL )
        {
           atta           = cre_struct( id_cdAtt, (cdHd *) dim );
           atta->name     = cr_asc_cpy( "psql_name" );
           atta->length   = strlen( dim->name );
           atta->datatype = cdChar;
           atta->values   = cr_asc_cpy( dim->name );

           strcpy( aa, "psql_" );
           strcat( aa, dim->name );
           free( dim->name );
           dim->name = cr_asc_cpy( aa );
        }
     }

     else if( att->datatype == cdCharTime )
     {
        vlst = att->values;

        for( j=0; j < att->length; j++ )
        {
           dim = nam_fnd( vlst[j], id_cdDim, hd );

           if( dim!= NULL )
           {
              atta           = cre_struct( id_cdAtt, (cdHd *) dim );
              atta->name     = cr_asc_cpy( "psql_name" );
              atta->length   = strlen( dim->name );
              atta->datatype = cdChar;
              atta->values   = cr_asc_cpy( dim->name );

              strcpy( aa, "psql_" );
              strcat( aa, dim->name );
              free( dim->name );
              dim->name = cr_asc_cpy( aa );
           }
        }
     }

     return;
  }


 /*---------------------------------
  * Prepare to execute 'delvar' 'alterfile' line.

  * Note: new variable name is prefixed with 'psql_'
  *    Original name stored in new 'psql_name' attribute.
  *---------------------------------*/


  if( !strcmp( att->name, "psql_delvar" ) )
  {
     if( att->datatype == cdChar )
     {
        var = nam_fnd( (char *) att->values, id_cdVar, hd );

        if( var != NULL )
        {
           atta           = cre_struct( id_cdAtt, (cdHd *) var );
           atta->name     = cr_asc_cpy( "psql_name" );
           atta->length   = strlen( var->name );
           atta->datatype = cdChar;
           atta->values   = cr_asc_cpy( var->name );

           strcpy( aa, "psql_" );
           strcat( aa, var->name );
           free( var->name );
           var->name = cr_asc_cpy( aa );
        }
     }

     else if( att->datatype == cdCharTime )
     {
        vlst = att->values;

        for( j=0; j < att->length; j++ )
        {
           var = nam_fnd( vlst[j], id_cdVar, hd );

           if( var!= NULL )
           {
              atta           = cre_struct( id_cdAtt, (cdHd *) var );
              atta->name     = cr_asc_cpy( "psql_name" );
              atta->length   = strlen( var->name );
              atta->datatype = cdChar;
              atta->values   = cr_asc_cpy( var->name );

              strcpy( aa, "psql_" );
              strcat( aa, var->name );
              free( var->name );
              var->name = cr_asc_cpy( aa );
           }
        }
     }

     return;
  }
  return;
}


/*********************************************************************
 * Function: alter a dataset (ie. file) from alter-file directives.
 *********************************************************************/


void alter_dataset( cdHd *cur )  /* current struct */
{
  int        n;
  cdDset_new *ds;
  cdVar_new  *var, *vara;
  cdDim_new  *dim, *dima;
  cdCheck    **cklst;


 /*---------------------------------
  * Set pointer to input struct.
  *---------------------------------*/


  if( cur == NULL || cur->id != id_cdDset )
     return;

  ds   = (cdDset_new *) cur;

  for( dima=ds->dims; dima->next != NULL; dima=dima->next )
     ;

  for( vara=ds->vars; vara->next != NULL; vara=vara->next )
     ;


 /*---------------------------------
  * See if alter-file has 1 directive section for dataset.
  *---------------------------------*/


  ck_nam_fnd( id_cdDset, NULL, NULL, &n, &cklst );
     err_r( );

  if( n == 1 )
  {
     alt_one_ds( ds, cklst[0] );
        err_r( );
  }
  else if( n > 1 )
  {
     sprintf( PSQL_MSG, "%d alter sections for dataset\n", n );
     wrt_msg( );
  }

  free( cklst );


 /*---------------------------------
  * Loop over dimensions in file.
  * See if alter-file has 1 directive section for dim.
  *---------------------------------*/


  for( dim=ds->dims; dim; dim=dim->next )
  {
     ck_nam_fnd( id_cdDim, dim->name, NULL, &n, &cklst );
        err_r( );

     if( n == 1 )
     {
        alt_one_dim( dim, cklst[0] );
           err_r( );
     }
     else if( n > 1 )
     {
        sprintf( PSQL_MSG, "%s dimension has %d alter sections\n",
                 dim->name, n );
        wrt_msg( );
     }

     free( cklst );

     if( dim == dima )
        break;
  }


 /*---------------------------------
  * Loop over variables in file.
  * See if alter-file has 1 directive section for var.
  *---------------------------------*/


  for( var=ds->vars; var; var=var->next )
  {
     ck_nam_fnd( id_cdVar, var->name, NULL, &n, &cklst );
        err_r( );

     if( n == 1 )
     {
        alt_one_var( var, cklst[0] );
           err_r( );
     }
     else if( n > 1 )
     {
        sprintf( PSQL_MSG, "%s variable has %d alter sections\n",
                 var->name, n );
        wrt_msg( );
     }

     free( cklst );

     if( var == vara )
        break;
  }


 /*---------------------------------
  * Execute non-data-value modification 'alterfile' directives.

  * Note: The 'alterfile' directives which actually modify
  *    dim-coord and var-data values are executed as the
  *    data, or data subset, is read from disc (ie. wrtadd,
  *    wrttype, newmiss wrtmult, wrtshape).
  *---------------------------------*/


  alt_wrt_cards( ds, "psql_delvar" );
     err_r( );

  alt_wrt_cards( ds, "psql_deldim" );
     err_r( );

  alt_wrt_cards( ds, "psql_wrtname" );
     err_r( );

  alt_wrt_cards( ds, "psql_wrttype" );
     err_r( );

  alt_wrt_cards( ds, "psql_wrtshape" );
     err_r( );
  return;
}


/*********************************************************************
 * Function: to process ':timmomid', ':timmobound', ':timmoabs'
 * cards for time dimension and bounds_time,abs_time variables.

 * Note: A specialized sub-function of 'Alter' ds instruction.
 *********************************************************************/


void alt_midmonth( cdHd *hd,     /* dimension struct */
                   char *nmod,   /* timmomid,timmobound,timmoabs */
                   char *ncal,   /* name time calender */
                   char *ntim )  /* name time dimension */

{
  int       m, ct, yr, mo;
  char      *uni, atime[CD_MAX_PATH];
  long      i, j, l, lvv, *llabs;
  double    d1, d2, *dd, *ddtim, *ddbd;
  cdType    typ;
  cdDim_new *dim, *dima;
  cdVar_new *var;
  cdAtt_new *atta;
  void      *vv, *pp;


 /*---------------------------------
  * Get calender id for the time routines.
  *---------------------------------*/


  ct = typ_of_calendar( ncal );


 /*---------------------------------
  * Get the reference time dimension.
  *---------------------------------*/


  dima = nam_fnd( ntim, id_cdDim, hd );
     err_t( dima == NULL, "alt_midmonth: dimension not in file" );

  l = dima->length;

  if( dima->data == NULL )
  {
     vv = rd_dim_array( dima, -1, -1 );
             err_r( );

     dd = ary_trans( dima->datatype, l, vv, cdDouble );
             err_r( );

     free( vv );
  }
  else
  {
     dd = ary_trans( dima->datatype, l, dima->data, cdDouble );
             err_r( );
  }

  atta = scn_lnk_list( "units", id_cdAtt, (cdHd *) dima );
     err_t( atta == NULL, "alt_midmonth: dimension has no units" );

  uni = (char *) atta->values;


 /*---------------------------------
  * Generate mid, bound and abs output arrays.
  *---------------------------------*/


  llabs = (long *) malloc( l * sizeof(long) );
  ddtim = (double *) malloc( l * sizeof(double) );
  ddbd  = (double *) malloc( (l*2) * sizeof(double) );

  if( llabs == NULL || ddtim == NULL || ddbd == NULL )
     err_x( "alt_midmonth: trouble getting memory" );


  for( i=0, j=0; i < l; i++ )
  {
     cdRel2Char( ct, uni, dd[i], atime );
     m = sscanf( atime, "%d%*1c%d", &yr,&mo );

     sprintf( atime, "%4d-%d-%d", yr, mo, 1 );
     cdChar2Rel(ct, atime, uni, &d1 );

     if( mo == 12 )
        sprintf( atime, "%4d-%d-%d", yr+1, 1, 1 );
     else
        sprintf( atime, "%4d-%d-%d", yr, mo+1, 1 );

     cdChar2Rel(ct, atime, uni, &d2 );

     ddbd[j++] = d1;
     ddbd[j++] = d2;
     ddtim[i]  = d1 + (( d2 - d1 ) * 0.5);
     llabs[i]  = yr*100 + mo;
  }


 /*---------------------------------
  * Decide which output array to use.
  *---------------------------------*/


  if( !strcmp( "timmomid", nmod ) )
  {
     vv  = (void *) ddtim;
     lvv = l;
     typ = cdDouble;
  }

  else if( !strcmp( "timmobound", nmod ) )
  {
     vv  = (void *) ddbd;
     lvv = 2 * l;
     typ = cdDouble;
  }

  else if( !strcmp( "timmoabs", nmod ) )
  {
     vv  = (void *) llabs;
     lvv = l;
     typ = cdLong;
  }


 /*---------------------------------
  * Set output dim,var data values.
  *---------------------------------*/


  if( hd->id == id_cdDim )
  {
     dim = (cdDim_new *) hd;

     if( dim->data != NULL )
        free( dim->data );

     if( lvv != dim->length )
        err_x( "alt_midmonth: dimension length problems" );

     dim->data = ary_trans( typ, lvv, vv, dim->datatype );
                    err_r( );
  }

  else if( hd->id == id_cdVar )
  {
     var = (cdVar_new *) hd;

     if( var->data != NULL )
        free( var->data );

     if( lvv != var->length )
        err_x( "alt_midmonth: variable length problems" );

     var->data = ary_trans( typ, lvv, vv, var->datatype );
                    err_r( );
  }

  free( dd );
  free( llabs );
  free( ddtim );
  free( ddbd );
  return;
}


/*********************************************************************
 * Function: execute alter 'wrtshape' attribute.
 *********************************************************************/


void alt_mod_shape( cdAtt_new *att )  /* attribute struct */
{
  int       i, j, k, m, n, *ord, *l1;
  char      *aa, **nlst;
  long      l;
  cdVar_new *var;
  cdDim_new **alst, **blst;
  cdAtt_new *atta;
  cdTmp     *tmp, *tmpa;
  cdDim_new *dim;


 /*---------------------------------
  * Crack attribute value -- "(time,level,latitude,longitude)"
  *---------------------------------*/


  if( att == NULL || att->id_above != id_cdVar )
     return;

  if( att->datatype == cdChar )
  {
     m  = 1;
     aa = att->values;
  }
  else
  {
     m    = att->length;
     nlst = att->values;
     aa   = nlst[0];
  }


 /*---------------------------------
  * Prepare to insert new dims to var->dim linked list.
  *---------------------------------*/


  var  = (cdVar_new *) att->above;
  tmpa = var->dim;
  l    = var->length;
  n    = var->ndims;

  l1    = (int *) malloc( sizeof( int ) );
  ord   = (int *) malloc( n * sizeof( int ) );
  alst = malloc( n * sizeof( cdDim_new ) );
  blst = malloc( m * sizeof( cdDim_new ) );

  if( l1 == NULL || ord == NULL || alst == NULL || blst == NULL )
     err_x( "alt_mod_shape: trouble getting memory" );

  for( i=0; i < n; i++ )
     ord[i] = -1;

  for( tmp=var->dim, i=0; i < var->ndims; tmp=tmp->next, i++ )
     alst[i] = tmp->want;

  *l1 = var->ndims;

  atta           = cre_struct( id_cdAtt, (cdHd *) var );
  atta->name     = cr_asc_cpy( "psql_ndims" );
  atta->length   = 1;
  atta->datatype = cdInt;
  atta->values   = l1;


 /*---------------------------------
  * Add new variable dimensionality.
  *---------------------------------*/

  var->dim    = NULL;
  var->ndims  = 0;
  var->length = 1;

  for( j=0, i=0; j < m; j++ )
  {
     if( m > 1 )
        aa = nlst[j];

     dim = nam_fnd( aa, id_cdDim, (cdHd *) var );

     if( dim == NULL )
        err_x( "alt_mod_shape: no-def  shape-dimension" );

     tmp           = cre_struct( id_cdTmp, (cdHd *) var );
     tmp->id_want  = id_cdDim;
     tmp->nam_want = cr_asc_cpy( aa );
     tmp->want     = dim;

     var->ndims++;
     var->length *= dim->length;

     blst[i++] = dim;
  }

  tmp->next = tmpa;

  if( var->length != l )
     err_x( "alt_mod_shape: bad wrtshape variable length" );


 /*---------------------------------
  * Set 'dimorder' dimension transposing.
  *---------------------------------*/


  for( i=var->ndims-1; i > -1; i-- )
  {
     dim = blst[i];
     k   = 0;

     for( j=0; j < n; j++ )
     {
        if( dim == alst[j] )
        {
           ord[j] = i;
           k      = 5;
        }
     }

     if( k == 0 && dim->length > 1 )
        err_x( "alt_mod_shape: bad wrtshape dim mapping" );
  }

  atta           = cre_struct( id_cdAtt, (cdHd *) var );
  atta->name     = cr_asc_cpy( "psql_dimorder" );
  atta->length   = n;
  atta->datatype = cdInt;
  atta->values   = ord;

  free( alst );
  free( blst );
  return;
}


/*********************************************************************
 * Function to process ':newunits = ...' card for time-dim struct.

 * Note: A specialized sub-function of 'Alter' ds instruction.
 *********************************************************************/


void alt_newtim( cdms_card *line,  /* struct metafile line */
                 cdDim_new *dim,   /* time-dim struct */
                 int       idx )   /* input symbol index */
{
  int       i, j, k, ct;
  long      l;
  double    t, *dd;
  void      *v;
  cdAtt_new *att, *atta;


 /*---------------------------------
  * Load time coordinates into memory.
  *---------------------------------*/


  if( dim->data == NULL ) 
  {
     dim->data = get_coord( dim, cdDouble, &l );
                    err_r( );

     dim->datatype = cdDouble;
  }


 /*---------------------------------
  * Get calendar to use.
  *---------------------------------*/


  att = nam_fnd( "calendar", id_cdAtt, (cdHd *) dim );

  if( att != NULL )
     ct = typ_of_calendar( (char *) att->values );
  else
     ct = typ_of_calendar( NULL );


 /*---------------------------------
  * Get current 'units' attribute.
  *---------------------------------*/


  atta = scn_lnk_list( "units", id_cdAtt, (cdHd *) dim );

  if( atta == NULL )
     err_x( "alt_newtim: bad time dimension" );


 /*---------------------------------
  * Get new 'units' attribute.
  *---------------------------------*/


  att       = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name = cr_asc_cpy( "units" );

  j = idx;

  if( line->cls_sym[j] == '=' )
     j++;

  v = meta_after_eq( j, line, &att->datatype, &att->length );
         err_r( );

  att->values = v;


 /*---------------------------------
  * Convert coordinates to new basetime.
  *---------------------------------*/


  if( dim->datatype != cdDouble )
     err_x( "alt_newtim: time dim. must be double" );

  dd = dim->data;

  for( i=0; i < dim->length; i++ )
  {
     cdRel2Rel( ct, (char *) atta->values, dd[i],
                (char *) att->values, &t );

     dd[i] = t;
  }


 /*---------------------------------
  * Delete original 'units' attribute.
  *---------------------------------*/


  delete_struct( (cdHd *) atta );
     err_r( );

  return;
}


/*********************************************************************
 * Function: Alter 'dimension' struct with 'dimension' alter-struct
 *********************************************************************/


void alt_one_dim( cdDim_new *dim,  /* dimension struct */
                  cdCheck   *ck )  /* dimension alter struct */
{
  int           i, j, k, m, num_sym, len_str;
  long          l, n;
  cdAtt_new     *att;
  cdms_card     *line;
  cdms_pql_list *pql;
  cdType        typ;
  CuType        cutyp;


  mem_cdms_card( &line );
     err_r( );

  mem_cdms_pql_list( &pql );
     err_r( );

  l            = 0;
  pql->length  = 1;
  pql->list[0] = dim;


 /*---------------------------------
  * Loop over cards of alter dimension struct.
  *---------------------------------*/


  for( i=0; i < ck->ncard; i++ )
  {
    /*---------------------------------
     * Crack card into symbols.
     *---------------------------------*/


     alt_arg_pat( line->asc_line, ck->card[i] );
     len_str       = strlen( line->asc_line );
     line->num_sym = 0;

     num_sym = meta_str_sym( line, 0, len_str );

     ins_asym_eos( line );

     pql_key_sym( line );

     line->cls_sym[num_sym] = ';';
     line->num_sym          = ++num_sym;


    /*---------------------------------
     * Process ': ascii ....' cards.
     *---------------------------------*/


     if( num_sym > 1 && line->cls_sym[0] == ':' &&
         line->cls_sym[1] == 'a' )
     {
       /*---------------------------------
        * :delatt or :delatt name, name, name
        *    delete dimension attribute.
        *---------------------------------*/

        k = line->idx_sym[1];

        if( !strcmp( &line->asc_line[k], "delatt" ) )
        {
           alt_delatt( line, (cdHd *) dim );
              err_r( );
        }


       /*---------------------------------
        * :wrtname abc
        *    set dimension attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "wrtname" ) )
        {
           alt_addatt( line, (cdHd *) dim, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :wrttype float, double, int, long
        *    convert type of dimension.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "wrttype" ) )
        {
           alt_addatt( line, (cdHd *) dim, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :wrtadd 3000.0
        *    set dimension attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "wrtadd" ) )
        {
           alt_addatt( line, (cdHd *) dim, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :wrtmult -2.0
        *    set dimension attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "wrtmult" ) )
        {
           alt_addatt( line, (cdHd *) dim, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :newmiss = oldval, newval, tol(default=1.0e-6), ...repeat
        *    set dimension attribute.
        * pt1 = oldval - ( tol * fabs( oldval ) )
        * pt2 = oldval + ( tol * fabs( oldval ) )
        * if( v > pt1 && v < pt2 ) : v = newval
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "newmiss" ) )
        {
           alt_addatt( line, (cdHd *) dim, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :wrtinvert
        *    set dimension attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "wrtinvert" ) )
        {
           alt_addatt( line, (cdHd *) dim, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :deldim orgname, orgname, ....
        *    set dimension attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "deldim" ) )
        {
           alt_addatt( line, (cdHd *) dim, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :delvar orgname, orgname, ....
        *    set dimension attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "delvar" ) )
        {
           alt_addatt( line, (cdHd *) dim, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :attconcat name = "...."
        *    append to ascii dimension attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "attconcat" ) )
        {
           alt_att_concat( line, (cdHd *) dim, 2 );
              err_r( );
        }


       /*---------------------------------
        * :newunits = "days since 1979-1-1 0"
        *    convert time coordinates to new basetime.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "newunits" ) )
        {
           alt_newtim( line, dim, 2 );
              err_r( );
        }


       /*---------------------------------
        * :values = v,v,v,v,v,v,v
                    v,v,v,v,v,v,v ;
        *---------------------------------*/
        else if( !strcmp( &line->asc_line[k], "values" ) )
        {
           j = 2;
           if( line->cls_sym[j] == '=' )
              j++;

           l = 0;
           l = alt_values( line, j, (cdHd *) dim, l );
                  err_r( );
        }


        else
        {
           sprintf( PSQL_MSG, "bad card -- %s\n", ck->card[i] ); 
           wrt_msg();
        }
     }


    /*---------------------------------
     * Process ': keyname ....' cards.
     *---------------------------------*/


     else if( num_sym > 2 && line->cls_sym[0] == ':' &&
              line->cls_sym[1] == 'k' )
     {
       /*---------------------------------
        * :attribute name = value
        *    set dimension attribute.
        *---------------------------------*/
        if( line->len_sym[1] == i_attribute )
        {
           alt_addatt( line, (cdHd *) dim, 2, 0 );
              err_r( );
        }


       /*---------------------------------
        * :length int
        *    set length of dimension.
        *---------------------------------*/
        else if( line->len_sym[1] == i_length )
        {
           j = 2;
           if( line->cls_sym[j] == '=' )
              j++;

           k = line->idx_sym[j];

           dim->length = atol( &line->asc_line[k] );
        }


       /*---------------------------------
        * :type float
        *    set type of dimension values.
        *---------------------------------*/
        else if( line->len_sym[1] == i_type )
        {
           j = 2;
           if( line->cls_sym[j] == '=' )
              j++;

           k = line->idx_sym[j];

           dim->datatype = typ_from_ascii( &line->asc_line[k] );
                              err_r( );
        }


       /*---------------------------------
        * :dimension v8:tg1
        *    if dimension doesn't exist, create it, else nothing.
        *---------------------------------*/
        else if( line->len_sym[1] == i_dimension )
        {
           k = line->idx_sym[2];

           alt_adddim( line, (cdHd *) dim->above, 0 );
              err_r( );
        }


       /*---------------------------------
        * :variable v8:tg1
        *    if variable doesn't exist create it, else nothing.
        *---------------------------------*/
        else if( line->len_sym[1] == i_variable )
        {
           k = line->idx_sym[2];

           alt_addvar( line, (cdHd *) dim->above, 0 );
              err_r( );
        }
     }


    /*---------------------------------
     * Process continuation of ':values = ....' card.
     *---------------------------------*/


     else
     {
        l = alt_values( line, 0, (cdHd *) dim, l );
               err_r( );
     }
  }

  rel_cdms_card( line );

  rel_cdms_pql_list( pql );

  return;
}


/*********************************************************************
 * Function: Alter 'dataset' struct with 'dataset' alter-struct
 *********************************************************************/


void alt_one_ds( cdDset_new *ds,   /* dataset struct */
                 cdCheck    *ck )  /* dataset alter struct */
{
  int           i, j, k, n, num_sym, len_str;
  cdAtt_new     *att;
  cdms_card     *line;
  cdms_pql_list *pql;


  mem_cdms_card( &line );
     err_r( );

  mem_cdms_pql_list( &pql );
     err_r( );

  pql->length  = 1;
  pql->list[0] = ds;


 /*---------------------------------
  * Loop over cards of alter dataset struct.
  *---------------------------------*/


  for( i=0; i < ck->ncard; i++ )
  {
    /*---------------------------------
     * Crack card into symbols.
     *---------------------------------*/


     alt_arg_pat( line->asc_line, ck->card[i] );
     len_str        = strlen( line->asc_line );
     line->num_sym  = 0;

     num_sym = meta_str_sym( line, 0, len_str );

     ins_asym_eos( line );

     pql_key_sym( line );

     line->cls_sym[num_sym] = ';';
     line->num_sym          = ++num_sym;


    /*---------------------------------
     * Process ': ascii ....' cards.
     *---------------------------------*/


     if( num_sym > 1 && line->cls_sym[0] == ':' &&
         line->cls_sym[1] == 'a' )
     {
       /*---------------------------------
        * :delatt
        *---------------------------------*/

        k = line->idx_sym[1];

        if( !strcmp( &line->asc_line[k], "delatt" ) )
        {
           alt_delatt( line, (cdHd *) ds );
              err_r( );
        }


       /*--------------------------------- 
        * :deldim orgname, orgname, ....
        *    set dataset attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "deldim" ) )
        { 
           alt_addatt( line, (cdHd *) ds, 1, 1 );
              err_r( );
        }


       /*--------------------------------- 
        * :attconcat name = "...."
        *    append to ascii dataset attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "attconcat" ) )
        { 
           alt_att_concat( line, (cdHd *) ds, 2 );
              err_r( );
        }


       /*--------------------------------- 
        * :delvar orgname, orgname, ....
        *    set dataset attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "delvar" ) )
        {
           alt_addatt( line, (cdHd *) ds, 1, 1 );
              err_r( );
        }


        else
        {
           sprintf( PSQL_MSG, "bad card -- %s\n", ck->card[i] ); 
           wrt_msg();
        }
     }


    /*---------------------------------
     * Process ': keyname ascii ....' cards.
     *---------------------------------*/

     else if( num_sym > 2 && line->cls_sym[0] == ':' &&
              line->cls_sym[1] == 'k' && line->cls_sym[2] == 'a' )
     {
       /*---------------------------------
        * :attribute name = value
        *---------------------------------*/
        if( line->len_sym[1] == i_attribute )
        {
           alt_addatt( line, (cdHd *) ds, 2, 0 );
              err_r( );
        }


       /*---------------------------------
        * :dimension latitude:tg
        *---------------------------------*/
        else if( line->len_sym[1] == i_dimension )
        {
           alt_adddim( line, (cdHd *) ds, 1 );
              err_r( );
        }


       /*---------------------------------
        * :variable ps:tag3
        *---------------------------------*/
        else if( line->len_sym[1] == i_variable )
        {
           alt_addvar( line, (cdHd *) ds, 1 );
              err_r( );
        }


        else
        {
           sprintf( PSQL_MSG, "bad card -- %s\n", ck->card[i] ); 
           wrt_msg();
        }
     }


    /*---------------------------------
     * Unknown card.
     *---------------------------------*/
     else
     {
        sprintf( PSQL_MSG, "bad card -- %s\n", ck->card[i] ); 
        wrt_msg();
     }
  }

  rel_cdms_card( line );
  rel_cdms_pql_list( pql );
  return;
}


/*********************************************************************
 * Function: Alter 'variable' struct with 'variable' alter-struct
 *********************************************************************/


void alt_one_var( cdVar_new *var,  /* variable struct */
                  cdCheck   *ck )  /* variable alter struct */
{
  int           i, j, k, m, num_sym, len_str;
  long          l, n;
  cdAtt_new     *att;
  cdDim_new     *dim;
  cdTmp         *tmp;
  cdms_card     *line;
  cdms_pql_list *pql;
  cdType        typ;
  CuType        cutyp;


  mem_cdms_card( &line );
     err_r( );

  mem_cdms_pql_list( &pql );
     err_r( );

  l            = 0;
  pql->length  = 1;
  pql->list[0] = var;


 /*---------------------------------
  * Loop over cards of alter variable struct.
  *---------------------------------*/


  for( i=0; i < ck->ncard; i++ )
  {
    /*---------------------------------
     * Crack card into symbols.
     *---------------------------------*/


     alt_arg_pat( line->asc_line, ck->card[i] );
     len_str       = strlen( line->asc_line );
     line->num_sym = 0;

     num_sym = meta_str_sym( line, 0, len_str );

     ins_asym_eos( line );

     pql_key_sym( line );

     line->cls_sym[num_sym] = ';';
     line->num_sym          = ++num_sym;


    /*---------------------------------
     * Process ': ascii ....' cards.
     *---------------------------------*/


     if( num_sym > 1 && line->cls_sym[0] == ':' &&
         line->cls_sym[1] == 'a' )
     {
       /*---------------------------------
        * :delatt or :delatt name, name, name
        *    delete variable attribute(s).
        *---------------------------------*/

        k = line->idx_sym[1];

        if( !strcmp( &line->asc_line[k], "delatt" ) )
        {
           alt_delatt( line, (cdHd *) var );
              err_r( );
        }


       /*---------------------------------
        * :wrtname abc
        *    set output variable name.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "wrtname" ) )
        {
           alt_addatt( line, (cdHd *) var, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :wrttype float, double, int, long
        *    convert type of dimension.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "wrttype" ) )
        {
           alt_addatt( line, (cdHd *) var, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :wrtadd 3000.0
        *    set output variable data modification.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "wrtadd" ) )
        {
           alt_addatt( line, (cdHd *) var, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :wrtmult -2.0
        *    set output variable data modification.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "wrtmult" ) )
        {
           alt_addatt( line, (cdHd *) var, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :newmiss = oldval, newval, tol(default=1.0e-6), ...repeat
        *    set variable attribute.
        * pt1 = oldval - ( tol * fabs( oldval ) )
        * pt2 = oldval + ( tol * fabs( oldval ) )
        * if( v > pt1 && v < pt2 ) : v = newval
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "newmiss" ) )
        {
           alt_addatt( line, (cdHd *) var, 1, 1 );
              err_r( );
        }


       /*--------------------------------- 
        * :deldim orgname, orgname, ....
        *    set variable attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "deldim" ) )
        { 
           alt_addatt( line, (cdHd *) var, 1, 1 );
              err_r( );
        }


       /*--------------------------------- 
        * :delvar orgname, orgname, ....
        *    set variable attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "delvar" ) )
        { 
           alt_addatt( line, (cdHd *) var, 1, 1 );
              err_r( );
        }


       /*--------------------------------- 
        * :attconcat name = "...."
        *    append to ascii variable attribute.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "attconcat" ) )
        { 
           alt_att_concat( line, (cdHd *) var, 2 );
              err_r( );
        }


       /*---------------------------------
        * :wrtshape "(time,level,latitude,longitude)"
        *    set output variable shape.
        * Note: May need to erase '"' and re-crack card.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "wrtshape" ) )
        {


          /*---------------------------------
           * Re-crack card without '"' letter.
           *---------------------------------*/


           alt_arg_pat( line->asc_line, ck->card[i] );
           len_str       = strlen( line->asc_line );
           line->num_sym = 0;

           for( j=0; j < len_str; j++ )
           {
              if( line->asc_line[j] == '"' )
                 line->asc_line[j] = ' ';
           }

           num_sym = meta_str_sym( line, 0, len_str );

           ins_asym_eos( line );

           pql_key_sym( line );

           line->cls_sym[num_sym] = ';';
           line->num_sym          = ++num_sym;


          /*---------------------------------
           * Attach 'wrtshape' attribute.
           *---------------------------------*/


           alt_addatt( line, (cdHd *) var, 1, 1 );
              err_r( );
        }


       /*---------------------------------
        * :shape (time,level,latitude,longitude)
        *    set variable shape.
        *---------------------------------*/

        else if( !strcmp( &line->asc_line[k], "shape" ) )
        {
           if( var->dim != NULL )
           {
              for( tmp=var->dim; tmp; )
                 tmp = empty_struct( (cdHd *) tmp );
           }

           var->dim    = NULL;
           var->ndims  = 0;
           var->length = 1;

           for( j=2; j < line->num_sym; j++ )
           {
              if( line->cls_sym[j] == 'a' )
              {
                 k = line->idx_sym[j];

                 dim = nam_fnd( &line->asc_line[k], id_cdDim,
                                (cdHd *) var );

                 if( dim == NULL )
                    err_x( "alt_one_var: no-def  shape-dimension" );

                 tmp           = cre_struct( id_cdTmp, (cdHd *) var );
                 tmp->id_want  = id_cdDim;
                 tmp->nam_want = cr_asc_cpy( &line->asc_line[k] );
                 tmp->want     = dim;

                 var->ndims++;
                 var->length *= dim->length;
              }
           }
        }


       /*---------------------------------
        * :values = v,v,v,v,v,v,v
                    v,v,v,v,v,v,v ;
        *---------------------------------*/
        else if( !strcmp( &line->asc_line[k], "values" ) )
        {
           j = 2;
           if( line->cls_sym[j] == '=' )
              j++;

           l = 0;
           l = alt_values( line, j, (cdHd *) var, l );
                  err_r( );
        }


        else
        {
           sprintf( PSQL_MSG, "bad card -- %s\n", ck->card[i] ); 
           wrt_msg();
        }
     }


    /*---------------------------------
     * Process ': keyname ....' cards.
     *---------------------------------*/


     else if( num_sym > 2 && line->cls_sym[0] == ':' &&
         line->cls_sym[1] == 'k' )
     {
       /*---------------------------------
        * :attribute name = value
        *    set variable attribute.
        *---------------------------------*/
        if( line->len_sym[1] == i_attribute )
        {
           alt_addatt( line, (cdHd *) var, 2, 0 );
              err_r( );
        }


       /*---------------------------------
        * :length int
        *    set length of variable.
        *---------------------------------*/
        else if( line->len_sym[1] == i_length )
        {
           j = 2;
           if( line->cls_sym[j] == '=' )
              j++;

           k = line->idx_sym[j];

           var->length = atol( &line->asc_line[k] );
        }


       /*---------------------------------
        * :ndim int
        *    set ndim of variable.
        *---------------------------------*/
        else if( line->len_sym[1] == i_ndim )
        {
           j = 2;
           if( line->cls_sym[j] == '=' )
              j++;

           k = line->idx_sym[j];

           var->ndims = atol( &line->asc_line[k] );
        }


       /*---------------------------------
        * :type float
        *    set type of variable values.
        *---------------------------------*/
        else if( line->len_sym[1] == i_type )
        {
           j = 2;
           if( line->cls_sym[j] == '=' )
              j++;

           k = line->idx_sym[j];

           var->datatype = typ_from_ascii( &line->asc_line[k] );
                              err_r( );
        }


       /*---------------------------------
        * :dimension v8:tg1
        *    if dimension doesn't exist, create it, else nothing.
        *---------------------------------*/
        else if( line->len_sym[1] == i_dimension )
        {
           k = line->idx_sym[2];

           alt_adddim( line, (cdHd *) var->above, 0 );
              err_r( );
        }


       /*---------------------------------
        * :variable v8:tg1
        *    if variable doesn't exist create it, else nothing.
        *---------------------------------*/
        else if( line->len_sym[1] == i_variable )
        {
           k = line->idx_sym[2];

           alt_addvar( line, (cdHd *) var->above, 0 );
              err_r( );
        }
     }


    /*---------------------------------
     * Process continuation of ':values = ....' card.
     *---------------------------------*/


     else
     {
        l = alt_values( line, 0, (cdHd *) var, l );
               err_r( );
     }
  }

  rel_cdms_card( line );

  rel_cdms_pql_list( pql );

  return;
}


/*********************************************************************
 * Function: to process ':tim6hbound', ':tim6habs'
 * cards bounds_time, abs_time variables.

 * Note: A specialized sub-function of 'Alter' ds instruction.
 *********************************************************************/


void alt_tim6h( cdHd *hd,     /* dimension struct */
                char *nmod,   /* tim6hbound,tim6habs */
                char *ncal,   /* name time calender */
                char *ntim )  /* name time dimension */

{
  int       m, ct, yr, mo, da, hr;
  char      atime[CD_MAX_PATH];
  long      i, j, l, lvv;
  double    del, *dd, *dd1, *dd2, *ddbnd, *ddbd, *ddabs;
  cdType    typ;
  cdDim_new *dim, *dima;
  cdVar_new *var;
  cdAtt_new *atta;
  void      *vv, *pp;


 /*---------------------------------
  * Get calender id for the time routines.
  *---------------------------------*/


  ct = typ_of_calendar( ncal );


 /*---------------------------------
  * Get the reference time dimension.
  *---------------------------------*/


  dima = nam_fnd( ntim, id_cdDim, hd );
     err_t( dima == NULL, "alt_tim6h: dimension not in file" );

  l = dima->length;

  if( dima->data == NULL )
  {
     vv = rd_dim_array( dima, -1, -1 );
             err_r( );

     dd = ary_trans( dima->datatype, l, vv, cdDouble );
             err_r( );

     free( vv );
  }
  else
  {
     dd = ary_trans( dima->datatype, l, dima->data, cdDouble );
             err_r( );
  }

  atta = scn_lnk_list( "units", id_cdAtt, (cdHd *) dima );
     err_t( atta == NULL, "alt_tim6h: dimension has no units" );


 /*---------------------------------
  * Generate mid, bound and abs output arrays.
  *---------------------------------*/


  ddabs = (double *) malloc( l * sizeof(double) );
  ddbnd = (double *) malloc( (l+1) * sizeof(double) );
  ddbd  = (double *) malloc( (l*2) * sizeof(double) );

  if( ddabs == NULL || ddbnd == NULL || ddbd == NULL )
     err_x( "alt_tim6h: trouble getting memory" );


  for( i=0; i < l; i++ )
  {
     cdRel2Char( ct, (char *) atta->values, dd[i], atime );
       m = sscanf( atime, "%d%*1c%d%*1c%d%d", &yr, &mo, &da, &hr );

     if( i == 0 )
     {
        if( l == 1 )
        {
           del = dd[0] * .5;
           ddbnd[0] = dd[0] - del;
        }
        else
        {
           del = ( dd[1] - dd[0] ) * .5;
           ddbnd[0] = dd[0] - del;
        }
     }

     ddbnd[i+1] = dd[i] + del;
     ddabs[i] = yr*10000 + mo*100 + da + (hr/24.0);
  }

  for( i=0, j=0; i < l; i++ )
  {
     ddbd[j++] = ddbnd[i];
     ddbd[j++] = ddbnd[i+1];
  }


 /*---------------------------------
  * Decide which output array to use.
  *---------------------------------*/


  if( !strcmp( "tim6hbound", nmod ) )
  {
     vv  = (void *) ddbd;
     lvv = 2 * l;
     typ = cdDouble;
  }

  else if( !strcmp( "tim6habs", nmod ) )
  {
     vv  = (void *) ddabs;
     lvv = l;
     typ = cdDouble;
  }


 /*---------------------------------
  * Set output dim,var data values.
  *---------------------------------*/


  if( hd->id == id_cdDim )
  {
     dim = (cdDim_new *) hd;

     if( dim->data != NULL )
        free( dim->data );

     if( lvv != dim->length )
        err_x( "alt_tim6h: dimension length problems" );

     dim->data = ary_trans( typ, lvv, vv, dim->datatype );
                    err_r( );
  }

  else if( hd->id == id_cdVar )
  {
     var = (cdVar_new *) hd;

     if( var->data != NULL )
        free( var->data );

     if( lvv != var->length )
        err_x( "alt_tim6h: variable length problems" );

     var->data = ary_trans( typ, lvv, vv, var->datatype );
                    err_r( );
  }

  free( dd );
  free( ddabs );
  free( ddbnd );
  free( ddbd );
  return;
}


/*********************************************************************
 * Function to process ':values ...' card for ds, dim, var structs.

 * Note: A specialized sub-function of 'Alter' ds instruction.
 * Note: multiple cards allowed for ':values=1,2,3,4,...' input.
 *********************************************************************/


long alt_values( cdms_card *line,  /* struct metafile line */
                 int       idx,    /* card index after '=' */
                 cdHd      *hd,    /* dim,var struct */
                 long      len )   /* length stored values dim,var */
{
  int       i, j, k, m, f;
  char      *aa, **aalst;
  long      num, n, l, *ll;
  double    *dd, *dd1, first, delta, *bd, *wt;
  cdType    typ, typ_l;
  CuType    cutyp;
  cdVar_new *var;
  cdDim_new *dim, *dima;
  void      *pp, *pa, *vv, *zz;


 /*---------------------------------
  * Get memory for dim,var data array.
  *---------------------------------*/


  num = len;
  dim = NULL;
  var = NULL;

  if( hd->id == id_cdDim )
  {
     dim = (cdDim_new *) hd;

     l     = dim->length;
     typ_l = dim->datatype;
     pp    = dim->data;

     if( dim->data == NULL )
     {
        k = line->idx_sym[idx];

        if( line->cls_sym[idx] == 'a' &&
            ( !strcmp( "halfshift", &line->asc_line[k] ) ||
              !strncmp( "tim6h", &line->asc_line[k], 5 ) ||
              !strncmp( "timmo", &line->asc_line[k], 5 ) ) )
        {
           dim->data = rd_dim_array( dim, -1, -1 );
                          err_rl( );

           num = 0;
           pp  = dim->data;
        }
        else
        {
           typ_to_cdunif( dim->datatype, &cutyp, &m );

           dim->data = malloc( m * dim->length );
           if( dim->data == NULL )
              err_xl( "alt_values: trouble getting memory" );

           num = 0;
           pp  = dim->data;
        }
     }
  }

  else if( hd->id == id_cdVar )
  {
     var = (cdVar_new *) hd;

     l     = var->length;
     typ_l = var->datatype;
     pp    = var->data;

     if( var->data == NULL )
     {
        typ_to_cdunif( var->datatype, &cutyp, &m );

        var->data = malloc( m * var->length );
        if( var->data == NULL )
           err_xl( "alt_values: trouble getting memory" );

        num = 0;
        pp  = var->data;
     }
  }

  else
  {
     err_xl( "alt_values: bad input arg." );
  }


 /*---------------------------------
  * Check if linear,range card given to set array.

  * lat = linear(-90.0, 2.8125);  ... point, delta
  * lat = range( 90.0, -90.0 );   ... begin, end
  *---------------------------------*/


  k = line->idx_sym[idx];
  f = 0;

  if( line->cls_sym[idx] == 'a' )
  {
     if( !strcmp( "linear", &line->asc_line[k] )
                  && line->cls_sym[idx+1] == '(' )
        f = 1;

     else if( !strcmp( "range", &line->asc_line[k] ) 
              && line->cls_sym[idx+1] == '(' )
        f = 2;

     else if( !strcmp( "bounds", &line->asc_line[k] ) 
              && line->cls_sym[idx+1] == 'a' )
        f = 3;

     else if( !strcmp( "weights", &line->asc_line[k] ) 
              && line->cls_sym[idx+1] == 'a' )
        f = 4;

     else if( !strcmp( "halfshift", &line->asc_line[k] ) 
              && line->cls_sym[idx+1] == 'a' )
        f = 5;

     else if( !strcmp( "timmomid", &line->asc_line[k] ) || 
              !strcmp( "timmobound", &line->asc_line[k] ) ||
              !strcmp( "timmoabs", &line->asc_line[k] ) )
        f = 6;

     else if( !strcmp( "tim6hbound", &line->asc_line[k] ) ||
              !strcmp( "tim6habs", &line->asc_line[k] ) )
        f = 7;
  }


 /*---------------------------------
  * Store list of numbers into next part of data array.
  *---------------------------------*/


  if( f == 0 )
  {
     zz = meta_after_eq( idx, line, &typ, &n );
             err_rl( );

     vv = ary_trans( typ, n, zz, typ_l );
             err_rl( );

     typ_to_cdunif( typ_l, &cutyp, &m );

     pa = ary_off( typ_l, num, pp );

     memcpy( pa, vv, n*m );

     num += n;
     free( zz );
     free( vv );

     return num;
  }


 /*---------------------------------
  * Process linear,range card to set entire output array.
  *---------------------------------*/


  else if( f == 1 || f == 2 )
  {
    /*---------------------------------
     * Get [first,delta] from linear,range card.
     *---------------------------------*/


     zz = meta_after_eq( 4, line, &typ, &n );
             err_rl( );

     if( typ == cdLong )
     {
        ll    = (long *) zz;
        first = ll[0];
        delta = ll[1];
     }
     else if( typ == cdDouble )
     {
        dd1   = (double *) zz;
        first = dd1[0];
        delta = dd1[1];
     }
     else
     {
        err_xl( "alt_values: bad card" );
     }

     if( l < 1 )
     {
        err_xl( "alt_values: bad dim,var length" );
     }

     if( f == 2 )
        delta = ( delta - first ) / ( l - 1 );


    /*---------------------------------
     * Generate entire output array.
     *---------------------------------*/


     dd = (double *) malloc( l * sizeof( double ) );
     if( dd == NULL )
        err_xl( "alt_values: trouble getting memory" );

     dd[0] = first;

     for( i=1; i < l; i++ )
        dd[i] = dd[i-1] + delta;

     vv = ary_trans( cdDouble, l, dd, typ_l );
             err_rl( );

     typ_to_cdunif( typ_l, &cutyp, &m );

     memcpy( pp, vv, l*m );

     free( zz );
     free( dd );
     free( vv );

     return l;
  }


 /*---------------------------------
  * Process 'bounds dimname [gridname]' or 'weights dimname [gridname]'
  *    card to set entire output array.
  *    dimname  = "latitude", "longitude", "level", "time"
  *    gridname = "gaussian", "lmd", "linear", "Unknown", "csu", "gis"
  *---------------------------------*/


  else if( f == 3 || f == 4 || f == 5 )
  {
     k = line->idx_sym[idx+1];
     dima = nam_fnd( &line->asc_line[k], id_cdDim, hd );
        err_tl( dima == NULL, "alt_values: dimension not in file" );

     if( idx+2 < line->num_sym && line->cls_sym[idx+2] == 'a' )
     {
        k  = line->idx_sym[idx+2];
        aa = cr_asc_cpy( &line->asc_line[k] );
     }
     else
        aa = cr_asc_cpy( "Unknown" );

     l = dima->length;


    /*---------------------------------
     * Get coordinates in scratch array as type double.
     *---------------------------------*/
     if( dima->data == NULL )
     {
        vv = rd_dim_array( dima, -1, -1 );
                err_rl( );

        dd = ary_trans( dima->datatype, l, vv, cdDouble );
                err_rl( );

        free( vv );
     }
     else
     {
        dd = ary_trans( dima->datatype, l, dima->data, cdDouble );
                err_rl( );
     }


    /*---------------------------------
     * Get weights and bounds arrays.
     *---------------------------------*/
     qlget_bw( aa, dima->name, l, dd[0], dd[l-1],
               l, dd[0], dd[l-1], dd, dd, &bd, &wt );

     typ_to_cdunif( cdDouble, &cutyp, &m );

     if( typ_l != cdDouble )
        err_xl( "alt_values: weights,bounds not type double" );


    /*---------------------------------
     * Store bounds values.
     *---------------------------------*/
     if( f == 3 )
     {
        dd1 = (double *) pp;

        for( i=0, j=0; i < l; )
        {
           dd1[j++] = bd[i++];
           dd1[j++] = bd[i];
        }
     }


    /*---------------------------------
     * Store weights values.
     *---------------------------------*/
     else if( f == 4 )
        memcpy( pp, wt, l*m );


    /*---------------------------------
     * Store 'halfshift' time and reset bounds_time.
     *---------------------------------*/
     else if( f == 5 )
     {
        dima = nam_fnd( "bounds_time", id_cdDim, hd );

        if( dima != NULL && dima->data != NULL )
        {
           if( dima->datatype != cdDouble )
              err_xl( "alt_values: bounds_time must be double" );

           dd1 = (double *) dima->data;

           for( i=0, j=0; i < l; )
           {
              dd1[j++] = dd[i++];

              if( i < l )
                 dd1[j++] = dd[i];
              else
                 dd1[j++] = dd[l-1] + (dd[l-1] - dd[l-2]);
           }
        }

        memcpy( pp, &bd[1], l*m );
     }

     free( dd );
     free( aa );
     free( bd );
     free( wt );

     return l;
  }

  else if( f == 6 )
  {
     if( line->cls_sym[idx+1] != 'a' || line->cls_sym[idx+2] != 'a' )
        err_xl( "alt_values: bad ...momid,mobound,moabs alter card" );

     k = line->idx_sym[idx];
     i = line->idx_sym[idx+1];
     j = line->idx_sym[idx+2];

     alt_midmonth( hd, &line->asc_line[k], &line->asc_line[i],
                       &line->asc_line[j] );
        err_rl( )

     return l;
  }

  else if( f == 7 )
  {
     if( line->cls_sym[idx+1] != 'a' || line->cls_sym[idx+2] != 'a' )
        err_xl( "alt_values: bad ...6hbound,6habs alter card" );

     k = line->idx_sym[idx];
     i = line->idx_sym[idx+1];
     j = line->idx_sym[idx+2];

     alt_tim6h( hd, &line->asc_line[k], &line->asc_line[i],
                    &line->asc_line[j] );
        err_rl( )

     return l;
  }
  return (long) 0;
}


/*********************************************************************
 * Function: process the 'alterfile' on-write-execute directives
 *    which don't involve modifying dim coordinates or var values
 *    (ie. delvar, deldim, wrtname, wrtshape, wrttype).
 *********************************************************************/


void alt_wrt_cards( cdDset_new *dset,   /* dataset struct */
                    char       *name )  /* attribute name */
{
  char      *aa;
  cdVar_new *var;
  cdDim_new *dim;
  cdAtt_new *att, *atta;


  if( dset == NULL || dset->id != id_cdDset )
     return;


 /*---------------------------------
  * Loop over dataset node attributes.
  * See if dataset has 'name-ed' attribute.
  *---------------------------------*/


  for (att=dset->atts; att; att=att->next )
  {
     if( !strcmp( name, att->name ) )
     {
        if( !strcmp( name, "psql_delvar" ) ||
            !strcmp( name, "psql_deldim" ) )
        {
           alt_del_node( att );
              err_r( );
        }
     }
  }


 /*---------------------------------
  * Loop over variables in dataset.
  * See if variable has 'name-ed' attribute.
  *---------------------------------*/


  for( var=dset->vars; var; var=var->next )
  {
     for( att=var->atts; att; att=att->next )
     {
        if( !strcmp( name, att->name ) )
        {
           if( !strcmp( name, "psql_delvar" ) ||
               !strcmp( name, "psql_deldim" ) )
           {
              alt_del_node( att );
                 err_r( );
           }

           else if( !strcmp( name, "psql_wrtname" ) )
           {
              atta           = cre_struct( id_cdAtt, (cdHd *) var );
              atta->name     = cr_asc_cpy( "psql_name" );
              atta->length   = strlen( var->name );
              atta->datatype = cdChar;
              atta->values   = cr_asc_cpy( var->name );

              free( var->name );
              var->name = cr_asc_cpy( (char *) att->values );
           }

           else if( !strcmp( name, "psql_wrttype" ) )
           {
              aa = typ_as_ascii( var->datatype );

              atta           = cre_struct( id_cdAtt, (cdHd *) var );
              atta->name     = cr_asc_cpy( "psql_type" );
              atta->length   = strlen( aa );
              atta->datatype = cdChar;
              atta->values   = aa;

              var->datatype = typ_from_ascii( (char *) att->values );
           }

           else if( !strcmp( name, "psql_wrtshape" ) )
           {
              alt_mod_shape( att );
                 err_r( );
           }
        }
     }
  }


 /*---------------------------------
  * Loop over dimensions in dataset.
  * See if dimension has 'name-ed' attribute.
  *---------------------------------*/


  for( dim=dset->dims; dim; dim=dim->next )
  {
     for( att=dim->atts; att; att=att->next )
     {
        if( !strcmp( name, att->name ) )
        {
           if( !strcmp( name, "psql_delvar" ) ||
               !strcmp( name, "psql_deldim" ) )
           {
              alt_del_node( att );
                 err_r( );
           }

           else if( !strcmp( name, "psql_wrtname" ) )
           {
              atta           = cre_struct( id_cdAtt, (cdHd *) dim );
              atta->name     = cr_asc_cpy( "psql_name" );
              atta->length   = strlen( dim->name );
              atta->datatype = cdChar;
              atta->values   = cr_asc_cpy( dim->name );

              free( dim->name );
              dim->name = cr_asc_cpy( (char *) att->values );
           }

           else if( !strcmp( name, "psql_wrttype" ) )
           {
              aa = typ_as_ascii( dim->datatype );

              atta           = cre_struct( id_cdAtt, (cdHd *) dim );
              atta->name     = cr_asc_cpy( "psql_type" );
              atta->length   = strlen( aa );
              atta->datatype = cdChar;
              atta->values   = aa;

              dim->datatype = typ_from_ascii( (char *) att->values );
           }
        }
     }
  }
  return;
}


/*********************************************************************
 * Function:  determine minimum and maximum of array.
 *********************************************************************/


void ary_min_max( cdType typ,    /* array type */
                  long   len,    /* array length */
                  void   *ary,   /* array */
                  double *min,   /* output minimum */
                  double *max )  /* output maximum */
{
  unsigned char *in_b;
  short         *in_s;
  int           *in_i, i;
  long          *in_l;
  float         *in_f;
  double        *in_d, mn, mx, pt1, pt2;

  pt1 = 1.0e+20 - (1.e-6 * 1.0e+20);
  pt2 = 1.0e+20 + (1.e-6 * 1.0e+20);


 /*---------------------------------
  * Find min-max
  *---------------------------------*/


  if( typ == cdByte )
  {
     in_b = (unsigned char *) ary;
     mn   = mx = in_b[0];

     for( i=1; i < len; i++ )
     {
        if( in_b[i] < mn )
           mn = in_b[i];
        if( in_b[i] > mx )
           mx = in_b[i];
     }
  }

  else if( typ == cdShort )
  {
     in_s = (short *) ary;
     mn   = mx = in_s[0];

     for( i=1; i < len; i++ )
     {
        if( in_s[i] < mn )
           mn = in_s[i];
        if( in_s[i] > mx )
           mx = in_s[i];
     }
  }

  else if( typ == cdInt )
  {
     in_i = (int *) ary;
     mn   = mx = in_i[0];

     for( i=1; i < len; i++ )
     {
        if( in_i[i] < mn )
           mn = in_i[i];
        if( in_i[i] > mx )
           mx = in_i[i];
     }
  }

  else if( typ == cdLong )
  {
     in_l = (long *) ary;
     mn   = mx = in_l[0];

     for( i=1; i < len; i++ )
     {
        if( in_l[i] < mn )
           mn = in_l[i];
        if( in_l[i] > mx )
           mx = in_l[i];
     }
  }

  else if( typ == cdFloat )
  {
     in_f = (float *) ary;
     mn   = mx = in_f[0];

     for( i=1; i < len; i++ )
     {
        if( in_f[i] < pt1 || in_f[i] > pt2 )
        {
           if( in_f[i] < mn )
              mn = in_f[i];
           if( in_f[i] > mx )
              mx = in_f[i];
        }
     }
  }

  else if( typ == cdDouble )
  {
     in_d = (double *) ary;
     mn   = mx = in_d[0];

     for( i=1; i < len; i++ )
     {
        if( in_d[i] < pt1 || in_d[i] > pt2 )
        {
           if( in_d[i] < mn )
              mn = in_d[i];
           if( in_d[i] > mx )
              mx = in_d[i];
        }
     }
  }

  else if( typ == cdLongDouble )
  {
     in_d = (double *) ary;
     mn   = mx = in_d[0];

     for( i=1; i < len; i++ )
     {
        if( in_d[i] < pt1 || in_d[i] > pt2 )
        {
           if( in_d[i] < mn )
              mn = in_d[i];
           if( in_d[i] > mx )
              mx = in_d[i];
        }
     }
  }

  else
  {
     err_x( "ary_min_max: bad array type" );
  }

  if( mn < -1.e+300 )
     mn = -1.e+300;
  if( mn > 1.e+300 )
     mn = 1.e+300;

  if( mx < -1.e+300 )
     mx = -1.e+300;
  if( mx > 1.e+300 )
     mx = 1.e+300;

  *min = mn;
  *max = mx;
  return;
}


/*********************************************************************
 * Function:  determine address of index of array.
 *********************************************************************/


void *ary_off( cdType typ,    /* array type */
               long   idx,    /* array index */
               void   *ary )  /* array */
{
  unsigned char *in_b;
  short         *in_s;
  int           *in_i;
  long          *in_l;
  float         *in_f;
  double        *in_d;


 /*---------------------------------
  * Determine address of array index.
  *---------------------------------*/


  if( typ == cdByte )
  {
     in_b = (unsigned char *) ary;

     return &in_b[idx];
  }

  else if( typ == cdShort )
  {
     in_s = (short *) ary;
 
     return &in_s[idx];
  }

  else if( typ == cdInt )
  {
     in_i = (int *) ary;
 
     return &in_i[idx];
  }

  else if( typ == cdLong )
  {
     in_l = (long *) ary;
 
     return &in_l[idx];
  }

  else if( typ == cdFloat )
  {
     in_f = (float *) ary;
 
     return &in_f[idx];
  }

  else if( typ == cdDouble )
  {
     in_d = (double *) ary;
 
     return &in_d[idx];
  }

  else if( typ == cdLongDouble )
  {
     in_d = (double *) ary;

     return &in_d[idx];
  }

  else
  {
     err_xv( "ary_off: bad array type" );
  }
}


/*********************************************************************
 * Function to translate an array to another type
 *********************************************************************/


void *ary_trans( cdType typ,       /* input datatype */
                 long   len,       /* input length */
                 void   *ary,      /* input array */
                 cdType out_typ )  /* output datatype */
{
  unsigned char *in_b, *o_b;
  short         *in_s, *o_s;
  int           i, *in_i, *o_i;
  long          *in_l, *o_l;
  float         *in_f, *o_f;
  double        *in_d, *o_d;


 /*---------------------------------
  * set pointer input array
  *---------------------------------*/


  if( len < 1 || ary == NULL )
     err_xv( "ary_trans: bad input array" );

  if( typ == cdLongDouble )
     typ = cdDouble;

  if( out_typ == cdLongDouble )
     out_typ = cdDouble;


  if( typ == cdByte )
     in_b = (unsigned char *) ary;

  else if( typ == cdShort )
     in_s = (short *) ary;

  else if( typ == cdInt )
     in_i = (int *) ary;

  else if( typ == cdLong )
     in_l = (long *) ary;

  else if( typ == cdFloat )
     in_f = (float *) ary;

  else if( typ == cdDouble )
     in_d = (double *) ary;

  else
  {
     err_xv( "ary_trans: bad array type" );
  }


 /*---------------------------------
  * cdByte
  *---------------------------------*/


  if( out_typ == cdByte )
  {
     o_b = (unsigned char *) malloc( len * sizeof( unsigned char ) );
           err_tv( o_b == NULL, "ary_trans: trouble getting memory" );

     for( i=0; i < len; i++ )
     {
        if( typ == cdByte )
           o_b[i] = in_b[i];

        else if( typ == cdShort )
           o_b[i] = in_s[i];

        else if( typ == cdInt )
           o_b[i] = in_i[i];

        else if( typ == cdLong )
           o_b[i] = in_l[i];

        else if( typ == cdFloat )
           o_b[i] = in_f[i];

        else if( typ == cdDouble )
           o_b[i] = in_d[i];
     }

     return (void *) o_b;
  }


 /*---------------------------------
  * cdShort
  *---------------------------------*/


  else if( out_typ == cdShort )
  {
     o_s = (short *) malloc( len * sizeof( short ) );
           err_tv( o_s == NULL, "ary_trans: trouble getting memory" );

     for( i=0; i < len; i++ )
     {
        if( typ == cdByte )
           o_s[i] = in_b[i];

        else if( typ == cdShort )
           o_s[i] = in_s[i];

        else if( typ == cdInt )
           o_s[i] = in_i[i];

        else if( typ == cdLong )
           o_s[i] = in_l[i];

        else if( typ == cdFloat )
           o_s[i] = in_f[i];

        else if( typ == cdDouble )
           o_s[i] = in_d[i];
     }

     return (void *) o_s;
  }


 /*---------------------------------
  * cdInt
  *---------------------------------*/


  else if( out_typ == cdInt )
  {
     o_i = (int *) malloc( len * sizeof( int ) );
           err_tv( o_i == NULL, "ary_trans: trouble getting memory" );

     for( i=0; i < len; i++ )
     {
        if( typ == cdByte )
           o_i[i] = in_b[i];

        else if( typ == cdShort )
           o_i[i] = in_s[i];

        else if( typ == cdInt )
           o_i[i] = in_i[i];

        else if( typ == cdLong )
           o_i[i] = in_l[i];

        else if( typ == cdFloat )
           o_i[i] = in_f[i];

        else if( typ == cdDouble )
           o_i[i] = in_d[i];
     }

     return (void *) o_i;
  }


 /*---------------------------------
  * cdLong
  *---------------------------------*/


  else if( out_typ == cdLong )
  {
     o_l = (long *) malloc( len * sizeof( long ) );
           err_tv( o_l == NULL, "ary_trans: trouble getting memory" );

     for( i=0; i < len; i++ )
     {
        if( typ == cdByte )
           o_l[i] = in_b[i];

        else if( typ == cdShort )
           o_l[i] = in_s[i];

        else if( typ == cdInt )
           o_l[i] = in_i[i];

        else if( typ == cdLong )
           o_l[i] = in_l[i];

        else if( typ == cdFloat )
           o_l[i] = in_f[i];

        else if( typ == cdDouble )
           o_l[i] = in_d[i];
     }

     return (void *) o_l;
  }


 /*---------------------------------
  * cdFloat
  *---------------------------------*/


  else if( out_typ == cdFloat )
  {
     o_f = (float *) malloc( len * sizeof( float ) );
           err_tv( o_f == NULL, "ary_trans: trouble getting memory" );

     for( i=0; i < len; i++ )
     {
        if( typ == cdByte )
           o_f[i] = in_b[i];

        else if( typ == cdShort )
           o_f[i] = in_s[i];

        else if( typ == cdInt )
           o_f[i] = in_i[i];

        else if( typ == cdLong )
           o_f[i] = in_l[i];

        else if( typ == cdFloat )
           o_f[i] = in_f[i];

        else if( typ == cdDouble )
           o_f[i] = in_d[i];
     }

     return (void *) o_f;
  }


 /*---------------------------------
  * cdDouble
  *---------------------------------*/


  else if( out_typ == cdDouble )
  {
     o_d = (double *) malloc( len * sizeof( double ) );
           err_tv( o_d == NULL, "ary_trans: trouble getting memory" );

     for( i=0; i < len; i++ )
     {
        if( typ == cdByte )
           o_d[i] = in_b[i];

        else if( typ == cdShort )
           o_d[i] = in_s[i];

        else if( typ == cdInt )
           o_d[i] = in_i[i];

        else if( typ == cdLong )
           o_d[i] = in_l[i];

        else if( typ == cdFloat )
           o_d[i] = in_f[i];

        else if( typ == cdDouble )
           o_d[i] = in_d[i];
     }

     return (void *) o_d;
  }

  else
  {
     err_xv( "ary_trans: bad output array type" );
  }
}


/*********************************************************************
 * Function to replace placeholder attributes with actual structs
 *********************************************************************/


void att_to_dgv( cdDim_new  *dim,   /* root of dim linked list */
                 cdVar_new  *var )  /* root of variable linked list */
{
  int   i, j, l, n;
  char  *aa;
  cdTmp *tmp, *tmpa;


 /*---------------------------------
  * Loop across dimension linked list
  *---------------------------------*/


  for( ; dim; dim = dim->next )
  {
     if( dim->length < 1 )
        err_x( "att_to_dgv: dimension has 0 length" );
  }


 /*---------------------------------
  * Loop over variable linked list
  *---------------------------------*/


  for( ; var; var = var->next )
  {


    /*---------------------------------
     * variable dimensions
     *---------------------------------*/

     l = 1;
     n = 0;

     for( tmp=var->dim; tmp; tmp = tmp->next )
     {
        dim = tmp->want = nam_fnd( tmp->nam_want, id_cdDim,
                                   (cdHd *) var );
        if( dim == NULL )
           err_x( "att_to_dgv: variable dim missing" );

        l = l * dim->length;
        n++;
     }

     var->ndims  = n;
     var->length = l;
  }
  return;
}


/*********************************************************************
 * Function: User PQL Interface.
 *********************************************************************/


void cdPqltty(void)
{
  int  tty;
  char msg_line[180];

  tty     = 1;
  FPT_TTY = 1;

  sprintf( PSQL_MSG, "*** starting PSQL in interactive tty mode\n" );
  wrt_msg( );


 /*---------------------------------
  * Loop over instructions.
  *---------------------------------*/


  while( tty )
  {
     sprintf( PSQL_MSG, "psql> " );
     wrt_msg( );

     /* if( gets( msg_line ) == NULL ) */
     if( fgets( msg_line, 180, stdin ) == NULL )
     {
        sprintf( PSQL_MSG, "cdPqltty: trouble getting message\n" );
        wrt_msg( );
     }

     else
        pql_execute( msg_line );
  }
  return;
}


/*********************************************************************
 * Function: test a dataset (ie. file) against error-check-file.
 *********************************************************************/


void check_dataset( cdHd *cur,    /* current struct */
                    int  pflg )   /* fulledit flag */
{
  int        i, j, k;
  char       *aa, *bb, msg[120];
  cdDset_new *ds;
  cdAtt_new  *atta, *attb;


 /*---------------------------------
  * Set pointer to input struct.
  *---------------------------------*/


  if( cur == NULL || cur->id != id_cdDset )
     return;

  ds = (cdDset_new *) cur;


 /*---------------------------------
  * Print title line.
  *---------------------------------*/


  if( pflg == 1 )
  {
     sprintf( PSQL_MSG, "%s -- %s\n",
              "--------------------------------------------------------",
              "file" );
     wrt_msg( );

     atta = scn_lnk_list( "psql_path", id_cdAtt, (cdHd *) ds );
     attb = scn_lnk_list( "psql_file", id_cdAtt, (cdHd *) ds );
     if( atta == NULL || attb == NULL )
        err_x( "check_dataset: bad file pointer" );

     aa = (char *) atta->values;
     bb = (char *) attb->values;

     sprintf( PSQL_MSG, "%s -- %s\n", aa, bb );
     wrt_msg( );
  }


 /*---------------------------------
  * Check the file attributes.
  *---------------------------------*/


  i = ck_lnk_list_att( ds->atts, pflg );
         err_r( );

  j = ck_lnk_list_dim( ds->dims, pflg );
         err_r( );

  k = ck_lnk_list_var( ds->vars, pflg );
         err_r( );


 /*---------------------------------
  * Print 'no match' error message
  *---------------------------------*/


  if( pflg == 0 && ( i == 0 || j == 0 || k == 0 ) )
  {
     sprintf( PSQL_MSG, "%s -- %s\n",
              "--------------------------------------------------------",
              "file" );
     wrt_msg( );

     atta = scn_lnk_list( "psql_path", id_cdAtt, (cdHd *) ds );
     attb = scn_lnk_list( "psql_file", id_cdAtt, (cdHd *) ds );
     if( atta == NULL || attb == NULL )
        err_x( "check_dataset: bad file pointer" );

     aa = (char *) atta->values;
     bb = (char *) attb->values;

     sprintf( PSQL_MSG, "%s -- %s\n", aa, bb );
     wrt_msg( );

     if( i == 0 )
     {
        sprintf( PSQL_MSG, "%s -- %s\n", "                ",
                 "no match, file attributes" );
        wrt_msg( );
     }

     if( j == 0 )
     {
        sprintf( PSQL_MSG, "%s -- %s\n", "                ",
                 "no match, dimensions" );
        wrt_msg( );
     }

     if( k == 0 )
     {
        sprintf( PSQL_MSG, "%s -- %s\n", "                ",
                 "no match, variables" );
        wrt_msg( );
     }
  }
  return;
}


/*********************************************************************
 * Function: check if file is to be scanned (1 -- yes, 0 -- no).
 *********************************************************************/


int check_filename( char *name )   /* filename */

{
  int       i, j, k, l, n, flg;
  char      **vlst, *tst_nam;
  cdAtt_new *att;


 /*---------------------------------
  * Get 'filename' attribute from default database.
  *---------------------------------*/


  if( name == NULL )
     return 0;

  l = strlen( name );

  att = scn_lnk_list( "filename", id_cdAtt, (cdHd *) DB_ROOT_ADR );
  if( att == NULL || att->datatype != cdCharTime )
     err_xi( "check_filename: bad patterns for datafile names" );

  vlst = att->values;

 /*---------------------------------
  * Loop over user's acceptence patterns.
  *---------------------------------*/


  for( i=0; i < att->length; i++ )
  {
     tst_nam = vlst[i];
     k       = strlen( tst_nam );
     flg     = 1;


    /*---------------------------------
     * name = *abc*  --- pattern anywhere in string.
     *---------------------------------*/


     if( tst_nam[0] == '*' && tst_nam[k-1] == '*' )
     {
        flg = 0;
        n   = l - k + 3;

        for( j=0; j < n ; j++ )
        {
           if( name[j] == tst_nam[1] )
           {
              flg = !strncmp( &tst_nam[1], &name[j], k-2 );

              if( flg )
                 break;
           }
        }
     }


    /*---------------------------------
     * name = abc*  --- pattern at start of string.
     *---------------------------------*/


     else if( tst_nam[k-1] == '*' )
        flg = !strncmp( tst_nam, name, k-1 );


    /*---------------------------------
     * name = *abc  --- pattern at end of string.
     *---------------------------------*/


     else if( tst_nam[0] == '*' )
        flg = !strncmp( &tst_nam[1], &name[l-k+1], k-1 );


    /*---------------------------------
     * name = abc  --- pattern is entire string.
     *---------------------------------*/


     else
        flg = !strcmp( tst_nam, name );


     if( flg == 1 )
        return 1;
  }

  return 0;
}


/*********************************************************************
 * Function: Check linked-list of attribute structs.
 *********************************************************************/


int ck_lnk_list_att( cdAtt_new *att0,  /* first attribute struct */
                     int       pflg )  /* print flag (1 yes, 0 no) */
{
  int       i, j, k, l, n, f, oflg;
  cdAtt_new *att, *atta;
  cdCheck   *ck, **cklst;


 /*---------------------------------
  * Error check for duplicate attribute names.
  *---------------------------------*/


  for( att=att0; att->next != NULL; att=att->next )
  {
     for( atta=att->next; atta; atta=atta->next )
     {
        if( !strcmp( att->name, atta->name ) )
        {
           err_xi( "ck_lnk_list_att: duplicate attribute names" );
        }
     }
  }


 /*---------------------------------
  * Reset check attribute counts.
  *---------------------------------*/


  for( ck=DB_ROOT_ADR->ckdefs; ck; ck=ck->next )
  {
     if( ck->id == id_cdAtt )
        ck->nfstruct = 0;
  }

  if( pflg == 1 )
  {
     sprintf( PSQL_MSG, "%s -- \n", "-------------------" ); 
     wrt_msg();
  }


 /*---------------------------------
  * Loop over attributes of linked-list.
  *---------------------------------*/


  for( att=att0, oflg=1; att; att=att->next )
  {
    /*---------------------------------
     * For attribute get list of check structs.
     *---------------------------------*/


     if( pflg == 1 )
     {
        sprintf( PSQL_MSG, "%s -- %s\n", "attribute", att->name );
        wrt_msg();
     }

     ck_nam_fnd( id_cdAtt, att->name, NULL, &n, &cklst );
        err_ri( );

     f = 0;


    /*---------------------------------
     * Loop over list.
     *---------------------------------*/


     for( j=0; j < n; j++ )
     {


       /*---------------------------------
        * Compare attribute against check struct.
        *---------------------------------*/


        ck = cklst[j];

        k = ck_one_att( att, ck, pflg );
               err_ri( );

        if( k )
        {
           f              = 1;
           l              = ck->nfstruct++;
           ck->fstruct[l] = (cdHd *) att;
        }
     }


    /*---------------------------------
     * Release list.
     *---------------------------------*/


     free( cklst );

     if(  f == 0 )
     {
        oflg = 0;

        if( pflg == 1 )
        {
           sprintf( PSQL_MSG, "%s -- %s\n", "                ",
                    "no match" ); 
           wrt_msg();
        }
     }
  }

  return oflg;
}


/*********************************************************************
 * Function: Check linked-list of dimension structs.
 *********************************************************************/


int ck_lnk_list_dim( cdDim_new *dim0,  /* first dimension struct */
                     int       pflg )  /* print flag (1 yes, 0 no) */
{
  int       i, j, k, l, n, f, oflg;
  cdDim_new *dim, *dima;
  cdCheck   *ck, **cklst;


 /*---------------------------------
  * Error check for duplicate dimension names.
  *---------------------------------*/


  for( dim=dim0; dim->next != NULL; dim=dim->next )
  {
     for( dima=dim->next; dima; dima=dima->next )
     {
        if( !strcmp( dim->name, dima->name ) )
        {
           err_xi( "ck_lnk_list_dim: duplicate dimension names" );
        }
     }
  }


 /*---------------------------------
  * Reset check dimension counts.
  *---------------------------------*/


  for( ck=DB_ROOT_ADR->ckdefs; ck; ck=ck->next )
  {
     if( ck->id == id_cdDim )
        ck->nfstruct = 0;
  }

  if( pflg == 1 )
  {
     sprintf( PSQL_MSG, "%s -- \n", "-------------------" ); 
     wrt_msg();
  }


 /*---------------------------------
  * Loop over dimensions of linked-list.
  *---------------------------------*/


  for( dim=dim0, oflg=1; dim; dim=dim->next )
  {
    /*---------------------------------
     * For dimension get list of check structs.
     *---------------------------------*/


     if( pflg == 1 )
     {
        sprintf( PSQL_MSG, "%s -- %s\n", "dimension", dim->name );
        wrt_msg();
     }

     ck_nam_fnd( id_cdDim, dim->name, NULL, &n, &cklst );
        err_ri( );

     i = ck_lnk_list_att( dim->atts, 0 );
            err_ri( );

     f = 0;


    /*---------------------------------
     * Loop over list.
     *---------------------------------*/


     for( j=0; j < n; j++ )
     {


       /*---------------------------------
        * Compare dimension against check struct.
        *---------------------------------*/


        ck = cklst[j];

        k = ck_one_dim( dim, ck, pflg );
               err_ri( );

        if( k )
        {
           f              = 1;
           l              = ck->nfstruct++;
           ck->fstruct[l] = (cdHd *) dim;
        }
     }


    /*---------------------------------
     * Release list.
     *---------------------------------*/


     free( cklst );

     if(  f == 0 )
     {
        oflg = 0;

        if( pflg == 1 )
        {
           sprintf( PSQL_MSG, "%s -- %s\n", "                ",
                    "no match" ); 
           wrt_msg();
        }
     }
  }

  return oflg;
}


/*********************************************************************
 * Function: Check linked-list of variable structs.
 *********************************************************************/


int ck_lnk_list_var( cdVar_new *var0,  /* first variable struct */
                     int       pflg )  /* print flag (1 yes, 0 no) */
{
  int       i, j, k, l, n, f, oflg;
  cdVar_new *var, *vara;
  cdCheck   *ck, **cklst;


 /*---------------------------------
  * Error check for duplicate variable names.
  *---------------------------------*/


  for( var=var0; var->next != NULL; var=var->next )
  {
     for( vara=var->next; vara; vara=vara->next )
     {
        if( !strcmp( var->name, vara->name ) )
        {
           err_xi( "ck_lnk_list_var: duplicate variable names" );
        }
     }
  }


 /*---------------------------------
  * Reset check variable counts.
  *---------------------------------*/


  for( ck=DB_ROOT_ADR->ckdefs; ck; ck=ck->next )
  {
     if( ck->id == id_cdVar )
        ck->nfstruct = 0;
  }

  if( pflg == 1 )
  {
     sprintf( PSQL_MSG, "%s -- \n", "-------------------" ); 
     wrt_msg();
  }


 /*---------------------------------
  * Loop over variables of linked-list.
  *---------------------------------*/


  for( var=var0, oflg=1; var; var=var->next )
  {
    /*---------------------------------
     * For variable get list of check structs.
     *---------------------------------*/


     if( pflg == 1 )
     {
        sprintf( PSQL_MSG, "%s -- %s\n", "variable", var->name );
        wrt_msg();
     }

     ck_nam_fnd( id_cdVar, var->name, NULL, &n, &cklst );
        err_ri( );

     i = ck_lnk_list_att( var->atts, 0 );
            err_ri( );

     f = 0;


    /*---------------------------------
     * Loop over list.
     *---------------------------------*/


     for( j=0; j < n; j++ )
     {


       /*---------------------------------
        * Compare variable against check struct.
        *---------------------------------*/


        ck = cklst[j];

        k = ck_one_var( var, ck, pflg );
               err_ri( );

        if( k )
        {
           f              = 1;
           l              = ck->nfstruct++;
           ck->fstruct[l] = (cdHd *) var;
        }
     }


    /*---------------------------------
     * Release list.
     *---------------------------------*/


     free( cklst );

     if(  f == 0 )
     {
        oflg = 0;

        if( pflg == 1 )
        {
           sprintf( PSQL_MSG, "%s -- %s\n", "                ",
                    "no match" ); 
           wrt_msg( );
        }
     }
  }

  return oflg;
}


/*********************************************************************
 * Function: Scan list of check structs for name:tagname pair.
 *********************************************************************/


void ck_nam_fnd( cdms_Id idwant,      /* id of wanted struct */
                 char    name[],      /* name */
                 char    tagname[],   /* tagname or NULL*/
                 int     *num,        /* number of structs found */
                 cdCheck ***cklist )  /* list of structs found */
{
  int     i, n, f;
  cdCheck *ck, **cklst;


 /*---------------------------------
  * Create blank list of 50 check structs.
  *---------------------------------*/


  cklst = malloc( 50 * sizeof( ck ) );
  if( cklst == NULL )
     err_x( "ck_nam_fnd: trouble getting memory" );

  n = 0;


 /*---------------------------------
  * Loop across list of check structs.
  *---------------------------------*/


  for( ck=DB_ROOT_ADR->ckdefs; ck; ck=ck->next )
  {
     f = 1;


    /*---------------------------------
     * idwant
     *---------------------------------*/


     if( ck->id != idwant )
        f = 0;


    /*---------------------------------
     * tagname.
     *---------------------------------*/


     if( f && tagname != NULL )
     {
        if( ck->tagname != NULL && !strcmp( tagname, ck->tagname ) )
           f = 1;
        else
           f = 0;
     }


    /*---------------------------------
     * name
     *---------------------------------*/


     if( f && name != NULL )
     {
        if( ck->name != NULL && !strcmp( name, ck->name ) )
           f = 1;
        else
           f = 0;


       /*---------------------------------
        * alias names
        *---------------------------------*/


        if( f == 0 && ck->nalias > 0 )
        {
           for( i=0; i < ck->nalias; i++ )
           {
              if( !strcmp( name, ck->alias[i] ) )
              {
                 f = 1;
                 break;
              }
           }
        }
     }


    /*---------------------------------
     * Add struct to list.
     *---------------------------------*/


     if( f )
     {
        cklst[n++] = ck;

        if( n > 49 )
           err_x( "ck_nam_fnd: overflowed check list" );
     }
  }


 /*---------------------------------
  * Set output.
  *---------------------------------*/


  *num    = n;
  *cklist = cklst;
  return;
}


/*********************************************************************
 * Function: Check attribute struct against check struct.
 *********************************************************************/


int ck_one_att( cdAtt_new *att0,   /* attribute struct */
                cdCheck   *ck,     /* check struct */
                int        pflg )  /* print flag (1 yes, 0 no) */
{
  int           i, j, k, n, f, num_sym, len_str;
  cdAtt_new     *att;
  cdms_card     *line;
  cdms_pql_list *pql;


  mem_cdms_card( &line );
     err_ri( );

  mem_cdms_pql_list( &pql );
     err_ri( );

  f            = 1;
  pql->length  = 1;
  pql->list[0] = att0;

  if( pflg )
  {
     if( ck->tagname != NULL )
        sprintf( PSQL_MSG,
        "                 ----------------------------- %s:%s -- %s\n",
                 ck->name, ck->tagname, "checking" );
     else
        sprintf( PSQL_MSG,
        "                 ----------------------------- %s: -- %s\n",
                 ck->name, "checking" );

     wrt_msg();
  }


 /*---------------------------------
  * Loop over cards of check attribute struct.
  *---------------------------------*/


  for( i=0; i < ck->ncard; i++ )
  {
    /*---------------------------------
     * Crack card into symbols.
     *---------------------------------*/


     strcpy( line->asc_line, ck->card[i] );
     len_str        = strlen( ck->card[i] );
     line->num_sym  = 0;

     num_sym = meta_str_sym( line, 0, len_str );

     ins_asym_eos( line );

     pql_key_sym( line );

     line->cls_sym[num_sym] = ';';
     line->num_sym          = num_sym++;


    /*---------------------------------
     * Verify card format.
     *---------------------------------*/


     if( num_sym < 2 || line->cls_sym[0] != ':' ||
                        line->cls_sym[1] != 'k' )
     {
        if( pflg )
        {
           sprintf( PSQL_MSG, "%s -- %s\n",
                    "                 failed11", ck->card[i] ); 
           wrt_msg();
        }
        f = 0;
        break;
     }


    /*---------------------------------
     * Process card.
     *---------------------------------*/


    /*---------------------------------
     * :value  > 10
     * :value = *ab cd*
     *---------------------------------*/
     if( line->len_sym[1] == i_value ||
         line->len_sym[1] == i_type )
     {
        j = pql_fld4( line, pql, 1 );
               err_ri( );
        k = pql_whr_test( pql, 0, 1 );
               err_ri( );

        if( k == 0 )
        {
           if( pflg )
           {
              sprintf( PSQL_MSG, "%s -- %s\n",
                       "                 failed12", ck->card[i] ); 
              wrt_msg();
           }
           f = 0;
           break;
        }
     }


    /*---------------------------------
     * Unknown card.
     *---------------------------------*/
     else
     {
        if( pflg )
        {
           sprintf( PSQL_MSG, "%s -- %s\n",
                    "                 failed13", ck->card[i] ); 
           wrt_msg();
        }
        f = 0;
        break;
     }
  }

  rel_cdms_card( line );

  rel_cdms_pql_list( pql );

  if( f && pflg )
  {
     sprintf( PSQL_MSG, "%s -- %s\n", "                ", "OK" ); 
     wrt_msg();
  }

  return f;
}


/*********************************************************************
 * Function: Check dimension struct against check struct.
 *********************************************************************/


int ck_one_dim( cdDim_new *dim,    /* dimension struct */
                cdCheck   *ck,     /* check struct */
                int        pflg )  /* print flag (1 yes, 0 no) */
{
  int           i, j, k, l, n, f, num_sym, len_str;
  cdAtt_new     *att;
  cdms_card     *line;
  cdms_pql_list *pql;
  cdCheck       *cka, **cklst;

  mem_cdms_card( &line );
     err_ri( );

  mem_cdms_pql_list( &pql );
     err_ri( );

  f            = 1;
  pql->length  = 1;
  pql->list[0] = dim;

  if( pflg )
  {
     if( ck->tagname != NULL )
        sprintf( PSQL_MSG,
        "                 ----------------------------- %s:%s -- %s\n",
                 ck->name, ck->tagname, "checking" );
     else
        sprintf( PSQL_MSG,
        "                 ----------------------------- %s: -- %s\n",
                 ck->name, "checking" );

     wrt_msg();
  }


 /*---------------------------------
  * Loop over cards of check dimension struct.
  *---------------------------------*/


  for( i=0; i < ck->ncard; i++ )
  {
    /*---------------------------------
     * Crack card into symbols.
     *---------------------------------*/


     strcpy( line->asc_line, ck->card[i] );
     len_str        = strlen( ck->card[i] );
     line->num_sym  = 0;

     num_sym = meta_str_sym( line, 0, len_str );

     ins_asym_eos( line );

     pql_key_sym( line );

     line->cls_sym[num_sym] = ';';
     line->num_sym          = num_sym++;


    /*---------------------------------
     * Verify card format.
     *---------------------------------*/


     if( num_sym < 2 || line->cls_sym[0] != ':' ||
                        line->cls_sym[1] != 'k' )
     {
        if( pflg )
        {
           sprintf( PSQL_MSG, "%s -- %s\n",
                    "                 failed1", ck->card[i] );
           wrt_msg( );
        }
        f = 0;
        break;
     }


    /*---------------------------------
     * Process card.
     *---------------------------------*/


    /*---------------------------------
     * :attribute name = min value < 47.0
     *---------------------------------*/
     if( line->len_sym[1] == i_attribute &&
         line->cls_sym[2] == 'k' &&
         line->len_sym[2] == i_name )
     {
        j   = line->idx_sym[4];
        att = scn_lnk_list( &line->asc_line[j], id_cdAtt,
                            (cdHd *) dim );
        if( att == NULL )
        {
           if( pflg )
           {
              sprintf( PSQL_MSG, "%s -- %s\n",
                       "                 failed2", ck->card[i] );
              wrt_msg();
           }
           f = 0;
           break;
        }

        pql->list[0] = att;
        j = pql_fld4( line, pql, 2 );
               err_ri( );
        k = pql_whr_test( pql, 0, 1 );
               err_ri( );
        pql->list[0] = dim;

        if( k == 0 )
        {
           if( pflg )
           {
              sprintf( PSQL_MSG, "%s -- %s\n",
                       "                 failed3", ck->card[i] );
              wrt_msg();
           }

           f = 0;
           break;
        }
     }


    /*---------------------------------
     * :attribute uni, ab:, unifor:tag1,
     *---------------------------------*/
     else if( line->len_sym[1] == i_attribute )
     {
        line->cls_sym[num_sym-1] = ',';
        f = 0;


       /*---------------------------------
        * Loop across symbols on card.
        *---------------------------------*/
        for( j=2; j < num_sym; j++ )
        {
           if( line->cls_sym[j] != 'a' )
              break;


          /*---------------------------------
           * uni,      defined dimension attribute of name 'uni'
           *---------------------------------*/
           if( line->cls_sym[j+1] == ',' )
           {
              k   = line->idx_sym[j];
              att = scn_lnk_list( &line->asc_line[k], id_cdAtt,
                                  (cdHd *) dim );
              if( att != NULL )
              {
                 f = 1;
                 break;
              }
              j++;
           }

          /*---------------------------------
           * ab:,      all check attributes with name 'ab'
           *---------------------------------*/
           else if( line->cls_sym[j+1] == ':' &&
                    line->cls_sym[j+2] == ',' )
           {
              k   = line->idx_sym[j];
              ck_nam_fnd( id_cdAtt, &line->asc_line[k], NULL, &n,
                          &cklst );
                 err_ri( );
              for( l=0; l < n; l++ )
              {
                 cka = cklst[l];

                 if( cka->nfstruct > 0 )
                    f = 1;
              }
              free( cklst );
              if( f )
                 break;
              j += 2;
           }

          /*---------------------------------
           * unifor:tag1,      check attribute named 'unifor:tag1'
           *---------------------------------*/
           else if( line->cls_sym[j+1] == ':' &&
                    line->cls_sym[j+2] == 'a' &&
                    line->cls_sym[j+3] == ',' )
           {
              k   = line->idx_sym[j];
              l   = line->idx_sym[j+2];
              ck_nam_fnd( id_cdAtt, &line->asc_line[k],
                          &line->asc_line[l], &n, &cklst );
                 err_ri( );
              for( l=0; l < n; l++ )
              {
                 cka = cklst[l];

                 if( cka->nfstruct > 0 )
                    f = 1;
              }
              free( cklst );
              if( f )
                 break;
              j += 3;
           }
           else
           {
              if( pflg )
              {
                 sprintf( PSQL_MSG, "%s -- %s\n",
                          "                 failed4", ck->card[i] );
                 wrt_msg();
              }

              break;
           }
        }
        if( f == 0 )
        {
           if( pflg )
           {
              sprintf( PSQL_MSG, "%s -- %s\n",
                       "                 failed5", ck->card[i] );
              wrt_msg();
           }
           break;
        }
     }


    /*---------------------------------
     * :length > 10
     * :type = float
     *---------------------------------*/
     else if( line->len_sym[1] == i_length ||
              line->len_sym[1] == i_type )
     {
        j = pql_fld4( line, pql, 1 );
               err_ri( );
        k = pql_whr_test( pql, 0, 1 );
               err_ri( );

        if( k == 0 )
        {
           if( pflg )
           {
              sprintf( PSQL_MSG, "%s -- %s\n",
                       "                 failed6", ck->card[i] );
              wrt_msg();
           }
           f = 0;
           break;
        }
     }


    /*---------------------------------
     * Unknown card.
     *---------------------------------*/
     else
     {
        if( pflg )
        {
           sprintf( PSQL_MSG, "%s -- %s\n",
                    "                 failed7", ck->card[i] );
           wrt_msg();
        }
        f = 0;
        break;
     }
  }

  rel_cdms_card( line );

  rel_cdms_pql_list( pql );

  if( f && pflg )
  {
     sprintf( PSQL_MSG, "%s -- %s\n", "                ", "OK" );
     wrt_msg();
  }

  return f;
}


/*********************************************************************
 * Function: Check variable struct against check struct.
 *********************************************************************/


int ck_one_var( cdVar_new *var,    /* variable struct */
                cdCheck   *ck,     /* check struct */
                int        pflg )  /* print flag (1 yes, 0 no) */
{
  int           i, j, k, l, m, n, f, d, num_sym, len_str, fdm;
  cdAtt_new     *att;
  cdDim_new     *dim, *dima;
  cdTmp         *tmp;
  cdms_card     *line;
  cdms_pql_list *pql;
  cdCheck       *cka, **cklst;


 /*---------------------------------
  * Get memory for internal card scanning structs.
  *---------------------------------*/


  mem_cdms_card( &line );
     err_ri( );

  mem_cdms_pql_list( &pql );
     err_ri( );

  f            = 1;
  pql->length  = 1;
  pql->list[0] = var;

  if( pflg )
  {
     if( ck->tagname != NULL )
        sprintf( PSQL_MSG,
        "                 ----------------------------- %s:%s -- %s\n",
                 ck->name, ck->tagname, "checking" );
     else
        sprintf( PSQL_MSG,
        "                 ----------------------------- %s: -- %s\n",
                 ck->name, "checking" );

     wrt_msg();
  }


 /*---------------------------------
  * Loop over cards of check variable struct.
  *---------------------------------*/


  for( i=0; i < ck->ncard; i++ )
  {
    /*---------------------------------
     * Crack card into symbols.
     *---------------------------------*/


     strcpy( line->asc_line, ck->card[i] );
     len_str        = strlen( ck->card[i] );
     line->num_sym  = 0;

     num_sym = meta_str_sym( line, 0, len_str );

     ins_asym_eos( line );

     pql_key_sym( line );

     line->cls_sym[num_sym] = ';';
     line->num_sym          = num_sym++;


    /*---------------------------------
     * Verify card format.
     *---------------------------------*/


     if( num_sym < 2 || line->cls_sym[0] != ':' ||
                        line->cls_sym[1] != 'k' )
     {
        if( pflg )
        {
           sprintf( PSQL_MSG, "%s -- %s\n",
                    "                 failed21", ck->card[i] );
           wrt_msg();
        }
        f = 0;
        break;
     }


    /*---------------------------------
     * Process card.
     *---------------------------------*/


    /*---------------------------------
     * :attribute name = min value < 47.0
     *---------------------------------*/
     if( line->len_sym[1] == i_attribute &&
         line->cls_sym[2] == 'k' &&
         line->len_sym[2] == i_name )
     {
        j   = line->idx_sym[4];
        att = scn_lnk_list( &line->asc_line[j], id_cdAtt,
                            (cdHd *) var );
        if( att == NULL )
        {
           if( pflg )
           {
              sprintf( PSQL_MSG, "%s -- %s\n",
                       "                 failed22", ck->card[i] );
              wrt_msg();
           }
           f = 0;
           break;
        }

        pql->list[0] = att;
        j = pql_fld4( line, pql, 2 );
               err_ri( );
        k = pql_whr_test( pql, 0, 1 );
               err_ri( );
        pql->list[0] = var;

        if( k == 0 )
        {
           if( pflg )
           {
              sprintf( PSQL_MSG, "%s -- %s\n",
                       "                 failed23", ck->card[i] );
              wrt_msg();
           }
           f = 0;
           break;
        }
     }


    /*---------------------------------
     * :attribute uni, ab:, unifor:tag1,
     *---------------------------------*/
     else if( line->len_sym[1] == i_attribute )
     {
        line->cls_sym[num_sym-1] = ',';
        f = 0;


       /*---------------------------------
        * Loop across symbols on ':attribute...' card.
        *---------------------------------*/
        for( j=2; j < num_sym; j++ )
        {
           if( line->cls_sym[j] != 'a' )
              break;


          /*---------------------------------
           * uni,  -- just want an attribute with name 'uni'
           *---------------------------------*/
           if( line->cls_sym[j+1] == ',' )
           {
              k   = line->idx_sym[j];
              att = scn_lnk_list( &line->asc_line[k], id_cdAtt,
                                  (cdHd *) var );
              if( att != NULL )
              {
                 f = 1;
                 break;
              }
              j++;
           }

          /*---------------------------------
           * ab:,  -- want an attribute satisfy oneof 'ab' defs.
           *---------------------------------*/
           else if( line->cls_sym[j+1] == ':' &&
                    line->cls_sym[j+2] == ',' )
           {
              k   = line->idx_sym[j];
              ck_nam_fnd( id_cdAtt, &line->asc_line[k], NULL, &n,
                          &cklst );
                 err_ri( );
              for( l=0; l < n; l++ )
              {
                 cka = cklst[l];

                 if( cka->nfstruct > 0 )
                    f = 1;
              }
              free( cklst );
              if( f )
                 break;
              j += 2;
           }

          /*---------------------------------
           * unifor:tag1,  -- want an attribute satisfying 'unifor:tag1'
           *---------------------------------*/
           else if( line->cls_sym[j+1] == ':' &&
                    line->cls_sym[j+2] == 'a' &&
                    line->cls_sym[j+3] == ',' )
           {
              k   = line->idx_sym[j];
              l   = line->idx_sym[j+2];
              ck_nam_fnd( id_cdAtt, &line->asc_line[k],
                          &line->asc_line[l], &n, &cklst );
                 err_ri( );
              for( l=0; l < n; l++ )
              {
                 cka = cklst[l];

                 if( cka->nfstruct > 0 )
                    f = 1;
              }
              free( cklst );
              if( f )
                 break;
              j += 3;
           }
           else
           {
              if( pflg )
              {
                 sprintf( PSQL_MSG, "%s -- %s\n",
                          "                 failed24", ck->card[i] );
                 wrt_msg();
              }

              break;
           }
        }
        if( f == 0 )
        {
           if( pflg )
           {
              sprintf( PSQL_MSG, "%s -- %s\n",
                       "                 failed25", ck->card[i] );
              wrt_msg();
           }
           break;
        }
     }


    /*---------------------------------
     * :dimension uni, ab:, unifor:tag1,
     *---------------------------------*/
     else if( line->len_sym[1] == i_dimension )
     {
        line->cls_sym[num_sym-1] = ',';

        f   = 0;
        j   = 2;
        fdm = -1;


       /*---------------------------------
        * See if :dimension 3...... (ie. dimension number also given)
        *---------------------------------*/


        if( line->cls_sym[j] == 'i' )
        {
           k   = line->idx_sym[j];
           fdm = atol( &line->asc_line[k] );
           j++;
        }


       /*---------------------------------
        * Loop across symbols on card.
        *---------------------------------*/


        for( ; j < num_sym; j++ )
        {

           if( line->cls_sym[j] != 'a' )
              break;


          /*---------------------------------
           * uni,      defined variable dimension of name 'uni'
           *---------------------------------*/
           if( line->cls_sym[j+1] == ',' )
           {
              k   = line->idx_sym[j];

              for( tmp=(cdTmp *) var->dim, d=1; tmp; 
                   tmp=tmp->next, d++ )
              {
                 if( fdm == -1 || d == fdm )
                 {
                    dim = (cdDim_new *) tmp->want;

                    if( !strcmp( &line->asc_line[k], dim->name ) )
                       f = 1;
                 }
              }

              if( f )
                 break;

              j++;
           }

          /*---------------------------------
           * ab:,      all check dimension with name 'ab'
           *---------------------------------*/
           else if( line->cls_sym[j+1] == ':' &&
                    line->cls_sym[j+2] == ',' )
           {
              k   = line->idx_sym[j];
              ck_nam_fnd( id_cdDim, &line->asc_line[k], NULL, &n,
                          &cklst );
                 err_ri( );
             /*---------------------------------
              * Loop over check structs with this name.
              *---------------------------------*/
              for( l=0; l < n; l++ )
              {
                 cka = cklst[l];

                /*---------------------------------
                 * Loop over dimensions accepted by check struct.
                 *---------------------------------*/
                 for( m=0; m < cka->nfstruct; m++ )
                 {
                    dima = (cdDim_new *) cka->fstruct[m];

                   /*---------------------------------
                    * Loop over dimensions of variable.
                    *---------------------------------*/
                    for( tmp=(cdTmp *) var->dim, d=1; tmp;
                         tmp=tmp->next, d++ )
                    {
                       if( fdm == -1 || d == fdm )
                       {
                          dim = (cdDim_new *) tmp->want;

                          if( dima == dim )
                             f = 1;
                       }
                    }
                 }
              }
              free( cklst );
              if( f )
                 break;
              j += 2;
           }

          /*---------------------------------
           * unifor:tag1,      check dimension named 'unifor:tag1'
           *---------------------------------*/
           else if( line->cls_sym[j+1] == ':' &&
                    line->cls_sym[j+2] == 'a' &&
                    line->cls_sym[j+3] == ',' )
           {
              k   = line->idx_sym[j];
              l   = line->idx_sym[j+2];
              ck_nam_fnd( id_cdDim, &line->asc_line[k],
                          &line->asc_line[l], &n, &cklst );
                 err_ri( );
             /*---------------------------------
              * Loop over check structs with this name.
              *---------------------------------*/
              for( l=0; l < n; l++ )
              {
                 cka = cklst[l];

                /*---------------------------------
                 * Loop over dimensions accepted by check struct.
                 *---------------------------------*/
                 for( m=0; m < cka->nfstruct; m++ )
                 {
                    dima = (cdDim_new *) cka->fstruct[m];

                   /*---------------------------------
                    * Loop over dimensions of variable.
                    *---------------------------------*/
                    for( tmp=(cdTmp *) var->dim, d=1; tmp;
                         tmp=tmp->next, d++ )
                    {
                       if( fdm == -1 || d == fdm )
                       {
                          dim = (cdDim_new *) tmp->want;

                          if( dima == dim )
                             f = 1;
                       }
                    }
                 }
              }
              free( cklst );
              if( f )
                 break;
              j += 3;
           }
           else
           {
              if( pflg )
              {
                 sprintf( PSQL_MSG, "%s -- %s\n",
                          "                 failed26", ck->card[i] );
                 wrt_msg();
              }
              break;
           }
        }
        if( f == 0 )
        {
           if( pflg )
           {
              sprintf( PSQL_MSG, "%s -- %s\n",
                       "                 failed27", ck->card[i] );
              wrt_msg();
           }
           break;
        }
     }


    /*---------------------------------
     * :length > 10
     * :type = float
     *---------------------------------*/
     else if( line->len_sym[1] == i_length ||
              line->len_sym[1] == i_type ||
              line->len_sym[1] == i_ndim )
     {
        j = pql_fld4( line, pql, 1 );
               err_ri( );
        k = pql_whr_test( pql, 0, 1 );
               err_ri( );

        if( k == 0 )
        {
           if( pflg )
           {
              sprintf( PSQL_MSG, "%s -- %s\n",
                       "                 failed28", ck->card[i] );
              wrt_msg();
           }
           f = 0;
           break;
        }
     }


    /*---------------------------------
     * Unknown card.
     *---------------------------------*/
     else
     {
        if( pflg )
        {
           sprintf( PSQL_MSG, "%s -- %s\n",
                    "                 failed29", ck->card[i] );
           wrt_msg();
        }

        f = 0;
        break;
     }
  }


 /*---------------------------------
  * Release memory for internal card scanning structs.
  *---------------------------------*/


  rel_cdms_card( line );

  rel_cdms_pql_list( pql );

  if( f && pflg )
  {
     sprintf( PSQL_MSG, "%s -- %s\n", "                ", "OK" );
     wrt_msg();
  }

  return f;
}


/*********************************************************************
 * Function: create a copy of struct.

 * Note: contents copied; new struct spliced in linked-list.
 *********************************************************************/


void *copy_struct( cdHd *cur,   /* current struct */
                   cdHd *abv )  /* above struct to store copy */
{
  int        i;
  char       *aa, **vlst, **vlsta;
  cdDb_new   *db, *dba;
  cdDset_new *dset, *dseta;
  cdVar_new  *var, *vara;
  cdDim_new  *dim, *dima;
  cdAtt_new  *att, *atta;
  cdHd       *hd;
  cdTmp      *tmp, *tmpa;
  cdPql      *pql, *pqla;


  if( cur == NULL )
     return NULL;


 /*---------------------------------
  * Database
  *---------------------------------*/


  if( cur->id == id_cdDb )
  {
     db = (cdDb_new *) cur;

     dba       = cre_struct( id_cdDb, abv );
     dba->name = cr_asc_cpy( "copy_of_database" );

     if( db->atts != NULL )
     {
        for( att=db->atts; att; att=att->next )
           cr_att_cpy( att, (cdHd *) dba );
     }

     if( db->dims != NULL )
     {
        for( dim=db->dims; dim; dim=dim->next )
           hd = copy_struct( (cdHd *) dim, (cdHd *) dba );
     }

     if( db->vars != NULL )
     {
        for( var=db->vars; var; var=var->next )
           hd = copy_struct( (cdHd *) var, (cdHd *) dba );
     }

     if( db->pqls != NULL )
     {
        for( pql=db->pqls; pql; pql=pql->next )
           hd = copy_struct( (cdHd *) pql, (cdHd *) dba );
     }

     if( db->dsets != NULL )
     {
        for( dset=db->dsets; dset; dset=dset->next )
           hd = copy_struct( (cdHd *) dset, (cdHd *) dba );
     }

     return dba;
  }


 /*---------------------------------
  * Dataset
  *---------------------------------*/


  else if( cur->id == id_cdDset )
  {
     dset = (cdDset_new *) cur;

     dseta       = cre_struct( id_cdDset, abv );
     dseta->name = cr_asc_cpy( "copy_of_dataset" );

     if( dset->atts != NULL )
     {
        for( att=dset->atts; att; att=att->next )
           cr_att_cpy( att, (cdHd *) dseta );
     }

     if( dset->dims != NULL )
     {
        for( dim=dset->dims; dim; dim=dim->next )
           hd = copy_struct( (cdHd *) dim, (cdHd *) dseta );
     }

     if( dset->vars != NULL )
     {
        for( var=dset->vars; var; var=var->next )
           hd = copy_struct( (cdHd *) var, (cdHd *) dseta );
     }

     return dseta;
  }


 /*---------------------------------
  * Variable
  *---------------------------------*/


  else if( cur->id == id_cdVar )
  {
     var = (cdVar_new *) cur;

     vara             = cre_struct( id_cdVar, abv );

     vara->name       = cr_asc_cpy( var->name );
     vara->ndims      = var->ndims;
     vara->datatype   = var->datatype;
     vara->length     = var->length;

     if( var->atts != NULL )
     {
        for( att=var->atts; att; att=att->next )
           cr_att_cpy( att, (cdHd *) vara );
     }

     if( var->dim != NULL )
     {
        for( tmp=var->dim; tmp; tmp=tmp->next )
           hd = copy_struct( (cdHd *) tmp, (cdHd *) vara );
     }

     if( var->data != NULL )
     {
        vara->data = ary_trans( var->datatype, var->length,
                                var->data, var->datatype );
                        err_rv( );
     }

     return vara;
  }


 /*---------------------------------
  * Dimension
  *---------------------------------*/


  else if( cur->id == id_cdDim )
  {
     dim = (cdDim_new *) cur;

     dima = cre_struct( id_cdDim, abv );

     dima->name     = cr_asc_cpy( dim->name );
     dima->units    = cr_asc_cpy( dim->units );
     dima->datatype = dim->datatype;
     dima->length   = dim->length;

     if( dim->atts != NULL )
     {
        for( att=dim->atts; att; att=att->next )
           cr_att_cpy( att, (cdHd *) dima );
     }

     if( dim->data != NULL )
     {
        dima->data = ary_trans( dim->datatype, dim->length,
                                dim->data, dim->datatype );
                        err_rv( );
     }

     return dima;
  }


 /*---------------------------------
  * Attribute
  *---------------------------------*/


  else if( cur->id == id_cdAtt )
  {
     att = (cdAtt_new *) cur;

     atta           = cre_struct( id_cdAtt, abv );
     atta->name     = cr_asc_cpy( att->name );
     atta->datatype = att->datatype;
     atta->length   = att->length;
     if( att->datatype == cdChar )
        atta->values = cr_asc_cpy( att->values );

     else if( att->datatype == cdCharTime )
     {
        atta->values = malloc( att->length * sizeof( aa ) );
        if( atta->values == NULL )
           err_xv( "copy_struct: trouble getting memory" );

        vlsta = atta->values;
        vlst  = att->values;

        for( i=0; i < att->length; i++ )
        {
           vlsta[i] = cr_asc_cpy( vlst[i] );
        }
     }
     else
        atta->values = ary_trans( att->datatype, att->length, 
                                  att->values, att->datatype );

     return atta;
  }


 /*---------------------------------
  * Substitution Struct
  *---------------------------------*/


  else if( cur->id == id_cdTmp )
  {
     tmp = (cdTmp *) cur;

     tmpa = cre_struct( id_cdTmp, abv );

     tmpa->nam_want = cr_asc_cpy( tmp->nam_want );
     tmpa->id_want  = tmp->id_want;

     if( tmp->want != NULL )
     {
        dim = (cdDim_new *) tmp->want;

        if( dim->id == id_cdDim )
           tmpa->want = nam_fnd( dim->name, id_cdDim, abv );
     }

     return tmpa;
  }


 /*---------------------------------
  * Pql list Struct
  *---------------------------------*/


  else if( cur->id == id_cdPql )
  {
     pql = (cdPql *) cur;

     pqla = cre_struct( id_cdPql, abv );

     pqla->name   = cr_asc_cpy( pql->name );
     pqla->pqlmsg = cr_asc_cpy( pql->pqlmsg );
     pqla->length = pql->length;

     if( pql->list != NULL )
     {
        pqla->list = ary_trans( cdLong, pql->length, pql->list,
                                cdLong );
                        err_rv( );
     }

     return pqla;
  }

  else
     err_xv( "copy_struct: bad struct ID" );
}


/*********************************************************************
 * Function to test, get memory, and form a copy of a string
 *********************************************************************/


char *cr_asc_cpy( char *in )  /* input string */
{
  int i;
  char *aa;

  if( in == NULL )
     return NULL;

  if( (i = strlen( in )) > 0 )
  {
     aa = (char *) malloc( i + 1 );
     if( aa == NULL )
        err_cdms( "cr_asc_cpy: trouble getting memory", 1 );

     strcpy( aa, in );
     return aa;
  }

  return NULL;
}


/*********************************************************************
 * Function: Copy an attribute from 1 tree node to another.
 *********************************************************************/


void cr_att_cpy( cdAtt_new *atta,  /* attribute struct want to copy */
                 cdHd      *cur )  /* tree node to receive attribute */
{
  int       i, j;
  long      len;
  char      *aa, **vlst, **vlsta;
  cdAtt_new *att;
  cdType    typ;


 /*---------------------------------
  * Create output attribute.
  *---------------------------------*/


  if( atta == NULL || atta->id != id_cdAtt )
     err_x( "cr_att_cpy: bad input attribute" );

  typ = atta->datatype;
  len = atta->length;

  att           = cre_struct( id_cdAtt, cur );
  att->name     = cr_asc_cpy( atta->name );
  att->datatype = typ;
  att->length   = len;


 /*---------------------------------
  * Copy attribute values.
  *---------------------------------*/


  if( typ == cdChar )
     att->values = cr_asc_cpy( atta->values );

  else if( typ == cdCharTime )
  {
     att->values = malloc( len * sizeof( aa ) );
     if( att->values == NULL )
        err_x( "cr_att_cpy: trouble getting memory" );

     vlst  = att->values;
     vlsta = atta->values;

     for( i=0; i < len; i++ )
     {
        vlst[i] = cr_asc_cpy( vlsta[i] );
     }
  }

  else
     att->values = ary_trans( typ, len, atta->values, typ );

  return;
}


/*********************************************************************
 * Function: create new struct of type 'idwant', and if 'cur'
             contains a linked-list of this type, insert new
             struct at its end.
 *********************************************************************/


void *cre_struct( cdms_Id idwant,  /* id of desired struct */
                  cdHd    *cur )   /* current struct */
{
  cdDb_new   *db;
  cdDset_new *ds;
  cdVar_new  *var;
  cdDim_new  *dim;
  cdAtt_new  *att;
  cdHd       *hd;
  cdTmp      *tmp;
  cdPql      *pql;
  void       **v;


 /*---------------------------------
  * Get memory for new struct
  *---------------------------------*/


  hd = mem_struct( idwant, cur );


 /*---------------------------------
  * Try to place it at end of cur's idwant-linked-list.
  *---------------------------------*/


  v = fnd_lnk_list( idwant, cur );

  if( v == NULL )
     ;

  else if( *v == NULL )
     *v = (void *) hd;

  else if( idwant == id_cdDb )
  {
     for( db=(cdDb_new *) *v; db->next != NULL; db=db->next )
        ;
     db->next = (cdDb_new *) hd;
  }

  else if( idwant == id_cdDset )
  {
     for( ds=(cdDset_new *) *v; ds->next != NULL; ds=ds->next )
        ;
     ds->next = (cdDset_new *) hd;
  }

  else if( idwant == id_cdVar )
  {
     for( var=(cdVar_new *) *v; var->next != NULL; var=var->next )
        ;
     var->next = (cdVar_new *) hd;
  }

  else if( idwant == id_cdDim )
  {
     for( dim=(cdDim_new *) *v; dim->next != NULL; dim=dim->next )
        ;
     dim->next = (cdDim_new *) hd;
  }

  else if( idwant == id_cdAtt )
  {
     for( att=(cdAtt_new *) *v; att->next != NULL; att=att->next )
        ;
     att->next = (cdAtt_new *) hd;
  }

  else if( idwant == id_cdTmp )
  {
     for( tmp=(cdTmp *) *v; tmp->next != NULL; tmp=tmp->next )
        ;
     tmp->next = (cdTmp *) hd;
  }

  else if( idwant == id_cdPql )
  {
     for( pql=(cdPql *) *v; pql->next != NULL; pql=pql->next )
        ;
     pql->next = (cdPql *) hd;
  }

  return (void *) hd;
}


/*********************************************************************
 * Function: delete struct from memory.

 * Note: spliced out of linked-list; contents removed; memory freed.
 *********************************************************************/


void delete_struct( cdHd *cur )      /* current struct */
{
  cdDb_new   *db, *dba;
  cdDset_new *dset, *dseta;
  cdVar_new  *var, *vara;
  cdDim_new  *dim, *dima;
  cdAtt_new  *att, *atta;
  cdHd       *hd;
  cdTmp      *tmp, *tmpa;
  cdPql      *pql, *pqla;

  if( cur == NULL )
     return;

  hd = (cdHd *) cur->above;


 /*---------------------------------
  * Splice out database tree node.
  *---------------------------------*/


  if( cur->id == id_cdDb )
  {
     db = (cdDb_new *) cur;

     for( dba=DB_ROOT_ADR; dba; dba=dba->next )
     {
        if( dba->next == db )
           dba->next = db->next;
     }
  }

  else if( hd == NULL )
     ;


 /*---------------------------------
  * Splice out tree node below database.
  *---------------------------------*/


  else if( hd->id == id_cdDb )
  {
     db = (cdDb_new *) hd;

     if( cur->id == id_cdDset )
     {
        dset = (cdDset_new *) cur;

        if( db->dsets == dset )
           db->dsets = dset->next;
        else
        {
           for( dseta=db->dsets; dseta; dseta=dseta->next)
           {
              if( dseta->next == dset )
                 dseta->next = dset->next;
           }
        }
     }

     else if( cur->id == id_cdVar )
     {
        var = (cdVar_new *) cur;

        if( db->vars == var )
           db->vars = var->next;
        else
        {
           for( vara=db->vars; vara; vara=vara->next)
           {
              if( vara->next == var )
                 vara->next = var->next;
           }
        }
     }

     else if( cur->id == id_cdDim )
     {
        dim = (cdDim_new *) cur;

        if( db->dims == dim )
           db->dims = dim->next;
        else
        {
           for( dima=db->dims; dima; dima=dima->next)
           {
              if( dima->next == dim )
                 dima->next = dim->next;
           }
        }
     }

     else if( cur->id == id_cdAtt )
     {
        att = (cdAtt_new *) cur;

        if( db->atts == att )
           db->atts = att->next;
        else
        {
           for( atta=db->atts; atta; atta=atta->next)
           {
              if( atta->next == att )
                 atta->next = att->next;
           }
        }
     }

     else if( cur->id == id_cdPql )
     {
        pql = (cdPql *) cur;

        if( db->pqls == pql )
           db->pqls = pql->next;
        else
        {
           for( pqla=db->pqls; pqla; pqla=pqla->next)
           {
              if( pqla->next == pql )
                 pqla->next = pql->next;
           }
        }
     }
  }


 /*---------------------------------
  * Splice out tree node below dataset.
  *---------------------------------*/

  
  else if( hd->id == id_cdDset )
  {
     dset = (cdDset_new *) hd;

     if( cur->id == id_cdVar )
     {
        var = (cdVar_new *) cur;

        if( dset->vars == var )
           dset->vars = var->next;
        else
        {
           for( vara=dset->vars; vara; vara=vara->next)
           {
              if( vara->next == var )
                 vara->next = var->next;
           }
        }
     }

     else if( cur->id == id_cdDim )
     {
        dim = (cdDim_new *) cur;

        if( dset->dims == dim )
           dset->dims = dim->next;
        else
        {
           for( dima=dset->dims; dima; dima=dima->next)
           {
              if( dima->next == dim )
                 dima->next = dim->next;
           }
        }
     }

     else if( cur->id == id_cdAtt )
     {
        att = (cdAtt_new *) cur;

        if( dset->atts == att )
           dset->atts = att->next;
        else
        {
           for( atta=dset->atts; atta; atta=atta->next)
           {
              if( atta->next == att )
                 atta->next = att->next;
           }
        }
     }
  }


 /*---------------------------------
  * Splice out tree node below variable.
  *---------------------------------*/

  
  else if( hd->id == id_cdVar )
  {
     var = (cdVar_new *) hd;

     if( cur->id == id_cdTmp )
     {
        tmp = (cdTmp *) cur;

        if( var->dim == tmp )
           var->dim = tmp->next;
        else
        {
           for( tmpa=var->dim; tmpa; tmpa=tmpa->next)
           {
              if( tmpa->next == tmp )
                 tmpa->next = tmp->next;
           }
        }
     }

     else if( cur->id == id_cdAtt )
     {
        att = (cdAtt_new *) cur;

        if( var->atts == att )
           var->atts = att->next;
        else
        {
           for( atta=var->atts; atta; atta=atta->next)
           {
              if( atta->next == att )
                 atta->next = att->next;
           }
        }
     }
  }


 /*---------------------------------
  * Splice out tree node below dimension.
  *---------------------------------*/

  
  else if( hd->id == id_cdDim )
  {
     dim = (cdDim_new *) hd;

     if( cur->id == id_cdAtt )
     {
        att = (cdAtt_new *) cur;

        if( dim->atts == att )
           dim->atts = att->next;
        else
        {
           for( atta=dim->atts; atta; atta=atta->next)
           {
              if( atta->next == att )
                 atta->next = att->next;
           }
        }
     }
  }


 /*---------------------------------
  * Now delete struct and its contents from memory.
  *---------------------------------*/


  empty_struct( cur );
  return;
}


/*********************************************************************
 * Function to process dimension line field 1 -- type
 *********************************************************************/


int dim_f1( int       idx,   /* symbol index */
            cdms_card *line, /* struct metafile line */
            cdDim_new *dim ) /* dimension struct */
{
  int  i, j, *idx_sym, *cls_sym;
  char *asc_line;

  asc_line = line->asc_line;
  idx_sym  = line->idx_sym;
  cls_sym  = line->cls_sym;

  if( dim == NULL || cls_sym[idx] != 'a' )
  {
     err_xi( "dim_f1: dimension type" );
  }


 /*---------------------------------
  * Field 1 -- keyword -- optional -- default 'mono'
  * dimension type   ex. 'lond'
  *---------------------------------*/


  i = idx;
  j = idx_sym[i];

  if( !strcmp( "lond", &asc_line[j] ) )
     ;

  else if( !strcmp( "latd", &asc_line[j] ) )
     ;

  else if( !strcmp( "levd", &asc_line[j] ) )
     ;

  else if( !strcmp( "calen", &asc_line[j] ) )
     ;

  else if( !strcmp( "clim", &asc_line[j] ) )
     ;

  else if( !strcmp( "mono", &asc_line[j] ) )
     ;

  else
     return idx;

  return ++i;
}


/*********************************************************************
 * Function to process dimension line field 2 -- subtype
 *********************************************************************/


int dim_f2( int       idx,     /* symbol index */
            cdms_card *line,   /* struct metafile line */
            cdDim_new *dim )   /* dimension struct */
{
  int  i, j, *idx_sym, *cls_sym;
  char *asc_line;

  asc_line = line->asc_line;
  idx_sym  = line->idx_sym;
  cls_sym  = line->cls_sym;

  if( cls_sym[idx] != '(' )
     return idx;


 /*---------------------------------
  * Field 2 -- (keyword,keyword,...) -- optional
  * dimension subtype   ex. '(wrap,leap)'
  *---------------------------------*/


  for( i=idx; i<line->num_sym; i++ )
  {
     if( cls_sym[i] == '(' || cls_sym[i] == ',' )
        ;

     else if( cls_sym[i] == ')' )
        return i+1;

     else
     {
        if( cls_sym[i] != 'a' )
           err_xi( "dim_f2: dim subtype" );

        j = idx_sym[i];

        if( !strcmp( "wrap", &asc_line[j] ) )
           ;

        else if( !strcmp( "nowrap", &asc_line[j] ) )
           ;

        else if( !strcmp( "atmosphere", &asc_line[j] ) )
           ;

        else if( !strcmp( "hybrid", &asc_line[j] ) )
           ;

        else if( !strcmp( "pressure", &asc_line[j] ) )
           ;

        else if( !strcmp( "sealevel", &asc_line[j] ) )
           ;

        else if( !strcmp( "skin", &asc_line[j] ) )
           ;

        else if( !strcmp( "sigma", &asc_line[j] ) )
           ;

        else if( !strcmp( "soil", &asc_line[j] ) )
           ;

        else if( !strcmp( "surface", &asc_line[j] ) )
           ;

        else if( !strcmp( "toa", &asc_line[j] ) )
           ;

        else if( !strcmp( "twometer", &asc_line[j] ) )
           ;

        else if( !strcmp( "julian", &asc_line[j] ) )
           ;

        else if( !strcmp( "leap", &asc_line[j] ) )
           ;

        else if( !strcmp( "noleap", &asc_line[j] ) )
           ;

        else if( !strcmp( "standard", &asc_line[j] ) )
           ;

        else if( !strcmp( "year360", &asc_line[j] ) )
           ;

        else
        {
           err_xi( "dim_f2: bad dim subtype" );
        }
     }
  }

  err_xi( "dim_f2: dimension subtype" );
}


/*********************************************************************
 * Function to process dimension line field 3 -- datatype
 *********************************************************************/


int dim_f3( int       idx,     /* symbol index */
            cdms_card *line,   /* struct metafile line */
            cdDim_new *dim )   /* dimension struct */
{
  int  i, j, f, *idx_sym, *cls_sym;
  char *asc_line;

  asc_line = line->asc_line;
  idx_sym  = line->idx_sym;
  cls_sym  = line->cls_sym;


 /*---------------------------------
  * Field 3 -- keyword -- optional -- default 'int'
  * dimension datatype   ex. 'double'
  *---------------------------------*/


  i = idx;
  j = idx_sym[i];
  f = 0;

  if( cls_sym[i] != 'a' )
     err_xi( "dim_f3: dimension type" );

  if( !strcmp( "byte", &asc_line[j] ) )
     dim->datatype = cdByte;

  else if( !strcmp( "char", &asc_line[j] ) )
     dim->datatype = cdChar;

  else if( !strcmp( "short", &asc_line[j] ) )
     dim->datatype = cdShort;

  else if( !strcmp( "int", &asc_line[j] ) )
     dim->datatype = cdInt;

  else if( !strcmp( "long", &asc_line[j] ) )
     dim->datatype = cdLong;

  else if( !strcmp( "float", &asc_line[j] ) )
     dim->datatype = cdFloat;

  else if( !strcmp( "double", &asc_line[j] ) )
     dim->datatype = cdDouble;

  else if( !strcmp( "longdouble", &asc_line[j] ) )
     dim->datatype = cdLongDouble;

  else if( !strcmp( "chartime", &asc_line[j] ) )
     dim->datatype = cdCharTime;

  else
  {
     dim->datatype = cdInt;
     f             = 1;
  }

  if( f == 0 )
     i++;


 /*---------------------------------
  * Field 4 -- keyword or keyword<keyword> -- mandatory
  * dimension name,alias   ex. 'longitude<Longitude>'
  * NOTE: alias names no longer used (alterfile replaces it).
  *---------------------------------*/


  j = idx_sym[i];

  if( cls_sym[i] != 'a' )
     err_xi( "dim_f3: dimension name" );

  dim->name = cr_asc_cpy( &asc_line[j] );
  i++;


 /*---------------------------------
  * Field 5 -- (integer) or =integer -- mandatory
  * dimension length   ex. '(64)'
  *---------------------------------*/


  if( cls_sym[i] == '(' && cls_sym[i+1] == 'i' && cls_sym[i+2] == ')' )
  {
     j           = idx_sym[i+1];
     dim->length = atol( &asc_line[j] );
     return i+3;
  }

  else if( cls_sym[i] == '=' && cls_sym[i+1] == 'i' )
  {
     j           = idx_sym[i+1];
     dim->length = atol( &asc_line[j] );
     return i+2;
  }

  return i;
}


/*********************************************************************
 * Function: For a directory file, create dset,variable tree structs.
 *********************************************************************/


void dir_file( cdms_dir *dir,   /* struct for directory scanning */
               char     *path,  /* current directory path */
               char     *name,  /* name of file */
               cdHd     *cur )  /* current tree node */
{
  int        i, j, k, n, f_id, f_ndims, f_nvars, natts, f_recdim,
             ndims, len, dary[CD_MAX_VAR_DIMS];
  long       l, start[CU_MAX_VAR_DIMS], count[CU_MAX_VAR_DIMS];
  char       nam[CD_MAX_NAME], units[CD_MAX_PATH], aa[CD_MAX_PATH];
  double     *min, *max;
  cdDb_new   *db;
  cdDset_new *ds;
  cdVar_new  *var;
  cdDim_new  *dim;
  cdAtt_new  *att;
  cdTmp      *tmp;
  cdHd       *hd;
  CuType     dtype;
  cdType     typ;
  void       *v;


 /*---------------------------------
  * Attach file.
  *---------------------------------*/


  sprintf( nam, "%s/%s", path, name );
  /***debug***printf( "%s/%s\n", path, name );*/

  f_id = cuopenread( nam, NULL );
  if( f_id == -1 )
  {
     sprintf( PSQL_MSG, "%s -- %s\n", "PSQL BADFILE", nam ); 
     wrt_msg();

     if( dir->conflag == 0 )
     {
        err_x( "dir_file: cuopenread trouble");
     }
     else   /* ignore file */
        return;
  }


  f_ndims = f_nvars = natts = f_recdim = 0;

  j = cuinquire( f_id, &f_ndims, &f_nvars, &natts, &f_recdim );
  if( j == -1 )
  {
     sprintf( PSQL_MSG, "%s -- %s\n", "PSQL BADFILE", nam ); 
     wrt_msg();
     j = cuclose( f_id );

     if( dir->conflag == 0 )
     {
         err_x( "dir_file: cuinquire trouble");
     }
     else   /* ignore file */
        return;
  }


 /*---------------------------------
  * Create a dataset struct for this file.
  *---------------------------------*/


  hd = dir_struct( dir, path, cur, 1 );
          err_r( );

  if( hd->id != id_cdDset )
     err_x( "dir_file: can't handle dset directories yet" );

  ds = (cdDset_new *) hd;

  att           = cre_struct( id_cdAtt, (cdHd *) ds );
  att->name     = cr_asc_cpy( "psql_file" );
  att->datatype = cdChar;
  att->length   = strlen( name );
  att->values   = cr_asc_cpy( name );


 /*---------------------------------
  * Create dataset attributes.
  *---------------------------------*/


  for( i=0; i < natts; i++ )
  {
     j = cuattname( f_id, CU_GLOBAL, i, aa );
     if( j == -1 )
     {
        sprintf( PSQL_MSG, "%s -- %s\n", "PSQL BADFILE", nam ); 
        wrt_msg();
        delete_struct( (cdHd *) ds );
           err_r( );
        j = cuclose( f_id );

        if( dir->conflag == 0 )
        {
            err_x( "dir_file: cuattname trouble");
        }
        else   /* ignore file */
           return;
     }


     len = 0;
     j = cuattinq( f_id, CU_GLOBAL, aa, &dtype, &len );
     if( j == -1 )
     {
        sprintf( PSQL_MSG, "%s -- %s\n", "PSQL BADFILE", nam ); 
        wrt_msg();
        delete_struct( (cdHd *) ds );
           err_r( );
        j = cuclose( f_id );

        if( dir->conflag == 0 )
        {
            err_x( "dir_file: cuattinq trouble");
        }
        else   /* ignore file */
           return;
     }

     typ_from_cdunif( dtype, &typ, &n );
        err_r( );

     if( typ == cdChar)
        v = malloc( len * n + 1 );
     else
        v = malloc( len * n );

     err_t( v == NULL, "dir_file: memory trouble");

     j = cuattget( f_id, CU_GLOBAL, aa, v );
     if( j == -1 )
     {
        sprintf( PSQL_MSG, "%s -- %s\n", "PSQL BADFILE", nam );
        wrt_msg();
        delete_struct( (cdHd *) ds );
           err_r( );
        j = cuclose( f_id );

        if( dir->conflag == 0 )
        {
            err_x( "dir_file: cuattget trouble");
        }
        else   /* ignore file */
           return;
     }


    /*---------------------------------
     * Reset name with '_' in non-printing letters.
     *---------------------------------*/
     for( j=0; j < strlen(aa); j++ )
     {
        if( !isgraph( aa[j] ) )
           aa[j] = '_';
     }

     att           = cre_struct( id_cdAtt, (cdHd *) ds );
     att->name     = cr_asc_cpy( aa );
     att->datatype = typ;
     att->values   = v;
     if( typ == cdChar)
        att->length = strlen( (char *) v );
     else
        att->length = len;
  }


 /*---------------------------------
  * Read dimensions and store in tree structure.
  *---------------------------------*/


  dir_file_dim( dir, path, name, f_id, f_ndims, ds );
  if( ERR_no )
  {
     err_i( 0 );
     sprintf( PSQL_MSG, "%s -- %s\n", "PSQL BADFILE", nam );
     wrt_msg();
     delete_struct( (cdHd *) ds );
        err_r( );
     j = cuclose( f_id );

     if( dir->conflag == 0 )
     {
        err_x( "dir_file: dir_file_dim trouble" );
     }
     else
        return;
  }


 /*---------------------------------
  * Loop over variables.
  *---------------------------------*/


  for( i=0; i < f_nvars; i++ )
  {
    /*---------------------------------
     * Create variable struct.
     *---------------------------------*/


     ndims = natts = 0;
     j = cuvarinq( f_id, i, aa, &dtype, &ndims, dary, &natts );
     if( j == -1 )
     {
        sprintf( PSQL_MSG, "%s -- %s\n", "PSQL BADFILE", nam );
        wrt_msg();
        delete_struct( (cdHd *) ds );
           err_r( );
        j = cuclose( f_id );

        if( dir->conflag == 0 )
        {
            err_x( "dir_file: cuvarinq trouble" );
        }
        else   /* ignore file */
           return;
     }

    /*---------------------------------
     * Reset name with '_' in non-printing letters.
     *---------------------------------*/
     for( j=0; j < strlen(aa); j++ )
     {
        if( !isgraph( aa[j] ) )
           aa[j] = '_';
     }

     typ_from_cdunif( dtype, &typ, &n );
        err_r( );

     var = cre_struct( id_cdVar, (cdHd *) ds );

     var->name     = cr_asc_cpy( aa );
     var->datatype = typ;
     var->ndims    = ndims;


    /*---------------------------------
     * Loop over dimensions of variable.
     *---------------------------------*/


     len = 1;

     for( j=0; j < ndims; j++ )
     {
        l = 0;
        k = cudiminq( f_id, dary[j], aa, NULL, NULL, NULL, NULL, &l );
        if( k == -1 )
        {
           sprintf( PSQL_MSG, "%s -- %s\n", "PSQL BADFILE", nam );
           wrt_msg();
           delete_struct( (cdHd *) ds );
              err_r( );
           j = cuclose( f_id );

           if( dir->conflag == 0 )
           {
               err_x( "dir_file: cudiminq trouble");
           }
           else   /* ignore file */
              return;
        }

       /*---------------------------------
        * Reset name with '_' in non-printing letters.
        *---------------------------------*/
        for( k=0; k < strlen(aa); k++ )
        {
           if( !isgraph( aa[k] ) )
              aa[k] = '_';
        }


        dim = nam_fnd( aa, id_cdDim, (cdHd *) var );
        if( dim == NULL )
        {
           sprintf( PSQL_MSG, "%s -- %s\n", "PSQL BADFILE", nam );
           wrt_msg();
           err_m("dir_file: dimension of variable missing");
           delete_struct( (cdHd *) ds );
              err_r( );
           j = cuclose( f_id );

           if( dir->conflag == 0 )
           {
               err_x( "dir_file: var-dim missing");
           }
           else   /* ignore file */
              return;
        }

        tmp           = cre_struct( id_cdTmp, (cdHd *) var );
        tmp->id_want  = id_cdDim;
        tmp->want     = dim;
        tmp->nam_want = cr_asc_cpy( dim->name );

        len = len * dim->length;

        if( dir->vmxflag )
        {
           start[j] = 0;
           count[j] = dim->length;
        }
     }

     var->length = len;


    /*---------------------------------
     * Create variable 'file,path' attributes.
     *---------------------------------*/


     att           = cre_struct( id_cdAtt, (cdHd *) var );
     att->name     = cr_asc_cpy( "psql_file" );
     att->datatype = cdChar;
     att->length   = strlen( name );
     att->values   = cr_asc_cpy( name );

     att           = cre_struct( id_cdAtt, (cdHd *) var );
     att->name     = cr_asc_cpy( "psql_path" );
     att->datatype = cdChar;
     att->length   = strlen( path );
     att->values   = cr_asc_cpy( path );


    /*---------------------------------
     * Create variable 'min,max' attributes.
     *---------------------------------*/


     if( dir->vmxflag != 0 && typ != cdChar && len != 0 )
     {
        v   = malloc( len * n);
        min = (double *) malloc( sizeof( double ) );
        max = (double *) malloc( sizeof( double ) );
        if( v == NULL || min == NULL || max == NULL )
           err_x( "dir_file: malloc memory trouble" );

        j = cuvarget( f_id, i, start, count, v );
               err_t( j == -1, "dir_file: cuvarget trouble");

        l = len;
        ary_min_max( typ, l, v, min, max );
           err_r( );
        free( v );

        att           = cre_struct( id_cdAtt, (cdHd *) var );
        att->name     = cr_asc_cpy( "psql_min" );
        att->datatype = cdDouble;
        att->length   = 1;
        att->values   = min;

        att           = cre_struct( id_cdAtt, (cdHd *) var );
        att->name     = cr_asc_cpy( "psql_max" );
        att->datatype = cdDouble;
        att->length   = 1;
        att->values   = max;
     }


    /*---------------------------------
     * Create variable attributes.
     *---------------------------------*/


     for( j=0; j < natts; j++ )
     {
        k = cuattname( f_id, i, j, aa );
        if( k == -1 )
        {
           sprintf( PSQL_MSG, "%s -- %s\n", "PSQL BADFILE", nam );
           wrt_msg();
           delete_struct( (cdHd *) ds );
              err_r( );
           j = cuclose( f_id );

           if( dir->conflag == 0 )
           {
               err_x( "dir_file: cuattname trouble");
           }
           else   /* ignore file */
              return;
        }

        len = 0;
        k   = cuattinq( f_id, i, aa, &dtype, &len );
        if( k == -1 )
        {
           sprintf( PSQL_MSG, "%s -- %s\n", "PSQL BADFILE", nam );
           wrt_msg();
           delete_struct( (cdHd *) ds );
              err_r( );
           j = cuclose( f_id );

           if( dir->conflag == 0 )
           {
               err_x( "dir_file: cuattinq trouble");
           }
           else   /* ignore file */
              return;
        }

        typ_from_cdunif( dtype, &typ, &n );
           err_r( );

        if( typ == cdChar)
           v = malloc( len * n + 1 );
        else
           v = malloc( len * n );

        err_t( v == NULL, "dir_file: memory trouble");


        k = cuattget( f_id, i, aa, v );
        if( k == -1 )
        {
           sprintf( PSQL_MSG, "%s -- %s\n", "PSQL BADFILE", nam );
           wrt_msg();
           delete_struct( (cdHd *) ds );
              err_r( );
           j = cuclose( f_id );

           if( dir->conflag == 0 )
           {
               err_x( "dir_file: cuattget trouble");
           }
           else   /* ignore file */
              return;
        }

       /*---------------------------------
        * Reset name with '_' in non-printing letters.
        *---------------------------------*/
        for( k=0; k < strlen(aa); k++ )
        {
           if( !isgraph( aa[k] ) )
              aa[k] = '_';
        }

        att           = cre_struct( id_cdAtt, (cdHd *) var );
        att->name     = cr_asc_cpy( aa );
        att->datatype = typ;
        att->values   = v;
        if( typ == cdChar)
           att->length = strlen( (char *) v );
        else
           att->length = len;

     }
  }

  j = cuclose( f_id );
  return;
}


/*********************************************************************
 * Function: Special sub-function of 'dir_file'.

 * Given dimension name and file id, scan dimension in a currently
 * opened by 'cdunif' file.  Check if dimension already exists in
 * the tree structure.  If not create it.
 *********************************************************************/


void dir_file_dim( cdms_dir   *dir,     /* directory scanning struct */
                   char       *path,    /* file directory */
                   char       *name,    /* filename */
                   int        f_id,     /* id 'cdunif' file */
                   int        f_ndims,  /* ndims 'cdunif'file */
                   cdDset_new *ds )     /* tree dataset for file */
{
  int        i, j, k, n, f_dblev, ct;
  long       l;
  char       nam[CD_MAX_PATH], units[CD_MAX_PATH], atime[CD_MAX_PATH];
  cdDim_new  *dim;
  cdAtt_new  *att;
  cdHd       *hd;
  CuType     dtype;
  cdType     typ;
  void       *v;
  double     tol, *d1, *d2, *min, *max, *dd;

  f_dblev = 1; /* flag to not put dimension at database level */


 /*---------------------------------
  * Loop over dimensions.
  *---------------------------------*/


  for( i=0; i < f_ndims; i++ )
  {


    /*---------------------------------
     * Read in dimension.
     *---------------------------------*/


     l = 0;
     j = cudiminq( f_id, i, nam, units, &dtype, NULL, NULL, &l );
            err_t( j == -1, "dir_file_dim: cudiminq trouble");

    /*---------------------------------
     * Reset name with '_' in non-printing letters.
     *---------------------------------*/
     for( j=0; j < strlen(nam); j++ )
     {
        if( !isgraph( nam[j] ) )
           nam[j] = '_';
     }

     typ_from_cdunif( dtype, &typ, &n );
        err_r( );

     if( l == 0 )
        v = NULL;
     else if( dir->tmxflag == 0 && dir->vmxflag == 0 )
        v = NULL;
     else
     {
        v = malloc( l * n );
               err_t( v == NULL, "dir_file_dim: malloc memory trouble");

        j  = cudimget( f_id, i, v );
                err_t( j == -1, "dir_file_dim: cudimget trouble");
     }


    /*---------------------------------
     * Check if dimension already exits at database level.
     *---------------------------------*/


     if( f_dblev )
     {
        dim = NULL;
        hd  = (cdHd *) ds;
     }

     else
     {
        hd = nam_fnd( nam, id_cdDim, (cdHd *) ds );

        dim = (cdDim_new *) hd;

       /* ******* check if lengths match ******* */
        if( dim != NULL && dim->length != l )
           dim = NULL;

       /* ******* check if coordinates match ******* */
        if( dim != NULL && v != NULL )
        {
           if( dim->data == NULL )  /* first dirtree of database */
              dim = NULL;

           else
           {
              d1  = ary_trans( dim->datatype, l, dim->data, cdDouble );
                       err_r( );
              d2  = ary_trans( typ, l, v, cdDouble );
                       err_r( );
              tol = 1.e-17;

              for( j=0; j < l; j++ )
              {
                   if( d1[j] < d2[j] - tol || d1[j] > d2[j] + tol )
                   {
                      dim = NULL;
                      break;
                   }
              }

              free( d1 );
              free( d2 );
           }
        }

       /* ******* check if units match ******* */
        if( dim != NULL && strlen( units ) > 0 )
        {
           for( k=0, att=dim->atts; att; att=att->next )
           {
              if( !strcmp( "units", att->name ) &&
                  att->datatype == cdChar &&
                  !strcmp( units, (char *) att->values ) )
              {
                 k = 1;
                 break;
              }
           }

           if( k == 0 )
              dim = NULL;
        }
     }


    /*---------------------------------
     * Create dimension struct.

     * Note: if hd is NULL, under database, otherwise under dataset.
     *---------------------------------*/


     if( dim == NULL )
     {
        if( hd == NULL )
           dim = cre_struct( id_cdDim, (cdHd *) ds->above );
        else
           dim = cre_struct( id_cdDim, (cdHd *) ds );

        dim->name     = cr_asc_cpy( nam );
        dim->datatype = typ;
        dim->length   = l;
        dim->data     = v;


       /*---------------------------------
        * Create dimension attributes.
        *---------------------------------*/
        att           = cre_struct( id_cdAtt, (cdHd *) dim );
        att->name     = cr_asc_cpy( "psql_file" );
        att->datatype = cdChar;
        att->length   = strlen( name );
        att->values   = cr_asc_cpy( name );

        att           = cre_struct( id_cdAtt, (cdHd *) dim );
        att->name     = cr_asc_cpy( "psql_path" );
        att->datatype = cdChar;
        att->length   = strlen( path );
        att->values   = cr_asc_cpy( path );


       /*---------------------------------
        * Get calendar to use.
        *---------------------------------*/


        att = nam_fnd( "calendar", id_cdAtt, (cdHd *) dim );

        if( att != NULL )
           ct = typ_of_calendar( (char *) att->values );
        else
           ct = typ_of_calendar( NULL );


       /*---------------------------------
        * Dirtree 'trange' option.
        *---------------------------------*/
        if( dir->tmxflag != 0 && l != 0 )
        {
           if( typ_time_dim( nam ) )
           {
              dd = ary_trans( dim->datatype, l, dim->data, cdDouble );
                      err_r( );

              cdRel2Char( ct, units, dd[0], atime );
              att           = cre_struct( id_cdAtt, (cdHd *) dim );
              att->name     = cr_asc_cpy( "psql_tfirst" );
              att->datatype = cdChar;
              att->length   = strlen( atime );
              att->values   = cr_asc_cpy( atime );

              cdRel2Char( ct, units, dd[l-1], atime );
              att           = cre_struct( id_cdAtt, (cdHd *) dim );
              att->name     = cr_asc_cpy( "psql_tlast" );
              att->datatype = cdChar;
              att->length   = strlen( atime );
              att->values   = cr_asc_cpy( atime );

              free( dd );
           }
        }

       /*---------------------------------
        * Dirtree 'minmax' option.
        *---------------------------------*/
        if( dir->vmxflag != 0 && typ != cdChar && l != 0 )
        {
           min = (double *) malloc( sizeof( double ) );
           max = (double *) malloc( sizeof( double ) );
           if( min == NULL || max == NULL )
              err_x( "dir_file_dim: malloc memory trouble" );

           ary_min_max( typ, l, v, min, max );
              err_r( );

           att           = cre_struct( id_cdAtt, (cdHd *) dim );
           att->name     = cr_asc_cpy( "psql_min" );
           att->datatype = cdDouble;
           att->length   = 1;
           att->values   = min;

           att           = cre_struct( id_cdAtt, (cdHd *) dim );
           att->name     = cr_asc_cpy( "psql_max" );
           att->datatype = cdDouble;
           att->length   = 1;
           att->values   = max;
        }

        if( (j = strlen( units )) > 0 )
        {
           att           = cre_struct( id_cdAtt, (cdHd *) dim );
           att->name     = cr_asc_cpy( "units" );
           att->datatype = cdChar;
           att->length   = j;
           att->values   = cr_asc_cpy( units );
        }

        if( dim->id_above == id_cdDset )
        {
           if( v != NULL )
              free( v );
           dim->data = NULL;
        }
     }
  }
  return;
}


/*********************************************************************
 * Function: For a directory file, create dset,variable tree structs.
 *********************************************************************/


void dir_file_noscan( cdms_dir *dir,   /* directory scanning struct */
                      char     *path,  /* current directory path */
                      char     *name,  /* name of file */
                      cdHd     *cur )  /* current tree node */
{
  int           i, j;
  char          nam[CD_MAX_NAME], *aa;
  long          *ll;
  cdAtt_new     *att;
  cdHd          *hd;
  struct stat   *buf;
  struct passwd *pw;
  struct group  *gp;


 /*---------------------------------
  * Find age of file.
  *---------------------------------*/


  sprintf( nam, "%s/%s", path, name );

  buf = calloc( 1, sizeof( struct stat ) );

  i = stat( nam, buf );


 /*---------------------------------
  * Create a dataset struct for this file.
  *---------------------------------*/


  hd = dir_struct( dir, path, cur, 1 );
          err_r( );

  if( hd->id != id_cdDset )
     err_x( "dir_file_noscan: can't handle dset directories yet" );

  att           = cre_struct( id_cdAtt, hd );
  att->name     = cr_asc_cpy( "psql_file" );
  att->datatype = cdChar;
  att->length   = strlen( name );
  att->values   = cr_asc_cpy( name );


 /*---------------------------------
  * Age attribute.
  *---------------------------------*/


  aa = ctime( &buf->st_mtime );
  i  = strlen( aa );

  if( aa[i-1] == '\n' )
     aa[i-1] = '\0';

  att           = cre_struct( id_cdAtt, hd );
  att->name     = cr_asc_cpy( "psql_age" );
  att->datatype = cdChar;
  att->length   = strlen( aa );
  att->values   = cr_asc_cpy( aa );




 /*---------------------------------
  * Length attribute.
  *---------------------------------*/


  ll = (long *) malloc( sizeof( long ) );
  err_t( ll == NULL, "dir_file_noscan: trouble getting memory" );

  *ll = buf->st_size;

  att           = cre_struct( id_cdAtt, hd );
  att->name     = cr_asc_cpy( "psql_size" );
  att->datatype = cdLong;
  att->length   = 1;
  att->values   = ll;


 /*---------------------------------
  * Owner attribute.
  *---------------------------------*/


  if( dir->ownflag )
  {
     pw = getpwuid( buf->st_uid );

     att           = cre_struct( id_cdAtt, hd );
     att->name     = cr_asc_cpy( "psql_owner" );
     att->datatype = cdChar;
     att->length   = strlen( pw->pw_name );
     att->values   = cr_asc_cpy( pw->pw_name );
  }


 /*---------------------------------
  * Group attribute.
  *---------------------------------*/


/*.......debug
  gp = getgrgid( buf->st_gid );

  att           = cre_struct( id_cdAtt, hd );
  att->name     = cr_asc_cpy( "psql_group" );
  att->datatype = cdChar;
  att->length   = strlen( gp->gr_name );
  att->values   = cr_asc_cpy( gp->gr_name );
*/

  free( buf );
  return;
}


/*********************************************************************
 * Function: For current directory create db,dset tree struct.
 *           flg -- 0 create db struct with path attribute
 *           flg -- 1 create dset struct with path attribute
 *           flg -- 2 return last defined db struct
 *           flg -- 3 return last defined dset struct
 *********************************************************************/


cdHd *dir_struct( cdms_dir *dir,   /* struct for directory scanning */
                  char     *path,  /* current directory path */
                  cdHd     *hd,    /* current tree node */
                  int      flg )   /* create 0 -- db, 1 -- dset */
                                   /* last locate 2 -- db, 3 -- dset */
{
  char       nam[CD_MAX_PATH];
  cdDb_new   *db;
  cdDset_new *ds;
  cdAtt_new  *att;


 /*---------------------------------
  * Create database struct.
  *---------------------------------*/


  if( flg == 0 )
  {
     sprintf( nam, "db%s%d", dir->aaflag, ++dir->db_n );

     db       = cre_struct( id_cdDb, NULL );
     db->name = cr_asc_cpy( nam );

     att           = cre_struct( id_cdAtt, (cdHd *) db );
     att->name     = cr_asc_cpy( "psql_path" );
     att->datatype = cdChar;
     att->length   = strlen( path );
     att->values   = cr_asc_cpy( path );

     return (cdHd *) db;
  }


 /*---------------------------------
  * Create dataset struct.
  *---------------------------------*/


  else if( flg == 1 )
  {
     sprintf( nam, "dset%s%d", dir->aaflag, ++dir->ds_n );

     ds       = cre_struct( id_cdDset, hd );
     ds->name = cr_asc_cpy( nam );

     att           = cre_struct( id_cdAtt, (cdHd *) ds );
     att->name     = cr_asc_cpy( "psql_path" );
     att->datatype = cdChar;
     att->length   = strlen( path );
     att->values   = cr_asc_cpy( path );

     return (cdHd *) ds;
  }


 /*---------------------------------
  * Locate last defined database
  *---------------------------------*/


  else if( flg == 2 )
  {
     for( db=DB_ROOT_ADR; db->next != NULL; db=db->next )
        ;

     return (cdHd *) db;
  }


 /*---------------------------------
  * Locate last defined dataset
  *---------------------------------*/


  else
  {
     for( db=DB_ROOT_ADR; db->next != NULL; db=db->next )
        ;

     for( ds=db->dsets; ds->next != NULL; ds=ds->next )
        ;

     return (cdHd *) ds;
  }
}


/*********************************************************************
 * Function: For current directory, list sub-dir. and 'cdunif' files.
 *********************************************************************/


void dir_to_tree( cdms_dir *dir,   /* struct for directory scanning */
                  int      lev,    /* level of directory */
                  cdHd     *cur )  /* current tree node */
{
  int  i, k, l, *ff;
  char **aa, *bb;


 /*---------------------------------
  * Scan current directory for sub-directories and 'cdunif' files.
  *---------------------------------*/


  fnd_dir_list( &aa, &ff, &l, dir->scnflag );
     err_r( );

  if( l < 1 )
     return;

  bb = get_cur_dir( );

  sprintf( PSQL_MSG,
           " \n \n---directory: %s  level: %d\n", bb, lev+1 );
  wrt_msg( );


 /*---------------------------------
  * Loop over files in directory.
  *---------------------------------*/


  for( i=0; i < l; i++ )
  {
     if( ff[i] == 0 )
     {
       /*---------------------------------
        * Open file.  List contents into dataset tree node.
        *---------------------------------*/
        if( dir->scnflag == 0 )
        {
           dir_file( dir, bb, aa[i], cur );
              err_r( );
        }


       /*---------------------------------
        * Don't open file.  Just list it in dataset tree node.
        *---------------------------------*/
        else
        {
           dir_file_noscan( dir, bb, aa[i], cur );
              err_r( );
        }
     }
  }

  if( FPT_TTY == 1 || FPT_PY == 1 )
  {
     for( i=0; i < l; i++ )
     {
        if( ff[i] )
        {
           sprintf( PSQL_MSG, "   ***dir: %s\n", aa[i] );
           wrt_msg( );
        }
        else
        {
           sprintf( PSQL_MSG, "      ***file: %s\n", aa[i] );
           wrt_msg( );
        }
     }
  }


 /*---------------------------------
  * Loop over sub-directories in directory.
  *---------------------------------*/


  if( ++lev < dir->numflag )
  {
     for( i=0; i < l; i++ )
     {
        if( ff[i] )
        {
           chdir( aa[i] );
           dir_to_tree( dir, lev, cur );
              err_r( );
           chdir( ".." );
        }
     }
  }


 /*---------------------------------
  * Release memory.
  *---------------------------------*/


  for( i=0; i < l; i++ )
     free( aa[i] );

  free( aa );
  free( ff );
  free( bb );
  return;
}


/*********************************************************************
 * Function to edit the contents of a struct.

 * Note: All sub-levels and sub-linked-lists] are also edited.
 *********************************************************************/


void edit_struct( cdHd *cur )  /* header of current struct */
{
  extern   int  EDIT_STRUCT_DATA_FLG;
  int           j;
  cdDb_new      *db;
  cdDset_new    *s1, *dset;
  cdVar_new     *var;
  cdDim_new     *dim;
  cdAtt_new     *att;
  cdTmp         *tmp;


 /*---------------------------------
  * Database
  *---------------------------------*/


  if( cur->id == id_cdDb )
  {
     db = (cdDb_new *) cur;

     if( db->name == NULL )
        return;

     edit_struct_hdr( "database" );
     fprintf( FPT_OUT, "\tname = \"%s\" ;\n", db->name );
   
   
     if( db->atts != NULL )
     {
        for( att=db->atts;  att;  att=att->next )
           edit_struct_att( att );
     }
   
     if( db->dims != NULL )
     {
        edit_struct_hdr( "dimensions" );

        for( dim=db->dims;  dim;  dim=dim->next )
           edit_struct_dim( dim, 0 );
     }
   
     if( db->vars != NULL )
     {
        edit_struct_hdr( "variables" );

        for( var=db->vars;  var;  var=var->next )
           edit_struct_var( var, 0 );
     }


    /*---------- ':data' section ----------*/
     if( db->dims != NULL || db->vars != NULL )
     {
        EDIT_STRUCT_DATA_FLG = 1;

        if( db->dims != NULL )
        {
           for( dim=db->dims;  dim;  dim=dim->next )
              edit_struct_dim( dim, 1 );
        }

        if( db->vars != NULL )
        {
           for( var=db->vars;  var;  var=var->next )
              edit_struct_var( var, 1 );
        }

        EDIT_STRUCT_DATA_FLG = 0;
     }


     if( db->dsets != NULL )
     {
        for( s1=db->dsets;  s1;  s1=s1->next )
           edit_struct( (cdHd *) s1 );
     }
  }


 /*---------------------------------
  * Dataset
  *---------------------------------*/


  else if( cur->id == id_cdDset )
  {
     dset = (cdDset_new *) cur;

     if( dset->name == NULL )
        return;

     edit_struct_hdr( "dataset" );
     fprintf( FPT_OUT, "\tname = \"%s\" ;\n", dset->name );

     if( dset->atts != NULL )
     {
        for( att=dset->atts;  att;  att=att->next )
           edit_struct_att( att );
     }

     if( dset->dims != NULL )
     {
        edit_struct_hdr( "dimensions" );

        for( dim=dset->dims;  dim;  dim=dim->next )
           edit_struct_dim( dim, 0 );
     }

     if( dset->vars != NULL )
     {
        edit_struct_hdr( "variables" );

        for( var=dset->vars;  var;  var=var->next )
           edit_struct_var( var, 0 );
     }


    /*---------- ':data' section ----------*/
     if( dset->dims != NULL || dset->vars != NULL )
     {
        EDIT_STRUCT_DATA_FLG = 1;

        if( dset->dims != NULL )
        {
           for( dim=dset->dims;  dim;  dim=dim->next )
              edit_struct_dim( dim, 1 );
        }

        if( dset->vars != NULL )
        {
           for( var=dset->vars;  var;  var=var->next )
              edit_struct_var( var, 1 );
        }

        EDIT_STRUCT_DATA_FLG = 0;
     }
  }


  else
     err_cdms( "edit_struct: bad struct ID", 1 );
  return;
}


/*********************************************************************
 * Function to edit the contents of a attribute struct.
 *********************************************************************/


void edit_struct_att( cdAtt_new *att )  /* attribute */
{
  if( att == NULL || att->length < 1 ) return;

  fprintf( FPT_OUT, "\t\t:" );

  edit_struct_num( att->name, att->datatype, att->length,
                   att->values );
  return;
}


/*********************************************************************
 * Function to edit the contents of a dimension struct.
 *********************************************************************/


void edit_struct_dim( cdDim_new *dim,   /* dimension */
                      int       dedit ) /* 0,1 def or data edit */
{
  int        j;
  char       *aa, *bb;
  cdAtt_new  *att;
  cdTmp      *tmp;


  if( dim == NULL || dim->name == NULL )
     return;


 /*---------------------------------
  * Dimension definition edit
  *---------------------------------*/


  if( dedit == 0 )
  {


    /*---------- Field 1 -- type ----------*/


    /*---------- Field 2 -- (keyword,keyword,...) ----------*/


    /*---------- Field 3 -- datatype ----------*/

     if( dim->datatype == cdByte )
        fprintf( FPT_OUT, "%s ",  "byte" );

     else if( dim->datatype == cdChar )
        fprintf( FPT_OUT, "%s ", "char" );

     else if( dim->datatype == cdShort )
        fprintf( FPT_OUT, "%s ", "short" );

     else if( dim->datatype == cdInt )
        fprintf( FPT_OUT, "%s ", "int" );

     else if( dim->datatype == cdLong )
        fprintf( FPT_OUT, "%s ", "long" );

     else if( dim->datatype == cdFloat )
        fprintf( FPT_OUT, "%s ", "float" );

     else if( dim->datatype == cdDouble )
        fprintf( FPT_OUT, "%s ", "double" );

     else if( dim->datatype == cdLongDouble )
        fprintf( FPT_OUT, "%s ", "longdouble" );

     else if( dim->datatype == cdCharTime )
        fprintf( FPT_OUT, "%s ", "chartime" );

     else
        err_cdms( "edit_struct_dim: bad dimension datatype", 2 );


    /*---------- Field 4 -- name<alias> ----------*/
    /* NOTE: alias names no longer used (see alterfile) */

     fprintf( FPT_OUT, "%s ", dim->name );


    /*---------- Field 5 -- (integer) ----------*/
     fprintf( FPT_OUT, "( %d );\n", dim->length );


     if( dim->atts != NULL )
     {
        for( att=dim->atts;  att;  att=att->next )
           edit_struct_att( att );
     }

     if( dim->units != NULL )
        fprintf( FPT_OUT, "\t\t:units = \"%s\";\n", dim->units );
  }


 /*---------------------------------
  * Dimension data edit
  *---------------------------------*/


  else if( dedit == 1 )
  {
     if( dim->data != NULL )
        edit_struct_num( dim->name, dim->datatype, dim->length,
                         dim->data );
  }
  return;
}


/*********************************************************************
 * Function to edit the header of a metafile section.

 * Note: A specialized sub-function of edit_struct function.
 *********************************************************************/


void edit_struct_hdr( char name[] )  /* metafile section name */
{
  fprintf( FPT_OUT, " \n \n// ----------------------------" );
  fprintf( FPT_OUT, "---------------------------------------\n" );

  fprintf( FPT_OUT, "%s:\n", name );
  return;
}


/*********************************************************************
 * Function to edit the contents of an array.
 *********************************************************************/


void edit_struct_num( char   name[],  /* ascii label for array */
                      cdType typ,     /* type of array */
                      long   len,     /* length of array */
                      void   *ary )   /* array */
{
  extern   int  EDIT_STRUCT_DATA_FLG;
  unsigned char *in_b;
  char          *in_a, **in_alst;
  short         *in_s;
  int           i, j, l, n, num, *in_i;
  long          *in_l;
  float         *in_f;
  double        *in_d;


 /*---------------------------------
  * set pointer input array
  *---------------------------------*/


  if( len < 1 )  return;

  if( EDIT_STRUCT_DATA_FLG == 1 )
  {
     edit_struct_hdr( "data" );

     EDIT_STRUCT_DATA_FLG = 0;
  }

  if( typ == cdByte )
     in_b = (unsigned char *) ary;

  else if( typ == cdChar )
     in_a = (char *) ary;

  else if( typ == cdShort )
     in_s = (short *) ary;

  else if( typ == cdInt )
     in_i = (int *) ary;

  else if( typ == cdLong )
     in_l = (long *) ary;

  else if( typ == cdFloat )
     in_f = (float *) ary;

  else if( typ == cdDouble )
     in_d = (double *) ary;

  else if( typ == cdLongDouble )
     in_d = (double *) ary;

  else if( typ == cdCharTime )
     in_alst = ary;

  else
     err_cdms( "edit_struct_num: bad array type", 1 );



 /*---------------------------------
  * Array edit
  *---------------------------------*/


  fprintf( FPT_OUT, "%s ", name );

  if( typ == cdChar )
  {
     if( in_a[0] == '"' )
        fprintf( FPT_OUT, "= %s ;\n", in_a );
     else
        fprintf( FPT_OUT, "= \"%s\" ;\n", in_a );

     return;
  }

  n   = 0;
  j   = '=';
  num = 4;


  while( n < len )
  {
     l = len - n;

     if( l > num )
        l = num;


     for( i=n; i<n+l; i++, j=',' )
     {
        if( typ == cdByte )
           fprintf( FPT_OUT, "%c %c", j, in_b[i] );
   
        else if( typ == cdShort )
           fprintf( FPT_OUT, "%c %d", j, in_s[i] );
   
        else if( typ == cdInt )
           fprintf( FPT_OUT, "%c %d", j, in_i[i] );
   
        else if( typ == cdLong )
           fprintf( FPT_OUT, "%c %d", j, in_l[i] );
   
        else if( typ == cdFloat )
           fprintf( FPT_OUT, "%c %e", j, in_f[i] );
   
        else if( typ == cdDouble )
           fprintf( FPT_OUT, "%c %e", j, in_d[i] );
   
        else if( typ == cdLongDouble )
           fprintf( FPT_OUT, "%c %e", j, in_d[i] );
   
        else if( typ == cdCharTime )
        {
           if( in_alst[0][0] == '"' )
              fprintf( FPT_OUT, "%c %s\n", j, in_alst[i] );
           else
              fprintf( FPT_OUT, "%c \"%s\"\n", j, in_alst[i] );
        }
     }
   
     n += l;

     if( n < len && typ != cdCharTime )
        fprintf( FPT_OUT, "\n" );
  }

  fprintf( FPT_OUT, " ;\n" );
  return;
}


/*********************************************************************
 * Function to edit the contents of a variable struct.
 *********************************************************************/


void edit_struct_var( cdVar_new *var,   /* variable */
                      int       dedit ) /* 0,1 def or data edit */
{
  int        i, j;
  cdAtt_new  *att;
  cdDim_new  *dim;
  cdTmp      *tmp;


  if( var == NULL || var->name == NULL )
     return;


 /*---------------------------------
  * Variable definition edit
  *---------------------------------*/


  if( dedit == 0 )
  {
    /*---------- Field 1 -- datatype ----------*/

     if( var->datatype == cdByte )
        fprintf( FPT_OUT, "%s ", "byte" );

     else if( var->datatype == cdChar )
        fprintf( FPT_OUT, "%s ", "char" );

     else if( var->datatype == cdShort )
        fprintf( FPT_OUT, "%s ", "short" );

     else if( var->datatype == cdInt )
        fprintf( FPT_OUT, "%s ", "int" );

     else if( var->datatype == cdLong )
        fprintf( FPT_OUT, "%s ", "long" );

     else if( var->datatype == cdFloat )
        fprintf( FPT_OUT, "%s ", "float" );

     else if( var->datatype == cdDouble )
        fprintf( FPT_OUT, "%s ", "double" );

     else if( var->datatype == cdLongDouble )
        fprintf( FPT_OUT, "%s ", "longdouble" );

     else if( var->datatype == cdCharTime )
        fprintf( FPT_OUT, "%s ", "chartime" );

     else
        err_cdms( "edit_struct_var: bad variable datatype", 1 );


    /*---------- Field 2 -- name<alias> ----------*/
    /* NOTE: alias names no longer used (see alterfile) */

     fprintf( FPT_OUT, "%s ", var->name );


    /*---------- Field 3 -- (dim1,dim2,...) ----------*/

     if( var->dim == NULL )
        fprintf( FPT_OUT, "( 0" );

     for( i=0, tmp=var->dim, j='('; i < var->ndims;
          i++, tmp=tmp->next, j=',' )
     {
        dim = tmp->want;
        fprintf( FPT_OUT, "%c %s", j, dim->name );
     }

     fprintf( FPT_OUT, " );\n" );

     if( var->atts != NULL )
     {
        for( att=var->atts;  att;  att=att->next )
           edit_struct_att( att );
     }
  }


 /*---------------------------------
  * Variable data edit
  *---------------------------------*/


  else if( dedit == 1 )
  {
     if( var->data != NULL )
        edit_struct_num( var->name, var->datatype, var->length,
                         var->data );
  }
  return;
}


/*********************************************************************
 * Function to empty contents of struct and release it from memory.

 * Note: All sub-[levels,arrays,linked-lists] are released.

 * Note: If 'cur' is itself part of a linked-list, be sure to splice
         it out before calling 'empty_struct'.  This routine will not.
 *********************************************************************/


void *empty_struct( cdHd *cur )  /* header of current struct */
{
  int        i;
  char       **vlst;
  cdDb_new   *db;
  cdDset_new *dset;
  cdVar_new  *var;
  cdDim_new  *dim;
  cdAtt_new  *att;
  cdTmp      *tmp;
  cdPql      *pql;
  void       *v;


 /*---------------------------------
  * Database
  *---------------------------------*/


  if( cur->id == id_cdDb )
  {
     db = (cdDb_new *) cur;

     if( db->dsets != NULL )
     {
        for( dset=db->dsets; dset; )
           dset = empty_struct( (cdHd *) dset );
     }

     if( db->vars != NULL )
     {
        for( var=db->vars; var; )
           var = empty_struct( (cdHd *) var );
     }

     if( db->dims != NULL )
     {
        for( dim=db->dims; dim; )
           dim = empty_struct( (cdHd *) dim );
     }

     if( db->atts != NULL )
     {
        for( att=db->atts; att; )
           att = empty_struct( (cdHd *) att );
     }

     if( db->pqls != NULL )
     {
        for( pql=db->pqls; pql; )
           pql = empty_struct( (cdHd *) pql );
     }

     if( db->name != NULL )
        free( db->name );

     v = (void *) db->next;
     free( db );
     return v;
  }


 /*---------------------------------
  * Dataset
  *---------------------------------*/


  else if( cur->id == id_cdDset )
  {
     dset = (cdDset_new *) cur;

     if( dset->vars != NULL )
     {
        for( var=dset->vars; var; )
           var = empty_struct( (cdHd *) var );
     }

     if( dset->dims != NULL )
     {
        for( dim=dset->dims; dim; )
           dim = empty_struct( (cdHd *) dim );
     }

     if( dset->atts != NULL )
     {
        for( att=dset->atts; att; )
           att = empty_struct( (cdHd *) att );
     }

     if( dset->name != NULL )
        free( dset->name );

     v = (void *) dset->next;
     free( dset );
     return v;
  }


 /*---------------------------------
  * Variable
  *---------------------------------*/


  else if( cur->id == id_cdVar )
  {
     var = (cdVar_new *) cur;

     if( var->atts != NULL )
     {
        for( att=var->atts; att; )
           att = empty_struct( (cdHd *) att );
     }

     if( var->dim != NULL )
     {
        for( tmp=var->dim; tmp; )
           tmp = empty_struct( (cdHd *) tmp );
     }

     if( var->name != NULL )
        free( var->name );

     if( var->data != NULL )
        free( var->data );

     v = (void *) var->next;
     free( var );
     return v;
  }


 /*---------------------------------
  * Dimension
  *---------------------------------*/


  else if( cur->id == id_cdDim )
  {
     dim = (cdDim_new *) cur;

     if( dim->atts != NULL )
     {
        for( att=dim->atts; att; )
           att = empty_struct( (cdHd *) att );
     }

     if( dim->name != NULL )
        free( dim->name );

     if( dim->units != NULL )
        free( dim->units );

     if( dim->data != NULL )
        free( dim->data );

     v = (void *) dim->next;
     free( dim );
     return v;
  }


 /*---------------------------------
  * Attribute
  *---------------------------------*/


  else if( cur->id == id_cdAtt )
  {
     att = (cdAtt_new *) cur;

     if( att->name != NULL )
        free( att->name );

     if( att->values != NULL )
     {
        if( att->datatype == cdCharTime )
        {
           vlst = att->values;

           for( i=0; i < att->length; i++ )
              free( vlst[i] );
        }

        free( att->values );
     }

     v = (void *) att->next;
     free( att );
     return v;
  }


 /*---------------------------------
  * Substitution Struct
  *---------------------------------*/


  else if( cur->id == id_cdTmp )
  {
     tmp = (cdTmp *) cur;

     if( tmp->nam_want != NULL )
        free( tmp->nam_want );

     v = (void *) tmp->next;
     free( tmp );
     return v;
  }


 /*---------------------------------
  * Pql list Struct
  *---------------------------------*/


  else if( cur->id == id_cdPql )
  {
     pql = (cdPql *) cur;

     if( pql->name != NULL )
        free( pql->name );

     if( pql->pqlmsg != NULL )
        free( pql->pqlmsg );

     if( pql->list != NULL )
        free( pql->list );

     v = (void *) pql->next;
     free( pql );
     return v;
  }

  else
     err_cdms( "empty_struct: bad struct ID", 1 );
  return NULL;
}


/*********************************************************************
 * Function to scan for ending '"' of a quoted string.
 *********************************************************************/


int ending_quote( char *asc_line,  /* struct metafile line */
                  int       idx1,  /* char start index */
                  int       idx2 ) /* char stop+1 index */
{
  int  i, j, k, l, n, f, hh[3];

  f = 1;
  i = idx1;


 /*---------------------------------
  * Loop over quotes untill find ending '"' or end-of-line.
  *---------------------------------*/


  while( f )
  {


    /*---------------------------------
     * Scan for '"'.
     *---------------------------------*/


     for( ; i < idx2; i++ )
     {
        if( asc_line[i] == '"' )
           break;
     }

     if( i == idx2 )
        return idx2;

     if( asc_line[i] != '"' )
        err_xi( "ending_quote: code logic error" );


    /*---------------------------------
     * After the '"', find the next 3 non-space letters.
     *---------------------------------*/


     for( k=i+1, n=0; k < idx2; k++ )
     {
        j = asc_line[k];

        if( j == ' ' || j == '\t' || j == '\n' )
           ;
        else
           hh[n++] = j;

        if( n == 3 )
           break;
     }


    /*---------------------------------
     * Determine if '"' is interior to string or ends quoted string.
     *---------------------------------*/


    /*---------------------------------
     * If n=0, accept '"' as ending the quoted string.
     *---------------------------------*/

     if( n == 0 )
        return i;

    /*---------------------------------
     * If n=1 and it's (;,), accept '"' as ending the quoted string.
     *---------------------------------*/

     else if( n == 1 && ( hh[0] == ';' || hh[0] == ',' ) )
        return i;

    /*---------------------------------
     * If n=3 and it's (,"a), accept '"' as ending the quoted string.
     *---------------------------------*/

     else if( n == 3 && hh[0] == ',' && hh[1] == '"' && hh[2] != '"' )
     {
       /*---------------------------------
        * If here, then it looks like a list of quoted strings
        * separated with commas.  If this is true, then the ending '"'
        * for next quoted string should be on line, and should also be
        * the next '"' encountered.
        * Let's verify it or consider this an interior '"'.
        *---------------------------------*/


        for( l=k; l < idx2; l++ )  /* find end '"' of 2nd string */
        {
           if( asc_line[l] == '"' )
              break;
        }

        if( l < idx2 && asc_line[l] == '"' )
        {
           for( k=l+1, n=0; k < idx2; k++ )  /* next 3 non-space */
           {
              j = asc_line[k];
              if( j == ' ' || j == '\t' || j == '\n' )
                 ;
              else
                 hh[n++] = j;

              if( n == 3 )
                 break;
           }

           if( n == 0 )  /* verify end '"' of 2nd string */
              return i;

           else if( n == 1 && ( hh[0] == ';' || hh[0] == ',' ) )
              return i;

           else if( n == 3 && hh[0] == ',' && hh[1] == '"' && 
                    hh[2] != '"' )
              return i;
        }
     }


    /*---------------------------------
     * If here '"' is interior to string.  Go on to next '"'.
     *---------------------------------*/
     i++;
  }
  return 0;
}


/*********************************************************************
 * Function to scan for ending '*' of a '*abc*' wildcharacter string.
 *********************************************************************/


int ending_star( char *asc_line,  /* struct metafile line */
                 int       idx1,  /* char start index */
                 int       idx2 ) /* char stop+1 index */
{
  int  i, f;


 /*---------------------------------
  * Verify first letter of name is '*'.
  *---------------------------------*/


  if( asc_line[idx1] != '*' )
     err_xi( "ending_star: begin star char. error" );


 /*---------------------------------
  * Scan for a 2nd '*'.
  *---------------------------------*/


  for( i=idx1+1, f=0; i < idx2; i++ )
  {
     if( asc_line[i] == '*' )
        break;
     else if( asc_line[i] == '=' || asc_line[i] == ',' )
     {
        f = 7;
        break;
     }
  }


 /*---------------------------------
  * If between the 2 '*' is '=', or ',' then not a '*abc*' string.
  * It must be a 'name = *file value = *.nc', or a
  *    'filename = *.dic, *.hdf, *.clt, *.nc' pattern.
  * Hence, go back and scan for normal end-of-symbol letter.
  *---------------------------------*/


  if( i == idx2 || f != 0 || asc_line[i] != '*' )
     return 0;

  return i;
}


/*********************************************************************
 * Function to error exit code with a message
 *********************************************************************/


void err_cdms( char *in,  /* error message */
               int  n )   /* error number */
{
  sprintf( PSQL_MSG, "ERROR %d -- %s\n", n, in );
  wrt_msg( );

  exit(1);
  return;
}


/*********************************************************************
 * Function to print an ascii error message.
 *********************************************************************/


void err_m( char *msg )  /* error message */
{
  int i, k;

 /*---------------------------------
  * Prepare to print error message.
  *---------------------------------*/


  /*i       = FPT_TTY;
  FPT_TTY = 1;*/

  sprintf( PSQL_MSG, "ERROR PSQL -- %s\n", msg ); 


 /*---------------------------------
  * Check for 'ql' routines error-control-flag.
  *---------------------------------*/


  if( DB_ROOT_ADR != NULL && DB_ROOT_ADR->f->qlerrflg != 0 )
  {
     k = DB_ROOT_ADR->f->qlerrflg;

     if( k == 1 )
        err_cdms( msg, 33 );

     if( k == 3 )
        FPT_TTY = 0;
  }


 /*---------------------------------
  * If reach here print error message.
  *---------------------------------*/

  if ( FPT_PY )
     fprintf(stderr, PSQL_MSG ); /* Replaced PySys_WriteStderr( PSQL_MSG );*/
  else
     wrt_msg( );

  /*FPT_TTY = i;*/
  return;
}


/*********************************************************************
 * Function: Scan current directory, list sub-dir. and 'cdunif' files.
 *********************************************************************/


void fnd_dir_list( char ***nam,  /* array of dir, cdunif names */
                   int  **flg,   /* flaggs 1 -- dir, 0 -- file */
                   int  *num,    /* number of entries */
                   int  scnflg ) /* scan file flag 0 -- yes, 1 -- no */
{
  int           i, j, k, l, l1, n, *ff, *ff1;
  char          **aa, **aa1, *bb;
  DIR           *dirp;
  struct dirent *dp;
  struct stat   s_buf;


 /*---------------------------------
  * Open current directory.
  *---------------------------------*/


  *num = 0;
  *flg = NULL;
  *nam = NULL;

  bb   = get_cur_dir( );
  dirp = opendir(bb);
  free( bb );
  if( dirp == NULL )
     err_x( "fnd_dir_list: trouble opening directory" );

  l  = 50;
  k  = 0;

  ff = (int *) malloc( l * sizeof(int) );
  aa = malloc( l * sizeof( *aa ) );

  if( ff == NULL || aa == NULL )
     err_x( "fnd_dir_list: trouble getting memory" );


 /*---------------------------------
  * Loop over entries in directory.
  *---------------------------------*/


  while( (dp = readdir(dirp) ) != NULL )
  {
     if( dp->d_name[0] != '.' )
     {
       /*---------------------------------
        * Check if a directory.
        *---------------------------------*/

       if( (stat(dp->d_name, &s_buf) != -1 ) &&
            (S_ISDIR(s_buf.st_mode)) )
	 
        {
           aa[k]   = cr_asc_cpy( dp->d_name );
           ff[k++] = 1;
        }


       /*---------------------------------
        * Check if 'noscan' option.  Just want list of filenames.
        *---------------------------------*/


        else if( scnflg )
        {
           aa[k]   = cr_asc_cpy( dp->d_name );
           ff[k++] = 0;
        }


       /*---------------------------------
        * Check if file is a datafile to be scanned.
        *---------------------------------*/


        else
        {
           n = check_filename( dp->d_name );
                  err_r( );

           if( n )
           {
              aa[k]   = cr_asc_cpy( dp->d_name );
              ff[k++] = 0;
           }
        }


       /*---------------------------------
        * Verify enough memory for names.
        *---------------------------------*/


        if( k + 1 > l )
        {
           l1  = l + 50;

           ff1 = (int *) malloc( l1 * sizeof(int) );
           aa1 = malloc( l1 * sizeof( *aa ) );

           if( ff1 == NULL || aa1 == NULL )
              err_x( "fnd_dir_list: trouble getting memory" );

           n = l * sizeof( int );
           memcpy( ff1, ff, n );
           n = l * sizeof( *aa );
           memcpy( aa1, aa, n );

           free( ff );
           free( aa );

           ff = ff1;
           aa = aa1;
           l  = l1;
        }
     }
  }


 /*---------------------------------
  * Close current directory.
  *---------------------------------*/


  closedir(dirp);


 /*---------------------------------
  * Return answers.
  *---------------------------------*/


  *num = k;
  *nam = aa;
  *flg = ff;
  return;
}


/*********************************************************************
 * Function: if current struct contains a linked-list of desired
             type, return its root address, else return NULL.
 *********************************************************************/


void **fnd_lnk_list( cdms_Id idwant,  /* id of wanted struct */
                     cdHd    *cur )   /* header of current struct */
{
  cdDb_new   *db;
  cdDset_new *ds;
  cdVar_new  *var;
  cdDim_new  *dim;
  cdAtt_new  *att;


 /*---------------------------------
  * search for root address of a linked list of wanted type
  *---------------------------------*/


  if( idwant == id_cdDb )
     return (void **) &DB_ROOT_ADR;

  else if( cur == NULL )
     return NULL;

  else if( cur->id == id_cdDb )
  {
     db = (cdDb_new *) cur;

     if( idwant == id_cdDset )
        return (void **) &db->dsets;

     else if( idwant == id_cdVar )
        return (void **) &db->vars;

     else if( idwant == id_cdDim )
        return (void **) &db->dims;

     else if( idwant == id_cdAtt )
        return (void **) &db->atts;

     else if( idwant == id_cdPql )
        return (void **) &db->pqls;
  }

  else if( cur->id == id_cdDset )
  {
     ds = (cdDset_new *) cur;

     if( idwant == id_cdVar )
        return (void **) &ds->vars;

     else if( idwant == id_cdDim )
        return (void **) &ds->dims;

     else if( idwant == id_cdAtt )
        return (void **) &ds->atts;
  }

  else if( cur->id == id_cdVar )
  {
     var = (cdVar_new *) cur;

     if( idwant == id_cdAtt )
        return (void **) &var->atts;

     else if( idwant == id_cdTmp )
        return (void **) &var->dim;
  }

  else if( cur->id == id_cdDim )
  {
     dim = (cdDim_new *) cur;

     if( idwant == id_cdAtt )
        return (void **) &dim->atts;
  }

  return NULL;
}


/*********************************************************************
 * Function to return copy of a dimensions coordinate array.
 * Output is converted to desired datatype
 *********************************************************************/


void *get_coord( cdDim_new *dim,   /* dimension struct */
                 cdType    typ,    /* wanted datatype */
                 long      *len )  /* output length */
{
  int     i, j;
  char    *aa;
  long    l, n, *ll;
  double  *dd, *dd1, delta;
  void    *v, *va;

  if( dim->id != id_cdDim || dim->length < 1 )
     err_xv( "get_coord: dimension arg" );

  *len = l = dim->length;


 /*---------------------------------
  * Return copy of existing coordinate array
  *---------------------------------*/


  if( dim->data != NULL )
     return ary_trans( dim->datatype, l, dim->data, typ );


 /*---------------------------------
  * Read coordinates from file and convert to output type.
  *---------------------------------*/


  va = rd_dim_array( dim, -1, -1 );
          err_rv( );

  v = ary_trans( dim->datatype, dim->length, va, typ );
         err_rv( );

  free( va );
  return( v );
}


/*********************************************************************
 * Function to return current directory with TempMount removed.

 * Note: 'TempMount' is ascii tempory mount directory.
 * ie. at PCMDI if you type: 'cd /pcmdi/...' to go to a directory,
 * then type: 'pwd' you get '/tmp_mnt/pcmdi/...'
 *********************************************************************/


char *get_cur_dir( void )  /* input directory string */
{
  int  i, j, k;
  char get_dir[CD_MAX_PATH];


 /*---------------------------------
  * Get absolute path.
  *---------------------------------*/


  getwd( get_dir );
  /* getcwd( get_dir, CD_MAX_PATH ); linux finally caught up with the rest of the Unix world. */


 /*---------------------------------
  * Remove any tempory mount point from start of path.
  *---------------------------------*/


  i = 0;

  if( TempMount != NULL )
  {
     k = strlen( TempMount );
     j = 0;

     if( TempMount[0] != '/' )
        j = 1;

     if( !strncmp( &get_dir[j], TempMount, k ) )
        i = j + k;
  }

  return cr_asc_cpy( &get_dir[i] );
}


/*********************************************************************
 * Function to get path from filename or current directory.

 * Note: If filename is '../../name' in a short spanning file,
 *       'mpath' is the directory of the '.cdms' file.
 *********************************************************************/


char *get_path_of_file( char *filename,  /* file with path */
                        char *mpath )    /* NULL or '.cdms' path */
{
  int  i, n;
  char *aa, *bb;

  aa = get_cur_dir( );
  bb = NULL;


 /*---------------------------------
  * Check if 'filename' contains a directory.
  * Note: scan backwards looking for '/' character.
  *---------------------------------*/


  if( filename != NULL )
  {
     bb = cr_asc_cpy( filename );
     n  = strlen( bb );

     for( i=n-1; i > 0; i-- )
     {
        if( bb[i] == '/' )
        {
           bb[i] = '\0';
           break;
        }
     }

     
    /*---------------------------------
     * If found, go to the directory, then ask for path.
     * (ie. user might have given '../../abc/file' as filename)
     *---------------------------------*/


     if( i == 0 )
     {
        free( bb );
        bb = NULL;
     }

     else
     {
        if( mpath != NULL )
           chdir( mpath );
        chdir( bb );
        free( bb );

        bb = get_cur_dir( );

        chdir( aa );
        free( aa );

        return bb;
     }
  }


 /*---------------------------------
  * Get path from current directory code is at.
  *---------------------------------*/


  if( mpath != NULL )
  {
     free( aa );
     bb = cr_asc_cpy( mpath );
     return bb;
  }

  else
     return aa;

}


/*********************************************************************
 * Function: Create default database if it doesn't exist.
 *********************************************************************/


cdDb_new *init_user_db( void )
{
  int        n;
  char       *aa, **vlst;
  long       *lbuf;
  cdDb_new   *db;
  cdAtt_new  *att;
  cdDset_new *ds;


 /*---------------------------------
  * Check if default database already created.
  *---------------------------------*/


  err_i( 0 );

  if( DB_ROOT_ADR != NULL && DB_ROOT_ADR->id == id_cdDb )
     return DB_ROOT_ADR;


 /*---------------------------------
  * Create default database struct.
  *---------------------------------*/


  db          = cre_struct( id_cdDb, NULL );
  db->name    = cr_asc_cpy( "USER_DB" );
  DB_ROOT_ADR = db;


 /*---------------------------------
  * Create python-database global flags/scalars struct.
  *---------------------------------*/


  db->f     = calloc( 1, sizeof( cdDbFlag ) );
  db->f->id = id_cdNone;

  n                = 5;
  db->f->L_multi   = n;
  db->f->multi_ioc = (int *) calloc( 1, n * sizeof(int) );
  db->f->multi_ds  = malloc( n * sizeof(ds) );


 /*---------------------------------
  * Create default filename attribute.
  *---------------------------------*/


  att = cre_struct( id_cdAtt, (cdHd *) db );

  att->name     = cr_asc_cpy( "filename" );
  att->datatype = cdCharTime;
  att->length   = 4;
  att->values   = malloc( 4 * sizeof( aa ));

  vlst    = att->values;
  vlst[0] = cr_asc_cpy( "*.dic" );
  vlst[1] = cr_asc_cpy( "*.hdf" );
  vlst[2] = cr_asc_cpy( "*.ctl" );
  vlst[3] = cr_asc_cpy( "*.nc" );


 /*---------------------------------
  * Create default lenreadbuf attribute.
  *---------------------------------*/

  att = cre_struct( id_cdAtt, (cdHd *) db );

  att->name     = cr_asc_cpy( "lenreadbuf" );
  att->datatype = cdLong;
  att->length   = 1;
  att->values   = malloc( sizeof( long ) );

  lbuf  = att->values;
  *lbuf = 8000000;

 /*---------------------------------
  * Create default system directory tempory mount point.
  *---------------------------------*/


  if( TempMount == NULL )
     TempMount = cr_asc_cpy( "/tmp_mnt" );


 /*---------------------------------
  * Create python message arrays.
  *---------------------------------*/


  l_PY_PSQL_MSG = 0;
  L_PY_PSQL_MSG = 1024;

  PSQL_MSG    = malloc( 132 );
  PY_PSQL_MSG = malloc( L_PY_PSQL_MSG );

  PSQL_MSG[0]    = '\0';
  PY_PSQL_MSG[0] = '\0';

  return db;
}


/*********************************************************************
 * Function to insert end-of-string after names in metafile-line
 *********************************************************************/


void ins_asym_eos( cdms_card  *line )  /* struct metafile line */
{
  int  i, j, n, num_sym, *idx_sym, *len_sym, *cls_sym;
  char *asc_line;


  asc_line = line->asc_line;
  num_sym  = line->num_sym;
  idx_sym  = line->idx_sym;
  len_sym  = line->len_sym;
  cls_sym  = line->cls_sym;


 /*---------------------------------
  * Insert '\0' after ascii symbols in asc_line
  *---------------------------------*/


  for( i=0; i<num_sym; i++ )
  {
     if( cls_sym[i] == 'a' || cls_sym[i] == 'q' ||
         cls_sym[i] == 'i' || cls_sym[i] == 'f' )
     {
        j             = idx_sym[i];
        n             = len_sym[i];
        asc_line[j+n] = '\0';
     }
  }

  return;
}


/*********************************************************************
 * Function: read dataset dim,var arrays not in memory.
 *********************************************************************/


void load_ds_arrays( cdDset_new *dset )  /* dataset struct */
{
  cdVar_new *var;
  cdDim_new *dim;


  if( dset == NULL || dset->id != id_cdDset )
     return;


 /*---------------------------------
  * Loop over dataset dimensions.
  *---------------------------------*/


  if( dset->dims != NULL )
  {
     for( dim=dset->dims; dim; dim=dim->next )
     {
        if( dim->data == NULL && strstr( dim->name, "psql_" ) == NULL )
        {
           dim->data = rd_dim_array( dim, -1, -1 );
                          err_r( );
        }
     }
  }


 /*---------------------------------
  * Loop over dataset variables.
  *---------------------------------*/


  if( dset->vars != NULL )
  {
     for( var=dset->vars; var; var=var->next )
     {
        if( var->data == NULL && strstr( var->name, "psql_" ) == NULL )
        {
           var->data = rd_var_array( var, NULL, NULL );
                          err_r( );
        }
     }
  }
  return;
}


/*********************************************************************
 * Function: get memory for cdms_card struct.
 *********************************************************************/


void mem_cdms_card( cdms_card  **out1 )  /* struct metafile line */
{
  int       i, n;
  cdms_card *line;


 /*---------------------------------
  * Get memory for 'cdms_card' type struct.
  *---------------------------------*/


  line = (cdms_card *) calloc( 1, sizeof( cdms_card ) );

  if( line == NULL )
     err_x( "mem_cdms_card: trouble getting memory" );

  line->L_asc_line = 3072;
  line->L_idx_sym  = 512;

  n              = line->L_asc_line * sizeof(char);
  line->asc_line = (char *) malloc( n );

  n             = line->L_idx_sym * sizeof(int);
  line->idx_sym = (int *) malloc( n );
  line->len_sym = (int *) malloc( n );
  line->cls_sym = (int *) malloc( n );

  if( line->asc_line == NULL || line->idx_sym == NULL ||
      line->len_sym == NULL  || line->cls_sym == NULL )
     err_x( "mem_cdms_card: trouble getting memory" );

  *out1 = line;
  return;
}


/*********************************************************************
 * Function: get memory for cdms_pql_list struct.
 *********************************************************************/


void mem_cdms_pql_list( cdms_pql_list **out2 )  /* pql nodes */
{
  int           i, n;
  cdms_pql_list *pql_lst;


 /*---------------------------------
  * Get memory for 'cdms_pql_list' type struct.
  *---------------------------------*/


  pql_lst = (cdms_pql_list *) calloc( 1, sizeof( cdms_pql_list ) );

  if( pql_lst == NULL )
     err_x( "mem_cdms_pql_list: trouble getting memory" );

  pql_lst->L_list = 3072;

  n             = pql_lst->L_list * sizeof( long );
  pql_lst->list = malloc( n );

  if( pql_lst->list == NULL )
     err_x( "mem_cdms_pql_list: trouble getting memory" );

  *out2 = pql_lst;
  return;
}


/*********************************************************************
 * Function: get or expand memory for 'check' struct.
 *********************************************************************/


cdCheck *mem_check( cdCheck *cka,  /* NULL or check struct */
                    int     ncd,   /* 0 or amount to expand card */
                    int     nal,   /* 0 or amount to expand alias */
                    int     nfs )  /* 0 or amount to expand fstruct */

{
  int      j;
  char     *aa, **carda;
  cdCheck  *ck;
  cdHd     *hd;
  cdDb_new *db;


 /*---------------------------------
  * Get memory for check struct.
  *---------------------------------*/


  if( cka == NULL )
  {
     db = init_user_db( );

     ck = (cdCheck *) calloc( 1, sizeof( cdCheck ) );
     if( ck == NULL )
        err_xv( "mem_check: trouble getting memory1" );

     if( ncd != 0)
     {
        ck->L_card = ncd;
        ck->card   = malloc( ck->L_card * sizeof( aa ) );
        if( ck->card == NULL )
           err_xv( "mem_check: trouble getting memory2" );
     }

     if( nal != 0)
     {
        ck->L_alias = nal;
        ck->alias   = malloc( ck->L_alias * sizeof( aa ) );
        if( ck->alias == NULL )
           err_xv( "mem_check: trouble getting memory3" );
     }

     if( nfs != 0)
     {
        ck->L_fstruct = nfs;
        ck->fstruct   = malloc( ck->L_fstruct * sizeof( hd ) );
        if( ck->fstruct == NULL )
           err_xv( "mem_check: trouble getting memory4" );
     }

     ck->id = id_cdNone;

     if( db->ckdefs == NULL )
        db->ckdefs = ck;
     else
     {
        for( cka=db->ckdefs; cka->next != NULL; cka=cka->next )
           ;

        cka->next = ck;
     }

     return ck;
  }


 /*---------------------------------
  * Expand memory for existing check struct.
  *---------------------------------*/


  ck = cka;

  if( ck->nalias != 0 && (ck->nalias + nal ) > ck->L_alias )
  {
     ck->L_alias += nal;
     carda       = ck->alias;

     ck->alias   = malloc( ck->L_alias * sizeof(aa) );
     if( ck->alias == NULL )
        err_xv( "mem_check: trouble getting memory5" );

     if( carda != NULL )
     {
        j = ck->nalias * sizeof( aa );
        memcpy( ck->alias, carda, j );
        free( carda );
     }
  }

  if( ck->nfstruct != 0 && (ck->nfstruct + nfs ) > ck->L_fstruct )
  {
     ck->L_fstruct += nfs;

     if( ck->fstruct != NULL )
        free( ck->fstruct );

     ck->fstruct = malloc( ck->L_fstruct * sizeof( hd ) );
     if( ck->fstruct == NULL )
        err_xv( "mem_check: trouble getting memory6" );
  }

  if( ck->ncard != 0 && (ck->ncard + ncd ) > ck->L_card )
  {
     ck->L_card += ncd;
     carda      = ck->card;

     ck->card   = malloc( ck->L_card * sizeof( aa ) );
     if( ck->card == NULL )
        err_xv( "mem_check: trouble getting memory5" );

     if( carda != NULL )
     {
        j = ck->ncard * sizeof( aa );
        memcpy( ck->card, carda, j );
        free( carda );
     }
  }

  return ck;
}


/*********************************************************************
 * Function: get or expand memory for 'span' struct.
 *********************************************************************/


span_list *mem_span_list( span_list *spa,  /* NULL or span struct */
                          int       nfl,   /* n_flist space needed */
                          int       nsc )  /* n_cord space needed */
{
  int       j, n, *il, *fp;
  char      *aa, **fl, **pl;
  double    *sc;
  span_list *span;


 /*---------------------------------
  * Get memory for 'span_list' struct.
  *---------------------------------*/


  if( spa == NULL )
  {
     span = (span_list *) calloc( 1, sizeof( span_list ) );

     if( span == NULL )
        err_xv( "mem_span_list: trouble getting memory1" );
  }
  else
     span = spa;


 /*---------------------------------
  * Expand 1-entry-per-file size arrays.
  *---------------------------------*/


  if( (span->n_flist + nfl + 3) > span->L_flist )
  {
     n = span->L_flist;

     span->L_flist += (nfl + 15);
     fl            = span->flist;
     pl            = span->plist;
     il            = span->fdoff;

     span->flist = malloc( span->L_flist * sizeof(aa) );
     span->plist = malloc( span->L_flist * sizeof(aa) );
     span->fdoff = malloc( span->L_flist * sizeof(int) );

     if( span->flist == NULL || span->plist == NULL ||
         span->fdoff == NULL )
        err_xv( "mem_span_list: trouble getting memory2" );

     if( fl != NULL )
     {
        j = n * sizeof( aa );
        memcpy( span->flist, fl, j );
        free( fl );
     }

     if( pl != NULL )
     {
        j = n * sizeof( aa );
        memcpy( span->plist, pl, j );
        free( pl );
     }

     if( il != NULL )
     {
        j = n * sizeof( int );
        memcpy( span->fdoff, il, j );
        free( il );
     }
  }


 /*---------------------------------
  * Expand coordinate-axis size arrays.
  *---------------------------------*/


  if( (span->n_cord + nsc + 3) > span->L_cord )
  {
     n = span->L_cord;

     span->L_cord += (nsc + 15);
     sc           = span->scord;
     fp           = span->fpoint;

     span->scord  = malloc( span->L_cord * sizeof(double) );
     span->fpoint = malloc( span->L_cord * sizeof(int) );

     if( span->scord == NULL || span->fpoint == NULL )
        err_xv( "mem_span_list: trouble getting memory3" );

     if( sc != NULL )
     {
        j = n * sizeof( double );
        memcpy( span->scord, sc, j );
        free( sc );
     }

     if( fp != NULL )
     {
        j = n * sizeof( int );
        memcpy( span->fpoint, fp, j );
        free( fp );
     }
  }

  return span;
}


/*********************************************************************
 * Function: get memory for new struct of type 'idwant' under 'cur'
 *********************************************************************/


void *mem_struct( cdms_Id idwant,  /* id of desired struct */
                  cdHd    *cur )   /* current struct */
{
  cdHd *hd;
  void *v;


 /*---------------------------------
  * Get memory for new struct
  *---------------------------------*/


  if( idwant == id_cdDb )
     v = calloc( 1, sizeof(cdDb_new) );

  else if( idwant == id_cdDset )
     v = calloc( 1, sizeof(cdDset_new) );

  else if( idwant == id_cdVar )
     v = calloc( 1, sizeof(cdVar_new) );

  else if( idwant == id_cdDim )
     v = calloc( 1, sizeof(cdDim_new) );

  else if( idwant == id_cdAtt )
     v = calloc( 1, sizeof(cdAtt_new) );

  else if( idwant == id_cdTmp )
     v = calloc( 1, sizeof(cdTmp) );

  else if( idwant == id_cdPql )
     v = calloc( 1, sizeof(cdPql) );

  else
  {
     err_cdms( "mem_struct: bad idwant arg", 1 );
  }

  if( v == NULL )
     err_cdms( "mem_struct: trouble getting memory", 2 );

  hd           = (cdHd *) v;
  hd->id       = idwant;
  hd->id_above = id_cdNone;

  if( cur != NULL )
  {
     hd->id_above = cur->id;
     hd->above    = (void *) cur;
  }

  return v;
}


/*********************************************************************
 * Function to process metafile line after '='
 *********************************************************************/


void *meta_after_eq( int       idx,   /* symbol index after '=' */
                     cdms_card *line, /* struct metafile line */
                     cdType    *typ,  /* output type returned */
                     long      *num ) /* output length */
{
  int      i, j, f, k, n, err, num_sym, *idx_sym, *len_sym, *cls_sym;
  char     *asc_line, *aa, **aalst;
  long     *ll;
  double   *dd;

  asc_line = line->asc_line;
  num_sym  = line->num_sym;
  idx_sym  = line->idx_sym;
  len_sym  = line->len_sym;
  cls_sym  = line->cls_sym;


 /*---------------------------------
  * Scan for datatype and determine number of elements.
  *    Also error check ','  ')' or ';' after each element.
  *---------------------------------*/


  err = f = n = 0;

  for( i=idx; i<num_sym; i++ )
  {
     if( cls_sym[i] == '"' )
     {
        if( cls_sym[i+1] != 'q' && cls_sym[i+1] != ','
            && cls_sym[i+1] != ';' && cls_sym[i+1] != ')' )
           err = 1;
     }

     else if( cls_sym[i] == 'q' )
     {
        if( f == 0 )
           f = 'q';

        if( f != 'q' || cls_sym[i+1] != '"' )
           err = 2;

        n++;
     }

     else if( cls_sym[i] == 'i' )
     {
        if( f == 0 )
           f = 'i';

        if( f == 'q' || (cls_sym[i+1] != ',' && cls_sym[i+1] != ';'
            && cls_sym[i+1] != ')') )
           err = 3;

        n++;
     }

     else if( cls_sym[i] == 'f' )
     {
        if( f == 0 || f == 'i' )
           f = 'f';

        if( f != 'f' || (cls_sym[i+1] != ',' && cls_sym[i+1] != ';'
            && cls_sym[i+1] != ')') )
           err = 4;

        n++;
     }

     else if( cls_sym[i] == 'a' )
     {
        if( f == 0 )
           f = 'a';

        if( f != 'a' || (cls_sym[i+1] != ',' && cls_sym[i+1] != ';'
            && cls_sym[i+1] != ')') )
           err = 5;

        n++;
     }
  }

  if( f == 0 || n < 1 || err != 0 )
     err_xv( "meta_after_eq: bad values after =" );


 /*---------------------------------
  * Store Quoted string
  *---------------------------------*/


  if( f == 'q' )
  {
     if( n == 1 )
     {
        *typ = cdChar;
        *num = j = len_sym[idx+1];
        i    = idx_sym[idx+1];
        aa   = (char *) malloc( j + 1 );

        if( aa == NULL )
           err_xv( "meta_after_eq: trouble getting memory" );

        strcpy( aa, &asc_line[i] );
        return (void *) aa;
     }

     *typ = cdCharTime;
     *num = n;

     aalst = malloc( n * sizeof(aa) );
     if( aalst == NULL )
        err_xv( "meta_after_eq: trouble getting memory" );

     for( i=idx, k=0; i<num_sym; i++ )
     {
        if( cls_sym[i] == 'q' )
        {
           j          = idx_sym[i];
           aalst[k++] = cr_asc_cpy( &asc_line[j] );
        }
     }

     return (void *) aalst;
  }


 /*---------------------------------
  * Store list of ascii names
  *---------------------------------*/


  if( f == 'a' )
  {
     if( n == 1 )
     {
        *typ = cdChar;
        *num = j = len_sym[idx];
        i    = idx_sym[idx];
        aa   = (char *) malloc( j + 1 );

        if( aa == NULL )
           err_xv( "meta_after_eq: trouble getting memory" );

        strcpy( aa, &asc_line[i] );
        return (void *) aa;
     }

     *typ = cdCharTime;
     *num = n;

     aalst = malloc( n * sizeof(aa) );
     if( aalst == NULL )
        err_xv( "meta_after_eq: trouble getting memory" );

     for( i=idx, k=0; i<num_sym; i++ )
     {
        if( cls_sym[i] == 'a' )
        {
           j          = idx_sym[i];
           aalst[k++] = cr_asc_cpy( &asc_line[j] );
        }
     }

     return (void *) aalst;
  }


 /*---------------------------------
  * Store Integer
  *---------------------------------*/


  if( f == 'i' )
  {
     *typ = cdLong;
     *num = n;
     ll   = (long *) malloc( n * sizeof(long) );

     if( ll == NULL )
        err_xv( "meta_after_eq: trouble getting memory" );

     k    = 0;

     for( i=idx; i<num_sym; i++ )
     {
        if( cls_sym[i] == 'i' )
        {
           j     = idx_sym[i];
           ll[k] = atol( &asc_line[j] );
           k++;
        }
     }

     return (void *) ll;
  }


 /*---------------------------------
  * Store Floating
  *---------------------------------*/


  *typ = cdDouble;
  *num = n;
  dd   = (double *) malloc( n * sizeof(double) );

  if( dd == NULL )
     err_xv( "meta_after_eq: trouble getting memory" );

  k    = 0;

  for( i=idx; i<num_sym; i++ )
  {
     if( cls_sym[i] == 'i' || cls_sym[i] == 'f' )
     {
        j     = idx_sym[i];
        dd[k] = atof( &asc_line[j] );
        k++;
     }
  }

  return (void *) dd;
}


/*********************************************************************
 * Function to process metafile attribute-definition line
 *********************************************************************/


void *meta_att( cdms_card *line,  /* struct metafile line */
                cdHd      *mas )  /* struct above attribute */
{
  int        i, j, *idx_sym, *cls_sym;
  char       *asc_line;
  cdAtt_new  *att;
  void       *v, *va;

  asc_line = line->asc_line;
  idx_sym  = line->idx_sym;
  cls_sym  = line->cls_sym;


 /*---------------------------------
  * Check for optional struct-above-att argument (name:att = val;)

  * (ie. don't put 'att' under 'mas', switch instead to 'name')
  * Note: 'name' by definition has same id type as 'mas'
  * (ie. if 'mas' is a dataset in database 'ddb', then 'name'
  * must also be a dataset of database 'ddb').
  *---------------------------------*/


  if( mas == NULL )
     err_xv( "meta_att: nothing above attribute" );

  v = (void *) mas;

  if( cls_sym[0] == 'a' )
  {
     j = idx_sym[0];
     v = nam_fnd( &asc_line[j], mas->id, mas );

     if( v == NULL )
        err_xv( "meta_att: bad first symbol" );
  }


 /*---------------------------------
  * Get attribute name and create struct
  *---------------------------------*/


  i = ( cls_sym[0] == ':' ) ? 1 : 2;
  j = idx_sym[i];

  if( cls_sym[i] != 'a' || cls_sym[i+1] != '=' )
     err_xv( "meta_att: att card, name= args" );

  att       = cre_struct( id_cdAtt, (cdHd *) v );
  att->name = cr_asc_cpy( &asc_line[j] );


 /*---------------------------------
  * Process attribute line after '='
  *---------------------------------*/


  va = meta_after_eq( i+2, line, &att->datatype, &att->length );
          err_rv( );

  att->values = va;

  return v;
}


/*********************************************************************
 * Function crack metafile card into symbols
 *********************************************************************/


int meta_str_sym( cdms_card *line, /* struct metafile line */
                  int       idx,   /* char index start of line */
                  int       len )  /* char length of line */
{
  int    i, j, k, n, l, num_sym, *idx_sym, *len_sym, *cls_sym;
  static int f;
  char   *asc_line;


  asc_line = line->asc_line;
  idx_sym  = line->idx_sym;
  len_sym  = line->len_sym;
  cls_sym  = line->cls_sym;

  num_sym = line->num_sym;

  if( num_sym == 0 )
     len_sym[num_sym] = 0;


 /*---------------------------------
  * Loop over letters of line
  *---------------------------------*/


  for( i=idx; i < idx+len; i++ )
  {


    /*---------------------------------
     * check if between quotes "...", so space letter accepted
     *---------------------------------*/


     if( f == 'q' )
     {
        k = ending_quote( asc_line, i, idx+len );
               err_ri( );


       /*---------------------------------
        * Did we find the 2nd '"'.
        *---------------------------------*/
        if( k < idx+len )
        {
           f = 0;
           len_sym[num_sym] += k - i;

           num_sym++;
           idx_sym[num_sym] = i;
           len_sym[num_sym] = 1;
           cls_sym[num_sym] = '"';

           num_sym++;
           len_sym[num_sym] = 0;

           i = k + 1;
        }


       /*---------------------------------
        * Did we run off the end without finding the 2nd '"'.
        *---------------------------------*/
        else
        {
           len_sym[num_sym] += k - i;
           line->len_line    = idx + len;
           line->num_sym     = num_sym;
           return num_sym;
        }
     }


     j = asc_line[i];


    /*---------------------------------
     * check for symbol separater letters
     *---------------------------------*/


     n = 0;

     if( j == '(' )
        n = '(';

     else if( j == ')' )
        n = ')';

     else if( j == '<' )
        n = '<';

     else if( j == '>' )
        n = '>';

     else if( j == '{' )
        n = '{';

     else if( j == '}' )
        n = '}';

     else if( j == ',' )
        n = ',';

     else if( j == ':' )
        n = ':';

     else if( j == ';' )
        n = ';';

     else if( j == '=' )
        n = '=';

     else if( j == '!' )
        n = '!';


    /*---------------------------------
     * check if space ' ' letter
     *---------------------------------*/


     if( j == ' ' || j == '\t' || j == '\n' )
     {
        if( len_sym[num_sym] > 0 )
        {
           num_sym++;
           len_sym[num_sym] = 0;
        }
     }


    /*---------------------------------
     * check if quote '"' letter; the first one
     *---------------------------------*/


     else if( j == '"' )
     {
        if( len_sym[num_sym] > 0 )
           num_sym++;

        idx_sym[num_sym] = i;
        len_sym[num_sym] = 1;
        cls_sym[num_sym] = '"';

        num_sym++;
        idx_sym[num_sym] = i+1;
        len_sym[num_sym] = 0;
        cls_sym[num_sym] = 'q';

        f = 'q';
     }


    /*---------------------------------
     * check if special symbol separater letter
     *---------------------------------*/


     else if( n )
     {
        if( len_sym[num_sym] > 0 )
           num_sym++;

        idx_sym[num_sym] = i;
        len_sym[num_sym] = 1;
        cls_sym[num_sym] = n;

        num_sym++;
        len_sym[num_sym] = 0;
     }


    /*---------------------------------
     * if here it must be a letter of a symbol or number
     *---------------------------------*/


     else
     {
       /*---------------------------------
        * i,a,f type symbol from first letter
        *---------------------------------*/
        if( len_sym[num_sym] == 0 )
        {
           idx_sym[num_sym] = i;
           cls_sym[num_sym] = 'a';

           if( j == '+' || j == '-' )
              cls_sym[num_sym] = 'i';

           if( j >= '0' && j <= '9' )
           {
              cls_sym[num_sym] = 'i';

              if( asc_line[i+1] == '_' )   /* 1_levels */
                 cls_sym[num_sym] = 'a';
           }

          /*---------------------------------
           * check if . means float or ascii
           *---------------------------------*/
           if( j == '.' )
           {
              k = asc_line[i+1];

              if( k == ' ' || k == '.' || k == '/' )
                 ;
              else
                 cls_sym[num_sym] = 'f';
           }

          /*---------------------------------
           * check if between stars *abc*, so space letter accepted
           *---------------------------------*/
           if( j == '*' )
           {
              k = ending_star( asc_line, i, idx+len );
                     err_ri( );

              if( k != 0 )
              {
                 len_sym[num_sym] = k - i;
                 i = k;
              }
           }

          /*---------------------------------
           * check if // comment card
           *---------------------------------*/
           if( j == '/' )
           {
              if( asc_line[i+1] == '/' )
                 break;
           }
        }

        len_sym[num_sym]++;

        if( cls_sym[num_sym] == 'i' && ( j == '.' || j == 'e'
            || j == 'E' ) )
           cls_sym[num_sym] = 'f';
     }
  }

  line->len_line = idx + len;
  line->num_sym  = num_sym;
  return num_sym;
}


/*********************************************************************
 * Function to find struct,  given name, type, and current struct
 *********************************************************************/


void *nam_fnd( char    name[],  /* name of wanted struct */
               cdms_Id idwant,  /* id of wanted struct */
               cdHd    *cur )   /* header of current struct */
{
  void *v;

  if( idwant == id_cdDb && cur == NULL )
     cur = (cdHd *) DB_ROOT_ADR;


 /*---------------------------------
  * from current point, go up tree-structure a level at a time
  *---------------------------------*/


  while( cur != NULL )
  {
    /*---------------------------------
     * check if idwant-linked-list is in current struct
     *---------------------------------*/


     v = scn_lnk_list( name, idwant, cur );

     if( v != NULL )
        return v;


    /*---------------------------------
     * go up 1 level and repeat search
     *---------------------------------*/


     cur = cur->above;
  }

  return NULL;
}


/*********************************************************************
 * Function to scan tree structure for old psql-attribute-names 
 * and prefix them with 'psql_'.
 *********************************************************************/


void *old_att_name( cdHd *cur )  /* header of current struct */
{
  int        i;
  char       *aa;
  cdDb_new   *db;
  cdDset_new *dset;
  cdVar_new  *var;
  cdDim_new  *dim;
  cdAtt_new  *att;
  cdTmp      *tmp;
  cdPql      *pql;
  void       *v;


 /*---------------------------------
  * Database
  *---------------------------------*/


  if( cur->id == id_cdDb )
  {
     db = (cdDb_new *) cur;

     if( db->dsets != NULL )
     {
        for( dset=db->dsets; dset; )
           dset = old_att_name( (cdHd *) dset );
     }

     if( db->vars != NULL )
     {
        for( var=db->vars; var; )
           var = old_att_name( (cdHd *) var );
     }

     if( db->dims != NULL )
     {
        for( dim=db->dims; dim; )
           dim = old_att_name( (cdHd *) dim );
     }

     if( db->atts != NULL )
     {
        for( att=db->atts; att; )
           att = old_att_name( (cdHd *) att );
     }

     if( db->pqls != NULL )
     {
        for( pql=db->pqls; pql; )
           pql = old_att_name( (cdHd *) pql );
     }

     v = (void *) db->next;
     return v;
  }


 /*---------------------------------
  * Dataset
  *---------------------------------*/


  else if( cur->id == id_cdDset )
  {
     dset = (cdDset_new *) cur;

     if( dset->vars != NULL )
     {
        for( var=dset->vars; var; )
           var = old_att_name( (cdHd *) var );
     }

     if( dset->dims != NULL )
     {
        for( dim=dset->dims; dim; )
           dim = old_att_name( (cdHd *) dim );
     }

     if( dset->atts != NULL )
     {
        for( att=dset->atts; att; )
           att = old_att_name( (cdHd *) att );
     }

     v = (void *) dset->next;
     return v;
  }


 /*---------------------------------
  * Variable
  *---------------------------------*/


  else if( cur->id == id_cdVar )
  {
     var = (cdVar_new *) cur;

     if( var->atts != NULL )
     {
        for( att=var->atts; att; )
           att = old_att_name( (cdHd *) att );
     }

     if( var->dim != NULL )
     {
        for( tmp=var->dim; tmp; )
           tmp = old_att_name( (cdHd *) tmp );
     }

     v = (void *) var->next;
     return v;
  }


 /*---------------------------------
  * Dimension
  *---------------------------------*/


  else if( cur->id == id_cdDim )
  {
     dim = (cdDim_new *) cur;

     if( dim->atts != NULL )
     {
        for( att=dim->atts; att; )
           att = old_att_name( (cdHd *) att );
     }

     v = (void *) dim->next;
     return v;
  }


 /*---------------------------------
  * Attribute
  *---------------------------------*/


  else if( cur->id == id_cdAtt )
  {
     att = (cdAtt_new *) cur;

     if( att->name == NULL )
     {
        v = (void *) att->next;
        return v;
     }

     aa = NULL;

     if( !strcmp( "file", att->name ) )
        aa = cr_asc_cpy( "psql_file" );
     else if( !strcmp( "path", att->name ) )
        aa = cr_asc_cpy( "psql_path" );
     else if( !strcmp( "tfirst", att->name ) )
        aa = cr_asc_cpy( "psql_tfirst" );
     else if( !strcmp( "tlast", att->name ) )
        aa = cr_asc_cpy( "psql_tlast" );
     else if( !strcmp( "filelist", att->name ) )
        aa = cr_asc_cpy( "psql_filelist" );
     else if( !strcmp( "pathlist", att->name ) )
        aa = cr_asc_cpy( "psql_pathlist" );
     else if( !strcmp( "poslist", att->name ) )
        aa = cr_asc_cpy( "psql_poslist" );
     else if( !strcmp( "filepoint", att->name ) )
        aa = cr_asc_cpy( "psql_filepoint" );
     else if( !strcmp( "min", att->name ) )
        aa = cr_asc_cpy( "psql_min" );
     else if( !strcmp( "max", att->name ) )
        aa = cr_asc_cpy( "psql_max" );

     if( aa != NULL )
     {
        free( att->name );
        att->name = aa;
     }

     v = (void *) att->next;
     return v;
  }


 /*---------------------------------
  * Substitution Struct
  *---------------------------------*/


  else if( cur->id == id_cdTmp )
  {
     tmp = (cdTmp *) cur;

     v = (void *) tmp->next;
     return v;
  }


 /*---------------------------------
  * Pql list Struct
  *---------------------------------*/


  else if( cur->id == id_cdPql )
  {
     pql = (cdPql *) cur;

     v = (void *) pql->next;
     return v;
  }

  else
     err_cdms( "old_att_name: bad struct ID", 1 );

  return NULL;
}


/*********************************************************************
 * Function to allow, if called from CDAT, old internal PSQL names.

 * NOTE: All PSQL-generated attribute names are now prefixed with 'psql_'.
 *********************************************************************/


void old_py_name( cdms_card  *line )  /* struct metafile line */
{
  int  i, j, k, n, num_sym, *idx_sym, *len_sym, *cls_sym;
  char *asc_line;


  asc_line = line->asc_line;
  num_sym  = line->num_sym;
  idx_sym  = line->idx_sym;
  len_sym  = line->len_sym;
  cls_sym  = line->cls_sym;


 /*---------------------------------
  * Insert '\0' after ascii symbols in asc_line
  *---------------------------------*/


  for( i=0; i<num_sym; i++ )
  {
     if( cls_sym[i] == 'a' )
     {
        j = idx_sym[i];
        k = line->len_line;

        if( !strcmp( "file", &asc_line[j] ) )
        {
           strcpy( &asc_line[k], "psql_file" );

           idx_sym[i]     = k;
           len_sym[i]     = 9;
           line->len_line = k + 5;
        }

        else if( !strcmp( "path", &asc_line[j] ) )
        {
           strcpy( &asc_line[k], "psql_path" );

           idx_sym[i]     = k;
           len_sym[i]     = 9;
           line->len_line = k + 5;
        }

        else if( !strcmp( "tfirst", &asc_line[j] ) )
        {
           strcpy( &asc_line[k], "psql_tfirst" );

           idx_sym[i]     = k;
           len_sym[i]     = 11;
           line->len_line = k + 7;
        }

        else if( !strcmp( "tlast", &asc_line[j] ) )
        {
           strcpy( &asc_line[k], "psql_tlast" );

           idx_sym[i]     = k;
           len_sym[i]     = 10;
           line->len_line = k + 7;
        }

        else if( !strcmp( "wrtname", &asc_line[j] ) )
        {
           strcpy( &asc_line[k], "psql_wrtname" );

           idx_sym[i]     = k;
           len_sym[i]     = 12;
           line->len_line = k + 7;
        }
     }
  }
  return;
}


/*********************************************************************
 * Function to process pql 'alter' instruction.

 * alter database dbco3 dataset dsetco33 ;
 * alter dataset from bb, ;
 * alter 1984 dataset from bb, ;
 *********************************************************************/


int pql_alter( cdms_card     *line,  /* struct metafile line */
               cdms_pql_list *pql,   /* pql list */
               int           idx )   /* input symbol index */
{
  int  i, j, k;
  cdHd *hd;


 /*---------------------------------
  * Scan key-word symbols at start of input line.
  *---------------------------------*/


  if( DB_ROOT_ADR->f->ALT_PAT != NULL )
  {
     free( DB_ROOT_ADR->f->ALT_PAT );
     DB_ROOT_ADR->f->ALT_PAT = NULL;
  }

  if( line->cls_sym[idx] != 'k' )
  {
     j = line->idx_sym[idx];
     DB_ROOT_ADR->f->ALT_PAT = cr_asc_cpy( &line->asc_line[j] );

     idx++;
  }


 /*---------------------------------
  * Check 1 dataset tree node (ie. file ).
  *---------------------------------*/


  if( line->cls_sym[idx] == 'k' && line->cls_sym[idx+1] == 'a' )
  {
     hd = pql_fld3( line, idx, &i );
             err_ri( );

     alter_dataset( hd );
        err_ri( );
  }


 /*---------------------------------
  * Check list of tree nodes (ie. files ).
  *---------------------------------*/


  else   /* list-of-nodes case */
  {
     k = pql_fld2( line, idx );
            err_ri( );

     i = pql_gen_list( line, pql, idx, k );
            err_ri( );

     for( j=0; j < pql->length; j++ )
     {
        hd = pql->list[j];

        alter_dataset( hd );
           err_ri( );
     }
  }

  return i;
}


/*********************************************************************
 * Function to process pql 'check' instruction.

 * check database dbco3 dataset dsetco33 ;
 * check dataset from bb, ;
 * check fulledit dataset from bb, ;
 *********************************************************************/


int pql_check( cdms_card     *line,  /* struct metafile line */
               cdms_pql_list *pql,   /* pql list */
               int           idx )   /* input symbol index */
{
  int  i, j, k, flg;
  cdHd *hd;


 /*---------------------------------
  * 'fulledit' keyword for tty-write by att,var,dim tree node.
  *---------------------------------*/


  flg = 0;

  if( line->cls_sym[idx] == 'a' )
  {
     j = line->idx_sym[idx];

     if( !strcmp( &line->asc_line[j], "fulledit" ) )
     {
        flg = 1;
        idx++;
     }
  }


 /*---------------------------------
  * Check 1 dataset tree node (ie. file ).
  *---------------------------------*/


  if( line->cls_sym[idx] == 'k' && line->cls_sym[idx+1] == 'a' )
  {
     hd = pql_fld3( line, idx, &i );
             err_ri( );

     check_dataset( hd, flg );
        err_ri( );
  }


 /*---------------------------------
  * Check list of tree nodes (ie. files ).
  *---------------------------------*/


  else   /* list-of-nodes case */
  {
     k = pql_fld2( line, idx );
            err_ri( );

     i = pql_gen_list( line, pql, idx, k );
            err_ri( );

     for( j=0; j < pql->length; j++ )
     {
        hd = pql->list[j];

        check_dataset( hd, flg );
           err_ri( );
     }
  }


  return i;
}


/*********************************************************************
 * Function: compress out from idx-to-end the not-idwant nodes.
 *********************************************************************/


void pql_compress( cdms_pql_list *pql,     /* pql list */
                   int           idx,      /* node index */
                   cdms_Id       idwant )  /* node identifier */
{
  int  i, k;
  cdHd *hd;


 /*---------------------------------
  * Loop over nodes in list.
  *---------------------------------*/


  /*..........debug coding..........*/
err_t( pql->length > pql->L_list,"debug pql_compress");

  for( i=idx, k=idx; i < pql->length; i++ )
  {
     hd = pql->list[i];

     if( hd->id == idwant )
        pql->list[k++] = hd;
  }

  if( k < pql->length )
     pql->length = k;
  return;
}


/*********************************************************************
 * Function to process pql 'dirtree' instruction.

 * dirtree co 5 dset /pcmdi/staff/boyle/winterpark
 * Note: co -- ascii pattern in db,dset names (default co)
 *       5  -- number of dir. levels (default all levels)
 *       dset -- each dir. a dset (default is db)
 *********************************************************************/


int pql_dirtree( cdms_card     *line,  /* struct metafile line */
                 cdms_pql_list *pql,   /* pql list */
                 int           idx )   /* input symbol index */
{
  int        i, j, k, l, f;
  char       *aa;
  cdms_dir   *dir;
  cdDb_new   *db;
  cdDset_new *ds;
  cdDim_new  *dim;
  cdVar_new  *var;
  cdHd       *hd;


 /*---------------------------------
  * Get memory for directory-to-cdms-tree struct.
  *---------------------------------*/


  dir = calloc( 1, sizeof( cdms_dir ) );

  if( dir == NULL )
     err_xi( "pql_dirtree: trouble getting memory" );

  for( db=DB_ROOT_ADR, k=0; db->next != NULL; db=db->next )
     k++;

  dir->db_n    = k;
  dir->numflag = 1024;
  dir->aaflag  = cr_asc_cpy( "co" );


 /*---------------------------------
  * Loop over card symbols.
  *---------------------------------*/


  for( i=idx, f=0; i < line->num_sym; i++ )
  {
     j = line->idx_sym[i];


     if( line->cls_sym[i] == ',' )
        ;

     else if( line->cls_sym[i] == 'a' && line->cls_sym[i+1] == ';' )
     {
        chdir( &line->asc_line[j] );

        aa = get_cur_dir( );

        hd = dir_struct( dir, aa, NULL, f );
                err_ri( );

        free( aa );

        dir_to_tree( dir, 0, hd );
           err_ri( );

        db = (cdDb_new *) hd;

        for( dim=db->dims; dim; dim=dim->next )
        {
           if( dim->data != NULL )
           {
              free( dim->data );
              dim->data = NULL;
           }
        }
     }

     else if( line->cls_sym[i] == 'a' )
     {
       /*---------------------------------
        * Flag: to put files in last database, don't create one.
        *---------------------------------*/
        if( !strcmp( &line->asc_line[j], "nodatabase" ) )
        {
           f = 2;
           db = (cdDb_new *) dir_struct( dir, NULL, NULL, f );
                                err_ri( );
           for( ds=db->dsets, k=0; ds; ds=ds->next, k++ )
              ;
           dir->ds_n = k;
        }


       /*---------------------------------
        * Flag: to just get filename, don't open and scan them.
        *---------------------------------*/
        else if( !strcmp( &line->asc_line[j], "noscan" ) )
        {
           dir->scnflag = 3;
        }


       /*---------------------------------
        * Flag: part of 'noscan', include owner of file.
        *---------------------------------*/
        else if( !strcmp( &line->asc_line[j], "owner" ) )
        {
           dir->ownflag = 3;
        }


       /*---------------------------------
        * Flag: don't stop if I/O library has trouble with file.
        *---------------------------------*/
        else if( !strcmp( &line->asc_line[j], "nostop" ) )
        {
           dir->conflag = 3;
        }


       /*---------------------------------
        * Flag: store min,max attributes for variables,dimensions.
        *---------------------------------*/
        else if( !strcmp( &line->asc_line[j], "minmax" ) )
        {
           dir->vmxflag = 3;
        }


       /*---------------------------------
        * Flag: store begin-end attributes for time dimension.
        *---------------------------------*/
        else if( !strcmp( &line->asc_line[j], "trange" ) )
        {
           dir->tmxflag = 3;
        }


       /*---------------------------------
        * Use ascii pattern in created database,dataset names.
        *---------------------------------*/
        else
        {
           free( dir->aaflag );

           dir->aaflag = cr_asc_cpy( &line->asc_line[j] );
        }
     }

     else if( line->cls_sym[i] == 'i' )
        dir->numflag = atol( &line->asc_line[j] );

     else
        break;
  }


 /*---------------------------------
  * Release memory for directory-to-cdms-tree struct.
  *---------------------------------*/


  free( dir->aaflag );
  free( dir );


 /*---------------------------------
  * Check names for non-printing characters.
  * Verify memory size for list-of-nodes in pql struct.
  *---------------------------------*/


/*.....................debug....................debug.............
.......................debug....................debug.............
.......................debug....................debug.............
  db = (cdDb_new *) hd;
  k  = 0;

  for( dim=(cdDim_new *) db->dims; dim; dim=dim->next )
  {
     aa = dim->name;
     l  = strlen( aa );

     for( j=0; j < l; j++ )
     {
        if( !isgraph( aa[j] ) )
           aa[j] = '_';
     }
  }

  for( ds=(cdDset_new *) db->dsets; ds; ds=ds->next )
  {
     k++;

     for( dim=(cdDim_new *) ds->dims; dim; dim=dim->next )
     {
        aa = dim->name;
        l  = strlen( aa );

        for( j=0; j < l; j++ )
        {
           if( !isgraph( aa[j] ) )
              aa[j] = '_';
        }
     }

     for( var=(cdVar_new *) ds->vars; var; var=var->next )
     {
        aa = var->name;
        l  = strlen( aa );

        for( j=0; j < l; j++ )
        {
           if( !isgraph( aa[j] ) )
              aa[j] = '_';
        }
     }
  }
.......................debug....................debug.............
.......................debug....................debug.............
.......................debug....................debug.............*/

  j = 2 * k - pql->L_list;

  if( j > 0 )
  {
     pql_one_mem( pql, j );
        err_ri( );
  }

  return i;
}


/*********************************************************************
 * Function: execute a PQL instruction.
 *********************************************************************/


void pql_execute( char msg_ln[] )  /* pql message */
{
  int                  i, j, k, l, n, f;
  char                 *msg, *msga, *bb;
  cdHd                 *hd;
  cdPql                *pql;
  static cdms_card     *line = NULL;
  static cdms_pql_list *pql_lst = NULL;


 /*---------------------------------
  * Check if need to initalize structs.
  *---------------------------------*/


  if( ERR_no )   /* check if error in previous pql instruction */
  {
     line->len_line  = 0;
     line->num_sym   = 0;
     pql_lst->length = 0;
     err_i( 0 );
  }

  if( line == NULL )   /* once only get memory for structs */
  {
     mem_cdms_card( &line );
        err_r( );

     mem_cdms_pql_list( &pql_lst );
        err_r( );
  }

  else if( line->num_sym > 0 ) /* check if need to reset */
  {
     j = line->num_sym - 1;

     if( line->cls_sym[j] == ';' )
     {
        line->len_line  = 0;
        line->num_sym   = 0;
        pql_lst->length = 0;
     }
  }


 /*---------------------------------
  * Accumulate line until find ';'
  *---------------------------------*/


  msg = pql_rd_line( line, msg_ln );
           err_r( );

  if( msg == NULL )
     return;


 /*---------------------------------
  * Check if running from CDAT.  If so accept old names 'file','path'.
  *---------------------------------*/


  if( FPT_PY == 1 )
     old_py_name( line );


 /*---------------------------------
  * Note: If here have a complete pql line (ie. line ends with ';').

  * Scan field 1 (optional), name of the output 'cdPql' struct.
  * (ie. card starts with 'name =' or defaults to 'pql_default =')
  *---------------------------------*/


  pql         = pql_fld1( line, 0, &i );
  pql->pqlmsg = msg;
  f           = i_show;


 /*---------------------------------
  * Execute a pql instruction.
  *---------------------------------*/


  if( line->cls_sym[i] == 'k' )
  {
     l = line->len_sym[i];


    /*---------------------------------
     * Select instruction.
     *---------------------------------*/


     if( l == i_select )
     {
        f = i_select;
        i = pql_select( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Show instruction.
     *---------------------------------*/


     else if( l == i_show )
     {
        f = i_show;
        i = pql_show( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Dirtree instruction.
     *---------------------------------*/


     else if( l == i_dirtree )
     {
        f  = i_dirtree;
        bb = get_cur_dir( );
        i  = pql_dirtree( line, pql_lst, i+1 );

        chdir( bb );
        free( bb );
        err_r( );
     }


    /*---------------------------------
     * Release instruction.
     *---------------------------------*/


     else if( l == i_release )
     {
        f = i_release;
        i = pql_release( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Union instruction.
     *---------------------------------*/


     else if( l == i_union )
     {
        f = i_union;
        i = pql_union( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Readmeta instruction.
     *---------------------------------*/


     else if( l == i_readmeta )
     {
        f = i_readmeta;
        i = pql_readmeta( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Writemeta instruction.
     *---------------------------------*/


     else if( l == i_writemeta )
     {
        f = i_writemeta;
        i = pql_writemeta( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Pwd instruction.
     *---------------------------------*/


     else if( l == i_pwd )
     {
        f  = i_pwd;
        bb = get_cur_dir( );

        sprintf( PSQL_MSG, " \n \n***dir: %s\n", bb );
        wrt_msg( );

        free( bb );
        i++;
     }


    /*---------------------------------
     * Cd instruction.
     *---------------------------------*/


     else if( l == i_cd )
     {
        f = i_cd;
        j = line->idx_sym[i+1];

        chdir( &line->asc_line[j] );
        i += 2;
     }


    /*---------------------------------
     * Tempmount instruction.
     *---------------------------------*/


     else if( l == i_tempmount )
     {
        f = i_tempmount;
        if( TempMount != NULL )
           free( TempMount );

        j = line->idx_sym[i+1];
        TempMount = cr_asc_cpy( &line->asc_line[j] );
        i += 2;
     }


    /*---------------------------------
     * End instruction.
     *---------------------------------*/


     else if( l == i_end )
        exit(1);


    /*---------------------------------
     * Group instruction.
     *---------------------------------*/


     else if( l == i_group )
     {
        f = i_group;
        i = pql_group( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Subfgroup instruction.
     *---------------------------------*/


     else if( l == i_subfgroup )
     {
        f = i_subfgroup;
        i = pql_subfgroup( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Ttycopy instruction.
     *---------------------------------*/


     else if( l == i_ttycopy )
     {
        f = i_ttycopy;
        i += 1;

        if( FPT_OUT != NULL )
        {
           fclose( FPT_OUT );
           FPT_OUT = NULL;
        }

        if( line->num_sym > 2 )
        {
           j = line->idx_sym[i];
           FPT_OUT = fopen( &line->asc_line[j], "w+" );
           i += 1;
        }
     }


    /*---------------------------------
     * Filename instruction.
     *---------------------------------*/


     else if( l == i_filename )
     {
        f = i_filename;
        i = pql_filename( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Lenreadbuf instruction.
     *---------------------------------*/


     else if( l == i_lenreadbuf )
     {
        f = i_lenreadbuf;
        i = pql_lenreadbuf( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Readalter instruction.
     *---------------------------------*/


     else if( l == i_readalter )
     {
        f = i_readalter;
        i = pql_readalter( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Alter instruction.
     *---------------------------------*/


     else if( l == i_alter )
     {
        f = i_alter;
        i = pql_alter( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Readcheck instruction.
     *---------------------------------*/


     else if( l == i_readcheck )
     {
        f = i_readcheck;
        i = pql_readcheck( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Check instruction.
     *---------------------------------*/


     else if( l == i_check )
     {
        f = i_check;
        i = pql_check( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Read instruction.
     *---------------------------------*/


     else if( l == i_read )
     {
        f = i_read;
        i = pql_read( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Writevirtual instruction.
     *---------------------------------*/


     else if( l == i_writeds )
     {
        f = i_writeds;
        i = pql_writeds( line, pql_lst, i+1 );
               err_r( );
     }


    /*---------------------------------
     * Virtual instruction.
     *---------------------------------*/


     else if( l == i_virtual )
     {
        msga = cr_asc_cpy( msg );

        i = pql_virtual( line, pql_lst, i+1 );
               err_r( );

        f   = i_virtual;
        msg = pql_rd_line( line, msga );
                 err_r( );
        free( msga );
     }
  }


 /*---------------------------------
  * Store output.
  *---------------------------------*/


  if( line->cls_sym[i] != ';' )
  {
     err_x( "pql_execute: trouble processing pql instruction" );
  }

  else
  {
     if( f == i_select || f == i_union )
     {
        if( pql_lst->length > 0 )
        {
           pql->length = pql_lst->length;
           n           = pql_lst->length * sizeof( long );
           pql->list   = (long *) malloc( n );

           if( pql->list == NULL )
              err_x( "pql_execute: trouble getting memory" );

           for( j=0; j < pql_lst->length; j++ )
           {
              pt_to_long( pql_lst->list[j], pql->list[j] );
           }
        }

        sprintf( PSQL_MSG, "%d nodes stored into %s\n", pql->length,
                 pql->name );
        wrt_msg( );
     }
  }

 /*.........................
  line->len_line  = 0;
  line->num_sym   = 0;
  pql_lst->length = 0;
  ..........................*/
  return;
}


/*********************************************************************
 * Function to process pql 'filename' instruction.
 *********************************************************************/


/*--------------------------------------------------------------------
 * filename = *.nc,  = ecmwf*,  = *ts.avg* ;
 *-------------------------------------------------------------------*/

int pql_filename( cdms_card     *line,  /* struct metafile line */
                  cdms_pql_list *pql,   /* pql list */
                  int           idx )   /* input symbol index */
{
  int       i, j, k, n;
  char      *aa, **vlst;
  cdAtt_new *att;


 /*---------------------------------
  * Determine count of new datafile name filters.
  *---------------------------------*/


  for( i=idx, n=0; i < line->num_sym; i++ )
  {
     if( line->cls_sym[i] == 'a' )
        n++;
  }

  if( n == 0 )
     return line->num_sym - 1;


 /*---------------------------------
  * Get current datafile name filters.
  *---------------------------------*/


  att = scn_lnk_list( "filename", id_cdAtt, (cdHd *) DB_ROOT_ADR );
           err_ti( att == NULL, "pql_filename: code logic error 1" );


 /*---------------------------------
  * Release current filename filters.
  *---------------------------------*/


  vlst = att->values;

  for( i=0; i < att->length; i++ )
     free( vlst[i] );

  free( att->values );


 /*---------------------------------
  * Store new filename filters.
  *---------------------------------*/


  att->length = n;
  att->values = malloc( n * sizeof(aa) );
  vlst        = att->values;

  for( i=idx, j=0; i < line->num_sym; i++ )
  {
     if( line->cls_sym[i] == 'a' )
     {
        k         = line->idx_sym[i];
        vlst[j++] = cr_asc_cpy( &line->asc_line[k] );
     }
  }

  return line->num_sym - 1;
}


/*********************************************************************
 * Function: create/find output user struct for pql instruction.

 * Note: field 1 is first symbols of card.  If 'name =' not given
 *       then defaults to 'pql_default ='.
 *********************************************************************/


cdPql *pql_fld1( cdms_card  *line,     /* struct metafile line */
                 int        idx,       /* input symbol index */
                 int        *outidx )  /* output symbol index */
{
  int   j;
  cdPql *pql;


 /*---------------------------------
  * Check if field 1 given.
  *---------------------------------*/


  if( line->cls_sym[idx] == 'a' && line->cls_sym[idx+1] == '=' )
  {
     j = line->idx_sym[idx];

     pql = scn_lnk_list( &line->asc_line[j], id_cdPql,
                         (cdHd *) DB_ROOT_ADR );

     if( pql == NULL )   /* create struct if it doesn't exist */
     {
        pql       = cre_struct( id_cdPql, (cdHd *) DB_ROOT_ADR );
        pql->name = cr_asc_cpy( &line->asc_line[j] );
     }

     else   /* re-initalize struct */
     {
        if( pql->pqlmsg != NULL )
        {
           free( pql->pqlmsg );
           pql->pqlmsg = NULL;
        }

        if( pql->list != NULL )
        {
           free( pql->list );
           pql->list = NULL;
        }

        pql->length = 0;
     }

     *outidx = idx + 2;
     return pql;
  }


 /*---------------------------------
  * Return default pql.
  *---------------------------------*/


  pql = scn_lnk_list( "pql_default", id_cdPql, (cdHd *) DB_ROOT_ADR );

  if( pql == NULL )   /* create struct if it doesn't exist */
  {
     pql       = cre_struct( id_cdPql, (cdHd *) DB_ROOT_ADR );
     pql->name = cr_asc_cpy( "pql_default" );
  }

  else   /* re-initalize struct */
  {
     if( pql->pqlmsg != NULL )
     {
        free( pql->pqlmsg );
        pql->pqlmsg = NULL;
     }

     if( pql->list != NULL )
     {
        free( pql->list );
        pql->list = NULL;
     }

     pql->length = 0;
  }

  *outidx = idx;
  return pql;
}


/*********************************************************************
 * Function: find index after node identification list.

 * Note: field 2 is 'variable dimension attribute' tree node chain.
 *********************************************************************/


int pql_fld2( cdms_card *line,     /* struct metafile line */
              int       idx )      /* input symbol index */
{
  int i;


 /*---------------------------------
  * Search for end of field 2;
  *---------------------------------*/


  for( i=idx; i < line->num_sym; i++ )
  {
     if( line->cls_sym[i] == ',' )
        ;

     else if( line->cls_sym[i] == 'k' &&
              line->len_sym[i] < L_pql_node_type )
        ;

     else
        break;
  }


 /*---------------------------------
  * Return index after field 2;
  *---------------------------------*/


  if( i == idx )
     err_xi( "pql_fld2: missing tree node chain" );

  return i;
}


/*********************************************************************
 * Function: locate tree node referenced by user.

 * Note: field 3 of a pql instruction is 'database db4 dataset ds5'.
 *       Output is the CDMS tree node, and card index after field 3.
 *********************************************************************/


cdHd *pql_fld3( cdms_card *line,     /* struct metafile line */
                int       idx,       /* input symbol index */
                int       *outidx )  /* output symbol index */
{
  int     i, j, k;
  cdHd    *hd;
  cdms_Id ida;

  hd = NULL;


 /*---------------------------------
  * March across card symbols.
  *---------------------------------*/


  for( i=idx; i < line->num_sym; i++ )
  {
     if( line->cls_sym[i] == ',' )
     {
        i++;
        break;
     }


     else if( line->cls_sym[i] == 'k' &&
              line->len_sym[i] < L_pql_node_type &&
              line->cls_sym[i+1] == 'a' )
     {
       /*---------------------------------
        * Find referenced node under the current node.
        *---------------------------------*/


        j   = line->idx_sym[i+1];
        k   = line->len_sym[i];
        ida = pqlnode[k];

        hd = scn_lnk_list( &line->asc_line[j], ida, hd );

        if( hd == NULL )
           err_xv( "pql_fld3: bad tree node name" );

        i++;
     }

     else
        break;
  }


 /*---------------------------------
  * Return CDMS tree node and card index after field 3.
  *---------------------------------*/


  if( hd == NULL )
     hd = (cdHd *) DB_ROOT_ADR;

  *outidx = i;
  return hd;
}


/*********************************************************************
 * Function to locate card index after a 'where' instruction section.

 * Note: 'where' instruction ends with 'name=aa value=bb length=cc'
 *       node acceptance clause.

 * For example: 'where variable attribute name = Karl;'
 *     'idx' is card index of 'name = Karl'
 *     output = card index of ';'
 *     output flen,fval are    [= 1, != 2, < 3, <= 4, > 5, >= 6]
 *********************************************************************/


int pql_fld4( cdms_card     *line,  /* struct metafile line */
              cdms_pql_list *pql,   /* pql list */
              int           idx )   /* card index testing field */
{
  int i, j, k, n, flg, is, in, il, it, iv, id, ilt, igt, ieq, ine;

  pql->tst_nam = pql->tst_typ = NULL;
  pql->flg_nam = pql->flg_len = pql->flg_typ = pql->flg_val = 0;
  pql->flg_dim = pql->flg_v1d = pql->flg_grp = pql->flg_sid = 0;
  is = in = il = it = iv = id = ilt = igt = ieq = ine = 0;


 /*---------------------------------
  * Search for end of current where instruction section.
  *---------------------------------*/


  for( i=idx; i < line->num_sym; i++ )
  {
     j = line->cls_sym[i];
     k = line->len_sym[i];

     if( i == idx && j == 'a' )           /* default 'name = ...' */
     {
        n            = line->idx_sym[i];
        pql->tst_nam = &line->asc_line[n];
        pql->flg_nam = 1;
     }

     else if( j == ';' )                  /* end of section */
        break;

     else if( j == ',' )                  /* end of section */
     {
        i++;
        break;
     }

     else if( j == 'k' && k == i_id )
        is = 1;

     else if( j == 'k' && k == i_name )
        in = 1;

     else if( j == 'k' && k == i_length )
        il = 1;

     else if( j == 'k' && k == i_type )
        it = 1;

     else if( j == 'k' && k == i_value )
        iv = 1;

     else if( j == 'k' && k == i_ndim )
        id = 1;

     else if( j == '<' )
        ilt = 1;

     else if( j == '>' )
        igt = 1;

     else if( j == '=' )
        ieq = 1;

     else if( j == '!' )
        ine = 1;

     else
     {
       /*---------------------------------
        * Set flg '= 1, != 2, < 3, <= 4, > 5, >= 6' 
        *---------------------------------*/


        if( ine )
           flg = 1;
        else if( ilt )
           flg = 3;
        else if( igt )
           flg = 5;
        else
           flg = 0;

        if( ieq )
           flg++;

        if( flg == 0 )
           flg = 1;


       /*---------------------------------
        * Set 'name, length, value, ndim, variable=dimension ' tests.
        *---------------------------------*/


        n = line->idx_sym[i];

        if( is )
        {
           pql->tst_sid = atol( &line->asc_line[n] );
           pql->flg_sid = flg;
        }

        else if( in )
        {
           pql->tst_nam = &line->asc_line[n];
           pql->flg_nam = flg;
        }

        else if( il )
        {
           pql->tst_len = atol( &line->asc_line[n] );
           pql->flg_len = flg;
        }

        else if( it )
        {
           pql->tst_typ = &line->asc_line[n];
           pql->flg_typ = flg;
        }

        else if( iv )
        {
           pql->flg_val = flg;

           if( line->cls_sym[i] == 'i' )
           {
              pql->tst_i_val = atol( &line->asc_line[n] );
              pql->tst_vtyp  = cdLong;
           }

           else if( line->cls_sym[i] == 'f' )
           {
              pql->tst_f_val = atof( &line->asc_line[n] );
              pql->tst_vtyp  = cdDouble;
           }

           else
           {
              pql->tst_a_val = &line->asc_line[n];
              pql->tst_vtyp  = cdChar;
           }
        }

        else if( id )
        {
           pql->tst_dim = atol( &line->asc_line[n] );
           pql->flg_dim = flg;
        }

        else if( j == 'k' && k == i_dimension )
        {
           pql->flg_v1d = flg;
        }

        else if( j == 'k' && k == i_group )
        {
           pql->flg_grp = flg;
        }

        else
        {
           err_xi( "pql_fld4: bad where section end" );
        }

        is = in = il = it = iv = id = ilt = igt = ieq = ine = 0;
     }
  }


 /*---------------------------------
  * Return index after 'where' section.
  *---------------------------------*/


  if( i == idx )
     err_xi( "pql_fld4: bad where section end" );

  return i;
}


/*********************************************************************
 * Function: find index after show edit section.

 * Note: field 5 is 'id name length' editting list.
 *********************************************************************/


int pql_fld5( cdms_card *line,     /* struct metafile line */
              int       idx,       /* input symbol index */
              int       *idx_w )   /* output 'with' index */
{
  int i, j, k, f_w, f_c;

  *idx_w = f_w = f_c = 0;


 /*---------------------------------
  * Search for end of field 2;
  *---------------------------------*/


  for( i=idx; i < line->num_sym; i++ )
  {
     j = line->cls_sym[i];
     k = line->len_sym[i];

     if( f_c )  /* stop on symbol after ',' */
        break;

     else if( j == ',' )
        f_c = i;

     else if( j == ';' )
        break;

     else if( j == 'k' && ( k == i_id || k == i_name || k == i_value 
              || k == i_length || k == i_type || k == i_ndim ) )
        ;

     else if( j == 'k' && ( k == i_with || k == i_for ) )
        f_w = i;

     else if( f_w )  /* accept 'with' to comma */
        ;

     else
        break;
  }


 /*---------------------------------
  * Return index after field 5;
  *---------------------------------*/


  if( i == idx || i <= f_w )
     err_xi( "pql_fld5: bad identifier list" );

  *idx_w = f_w;
  return i;
}


/*********************************************************************
 * Function to scan 'with' part of 'show' instruction section.

 * show below, variable name ndim,
               variable attribute name length with name=a* name=b*;
 *********************************************************************/


int pql_fld6( cdms_card     *line,  /* struct metafile line */
              cdms_pql_list *pql,   /* pql list */
              int           idx )   /* card index testing field */
{
  int i, j, k, m, n, flg, is, in, il, it, iv, id, ilt, igt, ieq, ine;

  pql->tst_nam = pql->tst_typ = NULL;
  pql->flg_nam = pql->flg_len = pql->flg_typ = pql->flg_val = 0;
  pql->flg_dim = pql->flg_v1d = pql->flg_grp = pql->flg_sid = 0;
  is = in = il = it = iv = id = ilt = igt = ieq = ine = 0;


 /*---------------------------------
  * Search for end of current where instruction section.
  *---------------------------------*/


  for( i=idx; i < line->num_sym; i++ )
  {
     j = line->cls_sym[i];
     k = line->len_sym[i];

     if( i == idx && j == 'a' )           /* default 'name = ...' */
     {
        n            = line->idx_sym[i];
        pql->tst_nam = &line->asc_line[n];
        pql->flg_nam = 1;
     }

     else if( j == ';' )                  /* end of section */
        break;

     else if( j == ',' )                  /* end of section */
     {
        i++;
        break;
     }

     else if( j == 'k' && k == i_id )
        is = 1;

     else if( j == 'k' && k == i_name )
        in = 1;

     else if( j == 'k' && k == i_length )
        il = 1;

     else if( j == 'k' && k == i_type )
        it = 1;

     else if( j == 'k' && k == i_value )
        iv = 1;

     else if( j == 'k' && k == i_ndim )
        id = 1;

     else if( j == '<' )
        ilt = 1;

     else if( j == '>' )
        igt = 1;

     else if( j == '=' )
        ieq = 1;

     else if( j == '!' )
        ine = 1;

     else
     {
       /*---------------------------------
        * Set flg '= 1, != 2, < 3, <= 4, > 5, >= 6' 
        *---------------------------------*/


        if( ine )
           flg = 1;
        else if( ilt )
           flg = 3;
        else if( igt )
           flg = 5;
        else
           flg = 0;

        if( ieq )
           flg++;

        if( flg == 0 )
           flg = 1;


       /*---------------------------------
        * Set 'name, length, value, ndim, variable=dimension ' tests.
        *---------------------------------*/


        n = line->idx_sym[i];

        if( is )
        {
           pql->tst_sid = atol( &line->asc_line[n] );
           pql->flg_sid = flg;
           if( pql_whr_itest( pql ) )
              return 1;
        }

        else if( in )
        {
           pql->tst_nam = &line->asc_line[n];
           pql->flg_nam = flg;
           if( pql_whr_ntest( pql ) )
              return 1;
        }

        else if( il )
        {
           pql->tst_len = atol( &line->asc_line[n] );
           pql->flg_len = flg;
           if( pql_whr_ltest( pql ) )
              return 1;
        }

        else if( it )
        {
           pql->tst_typ = &line->asc_line[n];
           pql->flg_typ = flg;
           if( pql_whr_ttest( pql ) )
              return 1;
        }

        else if( iv )
        {
           pql->flg_val = flg;

           if( line->cls_sym[i] == 'i' )
           {
              pql->tst_i_val = atol( &line->asc_line[n] );
              pql->tst_vtyp  = cdLong;
           }

           else if( line->cls_sym[i] == 'f' )
           {
              pql->tst_f_val = atof( &line->asc_line[n] );
              pql->tst_vtyp  = cdDouble;
           }

           else
           {
              pql->tst_a_val = &line->asc_line[n];
              pql->tst_vtyp  = cdChar;
           }
           if( pql_whr_vtest( pql ) )
              return 1;
        }

        else if( id )
        {
           pql->tst_dim = atol( &line->asc_line[n] );
           pql->flg_dim = flg;
           if( pql_whr_dtest( pql ) )
              return 1;
        }

        else if( j == 'k' && k == i_dimension )
        {
           pql->flg_v1d = flg;
           if( pql_whr_1test( pql ) )
              return 1;
        }

        else if( j == 'k' && k == i_group )
        {
           pql->flg_grp = flg;
           if( pql_whr_gtest( pql ) )
              return 1;
        }

        else
        {
           err_xi( "pql_fld6: bad with part of show section" );
        }

        is = in = il = it = iv = id = ilt = igt = ieq = ine = 0;
     }
  }


 /*---------------------------------
  * No match, return flag not to edit node.
  *---------------------------------*/


  if( i == idx )
     err_xi( "pql_fld6: bad with part of show section" );

  return 0;
}


/*********************************************************************
 * Function to process pql 'below database db4 dataset ds3' section.
 *********************************************************************/


/*--------------------------------------------------------------------
 * select variable dimension below database db4 dataset ds3,
          where variable attribute Karl value ECMWF2,
                variable dimension length > 100,
          show dimension name, dimension length;

 * i2   = card index of 'variable dimension'
 * i3   = card index of 'below'
 * i3+1 = card index of 'database db4 dataset ds3'
 * i4   = card index of 'where variable attribute Karl value ECMWF2'
 * i5   = card index of 'show'
 * i6   = card index of ';'
 *-------------------------------------------------------------------*/

int pql_gen_list( cdms_card     *line,  /* struct metafile line */
                  cdms_pql_list *pql,   /* pql list */
                  int           i2,     /* card index 'variable' */
                  int           i3 )    /* card index 'below' */
{
  int        i, j, k, l, n, i4, i5, i6;
  cdHd       *hd, *hda;
  cdPql      *plist;
  cdms_Id    ida;
  cdVar_new  *var;
  cdTmp      *tmp;
  cdDset_new *ds;


  if( line->cls_sym[i3] != 'k' )
     err_xi( "pql_gen_list: bad tree node chain" );

  j  = line->len_sym[i3];



 /*---------------------------------
  * Pql keyword [from].
  *---------------------------------*/


  if( j == i_from )
  {
     if( line->cls_sym[i3+1] != 'a' )
        err_xi( "pql_gen_list: bad pql-list name" );

     n     = pql->length;
     k     = line->idx_sym[i3+1];
     plist = scn_lnk_list( &line->asc_line[k], id_cdPql,
                           (cdHd *) DB_ROOT_ADR );

     if( plist == NULL )
        err_xi( "pql_gen_list: bad pql-list name" );

     if( plist->length > 0 )
     {
       /*---------------------------------
        * Copy nodes from Pql struct.
        *---------------------------------*/


        if( pql->L_list < plist->length )
        {
           pql_one_mem( pql, plist->length );
              err_ri( );
        }

        pql->length += plist->length;

        for( i=0, j=n; i < plist->length; i++ )
        {
           long_to_pt( plist->list[i], pql->list[j++] );
        }


       /*---------------------------------
        * Compress out the non-i2 nodes.
        *---------------------------------*/


        if( i2 != 0 )
        {
           k = line->len_sym[i2];

           pql_compress( pql, n, pqlnode[k] );
        }
     }

     i4 = i3 + 2;

     if( line->cls_sym[i4] == ',' )
        i4++;

     return i4;
  }


 /*---------------------------------
  * Pql keyword [above] followed by a list of nodes.
  *---------------------------------*/


  else if( j == i_above && line->cls_sym[i3+1] == 'a' )
  { 
     n     = pql->length;
     k     = line->idx_sym[i3+1];
     plist = scn_lnk_list( &line->asc_line[k], id_cdPql,
                           (cdHd *) DB_ROOT_ADR );

     if( plist == NULL )
        err_xi( "pql_gen_list: bad pql-list name" );

     if( plist->length > 0 )
     {
        if( pql->L_list < plist->length )
        {
           pql_one_mem( pql, plist->length );
              err_ri( );
        }
        k   = line->len_sym[i2];
        ida = pqlnode[k];

       /*---------------------------------
        * Loop over nodes from Pql struct.
        *---------------------------------*/

        for( i=0, j=n; i < plist->length; i++ )
        {
           long_to_pt( plist->list[i], hd );

          /*---------------------------------
           * Special-case of variables over dimension.
           *---------------------------------*/

           if( ida == id_cdVar && hd->id == id_cdDim )
           {
              ds = hd->above;

              for( var=ds->vars; var; var=var->next )
              {
                 for( tmp=var->dim; tmp; tmp=tmp->next )
                 {
                    hda = tmp->want;

                   /*---------------------------------
                    * Check if 'above' node already in list.
                    *---------------------------------*/
                    if( hda == hd )
                    {
                       for( k=n, l=0; k < j; k++ )
                       {
                          if( pql->list[k] == var )
                             l = 4;
                       }

                       if( l == 0 )
                          pql->list[j++] = var;
                    }
                 }
              }
           }

          /*---------------------------------
           * March up the tree from current node point.
           *---------------------------------*/

           else
           {
              while( hd->id != ida && hd->id != id_cdNone )
                 hd = hd->above;

             /*---------------------------------
              * Check if 'above' node already in list.
              *---------------------------------*/
              if( hd->id == ida )
              {
                 for( k=n, l=0; k < j; k++ )
                 {
                    if( pql->list[k] == hd )
                       l = 4;
                 }

                 if( l == 0 )
                    pql->list[j++] = hd;
              }
           }
        }

        pql->length = j;
     }

     i4 = i3 + 2;

     if( line->cls_sym[i4] == ',' )
        i4++;

     return i4;
  }


 /*---------------------------------
  * Pql keyword [at,below,above] followed by single node.
  *---------------------------------*/


  else if( j == i_at || j == i_below || j == i_above )
  {


    /*---------------------------------
     * Find tree node and index after 'database db4 dataset ds5'.
     *---------------------------------*/


     hd = pql_fld3( line, i3+1, &i4 );
             err_ri( );


    /*---------------------------------
     * Get the nodes [at,below,above] tree position 'hd'.
     *---------------------------------*/


     i = pql->length;
     k = line->len_sym[i2];

     pql_tree( pql, pqlnode[k], j, hd );
        err_ri( );

     return i4;
  }

  return 0;
}


/*********************************************************************
 * Function to process pql 'group' instruction.
 *********************************************************************/


/*--------------------------------------------------------------------
 * group from pp, variable ta dimension time ;

 * i2   = card index of 'from pp'
 * i3   = card index of 'ta'
 * i4   = card index of 'time'
 * i5   = card index of ';'
 *-------------------------------------------------------------------*/

int pql_group( cdms_card     *line,  /* struct metafile line */
               cdms_pql_list *pql,   /* pql list */
               int           idx )   /* input symbol index */
{
  int        i, j, k, n, f, p, i2, i3, i4, i5, i6, ct, ct1,
             outlen, *ff1, pflg;
  double     t, t1, t2, *dd, *dd1;
  long       l;
  char       *aa, **aa1, **aa2, **aa3, *uni, *uni1, msg[120];
  cdDset_new *ds, *dsa;
  cdVar_new  *var, *vara;
  cdDim_new  *dim, *dima;
  cdAtt_new  *att, *atta;
  cdTmp      *tmp, *tmpa;
  cdHd       *hd;
  cdms_group *grp;


 /*---------------------------------
  * Scan symbols on line.  Determine i3, i4, i5.
  *---------------------------------*/


  i2   = idx;
  pflg = 0;
  i3   = i4 = i5 = 0;

  if( line->cls_sym[i2] == 'a' )
  {
     j = line->idx_sym[i2];

     if( !strcmp( "pathlist", &line->asc_line[j] ) )
     {
        pflg = 1;
        i2++;
     }
  }
  
  for( i=i2; i < line->num_sym; i++ )
  {
     if( line->cls_sym[i] == 'k' &&
         line->len_sym[i] == i_variable &&
         line->cls_sym[i+1] == 'a' )
        i3 = ++i;

     else if( line->cls_sym[i] == 'k' &&
         line->len_sym[i] == i_dimension &&
         line->cls_sym[i+1] == 'a' )
        i4 = ++i;

     else if( line->cls_sym[i] == ';' )
     {
        i5 = i;
        break;
     }
  }

  if( i3 == 0 || i4 == 0 || i5 == 0 )
     err_xi( "pql_group: bad instruction line" );


 /*---------------------------------
  * Get list of nodes 'from pp,'
  *---------------------------------*/


  pql_gen_list( line, pql, 0, i2 );
     err_ri( );

  if( pql->length == 0 )
     return i5;


 /*---------------------------------
  * Get memory for the group-instruction-tempory struct.
  *---------------------------------*/


  grp = (cdms_group *) malloc( sizeof( cdms_group ) );
     err_ti( grp == NULL, "pql_group: trouble getting memory" );

  n = pql->length * sizeof( hd );

  grp->vlist = malloc( n );
  grp->dlist = malloc( n );

  if( grp->vlist == NULL || grp->dlist == NULL )
     err_xi( "pql_group: trouble getting memory" );

  n = pql->length * sizeof( int );

  grp->index = malloc( n );
  grp->count = malloc( n );
  grp->order = malloc( n );

  if( grp->index == NULL || grp->count == NULL || grp->order == NULL )
     err_xi( "pql_group: trouble getting memory" );


 /*---------------------------------
  * Loop over dataset list setting variable list.
  *---------------------------------*/


  k = line->idx_sym[i3];

  for( i=0; i < pql->length; i++ )
  {
     grp->vlist[i] = scn_lnk_list( &line->asc_line[k], id_cdVar,
                                   (cdHd *) pql->list[i] );
                        err_ri( );
  }


 /*---------------------------------
  * Loop over variable list setting dimension list.
  *---------------------------------*/


  k = line->idx_sym[i4];

  for( i=0, n=0; i < pql->length; i++ )
  {
    /*---------------------------------
     * Does variable match reference.
debug...missing coding logic
     * same dimension count and order (time,lev,lat,lon)
     * identical dimensions for lev,lat,lon
     * have a time dimension
     *---------------------------------*/


     grp->dlist[i] = NULL;

     if( grp->vlist[i] != NULL )
     {
        var = (cdVar_new *) grp->vlist[i];

        for( tmp=(cdTmp *) var->dim; tmp; tmp=tmp->next )
        {
           dim = (cdDim_new *) tmp->want;

           if( !strcmp( dim->name, &line->asc_line[k] ) )
           {
              grp->dlist[i] = dim;
              n += dim->length;
              break;
           }
        }
     }
  }

  if( n == 0 )
     err_xi( "pql_group: found nothing to group" );

  outlen     = n;
  grp->coord = (double *) malloc( n * sizeof(double) );
  dd1        = (double *) malloc( n * sizeof(double) );
  ff1        = (int *) malloc( n * sizeof(int) );
  aa1        = malloc( n * sizeof(aa) );
  aa2        = malloc( n * sizeof(aa) );
  aa3        = malloc( n * sizeof(aa) );

  if( grp->coord == NULL || dd1 == NULL || ff1 == NULL ||
      aa1 == NULL || aa2 == NULL || aa3 == NULL )
     err_xi( "pql_group: trouble getting memory" );


 /*---------------------------------
  * Loop over dimension list setting index,count list.
  * Also convert each time axis to first files calendar, units.
  *---------------------------------*/


  for( i=0, k=0; i < pql->length; i++ )
  {
     dim = (cdDim_new *) grp->dlist[i];

     if( dim != NULL )
     {
        att = nam_fnd( "calendar", id_cdAtt, (cdHd *) dim );
        if( att != NULL )
           ct = typ_of_calendar( (char *) att->values );
        else
           ct = typ_of_calendar( NULL );

        grp->count[i] = n = dim->length;
        grp->index[i] = k;

        dd = get_coord( dim, cdDouble, &l );
                err_ri( );

        att = scn_lnk_list( "units", id_cdAtt, (cdHd *) dim );
        err_ti( att == NULL, "pql_group: no dim units" );
        uni = (char *) att->values;

        if( i == 0 )
        {
           ct1 = ct;
           uni1 = uni;
        }

        for( j=0; j < n; j++ )
        {
           cdRel2Char( ct, uni, dd[j], msg );
           cdChar2Rel( ct1, msg, uni1, &t );
           grp->coord[k++] = t;
        }

        free( dd );
     }
  }


 /*---------------------------------
  * Loop over dimension list setting order list.
  * .ie Sort the files in ascending time order.
  *---------------------------------*/


  k = 0;
  grp->order[0] = -1;

  for( i=1; i < pql->length; i++ )
  {
     n  = grp->count[i];
     j  = grp->index[i];
     t1 = grp->coord[j+n-1];      /* last coord. of dim. */

     f  = 1;
     j  = k;                      /* start of chain */

    /*---------------------------------
     * Find location in sorted list.
     *---------------------------------*/

     while( f )
     {
       n  = grp->index[j];
       t2 = grp->coord[n];        /* first coord. of dim. */

       if( t1 < t2 )              /* is i's last < j's first */
       {
          grp->order[i] = j;      /* point i to j */
 
          if( j == k )            /* start of chain */
             k = i;
          else
             grp->order[p] = i;   /* point previous to i */

          break;
       }

       if( grp->order[j] == -1 )  /* end of chain */
       {
          grp->order[j] = i;
          grp->order[i] = -1;
          break;
       }
       else                       /* go next chain dimension */
       {
          p = j;
          j = grp->order[j];
       }
     }
  }


 /*---------------------------------
  * Loop across the sorted list.
  * Create output time axis attributes and coordinates.
  * Convert time axis to first spanned files units, calendar.
  *---------------------------------*/


  dim  = (cdDim_new *) grp->dlist[k];

  atta = nam_fnd( "calendar", id_cdAtt, (cdHd *) dim );
  if( atta != NULL )
     ct = typ_of_calendar( (char *) atta->values );
  else
     ct = typ_of_calendar( NULL );

  atta = scn_lnk_list( "units", id_cdAtt, (cdHd *) dim );
  err_ti( atta == NULL, "pql_group: no dim units" );
  uni = (char *) atta->values;

  j    = k;
  n    = 0;
  f    = 1;


  for( i=0; i < pql->length; i++ )
  {
    /*---------------------------------
     * Create contents of 'filelist' attribute.
     *---------------------------------*/
     dim = (cdDim_new *) grp->dlist[j];
     att = scn_lnk_list( "psql_file", id_cdAtt, (cdHd *) dim );
              err_ri( );
     aa1[i] = cr_asc_cpy( att->values );


    /*---------------------------------
     * Create contents of 'pathlist' attribute.
     *---------------------------------*/
     dim = (cdDim_new *) grp->dlist[j];
     att = scn_lnk_list( "psql_path", id_cdAtt, (cdHd *) dim );
              err_ri( );
     aa3[i] = cr_asc_cpy( att->values );


    /*---------------------------------
     * Create contents of 'datasetlist' attribute.
     *---------------------------------*/
     ds  = (cdDset_new *) pql->list[j];
     aa2[i] = cr_asc_cpy( ds->name );
     dsa = scn_lnk_list( ds->name, id_cdDset, (cdHd *) ds->above );
              err_ri( );
     err_ti( dsa != ds, "pql_group: duplicate dataset names" );


     for( p=grp->index[j]; p < grp->index[j] + grp->count[j]; p++ )
     {
        cdRel2Char( ct1, uni1, grp->coord[p], msg );
        cdChar2Rel( ct, msg, uni, &t );
        dd1[n]   = t;
        ff1[n++] = i;
     }

     j = grp->order[j];
  }


 /*---------------------------------
  * Create output dataset struct.
  *---------------------------------*/


  dsa      = (cdDset_new *) pql->list[k];
  vara     = (cdVar_new *) grp->vlist[k];
  dima     = (cdDim_new *) grp->dlist[k];

  ds       = cre_struct( id_cdDset, (cdHd *) dsa->above );
  ds->name = cr_asc_cpy( "ds_group" );

  if( dsa->atts != NULL )  /* copy file attributes */
  {
     for( att=dsa->atts; att; att=att->next )
     {
        if( pflg == 0 && !strcmp( "psql_path", att->name ) )
           ;
        else if( !strcmp(att->name,"psql_min") ||
                 !strcmp(att->name,"psql_max") )
           ;
        else
           cr_att_cpy( att, (cdHd *) ds );
     }
  }

  var           = cre_struct( id_cdVar, (cdHd *) ds );
  var->name     = cr_asc_cpy( vara->name );
  var->ndims    = vara->ndims;
  var->datatype = vara->datatype;

  for( att=vara->atts, i=0; att; att=att->next, i++ )
  {
     if( i < 4 )
     {
        if( !strcmp(att->name,"psql_file") ||
            !strcmp(att->name,"psql_path") ||
            !strcmp(att->name,"psql_min") ||
            !strcmp(att->name,"psql_max") )
           ;

        else
           cr_att_cpy( att, (cdHd *) var );
     }

     else
        cr_att_cpy( att, (cdHd *) var );
  }

  dim           = cre_struct( id_cdDim, (cdHd *) ds );
  dim->name     = cr_asc_cpy( dima->name );
  dim->datatype = cdDouble;
  dim->length   = outlen;
  dim->data     = dd1;

  att           = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name     = cr_asc_cpy( "units" );
  att->datatype = cdChar;
  att->length   = strlen( uni );
  att->values   = cr_asc_cpy( uni );

  att           = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name     = cr_asc_cpy( "psql_filelist" );
  att->datatype = cdCharTime;
  att->length   = pql->length;
  att->values   = aa1;

  if( pflg )
  {
     att           = cre_struct( id_cdAtt, (cdHd *) dim );
     att->name     = cr_asc_cpy( "psql_pathlist" );
     att->datatype = cdCharTime;
     att->length   = pql->length;
     att->values   = aa3;
  }

 /*................................not needed anymore
  att           = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name     = cr_asc_cpy( "psql_datasetlist" );
  att->datatype = cdCharTime;
  att->length   = pql->length;
  att->values   = aa2;
   ................................*/

  att           = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name     = cr_asc_cpy( "psql_filepoint" );
  att->datatype = cdInt;
  att->length   = outlen;
  att->values   = ff1;

  tmp           = cre_struct( id_cdTmp, (cdHd *) var );
  tmp->id_want  = id_cdDim;
  tmp->nam_want = cr_asc_cpy( dim->name );
  tmp->want     = dim;

  l = dim->length;

  for( tmpa=vara->dim->next; tmpa; tmpa = tmpa->next )
  {
     dim = tmpa->want;
     l   = l * dim->length;

     tmp           = cre_struct( id_cdTmp, (cdHd *) var );
     tmp->id_want  = id_cdDim;
     tmp->nam_want = cr_asc_cpy( dim->name );
     tmp->want     = dim;

    /****************debug*****************/
    /**** put other dimensions in dataset as well */
     dima           = cre_struct( id_cdDim, (cdHd *) ds );
     dima->name     = cr_asc_cpy( dim->name );
     dima->datatype = dim->datatype;
     dima->length   = dim->length;
     dima->data     = NULL;
     for( att=dim->atts; att; att=att->next )
     {
        if( att->datatype == cdChar )
        {
           if( pflg || strcmp( "psql_path", att->name ) )
           {
              atta           = cre_struct( id_cdAtt, (cdHd *) dima );
              atta->name     = cr_asc_cpy( att->name );
              atta->datatype = att->datatype;
              atta->length   = att->length;
              atta->values   = cr_asc_cpy( att->values );
           }
        }
     }
  }

  var->length = l;

  free( grp->vlist );
  free( grp->dlist );
  free( grp->index );
  free( grp->count );
  free( grp->order );
  free( grp->coord );
  free( grp );


 /*---------------------------------
  * Because of alterfile wrtshape need ds_group complete (all dims).
  *---------------------------------*/


  dsa = copy_struct( (cdHd *) ds, (cdHd *) ds->above );
           err_ri( );

  delete_struct( (cdHd *) ds );
     err_ri( );

  free( dsa->name );
  dsa->name = cr_asc_cpy( "ds_group" );

  return i5;
}


/*********************************************************************
 * Function to scan pql-line for pql-keywords.

 * If found cls_sym set to 'k', and len_sym is set to id-of-key.
 *********************************************************************/


void pql_key_sym( cdms_card  *line )  /* struct metafile line */
{
  int  i, j, n;


 /*---------------------------------
  * Loop over ascii symbols on line.
  *---------------------------------*/


  for( i=0; i < line->num_sym; i++ )
  {
     if( line->cls_sym[i] == 'a' )
     {
       /*---------------------------------
        * Loop over pql keywords searching for match.
        *---------------------------------*/


        for( j=0; j < L_pql; j++ )
        {
           n = line->idx_sym[i];

           if( !strcmp( pqlkey[j], &line->asc_line[n] ) )
           {
              line->len_sym[i] = j;
              line->cls_sym[i] = 'k';
              break;
           }
        }
     }
  }

  return;
}


/*********************************************************************
 * Function to process pql 'lenreadbuf' instruction.
 *********************************************************************/


/*--------------------------------------------------------------------
 * lenreadbuf = 6000000 ;
 *-------------------------------------------------------------------*/

int pql_lenreadbuf( cdms_card     *line,  /* struct metafile line */
                    cdms_pql_list *pql,   /* pql list */
                    int           idx )   /* input symbol index */
{
  int       i, k;
  long      *lbuf;
  cdAtt_new *att;


 /*---------------------------------
  * Get current datafile name filters.
  *---------------------------------*/


  att = scn_lnk_list( "lenreadbuf", id_cdAtt, (cdHd *) DB_ROOT_ADR );
           err_ti( att == NULL, "pql_lenreadbuf: code logic error" );

  lbuf = att->values;


 /*---------------------------------
  * Store new lenreadbuf filters.
  *---------------------------------*/


  for( i=idx; i < line->num_sym; i++ )
  {
     if( line->cls_sym[i] == 'i' )
     {
        k     = line->idx_sym[i];
        *lbuf = atol( &line->asc_line[k] );

        break;
     }
  }

  return line->num_sym - 1;
}


/*********************************************************************
 * Function: append to pql list major nodes 1 level below 'cur'.
 *********************************************************************/


void pql_one_level( cdms_pql_list *pql,   /* pql list */
                    cdHd          *cur )  /* header of tree struct */
{
  long       k;
  cdDb_new   *db;
  cdDset_new *ds;
  cdVar_new  *var;
  cdDim_new  *dim;
  cdAtt_new  *att;
  cdTmp      *tmp;

  if( cur == NULL )
     return;


 /*---------------------------------
  * Check if enough memory to append nodes.
  *---------------------------------*/


  if( pql->length + 500 > pql->L_list )
  {
     pql_one_mem( pql, 500 );
        err_r( );
  }


 /*---------------------------------
  * Append to list all pertinent nodes 1 level below 'cur'.
  *---------------------------------*/


  k = pql->length;

  if( cur == (cdHd *) DB_ROOT_ADR )
  {
     db = (cdDb_new *) cur;

     for( db=db->next; db; db=db->next )
        pql->list[k++] = db;
  }

  else if( cur->id == id_cdDb )
  {
     db = (cdDb_new *) cur;

     for( ds=(cdDset_new *) db->dsets; ds; ds=ds->next )
        pql->list[k++] = ds;

     for( var=(cdVar_new *) db->vars; var; var=var->next )
        pql->list[k++] = var;

     for( dim=(cdDim_new *) db->dims; dim; dim=dim->next )
        pql->list[k++] = dim;

     for( att=(cdAtt_new *) db->atts; att; att=att->next )
        pql->list[k++] = att;
  }

  else if( cur->id == id_cdDset )
  {
     ds = (cdDset_new *) cur;

     for( var=(cdVar_new *) ds->vars; var; var=var->next )
        pql->list[k++] = var;

     for( dim=(cdDim_new *) ds->dims; dim; dim=dim->next )
        pql->list[k++] = dim;

     for( att=(cdAtt_new *) ds->atts; att; att=att->next )
        pql->list[k++] = att;
  }

  else if( cur->id == id_cdVar )
  {
     var = (cdVar_new *) cur;

     for( att=(cdAtt_new *) var->atts; att; att=att->next )
        pql->list[k++] = att;
  }

  else if( cur->id == id_cdDim )
  {
     dim = (cdDim_new *) cur;

     for( att=(cdAtt_new *) dim->atts; dim; dim=dim->next )
        pql->list[k++] = att;
  }

  pql->length = k;
err_t( pql->length > pql->L_list,"debug pql_one_level");
  return;
}


/*********************************************************************
 * Function: append to pql list all nodes 1 level below 'cur'.
 *********************************************************************/


void pql_one_level_all( cdms_pql_list *pql,   /* pql list */
                        cdHd          *cur )  /* header of tree struct */
{
  long       k;
  cdDb_new   *db;
  cdDset_new *ds;
  cdVar_new  *var;
  cdDim_new  *dim;
  cdAtt_new  *att;
  cdTmp      *tmp;

  if( cur == NULL ) return;


 /*---------------------------------
  * Check if enough memory to append nodes.
  *---------------------------------*/


  if( pql->length + 500 > pql->L_list )
  {
     pql_one_mem( pql, 500 );
        err_r( );
  }


 /*---------------------------------
  * Append to list all pertinent nodes 1 level below 'cur'.
  *---------------------------------*/


  k = pql->length;

  if( cur == (cdHd *) DB_ROOT_ADR )
  {
     db = (cdDb_new *) cur;

     for( db=db->next; db; db=db->next )
        pql->list[k++] = db;
  }

  else if( cur->id == id_cdDb )
  {
     db = (cdDb_new *) cur;

     for( ds=(cdDset_new *) db->dsets; ds; ds=ds->next )
        pql->list[k++] = ds;

     for( var=(cdVar_new *) db->vars; var; var=var->next )
        pql->list[k++] = var;

     for( dim=(cdDim_new *) db->dims; dim; dim=dim->next )
        pql->list[k++] = dim;

     for( att=(cdAtt_new *) db->atts; att; att=att->next )
        pql->list[k++] = att;
  }

  else if( cur->id == id_cdDset )
  {
     ds = (cdDset_new *) cur;

     for( var=(cdVar_new *) ds->vars; var; var=var->next )
        pql->list[k++] = var;

     for( dim=(cdDim_new *) ds->dims; dim; dim=dim->next )
        pql->list[k++] = dim;

     for( att=(cdAtt_new *) ds->atts; att; att=att->next )
        pql->list[k++] = att;
  }

  else if( cur->id == id_cdVar )
  {
     var = (cdVar_new *) cur;

     for( att=(cdAtt_new *) var->atts; att; att=att->next )
        pql->list[k++] = att;

     for( tmp=(cdTmp *) var->dim; tmp; tmp=tmp->next )
        pql->list[k++] = tmp->want;
  }

  else if( cur->id == id_cdDim )
  {
     dim = (cdDim_new *) cur;

     for( att=(cdAtt_new *) dim->atts; att; att=att->next )
        pql->list[k++] = att;
  }

  pql->length = k;
err_t( pql->length > pql->L_list,"debug pql_one_level_all");
  return;
}


/*********************************************************************
 * Function: increase memory for pql's list of nodes struct.
 *********************************************************************/


void pql_one_mem( cdms_pql_list *pql,  /* pql list */
                  int           num )  /* additional memory needed */
{
  int   j, l, n;
  cdTmp *tmp;
  void  **list;


 /*---------------------------------
  * Check if enough memory to append nodes.
  *---------------------------------*/


  n = ( num > pql->L_list ) ? num : pql->L_list;

  if( n < 3000 )
     n = 3000;

  l    = pql->L_list + n;
  list = malloc( l * sizeof(tmp) );

  if( list == NULL )
     err_x( "pql_one_mem: trouble getting memory" );

  if( pql->length > 0 )
  {
     j = pql->length * sizeof(tmp);
     memcpy( list, pql->list, j );
  }

  if( pql->L_list > 0 )
     free( pql->list );

  pql->L_list = l;
  pql->list   = list;
  return;
}


/*********************************************************************
 * Function to accumulate pql line ';' and crack it into symbols
 *********************************************************************/


char *pql_rd_line( cdms_card *line,      /* struct metafile line */
                   char      msg_ln[] )  /* user input line */
{
  int  i, l, n, num_sym, len_str, *idx_sym, *len_sym, *cls_sym;
  char *asc_line, *msg;


 /*---------------------------------
  * Append input line to existing line.
  *---------------------------------*/


  i = line->len_line;

  strcpy( &line->asc_line[i], msg_ln );

  if( FPT_OUT != NULL )
     fprintf( FPT_OUT, "%s\n", &line->asc_line[i] );

  len_str = strlen( &line->asc_line[i] );

  if( len_str > 0 && line->asc_line[i+len_str-1] == '\n' )
     ;
  else
  {
     line->asc_line[i+len_str] = ' ';
     len_str++;
     line->asc_line[i+len_str] = '\0';
  }


 /*---------------------------------
  * Crack line into symbols.
  *---------------------------------*/


  num_sym = meta_str_sym( line, i, len_str );


 /*---------------------------------
  * Check if have a complete pql line.
  *---------------------------------*/


  if( num_sym > 0 && line->cls_sym[num_sym-1] == ';')
  {
     if( FPT_OUT != NULL )
        fprintf( FPT_OUT, "***pql message line\n" );

     msg = cr_asc_cpy( line->asc_line );

     ins_asym_eos( line );  /* insert end-of-string after names */

     pql_key_sym( line );   /* flag pql keywords */

     return msg;
  }


 /*---------------------------------
  * Check if about to overflow asc_line/idx_sym memory
  *---------------------------------*/


  if( line->len_line + 120 > line->L_asc_line )
  {
     l        = line->L_asc_line + 2048;
     asc_line = (char *) malloc( l * sizeof(char) );

     if( asc_line == NULL )
        err_xv( "pql_rd_line: trouble getting memory" );

     memcpy( asc_line, line->asc_line, line->L_asc_line );
     free( line->asc_line );

     line->L_asc_line = l;
     line->asc_line   = asc_line;
  }

  if( line->num_sym + 50 > line->L_idx_sym )
  {
     l       = line->L_idx_sym + 512;
     n       = l * sizeof(int);
     idx_sym = (int *) malloc( n );
     len_sym = (int *) malloc( n );
     cls_sym = (int *) malloc( n );

     if( idx_sym == NULL || len_sym == NULL || cls_sym == NULL )
        err_xv( "pql_rd_line: trouble getting memory" );

     n       = line->L_idx_sym * sizeof(int);
     memcpy( idx_sym, line->idx_sym, n );
     memcpy( len_sym, line->len_sym, n );
     memcpy( cls_sym, line->cls_sym, n );
     free( line->idx_sym );
     free( line->len_sym );
     free( line->cls_sym );

     line->L_idx_sym = l;
     line->idx_sym   = idx_sym;
     line->len_sym   = len_sym;
     line->cls_sym   = cls_sym;
  }

  return NULL;
}


/*********************************************************************
 * Function to process pql 'read' instruction.
 *********************************************************************/


/*--------------------------------------------------------------------
 * read from pp, index variable ta dimension time 17 24, to filename;

 * Note: 'index' is keyword, default is 'value' for t1, t2.
 *       if no t1, t2 given then will prompt for them.
 *       dimensions not referenced means all of axis.
 *       'to' is keyword to write to file.
 * i2   = card index of 'from pp'
 * i3   = card index of 'ta'
 * i4   = card index of 'variable ta'
 * i5   = card index of ';'

 * read database db1 dataset ds3, value variable ta dimension lat;
 *           .....prompt for lat1,lat2 coordinates
 *-------------------------------------------------------------------*/

int pql_read( cdms_card     *line,  /* struct metafile line */
              cdms_pql_list *pql,   /* pql list */
              int           idx )   /* input symbol index */
{
  int          i, f, m, n, i1, i2, i3, i4, i5;
  cdVar_new    *var;
  cdAtt_new    *att;
  var_pql_read *rd;
  void         *vv;
  CuType       dtype;


 /*---------------------------------
  * Find i4, index of 'variable ta dimension time 17 24'
  *---------------------------------*/


  i2 = idx;

  for( i=i2, i4=0, f=0; i < line->num_sym; i++ )
  {
     if( f == 1 &&
         line->cls_sym[i] == 'k' &&
         line->len_sym[i] < L_pql_node_type )
     {
        i4 = i;
        break;
     }

     else if( line->cls_sym[i] == ',' )
        f = 1;
  }

  if( i4 == 0 )
     err_xi( "pql_read: bad read instruction" );


 /*---------------------------------
  * Get dataset node 'from pp,'
  *---------------------------------*/


  i3 = pql_gen_list( line, pql, i4, i2 );
          err_ri( );


 /*---------------------------------
  * Process 'variable ta dimension time 17 24'
  *---------------------------------*/


  i5 = pql_read_sec( line, pql, &rd, i3 );
          err_ri( );

  var = rd->var;


 /*---------------------------------
  * Get memory for output array.
  *---------------------------------*/


  for( i=0, n=1; i < rd->ndims; i++ )
     n *= rd->count[i];

  typ_to_cdunif( var->datatype, &dtype, &m );
     err_ri( );

  vv = malloc( n * m );

  if( vv == NULL )
     err_xi( "pql_read: trouble getting memory" );

  rd->data = vv;


 /*---------------------------------
  * Do multi-file and single file read.
  *---------------------------------*/


  pql_read_fil( rd );
     err_ri( );


 /*---------------------------------
  * Set global output struct.
  *---------------------------------*/

 /*---------------------------------
  * Check if 'to file' output option given.
  *---------------------------------*/


  pql_read_wrt( rd, "cdms.nc" );
     err_ri( );

  pql_read_dmem( rd );

  return i5;
}


/*********************************************************************
 * Function to process pql 'pql_readalter' instruction.

 * readalter /pcmdi/oconnor/chk3 ;
 *********************************************************************/


int pql_readalter( cdms_card     *line,  /* struct metafile line */
                   cdms_pql_list *pql,   /* pql list */
                   int           idx )   /* input symbol index */
{
  int j, n;


 /*---------------------------------
  * Read and store alter file to hidden database tree node.
  *---------------------------------*/


  if( line->cls_sym[idx] != 'a' )
     err_xi( "pql_readalter: bad readcheck instruction" );

  j = line->idx_sym[idx];

  n = rd_alter_file( &line->asc_line[j] );
         err_ri( );

  return ++idx;
}


/*********************************************************************
 * Function to process pql 'readcheck' instruction.

 * readcheck /pcmdi/oconnor/chk3 ;
 *********************************************************************/


int pql_readcheck( cdms_card     *line,  /* struct metafile line */
                   cdms_pql_list *pql,   /* pql list */
                   int           idx )   /* input symbol index */
{
  int j, n;


 /*---------------------------------
  * Read and store check file to hidden database tree node.
  *---------------------------------*/


  if( line->cls_sym[idx] != 'a' )
     err_xi( "pql_readcheck: bad readcheck instruction" );

  j = line->idx_sym[idx];

  n = rd_check_file( &line->asc_line[j] );
         err_ri( );

  return ++idx;
}


/*********************************************************************
 * Function:  compress full array to sub-set output array.
 *********************************************************************/


void pql_read_cmp( var_pql_read *rd,    /* pql read */
                   void         *ary1,  /* output array */
                   void         *ary2,  /* input read array */
                   cdType       typ )   /* array type */
{
  unsigned char *in_b, *o_b;
  short         *in_s, *o_s;
  int           i, j, j1, j2, k, l, ld, m, len, *in_i, *o_i;
  long          *in_l, *o_l, *idx;
  float         *in_f, *o_f;
  double        *in_d, *o_d;


 /*---------------------------------
  * Generate index array to compress ary2.
  *---------------------------------*/


  for( i=0, len=1; i < rd->ndims; i++ )
     len *= rd->rdcnt[i];

  idx = (long *) malloc( len * sizeof(long) );

  if( idx == NULL )
     err_x( "pql_read_cmp: trouble getting memory" );


 /***Process last dimension first.  (in C (i,j,k) k varies first) */

  i = rd->ndims - 1;
  m = rd->index[i];

  for( j=0; j < rd->count[i]; j++ )
     idx[j] = m++;

  l = rd->count[i];
  ld = rd->len[i];


 /***Process last-1 to 0 dimensions */

  for( i=rd->ndims-2; i > -1; i-- )
  {
     m = rd->index[i] * ld;

     for( j=0; j < l; j++ )
        idx[j] += m;

     for( j=1; j < rd->count[i]; j++ )
     {
        j2 = j * l;
        j1 = j2 - l;

        for( k=0; k < l; k++)
           idx[j2++] = idx[j1++] + ld;
     }

     l  *= rd->count[i];
     ld *= rd->len[i];
  }
  

 /*---------------------------------
  * Compress ary2 to ary1.
  *---------------------------------*/


  if( typ == cdByte )
  {
     o_b  = (unsigned char *) ary1;
     in_b = (unsigned char *) ary2;

     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_b[i] = in_b[j];
     }
  }

  else if( typ == cdShort )
  {
     o_s  = (short *) ary1;
     in_s = (short *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_s[i] = in_s[j];
     }
  }

  else if( typ == cdInt )
  {
     o_i  = (int *) ary1;
     in_i = (int *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_i[i] = in_i[j];
     }
  }

  else if( typ == cdLong )
  {
     o_l  = (long *) ary1;
     in_l = (long *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_l[i] = in_l[j];
     }
  }

  else if( typ == cdFloat )
  {
     o_f  = (float *) ary1;
     in_f = (float *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_f[i] = in_f[j];
     }
  }

  else if( typ == cdDouble )
  {
     o_d  = (double *) ary1;
     in_d = (double *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_d[i] = in_d[j];
     }
  }

  else if( typ == cdLongDouble )
  {
     o_d  = (double *) ary1;
     in_d = (double *) ary2;

     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_d[i] = in_d[j];
     }
  }

  else
  {
     err_x( "pql_read_cmp: bad array type" );
  }

  free( idx );
  return;
}


/*********************************************************************
 * Function: release struct memory for pql 'read' instruction.
 *********************************************************************/


void pql_read_dmem( var_pql_read *rd )   /* pql read */

{
  int i;


 /*---------------------------------
  * Release memory for struct.
  *---------------------------------*/


  for( i=0; i < rd->ndims; i++ )
  {
     if( rd->cord[i] != NULL )
        free( rd->cord[i] );
  }

  if( rd->data != NULL )
     free( rd->data );

  free( rd->len );
  free( rd->index );
  free( rd->count );
  free( rd->rdidx );
  free( rd->memidx );
  free( rd->rdcnt );
  free( rd->typ );
  free( rd->dims );
  free( rd->cord );
  free( rd->plst );
  free( rd->flst );
  free( rd->ilst );
  free( rd->fpt );

  free( rd );
  return;
}


/*********************************************************************
 * Function:  expand sub-set from file into output array.
 *********************************************************************/


void pql_read_exp( var_pql_read *rd,    /* pql read */
                   void         *ary1,  /* output array */
                   void         *ary2,  /* input read array */
                   cdType       typ )   /* array type */
{
  unsigned char *in_b, *o_b;
  short         *in_s, *o_s;
  int           i, j, len, *in_i, *o_i;
  long          *in_l, *o_l, *idx;
  float         *in_f, *o_f;
  double        *in_d, *o_d;


 /*---------------------------------
  * Generate index array to expand ary2.
  *---------------------------------*/


  idx = pql_read_idx( rd );
           err_r( );

  for( i=0, len=1; i < rd->ndims; i++ )
     len *= rd->rdcnt[i];


 /*---------------------------------
  * Expand ary2 into ary1.
  *---------------------------------*/


  if( typ == cdByte )
  {
     o_b  = (unsigned char *) ary1;
     in_b = (unsigned char *) ary2;

     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_b[j] = in_b[i];
     }
  }

  else if( typ == cdShort )
  {
     o_s  = (short *) ary1;
     in_s = (short *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_s[j] = in_s[i];
     }
  }

  else if( typ == cdInt )
  {
     o_i  = (int *) ary1;
     in_i = (int *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_i[j] = in_i[i];
     }
  }

  else if( typ == cdLong )
  {
     o_l  = (long *) ary1;
     in_l = (long *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_l[j] = in_l[i];
     }
  }

  else if( typ == cdFloat )
  {
     o_f  = (float *) ary1;
     in_f = (float *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_f[j] = in_f[i];
     }
  }

  else if( typ == cdDouble )
  {
     o_d  = (double *) ary1;
     in_d = (double *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_d[j] = in_d[i];
     }
  }

  else if( typ == cdLongDouble )
  {
     o_d  = (double *) ary1;
     in_d = (double *) ary2;

     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_d[j] = in_d[i];
     }
  }

  else
  {
     err_x( "pql_read_exp: bad array type" );
  }

  free( idx );
  return;
}


/*********************************************************************
 * Function: for pql 'read', do multi-file and single file read.
 *********************************************************************/


void pql_read_fil( var_pql_read *rd )   /* pql read */
{
  int        i, j, k, l, m, n, f, len, i1, i2, *fpt, *ipt;
  char       **aa, **bb;
  cdVar_new  *var;
  cdAtt_new  *att;
  cdDset_new *ds;
  void       *vv, *vva;
  CuType     dtype;


 /*---------------------------------
  * Set read sub-set region, find multi-file dimension.
  *---------------------------------*/


  var = rd->var;

  for( i=0, f=-1; i < rd->ndims; i++ )
  {
     rd->rdidx[i]  = rd->index[i];
     rd->rdcnt[i]  = rd->count[i];
     rd->memidx[i] = 0;

     if( rd->ilst[i] != NULL )
     {
        ipt = rd->ilst[i];
        rd->rdidx[i] = ipt[0] + rd->index[i];
     }

     if( rd->flst[i] != NULL )
     {
        err_t( f != -1, "pql_read_fil: two axis are multi-file" );
        f = i;
     }
  }


 /*---------------------------------
  * Check if variable already in memory.
  *---------------------------------*/


  if( var->data != NULL )
  {
     pql_read_var( rd );
        err_r( );

     return;
  }


 /*---------------------------------
  * Single file case.
  *---------------------------------*/


  else if( f == -1 )
  {
     if( rd->file == NULL )
     {
        att = nam_fnd( "psql_file", id_cdAtt, (cdHd *) var );

        if( att != NULL )
           rd->file = att->values;

        att = nam_fnd( "psql_path", id_cdAtt, (cdHd *) var );

        if( att != NULL )
           rd->path = att->values;
     }

     pql_read_var( rd );
        err_r( );

     return;
  }


 /*---------------------------------
  * Multi-file case.
  *---------------------------------*/


  else
  {


    /*---------------------------------
     * Loop down multi-file axis in file sections.
     *---------------------------------*/


     i1  = rd->index[f];
     i2  = i1 + rd->count[f];
     fpt = rd->fpt[f];
     ipt = rd->ilst[f];
     aa  = rd->flst[f];
     bb  = rd->plst[f];
     vv  = rd->data;

     typ_to_cdunif( rd->vtyp, &dtype, &m );
        err_r( );

     if( aa == NULL || fpt == NULL )
        err_x( "pql_read_fil: missing key psql attributes" );

     for( i=i1; i < i2; )
     {


       /*---------------------------------
        * Determine file-subset of variable to read.
        *---------------------------------*/


        l = fpt[i];

        rd->file = aa[l];

        if( bb != NULL )
           rd->path = bb[l];
        else
        {
           att = nam_fnd( "psql_path", id_cdAtt, (cdHd *) var );

           if( att != NULL )
              rd->path = att->values;
        }

        for( j=0, k=0; j < i; j++ )
        {
           if( fpt[j] == l )
              k++;
        }

        rd->rdidx[f] = k;

        if( ipt != NULL )
           rd->rdidx[f] = k + ipt[l];

        for( j=i, n=0; j < i2; j++ )
        {
           if( fpt[j] == l )
              n++;
        }

        rd->rdcnt[f] = n;


       /*---------------------------------
        * Get memory for file-subset array.
        *---------------------------------*/


        for( j=0, len=m; j < rd->ndims; j++ )
           len *= rd->rdcnt[j];

        vva = malloc( len );

        if( vva == NULL )
           err_x( "pql_read_fil: trouble getting memory" );

        rd->data = vva;


       /*---------------------------------
        * Read file-subset of variable.
        *---------------------------------*/


        pql_read_var( rd );
           err_r( );


       /*---------------------------------
        * Expand file-subset into output array.
        *---------------------------------*/


        pql_read_exp( rd, vv, vva, rd->vtyp );
           err_r( );

        free( vva );
        i             += n;
        rd->memidx[f] += n;
        rd->data       = vv;
     }

     return;
  }
}


/*********************************************************************
 * Function: for pql 'read' generate file-to-memory index array.

 * Note: multi-file read means each file is a sub-set.  Each read
 *       brings in sub-set compressed.  So need an index array to
 *       copy the file-array to the full size memory-array.
 *********************************************************************/


long *pql_read_idx( var_pql_read *rd )   /* pql read */

{
  int       i, j, k;
  long      l, m, n, l_count, l_rdcnt, *index;


 /*---------------------------------
  * Get memory for output index array.
  *---------------------------------*/


  for( i=0, n=1; i < rd->ndims; i++ )
     n *= rd->rdcnt[i];

  l     = n * sizeof( long );
  index = (long *) malloc( l );

  if( index == NULL )
     err_xv( "pql_read_idx: trouble getting memory" );


 /*---------------------------------
  * Process last dimension first.  (in C (i,j,k) k varies first)
  *---------------------------------*/


  i = rd->ndims - 1;
  m = rd->memidx[i];

  for( j=0; j < rd->rdcnt[i]; j++ )
     index[j] = m++;

  l_count = rd->count[i];
  l_rdcnt = rd->rdcnt[i];


 /*---------------------------------
  * Process last-1 to 0 dimensions.
  *---------------------------------*/


  for( i=rd->ndims-2; i > -1; i-- )
  {
     l = rd->memidx[i] * l_count;

     if( l != 0 )   /* offset index's from dim. i-1 */
     {
        for( k=0; k < l_rdcnt; k++ )
           index[k] += l;
     }

     m = 0;
     n = l_rdcnt;

     for( j=1; j < rd->rdcnt[i]; j++ )   /* index's this dim. */
     {
        for( k=0; k < l_rdcnt; k++ )
           index[n++] = index[m++] + l_count;
     }

     l_count *= rd->count[i];
     l_rdcnt *= rd->rdcnt[i];
  }

  return index;
}


/*********************************************************************
 * Function: get struct memory for pql 'read' instruction.
 *********************************************************************/


var_pql_read *pql_read_mem( cdVar_new *var )  /* pql variable */

{
  int          i, l, n;
  cdDim_new    *dim;
  cdTmp        *tmp;
  cdAtt_new    *att;
  var_pql_read *rd;


 /*---------------------------------
  * Get memory 'rd' struct.
  *---------------------------------*/


  rd = calloc( 1, sizeof( var_pql_read ) );

  if( rd == NULL )
     err_xv( "pql_read_mem: trouble getting memory" );

  rd->var   = var;
  rd->ndims = var->ndims;
  rd->vtyp  = var->datatype;

  att = scn_lnk_list( "psql_ndims", id_cdAtt, (cdHd *) var );
  if( att != NULL )
     rd->ndims = *(int *) att->values;

  att = scn_lnk_list( "psql_type", id_cdAtt, (cdHd *) var );
  if( att != NULL )
     rd->vtyp = typ_from_ascii( (char *) att->values );

  att = scn_lnk_list( "psql_dimorder", id_cdAtt, (cdHd *) var );
  if( att != NULL )
     rd->dorder = att->values;


 /*---------------------------------
  * Get memory for per-dimension arrays.
  *---------------------------------*/


  l          = rd->ndims * sizeof( long );
  rd->len    = (long *) malloc( l );
  rd->index  = (long *) malloc( l );
  rd->count  = (long *) malloc( l );
  rd->rdidx  = (long *) malloc( l );
  rd->memidx = (long *) malloc( l );
  rd->rdcnt  = (long *) malloc( l );

  if( rd->len == NULL || rd->index == NULL || rd->count == NULL ||
      rd->rdidx == NULL || rd->memidx == NULL || rd->rdcnt == NULL )
     err_xv( "pql_read_mem: trouble getting memory" );

  l       = rd->ndims * sizeof( cdType );
  rd->typ = (cdType *) malloc( l );

  if( rd->typ == NULL )
     err_xv( "pql_read_mem: trouble getting memory" );

  l         = rd->ndims * sizeof( tmp );
  rd->dims  = (void *) malloc( l );
  rd->cord  = (void *) calloc( 1, l );
  rd->plst  = (void *) calloc( 1, l );
  rd->flst  = (void *) calloc( 1, l );
  rd->ilst  = (void *) calloc( 1, l );
  rd->fpt   = (void *) calloc( 1, l );

  if( rd->dims == NULL || rd->cord == NULL || rd->flst == NULL ||
      rd->plst == NULL || rd->ilst == NULL || rd->fpt == NULL )
     err_xv( "pql_read_mem: trouble getting memory" );


 /*---------------------------------
  * Loop over input-file dimensions of variable.
  * Note: 'var->dim' is outdims-indims if ':wrtshape' for var
  *---------------------------------*/


  tmp = var->dim;

  if( rd->dorder != NULL )
  {
     for( i=0; i < var->ndims; i++ )
        tmp = tmp->next;
  }

  for( i=0; i < rd->ndims; tmp=tmp->next, i++ )
  {
     dim = (cdDim_new *) tmp->want;

     rd->dims[i] = dim;
     rd->len[i]  = dim->length;
     rd->typ[i]  = dim->datatype;

     att = scn_lnk_list( "psql_filelist", id_cdAtt, (cdHd *) dim );
     if( att != NULL )
     {
        if( att->datatype == cdCharTime )
           rd->flst[i] = att->values;
        else
           rd->file = att->values;
     }

     att = scn_lnk_list( "psql_pathlist", id_cdAtt, (cdHd *) dim );
     if( att != NULL )
     {
        if( att->datatype == cdCharTime )
           rd->plst[i] = att->values;
        else
           rd->path = att->values;
     }

     att = scn_lnk_list( "psql_poslist", id_cdAtt, (cdHd *) dim );
     if( att != NULL )
        rd->ilst[i] = att->values;

     att = scn_lnk_list( "psql_filepoint", id_cdAtt, (cdHd *) dim );
     if( att != NULL )
        rd->fpt[i] = att->values;

     rd->count[i] = dim->length;
     rd->index[i] = 0;
  }

  return rd;
}


/*********************************************************************
 * Function to process pql 'readmeta' instruction.

 * readmeta /pcmdi/staff/boyle/meta1 ;
 *********************************************************************/


int pql_readmeta( cdms_card     *line,  /* struct metafile line */
                  cdms_pql_list *pql,   /* pql list */
                  int           idx )   /* input symbol index */
{
  int        i, j, k, n;
  char       *aa;
  cdDb_new   *db;
  cdAtt_new  *att;


 /*---------------------------------
  * Read and translate metafile to in-memory tree structure.
  *---------------------------------*/


  if( line->cls_sym[idx] != 'a' )
     err_xi( "pql_readmeta: bad readmeta instruction" );

  j = line->idx_sym[idx];

  n = rd_meta_file( &line->asc_line[j] );

  k = 2 * n - pql->L_list;

  if( k > 0 )
  {
     pql_one_mem( pql, k );
        err_ri( );
  }

  /*.....return ++idx;.....*/
  return line->num_sym - 1;
}


/*********************************************************************
 * Function to scan dimension sections of pql 'read' instruction.
 *********************************************************************/


/*--------------------------------------------------------------------
 * index variable ta dimension time 17 24 dimension lev 2 14 ;
 * value variable ta dimension time "1984" "1987", to filename;

 * i1   = card index of 'variable' 
 * i2   = card index of 'name length'
 * i3   = card index after ','
 *-------------------------------------------------------------------*/

int pql_read_sec( cdms_card     *line,   /* struct metafile line */
                  cdms_pql_list *pql,    /* pql list */
                  var_pql_read  **outrd, /* pql subset read */
                  int           idx )    /* input read section index */
{
  int          i, j, k, l, n, f, i1, i2, i3, i4, flg;
  long         l1, l2, l3, len;
  cdHd         *hd;
  cdVar_new    *var, *vara;
  cdDim_new    *dim;
  var_pql_read *rd;


 /*---------------------------------
  * Check for 'index' dimension input keyword (default is coordinates).
  *---------------------------------*/


  flg = 1;
  i2  = idx;

  if( line->cls_sym[idx] == 'a' )
  {
     n = line->idx_sym[idx];

     if( !strcmp( "index", &line->asc_line[n] ) )
     {
        flg = 0;
        i2++;
     }
  }


 /*---------------------------------
  * Get variable to read in.
  *---------------------------------*/


  var = NULL;
  i3  = i2;

  if( line->cls_sym[i3] == 'k' &&
      line->len_sym[i3] == i_variable &&
      line->cls_sym[i3+1] == 'a' )

  {
     k  = line->idx_sym[i3+1];
     i3 +=2;

     for( j=0; j < pql->length; j++ )
     {
        hd = pql->list[j];

        if( hd->id == id_cdVar )
        {
           vara = (cdVar_new *) hd;

          if( vara->name != NULL &&
              !strcmp( vara->name, &line->asc_line[k] ) )
           {
              var = vara;
              break;
           }
        }
     }
  }

  err_ti( var == NULL, "pql_read_sec: bad tree node selection" );


 /*---------------------------------
  * Load variables' dimension arrays.
  *---------------------------------*/


  rd = pql_read_mem( var );
          err_ri( );

  *outrd = rd;

  for( i=0; i < rd->ndims; i++ )
  {
     dim = rd->dims[i];

     rd->cord[i] = get_coord( dim, dim->datatype, &len );
                      err_ri( );
  }


 /*---------------------------------
  * Process 'dimension time t1 t2,' sections.
  *---------------------------------*/


  for( i=i3; i < line->num_sym; i++ )
  {
     if( line->cls_sym[i] == 'k' &&
         line->len_sym[i] == i_dimension &&
         line->cls_sym[i+1] == 'a' )
     {
        k = line->idx_sym[i+1];
        f = -1;

        for( j=0; j < rd->ndims; j++ )
        {
           dim = rd->dims[j];

          if( dim->name != NULL &&
              !strcmp( dim->name, &line->asc_line[k] ) )
             f = j;
        }

        err_ti( f == -1, "pql_read_sec: bad dimension name" );


       /*---------------------------------
        * Index position dimension input.
        *---------------------------------*/


        if( flg == 0 )
        {
           if( line->cls_sym[i+2] != 'i' ||
               line->cls_sym[i+3] != 'i' )
              err_xi( "pql_read_sec: bad dimension index" );

           k  = line->idx_sym[i+2];
           l1 = atol( &line->asc_line[k] );

           k  = line->idx_sym[i+3];
           l2 = atol( &line->asc_line[k] );
        }


       /*---------------------------------
        * Coordinate value dimension input.
        *---------------------------------*/


        if( flg == 1 )
        {
        }


        l3 = rd->count[f] - 1;

        if( l1 < 0 )
           l1 = 0;
        if( l1 > l3 )
           l1 = l3;

        if( l2 < l1 )
           l2 = l1;
        if( l2 > l3 )
           l2 = l3;

        rd->index[f] = l1;
        rd->count[f] = l2 - l1 + 1;
        i += 3;
     }

     else if( line->cls_sym[i] == ',' )
        ;

     else
        break;
  }

  return i;
}


/*********************************************************************
 * Function:  read in sub-set of variable from file.
 *********************************************************************/


void pql_read_var( var_pql_read *rd )   /* pql read */
{
  int       i, j, m, n, k, f_id, f_nvars, id_var, ndims;
  long      start[CU_MAX_VAR_DIMS], count[CU_MAX_VAR_DIMS];
  char      *aa, *bb;
  cdVar_new *var;
  cdAtt_new *att;
  CuType    dtype;
  cdType    typ;


 /*---------------------------------
  * Determine if variable values in memory.
  *---------------------------------*/


  var = rd->var;

  if( var->data != NULL )
  {
     pql_read_cmp( rd, rd->data, var->data, rd->vtyp );
        err_r( );

     return;
  }


 /*---------------------------------
  * Determine name of variable in disc file.
  *---------------------------------*/


  bb = var->name;

  att = scn_lnk_list( "psql_name", id_cdAtt, (cdHd *) var );

  if( att != NULL )
     bb = (char *) att->values;


 /*---------------------------------
  * Create '/directory/filename' string.
  *---------------------------------*/


  aa = calloc( 1, CD_MAX_PATH );
  err_t( aa == NULL, "pql_read_var: trouble getting memory" );

  if( rd->path != NULL )
  {
     strcpy( aa, rd->path );

     i = strlen( aa );

     if( aa[i-1] != '/' )
        strcat( aa, "/" );
  }

  else
     aa[0] = '\0';

  strcat( aa, rd->file );


 /*---------------------------------
  * Attach file.
  *---------------------------------*/


  f_id = cuopenread( aa, NULL );
     err_t( f_id == -1, "pql_read_var: cuopenread trouble" );

  f_nvars = 0;
  j = cuinquire( f_id, NULL, &f_nvars, NULL, NULL );
     err_t( j == -1, "pql_read_var: cuinquire trouble" );


 /*---------------------------------
  * Search file for variable.
  *---------------------------------*/


  id_var = -1;

  for( i=0; i < f_nvars; i++ )
  {
     ndims = 0;
     j = cuvarinq( f_id, i, aa, &dtype, &ndims, NULL, NULL );
        err_t( j == -1, "pql_read_var: cuvarinq trouble" );

    /*---------------------------------
     * Reset name with '_' in non-printing letters.
     *---------------------------------*/
     for( j=0; j < strlen(aa); j++ )
     {
        if( !isgraph( aa[j] ) )
           aa[j] = '_';
     }

     if( bb != NULL && !strcmp( bb, aa ) )
     {
        id_var = i;
        break;
     }
  }


  typ_from_cdunif( dtype, &typ, &n );
     err_r( );

  if( id_var == -1 || rd->ndims != ndims || rd->vtyp != typ )
  {
     j = cuclose( f_id );
     err_x( "pql_read_var: bad variable in file" );
  }


 /*---------------------------------
  * Set subset to read.
  *---------------------------------*/


  for( i=0; i < rd->ndims; i++ )
  {
     start[i] = rd->rdidx[i];
     count[i] = rd->rdcnt[i];
  }


 /*---------------------------------
  * Read in subset of variable from file.
  *---------------------------------*/
  

  j = cuvarget( f_id, id_var, start, count, rd->data );
         err_t( j == -1, "pql_read_var: cuvarget trouble");

  j = cuclose( f_id );
  free( aa );
  return;
}


/*********************************************************************
 * Function to initalize output NetCDF file and write coordinates.
 *********************************************************************/


void pql_read_wrt( var_pql_read *rd,    /* pql read */
                   char         *nam )  /* filename */
{
  int       i, j, k, m, n, *dim_id, *var_id, ncid;
  long      *idx_bgn, *idx_cnt;
  nc_type   typ;
  cdDim_new *dim;
  cdAtt_new *att;
  cdVar_new *var;
  void      *vv;


/*---------------------------------
 * Create the output file.
 *---------------------------------*/


  var = rd->var;

  ncid = nccreate( nam, NC_CLOBBER );
  if( ncid < 0 )
     err_x( "pql_read_wrt: netCDF nccreate trouble" );

/*............
  k = ncredef( ncid );
  if( k < 0 )
     err_x( "pql_read_wrt: netCDF ncredef trouble" );
..............*/

  n       = rd->ndims;
  dim_id  = (int *) malloc( n * sizeof( int ) );
  var_id  = (int *) malloc( (n + 1) * sizeof( int ) );
  idx_bgn = (long *) malloc( n * sizeof( long ) );
  idx_cnt = (long *) malloc( n * sizeof( long ) );

  if( dim_id == NULL || var_id == NULL || idx_bgn == NULL ||
      idx_cnt == NULL )
     err_x( "pql_read_wrt: trouble getting memory" );


/*---------------------------------
 * Define dimensions in output file.
 *---------------------------------*/


  for( i=0; i < rd->ndims; i++ )
  {
     dim = rd->dims[i];

     dim_id[i] = ncdimdef( ncid, dim->name, rd->count[i] );
     if( dim_id[i] < 0 )
        err_x( "pql_read_wrt: netCDF ncdimdef trouble" );

     typ_to_netcdf( dim->datatype, &typ, &m );
        err_r( );

     var_id[i] = ncvardef( ncid, dim->name, typ, 1, &dim_id[i] );
     if( var_id[i] < 0 )
        err_x( "pql_read_wrt: netCDF ncvardef trouble" );


     if( dim->atts != NULL )
     {
        for( att=dim->atts; att; att=att->next )
        {
           if( att->datatype != cdCharTime )
           {
              typ_to_netcdf( att->datatype, &typ, &m );
                 err_r( );

              m = att->length;
              if( typ == NC_CHAR )
                 m = strlen( att->values );

              k = ncattput( ncid, var_id[i], att->name, typ, m, 
                            att->values );
              if( k < 0 )
                 err_x( "pql_read_wrt: netCDF ncattput trouble" );
           }
        }
     }
  }


/*---------------------------------
 * Define quantity in output file.
 *---------------------------------*/


  typ_to_netcdf( var->datatype, &typ, &m );
     err_r( );

  var_id[n] = ncvardef( ncid, var->name, typ, n, dim_id );
  if( var_id[n] < 0 )
     err_x( "pql_read_wrt: netCDF ncvardef trouble" );

  if( var->atts != NULL )
  {
     for( att=var->atts; att; att=att->next )
     {
        typ_to_netcdf( att->datatype, &typ, &m );
           err_r( );

        m = att->length;
        if( typ == NC_CHAR )
           m = strlen( att->values );

        k = ncattput( ncid, var_id[n], att->name, typ,
                      m, att->values );
        if( k < 0 )
           err_x( "pql_read_wrt: netCDF ncattput trouble" );
     }
  }


/*---------------------------------
 * Terminate Netcdf define mode.
 *---------------------------------*/


  k = ncendef( ncid );
  if( k < 0 )
     err_x( "pql_read_wrt: netCDF ncendef trouble" );


/*---------------------------------
 * Store coordinate arrays in output file.
 *---------------------------------*/


  for( i=0; i < rd->ndims; i++ )
  {
     dim = rd->dims[i];

     idx_bgn[i] = 0;
     idx_cnt[i] = rd->count[i];

     vv = ary_off( rd->typ[i], rd->index[i], rd->cord[i] );
             err_r( );

     k = ncvarput( ncid, var_id[i], &idx_bgn[i], &idx_cnt[i], vv );
     if( k < 0 )
        err_x( "pql_read_wrt: netCDF ncvarput trouble" );
  }


/*---------------------------------
 * Store variable in output file.
 *---------------------------------*/


  k = ncvarput( ncid, var_id[n], idx_bgn, idx_cnt, rd->data );
  if( k < 0 )
     err_x( "pql_read_wrt: netCDF ncvarput trouble" );

  k = ncclose( ncid );
  if( k < 0 )
     err_x( "pql_read_wrt: netCDF ncclose trouble" );


  free( dim_id );
  free( var_id );
  free( idx_bgn );
  free( idx_cnt );
  return;
}


/*********************************************************************
 * Function to process pql 'release' instruction.

 * release database dbco3 dataset dsetco33 ;
 * release dataset from bb, ;
 *********************************************************************/


int pql_release( cdms_card     *line,  /* struct metafile line */
                 cdms_pql_list *pql,   /* pql list */
                 int           idx )   /* input symbol index */
{
  int  i, j, k;
  cdHd *hd;


 /*---------------------------------
  * Release 1 tree node and everything under it.
  *---------------------------------*/


  if( line->cls_sym[idx] == 'k' && line->cls_sym[idx+1] == 'a' )
  {
     hd = pql_fld3( line, idx, &i );
             err_ri( );

     delete_struct( hd );
        err_ri( );
  }


 /*---------------------------------
  * Release list of tree nodes (and everything under them).
  *---------------------------------*/


  else   /* list-of-nodes case */
  {
     k = pql_fld2( line, idx );
            err_ri( );

     i = pql_gen_list( line, pql, idx, k );
            err_ri( );

     for( j=0; j < pql->length; j++ )
     {
        hd = pql->list[j];

        delete_struct( hd );
           err_ri( );
     }
  }


  return i;
}


/*********************************************************************
 * Function to process pql 'select' instruction.

 * Note: 'select' may be part of a 'union' instruction.
 *********************************************************************/


/*--------------------------------------------------------------------
 * select variable dimension below database db4 dataset ds3,
          where variable attribute Karl value ECMWF2,
                variable dimension length > 100,
          show dimension name, dimension length;

 * i2   = card index of 'variable dimension'
 * i3   = card index of 'below'
 * i3+1 = card index of 'database db4 dataset ds3'
 * i4   = card index of 'where variable attribute Karl value ECMWF2'
 * i5   = card index of 'show'
 * i6   = card index of ';'
 *-------------------------------------------------------------------*/

int pql_select( cdms_card     *line,  /* struct metafile line */
                cdms_pql_list *pql,   /* pql list */
                int           idx )   /* input symbol index */
{
  int  i, j, k, n, i2, i3, i4, i5, i6;
  cdHd *hd;


 /*---------------------------------
  * Find i3, index after 'variable dimension attribute etc.' list.
  *---------------------------------*/


  i2 = idx;
  i3 = pql_fld2( line, i2 );
          err_ri( );


 /*---------------------------------
  * Find i4, index after 'below database db4 dataset ds3,'
  * Generate list of tree nodes.
  *---------------------------------*/


  i4 = pql_gen_list( line, pql, i2, i3 );
          err_ri( );


 /*---------------------------------
  * Process 'where' or 'with' clause.
  *---------------------------------*/


  if( line->cls_sym[i4] == 'k' && line->len_sym[i4] == i_where )
  {
     i5 = pql_where( line, pql, i4+1 );
             err_ri( );
  }

  else if( line->cls_sym[i4] == 'k' && line->len_sym[i4] == i_with )
  {
     i5 = pql_with( line, pql, i4+1 );
             err_ri( );
  }

  else
     i5 = i4;


 /*---------------------------------
  * Process 'show' clause.
  *---------------------------------*/


  if( line->cls_sym[i5] == 'k' && line->len_sym[i5] == i_show )
  {
     i6 = pql_show_sec( line, pql, i5+1 );
             err_ri( );
  }
  else
     i6 = i5;


 /*---------------------------------
  * Process 'variable dimension' sublist from start of line.
  *---------------------------------*/

  if( (i3 - i2) > 1 && pql->length > 0 )
  {
     n = pql->length;

     for( j=0; j < n; j++ )
     {
        pql_sublist( line, pql, j, i2, i3 );
           err_ri( );
     }

     k = line->len_sym[i3-1];
     pql_compress( pql, 0, pqlnode[k] );
  }

  return i6;
}


/*********************************************************************
 * Function to process pql 'show' instruction.
 *********************************************************************/


/*--------------------------------------------------------------------
 * show from pp, 10 dataset variable attribute name value,
        dataset variable name id,
        dataset dataset name;
 * show at database db4, dataset name;

 * i2   = card index of 'from pp,'
 * i3   = card index of '10 dataset variable attribute name value,'
 * i4   = card index of 'dataset variable attribute'
 * i5   = card index of ';'
 *-------------------------------------------------------------------*/

int pql_show( cdms_card     *line,  /* struct metafile line */
              cdms_pql_list *pql,   /* pql list */
              int           idx )   /* input symbol index */
{
  int i, f, i2, i3, i4, i5;


 /*---------------------------------
  * Find index of 'dataset variable attribute'
  *---------------------------------*/

  i2 = idx;

  for( i=i2, i4=0, f=0; i < line->num_sym; i++ )
  {
     if( f == 1 && line->cls_sym[i] == 'k' &&
         line->len_sym[i] < L_pql_node_type )
     {
        i4 = i;
        break;
     }

     else if( line->cls_sym[i] == ',' )
        f = 1;
  }

  if( i4 == 0 )
     err_xi( "pql_show: bad show instruction" );


 /*---------------------------------
  * Get list of nodes 'from pp,'
  *---------------------------------*/


  i3 = pql_gen_list( line, pql, i4, i2 );
          err_ri( );


 /*---------------------------------
  * Process 'show' clause.
  *---------------------------------*/


  i5 = pql_show_sec( line, pql, i3 );
          err_ri( );

  return i5;
}


/*********************************************************************
 * Function to edit the contents of an array.
 *********************************************************************/


void pql_show_ary( cdType typ,     /* type of array */
                   long   len,     /* length of array */
                   void   *ary )   /* array */
{
  unsigned char *in_b;
  char          *in_a, **in_alst;
  short         *in_s;
  int           i, j, k, l, n, num, *in_i;
  long          *in_l;
  float         *in_f;
  double        *in_d;


 /*---------------------------------
  * set pointer input array
  *---------------------------------*/


  if( len < 1 )  return;

  if( typ == cdByte )
     in_b = (unsigned char *) ary;

  else if( typ == cdChar )
     in_a = (char *) ary;

  else if( typ == cdShort )
     in_s = (short *) ary;

  else if( typ == cdInt )
     in_i = (int *) ary;

  else if( typ == cdLong )
     in_l = (long *) ary;

  else if( typ == cdFloat )
     in_f = (float *) ary;

  else if( typ == cdDouble )
     in_d = (double *) ary;

  else if( typ == cdLongDouble )
     in_d = (double *) ary;

  else if( typ == cdCharTime )
     in_alst = ary;

  else
  {
     err_x( "pql_show_ary: bad array type" );
  }


 /*---------------------------------
  * Array edit
  *---------------------------------*/


  if( typ == cdChar )
  {
     sprintf( PSQL_MSG, " %s", in_a );
     wrt_msg( );

     return;
  }

  n   = 0;
  num = 4;

  if( FPT_PY == 1 )
     num = 1;


  while( n < len )
  {
     l = len - n;

     if( l > num )
        l = num;


     for( i=n; i<n+l; i++ )
     {
        if( typ == cdByte )
        {
           sprintf( PSQL_MSG, " %c", in_b[i] );
           wrt_msg( );
        }
   
        else if( typ == cdShort )
        {
           sprintf( PSQL_MSG, " %d", in_s[i] );
           wrt_msg( );
        }
   
        else if( typ == cdInt )
        {
           sprintf( PSQL_MSG, " %d", in_i[i] );
           wrt_msg( );
        }
   
        else if( typ == cdLong )
        {
           sprintf( PSQL_MSG, " %d", in_l[i] );
           wrt_msg( );
        }
   
        else if( typ == cdFloat )
        {
           sprintf( PSQL_MSG, " %e", in_f[i] );
           wrt_msg( );
        }
   
        else if( typ == cdDouble )
        {
           sprintf( PSQL_MSG, " %e", in_d[i] );
           wrt_msg( );
        }
   
        else if( typ == cdLongDouble )
        {
           sprintf( PSQL_MSG, " %e", in_d[i] );
           wrt_msg( );
        }
   
        else if( typ == cdCharTime )
        {
           sprintf( PSQL_MSG, " %s", in_alst[i] );
           wrt_msg( );
        }
     }
   
     n += l;

     if( n < len )
     {
        sprintf( PSQL_MSG, " \n" );
        wrt_msg( );
     }
  }
  return;
}


/*********************************************************************
 * Function: append to pql list all nodes 1 level below 'cur'.
 *********************************************************************/


void pql_show_ed( int           cnt,    /* count of show sections */
                  cdms_card     *line,  /* struct metafile line */
                  int           idxl1,  /* index to symbol */
                  int           idxl2,  /* index 'with' symbol */
                  int           idxl3,  /* index to symbol */
                  cdms_pql_list *pql,   /* pql list */
                  int           idx1,   /* index to list */
                  int           idx2 )  /* index to list */

{
  int       i, j, flg, ix, k, l, m, n;
  long      ida;
  cdHd      *hd;
  char      *aa;

  cnt++;


 /*---------------------------------
  * Loop over nodes.
  *---------------------------------*/


  for( i=idx1; i < idx2; i++ )
  {
    /*---------------------------------
     * Load tree node 'name,length,value'.
     *---------------------------------*/


     pql_whr_node( pql, i );


    /*---------------------------------
     * Check if 'with' part at end of 'show' section.
     * If true, determine if edit node or not.
     *---------------------------------*/


     flg = 5;
     ix  = idxl3;

     if( idxl2 )
     {
        flg = pql_fld6( line, pql, idxl2+1 );
        ix  = idxl2;
     }

     if( flg )
     {
       /*---------------------------------
        * Print the ascii show clause symbols.
        *---------------------------------*/


        if( pql->pflg )
        {
           for( j=0; j < cnt; j++)
           {
              sprintf( PSQL_MSG, "   " );
              wrt_msg( );
           }

           for( j=idxl1; j < ix; j++)
           {
              m = line->cls_sym[j];

              if( m == ',' || m == ';' ||
                  ( m == 'k' && line->len_sym[j] == i_for ) )
                 break;

              k = line->idx_sym[j];

              sprintf( PSQL_MSG, " %s", &line->asc_line[k] );
              wrt_msg( );
           }

           sprintf( PSQL_MSG, " = " );
           wrt_msg( );
        }


       /*---------------------------------
        * Loop over show clause symbols.
        *---------------------------------*/


        for( j=idxl1; j < ix; j++)
        {
           k = line->cls_sym[j];
           l = line->len_sym[j];

           if( k = 'k' && l == i_id )
           {
              hd = pql->list[i];
              pt_to_long( hd, ida );

              sprintf( PSQL_MSG, " %d", ida );
              wrt_msg( );
           }

           else if( k = 'k' && l == i_name )
           {
              if( pql->nod_nam != NULL )
              {
                 sprintf( PSQL_MSG, " %s", pql->nod_nam );
                 wrt_msg( );
              }
           }

           else if( k = 'k' && l == i_length )
           {
              sprintf( PSQL_MSG, " %d", pql->nod_len );
              wrt_msg( );
           }

           else if( k = 'k' && l == i_type )
           {
              aa = typ_as_ascii( pql->nod_typ );
                      err_r( );
              sprintf( PSQL_MSG, " %s", aa );
              wrt_msg( );
              free( aa );
           }

           else if( k = 'k' && l == i_value )
           {
              pql_show_value( pql, i );
                 err_r( );
           }

           else if( k = 'k' && l == i_ndim )
           {
              sprintf( PSQL_MSG, " %d", pql->nod_dim );
              wrt_msg( );
           }
        }
        sprintf( PSQL_MSG, " \n" );
        wrt_msg( );
     }
  }
  return;
}


/*********************************************************************
 * Function to process all sections of pql 'show' clause.
 *********************************************************************/


/*--------------------------------------------------------------------
 * show 10 variable id name,
        variable dimension name length,
        variable attribute name value with name=file name=g*;

 * i1   = card index of 'variable' 
 * i2   = card index of 'name length'
 * i3   = 0 or card index of 'with'
 * i4   = card index after ',' or index of ';'
 *-------------------------------------------------------------------*/

int pql_show_sec( cdms_card     *line,  /* struct metafile line */
                  cdms_pql_list *pql,   /* pql list */
                  int           idx )   /* input show section index */
{
  int     i, j, k, l, m, n, f, i1, i2, i3, i4, num, cnt;
  cdHd    *hd;
  cdms_Id ida;


 /*---------------------------------
  * Check first 2 symbols for 'count' and 'notitle'

  * 'count' is integer number of nodes to print
  * 'notitle' is ascii flag to not print titles
  *---------------------------------*/


  pql->pflg = 1;
  pql->cflg = 1;
  l     = num = pql->length;

  for( i=0; i < 3; i++ )
  {
     n = line->idx_sym[idx];

     if( line->cls_sym[idx] == 'a' )
     {
        if( !strcmp( "notitle", &line->asc_line[n] ) )
        {
           pql->pflg = 0;
           idx++;
        }

        else if( !strcmp( "noconvert", &line->asc_line[n] ) )
        {
           pql->cflg = 0;
           idx++;
        }
     }

     else if( line->cls_sym[idx] == 'i' )
     {
        num = atol( &line->asc_line[n] );

        if( num > pql->length )
           num = pql->length;

        idx++;
     }
  }


 /*---------------------------------
  * If there are no tree nodes, just find end of 'show' sections.
  *---------------------------------*/


  if( num == 0 )
  {
     i1 = idx;
     f  = 1;

     while( f )
     {
        i2  = pql_fld2( line, i1 );
                 err_ri( );

        i4  = pql_fld5( line, i2, &i3 );
                 err_ri( );
        i1  = i4;

        if( line->cls_sym[i4] == ';' )
           return i4;
     }
  }


 /*---------------------------------
  * Loop over the tree nodes.
  *---------------------------------*/


  for( i=0; i < num; i++ )
  {
     hd  = pql->list[i];

     i1  = idx;
     f   = 1;
     cnt = 0;


    /*---------------------------------
     * Loop over the 'show' sections.
     *---------------------------------*/


     while( f )
     {


       /*---------------------------------
        * Find i2 and i4 card symbol index's.

        * i1 is start of tree-node-chain section 'variable'.
        * i2 is start of show-edit section 'id name'.
        * i4 is ';', or is i1 of next show section.
        *---------------------------------*/


        i2  = pql_fld2( line, i1 );
                 err_ri( );

        i4  = pql_fld5( line, i2, &i3 );
                 err_ri( );

        n   = i2 - i1;
        j   = line->len_sym[i1];
        ida = pqlnode[j];


       /*---------------------------------
        * Edit out this show section.
        *---------------------------------*/


        if( hd->id == ida )
        {
          /*---------------------------------
           * Print the ascii show clause symbols.
           *---------------------------------*/


           if( pql->pflg == 1 && (FPT_TTY == 1 || FPT_PY == 1) )
           {
              sprintf( PSQL_MSG, " \n" );
              wrt_msg( );

              for( j=0; j < cnt; j++)
              {
                 sprintf( PSQL_MSG, "   " );
                 wrt_msg( );
              }

              sprintf( PSQL_MSG, "***" );
              wrt_msg( );

              for( j=i1; j < i4; j++)
              {
                 m = line->cls_sym[j];

                 if( m == ',' || m == ';' ||
                     ( m == 'k' && line->len_sym[j] == i_for ) )
                    break;

                 k = line->idx_sym[j];

                 sprintf( PSQL_MSG, " %s", &line->asc_line[k] );
                 wrt_msg( );
              }

              sprintf( PSQL_MSG, " \n" );
              wrt_msg( );
           }


          /*---------------------------------
           * Print the node info.
           *---------------------------------*/


           if( n == 1 && (FPT_TTY == 1 || FPT_PY == 1) )
              pql_show_ed( cnt, line, i2, i3, i4, pql, i, i+1 );
   
           else
           {
              pql_sublist( line, pql, i, i1, i2 );
                 err_ri( );
   
              if( FPT_TTY == 1 || FPT_PY == 1)
                 pql_show_ed( cnt, line, i2, i3, i4, pql, l,
                              pql->length );
   
              pql->length = l;
           }
        }


       /*---------------------------------
        * Advance to next show clause.
        *---------------------------------*/


        i1 = i4;
        cnt++;

        if( line->cls_sym[i4] == ';' )
           break;
     }
  }
  
  return i4;
}


/*********************************************************************
 * Function: edit 'value' field of a 'show' section.
 *********************************************************************/


void pql_show_value( cdms_pql_list *pql,  /* pql list */
                     int           idx )  /* index to list */

{
  int       i, flg, n, ct;
  cdHd      *hd;
  cdDim_new *dim;
  void      *vv;
  double    *dd;
  char      *aa, **aalst;
  cdAtt_new *att;


  hd = pql->list[idx];
  flg = 0;

  if( hd->id == id_cdDim )
  {
     dim = (cdDim_new *) hd;

     if( dim->datatype == cdDouble && typ_time_dim( dim->name ) )
        flg = pql->cflg;
  }


 /*---------------------------------
  * Get calendar to use.
  *---------------------------------*/


  if( flg != 0 )
  {
     att = nam_fnd( "calendar", id_cdAtt, (cdHd *) dim );

     if( att != NULL )
        ct = typ_of_calendar( (char *) att->values );
     else
        ct = typ_of_calendar( NULL );
  }


 /*---------------------------------
  * Check if 'value' field in memory.
  *---------------------------------*/


  if( pql->nod_val != NULL )
  {

     if( flg == 0 )
     {
        pql_show_ary( pql->nod_vtyp, pql->nod_len, pql->nod_val );
           err_r( );
     }

     else
     {
       /*---------------------------------
        * Convert time coordinates to ascii then edit.
        *---------------------------------*/

        att = scn_lnk_list( "units", id_cdAtt, hd );
           err_t( att==NULL, "pql_show_value: no units" );

        dd = (double *) pql->nod_val;

        aalst = malloc( dim->length * sizeof(aa) );

        for( n=0; n < dim->length; n++ )
        {
           aalst[n] = (char *) malloc( CD_MAX_CHARTIME );

           cdRel2Char( ct, (char *) att->values, dd[n], aalst[n] );
        }

        pql_show_ary( cdCharTime, dim->length, aalst );
           err_r( );

        for( n=0; n < dim->length; n++ )
           free( aalst[n] );

        free( aalst );
     }

     return;
  }


 /*---------------------------------
  * If here 'value' field not in memory.
  *---------------------------------*/


  if( hd->id != id_cdDim )
     return;

  vv = rd_dim_array( dim, -1, -1 );
          err_r( );

  if( flg == 0 )
  {
     pql_show_ary( dim->datatype, dim->length, vv );
        err_r( );
  }

  else
  {
    /*---------------------------------
     * Convert time coordinates to ascii then edit.
     *---------------------------------*/

     att = scn_lnk_list( "units", id_cdAtt, hd );
        err_t( att==NULL, "pql_show_value: no units" );

     dd = (double *) vv;

     aalst = malloc( dim->length * sizeof(aa) );

     for( n=0; n < dim->length; n++ )
     {
        aalst[n] = (char *) malloc( CD_MAX_CHARTIME );

        cdRel2Char( ct, (char *) att->values, dd[n], aalst[n] );
     }

     pql_show_ary( cdCharTime, dim->length, aalst );
        err_r( );

     for( n=0; n < dim->length; n++ )
        free( aalst[n] );

     free( aalst );
  }

  free( vv );
  return;
}


/*********************************************************************
 * Function to process pql 'subfgroup' instruction.
 *********************************************************************/


/*--------------------------------------------------------------------
 * subfgroup from pp, variable ta dimension time 0 12;

 * i2   = card index of 'from pp'
 * i3   = card index of 'ta'
 * i4   = card index of 'time'
 * i5   = card index of ';'

 * subfgroup into a single file for 12 times index 0.
 *-------------------------------------------------------------------*/

int pql_subfgroup( cdms_card     *line,  /* struct metafile line */
                   cdms_pql_list *pql,   /* pql list */
                   int           idx )   /* input symbol index */
{
  int        i, j, k, i2, i3, i4, i5, *ff1, *ff2;
  long       l, ipos, cnt;
  char       *aa;
  double     *dd, *dd1;
  cdDset_new *ds, *dsa;
  cdVar_new  *var, *vara;
  cdDim_new  *dim, *dima;
  cdAtt_new  *att, *atta;
  cdTmp      *tmp, *tmpa;


 /*---------------------------------
  * Find i4, index of 'variable ta dimension time 17 24'
  *---------------------------------*/


  i2 = idx;
  i3 = i4 = i5 = 0;

  for( i=i2; i < line->num_sym; i++ )
  {
     if( line->cls_sym[i] == 'k' &&
         line->len_sym[i] == i_variable &&
         line->cls_sym[i+1] == 'a' )
        i3 = ++i;

     else if( line->cls_sym[i] == 'k' &&
         line->len_sym[i] == i_dimension &&
         line->cls_sym[i+1] == 'a' &&
         line->cls_sym[i+2] == 'i' &&
         line->cls_sym[i+3] == 'i' )
        i4 = ++i;

     else if( line->cls_sym[i] == ';' )
     {
        i5 = i;
        break;
     }
  }

  if( i3 == 0 || i4 == 0 || i5 == 0 )
     err_xi( "pql_subfgroup: bad instruction line" );


 /*---------------------------------
  * Get dataset node 'from pp,'
  *---------------------------------*/


  pql_gen_list( line, pql, 0, i2 );
     err_ri( );

  if( pql->length != 1 )
     err_xi( "pql_subfgroup: bad tree node" );


 /*---------------------------------
  * Scan for variable and dimension in datafile.
  *---------------------------------*/


  k   = line->idx_sym[i3];
  ds  = (cdDset_new *) pql->list[0];
  dim = NULL;

  var = scn_lnk_list( &line->asc_line[k], id_cdVar, (cdHd *) ds );
           err_ri( );
  if( var == NULL )
     err_xi( "pql_subfgroup: bad file variable" );

  k = line->idx_sym[i4];

  for( tmp=(cdTmp *) var->dim; tmp; tmp=tmp->next )
  {
     dima = (cdDim_new *) tmp->want;

     if( !strcmp( dima->name, &line->asc_line[k] ) )
     {
        dim = dima;
        break;
     }
  }

  if( dim == NULL )
     err_xi( "pql_subfgroup: bad file dimension" );

  k   = line->idx_sym[i4+1];
  ipos = atol( &line->asc_line[k] );

  k   = line->idx_sym[i4+2];
  cnt = atol( &line->asc_line[k] );

  if( ipos < 0 || cnt < 1 || (ipos + cnt) > dim->length )
     err_xi( "pql_subfgroup: bad index, count args" );


 /*---------------------------------
  * Generate output dimension coordinates.
  *---------------------------------*/


  dd1 = (double *) malloc( cnt * sizeof(double) );
  ff1 = (int *) malloc( cnt * sizeof(int) );
  ff2 = (int *) malloc( sizeof(int) );
  if( dd1 == NULL || ff1 == NULL || ff2 == NULL )
     err_xi( "pql_subfgroup: trouble getting memory" );

  dd = get_coord( dim, cdDouble, &l );
          err_ri( );

  for( j=0, k=ipos; j < cnt; j++ )
  {
     if( k >= dim->length )
        k = 0;

     dd1[j] = dd[k++];
  }

  free( dd );

  ff2[0] = ipos;

  for( i=0; i < cnt; i++ )
     ff1[i] = 0;


 /*---------------------------------
  * Create output dataset struct.
  *---------------------------------*/


  dsa  = ds;
  vara = var;
  dima = dim;

  ds       = cre_struct( id_cdDset, (cdHd *) dsa->above );
  ds->name = cr_asc_cpy( "ds_group" );

  if( dsa->atts != NULL )  /* copy file attributes */
  {
     for( att=dsa->atts; att; att=att->next )
        cr_att_cpy( att, (cdHd *) ds );
  }

  var           = cre_struct( id_cdVar, (cdHd *) ds );
  var->name     = cr_asc_cpy( vara->name );
  var->ndims    = vara->ndims;
  var->datatype = vara->datatype;

  for( att=vara->atts, i=0; att; att=att->next, i++ )
  {
     if( i < 4 )
     {
        if( !strcmp(att->name,"psql_file") ||
            !strcmp(att->name,"psql_path") ||
            !strcmp(att->name,"psql_min") ||
            !strcmp(att->name,"psql_max") )
           ;

        else
           cr_att_cpy( att, (cdHd *) var );
     }

     else
        cr_att_cpy( att, (cdHd *) var );
  }

  dim           = cre_struct( id_cdDim, (cdHd *) ds );
  dim->name     = cr_asc_cpy( dima->name );
  dim->datatype = cdDouble;
  dim->length   = cnt;
  dim->data     = dd1;

  att = scn_lnk_list( "units", id_cdAtt, (cdHd *) dima );
  cr_att_cpy( att, (cdHd *) dim );

  att = scn_lnk_list( "psql_file", id_cdAtt, (cdHd *) dima );
  aa  = (char *) att->values;
  att           = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name     = cr_asc_cpy( "psql_filelist" );
  att->datatype = cdChar;
  att->length   = strlen( aa );
  att->values   = cr_asc_cpy( aa );

  att = scn_lnk_list( "psql_path", id_cdAtt, (cdHd *) dima );
  aa  = (char *) att->values;
  att           = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name     = cr_asc_cpy( "psql_pathlist" );
  att->datatype = cdChar;
  att->length   = strlen( aa );
  att->values   = cr_asc_cpy( aa );

  att           = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name     = cr_asc_cpy( "psql_poslist" );
  att->datatype = cdInt;
  att->length   = 1;
  att->values   = ff2;

  att           = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name     = cr_asc_cpy( "psql_filepoint" );
  att->datatype = cdInt;
  att->length   = cnt;
  att->values   = ff1;

  tmp           = cre_struct( id_cdTmp, (cdHd *) var );
  tmp->id_want  = id_cdDim;
  tmp->nam_want = cr_asc_cpy( dim->name );
  tmp->want     = dim;

  l = dim->length;

  for( tmpa=vara->dim->next; tmpa; tmpa = tmpa->next )
  {
     dim = tmpa->want;
     l   = l * dim->length;

     tmp           = cre_struct( id_cdTmp, (cdHd *) var );
     tmp->id_want  = id_cdDim;
     tmp->nam_want = cr_asc_cpy( dim->name );
     tmp->want     = dim;

    /****************debug*****************/
    /**** put other dimensions in dataset as well */
     dima           = cre_struct( id_cdDim, (cdHd *) ds );
     dima->name     = cr_asc_cpy( dim->name );
     dima->datatype = dim->datatype;
     dima->length   = dim->length;
     dima->data     = NULL;
     for( att=dim->atts; att; att=att->next )
     {
        if( att->datatype == cdChar )
        {
           atta           = cre_struct( id_cdAtt, (cdHd *) dima );
           atta->name     = cr_asc_cpy( att->name );
           atta->datatype = att->datatype;
           atta->length   = att->length;
           atta->values   = cr_asc_cpy( att->values );
        }
     }
  }

  var->length = l;


 /*---------------------------------
  * Because of alterfile wrtshape need ds_group complete (all dims).
  *---------------------------------*/


  dsa = copy_struct( (cdHd *) ds, (cdHd *) ds->above );
           err_ri( );

  delete_struct( (cdHd *) ds );
     err_ri( );

  free( dsa->name );
  dsa->name = cr_asc_cpy( "ds_group" );

  return i5;
}


/*********************************************************************
 * Function: append to pql list 'idwant' nodes 1 level below 'cur'.
 *********************************************************************/


void pql_sublist( cdms_card     *line,   /* struct metafile line */
                  cdms_pql_list *pql,    /* pql list */
                  int           idx,     /* index to list */
                  int           idxl1,   /* card idx tree node chain */
                  int           idxl2 )  /* card idx next section */
{
  int  i, j, j1, j2, j3, n;
  cdHd *hd;

  j1 = idx;
  j2 = idx + 1;
  j3 = pql->length;


 /*---------------------------------
  * Loop over 'variable dimension attribute' tree node chain.
  *---------------------------------*/


  for( i=idxl1+1; i < idxl2; i++ )
  {
    /*---------------------------------
     * Loop over sub-set of nodes in pql list.
     *---------------------------------*/


     for( j=j1; j < j2; j++ )
     {
        hd = pql->list[j];


       /*---------------------------------
        * Append to list the nodes 1 level below 'hd'.
        *---------------------------------*/


        if( hd->id != id_cdAtt )
        {
           pql_one_level_all( pql, hd );
              err_r( );
        }
     }


    /*---------------------------------
     * Compress from appended list all but i'th card symbol type nodes.
     *---------------------------------*/


     n = line->len_sym[i];

     if( j3 < pql->length )
        pql_compress( pql, j3, pqlnode[n] );

     j1 = j3;
     j2 = pql->length;
  }

  return;
}


/*********************************************************************
 * Function: append to pql-list tree nodes at/below/above 'cur'.
 *********************************************************************/


void pql_tree( cdms_pql_list *pql,    /* pql list */
               cdms_Id       idwant,  /* node identifier */
               int           tflg,    /* at/below/above flag */
               cdHd          *cur )   /* header of tree struct */
{
  long i, j, k1, k2, n;
  cdHd *hd;

  if( cur == NULL ) return;


 /*---------------------------------
  * Append to list tree nodes 'at' 'cur'.
  *---------------------------------*/


  if( tflg == i_at )
  {
     k1 = pql->length;

     pql_one_level( pql, cur );
        err_r( );

     if( k1 < pql->length )
        pql_compress( pql, k1, idwant );
  }


 /*---------------------------------
  * Append to list tree nodes 'below' 'cur'.
  *---------------------------------*/


  else if( tflg == i_below )
  {
     k1 = k2 = pql->length;
     pql_one_level( pql, cur );
        err_r( );

     while( k2 < pql->length )
     {
       /*---------------------------------
        * Check if memory at high end for 'gathered' nodes.
        *---------------------------------*/

        n = pql->length - k2;
        j = pql->L_list - pql->length;

        if( j < n )
        {
           pql_one_mem( pql, n );
              err_r( );
        }


       /*---------------------------------
        * Loop over added nodes.
        *    Compress the output 'idwant' nodes to low end.
        *    Gather the non-attribute others to high end.
        *---------------------------------*/

        for( i=k2, n=pql->L_list; i < pql->length; i++ )
        {
           hd = (cdHd *) pql->list[i];

           if(  hd->id == idwant )
              pql->list[k1++] = hd;

           else if( hd->id != id_cdAtt )
              pql->list[--n] = hd;
        }


       /*---------------------------------
        * Copy high end to low end.
        *---------------------------------*/

        if( n == pql->L_list )        /* 0 at high end */
        {
           pql->length = k1;
           break;
        }

        else                          /* ok to copy list */
        {
           for( j=pql->L_list-1, k2=k1; j >= n; j-- )
              pql->list[k2++] = pql->list[j];

           pql->length = k2;
        }


       /*---------------------------------
        * Loop from k1 to k2 expanding 1 level.
        *---------------------------------*/

        for( i=k1; i < k2; i++ )
        {
           hd = (cdHd *) pql->list[i];

           if( hd->id != id_cdAtt )
           {
              pql_one_level( pql, hd );
                 err_r( );
           }
        }

        if( pql->length == k2 )   /* 0 nodes added */
        {
           pql->length == k1;
           break;
        }
     }
  }


 /*---------------------------------
  * Append to list tree nodes 'above' 'cur'.
  *---------------------------------*/


  else if( tflg == i_above )
  {
     k1 = pql->length;

     while( cur->id != id_cdNone )
     {
        cur = cur->above;

        pql_one_level( pql, cur );
           err_r( );
     }

     if( k1 < pql->length )
        pql_compress( pql, k1, idwant );
  }
  return;
}


/*********************************************************************
 * Function to process pql 'union' instruction.
 *********************************************************************/


/*--------------------------------------------------------------------
 * union from pp, from aa;
 * bb = union from pp, select ......;
 *-------------------------------------------------------------------*/

int pql_union( cdms_card     *line,  /* struct metafile line */
               cdms_pql_list *pql,   /* pql list */
               int           idx )   /* input symbol index */
{
  int i2, i3, i4;


 /*---------------------------------
  * Process first part of union.
  *---------------------------------*/


  i2 = idx;

  if( line->cls_sym[i2] == 'k' && line->len_sym[i2] == i_select )
  {
     i3 = pql_select( line, pql, i2+1 );
             err_ri( );
  }

  else
  {
     i3 = pql_gen_list( line, pql, 0, i2 );
             err_ri( );
  }


 /*---------------------------------
  * Process second part of union.
  *---------------------------------*/


  if( line->cls_sym[i3] == 'k' && line->len_sym[i3] == i_select )
  {
     i4 = pql_select( line, pql, i3+1 );
             err_ri( );
  }

  else
  {
     i4 = pql_gen_list( line, pql, 0, i3 );
             err_ri( );
  }

  return i4;
}


/*********************************************************************
 * Function to process pql 'virtual' macro instruction.
 *********************************************************************/


/*--------------------------------------------------------------------
 * virtual  /aa/bb  tam2  time  xx.cdms ;

 * Note: 'virtual /aa/bb tam2 time xx.cdms' is a macro for:
 *       dirtree 1 jj /aa/bb ;
 *       aa = select dataset at database dbjj1,
 *                   where dataset variable name = tam2 ;
 *       group from aa, variable tam2 dimension time ;
 *       writemata database dbjj1 xx.cdms ;
 *-------------------------------------------------------------------*/

int pql_virtual( cdms_card     *line,  /* struct metafile line */
                 cdms_pql_list *pql,   /* pql list */
                 int           idx )   /* input symbol index */
{
  int  i, k, i6;
  char msg[200], *nam_dir, *nam_var, *nam_dim, *nam_out;


 /*---------------------------------
  * Scan execute line.
  *---------------------------------*/


  if( idx + 3 > line->num_sym )
     err_xi( "pql_virtual: bad virtual instruction" );

  i = idx;
  if( line->cls_sym[i] != 'a' )
     err_xi( "pql_virtual: bad directory arg" );
  k = line->idx_sym[i];
  nam_dir = cr_asc_cpy( &line->asc_line[k] );

  i++;
  if( line->cls_sym[i] != 'a' )
     err_xi( "pql_virtual: bad variable arg" );
  k = line->idx_sym[i];
  nam_var = cr_asc_cpy( &line->asc_line[k] );

  i++;
  if( line->cls_sym[i] != 'a' )
     err_xi( "pql_virtual: bad dimension arg" );
  k = line->idx_sym[i];
  nam_dim = cr_asc_cpy( &line->asc_line[k] );

  i++;
  nam_out = NULL;
  if( line->cls_sym[i] == 'a' )
  {
     k = line->idx_sym[i];
     nam_out = cr_asc_cpy( &line->asc_line[k] );
     i++;
  }

  i6 = i;


 /*---------------------------------
  * Execute the 'dirtree' instruction.
  *---------------------------------*/


  sprintf( msg, "dirtree 1 jj %s ;", nam_dir );

  line->len_line = 0;
  line->num_sym  = 0;
  pql->length    = 0;

  pql_execute( msg );
     err_ri( );


 /*---------------------------------
  * Execute the 'select' instruction.
  *---------------------------------*/


  sprintf( msg, "aa = select dataset at database dbjj1, "
                "where dataset variable name = %s ;",
                nam_var );

  pql_execute( msg );
     err_ri( );


 /*---------------------------------
  * Execute the 'group' instruction.
  *---------------------------------*/


  sprintf( msg, "group from aa, variable %s dimension %s ;",
                nam_var, nam_dim );

  pql_execute( msg );
     err_ri( );


 /*---------------------------------
  * Execute the 'writemeta' instruction.
  *---------------------------------*/


  if( nam_out )
  {
     sprintf( msg, "writemeta database dbjj1 %s ;",
                   nam_out );

     pql_execute( msg );
        err_ri( );
  }

  return i6;
}


/*********************************************************************
 * Function to process all sections of pql 'where' clause.
 *********************************************************************/


/*--------------------------------------------------------------------
 * where variable attribute Karl value ECMWF2,
         variable dimension length > 100;

 * i1   = card index of 'variable attribute' 
 * i2   = card index of 'Karl value ECMWF2'
 * i3   = card index after ','
 *-------------------------------------------------------------------*/

int pql_where( cdms_card     *line,  /* struct metafile line */
               cdms_pql_list *pql,   /* pql list */
               int           idx )   /* input where section index */
{
  int     i, j, k, l, n, i1, i2, i3, flg;
  cdHd    *hd;
  cdms_Id ida;

  flg = 1;
  i1  = idx;


 /*---------------------------------
  * Check for keyword 'oneof' (ie. 'or' instead of 'and' the sections)
  *---------------------------------*/


     if( line->cls_sym[i1] == 'a' )
     {
        j = line->idx_sym[i1];

        if( !strcmp( &line->asc_line[j], "oneof" ) )
        {
           i3 = pql_with( line, pql, i1+1 );
              err_ri( );

           return i3;
        }
     }


 /*---------------------------------
  * Loop over the 'where' sections.
  *---------------------------------*/


  while( flg )
  {


    /*---------------------------------
     * Find i2 and i3 card symbol index's.

     * i1 is start of tree-node-chain section 'variable attribute'.
     * i2 is start of where-test section 'Karl value ECMWF2'.
     * i3 is ';', start of 'show', or i1 of next where section.
     *---------------------------------*/


     i2  = pql_fld2( line, i1 );
              err_ri( );

     i3  = pql_fld4( line, pql, i2 );
              err_ri( );

     n   = i2 - i1;
     l   = pql->length;
     i   = line->len_sym[i1];
     ida = pqlnode[i];


    /*---------------------------------
     * Loop over nodes.

     * Keep only those which satisfy this 'where' clause.
     *---------------------------------*/


     for( j=0, k=0; j < l; j++ )
     {
        hd = pql->list[j];


       /*---------------------------------
        * Determine if node satisfies 'where' clause.
        *---------------------------------*/


        if( hd->id == ida )
        {
           if( n == 1 )
           {
              i = pql_whr_test( pql, j, j+1 );
                     err_ri( );
           }
   
           else
           {
              pql_sublist( line, pql, j, i1, i2 );
                 err_ri( );
   
              i = pql_whr_test( pql, l, pql->length );
                     err_ri( );
   
              pql->length = l;
           }
        }


       /*---------------------------------
        * Store accepted node.
        *---------------------------------*/


        if( i )
           pql->list[k++] = hd;
     }


    /*---------------------------------
     * Advance to next 'where' clause.
     *---------------------------------*/


     pql->length = k;
     i1          = i3;

     if( line->cls_sym[i3] == ';' ||
         (line->cls_sym[i3] == 'k' && line->len_sym[i3] == i_show) )
        return i3;
  }
  return 0;
}


/*********************************************************************
 * Function: where clause's '== dimension' test for 1 node point.
 *********************************************************************/


int pql_whr_1test( cdms_pql_list *pql )   /* pql list */

{
  int k, flg;

  flg = 1;


 /*---------------------------------
  * Compare tree node against where clause testing values.
  *---------------------------------*/


  k = pql->flg_v1d;

  if( pql->nod_v1d )   /* variable is a dimension */
  {
     if( k == 2 || k == 3 || k == 5 )
        flg = 0;
  }

  else                 /* variable not a dimension */
  {
     if( k == 1 || k == 4 || k == 6 )
        flg = 0;
  }

  return flg;
}


/*********************************************************************
 * Function: where clause's 'ndim' test for 1 node point.
 *********************************************************************/


int pql_whr_dtest( cdms_pql_list *pql )   /* pql list */

{
  int k, flg;

  flg = 1;


 /*---------------------------------
  * Compare tree node against where clause testing values.
  *---------------------------------*/


  k = pql->flg_dim;

  if( pql->nod_dim < pql->tst_dim )
  {
     if( k == 1 || k == 5 || k == 6 )
        flg = 0;
  }

  else if( pql->nod_dim == pql->tst_dim )
  {
     if( k == 2 || k == 3 || k == 5 )
        flg = 0;
  }

  else if( pql->nod_dim > pql->tst_dim )
  {
     if( k == 1 || k == 3 || k == 4 )
        flg = 0;
  }

  return flg;
}


/*********************************************************************
 * Function: where clause's '== group' test.  Test if a dataset
 *           is used by any 'ds_group' file spanning dataset.
 *********************************************************************/


int pql_whr_gtest( cdms_pql_list *pql )   /* pql list */

{
  int k, flg;

  flg = 1;


 /*---------------------------------
  * Compare tree node against where clause testing values.
  *---------------------------------*/


  k = pql->flg_grp;

  if( pql->nod_grp )   /* dataset used in file-spanning */
  {
     if( k == 2 || k == 3 || k == 5 )
        flg = 0;
  }

  else                 /* dataset not used in file-spanning */
  {
     if( k == 1 || k == 4 || k == 6 )
        flg = 0;
  }

  return flg;
}


/*********************************************************************
 * Function: where clause's 'id' test for 1 node point.
 *********************************************************************/


int pql_whr_itest( cdms_pql_list *pql )   /* pql list */

{
  int k, flg;

  flg = 1;


 /*---------------------------------
  * Compare tree node against where clause testing values.
  *---------------------------------*/


  k = pql->flg_sid;

  if( pql->nod_sid < pql->tst_sid )
  {
     if( k == 1 || k == 5 || k == 6 )
        flg = 0;
  }

  else if( pql->nod_sid == pql->tst_sid )
  {
     if( k == 2 || k == 3 || k == 5 )
        flg = 0;
  }

  else if( pql->nod_sid > pql->tst_sid )
  {
     if( k == 1 || k == 3 || k == 4 )
        flg = 0;
  }

  return flg;
}


/*********************************************************************
 * Function: where clause's 'length' test for 1 node point.
 *********************************************************************/


int pql_whr_ltest( cdms_pql_list *pql )   /* pql list */

{
  int k, flg;

  flg = 1;


 /*---------------------------------
  * Compare tree node against where clause testing values.
  *---------------------------------*/


  k = pql->flg_len;

  if( pql->nod_len < pql->tst_len )
  {
     if( k == 1 || k == 5 || k == 6 )
        flg = 0;
  }

  else if( pql->nod_len == pql->tst_len )
  {
     if( k == 2 || k == 3 || k == 5 )
        flg = 0;
  }

  else if( pql->nod_len > pql->tst_len )
  {
     if( k == 1 || k == 3 || k == 4 )
        flg = 0;
  }

  return flg;
}


/*********************************************************************
 * Function: load tree nodes 'name,length,value' testing values.
 *********************************************************************/


void pql_whr_node( cdms_pql_list *pql,   /* pql list */
                   int           idx )   /* index to list */

{
  int        i;
  char       **vlst;
  cdDb_new   *db;
  cdDset_new *ds, *dsa;
  cdVar_new  *var;
  cdDim_new  *dim;
  cdAtt_new  *att;
  cdHd       *hd;
  cdTmp      *tmp;


 /*---------------------------------
  * Initalize tree node test values.
  *---------------------------------*/


  hd = pql->list[idx];

  pql->nod_sid  = 0;
  pql->nod_nam  = NULL;
  pql->nod_len  = 0;
  pql->nod_typ  = cdInvalidType;
  pql->nod_val  = NULL;
  pql->nod_vtyp = cdInvalidType;
  pql->nod_dim  = 0;
  pql->nod_v1d  = 0;
  pql->nod_grp  = 0;


 /*---------------------------------
  * Set name,length,value from the tree node.
  *---------------------------------*/


  if( hd->id == id_cdDb )
  {
     db           = (cdDb_new *) hd;
     pt_to_long( hd, pql->nod_sid );
     pql->nod_nam = db->name;
  }

  else if( hd->id == id_cdDset )
  {
     ds           = (cdDset_new *) hd;
     pt_to_long( hd, pql->nod_sid );
     pql->nod_nam = ds->name;


    /*---------------------------------
     * Check dataset==group (ie. is it used in file-spanning).
     *---------------------------------*/
     if( pql->flg_grp )
     {
        if( !strcmp( ds->name, "ds_group" ) )
           pql->nod_grp = 7;

        else
        {
           for( dsa=ds->next; dsa; dsa=dsa->next )
           {
              if( !strcmp( dsa->name, "ds_group" ) )
              {
                 att = scn_lnk_list( "psql_datasetlist", id_cdAtt,
                                     (cdHd *) dsa->dims );

                 if( att != NULL && att->length > 0 )
                 {
                    vlst = att->values;

                    for( i=0; i < att->length; i++ )
                    {
                       if( !strcmp( vlst[i], ds->name) )
                       {
                          pql->nod_grp = 7;
                          break;
                       }
                    }
                 }
              }

              if( pql->nod_grp )
                 break;
           }
        }
     }
  }

  else if( hd->id == id_cdVar )
  {
     var           = (cdVar_new *) hd;
     pt_to_long( hd, pql->nod_sid );
     pql->nod_nam  = var->name;
     pql->nod_len  = var->length;
     pql->nod_typ  = var->datatype;
     pql->nod_val  = var->data;
     pql->nod_dim  = var->ndims;
     pql->nod_vtyp = var->datatype; 


    /*---------------------------------
     * Check variable==dimension.  Have same name as it's dimension.
     *---------------------------------*/
     if( pql->flg_v1d )
     {
        for( tmp=(cdTmp *) var->dim; tmp; tmp=tmp->next )
        {
           dim = (cdDim_new *) tmp->want;

           if( !strcmp( var->name, dim->name ) )
           {
              pql->nod_v1d = 7;
              break;
           }
        }
     }
  }

  else if( hd->id == id_cdDim )
  {
     dim           = (cdDim_new *) hd;
     pt_to_long( hd, pql->nod_sid );
     pql->nod_nam  = dim->name;
     pql->nod_len  = dim->length;
     pql->nod_typ  = dim->datatype;
     pql->nod_val  = dim->data;
     pql->nod_vtyp = dim->datatype;
  }

  else if( hd->id == id_cdAtt )
  {
     att           = (cdAtt_new *) hd;
     pt_to_long( hd, pql->nod_sid );
     pql->nod_nam  = att->name;
     pql->nod_len  = att->length;
     pql->nod_typ  = att->datatype;
     pql->nod_vtyp = att->datatype;
     pql->nod_val  = att->values;
  }
  return;
}


/*********************************************************************
 * Function: where clause's 'name' test for 1 node point.
 *********************************************************************/


int pql_whr_ntest( cdms_pql_list *pql )   /* pql list */

{
  int  i, k, l, n, flg;
  char *aa;

  flg = 1;


 /*---------------------------------
  * Compare tree node against where clause testing values.
  *---------------------------------*/


  if( pql->nod_nam == NULL )
     return 0;

  k = strlen( pql->tst_nam );
  l = strlen( pql->nod_nam );


 /*---------------------------------
  * name = *abc*  --- pattern anywhere in string.
  *---------------------------------*/


  if( pql->tst_nam[0] == '*' && pql->tst_nam[k-1] == '*' )
  {
     flg = 0;
     n   = l - k + 3;

     for( i=0; i < n ; i++ )
     {
        if( pql->nod_nam[i] == pql->tst_nam[1] )
        {
           flg = !strncmp( &pql->tst_nam[1], &pql->nod_nam[i], k-2 );

           if( flg )
              break;
        }
     }
  }


 /*---------------------------------
  * name = abc*  --- pattern at start of string.
  *---------------------------------*/


  else if( pql->tst_nam[k-1] == '*' )
     flg = !strncmp( pql->tst_nam, pql->nod_nam, k-1 );


 /*---------------------------------
  * name = *abc  --- pattern at end of string.
  *---------------------------------*/


  else if( pql->tst_nam[0] == '*' )
     flg = !strncmp( &pql->tst_nam[1], &pql->nod_nam[l-k+1], k-1 );


 /*---------------------------------
  * name = abc  --- pattern is entire string.
  *---------------------------------*/


  else
     flg = !strcmp( pql->tst_nam, pql->nod_nam );


  if( pql->flg_nam == 2 )
     flg = !flg;

  return flg;
}


/*********************************************************************
 * Function: append to pql list all nodes 1 level below 'cur'.
 *********************************************************************/


int pql_whr_test( cdms_pql_list *pql,    /* pql list */
                  int           idx1,    /* index to list */
                  int           idx2 )   /* index to list */

{
  int    i, k, flg;
  char   *aa;
  long   *ll;
  double *dd;


 /*---------------------------------
  * Loop over nodes.
  *---------------------------------*/


  for( i=idx1; i < idx2; i++ )
  {
    /*---------------------------------
     * Load tree node 'name,length,value' testing values.
     *---------------------------------*/


     pql_whr_node( pql, i );

     flg = 1;


    /*---------------------------------
     * Compare tree node against where clause testing values.
     *---------------------------------*/


    /*---------------------------------
     * 'id'
     *---------------------------------*/


     if( pql->flg_sid != 0 && flg == 1 )
     {
        flg = pql_whr_itest( pql );
     }


    /*---------------------------------
     * 'name'
     *---------------------------------*/

     if( pql->flg_nam != 0 )
     {
        flg = pql_whr_ntest( pql );
     }


    /*---------------------------------
     * 'length'
     *---------------------------------*/


     if( pql->flg_len != 0 && flg == 1 )
     {
        flg = pql_whr_ltest( pql );
     }


    /*---------------------------------
     * 'type'
     *---------------------------------*/


     if( pql->flg_typ != 0 && flg == 1 )
     {
        flg = pql_whr_ttest( pql );
     }


    /*---------------------------------
     * 'value'
     *---------------------------------*/


     if( pql->flg_val != 0 && flg == 1 )
     {
        flg = pql_whr_vtest( pql );
                 err_ri( );
     }


    /*---------------------------------
     * 'ndim'  (ie. is variable number of dimensions)
     *---------------------------------*/


     if( pql->flg_dim != 0 && flg == 1 )
     {
        flg = pql_whr_dtest( pql );
     }


    /*---------------------------------
     * 'dimension'  (ie. is variable also a dimension)
     *---------------------------------*/


     if( pql->flg_v1d != 0 && flg == 1 )
     {
        flg = pql_whr_1test( pql );
     }


    /*---------------------------------
     * 'group'  (ie. is dataset used in file-spanning)
     *---------------------------------*/


     if( pql->flg_grp != 0 && flg == 1 )
     {
        flg = pql_whr_gtest( pql );
     }


    /*---------------------------------
     * Check if node passes test.
     *---------------------------------*/

     if( flg == 1 )
        return 1;
  }


 /*---------------------------------
  * If here looped over nodes and all failed.
  *---------------------------------*/

  return 0;
}


/*********************************************************************
 * Function: where clause's 'type' test for 1 node point.
 *********************************************************************/


int pql_whr_ttest( cdms_pql_list *pql )   /* pql list */

{
  int  flg;
  char *aa;

  flg = 1;


 /*---------------------------------
  * Compare tree node against where clause testing values.
  *---------------------------------*/


  if( pql->nod_typ == cdInvalidType )
     return 0;


 /*---------------------------------
  * type = int
  *---------------------------------*/


  aa = typ_as_ascii( pql->nod_typ );

  flg = !strcmp( pql->tst_typ, aa );

  free( aa );


  if( pql->flg_typ == 2 )
     flg = !flg;

  return flg;
}


/*********************************************************************
 * Function: where clause's 'value' test for 1 node point.
 *********************************************************************/


int pql_whr_vtest( cdms_pql_list *pql )   /* pql list */

{
  int    f, k, flg;
  char   *aa, *bb;
  long   *ll;
  double *dd;

  flg = 1;


 /*---------------------------------
  * Compare tree node against where clause testing values.
  *---------------------------------*/


 /*---------------------------------
  * ascii
  *---------------------------------*/


  if( pql->tst_vtyp == cdChar )
  {
     if( pql->nod_vtyp != cdChar )
        return 0;

     f  = pql->flg_nam;
     aa = pql->tst_nam;
     bb = pql->nod_nam;

     pql->flg_nam = pql->flg_val;
     pql->tst_nam = pql->tst_a_val;
     pql->nod_nam = pql->nod_val;

     flg = pql_whr_ntest( pql );

     pql->flg_nam = f;
     pql->tst_nam = aa;
     pql->nod_nam = bb;
  }


 /*---------------------------------
  * integer
  *---------------------------------*/


  else if( pql->tst_vtyp == cdLong )
  {
     if( pql->nod_vtyp == cdChar || pql->nod_len != 1 )
        return 0;

     k = pql->flg_val;

     ll = ary_trans( pql->nod_vtyp, 1, pql->nod_val, cdLong );
             err_ri( );

     if( *ll < pql->tst_i_val )
     {
        if( k == 1 || k == 5 || k == 6 )
           flg = 0;
     }

     else if( *ll == pql->tst_i_val )
     {
        if( k == 2 || k == 3 || k == 5 )
           flg = 0;
     }

     else if( *ll > pql->tst_i_val )
     {
        if( k == 1 || k == 3 || k == 4 )
           flg = 0;
     }

     free( ll );
  }


 /*---------------------------------
  * real
  *---------------------------------*/


  else if( pql->tst_vtyp == cdDouble )
  {
     if( pql->nod_vtyp == cdChar || pql->nod_len != 1 )
        return 0;

     k = pql->flg_val;

     dd = ary_trans( pql->nod_vtyp, 1, pql->nod_val, cdDouble );
             err_ri( );

     if( *dd < pql->tst_f_val )
     {
        if( k == 1 || k == 5 || k == 6 )
           flg = 0;
     }

     else if( *dd == pql->tst_f_val )
     {
        if( k == 2 || k == 3 || k == 5 )
           flg = 0;
     }

     else if( *dd > pql->tst_f_val )
     {
        if( k == 1 || k == 3 || k == 4 )
           flg = 0;
     }

     free( dd );
  }

  return flg;
}


/*********************************************************************
 * Function to process all sections of pql 'with' clause.
 *********************************************************************/


/*--------------------------------------------------------------------
 * NOTE: format of 'with' (OR) is identical to 'where' (AND) clause.  
 *       'with' accepts node if any 'with' section is satisfied.
 *       'where' accepts node if all 'where' sections are satisfied.

 * with variable attribute Karl value ECMWF2,
         variable dimension length > 100;

 * i1   = card index of 'variable attribute' 
 * i2   = card index of 'Karl value ECMWF2'
 * i3   = card index after ','
 *-------------------------------------------------------------------*/

int pql_with( cdms_card     *line,  /* struct metafile line */
               cdms_pql_list *pql,   /* pql list */
               int           idx )   /* input with section index */
{
  int     i, j, k, l, n, i1, i2, i3, flg, *node_keep;
  cdHd    *hd;
  cdms_Id ida;

  flg = 1;
  i1  = idx;
  l   = pql->length;

  if( l > 0 )
     node_keep = (int *) calloc( 1, l * sizeof(int) );


 /*---------------------------------
  * Loop over the 'with' sections.
  *---------------------------------*/


  while( flg )
  {


    /*---------------------------------
     * Find i2 and i3 card symbol index's.

     * i1 is start of tree-node-chain section 'variable attribute'.
     * i2 is start of with-test section 'Karl value ECMWF2'.
     * i3 is ';', start of 'show', or i1 of next with section.
     *---------------------------------*/


     i2  = pql_fld2( line, i1 );
              err_ri( );

     i3  = pql_fld4( line, pql, i2 );
              err_ri( );

     n   = i2 - i1;
     i   = line->len_sym[i1];
     ida = pqlnode[i];


    /*---------------------------------
     * Loop over nodes.

     * Keep only those which satisfy this 'with' clause.
     *---------------------------------*/


     for( j=0; j < l; j++ )
     {
       /*---------------------------------
        * Check if node not already accepted.
        *---------------------------------*/


        if( node_keep[j] == 0 )
        {
           hd = pql->list[j];


          /*---------------------------------
           * Determine if node satisfies 'with' clause.
           *---------------------------------*/


           if( hd->id == ida )
           {
              if( n == 1 )
              {
                 i = pql_whr_test( pql, j, j+1 );
                        err_ri( );
              }
   
              else
              {
                 pql_sublist( line, pql, j, i1, i2 );
                    err_ri( );
   
                 i = pql_whr_test( pql, l, pql->length );
                        err_ri( );
   
                 pql->length = l;
              }
           }


          /*---------------------------------
           * Store accepted node.
           *---------------------------------*/


           if( i )
              node_keep[j] = 33;
        }
     }


    /*---------------------------------
     * Advance to next 'with' clause.
     *---------------------------------*/


     i1 = i3;

     if( line->cls_sym[i3] == ';' ||
         (line->cls_sym[i3] == 'k' && line->len_sym[i3] == i_show) )
     {
       /*---------------------------------
        * Keep nodes which passed any of the 'with' sections.
        *---------------------------------*/
        if( l > 0 )
        {
           for( j=0, k=0; j < l; j++ )
           {
              hd = pql->list[j];

              if( node_keep[j] )
                 pql->list[k++] = hd;
           }
           pql->length = k;
           free( node_keep );
        }

        return i3;
     }
  }
  return 0;
}


/*********************************************************************
 * Function to process pql 'writeds' instruction.
 *********************************************************************/


/*--------------------------------------------------------------------
 * writeds time dataset from pp, netcdf-file-name;

 * i2   = card index of 'from pp'
 * i3   = card index of 'filename'
 * i4   = card index of ';'

 * writeds database db1 dataset ds3, netcdf-file-name;
 * writeds  time  database db1 dataset ds3, netcdf-file-name;
 *-------------------------------------------------------------------*/

int pql_writeds( cdms_card     *line,  /* struct metafile line */
                 cdms_pql_list *pql,   /* pql list */
                 int           idx )   /* input symbol index */
{
  int        i, j, k;
  char       *aa, *bb;
  cdDset_new *dset, *dseta;
  cdDim_new  *dim, *dima;
  cdVar_new  *var, *vara;
  cdTmp      *tmp;
  cdHd *hd;


 /*---------------------------------
  * Scan input line.
  *---------------------------------*/


    /*---------------------------------
     * Check if optional record-dimension-name given.
     *---------------------------------*/


     if( line->cls_sym[idx] == 'a' )
     {
        j  = line->idx_sym[idx++];
        bb = cr_asc_cpy( &line->asc_line[j] );
     }
     else
        bb = cr_asc_cpy( "time" );


    /*---------------------------------
     * If not 'database db1 dataset ds3' then 'dataset from pp'.
     *---------------------------------*/


     if( line->cls_sym[idx] == 'k' && line->cls_sym[idx+1] == 'a' )
     {
        hd = pql_fld3( line, idx, &i );
                err_ri( );
     }
     else
     {
        k = pql_fld2( line, idx );
               err_ri( );

        i = pql_gen_list( line, pql, idx, k );
               err_ri( );

        if( pql->length != 1 )
           err_xi( "writeds: must give 1 dataset to be written" );

        hd = pql->list[0];
     }

     if( hd->id != id_cdDset || line->cls_sym[i] != 'a' )
        err_xi( "pql_writeds: not a dataset node" );

     j  = line->idx_sym[i];
     aa = cr_asc_cpy( &line->asc_line[j] );


 /*---------------------------------
  * Copy entire dataset to a scratch dataset node.
  *---------------------------------*/


  dset = (cdDset_new *) hd;

  dseta = copy_struct( hd, (cdHd *) hd->above );
             err_ri( );


 /*---------------------------------
  * Load from disc var,dim data arrays.
  *---------------------------------*/


  load_ds_arrays( dseta );
     err_ri( );


 /*---------------------------------
  * Erase variables that are also dimensions.
  * Note: I create them myself (ie. NetCDF quirk/bug: to write out
  *       coordinate array it must be a NetCDF variable).
  *---------------------------------*/


  for( dim=dseta->dims; dim; dim=dim->next )
  {
     for( var=dseta->vars; var; var=var->next )
     {
        if( var->ndims == 1 && !strcmp( var->name, dim->name ) )
        {
           delete_struct( (cdHd *) var );
              err_ri( );

           break;
        }
     }
  }


 /*---------------------------------
  * Erase dim whose name starts with "psql_" (ie. alterfile :deldim).
  *---------------------------------*/


  for( dim=dseta->dims; dim; )
  {
     if( strstr( dim->name, "psql_" ) != NULL )
     {
        dima = dim->next;

        delete_struct( (cdHd *) dim );
           err_ri( );

        dim = dima;
     }

     else
        dim = dim->next;
  }


 /*---------------------------------
  * Erase var whose name starts with "psql_" (ie. alterfile :delvar).
  *---------------------------------*/


  for( var=dseta->vars; var; )
  {
     if( strstr( var->name, "psql_" ) != NULL )
     {
        vara = var->next;

        delete_struct( (cdHd *) var );
           err_ri( );

        var = vara;
     }

     else
        var = var->next;
  }


 /*---------------------------------
  * NetCDF-write ds copy struct.
  *---------------------------------*/


  wrt_ds_to_netcdf( dseta, aa, bb );
     err_ri( );


 /*---------------------------------
  * Delete the ds copy struct from memory.
  *---------------------------------*/


  free( aa );
  free( bb );

  delete_struct( (cdHd *) dseta );
     err_ri( );

  return i+1;
}


/*********************************************************************
 * Function to process pql 'writemeta' instruction.

 * writemeta database db1, myout1
 * writemeta shortspan hours dataset from aa, myout1
 *********************************************************************/


int pql_writemeta( cdms_card     *line,  /* struct metafile line */
                   cdms_pql_list *pql,   /* pql list */
                   int           idx )   /* input symbol index */
{
  int       i, j, k, f, smflg;
  char      *unam;
  cdDb_new  *db;
  cdHd      *hd;
  cdAtt_new *att;

  hd    = NULL;
  smflg = 0;


 /*---------------------------------
  * Scan key-word symbols at start of input line.
  *---------------------------------*/


  if( line->cls_sym[idx] == 'a' )
  {
     j = line->idx_sym[idx];
     if( !strcmp( &line->asc_line[j], "shortspan" ) )
     {
        smflg = 1;
        idx++;
     }
  }

  if( line->cls_sym[idx] == 'a' )
  {
     j = line->idx_sym[idx];
     unam = cr_asc_cpy( &line->asc_line[j] );
     idx++;
  }
  else
     unam = cr_asc_cpy( "hours" );


 /*---------------------------------
  * Scan 'database db1' or 'from aa' symbols on input line.
  *---------------------------------*/


  if( line->cls_sym[idx] == 'k' && line->len_sym[idx] != i_from )
  {
    /*---------------------------------
     * Process 'database db1,' single-tree-node case..
     *---------------------------------*/


     f = 0;

     hd = pql_fld3( line, idx, &i );
             err_ri( );

     if( hd->id != id_cdDb )
        err_xi( "pql_writemeta: bad writemeta instruction" );
  }

  else
  {
    /*---------------------------------
     * Process list-of-nodes case.
     *---------------------------------*/


     f = 3;

     i = pql_gen_list( line, pql, 0, idx );
            err_ri( );
  }


 /*---------------------------------
  * Create output file.
  *---------------------------------*/


  if( line->cls_sym[i] != 'a' )
     err_xi( "pql_writemeta: bad writemeta instruction" );

  j = line->idx_sym[i];

  sprintf( PSQL_MSG, "***writemeta: %s\n", &line->asc_line[j] );
  wrt_msg( );

  if( FPT_OUT != NULL )
  {
     fprintf( FPT_OUT, "*** WRITEMETA disconnecting TTYCOPY file\n" );
     fclose( FPT_OUT );
     FPT_OUT = NULL;
  }

  FPT_OUT = fopen( &line->asc_line[j], "w+" );


 /*---------------------------------
  * Edit to output file.
  *---------------------------------*/


     if( f == 0 )
     {
       /*---------------------------------
        * Write 1 tree node and everything under it.
        *---------------------------------*/


        if( hd == (cdHd *) DB_ROOT_ADR )
        {
           db = (cdDb_new *) hd;

           for( db=db->next; db; db=db->next )
              edit_struct( (cdHd *) db );
        }

        else
           edit_struct( hd );

        fprintf( FPT_OUT, "end:\n" );
     }

     else
     {
       /*---------------------------------
        * Write list of tree nodes (and everything under them).
        *---------------------------------*/


        if( smflg == 1 )
        {
           wrt_small_file( pql->list[0], unam );
              err_ri( );
        }

        else
        {
           for( hd=pql->list[0]; hd->id != id_cdDb; hd=hd->above )
              ;

           db = (cdDb_new *) hd;

           edit_struct_hdr( "database" );
           fprintf( FPT_OUT, "\tname = \"%s\" ;\n", db->name );

           for( j=0; j < pql->length; j++ )
           {
              hd = pql->list[j];

              edit_struct( hd );
                 err_ri( );
           }

           fprintf( FPT_OUT, "end:\n" );
        }
     }

  fclose( FPT_OUT );
  FPT_OUT = NULL;
  free( unam );

  return ++i;
}


/*********************************************************************
 * Function to process metafile 'data' lines
 *********************************************************************/


void pro_data_lines( cdms_card *line,  /* struct metafile line */
                     cdHd      *mas )  /* above struct */
{
  int       i, j, k, num_sym;
  long      num, l, *ll;
  double    *dd, *dd1, first, delta;
  cdType    typ, typ_l;
  cdVar_new *var;
  cdDim_new *dim;
  void      *pp, *vv, *zz;

  if( FPT_OUT != NULL )
     fprintf( FPT_OUT, "***found data:\n" );

  dim = NULL;
  var = NULL;
  pp  = NULL;
  vv  = NULL;

  if( mas->id != id_cdDset && mas->id != id_cdDb )
     err_x( "pro_data_lines: need a dataset for data lines" );


 /*---------------------------------
  * Loop lines of data section
  *---------------------------------*/


  while( line->eof != NULL )
  {


    /*---------------------------------
     * Read a complete metafile line and crack it into symbols
     *---------------------------------*/


     num_sym = rd_meta_line( line );
                  err_r( );


    /*---------------------------------
     * Check if line is first line of next section (ie. 'name:' )
     *---------------------------------*/


     if( (num_sym == 2 && line->cls_sym[0] == 'a'
         && line->cls_sym[1] == ':') || num_sym < 4 )
        return;


    /*---------------------------------
     * Find the dimension or variable to store into
     *---------------------------------*/


     if( line->cls_sym[0] != 'a' || line->cls_sym[1] != '=' )
        err_x( "pro_data_lines: bad data line" );

     j = line->idx_sym[0];

     dim = nam_fnd( &line->asc_line[j], id_cdDim, mas );

     if( dim != NULL )
     {
        l     = dim->length;
        typ_l = dim->datatype;
     }

     else
     {
        var = nam_fnd( &line->asc_line[j], id_cdVar, mas );

        if( var == NULL )
           err_x( "pro_data_lines: bad data line" );

        l     = var->length;
        typ_l = var->datatype;
     }


    /*---------------------------------
     * Check of linear data card to calculate coordinates.

     * lat = linear(-90.0, 2.8125);  ... point, delta
     * lat = range( 90.0, -90.0 );   ... begin, end
     *---------------------------------*/


     j = line->idx_sym[2];
     k = 0;

     if( line->cls_sym[2] == 'a' && !strcmp( "linear",
         &line->asc_line[j] ) && line->cls_sym[3] == '(' )
        k = 1;

     else if( line->cls_sym[2] == 'a' && !strcmp( "range",
         &line->asc_line[j] ) && line->cls_sym[3] == '(' )
        k = 2;


    /*---------------------------------
     * Get coordinates  (ie. 'lon=-87.86, -86., -85, ...)
     *---------------------------------*/


     if( k == 0 )
     {
        vv = meta_after_eq( 2, line, &typ, &num );
                err_r( );

        if( dim != NULL && dim->length != num &&
                           (FPT_TTY == 1 || FPT_PY == 1) )
        {
           sprintf( PSQL_MSG, "pro_data_lines %s %d %d\n", dim->name,
                              dim->length, num );
           wrt_msg( );
        }

        if( typ_l != typ )
        {
           zz = vv;
           vv = ary_trans( typ, num, zz, typ_l );
                   err_r( );
           free( zz );
        }
     }


    /*---------------------------------
     * Scan linear data values.
     *---------------------------------*/


     else
     {
       /*---------------------------------
        * Get linear_data and set [first,delta]
        *---------------------------------*/


        pp = meta_after_eq( 4, line, &typ, &num );
                err_r( );

        if( typ == cdLong )
        {
           ll    = (long *) pp;
           first = ll[0];
           delta = ll[1];
        }

        else if( typ == cdDouble )
        {
           dd1   = (double *) pp;
           first = dd1[0];
           delta = dd1[1];
        }

        else
        {
           err_x( "pro_data_lines: bad card" );
        }


       /*---------------------------------
        * Convert linear_data to a  coordinate array.
        *---------------------------------*/


        if( l < 1 )
        {
           err_x( "pro_data_lines: bad length" );
        }

        if( k == 2 )
           delta = ( delta - first ) / ( l - 1 );

        dd = (double *) malloc( l * sizeof( double ) );
        if( dd == NULL )
           err_x( "pro_data_lines: trouble getting memory" );

        dd[0] = first;

        for( i=1; i<l; i++ )
           dd[i] = dd[i-1] + delta;


       /*---------------------------------
        * Finalize coordinate array.
        *---------------------------------*/


        vv = ary_trans( cdDouble, l, dd, typ_l );
                err_r( );

        free( dd );
     }


    /*---------------------------------
     * Set output struct.
     *---------------------------------*/


     if( pp != NULL )
        free( pp );

     if( dim != NULL )
        dim->data = vv;
     else
        var->data = vv;
  }
  return;
}


/*********************************************************************
 * Function to process metafile 'database' lines
 *********************************************************************/


cdDb_new *pro_db_lines( cdms_card *line,  /* struct metafile line */
                        cdHd      *mas )  /* current struct */
{
  int        i, j, num_sym;
  cdDb_new   *db;

  if( FPT_OUT != NULL )
     fprintf( FPT_OUT, "***found database:\n" );

  if( mas == NULL )
     err_xv( "pro_db_lines: logic error" );

  db = (cdDb_new *) mas;

  if( mas->id == id_cdDset )
     db = (cdDb_new *) mas->above;


 /*---------------------------------
  * Loop over lines of database section
  *---------------------------------*/


  while( line->eof != NULL )
  {
    /*---------------------------------
     * Read a complete metafile line and crack it into symbols
     *---------------------------------*/


     num_sym = rd_meta_line( line );
                  err_rv( );


    /*---------------------------------
     * Check if line is first line of next section (ie. 'name:' )
     *---------------------------------*/


     if( (num_sym == 2 && line->cls_sym[0] == 'a'
         && line->cls_sym[1] == ':') || num_sym < 4 )
        return db;


    /*---------------------------------
     * Check for attribute definition card
     *---------------------------------*/


     else if( db != NULL && (line->cls_sym[0] == ':'
              || line->cls_sym[1] == ':') )
     {
        db = meta_att( line, (cdHd *) db );
                err_rv( );
     }


    /*---------------------------------
     * If here must be a database definition card
     *              name = "ducksoup"; -- 6 symbols
     *              locate = "ducksoup"; -- go back to db
     *---------------------------------*/


     else
     {
        if( num_sym != 6 || line->cls_sym[0] != 'a'
            || line->cls_sym[1] != '=' || line->cls_sym[2] != '"'
            || line->cls_sym[3] != 'q' || line->cls_sym[4] != '"'
            || line->cls_sym[5] != ';' )
           err_xv( "pro_db_lines: bad card" );

        i = line->idx_sym[0];
        j = line->idx_sym[3];

        if( !strcmp( "name", &line->asc_line[i] ) )
        {
           db       = cre_struct( id_cdDb, NULL );
           db->name = cr_asc_cpy( &line->asc_line[j] );
        }

        else if( !strcmp( "locate", &line->asc_line[i] ) )
        {
           db = nam_fnd( &line->asc_line[j], id_cdDb, NULL );

           if( db == NULL )
              err_xv( "pro_db_lines: bad locate" );
        }

        else
        {
           err_xv( "pro_db_lines: bad card" );
        }
     }
  }

  return db;
}


/*********************************************************************
 * Function to process metafile 'dimension' lines
 *********************************************************************/


void pro_dim_lines( cdms_card  *line,  /* struct metafile line */
                    cdHd       *mas )  /* above struct */
{
  int       i, num_sym;
  cdDim_new *dim;

  if( FPT_OUT != NULL )
     fprintf( FPT_OUT, "***found dimensions:\n" );

  dim = NULL;


 /*---------------------------------
  * Loop lines of dimension section
  *---------------------------------*/


  while( line->eof != NULL )
  {


    /*---------------------------------
     * Read a complete metafile line and crack it into symbols
     *---------------------------------*/


     num_sym = rd_meta_line( line );
                  err_r( );


    /*---------------------------------
     * Check if line is first line of next section (ie. 'name:' )
     *---------------------------------*/


     if( (num_sym == 2 && line->cls_sym[0] == 'a'
         && line->cls_sym[1] == ':') || num_sym < 4 )
        return;


    /*---------------------------------
     * Check for attribute definition card
     *---------------------------------*/


     else if( dim != NULL && (line->cls_sym[0] == ':'
              || line->cls_sym[1] == ':') )
     {
        dim = (cdDim_new *) meta_att( line, (cdHd *) dim );
                 err_r( );
     }


    /*---------------------------------
     * If here must be a dimension definition card
     *---------------------------------*/


     else
     {
        dim = (cdDim_new *) cre_struct( id_cdDim, mas );

        i = dim_f1( 0, line, dim );
               err_r( );

        i = dim_f2( i, line, dim );
               err_r( );

        i = dim_f3( i, line, dim );
               err_r( );
     }
  }
  return;
}


/*********************************************************************
 * Function to process metafile 'dataset' lines
 *********************************************************************/


cdDset_new *pro_dset_lines( cdms_card *line,  /* metafile line */
                            cdHd      *mas )  /* above struct */
{
  int        i, j, num_sym;
  cdDset_new *ds;
  cdHd       *hd;

  if( FPT_OUT != NULL )
     fprintf( FPT_OUT, "***found dataset:\n" );

  if( mas == NULL )
     err_xv( "pro_dset_lines: logic error" );

  hd = mas;
  ds = NULL;

  if( mas->id == id_cdDset )
  {
     hd = (cdHd *) mas->above;
     ds = (cdDset_new *) mas;
  }


 /*---------------------------------
  * Loop lines of dataset section
  *---------------------------------*/


  while( line->eof != NULL )
  {
    /*---------------------------------
     * Read a complete metafile line and crack it into symbols
     *---------------------------------*/


     num_sym = rd_meta_line( line );
                  err_rv( );


    /*---------------------------------
     * Check if line is first line of next section (ie. 'name:' )
     *---------------------------------*/


     if( (num_sym == 2 && line->cls_sym[0] == 'a'
         && line->cls_sym[1] == ':') || num_sym < 4 )
        return ds;


    /*---------------------------------
     * Check for attribute definition card
     *---------------------------------*/


     else if( ds != NULL && (line->cls_sym[0] == ':'
              || line->cls_sym[1] == ':') )
     {
        ds = meta_att( line, (cdHd *) ds );
                err_rv( );
     }


    /*---------------------------------
     * If here must be a dataset definition card
     *              name = "ducksoup"; -- 6 symbols
     *              locate = "ducksoup"; -- return to already defined
     *---------------------------------*/


     else
     {
        if( num_sym != 6 || line->cls_sym[0] != 'a'
            || line->cls_sym[1] != '=' || line->cls_sym[2] != '"'
            || line->cls_sym[3] != 'q' || line->cls_sym[4] != '"'
            || line->cls_sym[5] != ';' )
           err_xv( "pro_dset_lines: bad card" );

        i = line->idx_sym[0];
        j = line->idx_sym[3];

        if( !strcmp( "name", &line->asc_line[i] ) )
        {
           ds       = cre_struct( id_cdDset, hd );
           ds->name = cr_asc_cpy( &line->asc_line[j] );
        }

        else if( !strcmp( "locate", &line->asc_line[i] ) )
        {
           ds = nam_fnd( &line->asc_line[j], id_cdDset, (cdHd *) ds );

           if( ds == NULL )
              err_xv( "pro_dset_lines: bad locate" );
        }

        else
        {
           err_xv( "pro_dset_lines: bad card" );
        }
     }
  }

  return ds;
}


/*********************************************************************
 * Function to replace placeholder attributes with actual structs.

 * Note: As metafile lines are scanned, just the names of sub-fields
         are stored.  That's because, in a metafile, users can define
         'variables' before defineing 'dimensions'.

 * Note: The placeholder 'names' are stored as attributes.  It is now
         time to find these attribute structs; delete them from
         memory; find the referenced-to struct (which should exist
         by this time); and insert a pointer to it where required.
 *********************************************************************/


void pro_tmp_att( void ) /* no input arguments */
{
  cdDb_new   *db;
  cdDset_new *ds;

  if( FPT_OUT != NULL )
     fprintf( FPT_OUT, "***found end:\n" );


 /*---------------------------------
  * Loop over databases
  *---------------------------------*/


  for( db = DB_ROOT_ADR; db; db = db->next )
  {
    /*---------------------------------
     * Finalize database-wide dimension/variable structs
     *---------------------------------*/


     att_to_dgv( db->dims, db->vars );
        err_r( );


    /*---------------------------------
     * Loop over datasets
     *---------------------------------*/


     for( ds = db->dsets; ds; ds = ds->next )
     {
       /*---------------------------------
        * Finalize dataset dimension/variable structs
        *---------------------------------*/


        att_to_dgv( ds->dims, ds->vars );
           err_r( );
     }
  }
  return;
}


/*********************************************************************
 * Function to process metafile 'variable' lines
 *********************************************************************/


void pro_var_lines( cdms_card *line,  /* struct metafile line */
                    cdHd      *mas )  /* above struct */
{
  int       i, j, num_sym;
  cdVar_new *var;
  cdTmp     *tmp;

  if( FPT_OUT != NULL )
     fprintf( FPT_OUT, "***found variables:\n" );

  var = NULL;


 /*---------------------------------
  * Loop lines of variable section
  *---------------------------------*/


  while( line->eof != NULL )
  {


    /*---------------------------------
     * Read a complete metafile line and crack it into symbols
     *---------------------------------*/


     num_sym = rd_meta_line( line );
                  err_r( );


    /*---------------------------------
     * Check if line is first line of next section (ie. 'name:' )
     *---------------------------------*/


     if( (num_sym == 2 && line->cls_sym[0] == 'a'
         && line->cls_sym[1] == ':') || num_sym < 4 )
        return;


    /*---------------------------------
     * Check for attribute definition card
     *---------------------------------*/


     else if( var != NULL && (line->cls_sym[0] == ':'
              || line->cls_sym[1] == ':') )
     {
        var = meta_att( line, (cdHd *) var );
                 err_r( );
     }


    /*---------------------------------
     * If here must be a variable definition card
     *---------------------------------*/


     else
     {
        var = cre_struct( id_cdVar, mas );


       /*---------------------------------
        * Field 1 -- keyword -- mandatory
        * variable datatype   ex. 'double'
        *---------------------------------*/


        if( line->cls_sym[0] != 'a' )
           err_x( "pro_var_lines: variable datatype" );

        j = line->idx_sym[0];

        if( !strcmp( "byte", &line->asc_line[j] ) )
           var->datatype = cdByte;

        else if( !strcmp( "char", &line->asc_line[j] ) )
           var->datatype = cdChar;

        else if( !strcmp( "short", &line->asc_line[j] ) )
           var->datatype = cdShort;

        else if( !strcmp( "int", &line->asc_line[j] ) )
           var->datatype = cdInt;

        else if( !strcmp( "long", &line->asc_line[j] ) )
           var->datatype = cdLong;

        else if( !strcmp( "float", &line->asc_line[j] ) )
           var->datatype = cdFloat;

        else if( !strcmp( "double", &line->asc_line[j] ) )
           var->datatype = cdDouble;

        else if( !strcmp( "longdouble", &line->asc_line[j] ) )
           var->datatype = cdLongDouble;

        else if( !strcmp( "chartime", &line->asc_line[j] ) )
           var->datatype = cdCharTime;

        else
        {
           err_x( "pro_var_lines: variable datatype" );
        }


       /*---------------------------------
        * Field 2 -- keyword or keyword<keyword> -- mandatory
        * variable name,alias   ex. 'humspc<humidity>'
        * NOTE: alias no longer used (see alterfile)
        *---------------------------------*/


        if( line->cls_sym[1] != 'a' )
           err_x( "pro_var_lines: variable name" );

        j = line->idx_sym[1];
        var->name = cr_asc_cpy( &line->asc_line[j] );
        i = 2;


       /*---------------------------------
        * Field 3 -- (dim1,dim2,...) -- mandatory
        * variable dimensions   ex. '(lat,longitude)'
        *---------------------------------*/


        if( line->cls_sym[i] != '(' )
           err_x( "pro_var_lines: variable dims" );

        for( ; i < num_sym; i++ )
        {
           if( line->cls_sym[i] == '(' || line->cls_sym[i] == ',' )
              ;

           else if( line->cls_sym[i] == ')' ||
                    line->cls_sym[i] == 'i' )
              break;

           else
           {
              if( line->cls_sym[i] != 'a' )
                 err_x( "pro_var_lines: var-dim name" );

              j = line->idx_sym[i];
              var->ndims++;

              tmp           = cre_struct( id_cdTmp, (cdHd *) var );
              tmp->id_want  = id_cdDim;
              tmp->nam_want = cr_asc_cpy( &line->asc_line[j] );
           }
        }
     }
  }
  return;
}


/*********************************************************************
 * Function: execute a PQL instruction.
 *********************************************************************/


char *py_psql_execute( long ldb,        /* root database */
                       char msg_ln[] )  /* pql message */
{
  int      i;
  cdDb_new *db;


 /*---------------------------------
  * Set this python-psql database tree structure.
  *---------------------------------*/


  long_to_pt( ldb, db );

  if( db == NULL || db->id != id_cdDb )
     err_xv( "py_psql_execute: missing database tree" );

  DB_ROOT_ADR = db;


 /*---------------------------------
  * Load global flags/scalars from database.
  *---------------------------------*/


  ERR_no               = db->f->ERR_no;
  FPT_OUT              = db->f->FPT_OUT;
  FPT_TTY              = db->f->FPT_TTY;
  TempMount            = db->f->TempMount;
  EDIT_STRUCT_DATA_FLG = db->f->EDIT_STRUCT_DATA_FLG;
  FPT_PY               = db->f->FPT_PY;
  PSQL_MSG             = db->f->PSQL_MSG;
  PY_PSQL_MSG          = db->f->PY_PSQL_MSG;
  L_PY_PSQL_MSG        = db->f->L_PY_PSQL_MSG;

  l_PY_PSQL_MSG  = 0;
  PY_PSQL_MSG[0] = '\0';


 /*---------------------------------
  * Send instruction line to psql.
  *---------------------------------*/


  pql_execute( msg_ln );


 /*---------------------------------
  * Store copy of global flags/scalars in database.
  *---------------------------------*/


  db->f->ERR_no               = ERR_no;
  db->f->FPT_OUT              = FPT_OUT;
  db->f->FPT_TTY              = FPT_TTY;
  db->f->TempMount            = TempMount;
  db->f->EDIT_STRUCT_DATA_FLG = EDIT_STRUCT_DATA_FLG;
  db->f->FPT_PY               = FPT_PY;
  db->f->PSQL_MSG             = PSQL_MSG;
  db->f->PY_PSQL_MSG          = PY_PSQL_MSG;
  db->f->l_PY_PSQL_MSG        = l_PY_PSQL_MSG;
  db->f->L_PY_PSQL_MSG        = L_PY_PSQL_MSG;

  return PY_PSQL_MSG;
}


/*********************************************************************
 * Function: PYTHON initalization of PSQL.
 *********************************************************************/


long py_psql_init( void )
{
  long     ldb;
  cdDb_new *db;


 /*---------------------------------
  * Create default database, etc.
  *---------------------------------*/


  DB_ROOT_ADR = NULL;

  db = init_user_db( );

  pt_to_long( db, ldb );


 /*---------------------------------
  * Store in default database the global flags, scalars, etc.
  *---------------------------------*/


  FPT_PY = 1;

  db->f->ERR_no               = ERR_no;
  db->f->FPT_OUT              = FPT_OUT;
  db->f->FPT_TTY              = FPT_TTY;
  db->f->TempMount            = TempMount;
  db->f->EDIT_STRUCT_DATA_FLG = EDIT_STRUCT_DATA_FLG;
  db->f->FPT_PY               = FPT_PY;
  db->f->PSQL_MSG             = PSQL_MSG;
  db->f->PY_PSQL_MSG          = PY_PSQL_MSG;
  db->f->l_PY_PSQL_MSG        = l_PY_PSQL_MSG;
  db->f->L_PY_PSQL_MSG        = L_PY_PSQL_MSG;

  return ldb;
}


/*********************************************************************
 * Function: User interface to read attribute.
 *********************************************************************/


int qlattget( int   fileid,    /* file ioc */
              int   varid,     /* CU_GLOBAL or variable id */
              char  *name,     /* attribute name */
              void  *values )  /* attribute value */
{
  int       i, k, n;
  char      **vlst;
  cdVar_new *var;
  cdAtt_new *att;
  CuType    typ;


 /*---------------------------------
  * Read attribute from '.cdms' file.
  *---------------------------------*/


  var = ql_var_fnd( fileid, varid, NULL, NULL );
           qlerr_r( );

  if( varid == CU_GLOBAL )
     att = scn_lnk_list( name, id_cdAtt, (cdHd *) var->above );
  else
     att = scn_lnk_list( name, id_cdAtt, (cdHd *) var );

  if( att == NULL )
     qlerr_x( "qlattget: couldn't find attribute" );

  if( att->datatype == cdChar )
     strcpy( values, att->values );
  else if( att->datatype == cdCharTime )
  {
     vlst = att->values;
     strcpy( values, vlst[0] );
  }
  else
  {
     typ_to_cdunif( att->datatype, &typ, &k );
        qlerr_r( );
     n = k * att->length;
     memcpy( values, att->values, n );
  }

  return 0;
}


/*********************************************************************
 * Function: User interface to inquire attribute.
 *********************************************************************/


int qlattinq( int     fileid,     /* file ioc */
              int     varid,      /* CU_GLOBAL or variable id */
              char    *name,      /* attribute name */
              CuType  *datatype,  /* attribute datatype */
              int     *len )      /* attribute length */
{
  int       i, k;
  char      msg[80];
  cdVar_new *var;
  cdAtt_new *att;


 /*---------------------------------
  * Attribute inquire '.cdms' file.
  *---------------------------------*/


  var = ql_var_fnd( fileid, varid, NULL, NULL );
           qlerr_r( );

  if( varid == CU_GLOBAL )
     att = scn_lnk_list( name, id_cdAtt, (cdHd *) var->above );
  else
     att = scn_lnk_list( name, id_cdAtt, (cdHd *) var );

  if( att == NULL )
  {
     sprintf( msg, "qlattinq: couldn't find attribute %s", name );
     qlerr_x( msg );
  }

  if( datatype )
     typ_to_cdunif( att->datatype, datatype, &k );

  if( len )
     *len = att->length;

  return 0;
}


/*********************************************************************
 * Function: User interface to inquire attribute.

 * Note: Attributes whose names start with "psql_" are ignored.
 *********************************************************************/


int qlattname( int   fileid,  /* file ioc */
               int   varid,   /* CU_GLOBAL or variable id */
               int   attnum,  /* attribute number */
               char  *name )  /* attribute name */
{
  int        i, k;
  void       **v;
  cdVar_new  *var;
  cdAtt_new  *att;


 /*---------------------------------
  * Attribute name '.cdms' file.
  *---------------------------------*/


  var = ql_var_fnd( fileid, varid, NULL, NULL );
           qlerr_r( );


 /*---------------------------------
  * Get [dataset,variable] attribute linked list.
  *---------------------------------*/


  if( varid == CU_GLOBAL )
  {
     v = fnd_lnk_list( id_cdAtt, (cdHd *) var->above );

     if( v == NULL || *v == NULL )
        att = NULL;
     else
        att = (cdAtt_new *) *v;
  }
  else
     att = var->atts;

  if( att == NULL )
     qlerr_x( "qlattname: no attributes found" );


 /*---------------------------------
  * Go down linked list.
  *---------------------------------*/


  for( i=-1 ; att; att=att->next )
  {
     if( att->name != NULL && strstr( att->name, "psql_" ) == NULL )
     {
        i++;

        if( i == attnum )
        {
           strcpy( name, att->name );
           return 0;
        }
     }
  }

  name[0] = '\0';
  qlerr_x( "qlattname: couldn't find attribute" );
}


/*********************************************************************
 * Function: User interface to disconnect file.
 *********************************************************************/


int qlclose( int  fileid )  /* file ioc */
{
  int        i;
  cdDset_new *ds;


 /*---------------------------------
  * Disconnect '.cdms' file.
  *---------------------------------*/


  for( i=0, ds=NULL; i < DB_ROOT_ADR->f->L_multi; i++ )
  {
     if( DB_ROOT_ADR->f->multi_ioc[i] == fileid )
     {
        ds = DB_ROOT_ADR->f->multi_ds[i];
        break;
     }
  }

  if( ds == NULL || ds->id != id_cdDset )
     qlerr_x( "qlclose: trouble with multi-file ioc" );

  delete_struct( (cdHd *) ds->above );
     qlerr_r( );

  DB_ROOT_ADR->f->multi_ioc[i] = 0;
  DB_ROOT_ADR->f->multi_ds[i]  = NULL;

  return 0;
}


/*********************************************************************
 * Function: Internal-User-Interface, find dimension in '.cdms' file.

 * (fileid, --, -44, NULL, &n)    to get count of dimensions
 * (fileid, --, -33,  nam, &n)    to find dimension
 * (fileid, --,   8, NULL, NULL)  to get nineth dimension

 * Note: Dimensions whose names start with "psql_" are ignored.

 * Note: multi-file dimensions are now all 'CuGlobaldim'.
 *       Hence this routine shall ignore the 'varid' argument.
 *********************************************************************/


cdDim_new *ql_dim_fnd( int  fileid,   /* file ioc */
                       int  varid,    /* CU_GLOBAL or id of variable */
                       int  dimid,    /* id of dimension */
                                      /* -44 find no. dims,
                                         -33 use name arg. */
                       char *name,    /* dimension name */
                       int  *n_dim )  /* no. dims, or id of dim */
{
  int        i, k;
  char       msg[80];
  cdDset_new *ds;
  cdDim_new  *dim;

  if( n_dim )
     *n_dim = -1;


 /*---------------------------------
  * Find dataset for this ioc in the database tree.
  *---------------------------------*/


  for( i=0, ds=NULL; i < DB_ROOT_ADR->f->L_multi; i++ )
  {
     if( DB_ROOT_ADR->f->multi_ioc[i] == fileid )
     {
        ds = DB_ROOT_ADR->f->multi_ds[i];
        break;
     }
  }

  if( ds == NULL || ds->id != id_cdDset )
     err_xv( "ql_dim_fnd: trouble with multi-file ioc" );


 /*---------------------------------
  * Get count of dimensions in file.
  *---------------------------------*/


  if( dimid == -44 )
  {
     for( dim=ds->dims, k=0; dim; dim=dim->next )
     {
        if( dim->name != NULL && strstr( dim->name, "psql_" ) == NULL )
           k++;
     }

     if( n_dim )
        *n_dim = k;

     return NULL;
  }


 /*---------------------------------
  * Find dimension with given name.
  *---------------------------------*/


  else if( dimid == -33 && name != NULL )
  {
     for( dim=ds->dims, k=-1; dim; dim=dim->next )
     {
        if( dim->name != NULL && strstr( dim->name, "psql_" ) == NULL )
        {
           k++;

           if( !strcmp( dim->name, name ) )
           {
              if( n_dim )
                 *n_dim = k;

              return dim;
           }
        }
     }
  }


 /*---------------------------------
  * Find dimension with given id-number.
  *---------------------------------*/


  else
  {
     for( dim=ds->dims, k=-1; dim; dim=dim->next )
     {
        if( dim->name != NULL && strstr( dim->name, "psql_" ) == NULL )
        {
           k++;

           if( dimid == k )
              return dim;
        }
     }
  }

  if( dimid == -33 && name != NULL )
  {
     sprintf( msg, "ql_dim_fnd: couldn't find dimension %s", name );
     err_xv( msg );
  }

  err_xv( "ql_dim_fnd: trouble with multi-file dimension" );
}


/*********************************************************************
 * Function: User interface to read dimension coordinates.
 *********************************************************************/


int qldimget( int   fileid,    /* file ioc */
              int   dimid,     /* dimension id */
              void  *values )  /* coordinate array */
{
  int       k;
  long      l, n;
  cdDim_new *dim;
  void      *vv;
  CuType    typ;


 /*---------------------------------
  * Read dimension coordinates '.cdms' file.
  *---------------------------------*/


  dim = ql_dim_fnd( fileid, CU_GLOBAL, dimid, NULL, NULL );
           qlerr_r( );

  vv = rd_dim_array( dim, -1, -1 );
          qlerr_r( );

  typ_to_cdunif( dim->datatype, &typ, &k );
     qlerr_r( );

  n = k * dim->length;
  memcpy( values, vv, n );
  free( vv );

  return 0;
}


/*********************************************************************
 * Function: User interface to return dimension id given dimension name.
 *********************************************************************/


int qldimid( int   fileid,  /* file ioc */
             int   varid,   /* CU_GLOBAL or variable id */
             char  *name )  /* dimension name */
{
  int       k;
  cdDim_new *dim;


 /*---------------------------------
  * Inquire '.cdms' file.
  *---------------------------------*/


  dim = ql_dim_fnd( fileid, varid, -33, name, &k );
           qlerr_r( );

  if( dim == NULL || dim->id != id_cdDim )
     qlerr_x( "qldimid: trouble with multi-file dimension" );

  return k;
}


/*********************************************************************
 * Function: User interface to inquire dimension.
 *********************************************************************/


int qldiminq( int        fileid,     /* file ioc */
              int        dimid,      /* dimension id */
              char       *dimname,   /* dimension name */
              char       *dimunits,  /* dimension units or NULL */
              CuType     *datatype,  /* dimension datatype */
              CuDimType  *dimtype,   /* CuGlobalDim or CuLocalDim */
              int        *varid,     /* CU_GLOBAL or variable id */
              long       *length )   /* dimension length */
{
  int       k;
  cdDim_new *dim;
  cdAtt_new *att;


 /*---------------------------------
  * Dimension inquire '.cdms' file.
  *---------------------------------*/


  dim = ql_dim_fnd( fileid, CU_GLOBAL, dimid, NULL, NULL );
           qlerr_r( );

  if( dim == NULL || dim->id != id_cdDim )
     qlerr_x( "qldiminq: unable to find dimension" );


  if( dimname )
  {
     dimname[0] = '\0';

     if( dim->name != NULL )
        strcpy( dimname, dim->name );
  }

  if( dimunits )
  {
     dimunits[0] = '\0';

     att = scn_lnk_list( "units", id_cdAtt, (cdHd *) dim );

     if( att != NULL )
        strcpy( dimunits, (char *) att->values );
  }

  if( datatype )
  {
     typ_to_cdunif( dim->datatype, datatype, &k );
        qlerr_r( );
  }

  if( length )
     *length = dim->length;


 /*---------------------------------
  * Determine variable id that dimension is attached to.
  *---------------------------------*/



  if( dimtype )
     *dimtype = CuGlobalDim;

  if( varid )
     *varid = CU_GLOBAL;

  return 0;
}


/*********************************************************************
 * Function: User interface to inquire a file.

 * Note: Attributes whose names start with "psql_" are ignored.
 *********************************************************************/


int qlinquire( int fileid,    /* file ioc */
               int *ndims,    /* number of file dimensions */
               int *nvars,    /* number of file variables */
               int *natts,    /* number of file attributes */
               int *recdim )  /* -1 the unlimited dimension */
{
  int        i, k, n;
  cdVar_new  *var;
  cdDim_new  *dim;
  cdDset_new *ds;
  cdAtt_new  *att;


 /*---------------------------------
  * Inquire '.cdms' file.
  *---------------------------------*/


  if( ndims )
  {
     dim = ql_dim_fnd( fileid, CU_GLOBAL, -44, NULL, &n );
              qlerr_r( );
     *ndims = n;
  }

  if( nvars )
  {
     var = ql_var_fnd( fileid, -44, NULL, &k );
              qlerr_r( );
     *nvars = k;
  }

  if( natts )
  {
     var = ql_var_fnd( fileid, 0, NULL, NULL );
              qlerr_r( );
     ds = (cdDset_new *) var->above;

     for( att=ds->atts, i=0; att; att=att->next )
     {
        if( att->name != NULL && strstr( att->name, "psql_" ) == NULL )
           i++;
     }

     *natts = i;
  }

  if( recdim )
     *recdim = -1;

  return 0;
}


/*********************************************************************
 * Function: User PSQL Interface.
 *********************************************************************/


int qlintr( char msg_ln[] )  /* pql message */
{
  cdDb_new   *db;

  db = init_user_db( );


 /*---------------------------------
  * Accumulate line until find ';' then execute it.
  *---------------------------------*/


  pql_execute( msg_ln );
     qlerr_r( );

  return 0;
}


/*********************************************************************
 * Function: User interface to open a file.
 *********************************************************************/


int qlopenread( int   fileid,       /* file ioc */
                char *controlpath,  /* dictionary file */
                char *datapath )    /* data file or NULL */
{
  int        i, f, k, n, *multi_ioc;
  char       *aa;
  cdDb_new   *db;
  cdDset_new *ds, **multi_ds;
  cdAtt_new  *att;
/*.................debug...................
  cdDim_new  *dim;
  cdVar_new  *var;
  int        nd, nv, na, nr;
.................debug...................*/

  db = init_user_db( );


 /*---------------------------------
  * Determine my 'ioc' number for this file.
  *---------------------------------*/


  for( i=0, f=-1; i < DB_ROOT_ADR->f->L_multi; i++ )
  {
     if( DB_ROOT_ADR->f->multi_ioc[i] == fileid )
     {
        qlerr_x( "qlopenread: ioc already in use" );
     }

     else if( f == -1 && DB_ROOT_ADR->f->multi_ioc[i] == 0 )
     {
        f            = i;
        DB_ROOT_ADR->f->multi_ioc[i] = fileid;
        DB_ROOT_ADR->f->multi_ds[i]  = NULL;
     }
  }


 /*---------------------------------
  * If no space increase table and assign 'ioc'.
  *---------------------------------*/


  if( f == -1 )
  {
     f         = DB_ROOT_ADR->f->L_multi;
     n         = f + 5;
     multi_ioc = (int *) calloc( 1, n * sizeof(int) );
     multi_ds  = malloc( n * sizeof(ds) );

     for( i=0; i < f; i++ )
     {
        multi_ioc[i] = DB_ROOT_ADR->f->multi_ioc[i];
        multi_ds[i]  = DB_ROOT_ADR->f->multi_ds[i];
     }

     free( DB_ROOT_ADR->f->multi_ioc );
     free( DB_ROOT_ADR->f->multi_ds );

     DB_ROOT_ADR->f->multi_ioc = multi_ioc;
     DB_ROOT_ADR->f->multi_ds  = multi_ds;

     DB_ROOT_ADR->f->multi_ioc[f] = fileid;
     DB_ROOT_ADR->f->multi_ds[f]  = NULL;
     DB_ROOT_ADR->f->L_multi      = n;
  }


 /*---------------------------------
  * Read metafile, create in-memory tree structure.
  *---------------------------------*/


  k = rd_meta_file( controlpath );
         qlerr_r( );


 /*---------------------------------
  * Set dataset to use for this ioc.
  *---------------------------------*/


  for( db=DB_ROOT_ADR; db->next != NULL; db=db->next )
     ;

  for( ds=db->dsets; ds->next != NULL; ds=ds->next )
     ;

  if( ds == NULL || ds->vars == NULL )
     qlerr_x( "qlopenread: no multi-file variables" );

  DB_ROOT_ADR->f->multi_ds[f] = ds;

/*.................debug...................
  k = qlinquire( fileid, &nd, &nv, &na, &nr );
  printf( "%d %d %d %d quinquire\n", nd, nv, na, nr );
  for( dim=ds->dims; dim; dim=dim->next )
     printf( "dim       %s\n", dim->name );
  for( var=ds->vars; var; var=var->next )
     printf( "variable %s\n", var->name );
.................debug...................*/

  return 0;
}


/*********************************************************************
 * Function: User interface to set error response of 'ql' routines.

 * Flag numbering for how PSQL is to respond on a detected error: 
 *      1 -- terminate code on error;
 *      2 -- return -1; print error messages. (default)
 *      3 -- return -1; don't print error messages.

 * Note: normal, non-error return of 'ql' routines is integer 0.
 *********************************************************************/


int qlseterropts( int  err_flg )  /* error flag */
{
  cdDb_new *db;


 /*---------------------------------
  * Store number in user's default database.
  *---------------------------------*/


  db = init_user_db( );

  if( DB_ROOT_ADR != NULL )
     DB_ROOT_ADR->f->qlerrflg = err_flg;

  return 0;
}


/*********************************************************************
 * Function: Internal-User-Interface, find variable in '.cdms' file.

 * (fileid, -44, NULL, &n)    to get count of variables
 * (fileid, -33,  nam, &n)    to find variable
 * (fileid,   8, NULL, NULL)  to get nineth variable

 * Note: Variables whose names start with "psql_" are ignored.
 *********************************************************************/


cdVar_new *ql_var_fnd( int  fileid,   /* file ioc */
                       int  varid,    /* id of variable */
                                      /* CU_GLOBAL same as 0,
                                         -44 find no. multi-f-vars,
                                         -33 use name arg. */
                       char *name,    /* variable name */
                       int  *n_var )  /* no. vars, or id of var */
{
  int        i, k;
  char       msg[80];
  cdDset_new *ds;
  cdVar_new  *var;

  if( n_var )
     *n_var = -1;

  if( varid == CU_GLOBAL )
     varid = 0;


 /*---------------------------------
  * Find dataset for this ioc in the database tree.
  *---------------------------------*/


  for( i=0, ds=NULL; i < DB_ROOT_ADR->f->L_multi; i++ )
  {
     if( DB_ROOT_ADR->f->multi_ioc[i] == fileid )
     {
        ds = DB_ROOT_ADR->f->multi_ds[i];
        break;
     }
  }

  if( ds == NULL || ds->id != id_cdDset )
     err_xv( "ql_var_fnd: trouble with multi-file ioc" );


 /*---------------------------------
  * Get count of variables in file.
  *---------------------------------*/


  if( varid == -44 )
  {
     for( var=ds->vars, k=0; var; var=var->next )
     {
        if( var->name != NULL && strstr( var->name, "psql_" ) == NULL )
           k++;
     }

     if( n_var )
        *n_var = k;

     return NULL;
  }


 /*---------------------------------
  * Find variable with given name.
  *---------------------------------*/


  else if( varid == -33 && name != NULL )
  {
     for( var=ds->vars, k=-1; var; var=var->next )
     {
        if( var->name != NULL && strstr( var->name, "psql_" ) == NULL )
        {
           k++;

           if( !strcmp( var->name, name ) )
           {
              if( n_var )
                 *n_var = k;

              return var;
           }
        }
     }
  }


 /*---------------------------------
  * Find variable with given id-number.
  *---------------------------------*/


  else
  {
     for( var=ds->vars, k=-1; var; var=var->next )
     {
        if( var->name != NULL && strstr( var->name, "psql_" ) == NULL )
        {
           k++;

           if( varid == k )
              return var;
        }
     }
  }

  if( varid == -33 && name != NULL )
  {
     sprintf( msg, "ql_var_fnd: couldn't find variable %s", name );
     err_xv( msg );
  }

  err_xv( "ql_var_fnd: trouble with multi-file variable" );
}


/*********************************************************************
 * Function: User interface to read variable.
 *********************************************************************/


int qlvarget( int   fileid,   /* file ioc */
              int   varid,    /* variable id */
              long  start[],  /* start dimension index's */
              long  count[],  /* dimension read lengths */
              void  *value )  /* data array */
{
  int       i, k, n;
  long      l, d1_bgn, d1_cnt, d1_inc, idx, len, *lbuf;
  cdVar_new *var;
  cdAtt_new *att;
  void      *vv, *dd;
  CuType    dtype;


 /*---------------------------------
  * Find database struct of variable.
  *---------------------------------*/


  var = ql_var_fnd( fileid, varid, NULL, NULL );
           qlerr_r( );


 /*---------------------------------
  * Determine read sections on variable first dimension (C order).
  * Required so 'alterfile' logic can have scratch memory.
  * Example: read array so big 2 cann't fit in memory.
  *---------------------------------*/


  att = scn_lnk_list( "lenreadbuf", id_cdAtt, (cdHd *) DB_ROOT_ADR );
  if( att == NULL )
     qlerr_x( "qlvarget: code logic error" );

  d1_bgn = start[0];
  d1_cnt = count[0];

  for( i=1, l=1; i < var->ndims; i++ )
     l *= count[i];

  lbuf   = att->values;
  d1_inc = *lbuf / l;
  
  if( d1_inc < 1 )
     d1_inc = 1;
  else if( d1_inc > d1_cnt )
     d1_inc = d1_cnt;


 /*---------------------------------
  * Loop over axis sections doing multi-file reads.
  *---------------------------------*/

  idx = 0;
  len = 0;

  while( len < d1_cnt )
  {
     k = d1_cnt - len;
     if( k > d1_inc )
        k = d1_inc;
     count[0] = k;

     vv = rd_var_array( var, start, count );
             qlerr_r( );

     dd = ary_off( var->datatype, idx, value );
             qlerr_r( );

     for( i=0, l=1; i < var->ndims; i++ )
        l *= count[i];

     typ_to_cdunif( var->datatype, &dtype, &n );
        qlerr_r( );

     memcpy( dd, vv, l * n );
     free( vv );
     
     idx += l;
     len += k;
     start[0] = start[0] + k;
  }

  start[0] = d1_bgn;
  count[0] = d1_cnt;

  return 0;
}


/*********************************************************************
 * Function: User interface to return variable id given name.
 *********************************************************************/


int qlvarid( int  fileid,  /* file ioc */
             char *name )  /* variable name */
{
  int       k;
  cdVar_new *var;


 /*---------------------------------
  * Variable id from '.cdms' file.
  *---------------------------------*/


  var = ql_var_fnd( fileid, -33, name, &k );
           qlerr_r( );

  if( var == NULL || var->id != id_cdVar )
     qlerr_x( "qlvarid: trouble with multi-file variable" );

  return k;
}


/*********************************************************************
 * Function: User interface to variable inquire.

 * Note: Attributes whose names start with "psql_" are ignored.
 *********************************************************************/


int qlvarinq( int     fileid,     /* file ioc */
              int     varid,      /* variable id */
              char    *name,      /* variable name */
              CuType  *datatype,  /* variable datatype */
              int     *ndims,     /* number of dimensions */
              int     dimids[],   /* dimension id's */
              int     *natts )    /* number of attributes */
{
  int       i, j, k, n;
  cdVar_new *var;
  cdAtt_new *att;
  cdDim_new *dim, *dima;
  cdTmp     *tmp;


 /*---------------------------------
  * Variable inquire '.cdms' file.
  *---------------------------------*/


  var = ql_var_fnd( fileid, varid, NULL, NULL );
           qlerr_r( );

  if( var == NULL || var->id != id_cdVar )
     qlerr_x( "qlvarinq: trouble with multi-file variable" );

  if( name )
  {
     name[0] = '\0';

     if( var->name != NULL )
        strcpy( name, var->name );
  }

  if( datatype )
  {
     typ_to_cdunif( var->datatype, datatype, &k );
        qlerr_r( );
  }

  if( ndims )
     *ndims = var->ndims;

  if( dimids != NULL )
  {
     for( tmp=var->dim, j=0; j < var->ndims; tmp=tmp->next, j++ )
     {
        dima = tmp->want;

        dim = ql_dim_fnd( fileid, CU_GLOBAL, -33, dima->name,
                          &dimids[j] );
                 qlerr_r( );
     }
  }

  if( natts )
  {
     for( att=var->atts, i=0; att; att=att->next )
     {
        if( att->name != NULL && strstr( att->name, "psql_" ) == NULL )
           i++;
     }

     *natts = i;
  }

  return 0;
}


/*********************************************************************
 * Function: execute alter attributes:
 *    wrttype, newmiss, wrtmult, wrtadd, wrtinvert
 *********************************************************************/


void rd_alt_dim_mod( dim_pql_read *rd,     /* dimension read struct */
                     char         *name )  /* alter command */
{
  int       j, k;
  long      i, l, *idx;
  cdDim_new *dim;
  cdAtt_new *att;
  double    *dd, *ss, pt1, pt2, tol, val;
  void      *v;

  dim = rd->dim;


 /*---------------------------------
  * Check if 'wrtinvert' transposing of dimension.
  *---------------------------------*/


  if( !strcmp( name, "psql_wrtinvert" ) )
  {
     att = scn_lnk_list( "psql_wrtinvert", id_cdAtt, (cdHd *) dim );

     if( att != NULL )
     {
        if( strcmp( (char *) att->values, "nodim" ) && rd->count > 1 )
        {
           idx = (long *) malloc( rd->count * sizeof( long ) );
           if( idx == NULL )
              err_x( "rd_alt_dim_mod: trouble getting memory" );

           for( i=0, k=rd->count-1; i < rd->count; i++ )
              idx[i] = k--;

           rd_alt_dim_tran( rd, idx );
              err_r( );

           free( idx );
        }
     }

     return;
  }


 /*---------------------------------
  * Check if 'wrttype' changing type of dimiable.
  *---------------------------------*/


  if( !strcmp( name, "psql_wrttype" ) )
  {
     if( rd->dtyp == dim->datatype )
        return;

     v = ary_trans( rd->dtyp, rd->count, rd->data, dim->datatype );
            err_r( );

     free( rd->data );

     rd->data = v;
     rd->dtyp = dim->datatype;

     return;
  }


 /*---------------------------------
  * Check if 'psql_newmiss' changing type of dimiable.
  *---------------------------------*/


  if( !strcmp( name, "psql_newmiss" ) )
  {
     att = scn_lnk_list( "psql_newmiss", id_cdAtt, (cdHd *) dim );

     if( att == NULL )
        return;

     ss = ary_trans( att->datatype, att->length, att->values,
                     cdDouble );
             err_r( );

     dd = ary_trans( rd->dtyp, rd->count, rd->data, cdDouble );
             err_r( );
     free( rd->data );
     rd->data = NULL;
     k = 0;

     while( k+1 < att->length )
     {
        tol = 1.0e-6;
        if( k+2 < att->length )
           tol = ss[k+2];

        pt1 = ss[k] - ( tol * fabs(ss[k]) );
        pt2 = ss[k] + ( tol * fabs(ss[k]) );
        val = ss[k+1];

        for( i=0; i < rd->count; i++ )
        {
           if( dd[i] > pt1 && dd[i] < pt2 )
              dd[i] = val;
        }

        k += 3;
     }

     free( ss );

     rd->data = ary_trans( cdDouble, rd->count, dd, rd->dtyp );
                   err_r( );
     free( dd );

     return;
  }


 /*---------------------------------
  * Check if 'psql_wrtmult' changing type of dimiable.
  *---------------------------------*/


  if( !strcmp( name, "psql_wrtmult" ) )
  {
     att = scn_lnk_list( "psql_wrtmult", id_cdAtt, (cdHd *) dim );

     if( att == NULL )
        return;

     ss = ary_trans( att->datatype, att->length, att->values,
                     cdDouble );
             err_r( );

     pt1 = 1.e+20 - (1.e-6 * 1.e+20);
     pt2 = 1.e+20 + (1.e-6 * 1.e+20);
     val = ss[0];
     free( ss );

     dd = ary_trans( rd->dtyp, rd->count, rd->data, cdDouble );
             err_r( );
     free( rd->data );
     rd->data = NULL;

     for( i=0; i < rd->count; i++ )
     {
        if( dd[i] < pt1 || dd[i] > pt2 )
           dd[i] *= val; 
     }

     rd->data = ary_trans( cdDouble, rd->count, dd, rd->dtyp );
                   err_r( );
     free( dd );

     return;
  }


 /*---------------------------------
  * Check if 'psql_wrtadd' changing type of dimiable.
  *---------------------------------*/


  if( !strcmp( name, "psql_wrtadd" ) )
  {
     att = scn_lnk_list( "psql_wrtadd", id_cdAtt, (cdHd *) dim );

     if( att == NULL )
        return;

     ss = ary_trans( att->datatype, att->length, att->values,
                     cdDouble );
             err_r( );

     pt1 = 1.e+20 - (1.e-6 * 1.e+20);
     pt2 = 1.e+20 + (1.e-6 * 1.e+20);
     val = ss[0];
     free( ss );

     dd = ary_trans( rd->dtyp, rd->count, rd->data, cdDouble );
             err_r( );
     free( rd->data );
     rd->data = NULL;

     for( i=0; i < rd->count; i++ )
     {
        if( dd[i] < pt1 || dd[i] > pt2 )
           dd[i] += val; 
     }

     rd->data = ary_trans( cdDouble, rd->count, dd, rd->dtyp );
                   err_r( );
     free( dd );

     return;
  }
  return;
}


/*********************************************************************
 * Function:  Transpose dimension read from disc.
 *********************************************************************/


void rd_alt_dim_tran( dim_pql_read *rd,    /* pql read */
                      long         *idx )  /* index array */
{
  int           i, j, n, len, *in_i, *o_i;
  unsigned char *in_b, *o_b;
  short         *in_s, *o_s;
  long          *in_l, *o_l;
  float         *in_f, *o_f;
  double        *in_d, *o_d;
  void          *ary1, *ary2;
  CuType        dtype;
  cdType        typ;


 /*---------------------------------
  * Get memory for transposed dimension.
  *---------------------------------*/


  typ_to_cdunif( rd->dtyp, &dtype, &n );
     err_r( );

  ary1 = malloc( rd->count * n );
  if( ary1 == NULL )
     err_x( "rd_alt_dim_tran: trouble getting memory" );

  ary2 = rd->data;
  typ  = rd->dtyp;
  len  = rd->count;


 /*---------------------------------
  * Transpose ary2 to ary1.
  *---------------------------------*/


  if( typ == cdByte )
  {
     o_b  = (unsigned char *) ary1;
     in_b = (unsigned char *) ary2;

     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_b[i] = in_b[j];
     }
  }

  else if( typ == cdShort )
  {
     o_s  = (short *) ary1;
     in_s = (short *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_s[i] = in_s[j];
     }
  }

  else if( typ == cdInt )
  {
     o_i  = (int *) ary1;
     in_i = (int *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_i[i] = in_i[j];
     }
  }

  else if( typ == cdLong )
  {
     o_l  = (long *) ary1;
     in_l = (long *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_l[i] = in_l[j];
     }
  }

  else if( typ == cdFloat )
  {
     o_f  = (float *) ary1;
     in_f = (float *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_f[i] = in_f[j];
     }
  }

  else if( typ == cdDouble )
  {
     o_d  = (double *) ary1;
     in_d = (double *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_d[i] = in_d[j];
     }
  }

  else if( typ == cdLongDouble )
  {
     o_d  = (double *) ary1;
     in_d = (double *) ary2;

     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_d[i] = in_d[j];
     }
  }

  else
  {
     err_x( "rd_alt_dim_tran: bad array type" );
  }


  free( rd->data );
  rd->data = ary1;

  return;
}


/*********************************************************************
 * Function to process alterfile into memory structs
 *********************************************************************/


int rd_alter_file( char *filename )  /* name-with-path input metafile */
{
  cdms_card *line;
  int       i, j, k, n, num_sym, len_str;
  char      *aa, *bb;
  cdCheck   *ck, *cka;


 /*---------------------------------
  * Erase any previous alter file from memory.
  *---------------------------------*/


  rel_check( NULL );


 /*---------------------------------
  * Open ascii metafile
  *---------------------------------*/


  mem_cdms_card( &line );
     err_ri( );

  line->fp = fopen( filename, "r" );
  if( line->fp == NULL )
     err_xi( "rd_alter_file: trouble opening input file" );

  if( FPT_OUT != NULL )
     fprintf( FPT_OUT, "***alterfile: %s\n", filename );


 /*---------------------------------
  * Loop over ascii lines in alterfile
  *---------------------------------*/


  while( (line->eof = fgets( line->asc_line, line->L_asc_line - 5,
          line->fp )) != NULL )
  {
    /*---------------------------------
     * Crack line into symbols.
     *---------------------------------*/


     if( FPT_OUT != NULL )
        fprintf( FPT_OUT, "%s", line->asc_line );

     len_str        = strlen( line->asc_line );
     bb             = cr_asc_cpy( line->asc_line );
     line->num_sym  = 0;

     num_sym = meta_str_sym( line, 0, len_str );
     ins_asym_eos( line );

     if( num_sym < 1 )
     {
        if( len_str > 0 )
           free( bb );

        continue;
     }


    /*---------------------------------
     * Check for a 'name:' directive type card.
     *---------------------------------*/

     
     if( num_sym == 2 && line->cls_sym[0] == 'a'
         && line->cls_sym[1] == ':' )
     {
        i  = line->idx_sym[0];
        aa = &line->asc_line[i];
        n  = 0;
        free( bb );


       /*---------------------------------
        * Look for dataset:, dimension:, variable:, or attribute:
        *---------------------------------*/


        if( !strcmp( "dimension", aa ) )
           n = id_cdDim;
        else if( !strcmp( "variable", aa ) )
           n = id_cdVar;
        else if( !strcmp( "attribute", aa ) )
           n = id_cdAtt;
        else if( !strcmp( "dataset", aa ) )
           n = id_cdDset;
        else                                    /* assume 'end:' card */
        {
           rel_cdms_card( line );

           return 0;
        }


       /*---------------------------------
        * Create check struct and place at end of list.
        *---------------------------------*/


        ck = mem_check( NULL, 10, 0, 0 );
           err_ri( );

        ck->id = n;
     }


    /*---------------------------------
     * Check for a ':name' modification type card.
     *---------------------------------*/


     else if( num_sym > 1 && line->cls_sym[0] == ':'
              && line->cls_sym[1] == 'a' )
     {
        i  = line->idx_sym[1];
        aa = &line->asc_line[i];
        j  = line->idx_sym[2];


       /*---------------------------------
        * :name
        *---------------------------------*/
        if( !strcmp( "name", aa ) && ck->name == NULL )
        {
           ck->name = cr_asc_cpy( &line->asc_line[j] );
           free( bb );
        }


       /*---------------------------------
        * :tagname
        *---------------------------------*/
        else if( !strcmp( "tagname", aa ) && ck->tagname == NULL )
        {
           ck->tagname = cr_asc_cpy( &line->asc_line[j] );
           free( bb );
        }


       /*---------------------------------
        * Add card to list.
        *---------------------------------*/


        else
        {
           k = ck->ncard + 3;

           if( k > ck->L_card )
           {
              cka = ck;
              ck = mem_check( cka, k+12, 0, 0 );
                 err_ri( );
           }

           n           = ck->ncard++;
           ck->card[n] = bb;
        }
     }


    /*---------------------------------
     * Add card to list (assume continuation of ':values' card).
     *---------------------------------*/


     else
     {
        k = ck->ncard + 3;

        if( k > ck->L_card )
        {
           cka = ck;
           ck = mem_check( cka, k+12, 0, 0 );
              err_ri( );
        }

        n           = ck->ncard++;
        ck->card[n] = bb;
     }
  }

  line->fp = NULL;

  rel_cdms_card( line );

  return 0;
}


/*********************************************************************
 * Function: execute alter attributes:
 *    wrttype, newmiss, wrtmult, wrtadd, wrtinvert, wrtshape
 *********************************************************************/


void rd_alt_var_mod( var_pql_read *rd,     /* variable read struct */
                     char         *name )  /* alter command */
{
  int       j, k;
  long      i, l, *idx;
  cdVar_new *var;
  cdAtt_new *att;
  double    *dd, *ss, pt1, pt2, tol, val;
  void      *v;

  var = rd->var;


 /*---------------------------------
  * Check if 'wrtinvert' transposing of variable.
  *---------------------------------*/


  if( !strcmp( name, "psql_wrtinvert" ) )
  {
     idx = rd_alt_var_ridx( rd );
              err_r( );

     if( idx == NULL )
        return;

     rd_alt_var_tran( rd, idx );
        err_r( );

     free( idx );

     return;
  }


 /*---------------------------------
  * Check if 'wrtshape' transposing of variable.
  *---------------------------------*/


  if( !strcmp( name, "psql_wrtshape" ) )
  {
     idx = rd_alt_var_tidx( rd );
              err_r( );

     if( idx == NULL )
        return;

     rd_alt_var_tran( rd, idx );
        err_r( );

     free( idx );

     return;
  }


 /*---------------------------------
  * Check if 'wrttype' changing type of variable.
  *---------------------------------*/


  if( !strcmp( name, "psql_wrttype" ) )
  {
     if( rd->vtyp == var->datatype )
        return;

     v = ary_trans( rd->vtyp, rd->vlen, rd->data, var->datatype );
            err_r( );

     free( rd->data );

     rd->data = v;
     rd->vtyp = var->datatype;

     return;
  }


 /*---------------------------------
  * Check if 'psql_newmiss' changing type of variable.
  *---------------------------------*/


  if( !strcmp( name, "psql_newmiss" ) )
  {
     att = scn_lnk_list( "psql_newmiss", id_cdAtt, (cdHd *) var );

     if( att == NULL )
        return;

     ss = ary_trans( att->datatype, att->length, att->values,
                     cdDouble );
             err_r( );

     dd = ary_trans( rd->vtyp, rd->vlen, rd->data, cdDouble );
             err_r( );
     free( rd->data );
     rd->data = NULL;
     k = 0;

     while( k+1 < att->length )
     {
        tol = 1.0e-6;
        if( k+2 < att->length )
           tol = ss[k+2];

        pt1 = ss[k] - ( tol * fabs(ss[k]) );
        pt2 = ss[k] + ( tol * fabs(ss[k]) );
        val = ss[k+1];

        for( i=0; i < rd->vlen; i++ )
        {
           if( dd[i] > pt1 && dd[i] < pt2 )
              dd[i] = val;
        }

        k += 3;
     }

     free( ss );

     rd->data = ary_trans( cdDouble, rd->vlen, dd, rd->vtyp );
                   err_r( );
     free( dd );

     return;
  }


 /*---------------------------------
  * Check if 'psql_wrtmult' changing type of variable.
  *---------------------------------*/


  if( !strcmp( name, "psql_wrtmult" ) )
  {
     att = scn_lnk_list( "psql_wrtmult", id_cdAtt, (cdHd *) var );

     if( att == NULL )
        return;

     ss = ary_trans( att->datatype, att->length, att->values,
                     cdDouble );
             err_r( );

     pt1 = 1.e+20 - (1.e-6 * 1.e+20);
     pt2 = 1.e+20 + (1.e-6 * 1.e+20);
     val = ss[0];
     free( ss );

     dd = ary_trans( rd->vtyp, rd->vlen, rd->data, cdDouble );
             err_r( );
     free( rd->data );
     rd->data = NULL;

     for( i=0; i < rd->vlen; i++ )
     {
        if( dd[i] < pt1 || dd[i] > pt2 )
           dd[i] *= val; 
     }

     rd->data = ary_trans( cdDouble, rd->vlen, dd, rd->vtyp );
                   err_r( );
     free( dd );

     return;
  }


 /*---------------------------------
  * Check if 'psql_wrtadd' changing type of variable.
  *---------------------------------*/


  if( !strcmp( name, "psql_wrtadd" ) )
  {
     att = scn_lnk_list( "psql_wrtadd", id_cdAtt, (cdHd *) var );

     if( att == NULL )
        return;

     ss = ary_trans( att->datatype, att->length, att->values,
                     cdDouble );
             err_r( );

     pt1 = 1.e+20 - (1.e-6 * 1.e+20);
     pt2 = 1.e+20 + (1.e-6 * 1.e+20);
     val = ss[0];
     free( ss );

     dd = ary_trans( rd->vtyp, rd->vlen, rd->data, cdDouble );
             err_r( );
     free( rd->data );
     rd->data = NULL;

     for( i=0; i < rd->vlen; i++ )
     {
        if( dd[i] < pt1 || dd[i] > pt2 )
           dd[i] += val; 
     }

     rd->data = ary_trans( cdDouble, rd->vlen, dd, rd->vtyp );
                   err_r( );
     free( dd );

     return;
  }
  return;
}


/*********************************************************************
 * Function: generate index array to invert read variable subset.

 * Note: multi-file read brought in variable in disc dim order.
 *       Index inverts dimensions of variable in disc dim order.
 *********************************************************************/


long *rd_alt_var_ridx( var_pql_read *rd )   /* pql read */

{
  int       i, j, k;
  long      l, n, bgn, lsec, nsec, inc, l1, l2, *index,
            *diminc, *dimlen, *dimbgn;
  cdDim_new *dim;
  cdAtt_new *att;


 /*---------------------------------
  * Get memory for output index array.
  *---------------------------------*/


  index  = (long *) malloc( rd->vlen * sizeof( long ) );
  diminc = (long *) malloc( rd->ndims * sizeof( long ) );
  dimlen = (long *) malloc( rd->ndims * sizeof( long ) );
  dimbgn = (long *) malloc( rd->ndims * sizeof( long ) );

  if( index == NULL || diminc == NULL || dimlen == NULL
      || dimbgn == NULL )
     err_xv( "rd_alt_var_ridx: trouble getting memory" );


 /*---------------------------------
  * Determine if variable needs to be transposed.
  *---------------------------------*/


  k = 0;
  l = 1;

  for( i=rd->ndims-1, l=1; i > -1; i-- )
  {
     dimbgn[i] = 0;
     diminc[i] = l;
     dimlen[i] = rd->count[i];

     dim = rd->dims[i];

     att = scn_lnk_list( "psql_wrtinvert", id_cdAtt, (cdHd *) dim );

     if( att != NULL )
     {
        if( strcmp( (char *) att->values, "dimonly" ) )
        {
           diminc[i] *= -1;
           dimbgn[i] = l;
           k         = 5;

           if( dimlen[i] > 1 )
              dimbgn[i] = (dimlen[i] - 1) * l;
        }
     }

     l *= dimlen[i];
  }

/*
  if( k == 0 )
  {
     free( index );
     free( diminc );
     free( dimlen );
     free( dimbgn );
     return NULL;
  }
*/


 /*---------------------------------
  * Loop over dimensions in reverse (ie. memory) order.
  * That is last dimension first.  (in C (i,j,k) k varies first)
  *---------------------------------*/


  index[0] = 0;
  lsec     = 1;

  for( i=rd->ndims-1; i > -1; i-- )
  {
     nsec = dimlen[i];
     inc  = diminc[i];
     bgn  = dimbgn[i];

     l1  = 0;
     l2  = lsec;


    /*---------------------------------
     * Initalize first section.
     *---------------------------------*/
     for( k=0; k < lsec; k++ )
     {
        index[k] += bgn;
     }


    /*---------------------------------
     * Loop over number of sections.
     *---------------------------------*/
     for( j=1; j < nsec; j++ )
     {


       /*---------------------------------
        * Loop over elements of section.
        *---------------------------------*/
        for( k=0; k < lsec; k++ )
        {
           index[l2++] = index[l1++] + inc;
        }
     }

     lsec *= nsec;
  }


 /*---------------------------------
  * Free memory.
  *---------------------------------*/


  free( diminc );
  free( dimlen );
  free( dimbgn );

  return index;
}


/*********************************************************************
 * Function: generate index array to transpose read variable subset.

 * Note: multi-file read brought in variable in disc dim order.
 *       Index array converts variable to output dim order.
 *********************************************************************/


long *rd_alt_var_tidx( var_pql_read *rd )   /* pql read */

{
  int       i, j, k, n, *ord;
  long      l, lsec, nsec, inc, l1, l2, *index, *diminc, *dimlen;
  cdAtt_new *att;


 /*---------------------------------
  * Determine if variable needs to be transposed.
  *---------------------------------*/


  k = 0;

  if( rd->dorder != NULL )
  {
     for( i=0, j=0; i < rd->ndims; i++ )
     {
        if( rd->dorder[i] != -1 )
        {
           if( rd->dorder[i] < j )
              k = 5;
           j = rd->dorder[i];
        }
     }
  }

  if( k == 0 )
     return NULL;


 /*---------------------------------
  * Get memory for output index array.
  *---------------------------------*/


  n = rd->var->ndims;

  index  = (long *) malloc( rd->vlen * sizeof( long ) );
  diminc = (long *) malloc( rd->ndims * sizeof( long ) );
  dimlen = (long *) malloc( rd->ndims * sizeof( long ) );
  ord    = (int *) malloc( n * sizeof( int ) );

  if( index == NULL || diminc == NULL || dimlen == NULL
     || ord == NULL )
     err_xv( "rd_alt_var_tidx: trouble getting memory" );


 /*---------------------------------
  * Determine transposing dimension order.
  *---------------------------------*/


  for( i=0; i < n; i++ )
     ord[i] = -1;

  if( rd->dorder != NULL )
  {
     for( i=0; i < rd->ndims; i++ )
     {
        j = rd->dorder[i];

        if( j != -1 )
           ord[j] = i;
     }
  }

  for( i=0, j=0; i < n; i++ )
  {
     if( ord[i] != -1 )
        ord[j++] = ord[i];
  }


 /*---------------------------------
  * Generate per-plane element-increment numbers.
  * Note: In 'C' if var shape is (4,3,2) then diminc is (6,2,1).
  *---------------------------------*/


  for( i=rd->ndims-1, l=1; i > -1; i-- )
  {
     diminc[i] = l;
     dimlen[i] = rd->count[i];
     l         *= rd->count[i];
  }


 /*---------------------------------
  * Loop over dimensions in reverse (ie. memory) order.
  * That is last dimension first.  (in C (i,j,k) k varies first)
  *---------------------------------*/


  index[0] = 0;
  lsec     = 1;

  for( i=rd->ndims-1; i > -1; i-- )
  {


    /*---------------------------------
     * Find the new (ie. transposed) position of dimension.
     *---------------------------------*/
     nsec = inc = -1;

     j = ord[i];

     if( j != -1 )
     {
        nsec = dimlen[j];
        inc  = diminc[j];
     }

     if( nsec == -1 )
        err_xv( "rd_alt_var_tidx: bad wrtshape in alterfile" );

     l1  = 0;
     l2  = lsec;


    /*---------------------------------
     * Loop over number of sections.
     *---------------------------------*/
     for( j=1; j < nsec; j++ )
     {


       /*---------------------------------
        * Loop over elements of section.
        *---------------------------------*/
        for( k=0; k < lsec; k++ )
        {
           index[l2++] = index[l1++] + inc;
        }
     }

     lsec *= nsec;
  }


 /*---------------------------------
  * Free memory.
  *---------------------------------*/


  free( diminc );
  free( dimlen );
  free( ord );

  return index;
}


/*********************************************************************
 * Function:  Transpose variable read from disc.
 *********************************************************************/


void rd_alt_var_tran( var_pql_read *rd,    /* pql read */
                      long         *idx )  /* index array */
{
  int           i, j, n, len, *in_i, *o_i;
  unsigned char *in_b, *o_b;
  short         *in_s, *o_s;
  long          *in_l, *o_l;
  float         *in_f, *o_f;
  double        *in_d, *o_d;
  void          *ary1, *ary2;
  CuType        dtype;
  cdType        typ;


 /*---------------------------------
  * Get memory for transposed variable.
  *---------------------------------*/


  typ_to_cdunif( rd->vtyp, &dtype, &n );
     err_r( );

  ary1 = malloc( rd->vlen * n );
  if( ary1 == NULL )
     err_x( "rd_alt_var_tran: trouble getting memory" );

  ary2 = rd->data;
  typ  = rd->vtyp;
  len  = rd->vlen;


 /*---------------------------------
  * Transpose ary2 to ary1.
  *---------------------------------*/


  if( typ == cdByte )
  {
     o_b  = (unsigned char *) ary1;
     in_b = (unsigned char *) ary2;

     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_b[i] = in_b[j];
     }
  }

  else if( typ == cdShort )
  {
     o_s  = (short *) ary1;
     in_s = (short *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_s[i] = in_s[j];
     }
  }

  else if( typ == cdInt )
  {
     o_i  = (int *) ary1;
     in_i = (int *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_i[i] = in_i[j];
     }
  }

  else if( typ == cdLong )
  {
     o_l  = (long *) ary1;
     in_l = (long *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_l[i] = in_l[j];
     }
  }

  else if( typ == cdFloat )
  {
     o_f  = (float *) ary1;
     in_f = (float *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_f[i] = in_f[j];
     }
  }

  else if( typ == cdDouble )
  {
     o_d  = (double *) ary1;
     in_d = (double *) ary2;
 
     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_d[i] = in_d[j];
     }
  }

  else if( typ == cdLongDouble )
  {
     o_d  = (double *) ary1;
     in_d = (double *) ary2;

     for( i=0; i < len; i++ )
     {
        j      = idx[i];
        o_d[i] = in_d[j];
     }
  }

  else
  {
     err_x( "rd_alt_var_tran: bad array type" );
  }


  free( ary2 );
  rd->data = ary1;

  return;
}


/*********************************************************************
 * Function to process checkfile into memory structs
 *********************************************************************/


int rd_check_file( char *filename )  /* name-with-path input metafile */
{
  cdms_card *line;
  int       i, j, k, n, num_sym, len_str;
  char      *aa, *bb, **carda;
  cdDb_new  *db;
  cdCheck   *ck, *cka;
  cdHd      *hd;


 /*---------------------------------
  * Create a print file; Create first database as user scratch.
  *---------------------------------*/


  db = init_user_db( );


 /*---------------------------------
  * Erase previous check file.
  *---------------------------------*/


  if( db->ckdefs != NULL )
  {
     for( cka=db->ckdefs; cka; )
     {
        ck  = cka;
        cka = cka->next;  /* point to next before release */

        if( ck->name != NULL )
           free( ck->name );

        for( i=0; i < ck->nalias; i++ )
           free( ck->alias[i] );

        if( ck->tagname != NULL )
           free( ck->tagname );

        for( i=0; i < ck->ncard; i++ )
           free( ck->card[i] );

        free( ck->alias );
        free( ck->card );
        free( ck->fstruct );
        free( ck );
     }

     db->ckdefs = NULL;
  }


 /*---------------------------------
  * Open ascii metafile
  *---------------------------------*/


  mem_cdms_card( &line );
     err_ri( );

  line->fp = fopen( filename, "r" );
  if( line->fp == NULL )
     err_xi( "rd_check_file: trouble opening input file" );

  if( FPT_OUT != NULL )
     fprintf( FPT_OUT, "***checkfile: %s\n", filename );


 /*---------------------------------
  * Loop over ascii lines in checkfile
  *---------------------------------*/


  while( (line->eof = fgets( line->asc_line, 120, line->fp )) != NULL )
  {
    /*---------------------------------
     * Crack line into symbols.
     *---------------------------------*/


     if( FPT_OUT != NULL )
        fprintf( FPT_OUT, "%s", line->asc_line );

     len_str        = strlen( line->asc_line );
     bb             = cr_asc_cpy( line->asc_line );
     line->num_sym  = 0;

     num_sym = meta_str_sym( line, 0, len_str );
     ins_asym_eos( line );

     if( num_sym < 2 )
     {
        if( len_str > 0 )
           free( bb );

        continue;
     }


    /*---------------------------------
     * Check for 'name:' directive card
     *---------------------------------*/

     
     if( num_sym == 2 && line->cls_sym[0] == 'a'
         && line->cls_sym[1] == ':' )
     {
        i  = line->idx_sym[0];
        aa = &line->asc_line[i];
        n  = 0;
        free( bb );


       /*---------------------------------
        * Look for dimension:, variable:, or attribute:
        *---------------------------------*/


        if( !strcmp( "dimension", aa ) )
           n = id_cdDim;
        else if( !strcmp( "variable", aa ) )
           n = id_cdVar;
        else if( !strcmp( "attribute", aa ) )
           n = id_cdAtt;
        else
        {
           rel_cdms_card( line );

           return 0;
        }


       /*---------------------------------
        * Create check struct and place at end of list.
        *---------------------------------*/


        ck = (cdCheck *) calloc( 1, sizeof( cdCheck ) );
        if( ck == NULL )
           err_xi( "rd_check_file: trouble getting memory1" );

        ck->L_card    = 50;
        ck->L_fstruct = 50;
        ck->L_alias   = 50;
        ck->card      = malloc( ck->L_card * sizeof( aa ) );
        ck->alias     = malloc( ck->L_alias * sizeof( aa ) );
        ck->fstruct   = malloc( ck->L_fstruct * sizeof( hd ) );
        if( ck->card == NULL || ck->alias == NULL ||
            ck->fstruct == NULL )
           err_xi( "rd_check_file: trouble getting memory2" );

        ck->id = n;

        if( db->ckdefs == NULL )
           db->ckdefs = ck;
        else
        {
           for( cka=db->ckdefs; cka->next != NULL; cka=cka->next )
              ;

           cka->next = ck;
        }
     }


    /*---------------------------------
     * Process card into check struct.
     *---------------------------------*/


     else
     {
        if( num_sym > 2 && line->cls_sym[0] == ':'
            && line->cls_sym[1] == 'a' )
        {
           i  = line->idx_sym[1];
           aa = &line->asc_line[i];
           j  = line->idx_sym[2];


          /*---------------------------------
           * :name
           *---------------------------------*/
           if( !strcmp( "name", aa ) )
           {
              if( ck->name != NULL )
                 err_xi( "rd_check_file: duplicate :name cards" );

              ck->name = cr_asc_cpy( &line->asc_line[j] );
              free( bb );
           }


          /*---------------------------------
           * :alias
           *---------------------------------*/
           else if( !strcmp( "alias", aa ) )
           {
              free( bb );


             /*---------------------------------
              * Determine how many alias names on card.
              *---------------------------------*/


              for( i=2, n=0; i < num_sym; i++ )
              {
                 if( line->cls_sym[i] == 'a' )
                    n++;
              }


             /*---------------------------------
              * Get memory for list of alias names.
              *---------------------------------*/


              if( (ck->nalias + n + 3) > ck->L_alias )
              {
                 ck->L_alias += 50;
                 carda       = ck->alias;
                 ck->alias   = malloc( ck->L_alias * sizeof(aa) );
                 if( ck->alias == NULL )
                    err_xi( "rd_check_file: trouble getting memory3" );
                 j = ck->nalias * sizeof( aa );
                 memcpy( ck->alias, carda, j );
                 free( carda );

                 ck->L_fstruct += 50;
                 free( ck->fstruct );
                 ck->fstruct = malloc( ck->L_fstruct * sizeof( hd ) );
                 if( ck->fstruct == NULL )
                    err_xi( "rd_check_file: trouble getting memory4" );
              }


             /*---------------------------------
              * Copy alias names into list.
              *---------------------------------*/


              for( i=2; i < num_sym; i++ )
              {
                 if( line->cls_sym[i] == 'a' )
                 {
                    k = ck->nalias++;
                    j = line->idx_sym[i];

                    ck->alias[k] = cr_asc_cpy( &line->asc_line[j] );
                 }
              }
           }


          /*---------------------------------
           * :tagname
           *---------------------------------*/
           else if( !strcmp( "tagname", aa ) )
           {
              if( ck->tagname != NULL )
                 err_xi( "rd_check_file: duplicate :tagname cards" );

              ck->tagname = cr_asc_cpy( &line->asc_line[j] );
              free( bb );
           }


          /*---------------------------------
           * Add card to list.
           *---------------------------------*/


           else
           {
              if( (ck->ncard + 3) > ck->L_card )
              {
                 ck->L_card += 50;
                 carda      = ck->card;
                 ck->card   = malloc( ck->L_card * sizeof( aa ) );
                 if( ck->card == NULL )
                    err_xi( "rd_check_file: trouble getting memory5" );

                 j = ck->ncard * sizeof( aa );
                 memcpy( ck->card, carda, j );
                 free( carda );
              }
              n           = ck->ncard++;
              ck->card[n] = bb;
           }
        }

        else
        {
           sprintf( PSQL_MSG, "%s -- %s\n", "IGNORING", bb ); 
           wrt_msg();
           free( bb );
        }
     }
  }

  rel_cdms_card( line );

  return 0;
}


/*********************************************************************
 * Function:  Read all or a subset of dimiable from disc.
 *            Return 'alterfile' modified version of dimiable.
 *********************************************************************/


void *rd_dim_array( cdDim_new *dim,    /* dimension struct */
                    long      start,   /* -1 or dim idx */
                    long      count )  /* -1 or dim cnt */
{
  int          i, j, k, n;
  long         l, l1, l2;
  cdTmp        *tmp;
  dim_pql_read *rd;
  void         *vv, *vva, *vvb;
  CuType       dtype;
  cdAtt_new    *att;
  cdType       typ;


 /*---------------------------------
  * Create pql 'rd' struct with dimension information.
  *---------------------------------*/


  rd = calloc( 1, sizeof( dim_pql_read ) );

  if( rd == NULL )
     err_xv( "rd_dim_array: trouble getting memory" );

  rd->dim  = dim;
  rd->dtyp = dim->datatype;

  att = scn_lnk_list( "psql_type", id_cdAtt, (cdHd *) dim );
  if( att != NULL )
     rd->dtyp = typ_from_ascii( (char *) att->values );


 /*---------------------------------
  * Determine dimension subset wanted.
  *---------------------------------*/


  l1 = 0;
  l2 = dim->length;

  if( start != -1 )
  {
     l1 = start;
     l2 = count;
  }

  if( l1 < 0 )
     l1 = 0;

  if( l1 > (dim->length - 1) )
     l1 = dim->length - 1;

  if( l2 < 1 )
     l2 = 1;

  if( (l1 + l2) > dim->length )
     l2 = dim->length - l1;

  rd->index = l1;
  rd->count = l2;


 /*---------------------------------
  * Get memory to read in dimiable from disc.
  *---------------------------------*/


  typ_to_cdunif( rd->dtyp, &dtype, &n );
     err_rv( );

  rd->data = malloc( rd->count * n );
  if( rd->data == NULL )
     err_xv( "rd_dim_array: trouble getting memory" );


 /*---------------------------------
  * Check if coordinate array is already in memory.
  *---------------------------------*/


  if( dim->data != NULL )
  {
     vva = ary_off( rd->dtyp, rd->index, dim->data );
              err_rv( );

     memcpy( rd->data, vva, rd->count*n);
  }


 /*---------------------------------
  * Read in coordinate array from disc.
  *---------------------------------*/


  else
  {
     vv = rd_dim_coord( rd );
             err_rv( );

     vva = ary_off( rd->dtyp, rd->index, vv );
              err_rv( );

     memcpy( rd->data, vva, rd->count*n);

     free( vv );
  }


 /*---------------------------------
  * For dimiable subset read from disc:
  *   Transpose to new shape (a, b, c) to (b, c, a).
  *   Convert to new data type (float, double, .etc).
  *   Change missing to new missing value.
  *   Multiply by scalar.
  *   Add by scalar.
  *---------------------------------*/


  rd_alt_dim_mod( rd, "psql_wrtinvert" );
     err_rv( );

  rd_alt_dim_mod( rd, "psql_wrttype" );
     err_rv( );

  rd_alt_dim_mod( rd, "psql_newmiss" );
     err_rv( );

  rd_alt_dim_mod( rd, "psql_wrtadd" );
     err_rv( );

  rd_alt_dim_mod( rd, "psql_wrtmult" );
     err_rv( );


 /*---------------------------------
  * Release memory for 'rd' struct.
  *---------------------------------*/


  vv       = rd->data;
  rd->data = NULL;
  free( rd );

  return vv;
}


/*********************************************************************
 * Function:  read dim. coordinates from file and return array.
 *********************************************************************/


void *rd_dim_coord( dim_pql_read *rd )   /* dimension read struct */
{
  int       i, j, n, k, f_id, f_ndims, f_id_dim;
  long      l;
  char      aa[CD_MAX_PATH], *bb;
  void      *v;
  cdDim_new *dim;
  cdAtt_new *att, *atta;
  CuType    dtype;
  cdType    typ;

  dim = rd->dim;


 /*---------------------------------
  * Create '/directory/filename' string from 'file', 'path' attributes.
  *---------------------------------*/


/*.......................................
  att = scn_lnk_list( "psql_file", id_cdAtt, (cdHd *) dim );
........................................*/
  att = nam_fnd( "psql_file", id_cdAtt, (cdHd *) dim );

  if( dim == NULL || att == NULL || att->values == NULL )
     return NULL;

  atta = nam_fnd( "psql_path", id_cdAtt, (cdHd *) dim );

  if( atta != NULL && atta->values != NULL )
  {
     strcpy( aa, atta->values );

     i = strlen( aa );

     if( aa[i-1] != '/' )
        strcat( aa, "/" );
  }
  else
     aa[0] = '\0';

  strcat( aa, att->values );


 /*---------------------------------
  * Get dimension name in disc file.
  *---------------------------------*/


  bb = dim->name;

  att = scn_lnk_list( "psql_name", id_cdAtt, (cdHd *) dim );

  if( att != NULL )
     bb = (char *) att->values;


 /*---------------------------------
  * Attach file.
  *---------------------------------*/


  f_id = cuopenread( aa, NULL );
     err_tv( f_id == -1, "rd_dim_coord: cuopenread trouble" );

  f_ndims = 0;
  j = cuinquire( f_id, &f_ndims, NULL, NULL, NULL );
     err_tv( j == -1, "rd_dim_coord: cuinquire trouble" );


 /*---------------------------------
  * Scan file for dimension.
  *---------------------------------*/


  f_id_dim = -1;

  for( i=0; i < f_ndims; i++ )
  {
     j = cudiminq( f_id, i, aa, NULL, &dtype, NULL, NULL, &l );
        err_tv( j == -1, "rd_dim_coord: cudiminq trouble" );

    /*---------------------------------
     * Reset name with '_' in non-printing letters.
     *---------------------------------*/
     for( j=0; j < strlen(aa); j++ )
     {
        if( !isgraph( aa[j] ) )
           aa[j] = '_';
     }

     if( !strcmp( bb, aa ) )
     {
        f_id_dim = i;
        break;
     }
  }


  typ_from_cdunif( dtype, &typ, &n );
     err_rv( );

  if( f_id_dim == -1 || dim->length != l || rd->dtyp != typ )
  {
     j = cuclose( f_id );
     return NULL;
  }


 /*---------------------------------
  * Read in dimension coordinates from file.
  *---------------------------------*/
  

  v = malloc( l * n );
         err_tv( v == NULL, "rd_dim_coord: malloc memory trouble");

  j  = cudimget( f_id, f_id_dim, v );
          err_tv( j == -1, "rd_dim_coord: cudimget trouble");

  j = cuclose( f_id );

  return v;
}


/*********************************************************************
 * Function to process metafile into memory structs
 *********************************************************************/


int rd_meta_file( char *filename )  /* name-with-path input metafile */
{
  int         i, n, num_sym;
  char        *aa;
  cdDb_new    *db;
  cdDset_new  *ds;
  cdHd        *hd;
  cdms_card   *line;
  cdAtt_new   *att;


 /*---------------------------------
  * Create a print file; Create first database as user scratch.
  *---------------------------------*/


  db = init_user_db( );
  hd = (cdHd *) db;


 /*---------------------------------
  * Open ascii metafile
  *---------------------------------*/


  mem_cdms_card( &line );
     err_ri( );

  aa = get_path_of_file( filename, NULL );
  
  line->fp = fopen( filename, "r" );
  if( line->fp == NULL )
     err_xi( "rd_meta_file: trouble opening input file" );

  if( FPT_OUT != NULL )
     fprintf( FPT_OUT, "***metafile: %s\n", filename );


 /*---------------------------------
  * Read first non-comment line and crack it into symbols.
  *---------------------------------*/


  num_sym = rd_meta_line( line );
               err_ri( );


 /*---------------------------------
  * Check if special-small-file form of a file-spanning-metafile.

  * Note: in special-file there is no ';' symbol, so first read 
  *       went to end of file.
  *---------------------------------*/


  if( line->eof == NULL )
  {
     rewind( line->fp );
     line->eof = NULL;

     rd_small_file( line, aa, "yes" );

     free( aa );
     rel_cdms_card( line );
     return 100;
  }


 /*---------------------------------
  * Loop over ascii lines in normal metafile
  *---------------------------------*/


  while( line->eof != NULL )
  {


    /*---------------------------------
     * Check for 'name:' directive card
     *---------------------------------*/

     
     if( num_sym == 2 && line->cls_sym[0] == 'a'
         && line->cls_sym[1] == ':' )
     {
        i = line->idx_sym[0];

        if( !strcmp( "dimensions", &line->asc_line[i] ) && hd != NULL )
        {
           pro_dim_lines( line, hd );
              err_ri( );
        }

        else if( !strcmp( "variables", &line->asc_line[i] )
                 && hd != NULL )
        {
           pro_var_lines( line, hd );
              err_ri( );
        }

        else if( !strcmp( "data", &line->asc_line[i] ) && hd != NULL )
        {
           pro_data_lines( line, hd );
              err_ri( );
        }

        else if( !strcmp( "dataset", &line->asc_line[i] ) && hd != NULL )
        {
           ds = pro_dset_lines( line, hd );
                   err_ri( );
           hd = (cdHd *) ds;
        }

        else if( !strcmp( "database", &line->asc_line[i] ) )
        {
           db = pro_db_lines( line, hd );
                   err_ri( );
           hd = (cdHd *) db;
        }

        else if( !strcmp( "end", &line->asc_line[i] ) )
        {
           break;
        }

        else
        {
           err_xi( "rd_meta_file: metaline and db problem" );
        }
     }

     else
     {
        err_xi( "rd_meta_file: bad metaline" );
     }
  }


 /*---------------------------------
  * Check if database node has a 'path' attribute.

  * NOTE: If spanning file and files spanned over in same directory
  *       'path' attribute sometimes not given.  This enables user
  *       to move directory around.
  * NOTE: If no 'path' attribute, then PSQL defines it.
  *---------------------------------*/


  hd = old_att_name( (cdHd *) db );

  for( db=DB_ROOT_ADR; db->next != NULL; db=db->next )
     ;

  att = scn_lnk_list( "psql_path", id_cdAtt, (cdHd *) db );

  if( att == NULL )
  {
     att           = cre_struct( id_cdAtt, (cdHd *) db );
     att->name     = cr_asc_cpy( "psql_path" );
     att->datatype = cdChar;
     att->length   = strlen( aa );
     att->values   = aa;
  }
  else
     free( aa );

  pro_tmp_att();
     err_ri( );

  rel_cdms_card( line );

  for( ds=db->dsets, n=0; ds; ds=ds->next )
     n++;

  return n;
}


/*********************************************************************
 * Function to read metafile up to ';' and crack it into symbols
 *********************************************************************/


int rd_meta_line( cdms_card *line )  /* struct metafile line */
{
  int  i, n, num_sym, len_str, lline, lsym, *idx_sym, *len_sym,
       *cls_sym;
  char *asc_line;

  asc_line = line->asc_line;
  idx_sym  = line->idx_sym;
  len_sym  = line->len_sym;
  cls_sym  = line->cls_sym;

  line->len_line = 0;
  line->num_sym  = 0;
  i              = 0;

  if( FPT_OUT != NULL )
     fprintf( FPT_OUT, "***card\n" );


 /*---------------------------------
  * Read and concatenate lines, until ';' symbol ends line,
  *    or line is just 2 symbols 'name:'
  *---------------------------------*/


  while( (line->eof = fgets( &asc_line[i], 120, line->fp )) != NULL )
  {


    /*---------------------------------
     * Process newline letter at end of line
     *---------------------------------*/


     if( FPT_OUT != NULL )
        fprintf( FPT_OUT, "%s", &asc_line[i] );

     len_str = strlen( &asc_line[i] );


    /*---------------------------------
     * Crack line into symbols
     *---------------------------------*/


     num_sym = meta_str_sym( line, i, len_str );
     i       = line->len_line;


    /*---------------------------------
     * Check if have a complete metafile line
     *---------------------------------*/


     if( (num_sym > 0 && cls_sym[num_sym-1] == ';') ||
         (num_sym == 2 && cls_sym[0] == 'a' && cls_sym[1] == ':') )
        break;


    /*---------------------------------
     * Check if about to overflow asc_line/idx_sym memory
     *---------------------------------*/

     if( line->len_line + 120 > line->L_asc_line )
     {
        lline          = line->L_asc_line + 2048;
        line->asc_line = (char *) malloc( lline * sizeof(char) );

     if( line->asc_line == NULL )
        err_xi( "rd_meta_line: trouble getting memory" );

        memcpy( line->asc_line, asc_line, line->L_asc_line );
        free( asc_line );

        line->L_asc_line = lline;
        asc_line         = line->asc_line;
     }

     if( line->num_sym + 50 > line->L_idx_sym )
     {
        lsym          = line->L_idx_sym + 512;
        n             = lsym * sizeof(int);
        line->idx_sym = (int *) malloc( n );
        line->len_sym = (int *) malloc( n );
        line->cls_sym = (int *) malloc( n );

        if( line->idx_sym == NULL || line->len_sym == NULL ||
            line->cls_sym == NULL )
           err_xi( "rd_meta_line: trouble getting memory" );

        n    = line->L_idx_sym * sizeof(int);
        memcpy( line->idx_sym, idx_sym, n );
        memcpy( line->len_sym, len_sym, n );
        memcpy( line->cls_sym, cls_sym, n );
        free( idx_sym );
        free( len_sym );
        free( cls_sym );

        line->L_idx_sym = lsym;
        idx_sym         = line->idx_sym;
        len_sym         = line->len_sym;
        cls_sym         = line->cls_sym;
     }
  }

  ins_asym_eos( line );
  return num_sym;
}


/*********************************************************************
 * Function to process dimension card from small-spanning-file.
 * 'time:idxidx altf:1980 ps, pr'
 *********************************************************************/


void rd_small_dim_card( cdms_card *line,   /* card cracking struct */
                        span_list *span )  /* span struct */
{
  int  i, j, k, n, l;
  char *aa, **aalst;


 /*---------------------------------
  * Determine name count and get memory for variable-name-list.
  *---------------------------------*/


  for( i=0, n=0; i < line->num_sym; i++ )
  {
     if( line->cls_sym[i] == 'a' )
        n++;

     else if( line->cls_sym[i] == ':' && line->cls_sym[i+1] == 'a' )
        i++;
  }

  if( n < 1 )
     err_x( "rd_small_dim_card: bad dimension name card" );

  if( n > 2 )
  {
     l = n - 2;

     aalst = malloc( l * sizeof(aa) );
     if( aalst == NULL )
        err_x( "rd_small_dim_card: trouble getting memory" );

     span->lvlst = l;
     span->vlst  = aalst;
  }


 /*---------------------------------
  * Scan dimension name card of short spanning file.
  *---------------------------------*/


  for( i=0, n=0, j=0; i < line->num_sym; i++ )
  {
     if( line->cls_sym[i] == 'a' )
     {
        n++;
        k = line->idx_sym[i];
        l = k + line->len_sym[i];
        line->asc_line[l] = '\0';


       /*---------------------------------
        * Check for dimension name on card.
        *---------------------------------*/

        if( n == 1 )
        {
           span->dname = cr_asc_cpy( &line->asc_line[k] );

           if( typ_time_dim( span->dname ) )
              span->flg_dtyp = 1;


          /*---------------------------------
           * See if ': idxidx/idxcnt/offoff/offcnt' also given.
           *---------------------------------*/

           if( line->cls_sym[i+1] == ':' && i+2 < line->num_sym )
           {
              i += 2;

              k = line->idx_sym[i];
              l = k + line->len_sym[i];
              line->asc_line[l] = '\0';

              if( line->cls_sym[i] == 'a' )
              {
                 if( !strcmp( &line->asc_line[k], "idxidx" ) )
                    span->select = 1;
                 else if( !strcmp( &line->asc_line[k], "idxcnt" ) )
                    span->select = 2;
                 else if( !strcmp( &line->asc_line[k], "offoff" ) )
                    span->select = 3;
                 else if( !strcmp( &line->asc_line[k], "offcnt" ) )
                    span->select = 4;
              }
           }
        }


       /*---------------------------------
        * Check for 'alterfile:*arg1*' also on card.
        *---------------------------------*/

        else if( n == 2 )
        {
           if( strcmp( &line->asc_line[k], "none" ) )
              span->altf = cr_asc_cpy( &line->asc_line[k] );


          /*---------------------------------
           * See if ':*arg1*' also given
           *---------------------------------*/

           if( line->cls_sym[i+1] == ':' && i+2 < line->num_sym )
           {
              i += 2;

              k = line->idx_sym[i];
              l = k + line->len_sym[i];
              line->asc_line[l] = '\0';

              if( DB_ROOT_ADR->f->ALT_PAT != NULL )
              {
                 free( DB_ROOT_ADR->f->ALT_PAT );
                 DB_ROOT_ADR->f->ALT_PAT = NULL;
              }

              DB_ROOT_ADR->f->ALT_PAT = \
                           cr_asc_cpy( &line->asc_line[k] );
           }
        }


       /*---------------------------------
        * Check for variable name list on card.
        *---------------------------------*/

        else
           aalst[j++] = cr_asc_cpy( &line->asc_line[k] );
     }
  }

  return;
}


/*********************************************************************
 * Function to process special-small-spanning-metafile.
 *********************************************************************/


void rd_small_file( cdms_card *line,   /* card cracking struct */
                    char      *path,   /* directory of file */
                    char      *fall )  /* yes/no flag all var in file */
{
  int        i, j, k, n, num_sym, len_str, cnt;
  char       *aa;
  cdDb_new   *db;
  cdDset_new *ds;
  cdDim_new  *dim;
  cdVar_new  *var;
  cdAtt_new  *att, *atta;
  cdTmp      *tmp;
  span_list  *span;


 /*---------------------------------
  * Get temp memory for 'span_list' type struct.
  *---------------------------------*/


  span = mem_span_list( NULL, 1, 50 );
            err_r( );


 /*---------------------------------
  * Loop over cards of special-small-spanning-metafile.

  * Note: Because tstart can be 1994-7-4' or '1994-7-4 10:37'
  *       there are no '\0' symbol termination letters on line.
  *---------------------------------*/


  cnt = 0;

  while( (line->eof = fgets( line->asc_line, 120, line->fp )) != NULL )
  {
     line->num_sym = 0;
     len_str       = strlen( line->asc_line );
     num_sym       = meta_str_sym( line, 0, len_str );

     if( num_sym < 1 )
        continue;

     cnt++;
     n = num_sym - 1;
     j = line->idx_sym[n];
     k = j + line->len_sym[n];
     line->asc_line[k] = '\0';


    /*---------------------------------
     * Dimension name card.
     *---------------------------------*/

     if( cnt == 1 )
     {
        rd_small_dim_card( line, span );
           err_r( );
     }


    /*---------------------------------
     * Units card (only if short 'delta' format).
     *---------------------------------*/

     else if( cnt == 2 && num_sym > 2 && line->cls_sym[0] == 'a'
              && line->cls_sym[1] == 'a' )
     {
        if( span->flg_dtyp != 1 )
           err_x( "rd_small_file: units card only if span on time" );

        span->flg_del = 1;
        span->uni  = cr_asc_cpy( line->asc_line );

        j = line->idx_sym[0];
        k = line->asc_line[j];

        if( k == 'M' )
        {
           span->flg_uni = 1;
           aa = span->uni; 
           aa[j+0] = 'd';
           aa[j+1] = 'a';
           aa[j+2] = 'y';
           aa[j+3] = 's';
           aa[j+4] = ' ';
           aa[j+5] = ' ';
        }
        else if( k == 'd' )
           span->flg_uni = 2;
        else if( k == 'h' )
           span->flg_uni = 3;
        else if( k == 'm' )
           span->flg_uni = 4;
        else
        {
           err_x( "rd_small_file: bad units card" );
        }
     }


    /*---------------------------------
     * Delta card (only if short 'delta' format).
     *---------------------------------*/

     else if( cnt == 3 && span->flg_del == 1 )
     {
        if( num_sym != 1 )
           err_x( "rd_small_file: bad delta card" )

        j = line->idx_sym[0];

        if( line->cls_sym[0] == 'f' )
           span->delt = atof( &line->asc_line[j] );
        else if( line->cls_sym[0] == 'i' )
           span->delt = atol( &line->asc_line[j] );
        else
        {
           err_x( "rd_small_file: bad delta card" )
        }
     }


    /*---------------------------------
     * 'file tstart tend' card.
     *---------------------------------*/

     else
     {
        scn_small_file( line, span, path );
           err_r( );
     }
  }


 /*---------------------------------
  * Locate spanning dimension, delete 'psql_...' attributes
  *---------------------------------*/


  db = span->dbs;
  if( db == NULL )
     err_x( "rd_small_file: no files spanned" );

  ds = db->dsets;

  dim = scn_lnk_list( span->dname, id_cdDim, (cdHd *) ds );
  if( dim == NULL )
     err_x( "rd_small_file: missing spanning dimension" );

  att = dim->atts;

  while( att != NULL )
  {
     aa = strstr( att->name, "psql_" );

     if(  aa == NULL )
        att = att->next;
     else
     {
        atta = att->next;
        delete_struct( (cdHd *) att );
           err_r( );
        att = atta;
     }
  }


 /*---------------------------------
  * Insert spanning 'psql_....' attributes.
  *---------------------------------*/


  dim->datatype = cdDouble;
  dim->length   = span->n_cord;
  dim->data     = span->scord;

  if( span->uni != NULL )
  {
     att = scn_lnk_list( "units", id_cdAtt, (cdHd *) dim );
     if( att != NULL )
     {
        delete_struct( (cdHd *) att );
           err_r( );
     }

     att           = cre_struct( id_cdAtt, (cdHd *) dim );
     att->name     = cr_asc_cpy( "units" );
     att->datatype = cdChar;
     att->length   = strlen( span->uni );
     att->values   = span->uni;
  }

  att           = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name     = cr_asc_cpy( "psql_filelist" );
  att->datatype = cdCharTime;
  att->length   = span->n_flist;
  att->values   = span->flist;

  att           = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name     = cr_asc_cpy( "psql_pathlist" );
  att->datatype = cdCharTime;
  att->length   = span->n_flist;
  att->values   = span->plist;

  att           = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name     = cr_asc_cpy( "psql_poslist" );
  att->datatype = cdInt;
  att->length   = span->n_flist;
  att->values   = span->fdoff;

  att           = cre_struct( id_cdAtt, (cdHd *) dim );
  att->name     = cr_asc_cpy( "psql_filepoint" );
  att->datatype = cdInt;
  att->length   = span->n_cord;
  att->values   = span->fpoint;


 /*---------------------------------
  * Reset variable lengths to reflect span dimension length.
  *---------------------------------*/


  for( var=ds->vars; var; var=var->next )
  {
     var->length = 1;

     for( tmp=var->dim; tmp; tmp=tmp->next )
     {
        if( tmp->id_want != id_cdDim )
           err_x( "rd_small_file: var-dim def trouble" );

        dim = (cdDim_new *) tmp->want;
        var->length *= dim->length;
     }
  }


 /*---------------------------------
  * Run 'alterfile' against created spaning dataset.
  * Release 'span_list' struct (it's empty now).
  * Release scratch storage.
  *---------------------------------*/


/*..................
  tmp = old_att_name( (cdHd *) db );
..................*/

  if( span->altf != NULL )
  {
     scn_small_alterf( ds, span->altf, path );
        err_r( );

     free( span->altf );
  }

  if( span->vlst != NULL )
  {
     for( i=0; i < span->lvlst; i++ )
        free( span->vlst[i] );

     free( span->vlst );
  }

  free( span );

  return;
}


/*********************************************************************
 * Function:  Read all or a subset of variable from disc.
 *            Return 'alterfile' modified version of variable.
 *********************************************************************/


void *rd_var_array( cdVar_new *var,      /* variable struct */
                    long      start[],   /* NULL or dim idx */
                    long      count[] )  /* NULL or dim cnt */
{
  int          i, j, k, n;
  long         l, l1, l2;
  cdDim_new    *dim;
  cdTmp        *tmp;
  var_pql_read *rd;
  void         *vv;
  CuType       dtype;


 /*---------------------------------
  * Create pql 'rd' struct with variable information.
  *---------------------------------*/


  rd = pql_read_mem( var );
          err_rv( );


 /*---------------------------------
  * Set struct start, count to subset wanted.

  * Note: Input arguments are in dimension order of the output.
  *       Struct is in dimension order of variable in disc files.

  * Note: 'dorder' gives disc-to-output dimension correspondence.
  *---------------------------------*/


  for( i=0; i < rd->ndims; i++ )
  {
     dim = rd->dims[i];

     l1 = 0;
     l2 = dim->length;

     j = i; 

     if( rd->dorder != NULL )
        j = rd->dorder[i];

     if( start != NULL && j != -1 )
     {
        l1 = start[j];
        l2 = count[j];
     }

     if( l1 < 0 )
        l1 = 0;

     if( l1 > (dim->length - 1) )
        l1 = dim->length - 1;

     if( l2 < 1 )
        l2 = 1;

     if( (l1 + l2) > dim->length )
        l2 = dim->length - l1;

     rd->index[i] = l1;
     rd->count[i] = l2;
  }


 /*---------------------------------
  * Get memory to read in variable from disc.
  *---------------------------------*/


  for( i=0, l=1; i < rd->ndims; i++ )
     l *= rd->count[i];

  rd->vlen = l;

  typ_to_cdunif( rd->vtyp, &dtype, &n );
     err_rv( );

  rd->data = malloc( rd->vlen * n );
  if( rd->data == NULL )
     err_xv( "rd_var_array: trouble getting memory" );


 /*---------------------------------
  * Multi-file variable read.
  *---------------------------------*/


  pql_read_fil( rd );
     err_rv( );


 /*---------------------------------
  * For variable subset read from disc:
  *   Transpose to new shape (a, b, c) to (b, c, a).
  *   Convert to new data type (float, double, .etc).
  *   Change missing to new missing value.
  *   Multiply by scalar.
  *   Add by scalar.
  *---------------------------------*/


  rd_alt_var_mod( rd, "psql_wrtinvert" );
     err_rv( );

  rd_alt_var_mod( rd, "psql_wrtshape" );
     err_rv( );

  rd_alt_var_mod( rd, "psql_wrttype" );
     err_rv( );

  rd_alt_var_mod( rd, "psql_newmiss" );
     err_rv( );

  rd_alt_var_mod( rd, "psql_wrtadd" );
     err_rv( );

  rd_alt_var_mod( rd, "psql_wrtmult" );
     err_rv( );


 /*---------------------------------
  * Release memory for 'rd' struct.
  *---------------------------------*/


  vv       = rd->data;
  rd->data = NULL;

  pql_read_dmem( rd );

  return vv;
}


/*********************************************************************
 * Function: release memory for cdms_card struct.
 *********************************************************************/


void rel_cdms_card( cdms_card  *line )  /* struct metafile line */
{
 /*---------------------------------
  * Disconnect ascii input file.
  *---------------------------------*/


  if( line->fp != NULL )
     fclose(line->fp);


 /*---------------------------------
  * Release memory for 'cdms_card' type struct.
  *---------------------------------*/


  free( line->asc_line );

  free( line->idx_sym );
  free( line->len_sym );
  free( line->cls_sym );

  free( line );
  return;
}


/*********************************************************************
 * Function: release memory for cdms_pql_list struct.
 *********************************************************************/


void rel_cdms_pql_list( cdms_pql_list *pql )  /* pql nodes */
{


 /*---------------------------------
  * Release memory for 'cdms_pql_list' type struct.
  *---------------------------------*/


  free( pql->list );

  free( pql );
  return;
}


/*********************************************************************
 * Function: release memory for 1 or all check structs.
 *********************************************************************/


void rel_check( cdCheck  *cka )  /* NULL or check struct */
{
  int       i;
  cdDb_new  *db;
  cdCheck   *ck, *ckb;
  cdHd      *hd;


 /*---------------------------------
  * Erase memory for any previous check/alter file.
  *---------------------------------*/


  db = init_user_db( );

  if( cka == NULL && db->ckdefs != NULL )
  {
     for( cka=db->ckdefs; cka; )
     {
        ck  = cka;
        cka = cka->next;  /* point to next before release */

        if( ck->name != NULL )
           free( ck->name );

        if( ck->L_alias > 0 )
        {
           for( i=0; i < ck->nalias; i++ )
              free( ck->alias[i] );

           free( ck->alias );
        }

        if( ck->tagname != NULL )
           free( ck->tagname );

        if( ck->L_card > 0 )
        {
           for( i=0; i < ck->ncard; i++ )
              free( ck->card[i] );

           free( ck->card );
        }

        if( ck->L_fstruct > 0 )
           free( ck->fstruct );

        free( ck );
     }

     db->ckdefs = NULL;

     return;
  }

  if( cka == NULL )
     return;


 /*---------------------------------
  * Release memory for check struct.
  *---------------------------------*/


  if( db->ckdefs == cka )
     db->ckdefs = db->ckdefs->next;
  else
  {
     for( ckb=db->ckdefs; ckb->next != cka; ckb=ckb->next )
        ;

     ckb->next = cka->next;
  }


  if( cka->name != NULL )
     free( cka->name );

  if( cka->L_alias > 0 )
  {
     for( i=0; i < cka->nalias; i++ )
        free( cka->alias[i] );

     free( cka->alias );
  }

  if( cka->tagname != NULL )
     free( cka->tagname );

  if( cka->L_card > 0 )
  {
     for( i=0; i < cka->ncard; i++ )
        free( cka->card[i] );

     free( cka->card );
  }

  if( cka->L_fstruct > 0 )
     free( cka->fstruct );

  free( cka );
  return;
}


/*********************************************************************
 * Function: search current struct for struct of desired type & name.

 * Note: This routine is 'nam_fnd' restricted to 'cur' level.
 *       If 'cur' contains a linked-list of desired 'type', then
 *       only this list is scanned for 'name'.
 *       If 'type' is database, then the top level database linked
 *       list is scanned (ie. 'cur' is ignored).
 *********************************************************************/


void *scn_lnk_list( char    name[],  /* name of wanted struct */
                    cdms_Id idwant,  /* id of wanted struct */
                    cdHd    *cur )   /* header of current struct */
{
  cdDb_new   *db;
  cdDset_new *ds;
  cdVar_new  *var;
  cdDim_new  *dim;
  cdAtt_new  *att;
  cdPql      *pql;
  void       **v;


 /*---------------------------------
  * check if idwant-linked-list is in current struct
  *---------------------------------*/


  v = fnd_lnk_list( idwant, cur );


 /*---------------------------------
  * go across linked-list searching for name
  *---------------------------------*/


  if( v != NULL && *v != NULL )
  {
     if( idwant == id_cdDb )
     {
        for( db = (cdDb_new *) *v; db; db = db->next )
        {
           if( db->id == id_cdDb && db->name != NULL && 
               !strcmp( name, db->name ) )
              return (void *) db;
        }
     }

     else if( idwant == id_cdDset )
     {
        for( ds = (cdDset_new *) *v; ds; ds = ds->next )
        {
           if( ds->id== id_cdDset && ds->name != NULL && 
               !strcmp( name, ds->name ) )
              return (void *) ds;
        }
     }

     else if( idwant == id_cdVar )
     {
        for( var = (cdVar_new *) *v; var; var = var->next )
        {
           if( var->id == id_cdVar && var->name != NULL && 
               !strcmp( name, var->name ))
              return (void *) var;
        }
     }

     else if( idwant == id_cdDim )
     {
        for( dim = (cdDim_new *) *v; dim; dim = dim->next )
        {
           if( dim->id == id_cdDim && dim->name != NULL && 
               !strcmp( name, dim->name ) )
              return (void *) dim;
        }
     }

     else if( idwant == id_cdAtt )
     {
        for( att = (cdAtt_new *) *v; att; att = att->next )
        {
           if( att->id == id_cdAtt && att->name != NULL && 
               !strcmp( name, att->name ) )
              return (void *) att;
        }
     }

     else if( idwant == id_cdPql )
     {
        for( pql = (cdPql *) *v; pql; pql = pql->next )
        {
           if( pql->id == id_cdPql && pql->name != NULL && 
               !strcmp( name, pql->name ) )
              return (void *) pql;
        }
     }
  }

  return NULL;
}


/*********************************************************************
 * Function to process alterfile through a .cdms file.
 *********************************************************************/


void scn_small_alterf( cdDset_new *ds,      /* span dataset */
                       char       *altf,    /* alterfile */
                       char       *mpath )  /* NULL or '.cdms' path */
{
  int  i, k;
  char *fnm, *pth, msg[120], *dnam;
  cdDim_new  *dim;
  cdVar_new  *var;
  cdAtt_new  *att, *atta;
  void *vv;


 /*---------------------------------
  * Get filename and path of alterfile.
  *---------------------------------*/


  fnm = cr_asc_cpy( altf );
  k   = strlen( altf );

  for( i=k-1; i > 0; i-- )
  {
     if( altf[i] == '/' )
     {
        free( fnm );
        fnm = cr_asc_cpy( &altf[i+1] );
        break;
     }
  }

  pth = get_path_of_file( altf, mpath );


 /*---------------------------------
  * Read alterfile.
  *---------------------------------*/


  sprintf( msg, "%s/%s", pth, fnm );
  free( fnm );
  free( pth );

  i = rd_alter_file( msg );
         err_r( );


 /*---------------------------------
  * Alter dataset.
  *---------------------------------*/


  alter_dataset( (cdHd *) ds );
     err_r( );


/*.................debug...................
  for( dim=ds->dims; dim; dim=dim->next )
     printf( "%s dimension\n", dim->name );
  for( var=ds->vars; var; var=var->next )
     printf( "%s variable\n", var->name );
.................debug...................*/

 /*---------------------------------
  * Copy dimension altered attributes, etc. to variable with same name.
  *---------------------------------*/

  for( dim=ds->dims; dim; dim=dim->next )
  {
     dnam = dim->name;

     att = scn_lnk_list( "psql_name", id_cdAtt, (cdHd *) dim );
     if( att != NULL )
        dnam = (char * ) att->values;

     var = scn_lnk_list( dnam, id_cdVar, (cdHd *) ds );

     if( var != NULL )
     {
        att = var->atts;

        while( att != NULL )
        {
           atta = att->next;
           delete_struct( (cdHd *) att );
              err_r( );
           att = atta;
        }

        for( att=dim->atts; att; att=att->next )
        {
           cr_att_cpy( att, (cdHd *) var );
              err_r( );
        }

        if( strcmp( var->name, dim->name ) )
        {
           free( var->name );

           var->name = cr_asc_cpy( dim->name );
        }

        if( dim->data != NULL && dim->length == var->length )
        {
           if( var->data != NULL )
              free( var->data );

           var->datatype = dim->datatype;

           var->data = ary_trans( dim->datatype, dim->length,
                                  dim->data, var->datatype );
              err_r( );
        }
     }
  }

  return;
}


/*********************************************************************
 * Function to squeeze db var,dim,att for spanning db creation.
 *********************************************************************/


void scn_small_db( span_list *span,  /* span creation struct */
                   cdDb_new  *db,    /* database to view */
                   char      *nam,   /* file name */
                   char      *pth )  /* file path */
{
  int        i, k, n;
  char       *aa;
  cdDset_new *ds;
  cdVar_new  *var, *vara;
  cdDim_new  *dim, *dima;
  cdAtt_new  *att, *atta;
  cdTmp      *tmp, *tmpa;

  ds = db->dsets;


 /*---------------------------------
  * Check if db contains first file spanned over (ie. span->dbs!=NULL).
  * If true, then loop over first files variables.
  * Keep variable only if it is also in this file and with same shape.
  * Note: Be sure to not delete non-span-variables from first file.
  *---------------------------------*/


  if( span->dbs == NULL )
     var = NULL;
  else
     var = span->dbs->dsets->vars;


  while( var != NULL )
  {
     k    = 0;
     n    = 0;
     vara = NULL;

    /*---------------------------------
     * Check if variable defined in terms of the spanning dimension.
     *---------------------------------*/

     for( tmp=var->dim; tmp; tmp=tmp->next )
     {
        dim = (cdDim_new *) tmp->want;
        if( !strcmp( dim->name, span->dname ) )
        {
           n = 5;
           break;
        }
     }

    /*---------------------------------
     * Check if spanning variable is in input file with same shape.
     *---------------------------------*/

     if( n )
     {
        vara = scn_lnk_list( var->name, id_cdVar, (cdHd *) ds );
        if( vara == NULL )
           k = 3;
     }

     if( n && vara != NULL )
     {
        tmp  = var->dim;
        tmpa = vara->dim;

        while( tmp != NULL )
        {
           if( tmpa == NULL || tmp->id_want != id_cdDim
               || tmpa->id_want != id_cdDim )
           {
              k = 5;
              break;
           }

           dim  = (cdDim_new *) tmp->want;
           dima = (cdDim_new *) tmpa->want;

           if( strcmp( dim->name, dima->name ) )
           {
              k = 7;
              break;
           }

           if( strcmp( dim->name, span->dname ) &&
               dim->length != dima->length )
           {
              k = 9;
              break;
           }

           tmp  = tmp->next;
           tmpa = tmpa ->next;
        }
     }

    /*---------------------------------
     * Delete variable from span->dbs and also this input file.
     *---------------------------------*/

     if( k )
     {
        if( vara != NULL )
        {
           delete_struct( (cdHd *) vara );
              err_r( );
        }

        vara = var->next;
        delete_struct( (cdHd *) var );
           err_r( );
        var = vara;
     }
     else
        var = var->next;
  }

  if( span->dbs != NULL && span->dbs->dsets->vars == NULL )
     err_x( "scn_small_db: var-over-span-files trouble" );


 /*---------------------------------
  * Check if db contains first file spanned over (span->dbs!=NULL).
  * If true, then loop over first file's dimensions.
  * Keep dimension only if it is used by remaining variables.
  *---------------------------------*/


  if( span->dbs == NULL )
     dim = NULL;
  else
     dim = span->dbs->dsets->dims;

  while( dim != NULL )
  {
     k = 0;

     for( var=span->dbs->dsets->vars; var; var=var->next )
     {
        for( tmp=var->dim; tmp; tmp=tmp->next )
        {
           if( tmp->id_want != id_cdDim )
              err_x( "scn_small_db: dims-of-var trouble" );

           dima = (cdDim_new *) tmp->want;

           if( dim == dima )
           {
              k = 7;
              break;
           }
        }

        if( k != 0 )
           break;
     }

     if( k != 0 )
        dim = dim->next;
     else
     {
        dima = dim->next;
        delete_struct( (cdHd *) dim );
           err_r( );
        dim = dima;
     }
  }


 /*---------------------------------
  * Loop over variables in ds (ie. this input file).
  * Delete ds variables if not in list supplied by user.

  * *** Logic change ***
  * Prev: Delete ds variables which are also a dimension.
  * New: Keep variable and copy it's attributes to dimension.
  *---------------------------------*/


  var = ds->vars;

  while( var != NULL )
  {
     k = 0;

    /*---------------------------------
     * Check if variable is a dimension.
     **** Logic change ***
     *  Original logic:  delete variable from dataset.
     *  New logic:  Keep variable and copy it's attributes to dimension.
     *---------------------------------*/

     if( var->ndims == 1 )
     {
        tmp = var->dim;
        if( tmp->id_want != id_cdDim )
           err_x( "scn_small_db: dims-of-var trouble" );

        dim = (cdDim_new *) tmp->want;

        if( !strcmp( var->name, dim->name ) )
        {
           for( att=var->atts; att; att=att->next )
           {
              if( strcmp(att->name,"units") &&
                  strstr( att->name, "psql_" ) == NULL )
                 cr_att_cpy( att, (cdHd *) dim );
           }
        }
     }

    /*---------------------------------
     * Check if variable is in user supplied list.
     *---------------------------------*/

     if( k == 0 && span->vlst != NULL )
     {
        k = 7;

        for( i=0; i < span->lvlst; i++ )
        {
           if( !strcmp( var->name, span->vlst[i] ) )
           {
              k = 0;
              break;
           }
        }
     }

    /*---------------------------------
     * Check if we delete variable.
     *---------------------------------*/

     if( k == 0 )
        var = var->next;
     else
     {
        vara = var->next;
        delete_struct( (cdHd *) var );
           err_r( );
        var = vara;
     }
  }

  if( ds->vars == NULL )
     err_x( "scn_small_db: no variable spans files" );


 /*---------------------------------
  * Loop over dimensions in ds (ie. this input file).
  * Delete dimensions not used by remaining ds variables.
  *---------------------------------*/


  dim = ds->dims;

  while( dim != NULL )
  {
     k = 0;

     for( var=ds->vars; var; var=var->next )
     {
        for( tmp=var->dim; tmp; tmp=tmp->next )
        {
           if( tmp->id_want != id_cdDim )
              err_x( "scn_small_db: dims-of-var trouble" );

           dima = (cdDim_new *) tmp->want;

           if( dim == dima )
           {
              k = 7;
              break;
           }
        }

        if( k != 0 )
           break;
     }

     if( k != 0 )
        dim = dim->next;
     else
     {
        dima = dim->next;
        delete_struct( (cdHd *) dim );
           err_r( );
        dim = dima;
     }
  }


 /*---------------------------------
  * Check if ds is first file spanned over (span->dbs==NULL).
  * If true, then set/unset appropriate 'psql_.....' named attributes.
  *---------------------------------*/


  if( span->dbs == NULL )
  {
    /*---------------------------------
     * Database attributes.
     *---------------------------------*/
     att = db->atts;

     while( att != NULL )
     {
        aa = strstr( att->name, "psql_" );

        if(  aa == NULL )
           att = att->next;
        else
        {
           atta = att->next;
           delete_struct( (cdHd *) att );
              err_r( );
           att = atta;
        }
     }

    /*---------------------------------
     * Dataset attributes.
     *---------------------------------*/

     att = ds->atts;

     while( att != NULL )
     {
        aa = strstr( att->name, "psql_" );

        if(  aa == NULL )
           att = att->next;
        else
        {
           atta = att->next;
           delete_struct( (cdHd *) att );
              err_r( );
           att = atta;
        }
     }

     free( ds->name );
     ds->name = cr_asc_cpy( "ds_group" );

     att           = cre_struct( id_cdAtt, (cdHd *) ds );
     att->name     = cr_asc_cpy( "psql_file" );
     att->datatype = cdChar;
     att->length   = strlen( nam );
     att->values   = cr_asc_cpy( nam );

     att           = cre_struct( id_cdAtt, (cdHd *) ds );
     att->name     = cr_asc_cpy( "psql_path" );
     att->datatype = cdChar;
     att->length   = strlen( pth );
     att->values   = cr_asc_cpy( pth );

    /*---------------------------------
     * Variable attributes.
     *---------------------------------*/

     for( var=ds->vars; var; var=var->next )
     {
        att = var->atts;

        while( att != NULL )
        {
           aa = strstr( att->name, "psql_" );

           if(  aa == NULL )
              att = att->next;
           else
           {
              atta = att->next;
              delete_struct( (cdHd *) att );
                 err_r( );
              att = atta;
           }
        }
     }

    /*---------------------------------
     * Dimension attributes.
     *---------------------------------*/

     for( dim=ds->dims; dim; dim=dim->next )
     {
        att = dim->atts;

        while( att != NULL )
        {
           aa = strstr( att->name, "psql_" );

           if(  aa == NULL )
              att = att->next;
           else
           {
              if( !strcmp( att->name, "psql_file" ) ||
                  !strcmp( att->name, "psql_path" ) )
                 att = att->next;
              else
              {
                 atta = att->next;
                 delete_struct( (cdHd *) att );
                    err_r( );
                 att = atta;
              }
           }
        }
     }
  }

  return;
}


/*********************************************************************
 * Function to scan file and add it to a list of spanned files.
 *********************************************************************/


void scn_small_file( cdms_card *line,    /* card cracking struct */
                     span_list *span,    /* file-span struct */
                     char      *mpath )  /* NULL or '.cdms' path */
{
  int        i, j, k, m, n, j1, j2, j3, ct, idx, cnt, yr, mo;
  long       l, l1, l2;
  char       *aa, *uni, *fnm, *pth, msg[120];
  double     *dd, d1, d2, b;
  cdDb_new   *db;
  cdDset_new *ds;
  cdDim_new  *dim;
  cdVar_new  *var;
  cdAtt_new  *att, *atta;

  ct = j1 = j2 = j3 = 0;


 /*---------------------------------
  * Locate '../dir/file tstart tend' symbols on input card.
  * Note: time can be 1 '1994-7-4' or 4 '1994-7-4 10:37' symbols.
  * j1--tstart, j2--tend, j3--file
  *---------------------------------*/


 /* 'file 1979-1-1 1979-8-1'  or  'file -90. +90.' */
  if( line->num_sym == 3 )
  {
     j1 = 1;
     j2 = 2;
  }

 /* 'file 1994-7-4 10:37  1994-8-1 12:0' */
  else if( line->num_sym == 9 && span->flg_dtyp == 1 )
  {
     j1 = 1;
     j2 = 5;
  }

 /* 'file 1994-7-4 10:37' */
  else if( line->num_sym == 5 && span->flg_del == 1
           && span->flg_dtyp == 1 )
  {
     j1 = 1;
     j2 = 1;
  }

 /* 'file 1979-1-1' */
  else if( line->num_sym == 2 && span->flg_del == 1
           && span->flg_dtyp == 1 )
  {
     j1 = 1;
     j2 = 1;
  }

 /* 'file' */
  else if( line->num_sym == 1 && span->flg_del == 0 )
     ;

  else
  {
     err_x( "scn_small_file: bad 'filename dim_bgn dim_end' card" );
  }


 /*---------------------------------
  * Insert symbol termination letter after symbols.
  * Note: ascii symbols don't have '\0'.
  *---------------------------------*/


  if( line->num_sym > 1 )
  {
     j = line->idx_sym[j3];
     k = j + line->len_sym[j3];
     line->asc_line[k] = '\0';

     k = line->idx_sym[j1] - 1;
     line->asc_line[k] = '\0';

     k = line->idx_sym[j2] - 1;
     line->asc_line[k] = '\0';
  }


 /*---------------------------------
  * Get filename and path of input file.
  * Determine if filename is a '.cdms' file
  *---------------------------------*/


  j = line->idx_sym[j3];
  k = j + line->len_sym[j3];

  fnm = cr_asc_cpy( &line->asc_line[j] );

  for( i=k-1; i > j; i-- )
  {
     if( line->asc_line[i] == '/' )
     {
        free( fnm );
        fnm = cr_asc_cpy( &line->asc_line[i+1] );
        break;
     }
  }

  pth = get_path_of_file( &line->asc_line[j], mpath );

  aa = strstr( fnm, ".cdms" );


 /*---------------------------------
  * Scan file into a new database placed at end of tree.
  *---------------------------------*/


  if( aa == NULL )
  {
     k       = FPT_TTY;
     FPT_TTY = 0;

     sprintf( msg, "filename = %s ;", fnm );
     pql_execute( msg );
        err_r( );

     sprintf( msg, "dirtree 1 %s ;", pth );
     pql_execute( msg );
        err_r( );

     FPT_TTY = k;
  }
  else
  {
     sprintf( msg, "%s/%s", pth, fnm );
     i = rd_meta_file( msg );
        err_r( );
  }

  for( db=DB_ROOT_ADR; db->next != NULL; db=db->next )
     ;

  if( db == NULL || db->dsets == NULL || db->dsets->vars == NULL
      || db->dsets->dims == NULL )
  {
     sprintf( msg, "%s/%s", pth, fnm );
     err_m( msg );
     err_x( "scn_small_file: data file bad or not found" );
  }

  ds = db->dsets;


 /*---------------------------------
  * Determine if first file spanned was itself a .cdms file.
  * If true, remove it's 'psql_filelist', etc. attributes.
  *---------------------------------*/


  if( aa != NULL && span->dbs == NULL )
  {
     dim = ds->dims;

     while( dim != NULL )
     {
        atta = scn_lnk_list( "psql_filelist", id_cdAtt, (cdHd *) dim );

        if( atta == NULL )
           dim = dim->next;
        else
        {
           att = dim->atts;

           while( att != NULL )
           {
              aa = strstr( att->name, "psql_" );

              if(  aa == NULL )
                 att = att->next;
              else
              {
                 atta = att->next;
                 delete_struct( (cdHd *) att );
                    err_r( );
                 att = atta;
              }
           }

           att           = cre_struct( id_cdAtt, (cdHd *) dim );
           att->name     = cr_asc_cpy( "psql_file" );
           att->datatype = cdChar;
           att->length   = strlen( fnm );
           att->values   = cr_asc_cpy( fnm );

           att           = cre_struct( id_cdAtt, (cdHd *) dim );
           att->name     = cr_asc_cpy( "psql_path" );
           att->datatype = cdChar;
           att->length   = strlen( pth );
           att->values   = cr_asc_cpy( pth );

           if( dim->data != NULL )
              free( dim->data );
           dim->data = NULL;
        }
     }
  }


 /*---------------------------------
  * Locate spanning dimension in created dataset.
  * Throw out unneeded dim and var not spanable.
  *---------------------------------*/


  scn_small_db( span, db, fnm, pth );
     err_r( );

  dim = scn_lnk_list( span->dname, id_cdDim, (cdHd *) ds );
  if( dim == NULL )
     err_x( "scn_small_file: no span dimension in file" );


 /*---------------------------------
  * Get calendar from input file.
  *---------------------------------*/


  if( span->flg_dtyp == 1 )
  {
     att = nam_fnd( "calendar", id_cdAtt, (cdHd *) ds );

     if( att != NULL )
        ct = typ_of_calendar( (char *) att->values );
     else
        ct = typ_of_calendar( NULL );
  }


 /*---------------------------------
  * Generate time coordinates for 'delta' generate format.
  *---------------------------------*/


  if( span->flg_del == 1 && span->flg_dtyp == 1 \
      && span->select == 0 )
  {
     idx = 0;
     cnt = dim->length;

     dd = (double *) malloc( cnt * sizeof(double) );
     if( dd == NULL )
        err_x( "scn_small_file: trouble getting memory" );

     j = line->idx_sym[j1];
     cdChar2Rel( ct, &line->asc_line[j], span->uni, &dd[0] );

     if( span->flg_uni > 1 )
     {
        for( i=1; i < cnt; i++ )
           dd[i] = dd[i-1] + span->delt;
     }
     else
     {
        m = sscanf( &line->asc_line[j], "%d%*1c%d", &yr, &mo );

        for( i=0; i < cnt; i++ )
        {
           if( i == 0 )
           {
              sprintf( msg, "%4d-%d-%d", yr, mo, 1 );
              cdChar2Rel(ct, msg, span->uni, &d1 );
           }
           else
              d1 = d2;

           if( mo == 12 )
           {
              yr++;
              mo = 1;
           }
           else
              mo++;

           sprintf( msg, "%4d-%d-%d", yr, mo, 1 );
           cdChar2Rel(ct, msg, span->uni, &d2 );

           dd[i] = d1 + (( d2 - d1 ) * 0.5);
        }
     }
  }


 /*---------------------------------
  * Locate time coordinates for 'tstart tend subset' format.
  *---------------------------------*/


  if( span->flg_del == 0 && span->flg_dtyp == 1 \
      && span->select == 0 )
  {
     att = scn_lnk_list( "units", id_cdAtt, (cdHd *) dim );
     if( att == NULL )
        err_x( "scn_small_file: no span units in file" );

     uni = (char *) att->values;

     dd = get_coord( dim, cdDouble, &l );
          err_r( );
     if( l < 1 )
        err_x( "scn_small_file: no span coordinates in file" );

     if( line->num_sym > 1 )
     {
        j = line->idx_sym[j1];
        cdChar2Rel(ct, &line->asc_line[j], uni, &d1 );

        j = line->idx_sym[j2];
        cdChar2Rel(ct, &line->asc_line[j], uni, &d2 );

        d1 = d1 + (1.e-8 * fabs(d1) );
        d2 = d2 - (1.e-8 * fabs(d2) );
     }


    /*---------------------------------
     * Use tstart,tend to generate time axis subset.
     *---------------------------------*/


     if( l == 1 )
     {
        idx = 0;
        cnt = 1;
     }
     else if( line->num_sym == 1 )
     {
        idx = 0;
        cnt = l;
     }
     else
     {
        j1 = j2 = -1;

        if( dd[0] < dd[1] )
        {
           for( i=0; i < l-1; i++ )
           {
              b = dd[i] + ( 0.5 * (dd[i+1] - dd[i]) );

              if( j1 == -1 && d1 < b )
                 j1 = i;
              if( j2 == -1 && d2 < b )
                 j2 = i;
           }

           if( j1 == -1 )
              j1 = l - 1;
           if( j2 == -1 )
              j2 = l - 1;
        }
        else
        {
           for( i=l-1; i > 1; i-- )
           {
              b = dd[i] + ( 0.5 * (dd[i-1] - dd[i]) );

              if( j1 == -1 && d1 < b )
                 j1 = i;
              if( j2 == -1 && d2 < b )
                 j2 = i;
           }

           if( j1 == -1 )
              j1 = 0;
           if( j2 == -1 )
              j2 = 0;
        }

        if( j1 < j2 )
        {
           idx = j1;
           cnt = (j2 - j1) + 1;
        }
        else
        {
           idx = j2;
           cnt = (j1 - j2) + 1;
        }

        if( (idx + cnt) > l )
           cnt = l - idx;
     }
  }


 /*---------------------------------
  * Locate lat-lon-lev coordinates for 'tstart tend subset' format.
  *---------------------------------*/


  if( span->flg_del == 0 && span->flg_dtyp == 0 \
      && span->select == 0 )
  {
     uni = NULL;

     dd = get_coord( dim, cdDouble, &l );
          err_r( );
     if( l < 1 )
        err_x( "scn_small_file: no span coordinates in file" );

     if( line->num_sym > 1 )
     {
        j = line->idx_sym[j1];

        if( line->cls_sym[j1] == 'f' )
           d1 = atof( &line->asc_line[j] );
        else
           d1 = atol( &line->asc_line[j] );

        j = line->idx_sym[j2];

        if( line->cls_sym[j2] == 'f' )
           d2 = atof( &line->asc_line[j] );
        else
           d2 = atol( &line->asc_line[j] );

        d1 = d1 + (1.e-8 * fabs(d1) );
        d2 = d2 - (1.e-8 * fabs(d2) );
     }


    /*---------------------------------
     * Use tstart,tend to locate lat-lon-lev axis subset.
     *---------------------------------*/


     if( l == 1 )
     {
        idx = 0;
        cnt = 1;
     }
     else if( line->num_sym == 1 )
     {
        idx = 0;
        cnt = l;
     }
     else
     {
        j1 = j2 = -1;

        if( dd[0] < dd[1] )
        {
           for( i=0; i < l-1; i++ )
           {
              b = dd[i] + ( 0.5 * (dd[i+1] - dd[i]) );

              if( j1 == -1 && d1 < b )
                 j1 = i;
              if( j2 == -1 && d2 < b )
                 j2 = i;
           }

           if( j1 == -1 )
              j1 = l - 1;
           if( j2 == -1 )
              j2 = l - 1;
        }
        else
        {
           for( i=l-1; i > 1; i-- )
           {
              b = dd[i] + ( 0.5 * (dd[i-1] - dd[i]) );

              if( j1 == -1 && d1 < b )
                 j1 = i;
              if( j2 == -1 && d2 < b )
                 j2 = i;
           }

           if( j1 == -1 )
              j1 = 0;
           if( j2 == -1 )
              j2 = 0;
        }

        if( j1 < j2 )
        {
           idx = j1;
           cnt = (j2 - j1) + 1;
        }
        else
        {
           idx = j2;
           cnt = (j1 - j2) + 1;
        }

        if( (idx + cnt) > l )
           cnt = l - idx;
     }
  }


 /*---------------------------------
  * Locate axis subset coordinates from integer 'tstart tend' format.
  * 'idx idx' 1's orgin index, 1's orgin index
  * 'idx cnt' 1's orgin index, count
  * 'off off' 0's orgin index, 0's orgin index
  * 'off cnt' 0's orgin index, count
  *---------------------------------*/


  if( span->select > 0 )
  {
     uni = NULL;

     dd = get_coord( dim, cdDouble, &l );
          err_r( );
     if( l < 1 )
        err_x( "scn_small_file: no span coordinates in file" );

     if( line->num_sym > 1 )
     {
        j = line->idx_sym[j1];

        if( line->cls_sym[j1] == 'f' )
           l1 = atof( &line->asc_line[j] );
        else
           l1 = atol( &line->asc_line[j] );

        j = line->idx_sym[j2];

        if( line->cls_sym[j2] == 'f' )
           l2 = atof( &line->asc_line[j] );
        else
           l2 = atol( &line->asc_line[j] );
     }


    /*---------------------------------
     * Use tstart,tend to locate lat-lon-lev axis subset.
     *---------------------------------*/


     if( l == 1 )
     {
        idx = 0;
        cnt = 1;
     }
     else if( line->num_sym == 1 )
     {
        idx = 0;
        cnt = l;
     }
     else if( span->select == 1 )
     {
        idx = l1 - 1;
        cnt = l2 - l1 + 1;
     }
     else if( span->select == 2 )
     {
        idx = l1 - 1;
        cnt = l2;
     }
     else if( span->select == 3 )
     {
        idx = l1;
        cnt = l2 - l1 + 1;
     }
     else if( span->select == 4 )
     {
        idx = l1;
        cnt = l2;
     }

     if( idx < 0 )
        idx = 0;

     if( cnt < 1 )
        cnt = 1;

     if( (idx + cnt) > l )
        cnt = l - idx;
  }


 /*---------------------------------
  * Get memory for file in the spanning struct.
  *---------------------------------*/


  span = mem_span_list( span, 1, cnt );
            err_r( );
         

 /*---------------------------------
  * Add file to spanning struct.
  *---------------------------------*/


  n = span->n_flist;

  span->flist[n] = fnm;
  span->plist[n] = pth;
  span->fdoff[n] = idx;

  j = idx;
  k = span->n_cord;

  for( i=0; i < cnt; i++ )
  {
/*........................previous logic
     if( span->flg_del == 1 || span->flg_dtyp == 0
         || span->uni == NULL )
        d1 = dd[j];
     else
     {
        cdRel2Char( ct, uni, dd[j], msg );
        cdChar2Rel( span->ct, msg, span->uni, &d1 );
     }

     span->fpoint[k] = n;
     span->scord[k]  = d1;
     j++;
     k++;
......................................*/
     span->fpoint[k] = n;
     span->scord[k]  = dd[j];
     j++;
     k++;
  }

  span->n_flist += 1;
  span->n_cord  += cnt;


 /*---------------------------------
  * Delete database and scratch storage.
  *---------------------------------*/


  free( dd );

  if( span->dbs == NULL )
  {
     span->dbs = db;
     span->ct  = ct;
     if( span->flg_del == 0 && uni != NULL )
        span->uni = cr_asc_cpy( uni );
  }
  else
  {
     delete_struct( (cdHd *) db );
        err_r( );
  }
  return;
}


/*********************************************************************
 * Function to give ascii string label for CDMS type.
 *********************************************************************/


char *typ_as_ascii( cdType  dtype )  /* cdms type */
{


 /*---------------------------------
  * Scan CDMS type.
  *---------------------------------*/


  if( dtype == cdByte )
     return cr_asc_cpy( "byte" );

  else if( dtype == cdShort )
     return cr_asc_cpy( "short" );

  else if( dtype == cdChar || dtype == cdCharTime )
     return cr_asc_cpy( "char" );

  else if( dtype == cdInt )
     return cr_asc_cpy( "int" );

  else if( dtype == cdLong )
     return cr_asc_cpy( "long" );

  else if( dtype == cdFloat )
     return cr_asc_cpy( "float" );

  else if( dtype == cdDouble || dtype == cdLongDouble )
     return cr_asc_cpy( "double" );

  else
  {
     err_xv( "typ_as_ascii: bad cdms type" );
  }
}


/*********************************************************************
 * Function to give CDMS type from ascii string label.
 *********************************************************************/


cdType typ_from_ascii( char *aa )  /* ascii type */
{


 /*---------------------------------
  * Scan ascii string.
  *---------------------------------*/


  if( !strcmp( "byte", aa ) )
     return cdByte;

  else if( !strcmp( "char", aa ) )
     return cdChar;

  else if( !strcmp( "ascii", aa ) )
     return cdChar;

  else if( !strcmp( "short", aa ) )
     return cdShort;

  else if( !strcmp( "int", aa ) )
     return cdInt;

  else if( !strcmp( "long", aa ) )
     return cdLong;

  else if( !strcmp( "float", aa ) )
     return cdFloat;

  else if( !strcmp( "double", aa ) )
     return cdDouble;

  else if( !strcmp( "longdouble", aa ) )
     return cdLongDouble;

  else if( !strcmp( "chartime", aa ) )
     return cdCharTime;

  else
  {
     err_m( "typ_from_ascii: bad ascii type " );
     ERR_no = 1;
     return cdByte;
  }
}


/*********************************************************************
 * Function to give CDMS type & type length given a "cdunif" type.
 *********************************************************************/


void typ_from_cdunif( CuType dtype,  /* "cdunif" type */
                      cdType *typ,   /* CDMS type */
                      int    *len )  /* sizeof type */
{


 /*---------------------------------
  * Scan "cdunif" type.
  *---------------------------------*/


  if( dtype == CuByte )
  {
     *typ = cdByte;
     *len = sizeof( unsigned char );
  }

  else if( dtype == CuChar )
  {
     *typ = cdChar;
     *len = sizeof( char );
  }

  else if( dtype == CuShort )
  {
     *typ = cdShort;
     *len = sizeof( short );
  }

  else if( dtype == CuInt )
  {
     *typ = cdInt;
     *len = sizeof( int );
  }

  else if( dtype == CuLong )
  {
     *typ = cdLong;
     *len = sizeof( long );
  }

  else if( dtype == CuFloat )
  {
     *typ = cdFloat;
     *len = sizeof( float );
  }

  else if( dtype == CuDouble )
  {
     *typ = cdDouble;
     *len = sizeof( double );
  }

  else if( dtype == CuLongDouble )
  {
     *typ = cdLongDouble;
     *len = sizeof( double );
  }

  else
  {
     err_x( "typ_from_cdunif: bad cdunif type" );
  }
  return;
}


/*********************************************************************
 * Function to give calendar id from ascii calendar name.
 *********************************************************************/


int typ_of_calendar( char *aa )  /* ascii calendar name */
{
  int  i, ct;
  char *bb;

  char *calnam[6] =
         {"gregorian", "julian", "noleap", "d360", "clim", "climleap"};

  int calid[6] =
          {cdStandard, cdJulian, cdNoLeap, cd360, cdClim, cdClimLeap};


 /*---------------------------------
  * Convert ascii string to lower case letters.
  *---------------------------------*/


  ct = calid[0];

  bb = cr_asc_cpy( aa );

  if( bb == NULL )
     return ct;

  for( i=0; i < strlen( aa ); i++ )
     bb[i] = tolower( aa[i] );


 /*---------------------------------
  * Get calender id for the time routines.
  *---------------------------------*/



  for( i=0; i < 6; i++ )
  {
     if( !strcmp( bb, calnam[i] ) )
        ct = calid[i];
  }

  free( bb );
  return ct;
}


/*********************************************************************
 * Function to check if dimension is a 'time' dimension.
 *********************************************************************/


int typ_time_dim( char *dnam )  /* dimension name */
{
  int i, k, n;


 /*---------------------------------
  * Scan dimension name
  *---------------------------------*/


  i = 0;
  n = strlen( dnam );

  if( n > 0 )
  {
     k = dnam[0];

     if( k == 't' || k == 'T' )
        i = 1;
  }

  if( i == 1 && n > 1 )
  {
     i = 0;
     k = dnam[1];

     if( k == 'i' || k == 'I' )
        i = 1;
  }

  if( i == 1 && n > 2 )
  {
     i = 0;
     k = dnam[2];

     if( k == 'm' || k == 'M' )
        i = 1;
  }

  if( i == 1 && n > 3 )
  {
     i = 0;
     k = dnam[3];

     if( k == 'e' || k == 'E' )
        i = 1;
  }

  return i;
}


/*********************************************************************
 * Function to give cdunif type given CDMS type.
 *********************************************************************/


void typ_to_cdunif( cdType dtype,   /* cdms type */
                    CuType *typ,    /* cdunif type */
                    int    *len )   /* sizeof type */
{


 /*---------------------------------
  * Scan CDMS type.
  *---------------------------------*/


  if( dtype == cdByte )
  {
     *typ = CuByte;
     *len = sizeof( unsigned char );
  }

  else if( dtype == cdChar )
  {
     *typ = CuChar;
     *len = sizeof( char );
  }

  else if( dtype == cdShort )
  {
     *typ = CuShort;
     *len = sizeof( short );
  }

  else if( dtype == cdInt )
  {
     *typ = CuInt;
     *len = sizeof( int );
  }

  else if( dtype == cdLong )
  {
     *typ = CuLong;
     *len = sizeof( long );
  }

  else if( dtype == cdFloat )
  {
     *typ = CuFloat;
     *len = sizeof( float );
  }

  else if( dtype == cdDouble )
  {
     *typ = CuDouble;
     *len = sizeof( double );
  }

  else if( dtype == cdLongDouble )
  {
     *typ = CuLongDouble;
     *len = sizeof( double );
  }

  else
  {
     err_x( "typ_to_cdunif: bad cdms type" );
  }
  return;
}


/*********************************************************************
 * Function to give netcdf type given CDMS type.
 *********************************************************************/


void typ_to_netcdf( cdType  dtype,   /* cdms type */
                    nc_type *typ,    /* netcdf type */
                    int     *len )   /* sizeof type */
{


 /*---------------------------------
  * Scan CDMS type.
  *---------------------------------*/


  if( dtype == cdByte )
  {
     *typ = NC_BYTE;
     *len = sizeof( unsigned char );
  }

  else if( dtype == cdChar || dtype == cdCharTime )
  {
     *typ = NC_CHAR;
     *len = sizeof( char );
  }

  else if( dtype == cdShort )
  {
     *typ = NC_SHORT;
     *len = sizeof( short );
  }

  else if( dtype == cdInt || dtype == cdLong )
  {
     *typ = NC_LONG;
     *len = sizeof( long );
  }

  else if( dtype == cdFloat )
  {
     *typ = NC_FLOAT;
     *len = sizeof( float );
  }

  else if( dtype == cdDouble || dtype == cdLongDouble )
  {
     *typ = NC_DOUBLE;
     *len = sizeof( double );
  }

  else
  {
     err_x( "typ_to_netcdf: bad cdms type" );
  }
  return;
}


/*********************************************************************
 * Function to write dataset struct to NetCDF file.
 *********************************************************************/


void wrt_ds_to_netcdf( cdDset_new *dset,  /* dataset struct */
                       char       *nam,   /* filename */
                       char       *tim )  /* record dimname */
{
  int       i, j, k, lentim, m, ndim, nvar, *dim_id, *var_id, 
            *v_dims, ncid, idxtim;
  long      l, *idx_bgn, *idx_cnt, *recl;
  nc_type   typ;
  cdDim_new *dim, *dima;
  cdAtt_new *att;
  cdVar_new *var, *vara;
  cdTmp     *tmp;
  void      *vv, **recv;


 /*---------------------------------
  * Create the output file.
  *---------------------------------*/


  ncid = nccreate( nam, NC_CLOBBER );
  if( ncid < 0 )
     err_x( "wrt_ds_to_netcdf: netCDF nccreate trouble" );

/*............
  k = ncredef( ncid );
  if( k < 0 )
     err_x( "wrt_ds_to_netcdf: netCDF ncredef trouble" );
..............*/


 /*---------------------------------
  * Get memory for scratch arrays.
  *---------------------------------*/


  for( dim=dset->dims, ndim=0; dim; dim=dim->next )
        ndim++;

  for( var=dset->vars, nvar=ndim; var; var=var->next )
        nvar++;

  v_dims  =  (int *) malloc( ndim * sizeof( int ) );
  dim_id  =  (int *) malloc( nvar * sizeof( int ) );
  var_id  =  (int *) malloc( nvar * sizeof( int ) );
  idx_bgn = (long *) malloc( ndim * sizeof( long ) );
  idx_cnt = (long *) malloc( ndim * sizeof( long ) );
  recl    = (long *) malloc( nvar * sizeof( long ) );
  recv    =          malloc( nvar * sizeof( vv ) );

  if( dim_id == NULL || var_id == NULL || idx_bgn == NULL ||
      idx_cnt == NULL || recl == NULL || recv == NULL )
     err_x( "wrt_ds_to_netcdf: trouble getting memory" );


 /*---------------------------------
  * Find netcdf 'record' dimension.
  *---------------------------------*/


  idxtim = -1;
  lentim = -1;

  for( dim=dset->dims, i=0; dim; dim=dim->next, i++ )
  {
     if( !strcmp( dim->name, tim ) && dim->length > 0 )
     {
        idxtim = i;
        lentim = dim->length;
     }
  }


 /*---------------------------------
  * Define global attributes in output file.
  *---------------------------------*/


  for( att=dset->atts; att; att=att->next )
  {
     if( att->datatype != cdCharTime
         && strstr( att->name, "psql_" ) == NULL )
     {
        typ_to_netcdf( att->datatype, &typ, &m );
           err_r( );

        m = att->length;
        if( typ == NC_CHAR )
           m = strlen( att->values );

        k = ncattput( ncid, NC_GLOBAL, att->name, typ, m,
                      att->values );
        if( k < 0 )
           err_x( "wrt_ds_to_netcdf: netCDF ncattput trouble" );
     }
  }


 /*---------------------------------
  * Define dimensions in output file.
  * NOTE: dim not also a var if =="bound" or dim->length < 1
  *---------------------------------*/


  for( dim=dset->dims, i=0; dim; dim=dim->next, i++ )
  {
     if( i == idxtim )
     {
        recl[i]   = 1;
        recv[i]   = dim->data;
        dim_id[i] = ncdimdef( ncid, dim->name, NC_UNLIMITED );
     }
     else
     {
        recl[i]   = 0;
        recv[i]   = NULL;
        dim_id[i] = ncdimdef( ncid, dim->name, dim->length );
     }

     if( dim_id[i] < 0 )
        err_x( "wrt_ds_to_netcdf: netCDF ncdimdef trouble" );

     if( !strcmp( dim->name, "bound" ) || dim->length < 1 )
     {
        var_id[i] = -1;
     }
     else
     {
        typ_to_netcdf( dim->datatype, &typ, &m );
           err_r( );

        v_dims[0] = dim_id[i];

        var_id[i] = ncvardef( ncid, dim->name, typ, 1, v_dims );
        if( var_id[i] < 0 )
           err_x( "wrt_ds_to_netcdf: netCDF ncvardef trouble" );

        for( att=dim->atts; att; att=att->next )
        {
           if( att->datatype != cdCharTime
               && strstr( att->name, "psql_" ) == NULL )
           {
              typ_to_netcdf( att->datatype, &typ, &m );
                 err_r( );

              m = att->length;
              if( typ == NC_CHAR )
                 m = strlen( att->values );

              k = ncattput( ncid, var_id[i], att->name, typ, m, 
                            att->values );
              if( k < 0 )
                 err_x( "wrt_ds_to_netcdf: netCDF ncattput trouble" );
           }
        }
     }
  }


 /*---------------------------------
  * Move first var to last so netcdf lists it as last.
  *---------------------------------*/


  vara = dset->vars;

  if( vara->ndims > 2 && vara->next != NULL )
  {
     dset->vars = vara->next;

     for( var=dset->vars; var->next != NULL; var=var->next )
        ;

     var->next  = vara;
     vara->next = NULL;
  }


 /*---------------------------------
  * Define quantity in output file.
  *---------------------------------*/


  for( var=dset->vars, i=ndim; var; var=var->next, i++ )
  {
     recl[i] = 0;
     recv[i] = NULL;

     for( tmp=var->dim, j=0; j < var->ndims; tmp=tmp->next, j++ )
     {
        dima = tmp->want;

        for( dim=dset->dims, k=0; dim; dim=dim->next, k++ )
        {
           if( dim == dima )
           {
              v_dims[j] = dim_id[k];

              if( k == idxtim )
              {
                 recl[i] = var->length / lentim;
                 recv[i] = var->data;
              }
           }
        }
     }

     typ_to_netcdf( var->datatype, &typ, &m );
        err_r( );

     var_id[i] = ncvardef( ncid, var->name, typ, var->ndims, v_dims );
     if( var_id[i] < 0 )
        err_x( "wrt_ds_to_netcdf: netCDF ncvardef trouble" );

     for( att=var->atts; att; att=att->next )
     {
        if( att->datatype != cdCharTime
            && strstr( att->name, "psql_" ) == NULL )
        {
           typ_to_netcdf( att->datatype, &typ, &m );
              err_r( );

           m = att->length;
           if( typ == NC_CHAR )
              m = strlen( att->values );

           k = ncattput( ncid, var_id[i], att->name, typ,
                         m, att->values );
           if( k < 0 )
              err_x( "wrt_ds_to_netcdf: netCDF ncattput trouble" );
        }
     }
  }


 /*---------------------------------
  * Terminate Netcdf define mode.
  *---------------------------------*/


  k = ncsetfill( ncid, NC_NOFILL );
  if( k < 0 )
     err_x( "wrt_ds_to_netcdf: netCDF ncsetfill trouble" );

  k = ncendef( ncid );
  if( k < 0 )
     err_x( "wrt_ds_to_netcdf: netCDF ncendef trouble" );


 /*---------------------------------
  * Store non-record dimension arrays in output file.
  *---------------------------------*/


  for( dim=dset->dims, i=0; dim; dim=dim->next, i++ )
  {
     if( recl[i] == 0 && var_id[i] != -1 )
     {
        idx_bgn[0] = 0;
        idx_cnt[0] = dim->length;

        k = ncvarput( ncid, var_id[i], idx_bgn, idx_cnt, dim->data );
        if( k < 0 )
           err_x( "wrt_ds_to_netcdf: netCDF ncvarput trouble" );
     }
  }


 /*---------------------------------
  * Store non-record variable arrays in output file.
  *---------------------------------*/


  for( var=dset->vars, i=ndim; var; var=var->next, i++ )
  {
     if( recl[i] == 0 )
     {
        for( tmp=var->dim, j=0; j < var->ndims; tmp=tmp->next, j++ )
        {
           dim        = tmp->want;
           idx_bgn[j] = 0;
           idx_cnt[j] = dim->length;
        }

        k = ncvarput( ncid, var_id[i], idx_bgn, idx_cnt, var->data );
        if( k < 0 )
           err_x( "wrt_ds_to_netcdf: netCDF ncvarput trouble" );
     }
  }


 /*---------------------------------
  * Store record dimension,variable arrays in output file.
  *---------------------------------*/


  for( l=0; l < lentim; l++ )
  {
     m = 0;

     for( dim=dset->dims, i=0; dim; dim=dim->next, i++ )
     {
        if( recl[i] > 0 )
        {
           recv[m++] = ary_off( dim->datatype, l * recl[i],
                                dim->data );
        }
     }

     for( var=dset->vars, i=ndim; var; var=var->next, i++ )
     {
        if( recl[i] > 0 )
        {
           recv[m++] = ary_off( var->datatype, l * recl[i],
                                var->data );
        }
     }

     k = ncrecput( ncid, l, recv );
     if( k < 0 )
        err_x( "wrt_ds_to_netcdf: netCDF ncrecput trouble" );
  }


 /*---------------------------------
  * Disconnect output file and free up memory.
  *---------------------------------*/


  k = ncclose( ncid );
  if( k < 0 )
     err_x( "wrt_ds_to_netcdf: netCDF ncclose trouble" );


  free( v_dims );
  free( dim_id );
  free( var_id );
  free( idx_bgn );
  free( idx_cnt );
  free( recl );
  free( recv );
  return;
}


/*********************************************************************
 * Function to print an ascii message.
 *********************************************************************/


void wrt_msg( void )
{
  int  i;
  char *aa;


 /*---------------------------------
  * print message to tty.
  *---------------------------------*/


  if( FPT_TTY )
     printf( "%s", PSQL_MSG );

 /*---------------------------------
  * print message to ascii file.
  *---------------------------------*/


  if( FPT_OUT != NULL )
     fprintf( FPT_OUT, "%s", PSQL_MSG );


 /*---------------------------------
  * accumulate message to send to python.
  *---------------------------------*/


  if( FPT_PY )
  {
    /*---------------------------------
     * get memory for python message array.
     *---------------------------------*/


     i = strlen( PSQL_MSG );

     if( (l_PY_PSQL_MSG + 120 + i) > L_PY_PSQL_MSG )
     {
         L_PY_PSQL_MSG += 4096;

         aa    = malloc( L_PY_PSQL_MSG );
         aa[0] = '\0';

         strcat( aa, PY_PSQL_MSG );

         free( PY_PSQL_MSG );

         PY_PSQL_MSG = aa;
     }


    /*---------------------------------
     * add message to end to python message array.
     *---------------------------------*/


     strcat( PY_PSQL_MSG, PSQL_MSG );

     l_PY_PSQL_MSG += i;
  }
  return;
}


/*********************************************************************
 * Function to process special-small-spanning-metafile.
 *********************************************************************/


/*--------------------------------------------------------------------
 * write spanning file with coordinates and attributes stripped off

 * Note: may induce error's in codes which use this to read files
 *-------------------------------------------------------------------*/

void wrt_small_file( cdHd *hd,     /* ds_group dataset struct */
                     char *unam )  /* mo,da,hr units flag */
{
  int        i, j, j1, j2, k, n, *ilist, ct;
  char       *uni, **flist, t1[120], t2[120];
  double     *dd;
  cdDset_new *ds;
  cdDim_new  *dim;
  cdAtt_new  *att;


 /*---------------------------------
  * Print first card: name of spanning dimension.
  *---------------------------------*/


  ds = (cdDset_new *) hd;

  if( strcmp(ds->name, "ds_group") )
     err_x( "wrt_small_file: bad spanning dataset" );

  dim = ds->dims;
  fprintf( FPT_OUT, "%s\n", dim->name );


 /*---------------------------------
  * Get coordinates, calendar, units to use.
  * Print 2nd card -- units; 3rd card -- delta
  *---------------------------------*/


  if( dim->datatype != cdDouble || dim->data == NULL )
     err_x( "wrt_small_file: bad dim-type" );
  dd = (double *) dim->data;

  att = scn_lnk_list( "units", id_cdAtt, (cdHd *) dim );
           err_r( );
  err_t( att==NULL, "wrt_small_file: no dim-units" );
  uni = (char *) att->values;

  if( unam != NULL && unam[0] == 'M' )
  {
     fprintf( FPT_OUT, "Months since 1979-1-1\n" );
     fprintf( FPT_OUT, "%e\n", 1.0 );
  }
  else if( unam != NULL && unam[0] == 'm' )
  {
     fprintf( FPT_OUT, "months since 1979-1-1\n" );
     fprintf( FPT_OUT, "%e\n", 1.0 );
  }
  else if( unam != NULL && unam[0] == 'd' )
  {
     fprintf( FPT_OUT, "days since 1979-1-1\n" );
     fprintf( FPT_OUT, "%e\n", 1.0 );
  }
  else
  {
     fprintf( FPT_OUT, "days since 1979-1-1\n" );
     fprintf( FPT_OUT, "%e\n", 0.25 );
  }

  att = nam_fnd( "calendar", id_cdAtt, (cdHd *) dim );

  if( att != NULL )
     ct = typ_of_calendar( (char *) att->values );
  else
     ct = typ_of_calendar( NULL );


 /*---------------------------------
  * Print 'file-tstart-tend' triple cards.
  *---------------------------------*/


  att = scn_lnk_list( "psql_filelist", id_cdAtt, (cdHd *) dim );
           err_r( );
  err_t( att==NULL, "wrt_small_file: no dim-filelist" );
  flist = att->values;

  att = scn_lnk_list( "psql_filepoint", id_cdAtt, (cdHd *) dim );
           err_r( );
  err_t( att==NULL, "wrt_small_file: no dim-filepoint" );
  ilist = (int *) att->values;

  j1 = j2 = k = 0;
  n  = dim->length;

  for( i=0; i < n; i++ )
  {
     if( ilist[i] != k || i == n - 1 )
     {
        if( i == n - 1 )
           j2 = i;
        else
           j2 = i - 1;

        cdRel2Char( ct, uni, dd[j1], t1 );
        cdRel2Char( ct, uni, dd[j2], t2 );

        fprintf( FPT_OUT, "%s  %s  %s\n", flist[k], t1, t2 );
        j1 = i;
        k  = ilist[i];
     }
  }

  return;
}
/*--------------------------------------------------------------------
/home/oconnor/wrk/cdat/python15/Modules/cdunifmodule.c
#include "Python.h"
#include "arrayobject.h"
#include <string.h>
#include <math.h>
#include "clui_output.h"
#include "cdms.h"

int get_bw(char    *grid_name    -- "gaussian"  "lmd"  "linear"
                                    "Unknown"   "csu"  "gis"
           char    *dimname      -- "latitude"  "longitude"
                                    "level"     "time"
           long    orig_dimlen   -- length of dimension
           double  orig_start    -- first dimension coordinate value
           double  orig_end      -- last dimension coordinate value
           long    len           -- length of subset
           double  start         -- first coordinate of subset
           double  end           -- last coordinate of subset
           double  *orig_values  -- dimension coordinates
           double  *values       -- subset dimension coordinates
           double  **bounds      -- output n+1 bounds array
           double  **weights)    -- output n weights array
--------------------------------------------------------------------*/


/* from........vi /home/oconnor/wrk/vcs/vcs_script/misc.c
              Compare only printing characters (non-case sensitive)
                 in two strings.

                Return =0 - if they are identical.
                Return >0 - if string 2 is larger.
                Return <0 - if string 1 is larger.                  */

    int qlcmpncs(s1,s2)
      char *s1,*s2;
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

int qlget_bw(char *grid_name, char *dimname, long orig_dimlen, double orig_start, double orig_end, long len, double start, double end, double *orig_values, double *values, double **bounds, double **weights)
{
   double *bn, *bs, *be, *bw, *bnds, *wgts, *paa;
   double *regionvals, *regionbn, *regionbs, *regionpw, *rbnds;
   int k;
   char buf[1024];

extern void qlunlatitude(int nlat, float blat, float elat, double *bn, double *bs,double *paa, double *pw);
extern void qlunlongitude(int nlon,float blon, float elon, double *be, double *bw, double *pw);
extern void qlgaubounds(int nlat, float blat, float elat,
                         double *bn, double *bs, double *paa, double *pw);
extern void qlequalarea(int nlati, float blat, float elat, double *bn, double *bs, double *paa, double *pw);
extern void qlgislatitude(int nlati, float blat, float elat, double *bn, double *bs, double *paa, double *pw);
extern void qlgenwtsbnds(int np, double *xp, double *bnds, double *wtp);
extern void qllatregion(int nlat, float blat, float elat, double *latvals, double *bnds, double *pw, double *regionvals, double *regionbn, double *regionbs, double *regionpw);
extern void qllevelregion(int nlat, float blat, float elat, double *latvals, double *bnds, double *pw, double *regionvals, double *rbnds, double *regionpw);



  if((qlcmpncs(dimname, "latitude") == 0) || (strncmp(dimname, "lat",3) == 0))
  {
     paa = (double *)malloc(orig_dimlen*sizeof(double));
     bnds = (double *)malloc((orig_dimlen+1)*sizeof(double));
     wgts = (double *)malloc(orig_dimlen*sizeof(double));
     rbnds = (double *)malloc((len+1)*sizeof(double));
     regionpw = (double *)malloc(len*sizeof(double));
     regionvals = (double *)malloc(len*sizeof(double));

     bn = (double *)malloc(orig_dimlen*sizeof(double));
     if(!bn) {
         printf("memory request failed for bn");
     }

     bs = (double *)malloc(orig_dimlen*sizeof(double));
     if(!bs) {
         printf("memory request failed for bs");
     }
     regionbn = (double *)malloc(len*sizeof(double));
     regionbs = (double *)malloc(len*sizeof(double));

     if(qlcmpncs(grid_name, "gaussian") == 0) 
       qlgaubounds(orig_dimlen,orig_start,orig_end, bn, bs, paa, wgts);
     else if(qlcmpncs(grid_name, "lmd") == 0) 
       qlequalarea(orig_dimlen,orig_start,orig_end, bn, bs, paa, wgts);
     else if((qlcmpncs(grid_name, "linear") == 0) ||
            (qlcmpncs(grid_name, "Unknown") == 0)) 
       qlunlatitude(orig_dimlen,orig_start,orig_end,bn, bs, paa, wgts);
     else if(qlcmpncs(grid_name, "csu") == 0) 
       qlunlatitude(orig_dimlen,orig_start,orig_end,bn, bs, paa, wgts);
     else if(qlcmpncs(grid_name, "gis") == 0) 
     {
       if((abs(orig_start) - 89.0) < 1.e-3)
         qlgislatitude(orig_dimlen,orig_start,orig_end,bn,bs, paa, wgts);
       else
         qlunlatitude(orig_dimlen,orig_start,orig_end,bn, bs, paa, wgts);
     }


     if(orig_start > orig_end)
     {
       bnds[0] = bn[0];
       for(k=1; k<=orig_dimlen; k++)
         bnds[k] = bs[k-1];
     }
     else
     {
       bnds[0] = bs[0];
       for(k=1; k<=orig_dimlen; k++)
         bnds[k] = bn[k-1];
     }

     free(bs);
     free(bn);

     qllatregion(orig_dimlen, start, end, paa, bnds, wgts, 
               regionvals, regionbn, regionbs, regionpw);

     if(start > end)
     {
       rbnds[0] = regionbn[0];
       for(k=1; k<=len; k++)
        rbnds[k] = regionbs[k-1];
     }
     else
     {
       rbnds[0] = regionbs[0];
       for(k=1; k<=len; k++)
         rbnds[k] = regionbn[k-1];
     }

     *bounds = rbnds;
     *weights = regionpw;

					     /* wgts is separate from regionpw, which is the returned */
					     /* weights array here, so wgts should be freed. Similarly, */
					     /* bnds is separate from rbnds, which is the returned array. */
     free(wgts);
     free(bnds);
     free(regionbn);
     free(regionbs);
     free(regionvals);
     free(paa);
  }
  else if((qlcmpncs(dimname, "longitude") == 0) || (strncmp(dimname, "lon",3) == 0))
  {
     bnds = (double *)malloc((len+1)*sizeof(double));
     wgts = (double *)malloc(len*sizeof(double));
     be = (double *)malloc(len*sizeof(double));
     if(!be) {
         printf("memory request failed for be");
     }
     bw = (double *)malloc(len*sizeof(double));
     if(!bw) {
         printf("memory request failed for bw");
     }

     qlunlongitude(len, start, end, be, bw, wgts);

     if(start > end)
     {
       bnds[0] = be[0];
       for(k=1; k<=len; k++)
        bnds[k] = bw[k-1];
     }
     else
     {
       bnds[0] = bw[0];
       for(k=1; k<=len; k++)
         bnds[k] = be[k-1];
     }

     *bounds = bnds;
     *weights = wgts;

     free(bw);
     free(be);
  }
  else
  {
     if(orig_values != NULL)     /*pressure level*/
     {
       bnds = (double *)malloc((orig_dimlen+1)*sizeof(double));
       wgts = (double *)malloc(orig_dimlen*sizeof(double));
       rbnds = (double *)malloc((len+1)*sizeof(double));
       regionpw = (double *)malloc(len*sizeof(double));
       regionvals = (double *)malloc(len*sizeof(double));

       qlgenwtsbnds(orig_dimlen, orig_values, bnds, wgts);
       qllevelregion(orig_dimlen, start, end, orig_values, bnds, wgts,
                   regionvals, rbnds, regionpw);

       *bounds = rbnds;
       *weights = regionpw;

       free(bnds);
       free(wgts);
       free(regionvals);
     }
     else                      /*time*/
     {
     /*  bnds = (double *)malloc((orig_dimlen+1)*sizeof(double));
       wgts = (double *)malloc(orig_dimlen*sizeof(double));
       qlgenwtsbnds(orig_dimlen, values, bnds, wgts);
     */
       bnds = (double *)malloc((len+1)*sizeof(double));
       wgts = (double *)malloc(len*sizeof(double));
       qlgenwtsbnds(len, values, bnds, wgts);
       *bounds = bnds;
       *weights = wgts;
     }
  }

  /* no error */
  return 0;
}
/*-----------------------------------------------------------------------*/
/* /home/oconnor/wrk/cdat/python15/Modules/pcmdimodule.c                 */
/*                                                                       */
/* Python module interface to the PCMDI routines                         */
/* ReGridder  - re-grid a slab of data to the desired grid               */
/*                                                                       */
/* copywrite 1997 Susan Marlais, Lawrence Livermore National Laboratory  */
/* Version dated 8/27/97                                                 */
/*-----------------------------------------------------------------------*/

/*#include "arrayobject.h"*/
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
/*#include "clui_output.h"*/

/*
 *      Specific details of a given architecture for directory information.
 */
#include <string.h>    /* string.h - Include file for strings. */
#include <dirent.h>    /* dirent.h - Include file for directory information. */
#include <sys/stat.h>  /* stat.h - Include file for directory status. */
#include <sys/param.h> /* param.h - This file is intended to contain the basic
#include <sys/types.h> /* types.h - Includes all systems types. */

#ifdef DRS
#include "drscdf.h" /* DRS include file */
#endif
#include "netcdf.h" /* netCDF include file */
#ifdef HDF
#include "hdf.h"    /* HDF include file */
#endif


#define MAX_PATH_LEN 1024
#define F_OK 0
#define STRMAX 256
#define TIME 1


/*--------------------------- Prototypes --------------------------------*/

void qlmaparea(int nloni, int nlono, int nlati, int nlato,
             float *bnin, float *bnout, float *bsin, float *bsout,
             float *bein, float *beout, float *bwin, float *bwout,
             int *londx,  int *lonpt, float *wtlon,
             int *latdx,  int *latpt, float *wtlat);

void qlrgdarea(int ilong, int ilat, int itim1, int itim2, int ntim1,
             int ntim2, int nloni, int nlono, int nlati, int nlato,
             int *londx,  int *lonpt, float *wtlon,
             int *latdx,  int *latpt, float *wtlat, float *amskin,
             float *ain, float *amskout, float *aout, float omit);

void qlmaplength(int nlati, int nlato, float *bnin, float *bnout, 
               float *bsin, float *bsout, int *latdx, int *latpt, 
               float *wtlat);

void qlrgdlength(int ilat, int itim1, int itim2, int ntim1, int ntim2, 
               int nlati, int nlato, int *latdx,  int *latpt, 
               float *wtlat, float *amskin, float *ain,
               float *aout, float omit);

void qllinear_interpolation(int d, double *x, float *y, int dp, double *xp, 
                          float *yp);

int qllocate(int d, double *x, double point);

void qlpressbounds(int np, double *xp, double *bnds, double *wtp);

void qlrgdpressure(int n, int np, int nlat, int ntim2, double *x, double *xp,
                 float *a, float *ap);

void qlgenericlat(int nlat, double *gridpts, double *bn, double *bs,double *pw);
void qlunlatitude(int nlat, float blat, float elat, double *bn, double *bs,double *paa, double *pw);
void qlunlongitude(int nlon,float blon, float elon, double *be, double *bw, double *pw);

void qlequalarea(int nlat, float blat, float elat, double *bn, double *bs, double *paa, double *pw);

void qlgislatitude(int nlat, float blat, float elat, double *bn, double *bs, double *paa, double *pw);

int qlgauaw(double *pa, double *pw, int k);
int qlbsslzr(double *pbes, int knum);

void qlgaubounds(int nlat, float blat, float elat,
                         double *bn, double *bs, double *paa, double *pw);

void qllatregion(int nlat, float blat, float elat, double *latvals, double *bnds, double *pw, double *regionvals, double *regionbn, double *regionbs, double *regionpw);
void qllevelregion(int nlat, float blat, float elat, double *latvals, double *bnds, double *pw, double *regionvals, double *rbnds, double *regionpw);
 
void qlfloattodouble(int size, float *datain, double *dataout);
void qldoubletofloat(int size, double *datain, float *dataout);
void qlzerodouble(int size, double *data);
void qlgenwtsbnds(int np, double *xp, double *bnds, double *wtp);

void qlzerointdata(int size, int *data);
void qlzerodata(int size, float *data);


/*-----------------------------------------------------------------------*/

/*----------------------------- Macros ----------------------------------*/

#define MISSING 1.e20

#define abs(x) ((x) >= 0 ? (x) : -(x))
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))

/*-----------------------------------------------------------------------*/


extern void pyoutput(char *buf, int bell);
extern void outputatts(int cuid, int varid, char *name);

/*
 * =================================================================
 *                           Global Variables
 * =================================================================
 */


/*
 * =================================================================
 *                           Functions
 * =================================================================
 */


 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * *
 *                                                                         *
 *  Function:   qlgislatitude.c                                              *
 *                                                                         *
 *  Purpose:    To generate a gis uneven latitude grid                     *
 *                                                                         *
 *  Passed:    nlat      number of latitudes                               *
 *             blat      beginning latitude to find N-S direction          *
 *             elat      ending latitude to find N-S direction             *
 *                                                                         *
 *  Return:    bn        array with northern boundary values               *
 *             bs        array with southern boundary values               *
 *             pw        array with weights                                *
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


void qlgislatitude(int nlat, float blat, float elat, double *bn, double *bs, double *paa, double *pw)
{
    int i;                    /* loop index */
    double delta;             /* increment in latitude */   
    double start;             /* start of uniform increment in latitude */   
    char buf[1024];
    double sum, small;
    double pi;
    double rad;               /* convert to radians */


    pi = 4.0 * atan(1.0);
    rad = pi/((double)180.0);
    small = 1.e-06;

    delta = 4.0;
    start = 84.0;

    if( blat > elat) {                     /* north to south case */

        bn[0] = 90.0;
        bn[1] = 87.5;
        bn[nlat-1] = -87.5;

        bs[0] = 87.5;
        bs[nlat-2] = -87.5;
        bs[nlat-1] = -90.0;

        for(i = 2; i < nlat-1 ; i++) {
            bn[i] = start - (i-2)*delta;    
            bs[i-1] = bn[i];
        }

    }
    else {                                 /* south to north case */

        bs[0] = -90.0;
        bs[1] = -87.5;
        bs[nlat-1] = 87.5;

        bn[0] = -87.5;
        bn[nlat-2] = 87.5;
        bn[nlat-1] = 90.0;

        for(i = 2; i < nlat-1 ; i++) {
            bs[i] = -( start - (i-2)*delta );    
            bn[i-1] = bs[i];
        }

    }

    /* set up points */
    if(elat < blat)
    {
      paa[0] = 89.0;
      paa[1] = 86.0;
      for(i=2; i<nlat-1; i++)
        paa[i] = paa[i-1] - delta;
      paa[nlat-1] = -89.0;
    }
    else
    {
      paa[0] = -89.0;
      paa[1] = -86.0;
      for(i=2; i<nlat-1; i++)
        paa[i] = paa[i-1] + delta;
      paa[nlat-1] = 89.0;
    }


    /* construct the weights */


    if( blat > elat) {                     /* north to south case */

        for(i = 0; i < nlat-1 ; i++) {
            pw[i] = sin(rad * bn[i]) - sin(rad * bn[i+1]);
        }
        pw[nlat -1] = sin(rad * bn[nlat-1]) - sin(rad * bs[nlat-1]);
    }
    else {                                 /* south to north case */

        for(i = 0; i < nlat-1 ; i++) {
            pw[i] = sin(rad * bs[i+1]) - sin(rad * bs[i]);
        }
        pw[nlat -1] = sin(rad * bn[nlat-1]) - sin(rad * bs[nlat-1]);
    }

}


 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                            *
 *  Function:   qllatregion.c                                                   *
 *                                                                            *
 *  Purpose:    To generate a regional latitude grid                          *
 *                                                                            *
 *  Passed:    nlat         number of latitudes                               *
 *             latvals      global latitude  grid values                      *
 *             bnds         global boundary values                            *
 *             pw           global weights                                    *
 *             blat         beginning latitude to extract                     *
 *             elat         ending latitude to  to extract                    *
 *                                                                            *
 *  Return:    regionvals   array with region latitude values                 *
 *             regionbn     array with region north boundary values           *
 *             regionbs     array with region south boundary values           *
 *             regionpw     array with region weight values                   *
 *                                                                            *
 *        This routine also works for pressure levels                         *  
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * */


void qllatregion(int nlat, float blat, float elat, double *latvals, double *bnds, double *pw, double *regionvals ,double *regionbn, double *regionbs, double *regionpw)
{
    int i;                           /* loop index */
    int bindex, eindex;                                             /*  begin and end indices for extraction */
    int number;                                                     /*  number of points extracted */
    double sum;

    /* ----  find the begining index for extracting the data ---- */

    if( latvals[0] > latvals[nlat - 1] ) {  /* north to south case for global grid */
        if(latvals[0] <= blat)         /* global slice */
            bindex=0;
        else if(latvals[nlat-1] >= blat) 
            bindex=nlat-1;
        else 
        {
          i = 0;
          while (latvals[i] > blat)
            i++;
          if((latvals[i - 1] - blat) >  (blat - latvals[i]))
            bindex = i;
          else
            bindex = i - 1;
        }
    }


    else {                                                           /* south to north case for global grid */
        if(latvals[0] >= blat)         /* global slice */
            bindex=0;
        else if(latvals[nlat-1] <= blat)
            bindex=nlat-1;
        else
        {
          i = 0;
          while (latvals[i] < blat)
            i++;
          if((latvals[i] - blat) <  (blat - latvals[i - 1]))
            bindex = i;
          else
            bindex = i - 1;
        }
    }

    /* ----  find the ending index for extracting the data ---- */

    if( latvals[0] > latvals[nlat - 1] ) {                             /* north to south case for global grid */
        if(latvals[0] <= elat)         /* global slice */
            eindex=0;
        else if(latvals[nlat-1] >= elat)
            eindex=nlat-1;
        else
        {
          i = 0;
          while (latvals[i] > elat)
            i++;
          if((latvals[i - 1] - elat) >  (elat - latvals[i]))
            eindex = i;
          else
            eindex = i - 1;
        }
    }

    else {                                                            /* south to north case for global grid */
        if(latvals[0] >= elat)         /* global slice */
            eindex=0;
        else if(latvals[nlat-1] <= elat)
            eindex=nlat-1;
        else
        {
          i = 0;
          while (latvals[i] < elat)
            i++;
          if((latvals[i] - elat) <  (elat - latvals[i - 1]))
            eindex = i;
          else
            eindex = i - 1;
        }
    }

    /* ----  find the extracted values ---- */

    number = abs(eindex -bindex) + 1;

    if (eindex > bindex) {
        for(i=0; i<number; i++) {
           regionvals[i] = latvals[bindex + i];
           regionpw[i]   = pw[bindex + i];
           if( latvals[0] > latvals[nlat - 1] ) {                         /* north to south case for global grid */
               regionbn[i] = bnds[bindex + i];
               regionbs[i] = bnds[bindex + i + 1];
           }
           else {                                                       /* south to north case for global grid */
               regionbn[i] = bnds[bindex + i + 1];
               regionbs[i] = bnds[bindex + i];
           }
        }
    } 
    else {
        for(i=0; i<number; i++) {
           regionvals[i] = latvals[bindex - i];
           regionpw[i]   = pw[bindex - i];
           if( latvals[0] > latvals[nlat - 1] ) {                         /* north to south case for global grid */
               regionbn[i] = bnds[bindex - i];
               regionbs[i] = bnds[bindex - i + 1];
           }
           else {                                                       /* south to north case for global grid */
               regionbn[i] = bnds[bindex - i + 1];
               regionbs[i] = bnds[bindex - i];
           }
       }
    }

}

void qllevelregion(int nlev, float blev, float elev, double *plevvals, double *bnds, double *pw, double *regionvals ,double *rbnds, double *regionpw)
{
    int i;                           /* loop index */
    int bindex, eindex;                                           
    int number;                    
    double sum;

    /* ----  find the begining index for extracting the data ---- */

    if( plevvals[0] > plevvals[nlev - 1] ) {  /* desending order */
        i = 0;
        while (plevvals[i] > blev)
            i++;
        if(plevvals[0] == blev)
            bindex = i;
        else if((plevvals[i - 1] - blev) >  (blev - plevvals[i]))
            bindex = i;
          else
            bindex = i - 1;
    }

    else {                               /* ascending order */
        i = 0;
        while (plevvals[i] < blev)
            i++;
        if(plevvals[0] == blev)
            bindex = i;
        else if((plevvals[i] - blev) <  (blev - plevvals[i - 1]))
            bindex = i;
          else
            bindex = i - 1;
    }

    /* ----  find the ending index for extracting the data ---- */

    if( plevvals[0] > plevvals[nlev - 1] ) {         /* descending order */
        i = 0;
        while (plevvals[i] > elev)
            i++;
        if(plevvals[0] == elev)
            eindex = i;
        else if((plevvals[i - 1] - elev) >  (elev - plevvals[i]))
            eindex = i;
          else
            eindex = i - 1;
    }

    else {                               /* ascending order */
        i = 0;
        while (plevvals[i] < elev)
            i++;
        if(plevvals[0] == elev)
            eindex = i;
        else if((plevvals[i] - elev) <  (elev - plevvals[i - 1]))
            eindex = i;
          else
            eindex = i - 1;
    }

    /* ----  find the extracted values ---- */

    number = abs(eindex - bindex) + 2;

    if (eindex > bindex) {
        for(i=0; i<number; i++) {
           rbnds[i] = bnds[bindex + i];
        }
    }
    else
    {
        for(i=0; i<number; i++) {
           rbnds[i] = bnds[bindex + 1 - i];
        }
     }

    if (eindex > bindex) {
        for(i=0; i<number-1; i++) {
           regionvals[i] = plevvals[bindex + i];
           regionpw[i]   = pw[bindex + i];
        }
    }
    else
    {
        for(i=0; i<number-1; i++) {
           regionvals[i] = plevvals[bindex - i];
           regionpw[i]   = pw[bindex - i];
        }
     }
}

 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * *
 *                                                                       *
 *  Function:   qlgenericlat.c                                             *
 *                                                                       *
 *  Purpose:    To generate a generic weights & bounds                   *
 *                                                                       *
 *  Passed:    nlat      number of latitudes                             *
 *             gridpts    latitude grid points                           *
 *                                                                       *
 *  Return:    bn        array with northern boundary gridpts            *
 *             bs        array with southern boundary gridpts            *
 *             pw        array with weights                              *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ** * */


void qlgenericlat(int nlat, double *gridpts, double *bn, double *bs, double *pw)
{
    int i;                    /* loop index */
    double pi;
    double rad;               /* convert to radians */


    pi = 4.0 * atan(1.0);
    rad = pi/((double)180.0);

    /* generate the boundaries */

    if( gridpts[0] > gridpts[nlat-1]) {                     /* north to south case */

        bn[0] = gridpts[0] + (gridpts[0] - gridpts[1])/2.0;
        for(i = 1; i < nlat; i++) {
            bn[i] = (gridpts[i] + gridpts[i-1])/2.0; 
        }

        bs[nlat-1] = gridpts[nlat-1] - (gridpts[nlat-2] - gridpts[nlat-1])/2.0; 
        for(i = 0; i < nlat-1; i++) {
            bs[i] = (gridpts[i] + gridpts[i+1])/2.0; 
        }

        if( bn[0] > 90.0) 
            bn[0] = 90.0;

        if( bs[nlat-1] < -90.0) 
            bs[nlat-1] = -90.0;

     }

    else {
        bs[0] = gridpts[0] - (gridpts[1] - gridpts[0])/2.0; 
        for(i = 1; i < nlat; i++) {
            bs[i] = (gridpts[i] + gridpts[i-1])/2.0; 
        }

        bn[nlat-1] = gridpts[nlat-1] + (gridpts[nlat-1] - gridpts[nlat-2])/2.0; 
        for(i = 0; i < nlat-1; i++) {
            bn[i] = (gridpts[i] + gridpts[i+1])/2.0; 
        }

        if( bs[0] < -90.0) 
            bs[0] = -90.0;

        if( bn[nlat-1] > 90.0) 
            bn[nlat-1] = 90.0;
    }




    /* construct the weights */


    if( gridpts[0] > gridpts[nlat-1]) {                     /* north to south case */

        for(i = 0; i < nlat-1 ; i++) {
            pw[i] = sin(rad * bn[i]) - sin(rad * bn[i+1]);
        }
        pw[nlat -1] = sin(rad * bn[nlat-1]) - sin(rad * bs[nlat-1]);
    }
    else {

        for(i = 0; i < nlat-1 ; i++) {
            pw[i] = sin(rad * bs[i+1]) - sin(rad * bs[i]);
        }
        pw[nlat -1] = sin(rad * bn[nlat-1]) - sin(rad * bs[nlat-1]);
    }

}
 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * *
 *                                                                         *
 *  Function:   qlunlatitude.c                                               *
 *                                                                         *
 *  Purpose:    To generate a uniform latitude grid                        *
 *                                                                         *
 *  Passed:    nlat      number of latitudes                               *
 *             blat      beginning latitude to find N-S direction          *
 *             elat      ending latitude to find N-S direction             *
 *                                                                         *
 *  Return:    bn        array with northern boundary values               *
 *             bs        array with southern boundary values               *
 *             pw        array with weights                                *
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


void qlunlatitude(int nlat, float blat, float elat, double *bn, double *bs, double *paa, double *pw)
{
    int i;                    /* loop index */
    double delta;             /* increment in latitude */   
    double pi;
    double rad;               /* convert to radians */


    pi = 4.0 * atan(1.0);
    rad = pi/((double)180.0);

    delta = (blat - elat)/( (double)(nlat- 1));

    if( elat < blat) {                     /* north to south case */
        bn[0] = blat + delta/2.0;
        bs[0] = blat - delta/2.0;
    }
    else {
        bn[0] = blat - delta/2.0;
        bs[0] = blat + delta/2.0;
    }

    for(i = 1; i < nlat ; i++) {
        bn[i] = bn[i-1] - delta;    
        bs[i] = bs[i-1] - delta;
     }

    /* fix the endpoints to values +90 and 90 */

    if( elat < blat) {                     /* north to south case */
        bn[0] = 90.0;
        bs[nlat-1] = -90.0;
    }
    else {
        bs[0] = -90.0;
        bn[nlat-1] = 90.0;
    }


    /* set up points */   /*took out N to S & S to N case*/
    paa[0] = blat;
    for(i=1; i<nlat; i++)
      paa[i] = paa[i-1] - delta;
    
    
    /* construct the weights */


    if( elat < blat) {                     /* north to south case */

        for(i = 0; i < nlat-1 ; i++) {
            pw[i] = sin(rad * bn[i]) - sin(rad * bn[i+1]);
        }
        pw[nlat -1] = sin(rad * bn[nlat-1]) - sin(rad * bs[nlat-1]);
    }
    else {

        for(i = 0; i < nlat-1 ; i++) {
            pw[i] = sin(rad * bs[i+1]) - sin(rad * bs[i]);
        }
        pw[nlat -1] = sin(rad * bn[nlat-1]) - sin(rad * bs[nlat-1]);
    }

}
 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * *
*
 *
*
 *  Function:   qlunlongitude.c
*
 *
*
 *  Purpose:    To generate a uniform longitude grid
*
 *
*
 *  Passed:    nlon      number of longitudes
*
 *             blon      beginning longitude to find E-W direction
*
 *             elon      ending longitude to find E-W direction
*
 *
*
 *  Return:    be        array with eastern boundary values
*
 *             bw        array with western boundary values
*
 *
*
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*/
void qlunlongitude(int nlon,float blon, float elon, double *be, double *bw, double *pw)
{
    int i;                     /* loop index */

    double delta;             /* increment in longitude */
    double sign;              /* +1.0 or -1.0 */

    /* assume uniform longitude grid */

    if(elon > blon)                 /* west to east */
        sign = 1.0;
    else
        sign = -1.0;                /* east to west */

    delta = sign * ((elon -blon) / ( (double)(nlon - 1)) );  /* delta is po
sitive */

    if(nlon == 1)
    {
      be[0] = 0;
      bw[0] = 0;
      pw[0] = 0;
      return;
    }
    else
    {
      be[0] = blon + delta / 2.0;
      bw[0] = blon - delta / 2.0;
      pw[0] = delta;
    }

    for(i = 1; i < nlon ; i++) {
        be[i] = be[i-1] + sign * delta;
        bw[i] = bw[i-1] + sign * delta;
        pw[i] = delta;
    }
}

 /* Function:   qlzerodata.c
 *
 *  Purpose:   Initializes array with data_12[i][j][tme] = 0.0
 */
void qlzerodata(int size, float *data)
{
  int n;         /* loop index */
  float *d;      /* pointer to data used in the loop */

  d = data;

  for(n=0; n<size; n++)
    *d++ = 0.0;
}


 /* Function:   qlzerointdata.c
 *
 *  Purpose:   Initializes array with data_12[i][j][tme] = 0.0
 */
void qlzerointdata(int size, int *data)
{
  int n;         /* loop index */
  int *d;        /* pointer to data used in the loop */

  d = data;

  for(n=0; n<size; n++)
    *d++ = 0.0;
}

 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * *
 *                                                                         *
 *  Function:   qlcint.c                                                     *
 *                                                                         *
 *  Purpose:    To generate a the number od 360 degree shifts in qlmaparea   *
 *                                                                         *
 *  Passed:    value     angle/360.                                        *
 *                                                                         *
 *  Return:    offset    number of shifts                                  *
 *                                                                         *
 *  Note:      Relpaces fint in fortran code ( does not need the 0.5)      * 
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

int qlcint(float value)
{
   int offset;
   double small = 0.000001;

   if (value >= 0.0) {
       offset = (int)ceil((double)value + small);
       return offset;
   }

   if (value < 0.0) {
       offset = (int)( floor((double)value - small) + 1.0 );
       return offset;
   }
}

/*  ************************************************************************* 
*                                                                           *
*                           MAPAREA SYNOPSIS                                *
*                                                                           *
*  Maparea maps the input areas into the output area for each output point. * 
*  Given an output cell centered on the point (io, jo) with the boundaries  *            
*                bwout(io) ----- beout(io)                                  *
*                bnout(jo) ----- bsout(jo)                                  *
*  it finds the set of input points (ii, ji) with the boundaries            *            
*                bwin(ii) ----- bein(ii)                                    *
*                bnin(ji) ----- bsin(ji)                                    *
*                                                                           *
*  which have at least two of these boundary lines crossing inside the      *  
*  the output closed boundary. The code finds the contributions in latitude *      
*  and in longitude separately. The two sets are combined as used in routine*  
*  qlrgdarea.                                                                 *
*                                                                           *
*  Assembling this set for latitudes is straight forward. It entails:       * 
*                                                                           *
*       1. Find indices ji such that bnin[ji] and/or bsin[ji] fall within   *
*          bnout[jo] ----- bsout[jo].                                       *
*                                                                           *
*       2. Record these indices in latpt[] in order output latitude by      *
*          latitude. The position of the last contributor to each jo is     *
*          recorded in latdx[jo].                                           *
*       3. Find the extend of each input cell (bnorth,bsouth) which lies in *
*          the output cell and store it as the latitude weight in wtlat[]   *
*                                                                           *
*                                                                           *
*                                                                           *
*  The problem of finding the input longitudes points ii contributing to    * 
*  each output longitude io is complicated by the invariance of the         *
*  longitude description under the transformation + - 360 degrees. Before   *
*  following the steps outlined above it is necessry to make new input      *
*  boundary arrays bwinl[] and beinl[] with overlap in order to align the   * 
*  input longitude grid so that the first input cell contributes to the     * 
*  first output longitude. The solution is found using the following steps: *
*                                                                           *
*       1. Find the first contributor which has a western boundary at or    *
*          just westward of the output boundary westout. This index is      *
*          labeled istradle.                                                *
*       2. Make bninl and bsinl starting with istradle adding + - 360       * 
*          degrees as needed to fold the input domain into the output       * 
*          domain.                                                          *
*                                                                           *
*       3. Follow the same steps as in the latitude case recording indices  *
*          in lonpt after correction by adding istradle back in ( the       *
*          use of bwinl and beinl was only an intermediate step allowing    *
*          use of the same algorithm as in the latitudes case)              *
*                                                                           *
****************************************************************************/ 


/*  ************************************************************************* 
*                                                                           *
*                        MAPAREA USAGE                                      *
*                                                                           *
*  This subroutine provides the information needed to interpolate           *
*  from the input grid to the output grid (i.e., it calculates              *
*  the weight of the contribution from each of the input cells              *
*  to each of the output cells.                                             *
*                                                                           *
*  The input and output grid-cell boundaries must be specified by the       *
*  user(bnin, bsin, bein, bwin, bnout, bsout, beout, and bwout), and        *
*  the ordering must be monotonic, but need not be continuous (input or     *
*  output cells may be missing). The units are degrees latitude and         *
*  longitude.                                                               *
*                                                                           *
*   The input and output domains do not need to be identical, and           *
*   the longitude coordinate will be "wrapped around" if necessary.         *
*   The coordinate ordering may differ between input and output grids       *
*   (for example N to S on input, S to N on output.                         *
*                                                                           *
*                                                                           *
*   Input:                                                                  *
*                                                                           *
*    nloni = number of input grid cell longitudes.                          *
*    nlono = number of output grid cells longitudes.                        *
*    nlati = number of input grid cell latitudes.                           *
*    nlato = number of output grid cells latitudes.                         *
*    bnin(nlati) = northern boundary of each grid cell of input field.      *
*    bsin(nlati) = southern boundary of each grid cell of input field.      *
*    bein(nloni) = eastern boundary of each grid cell of input field.       *
*    bwin(nloni) = western boundary of each grid cell of input field.       *
*    bnout(nlato) =northern boundary of each grid cell of output field.     *
*    bsout(nlato) =southern boundary of each grid cell of output field.     *
*    beout(nlono) = eastern boundary of each grid cell of output field.     *
*    bwout(nlono) = western boundary of each grid cell of output field.     *
*                                                                           *
*                                                                           *
*   Output                                                                  *
*                                                                           *
*                                                                           *
*    londx(nlono) = index of the last element in lonpt and wtlon that       *
*                   apply to each of the the output longitudes              *
*    lonpt(nloni+nlono) = array of indices pointing to input grid cells     * 
*                         that contribute to each output grid cell.         *
*    wtlon(nloni+nlono) = array of weights indicating how much each         *
*                         input grid cell contributes to each               *
*                         output grid cell.                                 *
*                                                                           *
*    latdx(nlato) = index of the last element in latpt and wtlat that       *
*                   apply to each of the the output latitudes               *
*    latpt(nlati+nlato) = array of indices pointing to input grid cells     *
*                         that contribute to each output grid cell.         *
*    wtlat(nlati+nlato) = array of weights indicating how much each         *
*                         input grid cell contributes to each               *
*                         output grid cell.                                 *
*                                                                           *
****************************************************************************/ 


void qlmaparea(int nloni, int nlono, int nlati, int nlato,
	     float *bnin, float *bnout, float *bsin, float *bsout, 
	     float *bein, float *beout, float *bwin, float *bwout,
	     int *londx,  int *lonpt, float *wtlon, 
	     int *latdx,  int *latpt, float *wtlat)
{    
     

    float pi, bnorth, bsouth;
    float westout, eastout;       /* output longitude boundaries */
    int istradle;                 /* first contributing input long to first output long */
    int last;                     /* storage index  in latpt,wtlat,lonpt,wtlon */
    int j1, j2, j3;               /* indices used in finding input contributors*/
    int i;                        /* loop index */
    int ji, jo;                   /* indices used in input ouput loops */
    int ierr1, ierr2;             /* malloc check */ 
    int ii, ip;                   /* used to find input cell straddling the out boundary */
    int found;                    /* used as a flag in breaking out fo loops */
    int iflag;                    /* used as a flag */
    float *bwinl, *beinl;
      
    pi = 4.*atan(1.0);
      

/*  cycle through output zones */

    if ((bsin[nlati-1] - bsin[0]) * (bsout[nlato-1] - bsout[0]) > 0.) {
	j1 = 0;
	j2 = nlati - 1;
	j3 = 1;
    } 

    else {
	j1 = nlati - 1;
	j2 = 0;
	j3 = -1;
    }

    last = -1;

    for (jo = 0; jo < nlato; ++jo) {
	iflag = 0;

	/* find index of input latitude zones that will contribute. */

	for (ji = j1; j3 < 0 ? ji >= j2 : ji <= j2; ji += j3) {

	    if (bsin[ji] < bnout[jo] && bnin[ji] > bsout[jo]) {
		iflag = 1;
		++last;
		latpt[last] = ji;
		bnorth = min( bnout[jo], bnin[ji] );
		bsouth = max( bsout[jo], bsin[ji] );
		wtlat[last] = sin( bnorth*pi/180. ) - sin( bsouth*pi/180. );
	    }

	     else if (iflag == 1) {
		break;
	    }
	}

	latdx[jo] = last;
	if (last != -1) {
	    j1 = latpt[last];
	}
    }

     
	    
    westout = min( bwout[0], bwout[nlono - 1] );
    eastout = max( beout[0], beout[nlono - 1] );

    /* find input grid cell that straddles the output domain boundary. */

    if (bwin[0] < bwin[nloni - 1]) {
	ii = qlcint( (westout - bwin[nloni - 1])/360.) + 1;
    }
     else {
	ii = qlcint( (westout - bwin[nloni - 1])/360.) - 1;
    }

    found = 0;

    for (i = 0; i < nloni; ++i) {
	ip = ii;
	ii = qlcint( (westout - bwin[i])/360.);
	if (ii != ip) {
	    if (bwin[0] < bwin[nloni - 1]) {
		if (westout == bwin[i]) {
		    istradle = i;
		} else {
		    istradle = i - 1;
		    if (istradle < 0) {
			istradle = nloni - 1;
		    }
		}
	    } else {
		istradle = i;
	    }
	    found = 1;     /* set if ii != ip  implying found the stradle cell */
	    break;
	}
    }

    if (!found) {
	if (bwin[0] < bwin[nloni - 1]) {
	    istradle = nloni - 1;
	}
	else {
	    istradle = 0;
	}
    }



    /* allocate enough space for one more than the number of longitude 
       grid cells and make a copy of longitudes that coincides 
       with output domain. */

    ierr1 = 0;
    ierr2 = 0;

    if( !(bwinl  = (float *)malloc((nloni + 1)*sizeof(float))) )
	ierr1 = 1;
    if( !(beinl  = (float *)malloc((nloni + 1)*sizeof(float))) )
	ierr2 = 1;


    if (ierr1 != 0 || ierr2 != 0) {
	printf("Error in attempting to allocate memory dynamically.\n");
	printf("You may need more memory.\n");
	printf("Try running on cirrus or retrieving smaller portions of the array.\n");
	printf("Error in qlmaparea\n");
	exit(1);
    }


    for (ii = 0; ii < (nloni + 1); ++ii) {

	i = istradle + ii;        /* xform bw(e)in[i] -> bw(e)inl[ii] */
	if (i > (nloni - 1)) {
	    i -= nloni;
	}

	/*  make western bdry of input grid cell fall within output domain. */

	bwinl[ii] = bwin[i] + 360.* qlcint((westout - bwin[i])/360.);
	beinl[ii] = bein[i] + 360.* qlcint((westout - bwin[i])/360.);

    }


    /* shift the first point back to place it just westward of westout */
    if (bwin[nloni - 1] > bwin[0]) {
	if (bwinl[0] > westout) {
	    bwinl[0] += -360.;
	    beinl[0] += -360.;
	}
    } 
    else {
	if (bwinl[nloni] > westout) {
	    bwinl[nloni] += -360.;
	    beinl[nloni] += -360.;
	}
    }


    /*    cycle through output longitudes */

    if ((bwinl[nloni] - bwinl[0]) * (bwout[nlono - 1] - bwout[0]) > 0.) {
	j1 = 0;
	j2 = nloni;
	j3 = 1;
    } else {
	j1 = nloni;
	j2 = 0;
	j3 = -1;
    }

    last = -1;

    for (jo = 0; jo < nlono; ++jo) {

	iflag = 0;

	/* find index of input longitude zones that will contribute. */

	for (ji = j1; j3 < 0 ? ji >= j2 : ji <= j2; ji += j3) {
	    if (bwinl[ji] < beout[jo] && beinl[ji] > bwout[jo]) {
		iflag = 1;
		++last;
		lonpt[last] = ji + istradle ;
		if (lonpt[last] > nloni - 1) {
		    lonpt[last] -= nloni;
		}

		wtlon[last] = min( beout[jo], beinl[ji] ) - max( bwout[jo], bwinl[ji] );
	    } 

	    else if (iflag == 1) {
		break;
	    }
	}


	londx[jo] = last;
	if (last != -1) {
	    j1 = lonpt[last] - istradle;
	}
	if (j1 < 0) {
	    j1 += nloni;
	}
    }


    free(bwinl);
    free(beinl);

} 

/*  ********************************************************************* 
*                                                                       *
*                         RGDAREA SYNOPSIS                              *
*                                                                       *
*  Rgdarea makes an area weighted average of all the input cell data    *
*  which contributes to the output data for each particular output cell.*
*                                                                       *
*  For each output cell centered on the point (io, jo), it finds the    *
*  input cells contributing using:                                      *
*                                                                       *
*   jb--je: the begin and end latitudes with index je from the latpt    *
*           array and the corresponding weights from the wtlat array    *
*                                                                       *
*   ib--ie: the begin and end longitudes with index ie from the lonpt   *
*           array and the corresponding weights from the wtlon array    *              
*                                                                       *
*  The weights provide a measure of the overlap of the input cell areas *
*  into the output cell areas. These contributions are the same for all *
*  levels and all times. The structure in level-time is the same for    *
*  the input and output fields. A straight forward area weighted        *
*  calculation produces the regridded data.                             *
*                                                                       *
*  The routine requires a mask, amskin, of 1.0s and 0.0s which is the   *
*  same size as the input data including levels and times. It generates *
*  the corresponding mask for the outdata, amskout. This mask can be    *
*  used to regrid the output data                                       * 
*                                                                       *
*************************************************************************/ 



/*  ********************************************************************* 
*                                                                       *
*                           RGDAREA USAGE                               *
*                                                                       *
*  This subroutine takes an input field (ain) of grid cells and fills   *
*  an output field (aout) of grid cells with area-weighted values,      *
*  thereby achieving a transformation from one grid to another.         *
*                                                                       *
*  The program uses pointers and weights generated outside this         *
*   routine to determine how much each input grid cell contributes      *
*   to each output grid cell                                            *
*                                                                       *
*   Besides the automatic area weighting, an additional user-supplied   *
*   weighting factor (amskin) must be passed to this subroutine, which  *
*   multiplies the area weighting.  If simple area-weighting is needed, *
*   supply a vector of 1.''s in the input.  However, if for example only* 
*   land values should contribute to the new gridded field, then 1.''s  *
*   should be assigned for land points, and 0.''s for ocean points.     *
*                                                                       *
*                                                                       *
*                                                                       *
*   Input:                                                              *
*                                                                       *
*                                                                       *
*    ilong = dimension position for longitude in input and output arrays* 
*          = 0 if first dimension                                       *
*          = 1 if second dimension                                      *
*          = 2 if third dimension                                       *
*          = 3 if fourth dimension                                      *
*    ilat = dimension position for latitude in input and output arrays  *
*    itim1 = dimension position for time or level in input and output   *
*              arrays (if no itim1 dimension, then this parameter is    *
*              ignored)                                                 *
*    itim2 = dimension position for time or level in input and output   *
*              arrays (if no itim2 dimension, then this parameter is    *
*              ignored)                                                 *
*    nloni = number of input grid cell longitudes.                      *
*    nlono = number of output grid cells longitudes.                    *
*    nlati = number of input grid cell latitudes.                       *
*    nlato = number of output grid cells latitudes.                     *
*                                                                       *
*    ntim1 = length of itim1 dimension (if this is a dummy dimension,   *
*              you should set ntim1 to 0)                               *
*    ntim2 = length of itim2 dimension (if this is a dummy dimension,   *
*              you should set ntim2 to 0)                               *
*                                                                       *
*    londx(nlono) = index of the last element in lonpt and wtlon that   *
*                   apply to each of the the output longitudes          *
*    lonpt(nloni+nlono) = array of indices pointing to input grid cells *
*                         that contribute to each output grid cell.     *
*    wtlon(nloni+nlono) = array of weights indicating how much each     *
*                         to input grid cells that contribute to each   *
*                         output grid cell.                             *
*                                                                       *
*    latdx(nlato) = index of the last element in latpt and wtlat that   *
*                   apply to each of the the output latitudes           *
*    latpt(nlati+nlato) = array of indices pointing to input grid cells *
*                         that contribute to each output grid cell.     *
*    wtlat(nlati+nlato) = array of weights indicating how much each     *
*                         to input grid cells that contribute to each   *
*                         output grid cell.                             *
*    omit = value that will be assigned grid points that have either    *
*       been masked out or cannot be computed for lack of input data.   *
*                                                                       *
*                                                                       *
*    Input and Output:                                                  *
*                                                                       *    
*    amskin(*) = input weighting factors (for masking purposes).        *
*    ain(*) = input field                                               *
*                                                                       *
*                                                                       *
*    amskout(*) = output weighting factors                              *
*    aout(*) = output (regridded) field                                 *
*                                                                       *
*                                                                       *
*************************************************************************/ 

void qlrgdarea(int ilong, int ilat, int itim1, int itim2, int ntim1,
             int ntim2, int nloni, int nlono, int nlati, int nlato,
             int *londx,  int *lonpt, float *wtlon,
             int *latdx,  int *latpt, float *wtlat, float *amskin,
             float *ain, float *amskout, float *aout, float omit)

{     
     
    int id[4];                     /* order of the dimensions */
    int nd[4];                     /* size of the dimensions */

    int found;                     /* used with break out of for loops */

    int imiss1, imiss2;            /* store the mising dimension numbers */   
    int jtim1, jtim2;              /* storing itim1(2) or imiss1(2) */ 
    int mtim1, mtim2;              /* storing mtim1(2) or 1 if missing */
    
    int i, j, k;                   /* loop indices */

    int leni[4], leno[4];          /* vector used in striding through arrays */
    int iai[4];                    /* holds 4 indices for input point */
    int iao[4];                    /* holds 4 indices for output point */  

    int ierr1, ierr2;              /* used in malloc check */           
    
    int ji, ii;                   /* loop indices used for input zones and latitudes */    
    int jo, io;                   /* loop indices used for output zones and latitudes */    
    int jb, je;                   /* loop limits for contributing input zones */
    int ib, ie;                   /* loop limits for contributing input latitudes */ 
    
    int nn;                       /* index into data arrays */  
    int kk;                       /* index into level-times */
    int k1, k2;                   /* loop indices for level-times */    

    double *accum; 
    double *wtmsk;
    double wt;

    /* Store the order (0 thru3) and the corresponding lengths of the dimensions */

    id[0] = ilong;
    id[1] = ilat;
    id[2] = itim1;
    id[3] = itim2;

    nd[0] = nloni;
    nd[1] = nlati;
    nd[2] = ntim1;
    nd[3] = ntim2;

    for (i = 2; i < 4; ++i) {     /* ntim1 and/or ntim2 = 0 if no dimension */
	if (nd[i] == 0) {
	    id[i] = -1;
	}
    }

    imiss1 = 0;
    imiss2 = 0;

    for (k = 0; k < 4; ++k) {

	found = 0;

	for (i = 0; i < 4; ++i) {

	    if (id[i] == k) {
		if (k < 3) {                       /* look for duplicates */

		    for (j = i + 1; j < 4; ++j) {
			if (id[j] == k) {
	  
			    printf("Error in specifying data structure while attempting\n");
			    printf("to regrid (horizontally interpolate) data.\n");
			    printf("You have set:\n");
			    printf("ilong =  %d\n", ilong);
			    printf("ilat =  %d\n", ilat);
			    printf("itim1 = %d\n", itim1);
			    printf("itim2 = %d\n", itim2);
			    printf("One of these should = 0, another = 1, another = 2, and another = 3\n");
			    printf("If itim1 and/or itim2 are dummy dimensions, then\n");
			    printf("be sure that ntim1 and/or ntim2 are 0, so that\n");
			    printf("itim1 and/or itim2 will be ignored.\n");
			    printf("Duplicates error in qlrgdarea\n"); 
			    exit(1);
			}
		    }
		}
		found = 1;        /* found a id[] = k match for this k */
		break;            /* break out of for over i after a match */
	    }
	}

	if(!found) {
	    if (!imiss1) {
		imiss1 = k;
	    } 
	    else {
		imiss2 = k;
	    }
	}
    }

    /* transfer itim1(2) and ntim1() into jtim1(2) and mtim1(2) to 
       set mtim1(2) = 1 when dimension is missing and jtim1(2) to
       the position of the mising dimensions for check below */

    if (ntim1 > 0) {
	jtim1 = itim1;
	mtim1 = ntim1;
    } 
    else {
	mtim1 = 1;
	jtim1 = imiss1;
    }

    if (ntim2 > 0) {
	jtim2 = itim2;
	mtim2 = ntim2;
    } else if (imiss2 > 0) {
	jtim2 = imiss2;
	mtim2 = 1;
    } else {
	jtim2 = imiss1;
	mtim2 = 1;
    }

    if (ilong + ilat + jtim1 + jtim2 != 6) {

	printf("Error in specifying data structure while attempting\n");
	printf("to regrid (horizontally interpolate) data.  You have set:\n");
        printf("ilong =  %d\n", ilong);
        printf("ilat =  %d\n", ilat);
        printf("itim1 = %d\n", itim1);
        printf("itim2 = %d\n", itim2);
        printf("One of these should = 0, another = 1, another = 2, and another = 3\n");
        printf("If itim1 and/or itim2 are dummy dimensions, then\n");
        printf("be sure that ntim1 and/or ntim2 are 0, so that\n");
	printf("itim1 and/or itim2 will be ignored.\n");
	printf("Data structure error in qlrgdarea\n"); 
	exit(1);
    }

    /* store the dimension lengths noting that jtim1(2), level-time are the 
       same in the input and output fields */

    leni[ilong] = nloni;
    leni[ilat]  = nlati;
    leni[jtim1] = mtim1;
    leni[jtim2] = mtim2;

    leno[ilong] = nlono;
    leno[ilat]  = nlato;
    leno[jtim1] = mtim1;
    leno[jtim2] = mtim2;


    /* allocate enough space for double precision variables */ 

    ierr1 = 0;
    ierr2 = 0;

    if( !(accum  = (double *)malloc(mtim1*mtim2*sizeof(double))) )
	ierr1 = 1;
    if( !(wtmsk  = (double *)malloc(mtim1*mtim2*sizeof(double))) )
	ierr2 = 1;
    
    if (ierr1 || ierr2 ) {
	printf("Error in attempting to allocate memory dynamically.\n");
	printf("You may need more memory.\n");
	printf("Try running on cirrus or retrieving smaller portions of the array.\n");
	printf("Error in qlrgdarea\n");
	exit(1);
    }

/*    cycle through output zones and longitudes */

    je = -1;
    for (jo = 0; jo < nlato; ++jo) {
	iao[ilat] = jo;
	jb = je + 1;
	je = latdx[jo];

	ie = -1;
	for (io = 0; io < nlono; ++io) {
	    iao[ilong] = io;
	    ib = ie + 1;
	    ie = londx[io];

	    for (k = 0; k < mtim1*mtim2; ++k) {
		wtmsk[k] = (double)0.;
		accum[k] = (double)0.;
	    }

	    if (je >= jb && ie >= ib) {               /* has contributing grid points */
		wt = (double)0.;

	      
		for (ji = jb; ji <= je; ++ji) {
		    iai[ilat] = latpt[ji];
		  
		    for (ii = ib; ii <= ie; ++ii) {
			iai[ilong] = lonpt[ii];
			wt += wtlat[ji] * wtlon[ii];       /* weights for all level_times */

		        /* cycle through the level-time structure which is the the same
                           for  input and output and for the input zones and longitudes
                           contribution */

			for (k2 = 0; k2 < mtim2; ++k2) {
			    iai[jtim2] = k2;
			   
			    for (k1 = 0; k1 < mtim1; ++k1) {
				iai[jtim1] = k1;

				kk = k2 * mtim1 + k1;

				nn = iai[0] + leni[0] * (iai[1] + leni[1] * (iai[2]
                                                                          + leni[2] * iai[3] ));

				wtmsk[kk] += wtlat[ji] * wtlon[ii] * amskin[nn];

				accum[kk] += wtlat[ji] * wtlon[ii] * 
					amskin[nn] * ain[nn];
			    }
			}
		    }
		}


		/* find the weighted output for this single output io, jo point */ 
		
		for (k2 = 0; k2 < mtim2; ++k2) {
		    iao[jtim2] = k2;
		   
		    for (k1 = 0; k1 < mtim1; ++k1) {
			iao[jtim1] = k1;

			kk = k2 * mtim1 + k1;

			nn = iao[0] + leno[0] * (iao[1]  + leno[1] * (iao[ 2]
                                                                    + leno[2] * iao[3] ));

			if (wtmsk[kk] > 0.) {
			    aout[nn] = accum[kk] / wtmsk[kk];
			    amskout[nn] = wtmsk[kk] / wt;
			} 
			else {
			    aout[nn] = omit;
			    amskout[nn] = 0.;
			}
		    }
		}
	    } 

	    else {

		/*  no input grid cells will contribute */

		for (k2 = 0; k2 < mtim2; ++k2) {
		    iao[jtim2] = k2;
		   
		    for (k1 = 0; k1 < mtim1; ++k1) {
			iao[jtim1] = k1;
			nn = iao[0] + leno[0] * (iao[1] + leno[1] * (iao[ 2]
                                                                   + leno[2] * iao[3] ));
			aout[nn] = omit;
			amskout[nn] = 0.;
		    }
		}
	    }
	}
    }


    free(accum);
    free(wtmsk);
}


 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * *
 *                                                                         *
 *  Function:   qlequalarea.c                                                *
 *                                                                         *
 *  Purpose:    To construct the equal area grid weights and boundaries    *
 *                                                                         *
 *  Passed:    nlat      number of latitudes                               *
 *             blat      beginning latitude used to find N-S direction     *
 *             elat      ending latitude used to find N-S direction        *
 *                                                                         *
 *  Return:    bn        array with northern boundary values               *
 *             bs        array with southern boundary values               *
 *             pw        array of weights                                  * 
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void qlequalarea(int nlat, float blat, float elat, 
                         double *bn, double *bs, double *paa, double *pw)
{
  double pi;                    /* pi */
  double degrees;               /* convert to degrees */
  int i;                        /* loop index */

  double *sinb;                 /* sin of the boundary values */
  double *sinpaa;               /* sin of grid points */

  double sign;                  /* sign -- + or - 1.0 */
  double pwconst;               /* the constant weight */

  pi = 4.0 * atan(1.0);
  degrees = ((double)180.0)/pi;

  sinb = (double *)malloc((nlat + 1)*sizeof(double));
  if(!sinb) {
      printf("memory request failed for sinb");
      exit(1);
  }
  qlzerodouble(nlat+1, sinb);

  pwconst = 2.0/((double)nlat); 

  if(blat > elat) 
      sign = 1.0;
  else 
      sign = -1.0;

  sinb[0] = sign;
  sinb[nlat] = -sign;

  sinpaa = (double *)malloc(nlat*sizeof(double));
  if(!sinpaa) {
      printf("memory request failed for sinpaa");
      exit(1);
  }

  sinpaa[0] = sinb[0] - sign*(pwconst/2.);

  for(i=1; i<nlat; i++) {
      sinpaa[i] = sinpaa[i-1] - sign * pwconst;
  } 

  for(i=1; i<=nlat/2; i++) {
      sinb[i] = sinb[i-1] - sign * pwconst;
      sinb[nlat-i] = -sinb[i];
  }

  /* convert to angles in degrees */

  if(blat > elat) {                       /* north to south */
    for(i=0; i<nlat; i++) {
        bn[i] = degrees * asin( sinb[i]);
        bs[i] = degrees * asin( sinb[i + 1]);

    } 
  }
  else {
    for(i=0; i<nlat; i++) {
        bs[i] = degrees * asin( sinb[i]);
        bn[i] = degrees * asin( sinb[i + 1]);
    } 
  }

  for(i=0; i<nlat; i++) {
      pw[i] = pwconst;

      paa[i] = degrees * asin( sinpaa[i]);
  } 

}


 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * *
 *                                                                         *
 *  Function:   qlgaubounds.c                                                *
 *                                                                         *
 *  Purpose:    To call qlgauaw to get gaussian grid parameters              *
 *                                                                         *
 *  Passed:    nlat      number of latitudes                               *
 *             blat      beginning latitude used to find N-S direction     *
 *             elat      ending latitude used to find N-S direction        *
 *                                                                         *
 *  Return:    bn        array with northern boundary values               *
 *             bs        array with southern boundary values               *
 *             paa       array with grid point values in degrees           *
 *             pw        array of weights as differences in sine of        * 
 *                      adjacent angles                                    *
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void qlgaubounds(int nlat, float blat, float elat, 
                         double *bn, double *bs, double *paa, double *pw)
{
  double pi;                    /* pi */
  double degrees;               /* convert to degrees */
  int i;                        /* loop index */

  double *pa;                   /* sin of the grid points */
  double *sinb;                 /* sin of the boundary values */

  double sign;                  /* sign -- + or - 1.0 */

  pi = (double)4.0 * atan((double)1.0);
  degrees = ((double)180.0)/pi;

  pa = (double *)malloc(nlat*sizeof(double));
  if(!pa) {
      printf("memory request failed for pa");
      exit(1);
  }
  qlzerodouble(nlat, pa);

  sinb = (double *)malloc((nlat + 1)*sizeof(double));
  if(!sinb) {
      printf("memory request failed for sinb");
      exit(1);
  }
  qlzerodouble(nlat+1, sinb);

  qlgauaw(pa, pw, nlat); 

  if(blat > elat) {                       /* north to south */
      sign = 1.0;
  }
  else {
      sign = -1.0;

      for(i=0; i<nlat; i++) {
          pa[i] = -pa[i];
      } 
  }

  sinb[0] = sign;
  sinb[nlat] = -sign;

  for(i=1; i<=nlat/2; i++) {
      sinb[i] = sinb[i-1] - sign * pw[i-1];
      sinb[nlat-i] = -sinb[i];
  }

  /* convert to angles in degrees */

  if(blat > elat) {                       /* north to south */
    for(i=0; i<nlat; i++) {
        bn[i] = degrees * asin( sinb[i]);
        bs[i] = degrees * asin( sinb[i + 1]);
    } 
  }
  else {
    for(i=0; i<nlat; i++) {
        bs[i] = degrees * asin( sinb[i]);
        bn[i] = degrees * asin( sinb[i + 1]);
    } 
  }

  for(i=0; i<nlat; i++) {
      paa[i] =  degrees * asin(pa[i]);
  }


}

/*  ************************************************************************* 
*                                                                           *
*                           MAPLENGTH SYNOPSIS                              *
*                                                                           *
*  Maplength maps the input lengths into the output length for each output  *
*  point.                                                                   * 
*  Given an output cell centered on the point jo with the boundaries        *            
*                bnout(jo) ----- bsout(jo)                                  *
*  it finds the set of input points ji with the boundaries ,                *            
*                bnin(ji) ----- bsin(ji)                                    *
*                                                                           *
*  which have boundary lines inside the output boundary. The code finds the *
*  contributions in latitude.                                               *  
*                                                                           *
*  Assembling this set for latitudes is straight forward. It entails:       * 
*                                                                           *
*       1. Find indices ji such that bnin[ji] and/or bsin[ji] fall within   *
*          bnout[jo] ----- bsout[jo].                                       *
*                                                                           *
*       2. Record these indices in latpt[] in order output latitude by      *
*          latitude. The position of the last contributor to each jo is     *
*          recorded in latdx[jo].                                           *
*       3. Find the extend of each input cell (bnorth,bsouth) which lies in *
*          the output cell and store it as the latitude weight in wtlat[]   *
*                                                                           *
****************************************************************************/ 


/*  ************************************************************************* 
*                                                                           *
*                        MAPLENGTH USAGE                                    *
*                                                                           *
*  This subroutine provides the information needed to interpolate           *
*  from the input grid to the output grid (i.e., it calculates              *
*  the weight of the contribution from each of the input cells              *
*  to each of the output cells.                                             *
*                                                                           *
*  The input and output grid-cell boundaries must be specified by the       *
*  user(bnin, bsin, bnout), and the ordering must be monotonic, but need not*
*  be continuous (input or output cells may be missing). The units are      *
*  degrees latitude.                                                        *
*                                                                           *
*   The coordinate ordering may differ between input and output grids       *
*   (for example N to S on input, S to N on output.                         *
*                                                                           *
*                                                                           *
*   Input:                                                                  *
*                                                                           *
*    nlati = number of input grid cell latitudes.                           *
*    nlato = number of output grid cells latitudes.                         *
*    bnin(nlati) = northern boundary of each grid cell of input field.      *
*    bsin(nlati) = southern boundary of each grid cell of input field.      *
*    bnout(nlato) =northern boundary of each grid cell of output field.     *
*    bsout(nlato) =southern boundary of each grid cell of output field.     *
*                                                                           *
*   Output                                                                  *
*                                                                           *
*                                                                           *
*    latdx(nlato) = index of the last element in latpt and wtlat that       *
*                   apply to each of the the output latitudes               *
*    latpt(nlati+nlato) = array of indices pointing to input grid cells     *
*                         that contribute to each output grid cell.         *
*    wtlat(nlati+nlato) = array of weights indicating how much each         *
*                         input grid cell contributes to each               *
*                         output grid cell.                                 *
*                                                                           *
****************************************************************************/ 


void qlmaplength(int nlati, int nlato, float *bnin, float *bnout, float *bsin,
             float *bsout, int *latdx,  int *latpt, float *wtlat)
{    
     

    float pi, bnorth, bsouth;
    int last;                     /* storage index  in latpt,wtlat,lonpt,wtlon */
    int j1, j2, j3;               /* indices used in finding input contributors*/
    int i;                        /* loop index */
    int ji, jo;                   /* indices used in input ouput loops */
    int ierr1, ierr2;             /* malloc check */ 
    int ii, ip;                   /* used to find input cell straddling the out boundary */
    int found;                    /* used as a flag in breaking out fo loops */
    int iflag;                    /* used as a flag */
    float *bwinl, *beinl;
      
    int size;                    /* used file write */
    FILE *fp;             /* file used in ascii write */ 

    pi = 4.*atan(1.0);
      

/*  cycle through output zones */

    if ((bsin[nlati-1] - bsin[0]) * (bsout[nlato-1] - bsout[0]) > 0.) {
	j1 = 0;
	j2 = nlati - 1;
	j3 = 1;
    } 

    else {
	j1 = nlati - 1;
	j2 = 0;
	j3 = -1;
    }

    last = -1;

    for (jo = 0; jo < nlato; ++jo) {
	iflag = 0;

	/* find index of input latitude zones that will contribute. */

	for (ji = j1; j3 < 0 ? ji >= j2 : ji <= j2; ji += j3) {

	    if (bsin[ji] < bnout[jo] && bnin[ji] > bsout[jo]) {
		iflag = 1;
		++last;
		latpt[last] = ji;
		bnorth = min( bnout[jo], bnin[ji] );
		bsouth = max( bsout[jo], bsin[ji] );
		wtlat[last] = sin( bnorth*pi/180. ) - sin( bsouth*pi/180. );
	    }

	     else if (iflag == 1) {
		break;
	    }
	}

	latdx[jo] = last;
	if (last != -1) {
	    j1 = latpt[last];
	}
    }

} 

/*  ********************************************************************* 
*                                                                       *
*                         RGDLENGTH SYNOPSIS                            *
*                                                                       *
*  Rgdlength makes an length weighted average of all the input cell data*
*  which contributes to the output data for each particular output cell.*
*                                                                       *
*  For each output cell centered on the point jo, it finds the input    *
*  cells contributing using:                                            *
*                                                                       *
*   jb--je: the begin and end latitudes with index je from the latpt    *
*           array and the corresponding weights from the wtlat array    *
*                                                                       *
*  The weights provide a measure of the overlap of the input cell       *
*  lengths into the output cell lengths. These contributions are the    *
*  same for all levels and all times. The structure in level-time is the*
*  same for the input and output fields. A straight forward length      *
*  weighted calculation produces the regridded data.                    *
*                                                                       *
*  The routine requires a mask, amskin, of 1.0s and 0.0s which is the   *
*  same size as the input data including levels and times. It generates *
*  the corresponding mask for the outdata, amskout. This mask can be    *
*  used to regrid the output data                                       * 
*                                                                       *
*************************************************************************/ 



/*  ********************************************************************* 
*                                                                       *
*                          RGLENGTH USAGE                               *
*                                                                       *
*   This subroutine takes an input field (ain) of grid cells and fills  *
*   an output field (aout) of grid cells with length-weighted values,   *
*   thereby achieving a 1D transformation from one grid to another.     *
*                                                                       *
*   The program uses pointers and weights generated outside this        *
*   routine to determine how much each input grid cell contributes      *
*   to each output grid cell                                            *
*                                                                       *
*   Besides the automatic area weighting, an additional user-supplied   *
*   weighting factor (amskin) must be passed to this subroutine, which  *
*   multiplies the area weighting.  If simple length-weighting is needed*
*   , supply a vector of 1.''s in the input.  However, if for example   *
*   only* land values should contribute to the new gridded field, then  *
*   1.''s should be assigned for land points, and 0.''s for ocean points*
*                                                                       *
*                                                                       *
*   Input:                                                              *
*                                                                       *
*    ilat = dimension position for latitude in input and output arrays  *
*          = 0 if first dimension                                       *
*          = 1 if second dimension                                      *
*          = 2 if third dimension                                       *
*    itim1 = dimension position for time or level in input and output   *
*              arrays (if no itim1 dimension, then this parameter is    *
*              ignored)                                                 *
*    itim2 = dimension position for time or level in input and output   *
*              arrays (if no itim2 dimension, then this parameter is    *
*              ignored)                                                 *
*    nlati = number of input grid cell latitudes.                       *
*    nlato = number of output grid cells latitudes.                     *
*                                                                       *
*    ntim1 = length of itim1 dimension (if this is a dummy dimension,   *
*              you should set ntim1 to 0)                               *
*    ntim2 = length of itim2 dimension (if this is a dummy dimension,   *
*              you should set ntim2 to 0)                               *
*                                                                       *
*    latdx(nlato) = index of the last element in latpt and wtlat that   *
*                   apply to each of the the output latitudes           *
*    latpt(nlati+nlato) = array of indices pointing to input grid cells *
*                         that contribute to each output grid cell.     *
*    wtlat(nlati+nlato) = array of weights indicating how much each     *
*                         to input grid cells that contribute to each   *
*                         output grid cell.                             *
*    omit = value that will be assigned grid points that have either    *
*       been masked out or cannot be computed for lack of input data.   *
*                                                                       *
*                                                                       *
*    Input and Output:                                                  *
*                                                                       *    
*    amskin(*) = input weighting factors (for masking purposes).        *
*    ain(*) = input field                                               *
*                                                                       *
*                                                                       *
*    amskout(*) = output weighting factors                              *
*    aout(*) = output (regridded) field                                 *
*                                                                       *
*                                                                       *
*************************************************************************/ 

void qlrgdlength(int ilat, int itim1, int itim2, int ntim1, 
	     int ntim2, int nlati, int nlato,
	     int *latdx,  int *latpt, float *wtlat,
	     float *amskin, float *ain,
	     float *aout, float omit)

{     
     
    int id[3];                     /* order of the dimensions */
    int nd[3];                     /* size of the dimensions */

    int found;                     /* used with break out of for loops */

    int imiss1, imiss2;            /* store the mising dimension numbers */   
    int jtim1, jtim2;              /* storing itim1(2) or imiss1(2) */ 
    int mtim1, mtim2;              /* storing mtim1(2) or 1 if missing */
    
    int i, j, k;                   /* loop indices */

    int leni[3], leno[3];          /* vector used in striding through arrays */
    int iai[3];                    /* holds 4 indices for input point */
    int iao[3];                    /* holds 4 indices for output point */  

    int ierr1, ierr2;              /* used in malloc check */           
    
    int ji;                        /* loop indices used for latitudes */    
    int jo;                        /* loop indices used for latitudes */    
    int jb, je;                   /* loop limits for contributing input latitudes */
    
    int nn;                       /* index into data arrays */  
    int kk;                       /* index into level-times */
    int k1, k2;                   /* loop indices for level-times */    

    double *accum; 
    double *wtmsk;
    double wt;

    /* Store the order (0 thru3) and the corresponding lengths of the dimensions */

    id[0] = ilat;
    id[1] = itim1;
    id[2] = itim2;

    nd[0] = nlati;
    nd[1] = ntim1;
    nd[2] = ntim2;

    for (i = 1; i < 3; ++i) {     /* ntim1 and/or ntim2 = 0 if no dimension */
	if (nd[i] == 0) {
	    id[i] = -1;
	}
    }

    imiss1 = 0;
    imiss2 = 0;

    for (k = 0; k < 3; ++k) {

	found = 0;

	for (i = 0; i < 3; ++i) {

	    if (id[i] == k) {
		if (k < 2) {                       /* look for duplicates */

		    for (j = i + 1; j < 3; ++j) {
			if (id[j] == k) {
	  
			    printf("Error in specifying data structure while attempting\n");
			    printf("to regrid (horizontally interpolate) data.\n");
			    printf("You have set:\n");
			    printf("ilat =  %d\n", ilat);
			    printf("itim1 = %d\n", itim1);
			    printf("itim2 = %d\n", itim2);
			    printf("One of these should = 0, another = 1, and another = 2\n");
			    printf("If itim1 and/or itim2 are dummy dimensions, then\n");
			    printf("be sure that ntim1 and/or ntim2 are 0, so that\n");
			    printf("itim1 and/or itim2 will be ignored.\n");
			    printf("Duplicates error in qlrgdlength\n"); 
			    exit(1);
			}
		    }
		}
		found = 1;        /* found a id[] = k match for this k */
		break;            /* break out of for over i after a match */
	    }
	}

	if(!found) {
	    if (!imiss1) {
		imiss1 = k;
	    } 
	    else {
		imiss2 = k;
	    }
	}
    }

    /* transfer itim1(2) and ntim1() into jtim1(2) and mtim1(2) to 
       set mtim1(2) = 1 when dimension is missing and jtim1(2) to
       the position of the mising dimensions for check below */

    if (ntim1 > 0) {
	jtim1 = itim1;
	mtim1 = ntim1;
    } 
    else {
	mtim1 = 1;
	jtim1 = imiss1;
    }

    if (ntim2 > 0) {
	jtim2 = itim2;
	mtim2 = ntim2;
    } else if (imiss2 > 0) {
	jtim2 = imiss2;
	mtim2 = 1;
    } else {
	jtim2 = imiss1;
	mtim2 = 1;
    }

    if (ilat + jtim1 + jtim2 != 3) {

	printf("Error in specifying data structure while attempting\n");
	printf("to regrid (horizontally interpolate) data.  You have set:\n");
        printf("ilat =  %d\n", ilat);
        printf("itim1 = %d\n", itim1);
        printf("itim2 = %d\n", itim2);
        printf("One of these should = 0, another = 1, another = 2, and another = 3\n");
        printf("If itim1 and/or itim2 are dummy dimensions, then\n");
        printf("be sure that ntim1 and/or ntim2 are 0, so that\n");
	printf("itim1 and/or itim2 will be ignored.\n");
	printf("Data structure error in qlrgdlength\n"); 
	exit(1);
    }

    /* store the dimension lengths noting that jtim1(2), level-time are the 
       same in the input and output fields */

    leni[ilat]  = nlati;
    leni[jtim1] = mtim1;
    leni[jtim2] = mtim2;

    leno[ilat]  = nlato;
    leno[jtim1] = mtim1;
    leno[jtim2] = mtim2;


    /* allocate enough space for double precision variables */ 

    ierr1 = 0;
    ierr2 = 0;

    if( !(accum  = (double *)malloc(mtim1*mtim2*sizeof(double))) )
	ierr1 = 1;
    if( !(wtmsk  = (double *)malloc(mtim1*mtim2*sizeof(double))) )
	ierr2 = 1;
    
    if (ierr1 || ierr2 ) {
	printf("Error in attempting to allocate memory dynamically.\n");
	printf("You may need more memory.\n");
	printf("Try retrieving smaller portions of the array.\n");
	printf("Error in qlrgdlength\n");
	exit(1);
    }

/*    cycle through output zones */

    je = -1;
    for (jo = 0; jo < nlato; ++jo) {
	iao[ilat] = jo;
	jb = je + 1;
	je = latdx[jo];

        for (k = 0; k < mtim1*mtim2; ++k) {
	    wtmsk[k] = (double)0.;
	    accum[k] = (double)0.;
        }

        if (je >= jb ) {               /* has contributing grid points */
            wt = (double)0.;

	      
	   for (ji = jb; ji <= je; ++ji) {
	        iai[ilat] = latpt[ji];
		  
		wt += wtlat[ji];                 /* weights for all level_times */

	        /* cycle through the level-time structure which is the the same
                   for  input and output and for the input zones contribution */

		for (k2 = 0; k2 < mtim2; ++k2) {
		    iai[jtim2] = k2;
			   
		    for (k1 = 0; k1 < mtim1; ++k1) {
			iai[jtim1] = k1;

			kk = k2 * mtim1 + k1;

			nn = iai[0] + leni[0] * (iai[1] + leni[1] * iai[2]);

			wtmsk[kk] += wtlat[ji] * amskin[nn];

			accum[kk] += wtlat[ji] * amskin[nn] * ain[nn];
		    }
		}
           }


		/* find the weighted output for this single output io, jo point */ 
		
		for (k2 = 0; k2 < mtim2; ++k2) {
		    iao[jtim2] = k2;
		   
		    for (k1 = 0; k1 < mtim1; ++k1) {
			iao[jtim1] = k1;

			kk = k2 * mtim1 + k1;

			nn = iao[0] + leno[0] * (iao[1]  + leno[1] * iao[ 2]);

			if (wtmsk[kk] > 0.) 
			    aout[nn] = accum[kk] / wtmsk[kk];
			else 
			    aout[nn] = omit;
		    }
		}
	 } 

	 else {

		/*  no input grid cells will contribute */

	    for (k2 = 0; k2 < mtim2; ++k2) {
	        iao[jtim2] = k2;
		   
	        for (k1 = 0; k1 < mtim1; ++k1) {
	            iao[jtim1] = k1;
		    nn = iao[0] + leno[0] * (iao[1] + leno[1] * iao[ 2]);
		    aout[nn] = omit;
	        }
	    }
         }
    }


    free(accum);
    free(wtmsk);
}

 /*     ------------------------------------------------------------------
 *
 *     qlpressbounds 
 *
 *     purpose: 
 *           given the target pressure grid, calcalate the weights
 *      passed:
 *           np = size of new (primed) pressure grid
 *           xp = new pressure grid values
 *      returned:
 *           bnds = pressure grid boundaries
 *           wtp = pressure grid weights  
 *
 *     ------------------------------------------------------------------*/

void qlpressbounds(int np, double *xp, double *bnds, double *wtp)
{

  int i;                                 /* loop index */

 /* ------------------------------------------------------------------- */

  /* construct the boundaries from points */

  bnds[0] = xp[0] -((xp[1] - xp[0])/2.0);
  bnds[np] = xp[np - 1] +((xp[np - 1] - xp[np - 2])/2.0);

  for(i=1; i<np; i++) 
      bnds[i] = (xp[i] + xp[i - 1])/2.0;

  /* construct the weights from boundaries */

  if(bnds[np] > bnds[0]) {
      for(i=0; i<np; i++) 
          wtp[i] = bnds[i + 1] - bnds[i];
  }
  else {
      for(i=0; i<np; i++) 
          wtp[i] = bnds[i] - bnds[i + 1];
  }

}


 /*     ------------------------------------------------------------------
 *
 *     qlrgdpressure 
 *
 *     purpose: 
 *           call the linear interpolation routine to regrid pressure 
 *           column by column.
 *      passed:
 *           n = size of original pressure grid
 *           np = size of new (primed) pressure grid
 *           x = original pressure grid values
 *           xp = new pressure grid values
 *           a = original data values
 *
 *      returned:
 *           ap = regridded data values
 *
 *     ------------------------------------------------------------------*/

void qlrgdpressure(int n, int np, int nlat, int ntim2, double *x, double *xp, float *a, float *ap)
{
  int index;                    /* index into original data */
  int indexp;                   /* index into regridded data */

  double *lnx;                  /* ln of original grid values */
  double *lnxp;                 /* ln of interpolated grid values(primed) */

  float *y;                     /* original data values */
  float *yp;                    /* interpolated data values(primed) */

  int t,j,k,kp;                 /* for indices */

 /* ------------------------------------------------------------------- */

  lnx = (double *)malloc(n*sizeof(double));
  if(!lnx) {
      printf("memory request failed for lnx");
      exit(1);
  }
  qlzerodouble(n, lnx);

  lnxp = (double *)malloc(np*sizeof(double));
  if(!lnxp) {
      printf("memory request failed for lnxp");
      exit(1);
  }
  qlzerodouble(np, lnxp);

  y = (float *)malloc(n*sizeof(float));
  if(!y) {
      printf("memory request failed for y");
      exit(1);
  }
  qlzerodata(n, y);
  
  yp = (float *)malloc(np*sizeof(float));
  if(!yp) {
      printf("memory request failed for yp");
      exit(1);
  }
  qlzerodata(np, yp);

 /* ------------------------------------------------------------------- */

  /* make log of pressure levels */

  for(k=0; k<n; k++) 
      lnx[k] = log( x[k] );

  for(kp=0; kp<np; kp++)  
      lnxp[kp] = log( xp[kp] );
 
  /* regrid data */

  /*check if there are no times*/
  if(ntim2 == 0)
    ntim2=1;

  for(t=0; t<ntim2; t++) {
      for(j=0; j<nlat; j++) {

          for(k=0; k<n; k++) {               /* select pressure column to regrid */ 
               index = j + nlat*k + nlat*n*t;
               y[k] = *(a + index); 
          }

          qllinear_interpolation(n, lnx, y, np, lnxp, yp);

          for(kp=0; kp<np; kp++) {                    /* store regridded pressure column */ 
              indexp = j + nlat*kp + nlat*np*t;
              *(ap + indexp) = yp[kp];
          }
      
      }
  }

  free(lnx);
  free(lnxp);
  free(y);
  free(yp);

}

 /*     ------------------------------------------------------------------
 *
 *     qllinear_interpolation 
 *
 *     purpose: 
 *           One dimensional linear_interpolation from y(x) to
 *           yp(xp), where p stands for prime ( or interpolated).
 *           The original data and the interpolated data are 
 *           dimensioned at d and dp respectively.
 *     procedure:
 *           Given a point xp[ip], the bracketing points in the 
 *           original data are x[i] and x[i+1] are found using routine 
 *           locate. Then a linear intepolation formula is used to 
 *           calculate yp[xp[ip].
 *
 *      passed:
 *           d,dp, x, xp and y
 *      returned:
 *           yp 
 *
 *     ------------------------------------------------------------------*/
 void qllinear_interpolation(int d, double *x, float *y, int dp, double *xp, float *yp)
 {    
   
    int ip;                          /* index into new data */
    int i;                           /* i index for linear interpolation */
    float N;                         /* used in MISSING check */


 /*     ------------------------------------------------------------------ */

    N = 0.99 * MISSING;

    for (ip = 0; ip < dp; ++ip) {

	    i = qllocate(d, x, xp[ip]);    /* find index below ip */

	    if( i >= d-1)
		yp[ip] = y[d-1];

	    else if ( i < 0)
		yp[ip] = y[0];

	    else {
                if( (y[i+1] > N) | (y[i] > N) )
                   yp[ip] = MISSING;
                else   
	           yp[ip] = y[i] +(y[i+1] - y[i])*( (xp[ip] - x[i])/(x[i+1] - x[i]) );
            }
    } 
}
 /*     ------------------------------------------------------------------
 *
 *     qllocate 
 *
 *     purpose: 
 *            Given a value for x in the new coordinate system (point)
 *            find out where it fits in the old x coordinates. Return
 *            -1 if it is to the left of the old system or nlev if it 
 *            is to the right. ( values increasing to the right in this 
 *            explantion.
 *
 *      passed:
 *           d = nlev -- the size of the old coordinate system.
 *           x = old coordinate x vector
 *           point = new coordinate x point to fit somewhere
 *      returned:
 *           i = index into old coordinate system. 
 *
 *     ------------------------------------------------------------------*/

int qllocate(int d, double *x, double point)
{    
   
    int lo;                        /* low part of bracket*/
    int hi;                        /* high part of bracket*/
    int mid;                       /* middle of bracket */
    int ascnd;                     /* 1 if x ascends, 0 otherwise */

 /*     ------------------------------------------------------------------ */

    lo = -1;
    hi = d;
    ascnd = ( x[d-1] > x[0] );       /* direction of the data */

    while( (hi -lo) > 1) {
	mid = (hi + lo)/2;
	if( (point > x[mid]) == ascnd )
	    lo = mid;
	else
	    hi = mid;
	}

	return lo;
}

 /*     ------------------------------------------------------------------
 *
 *     qlgauaw 
 *
 *     Purpose. 
 *     -------- 
 *
 *     qlgauaw is called to compute the gaussian abscissas and weights. 
 *
 *     Interface. 
 *     ---------- 
 *
 *          call qlgauaw(pa,pw,k)
 *
 *               pa     - array, length at least k, to receive abscissas. 
 *
 *               pw     - array, length at least k, to receive weights.
 *
 *     Method. 
 *     ------- 
 *
 *          The zeros of the bessel functions are used as starting 
 *     approximations for the abscissas. Newton iteration is used to 
 *     improve the values to within a tolerance of eps.
 *
 *     External. 
 *     --------- 
 *
 *          qlbsslzr - routine to obtain zeros of bessel functions. 
 *
 *     Reference. 
 *     ---------- 
 *     ------------------------------------------------------------------*/

 int qlgauaw(double *pa, double *pw, int k) 
 {    
    double eps = 1e-14;            /* newton iteration tolerance */
    int iter;                      /* iteration counter */
    double avsp;                    /* abs of change to xy per iterayion */
    double c;                       /* constant used in first estimate */
    int l, n;                       /* used in symmetry calculation */
    double pkmrk;
    double fk;                      /* k as a double */
    double fn;                      /* n as a double */
    int kk;                         /* k / 2 */
    double pi;                      /* pi */
    double pk;
    int is;                         /* loop index */
    double sp;                      /* slope of poynomial */
    double xz;                      /* polynomial zero */                     
    double pkm1;
    double pkm2;
    double tp;                    /* temporary storage for expressions */



/*     ------------------------------------------------------------------ */

	 /* 1.     set constants and find zeros of bessel function. 
		   --- --------- --- ---- ----- -- ------ --------- */

    pi = 4.0 * atan(1.0);
    tp = 2.0 / pi;
    c  = (double)0.25 * ( 1.0 -  tp * tp );
    fk = (double)k;

    kk = k / 2;
    qlbsslzr(pa, kk);

    for (is = 0; is < kk; ++is) {  
    
	tp = fk + (double)0.5;
	xz = cos( pa[is] / sqrt( tp * tp  + c) );

			 /* giving the first approximation for xz. */
	iter = 0;

/*     ------------------------------------------------------------------ 

	   2.     compute abscissas and weights. 
	          ------- --------- --- -------                               */


/*           2.1     set values for next iteration.                       */
L210:
	pkm2 = 1.0;
	pkm1 = xz;
	++iter;
	if (iter > 10) {
	    goto L300;
	}

/*           2.2     computation of the legendre polynomial.              */

	for (n = 2; n <= k; ++n) {
	    fn = (double) n;
	    pk = ((2.0 * fn - 1.0) * xz * pkm1 - (fn - 1.0)* pkm2) / fn; 
		    
	    pkm2 = pkm1;
	    pkm1 = pk;
	}

	pkm1 = pkm2;
	pkmrk = ( fk * (pkm1 - xz * pk) ) / (1.0 - xz * xz);
	sp = pk / pkmrk;
	xz -= sp;
	avsp = fabs(sp);
	if (avsp > eps) {
	    goto L210;                  /* do another iteration */
	}

/*           2.3     abscissas and weights.                               */

	pa[is] = xz;
	pw[is] = (1.0 - xz * xz) * 2.0 / ( (fk*pkm1) *(fk*pkm1) );

/*           2.4     odd k computation of weight at the equator.          */

	if (k != 2*kk) {
	    pa[kk] = 0.0;  
	    pk = 2.0 / (fk * fk);

	    for (n = 2; n <= k; n += 2) {
		fn = (double) n;
		tp = fn - 1.0;
		pk = ( pk * (fn * fn) ) / ( tp * tp );
	    }

	    pw[kk] = pk; 
	} else {

/*           2.5     use symmetry to obtain remaining values.              */


	    for (n = 0; n < kk; ++n) {
		l = k - 1 - n;
		pa[l] = -pa[n];
		pw[l] = pw[n];
	    }

	}
    }

    return 0;

/*     ------------------------------------------------------------------ 

	    3.     error processing. 
		   ----- -----------                                      */

L300:
    printf("  qlgauaw failed to converge after 10 iterations.\n");
	exit(1);

/*     ------------------------------------------------------------------ */ 

}

/*     qlbsslzr  
 *
 *     Purpose. 
 *     -------- 
 *
 *     qlbsslzr returns knum zeros, or if knum > 50, knum 
 *     approximate zeros of the j0 bessel function
 *
 *     Interface. 
 *     ---------- 
 *
 *            call qlbsslzr(pbes,knum) 
 *
 *               pbes   - array, dimensioned knum, to receive the values. 
 *               knum   - number of zeros requested. 
 *
 *     Method. 
 *     ------- 
 *
 *          The first 50 values are obtained from a look up table. Any 
 *     additional values requested are interpolated. 
 *
 *     Externals. 
 *     ---------- 
 *
 *          None. 
 *
 *     Reference. 
 *     ---------- 
 *
 *    ------------------------------------------------------------------ */ 



int qlbsslzr(double *pbes, int knum)
{

    static double zbes[50] = { 2.4048255577,5.5200781103,8.6537279129,
	    11.7915344391,14.9309177086,18.0710639679,21.2116366299,
	    24.3524715308,27.493479132,30.6346064684,33.7758202136,
	    36.9170983537,40.0584257646,43.1997917132,46.3411883717,
	    49.4826098974,52.6240518411,55.765510755,58.9069839261,
	    62.0484691902,65.1899648002,68.3314693299,71.4729816036,
	    74.6145006437,77.7560256304,80.8975558711,84.0390907769,
	    87.1806298436,90.3221726372,93.4637187819,96.605267951,
	    99.7468198587,102.8883742542,106.0299309165,109.1714896498,
	    112.3130502805,115.4546126537,118.5961766309,121.737742088,
	    124.8793089132,128.0208770059,131.1624462752,134.3040166383,
	    137.4455880203,140.5871603528,143.7287335737,146.8703076258,
	    150.011882457,153.1534580192,156.2950342685 };


    int j;                         /* loop index */
    int inum;                      /* the min of knum and 49 */
    double api;                    /* pi */


/*          1.     extract values from look up table. 
		    ------- ------ ---- ---- -- ------                    

		   SET API                                               */

    api = 4.0 * atan( 1.0 );

    inum = min(knum,49);
	

    for (j = 0; j <= inum; ++j) {
	    pbes[j] = zbes[j]; 
		/*printf("  %3.8lf", pbes[j]); */
    }
/*     ------------------------------------------------------------------ 

	   2.     interpolate remaining values. 
		  ----------- --------- -------                          */

    for (j = 50; j < knum; ++j) { 
	    pbes[j] = pbes[j - 1] + api;
    }

/*     ------------------------------------------------------------------ */ 

    return 0;
}

/**************************************************************************
******
* Function:   qlfloattodouble
*
*  Purpose:   changes a float into a double
*
***************************************************************************
*****/
void qlfloattodouble(int size, float *datain, double *dataout)
{
  int n;               /* loop index */

  for(n=0; n<size; n++)
    dataout[n] = (double)datain[n];
}

/**************************************************************************
******
* Function:   qldoubletofloat
*
*  Purpose:   changes a double into a float
*
***************************************************************************
*****/
void qldoubletofloat(int size, double *datain, float *dataout)
{
  int n;               /* loop index */

  for(n=0; n<size; n++)
    dataout[n] = (float)datain[n];
}



 /* Function:   qlzerodouble.c
 *
 *  Purpose:   Initializes array with data_12[i][j][tme] = 0.0
 */
void qlzerodouble(int size, double *data)
{
  int n;         /* loop index */
  double *d;     /* pointer to data used in the loop */

  d = data;

  for(n=0; n<size; n++)
    *d++ = 0.0;
}

 /*     ------------------------------------------------------------------
 *
 *     qlgenwtsbnds
 *
 *     purpose:
 *           from the grid vector, calculate the weights and bounds
 *      passed:
 *           np = size of grid
 *           xp = grid values
 *      returned:
 *           bnds = grid boundaries
 *           wtp =  grid weights
 *
 *     ------------------------------------------------------------------*/

void qlgenwtsbnds(int np, double *xp, double *bnds, double *wtp)
{

  int i;                                 /* loop index */

 /* ------------------------------------------------------------------- */

  /* construct the boundaries from points */

  if (np > 1) {
     bnds[0] = xp[0] -((xp[1] - xp[0])/2.0);
     bnds[np] = xp[np - 1] +((xp[np - 1] - xp[np - 2])/2.0);
  } else {
     bnds[0] = xp[0] - 0.5;
     bnds[np] = xp[0] + 0.5;
  }

  for(i=1; i<np; i++)
      bnds[i] = (xp[i] + xp[i - 1])/2.0;

  /* construct the weights from boundaries */

  if(bnds[np] > bnds[0]) {
      for(i=0; i<np; i++)
          wtp[i] = bnds[i + 1] - bnds[i];
  }
  else {
      for(i=0; i<np; i++)
          wtp[i] = bnds[i] - bnds[i + 1];
  }

}
/* cdunif driver interface functions */

int cuopenread_ql(const char* controlpath, const char* datapath){
	CuFile* file;
	int status;

	if((file = CuCreateFile(CuQL))==(CuFile*)0){
		return -1;
	}
	file->internid1 = file->id+1;	     /* ql takes positive IDs */

	if(qlopenread(file->internid1, (char *)controlpath, (char *)datapath)==-1)
		return -1;

	return file->id;
}
int cuclose_ql(CuFile* file){

	return qlclose(file->internid1);
}
int cuinquire_ql(CuFile* file, int* ngdims, int* nvars, int* natts, int* recdim){

	return qlinquire(file->internid1, ngdims, nvars, natts, recdim);
}
int cudimid_ql(CuFile* file, int varid, const char* name){

	return qldimid(file->internid1, varid, (char *)name);
}
int cudiminq_ql(CuFile* file, int dimid, char* dimname, char* dimunits, CuType* dataType, CuDimType* dimtype, int* varid, long* length){

	return qldiminq(file->internid1, dimid, dimname, dimunits, dataType, dimtype, varid, length);
}
int cudimget_ql(CuFile* file, int dimid, void* values){

	return qldimget(file->internid1, dimid, values);
}
int cuvarid_ql(CuFile* file, const char* name){
	int saveopts;
	int varid;

					     /* Suppress spurious error messages */
	saveopts = cuErrOpts;
	cuseterropts_ql(0);
	varid = qlvarid(file->internid1, (char *)name);
	cuseterropts_ql(saveopts);
	return varid;
}
int cuvarinq_ql(CuFile* file, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts){
	return qlvarinq(file->internid1, varid, name, datatype, ndims, dimids, natts);
}
int cuvarget_ql(CuFile* file, int varid, const long start[], const long count[], void* value){

	return qlvarget(file->internid1, varid, (long *)start, (long *)count, value);
}
int cuattinq_ql(CuFile* file, int varid, const char* name, CuType* datatype, int* len){
	int err, saveopts;

					     /* Suppress spurious error messages */
	saveopts = cuErrOpts;
	cuseterropts_ql(0);
	err = qlattinq(file->internid1, varid, (char *)name, datatype, len);
	cuseterropts_ql(saveopts);
	return err;
}
int cuattget_ql(CuFile* file, int varid, const char* name, void* value){

	return qlattget(file->internid1, varid, (char *)name, value);
}
int cuattname_ql(CuFile* file, int varid, int attnum, char* name){

	return qlattname(file->internid1, varid, attnum, name);
}
void cuseterropts_ql(int erropts){
	qlseterropts((erropts & CU_VERBOSE) ? 2 : 3);
	if (erropts & CU_FATAL) qlseterropts(1);
	return;
}
#endif
