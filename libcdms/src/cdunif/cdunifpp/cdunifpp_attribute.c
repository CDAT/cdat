/*
 *
 *    Copyright (C) 2004-2006 NERC DataGrid
 *    This software may be distributed under the terms of the
 *    CCLRC Licence for CCLRC Software
 * <CDATDIR>/External_License/CCLRC_CDAT_License.txt 
 *
 */
#ifdef HAVE_PP
#include "cdunifpp.h"

/*
 *  Attributes:
 *
 *   functions which create attributes during processing;
 *   these will later be copied to var->atts when the total number is known
 *
 *   the cu_att structure suits our needs just fine, so no need to define
 *   a "pp_att" structure in the way we've done for vars and dims
 */

CuAtt *pp_att_new(const char *name, CuType datatype, long len, const void *vals, PPlist *heaplist)
{
  int size;
  CuAtt *att;

  CKP(   att=pp_malloc(sizeof(CuAtt),heaplist)   );

  strncpy(att->name, name, CU_MAX_NAME);
  att->name[CU_MAX_NAME]='\0';

  att->datatype = datatype;
  att->len = len;

  size = cutypelen(datatype);

  if (len > 0) {
    CKP(   att->val = pp_malloc(len*size,heaplist)   ); 
    CKP(vals);
    memcpy(att->val, vals, len*size);

  } else {
    att->val=NULL;
  }

  return att;

  ERRBLKP("pp_att_new");
}

int pp_add_att(PPlist *attlist, const char *name, CuType datatype, long len, const void *vals, PPlist *heaplist)
{
  CuAtt *att;
  CKP(   att=pp_att_new(name,datatype,len,vals,heaplist)   );
  CKI(   pp_list_add(attlist,att,heaplist)   );  
  return 0;

  ERRBLKI("pp_add_att");
}

/*-------------------------------------------------
 * utility wrappers in case of string attributes
 */

CuAtt *pp_string_att_new(const char *name, const char *val, PPlist *heaplist)
{
  CKP(val);
  return pp_att_new(name, CuChar, strlen(val), val, heaplist);

  ERRBLKP("pp_string_att_new");
}

int pp_add_string_att(PPlist *attlist, const char *name, const char *val, PPlist *heaplist)
{
  CKP(val);
  return pp_add_att(attlist, name, CuChar, strlen(val), val, heaplist);

  ERRBLKI("pp_add_string_att");
}

int pp_add_string_att_if_set(PPlist *attlist, const char *name, const char *val, PPlist *heaplist)
{
  CKP(val);
  if (val[0] == '\0')
    return 0;
  return pp_add_string_att(attlist, name, val, heaplist);

  ERRBLKI("pp_add_string_att_if_set");
}

/*-------------------------------------------------*/

/* copy atts from temp storage into proper location allocated with CuCreateAtts
 *
 *
 * (NB the heaplist is contained indirectly in CuFile structure, but it's just as easy --
 * and in some ways more transparent -- for the calling routine to supply it)
 */
int pp_copy_and_free_atts(CuFile *file, CuVar *var, PPlist *attlist, PPlist *heaplist)
{
  int natts,i;
  CuAtt *atts, *savedatt;
  PPlisthandle handle;

  natts=pp_list_size(attlist);
  
  /* set natts - this ought to be done by CuCreateAtts ;-) */
  if (var==NULL)
    file->ngatts = natts;
  else
    var->natts=natts;

  /* copy atts from attlist to standard location
   *
   * (NB must not call CuCreateAtts with natts=0, else CuDeleteAtts later frees uninitialised pointer )
   */
  if (natts>0) {
    CKP(   atts=CuCreateAtts(file,var,natts)   );

    pp_list_startwalk(attlist, &handle);
    for (i=0; i<natts; i++) {
      savedatt = pp_list_walk(&handle,0);
      memcpy(&atts[i],savedatt,sizeof(CuAtt));
    }
  }

  /* The following line frees the list of attributes, and the cu_att structures it contains.
   * It does not free the attribute values themselves (malloced in pp_att_new) --
   * that is deliberate: we still want those.
   */
  CKI(  pp_list_free(attlist,1,heaplist)  );

  return 0;

  ERRBLKI("pp_copy_and_free_atts");
}
#endif
