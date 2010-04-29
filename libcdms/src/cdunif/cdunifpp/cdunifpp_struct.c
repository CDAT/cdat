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

/* allocating / freeing the file structure, an extension of CuCreateFile / CuDeleteFile */

CuFile *pp_create_file(const char *controlpath){

  CuFile *file;
  PPfile *ppfile;
  
  /* get file structure */
  file = CuCreateFile(CuPP);
  if (file == (CuFile*)0) goto err1;

  /* allocate internal structure - hang it off cu_file structure */
  ppfile=pp_malloc(sizeof(PPfile),NULL);
  if (ppfile==NULL) goto err2;
  
  file->internp = ppfile;
  ppfile->fh = NULL;
  strncpy(file->controlpath,controlpath,CU_MAX_PATH);
  ppfile->landmask = NULL;

  /* initialise heap list */
  ppfile->heaplist = pp_list_new(NULL);
  if (ppfile->heaplist == NULL) goto err3;

  return file;

 err3:
  pp_free(ppfile,NULL);
 err2:
  CuDeleteFile(file->id);
 err1:
  pp_error("pp_create_file");
  return NULL;
}

int pp_delete_file(CuFile *file){
  PPfile *ppfile = file->internp;
  int iatt,ivar;
  CuVar *var;

  /* Free the extra bits which cdunifpp allocated */
  pp_free_all(ppfile->heaplist);
  pp_free(ppfile,NULL);

  /* Now set all the attribute value ptrs to NULL.
   *
   * Reason: CuDeleteAtt (called by CuDeleteFile)
   * will assume that any non-NULL ptrs have to be freed.
   * But we already freed them.  Hence segfault. :-(
   */
  if (file->atts != NULL)
    for (iatt=0; iatt<file->ngatts; iatt++)
      file->atts[iatt].val=NULL;

  if (file->vars != NULL)
    for (ivar=0; ivar<file->nvars; ivar++) {
      var=&file->vars[ivar];
      if (var->atts != NULL)
	for (iatt=0; iatt<var->natts; iatt++)
	  var->atts[iatt].val=NULL;
    }
  
  /* CuDeleteFile commented out - CDMS seems to do that when calling cuclose 
   * and error if we do it twice.
   */
  
  /*  CuDeleteFile(file->id);  */

  return 0;
}

#endif
