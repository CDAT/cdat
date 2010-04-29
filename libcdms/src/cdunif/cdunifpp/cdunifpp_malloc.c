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

/* Malloc functions 
 *
 * These routines are closely integrated with the link list functions; they
 * are called with a linked list "heaplist"; the malloc function adds the
 * newly allocated pointer to this list, and the free function removes it from
 * the list.  (They can also be called with NULL in which case they reduce to 
 * simple malloc and free; this is necessary when allocating or freeing memory 
 * for the heaplist itself.)
 *
 * The idea is that all the dynamically memory allocation associated with
 * a given file should be through these functions.  Then whenever the file
 * is closed properly or because an error condition gave an abort, the
 * memory can be freed without needing complicated tests to work out what
 * has been allocated: just go through the linked list freeing pointers.
 *
 * NOTE: this routine now allocates a little more memory than requested,
 * and saves the pointer to the list element on the heaplist at the start,
 * before returning to the calling routine the pointer to the actual block
 * of memory that the caller is interested in.  This ensures that when freeing
 * the memory, pp_list_del_by_listel can be used instead of pp_list_del, giving
 * efficiency gains.
 */

static const int extrasize = sizeof(PPlistel*);

void *pp_malloc(size_t size, PPlist *heaplist){

  void *ptr;
  PPlistel* *elp;

  if (size==0)
    return NULL;

  /* The only call to malloc in cdunifpp_* */
  ptr=malloc(size+extrasize);

  if (ptr==NULL)
  {
    CuError(CU_EINTERN,"WARNING: unable to allocate of %d bytes of memory in cdunifpp",size);
  }
  else {

    /* copy the pointer so we can use the start of the address to store
     * the PPlistel* 
     */
    elp = (PPlistel**) ptr;

    /* Now increment the pointer (to after our stored PPlistel*) to give
     * what the calling routine calling routine sees the start of memory
     * (cast to char* for ptr arithmetic.  Do this *before* storing it
     * on the heaplist, because pointers on will be freed with pp_free
     */
    ptr = (void*) ((char*)ptr+extrasize);

    if (heaplist != NULL) {
      
      CKI(   pp_list_add(heaplist,ptr,NULL)   );

      /* we just added to the list, so that heaplist->last will
       * contain pointer to the relevant PPlistel*
       */
      *elp = heaplist->last;
    }
    else
      *elp = NULL;
  }

  return ptr;

  ERRBLKP("pp_malloc");
}


void *pp_dup(const void *inptr, size_t size, PPlist *heaplist) {
  
  void *outptr;
  
  CKP(   outptr = pp_malloc(size, heaplist)   );
  memcpy(outptr, inptr, size);
  return outptr;

  ERRBLKP("pp_dup");
}


int pp_free(void *ptr, PPlist *heaplist){

  PPlistel *el;

  CKP(ptr);

  /* first subtract off the extra size we added (see pp_malloc) */
  ptr = (void*) ((char*)ptr-extrasize);

  /* this is our list element */
  el=*(PPlistel**)ptr;
  
  /* The only call to free in cdunifpp_* */
  free(ptr);

  /*   printf ("pp_free: %p\n",ptr);   */
  if (heaplist != NULL)
    CKI(  pp_list_del_by_listel(heaplist,el,NULL)  );

  return 0;

  ERRBLKI("pp_free");
}


int pp_free_all(PPlist *heaplist) {
  return pp_list_free(heaplist,1,NULL);
}

#endif
