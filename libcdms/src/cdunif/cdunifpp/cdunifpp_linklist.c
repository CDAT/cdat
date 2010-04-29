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

/*------------------------------------------------------------------------------*/

/* LINKED LIST FUNCTIONS */

void *pp_list_new(PPlist *heaplist){
  PPlist *list;
  CKP(   list=pp_malloc(sizeof(PPlist),heaplist)   );
  list->first=NULL;
  list->last=NULL;
  list->n=0;
  return list;

  ERRBLKP("pp_list_new");
}

/* This function frees a list; 
 * Set free_ptrs if the pointers which have been explicitly stored on the 
 * list (2nd argument to pp_list_add) are to be freed, not just the pointers
 * which are implicit in the linked list structure.  NB there is no further
 * recursion, in the sense that if the stored pointers are to datatypes which
 * contain further pointers then these may have to be freed explicitly.
 */
int pp_list_free(PPlist *list, int free_ptrs, PPlist *heaplist){
  PPlistel *p, *next;
  CKP(list);
  for (p=list->first ; p != NULL ; p=next) {
    next=p->next;
    if (free_ptrs)
      CKI(  pp_free(p->ptr,heaplist)  );
    CKI(  pp_free(p,heaplist)  );
  }
  CKI(  pp_free(list,heaplist)  );
  return 0;

  ERRBLKI("pp_list_free");
}


int pp_list_size(const PPlist *list){
  CKP(list);
  return list->n;

  ERRBLKI("pp_list_size");
}


int pp_list_add(PPlist *list, void *ptr, PPlist *heaplist){
  PPlistel *el;
  CKP(list);
  CKP(   el = pp_malloc(sizeof(PPlistel),heaplist)   );
  list->n++;
  el->ptr=ptr;
  el->next=NULL;
  if (list->first == NULL) {
    el->prev=NULL;
    list->first=list->last=el;
  }
  else {
    list->last->next=el;
    el->prev=list->last;
    list->last=el;
  }
  return 0;

  ERRBLKI("pp_list_add");
}

/* pp_list_add_or_find takes a pointer to an item and tries to find it on the list, using
 * the comparision function as in pp_list_find.
 *
 * If it already exists, it changes the item to point to the old value, and calls the 
 *    supplied function (if non-null) to free the item.
 * If it does not exist, it adds the item to the list.
 *
 * Return values: 
 *   0  time already existed in axis
 *   1  time has been added to axis
 *  -1  an error occurred (probably in memory allocation)
 *
 * NOTE: the return value of this function may be tested with the CKI() macro.
 * Do not add non-error cases with negative return values.
 *
 * NOTE 2: The item is formally declared as a void* but the
 *         thing pointed to should itself be a pointer (to heap memory),
 *         so you should pass in a foo** of some sort.  The only reason for
 *         not declaring as void** is that void* has the special property of
 *         being treated by the compiler as a generic pointer hence no
 *         warnings about incompatible pointer type
 */
int pp_list_add_or_find(PPlist *list, void *item_in,
			int (*compar)(const void *, const void *), int matchval, 
			free_func free_function,
			int *index_return, PPlist *heaplist)
{
  void *oldptr;
  void **item = (void**) item_in;

  if ((oldptr = pp_list_find(list, *item, compar, matchval, index_return)) != NULL) {
    if (free_function != NULL)
      CKI(  free_function(*item,heaplist)  );
    *item = oldptr;
    return 0;

  } else {
    CKI(  pp_list_add(list, *item, heaplist)  );
    if (index_return != NULL)
      *index_return = pp_list_size(list) - 1;
    return 1;
  }

  ERRBLKI("pp_list_add_or_find");
}


/* call pp_list_del to find a pointer ("ptr" element contained within the
 * listel structure) on the list, and then delete that element from the list,
 * or call pp_list_del_by_listel directly (more efficient) if you already
 * have the listel structure pointer for what you want to delete.
 */

int pp_list_del(PPlist *list, void *ptr, PPlist *heaplist){
  PPlistel *p;
  CKP(list);
  for (p=list->first; p != NULL; p=p->next)
    if (p->ptr == ptr)
      return pp_list_del_by_listel(list,p,heaplist);

  /* if what we're trying to remove is not found, fall through
   * to error exit
   */

  ERRBLKI("pp_list_del");
}


int pp_list_del_by_listel(PPlist *list,PPlistel *p, PPlist *heaplist){
  PPlistel *prev, *next;
  next=p->next;
  prev=p->prev;
  if (next!=NULL) next->prev=prev;
  if (prev!=NULL) prev->next=next;
  if (p==list->first) list->first=next;
  if (p==list->last) list->last=prev;
  CKI(  pp_free(p,heaplist)  );
  list->n--;
  return 0;

  ERRBLKI("pp_list_del_by_listel");
}

/* call pp_list_startwalk before a sequence of calls to pp_list_walk */
int pp_list_startwalk(const PPlist *list, PPlisthandle *handle){
  CKP(list);
  CKP(handle);
  handle->current = list->first;
  handle->list = list;
  return 0;

  ERRBLKI("pp_list_startwalk");
}


/* pp_list_walk:
 *   designed to be called repeatedly, and returns the next element of the list
 * each time (but must not call either add or del between calls)
 *
 * (Set return_listel to nonzero to return the list element structure rather than
 *  the pointer it contains.  This is just so that if you put null pointers on the
 *  list you can tell the difference from end of list.)
 */
void *pp_list_walk(PPlisthandle *handle, int return_listel){
  void *ptr;
  CKP(handle);
  if (handle->current == NULL)
    return NULL;
  else {
    ptr = (return_listel) ? (void *)handle->current : handle->current->ptr;
    handle->current=handle->current->next;
    return ptr;
  }

  ERRBLKP("pp_list_walk");
}

/*------------------------------------------------------------------------------*/
/* pp_list_find: find first item on the list matching specified item,
 * where "compar" is the matching function, and "matchval" is return value from
 * compar in the event of a match
 *
 * The pointer index_return, if non-NULL, is used to return the index number on the list
 * (set to -1 if not found).
 */
void *pp_list_find(PPlist *list, const void *item,
		   int (*compar)(const void *, const void *), int matchval, int *index_return) {
  int found, index;
  PPlistel *listel;
  PPlisthandle handle;
  void *ptr;

  pp_list_startwalk(list,&handle);
  found=0;
  index=0;
  while ((listel = pp_list_walk(&handle,1)) != NULL) {
    ptr=listel->ptr;
    if (compar(&item, &ptr) == matchval) {
      if (index_return != NULL)
	*index_return = index;
      return ptr;
    }
    index++;
  }
  if (index_return != NULL)
    *index_return = -1;
    return NULL;
}

#endif
