/*****************************************************************************\
 *
 * squeue.h -- 
 *
 *
 * Author:      Fausto Bernardini (fxb@cs.purdue.edu)
 *
 * Created - June 15, 1993
 * Ported to C++ by Raymund Merkert - June 1995
 * Changes by Fausto Bernardini - Sept 1995 
 *
\*****************************************************************************/

// $Id: squeue.h,v 1.3 2005/09/11 04:47:11 rcook Exp $

#ifndef __SQUEUE_H
#define __SQUEUE_H

#include "queue.h"

//@ManMemo: A templated FIFO queue
/*@Doc: Implementation of a FIFO queue based on a circular array.
  Memory is allocated in blocks, given as a parameter when the
  object is created, or if no parameter is given, using the default
  block size.  Note: Currently, when a new block of memory is
  allocated, all of the current SQueue is recopied (because it is
  circular you cannot just increase the array size), so use caution
  when picking the block size. */
template <class T>
class SQueue : public Queue<T> {
public:
  //@ManDoc: Constructor with user define block size.
  SQueue(int blocksize=0);

  //@ManDoc: Destructor.
  virtual ~SQueue();

  int find(T&);

private:
};


/*****************************************************************************\
 * inline functions
\*****************************************************************************/

template <class T>
inline SQueue< T >::SQueue(int blocksize) : Queue<T>(blocksize)
{
}


template <class T>
inline SQueue< T >::~SQueue()
{
}


template <class T>
inline int SQueue< T >::find(T&e)
{
   int i, j;

   for ( j = this->head, i = 0; i < this->length; i++ ) {
      if (this->q[j] == e)
         return(j);
      j++;
      if( j == this->room ) { j = 0; }
   }
   return(-1);
}


#endif
