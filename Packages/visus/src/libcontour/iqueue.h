/*****************************************************************************\
 *
 * iqueue.h -- 
 *
 *
 * Author:      Fausto Bernardini (fxb@cs.purdue.edu)
 *
 * Created - June 15, 1993
 * Ported to C++ by Raymund Merkert - June 1995
 * Changes by Fausto Bernardini - Sept 1995 
 *
\*****************************************************************************/

// $Id: iqueue.h,v 1.4 2005/09/11 22:41:21 rcook Exp $

#ifndef __IQUEUE_H
#define __IQUEUE_H

#include "utilities.h"
#include "queue.h"

template <class T, class K>
class IndexedQueue;

template<class T, class K>
class Ihashrec {
public:
  Ihashrec(T t, K k) { item=t; key=k; }
  void setLoc(int l) { qloc = l; }
  int getLoc(void) const { return(qloc); }
  int eq(const K &k) const { return(k == key); }
private:

   friend class IndexedQueue<T,K>;

  // DATA
  T item;
  int qloc;
  K key;
};

template<class T, class K>
class Iqueuerec {
public:
  Iqueuerec(Ihashrec<T,K> *r) { rec=r; }
private:

   friend class IndexedQueue<T,K>;

  // DATA
  Ihashrec<T,K> *rec;
};

//@ManMemo: A templated FIFO queue
/*@Doc: Implementation of a FIFO queue based on a circular array.
  Memory is allocated in blocks, given as a parameter when the
  object is created, or if no parameter is given, using the default
  block size.  Note: Currently, when a new block of memory is
  allocated, all of the current Queue is recopied (because it is
  circular you cannot just increase the array size), so use caution
  when picking the block size. */
template <class T, class K>
class IndexedQueue : public Queue< Iqueuerec<T,K> > {
public:
  //@ManDoc: Constructor with user define block size.
  IndexedQueue(int blocksize=0);

  //@ManDoc: Destructor.
  virtual ~IndexedQueue();

  //@ManDoc: Returns the element at the head, or NULL if the Queue is empty.
#ifdef SP2					// modified by Emilio: bug!!
  inline T* first() { return(this->length == 0 ? 0 : &this->q[head].rec->item); }
#else
//  inline T* first() { return(&Queue::first()->rec->t); }
  inline T* first() { return(this->length == 0 ? 0 : &this->q[this->head].rec->item); }
#endif

  //@ManDoc: Places an item at the end of the queue.
  void enqueue(T &e, K k);

  //@ManDoc: Remove \& return the item at the head of the array.
  int dequeue(T &e);

  T* find(K k);
protected:
  static bool eqFun(const K &, const Ihashrec<T,K>&);
  HashTable < Ihashrec<T,K>, K > _h;
};


/*****************************************************************************\
 * inline functions
\*****************************************************************************/

#define frac(x) ((x) - floor(x))

static int
hashFun_iq(const int &i)
{
   static double A = (sqrt(5.0)-1)/2.0;

   return(floor(30011 * frac(A*i)));
}

template<class T, class K>
bool IndexedQueue<T,K>::eqFun(const K &k, const Ihashrec<T,K>&hr)
{
   return(hr.eq(k));
}

template <class T, class K>
inline IndexedQueue< T, K >::IndexedQueue(int blocksize)
                            : Queue< Iqueuerec<T,K> >(blocksize),
                              _h(30011, hashFun_iq, eqFun)
{
}


template <class T, class K>
inline IndexedQueue< T, K >::~IndexedQueue()
{
}


template <class T, class K>
inline void IndexedQueue< T, K >::enqueue(T &e, K key)
{
   int pos;
   Ihashrec<T,K> *inserted;

#if 0
if (_h.fetch(key) != NULL)
   printf("adding same key!!\n");
#endif

   _h.add(key, Ihashrec<T,K>(e, key), inserted);

   // Emilio: the following line was giving a warning: parameter to enqueue
   //	      had to be an lvalue, substituted by code directly below it
#ifdef EMILIO
   pos = Queue< Iqueuerec<T,K> >::enqueue( Iqueuerec<T,K>(inserted) );
#endif
   Iqueuerec<T,K> inserted_rec(inserted);
   pos = Queue< Iqueuerec<T,K> >::enqueue( inserted_rec );

   inserted->setLoc(pos);
}


template <class T, class K>
inline int IndexedQueue< T, K >::dequeue(T &e)
{
   Iqueuerec<T,K> *item;

   item=Queue< Iqueuerec<T,K> >::dequeue();
   if (item == NULL)
      return(0);

   e = item->rec->item;

   _h.remove(item->rec->key);
   return(1);
//   return(&item->rec->item);
}

template <class T, class K>
inline T* IndexedQueue<T,K>::find(K key)
{
   Ihashrec<T,K> *hr;

   hr = _h.fetch(key);

   if (hr == NULL) {

#if 0
      int a, cur;
      cur=head;

      for (a=0, cur=head; a<length; a++, cur=(cur==room-1)?0:cur+1) {
         if (q[cur].rec->key == key) {
            printf("ah-ha! I found it\n");
            sleep(5);
         }
      }
#endif

      return(NULL);
   }

   return(&hr->item);
}

#endif
