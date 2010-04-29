/*****************************************************************************\
 *
 * ipqueue.h -- 
 *
 *
 * Author:      Dan Schikore
 *
 * Adapted from pqueue.h
 * Ported to C++ by Raymund Merkert - June 1995
 * Changes by Fausto Bernardini - Sept 1995 
 *
\*****************************************************************************/

// $Id: ipqueue.h,v 1.6 2005/11/15 01:15:15 dlaney Exp $

#ifndef __IPQUEUE_H
#define __IPQUEUE_H

#ifdef WIN32
#pragma warning (disable:4244)
#pragma warning (disable:4018)
#pragma warning (disable:4800)
#endif

#include <algorithm>
#include <math.h>
#include "basic.h"
#include "bin.h"
#include "hash.h"


template <class T, class P, class K>
class IndexedPriorityQueue;
template <class T, class P, class K>
class IPqueuerec;
template <class T, class P, class K>
class IPhashrec;


// a wrapper for the items
template <class T, class P, class K>
class IPqueuerec {
public:
   IPqueuerec() {}
//   IPqueuerec(IPhashrec<T> &r, P p, K k) : rec(r), priority(p), key(k) { }
   T* getItem(void) { return(rec->getItem()); }
   const K& getKey(void) { return(key); }
private:
   friend class IndexedPriorityQueue<T,P,K>;

   IPhashrec<T,P,K> *rec;
   P priority;
   K key;
};


// a wrapper for the hash
template<class T, class P, class K>
class IPhashrec {
public:
  IPhashrec(T t) { item=t; }
  T* getItem(void) { return(&item); }
  void setLoc(int l) { qloc = l; }
  int getLoc(void) const { return(qloc); }
  int eq(const K &k) const { return(k == (*bin)[qloc].getKey()); }
private:

   friend class IndexedPriorityQueue<T,P,K>;

  // DATA
  T item;
  int qloc;
  Bin< IPqueuerec<T,P,K> > *bin;
};



/** A templated indexed priority queue.
    a priority queue which is searchable for a given item
  The number of #Pqueuerecs# that can be put into this queue is given as
  a parameter when the object is created ( or a default is used when no
  parameter is given).  This number does not change.
 */
template<class T, class P, class K>
class IndexedPriorityQueue {
public:
  /// Constructor.
  IndexedPriorityQueue( int blocksize=0 );

  /// Reinitialize.
  void cleanUp();

  /// Insert #item# into the queue with priority #priority#.
  void insert( const T& item, const P &priority, const K &key ); 

  /// Removes the element with highest priority from the queue.
  P extract(T& item);

  /// Return the element with highest priority from the queue.
  P max(T& item);

  /// Return #true# if the queus is empty, false otherwise.
  bool isEmpty();

  void remove(K);

  int numItems(void) { return(_q.numItems()); }

  T *find(const K &);

   int updatePriority(K k, P p);

protected:
//  int hashFun(const K&);
  static bool eqFun(const K &, const IPhashrec<T,P,K>&);

  // Used by extract to keep the tree partially ordered.
  void sink(int i);

private:
  // DATA
  HashTable < IPhashrec<T,P,K>, K > _h;     // hash table for finding items
  Bin < IPqueuerec<T,P,K> > _q;
};

#define frac(x) ((x) - floor(x))

static int
hashFun(const int &i)
{
   static double A = (sqrt(5.0)-1)/2.0;

   return(floor(30011 * frac(A*i)));
}

template<class T, class P, class K>
bool IndexedPriorityQueue<T,P,K>::eqFun(const K &k, const IPhashrec<T,P,K>&hr)
{
   return(hr.eq(k));
}

//template<class T, class P, int>
//inline
//int IndexedPriorityQueue<T,P,int>::hashFun(const int &i)
//{
//   static double A = (sqrt(5)-1)/2.0;
//
//   return(floor(30011 * frac(A*i)));
//}

//template<class T, class P, class K>
//inline
//bool IndexedPriorityQueue<T,P,K>::eqFun(const K &k, const IPhashrec<T>&hr)
//{
//    return(k == _q[hr.getLoc()].key);
//}

/*****************************************************************************\
 *
\*****************************************************************************/

template<class T, class P, class K>
inline IndexedPriorityQueue<T,P,K>::IndexedPriorityQueue( int blocksize )
                         : _h(30011, hashFun, eqFun)
{
  _q.setBlocksize(blocksize);
}


template<class T, class P, class K>
inline void IndexedPriorityQueue<T,P,K>::cleanUp()
{
  _q.cleanUp();
}


template<class T, class P, class K>
inline void IndexedPriorityQueue<T,P,K>::insert(const T& new_item, const P& priority,
                                                const K& key)
{
  IPhashrec<T,P,K> *inserted;
  int i, p;

//  if (_h.fetch(key) != NULL) {
//     printf("reinserting item!\n");
//     sleep(20);
//  }

  // add the item to the hash
  _h.add(key, IPhashrec<T,P,K>(new_item), inserted);

  inserted->bin = &_q;

  // insert the item into the priority queue
  i = _q.numItems();
  _q.add();
  p = (i-1)/2;
  while( i > 0 && _q[p].priority < priority ) {
    _q[i] = _q[p];
    _q[i].rec->setLoc(i);
    i = p;
    p = (i-1)/2;
  }
  _q[i].rec = inserted;
  _q[i].rec->setLoc(i);
  _q[i].priority = priority;
  _q[i].key = key;
}


template<class T, class P, class K>
inline void IndexedPriorityQueue<T,P,K>::sink(int i)
{
  int l, r, max;

  while( 1 ) {
    l = 2*i+1; r = 2*i+2;
    max = ( (l < _q.numItems()) && (_q[l].priority > _q[i].priority) ? l : i );
    max = ( (r < _q.numItems()) && (_q[r].priority > _q[max].priority) ? r : max );
    if( max == i ) {
      break;
    } else {
      std::swap(_q[max], _q[i]);
      _q[max].rec->setLoc(max);
      _q[i].rec->setLoc(i);
      i = max;
    }
  }
}


template<class T, class P, class K>
inline P IndexedPriorityQueue<T,P,K>::extract(T& item)
{
  P p = this->_q[0].priority;
#ifdef SP2			// modified by Emilio: it appears to be a bug!!
  K key = this->_q[0].key;
#else
  K key = this->q[0].key;
#endif

  // return the item
#ifdef SP2			// modified by Emilio: it appears to be a bug!!
  item = *(this->_q[0].rec->getItem());
#else
  item = this->_q[0].rec.getItem();
#endif


  // remove the item from the hash
  if (!this->_h.remove(key)) {
     printf("failed removing from hash\n");
#ifndef WIN32
     sleep(5);
#endif
  }

  // remove the item from the priority queue
  _q[0] = _q[_q.numItems()-1];
  _q[0].rec->setLoc(0);
  _q.removeLast();
  sink(0);



  return p;
}



template<class T, class P, class K>
inline void IndexedPriorityQueue<T,P,K>::remove(K k)
{
   IPhashrec<T,P,K> *hr;
   int i;

   hr = _h.fetch(k);
   i = hr->getLoc();

   // remove the item from the hash
  if (!_h.remove(k)) {
     printf("failed removing from hash\n");
#ifndef WIN32
     sleep(5);
#endif
  }

   // remove the item from the priority queue
   _q[i] = _q[_q.numItems()-1];
   _q[i].rec->setLoc(i);
   _q.removeLast();
   sink(i);

}


template<class T, class P, class K>
inline P IndexedPriorityQueue<T,P,K>::max(T& item)
{
  item = *(_q[0].rec->getItem());
  return _q[0].priority;
}


template<class T, class P, class K>
inline T *IndexedPriorityQueue<T,P,K>::find(const K &k)
{
   IPhashrec<T,P,K> *hr;

   hr = _h.fetch(k);

   if (hr == NULL)// {
//      int i;
//
//for (i=0; i<_q.numItems(); i++)
//   if (_q[i].key == k) {
//      printf("didn't fetch but I found it!\n");
//      sleep(5);
//   }
      return(NULL);
//}

   return(hr->getItem());
}


template <class T, class P, class K>
inline int IndexedPriorityQueue<T,P,K>::updatePriority(K k, P p)
{
   IPhashrec<T,P,K> *hr;

   hr = _h.fetch(k);

   if (hr == NULL)
      return(-1);

   // change the priority and sink the item
   _q[hr->getLoc()].priority = p;
   sink(hr->getLoc());

   return(1);
}

template<class T, class P, class K>
inline bool IndexedPriorityQueue<T,P,K>::isEmpty()
{
  return _q.numItems() == 0;
}


#endif
