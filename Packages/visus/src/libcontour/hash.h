/*****************************************************************************\
 *
 * hash.h -- a simple hash table
 *
 *
 * Author:      Fausto Bernardini (fxb@cs.purdue.edu)
 *
\*****************************************************************************/

// $Id: hash.h,v 1.1 2003/09/02 17:27:17 scorzell Exp $

#ifndef __HASH_H
#define __HASH_H

#include "basic.h"
#include "shelf.h"


/** A simple templated hash table.
  This is an implementation of a simple, general-purpose hash
  table. Collision resolution is by chaining. The hash function must
  be provided by the programmer, and is installed by the
  constructor. An equality operator must also be provided. A hash
  function that is likely to work well is the following [Cormen,
  Leiserson, Rivest]:
  
    $h(k) = floor( m * frac( k * A ) )$
  
  where $k$ is the key, $m$ is the size of the index, and $A$ is
  $(\sqrt(5)-1)/2$. 

  #T# is the templated type of a data item. It must contain a key of
  type #K#. For example:

    struct HashKey {
      int i, j;
    };

    struct HashItem {
      HashKey k;
      double p;
    }

*/
template<class T, class K>
class HashTable {
public:
  /** Constructor.
    #sz#: Size of the index.
    #hf#: Hash function. Takes a key, returns an int.
    #eq#: Equal. returns true if the key matches the item. */
  HashTable(int sz, int (*hf)(const K&), bool (*eq)(const K&, const T&));

  /// Destructor.
  ~HashTable();

  /// Reinitializes the table.
  void cleanUp();

  /** Add an item to the table. Result is #true# if the item
    has been added, #false# if it was already present. A pointer to
    the item in the table is returned in #ptr#. */
  bool add(const K& key, const T& item, T*& ptr);
  
  /// Delete item with key. return false if item is not in the table
  bool remove(const K& key);
  
  /** Returns a pointer to the item with #key#. NULL if not in
    the table. */
  T* fetch(const K& key);
  
  //@name Iterator
  //@{
  void first(int& i) const;
  bool more(const int& i) const;
  void next(int& i) const;
  //@}

  const T& operator[](int i) const;
  T& operator[](int i);

private:
  struct HashItem {
    T data;
    int next;
  };

  bool lookup(const K& key, int& index) const;
  
  // DATA
  int _sz;
  int (*_hf)(const K&);
  bool (*_eq)(const K&, const T&);
  int* _index;
  Shelf<HashItem> _buffer;
};


/*****************************************************************************\
 * inline functions
\*****************************************************************************/

template<class T, class K>
HashTable<T, K>::HashTable(int sz, int (*hf)(const K&), bool (*eq)(const K&, const T&))
{
  _sz = sz;
  _hf = hf;
  _eq = eq;
  _index = new int[_sz];
  for( int i = 0; i < _sz; ++i ) {
    _index[i] = INDEX_NULL;
  }
}


template<class T, class K>
HashTable<T, K>::~HashTable()
{
  delete[] _index;
}


template<class T, class K>
void HashTable<T, K>::cleanUp()
{
  _buffer.cleanUp();
  for( int i = 0; i < _sz; ++i ) {
    _index[i] = INDEX_NULL;
  }
}


template<class T, class K> 
bool HashTable<T, K>::lookup(const K& key, int& index) const
{
  int h = _hf(key);

  for( int i = _index[h]; i != INDEX_NULL; i = _buffer[i].next ) {
    if( _eq( key, _buffer[i].data ) ) {
      index = i;
      return true;
    }
  }
  index = h;
  return false;
}


template<class T, class K> 
T* HashTable<T, K>::fetch(const K& key)
{
  int i;
  if( lookup( key, i ) ) {
    return &(_buffer[i].data);
  } else {
    return NULL;
  }
}


template<class T, class K> 
bool HashTable<T, K>::add(const K& key, const T& item, T*& ptr)
{
  int i;
  if( lookup(key, i) ) {
    ptr = &_buffer[i].data;
    return false;
  } else {
    int tmp = _index[i];
    _index[i] = _buffer.put();
    _buffer[_index[i]].data = item;
    _buffer[_index[i]].next = tmp;
    ptr = &_buffer[_index[i]].data;
    return true;
  }
}


template<class T, class K> 
bool HashTable<T, K>::remove(const K& key)
{
  int i, p, h = _hf(key);

  for( i = _index[h], p = -1; i != INDEX_NULL; p = i, i = _buffer[i].next ) {
    if( _eq( key, _buffer[i].data ) ) {
      if( i == _index[h] ) {
	_index[h] = _buffer[i].next;
      } else {
	_buffer[p].next = _buffer[i].next;
      }
      _buffer.remove(i);
      return true;
    }
  }
  return false;
}


template<class T, class K> 
void HashTable<T, K>::first(int& i) const
{
  _buffer.first(i);
}


template<class T, class K> 
bool HashTable<T, K>::more(const int& i) const
{
  return !_buffer.end(i);
}


template<class T, class K> 
void HashTable<T, K>::next(int& i) const
{
  _buffer.next(i);
}


template<class T, class K> 
const T& HashTable<T, K>::operator[](int i) const
{
  return _buffer[i].data;
}


template<class T, class K> 
T& HashTable<T, K>::operator[](int i)
{
  return _buffer[i].data;
}


#endif
