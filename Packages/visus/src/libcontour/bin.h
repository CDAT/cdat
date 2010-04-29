/*****************************************************************************\
 *
 * bin.h -- 
 *
 *
 * Author:      Fausto Bernardini (fxb@cs.purdue.edu)
 *
 * Created - June 15, 1993
 * Ported to C++ by Raymund Merkert - June 1995
 * Changes by Fausto Bernardini - Sept 1995 
 *
\*****************************************************************************/

// $Id: bin.h,v 1.1 2003/09/02 17:27:15 scorzell Exp $

#ifndef __BIN_H
#define __BIN_H

#include <stdlib.h>
#include <assert.h>
#include <string.h>


//@ManMemo: A templated dynamic array
/*@Doc: A #Bin# is basically a dynamic array. Elements can be added to
  the #Bin# using the #add# method. Memory is allocated automatically
  upon need.
  */
template<class T> class Bin {
public:
  //@ManDoc: Contructor. Creates a Bin with block size equal to blocksize.
  Bin(int blocksize=0);

  //@ManDoc: Copy constructor. Copies the bin and its contents.
  Bin(const Bin<T> &bin) { *this = bin; }

  //@ManDoc: Assignment. Returns a copy of the bin and its contents.
  Bin<T> &operator=(const Bin<T> &bin);

  //@ManDoc: Destructor.
  ~Bin();

  //@ManDoc: Change the blocksize to be used from now on.
  void setBlocksize(int blocksize=0);
				
  //@ManDoc: Set the offset.
  void setOffset(int off);	
				
  //@ManDoc: Add T to the end of the Bin.
  int add(const T &e);		
				
  /*@ManDoc: Add an element to the Bin. 
    The default constructor is used to create the element.
  */
  int add() { return add(T()); }
				
  //@ManDoc: Remove last #num# items.
  int removeLast(int num=1);	
				
  int removePos(int pos=0);

  //@ManDoc: Returns the number of items in the Bin.
  int numItems() const;		
				
  //@ManDoc: Returns the T stored at position k in the bin.
  T& operator[](unsigned int k) const; 
				 
  /*@ManDoc: Resizes memory and returns a pointer to 
    the array. The Bin is reinitialized
  */
  T* done();			
				
  //@ManDoc: Returns a pointer without resizing memory.
  T* array() const;		
				
  //@ManDoc: Frees memory and reinitializes the Bin.
  void cleanUp();		
				
  //@ManDoc: Transfer the contents of this to newbin and clean this.
  void moveTo(Bin<T> &newbin);	 
				

private:
  int offset;			// Position of zero-th element
  int nitems;			// Number of items in the Bin  
  int block;			// Size of chunks for each realloc
  int room;			// Size of alloc'd memory
  T *a;				// Array of Ts 

  void init(int blocksize);	// init the bin
  void destroy();		// destroy the bin and its contents
  void grow();		        // create additional space
};


/*****************************************************************************\
 * inline functions
\*****************************************************************************/

template<class T> 
inline void Bin<T>::init(int blocksize) 
{
  if( blocksize == 0 ) {
    block = (size_t(1) > size_t(4096/sizeof(T)) ? 
	     size_t(1) : size_t(4096/sizeof(T)));
  } else {
    block = blocksize;
  }
  offset = 0;
  nitems = 0;
  room = 0;
  a = 0;
}


template<class T> 
inline void Bin<T>::destroy() 
{
  for( int i = 0; i < nitems; i++ ) { a[i].~T(); }
  free(a);
}


template<class T> 
inline Bin<T>::Bin(int blocksize) 
//
// Bin constructor 
// 
//	blocksize: the number of Ts to allocate memory for
//
{
  init(blocksize);
}


template<class T> 
inline Bin<T> &Bin<T>::operator=(const Bin<T> &bin)
{
  block = bin.block;
  offset = bin.offset;
  nitems = bin.nitems;
  room = bin.room;
  if( nitems > 0 ) {
    a = (T*)malloc( room*sizeof(T) );
    for( int i = 0; i < nitems; i++ ) {
      a[i] = bin.a[i];
    }
  } else {
    a = NULL;
  }
  return *this;
}


template<class T> 
inline Bin<T>::~Bin() 
//
// Bin destructor 
//
{
  destroy();
}


template<class T> 
inline void Bin<T>::cleanUp() 
{
  int bs = block;
  destroy();
  init(bs);
}


template<class T> 
inline int Bin<T>::numItems() const
//
// returns the number of items in the bin
//
{
  return nitems;
}


template <class T>
inline T& Bin<T>::operator[](unsigned int k) const
// 
// returns a data object from the bin
// 
//	k: the int in the bin of the T to return 
//
{
  return a[k+offset];
}


template<class T>
inline int Bin<T>::add(const T &e)
//
// adds a new data object to the bin, returns the index 
// where it was added to the bin
//
//	&e reference to the T that is to be added to the bin
//
{
  if(nitems == room) grow();
  a[nitems] = e;
  return (nitems++)-offset;
}


template<class T>
inline int Bin<T>::removeLast(int num)
//
// remove last num items from the Bin
// returns the index of the last element in the bin
//
//	num : number of items to remove
//
{
  assert( num <= nitems );
  for( int i = nitems-num; i < nitems; i++ ) a[i].~T();
  nitems -= num;
  return nitems-offset;
}


template<class T>
inline int Bin<T>::removePos(int pos)
{
#ifdef SP2			// modified by Emilio: looks like a real bug!!
   a[pos].~T();
#else
   delete a[pos];
#endif

   for ( int i=pos+1; i < nitems; i++ ) a[i-1] = a[i];
   nitems--;
   return nitems-offset;
}


template<class T> 
inline T* Bin<T>::array() const
//
// returns a pointer to the data objects in the bin
// 
{
  //@@ cast needed by g++ 2.7.2 (why?)
  return (T*)(&a[offset]);
}  


template<class T>
inline void Bin<T>::setBlocksize(int blocksize)
{
  if( blocksize == 0 ) {
    block = (size_t(1) > size_t(4096/sizeof(T)) ? 
	     size_t(1) : size_t(4096/sizeof(T)));
  } else {
    block = blocksize;
  }
}


template<class T>
inline void Bin<T>::setOffset(int off)
{
  offset = off;
}


template<class T> 
inline T* Bin<T>::done()
//
// reallocates memory to exactly fit the data objects in the bin
// and returns a pointer to the data objects. The Bin is reinitialized
//
{
  T *tmp;

  if( nitems == 0 ) {
    free( a );
    tmp = 0;
  } else {
    tmp = (T*)realloc( a, nitems*sizeof(T) );
    assert(tmp);
  }
  init(block);
  return tmp;
}


template<class T>
inline void Bin<T>::moveTo(Bin<T> &newbin)
// transfer the contents of this to newbin and cleanUp this
{
  memcpy( &newbin, this, sizeof( Bin<T> ) ); // do not use operator= !
  init(block);
}


template<class T>
inline void Bin<T>::grow()
{
  room += block;
  if(a == 0) {
    a = (T*)malloc( room*sizeof(T) );
  } else {
    a = (T*)realloc( a, room*sizeof(T) );
  }
  assert(a);
}


#endif
