/*****************************************************************************\
 *
 * basic.h -- basic definitions
 *
 *
 * Author:      Fausto Bernardini (fxb@cs.purdue.edu)
 *
\*****************************************************************************/

// $Id: basic.h,v 1.1 2003/09/02 17:27:14 scorzell Exp $

#ifndef __BASIC_H
#define __BASIC_H

// modified by Emilio: these are defined by default in SGI's new compiler
#if !defined(_BOOL) && !defined(SP2)
/* typedef int bool; */
/* const bool true = 1; */
/* const bool false = 0; */
#endif

/// Constant used to designate NULL when indexing into an array.
const int INDEX_NULL = -1;

/// Swap the contents of two variables.
template<class T>
inline void swap(T &a, T &b)
{
  T tmp = a;
  a = b;
  b = tmp;
}


/// Return the square of #x#.
template<class T>
inline T sqr(T x)
{
  return x*x;
}


/// Return the cube of #x#.
template<class T>
inline T cub(T x)
{
  return x*x*x;
}


/// Return the minimum between #x# and #y#.
template<class T>
inline T min(T x, T y)
{
  return x <= y ? x : y;
}


/// Return the maximum between #x# and #y#.
template<class T>
inline T max(T x, T y)
{
  return x >= y ? x : y;
}


/// Return the sign of #x#.
template<class T>
inline double sign( T x )
{
  return ((x >= 0.0) ? 1.0 : -1.0);
}


#endif
