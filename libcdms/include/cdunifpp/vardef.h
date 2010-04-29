#include "util.h"

#define _IEEE4 0
#define _IEEE8 1
#define _CRAY8 2

#ifndef _FLT_TYPE
#ifdef _CRAY
#ifndef _CRAYIEEE
#define _FLT_TYPE _CRAY8
#else
#define _FLT_TYPE _IEEE8
#endif
#else
#define _FLT_TYPE _IEEE4
#endif
#endif

#ifndef _INT_TYPE
#ifdef _CRAY
#ifndef _CRAYIEEE
#define _INT_TYPE _CRAY8
#else
#define _INT_TYPE _IEEE8
#endif
#else
#define _INT_TYPE _IEEE4
#endif
#endif

#if _FLT_TYPE == _CRAY8 || _FLT_TYPE == _IEEE8
#define _FLT_SIZE 8
#else
#define _FLT_SIZE 4
#endif

#if _INT_TYPE == _CRAY8 || _INT_TYPE == _IEEE8
#define _INT_SIZE 8
#else
#define _INT_SIZE 4
#endif

#if _FLT_SIZE == 8
#define real8
#ifdef FLOAT32
#define REAL double
#else
#define REAL float
#endif
#else
#define REAL float
#endif

#if _INT_SIZE == 8
#define int8
#ifdef INT32
#ifdef LONG64
#define INTEGER long
#else
#define INTEGER long long
#endif
#else
#define INTEGER int
#endif
#else
#define INTEGER int
#endif
