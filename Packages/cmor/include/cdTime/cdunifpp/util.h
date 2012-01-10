#if !defined(UTIL_HDR)
#define UTIL_HDR

#include <stdio.h>

#if defined _CRAY
#if defined _CRAYMPP
#define SHORT32
#define INT64
#define FLOAT32
#else
#define SHORT64
#define INT64
#define FLOAT64
#endif
#else
#define SHORT16
#define INT32
#define FLOAT32
#endif

#define DOUBLE64
#if defined _CRAY || defined __alpha || defined __ia64 || defined __x86_64__ || _MIPS_SZLONG == 64 || defined __64BIT__ 
#define LONG64
#else
#define LONG32
#define LONGLONG64
#endif

#ifdef _IBM

#ifdef SHORT16
typedef unsigned short int uint16;
#endif
#ifdef INT32
typedef unsigned int uint32;
#endif
#ifdef SHORT32
typedef unsigned short int uint32;
#endif
#ifdef FLOAT32
typedef float float32;
#endif
#ifdef DOUBLE64
typedef double float64;
#endif
#ifdef LONG64
typedef unsigned long int uint64;
#else
typedef unsigned long long int uint64;
#endif

#else

#ifdef SHORT16
typedef short int int16;
typedef unsigned short int uint16;
#endif
#ifdef INT32
typedef int int32;
typedef unsigned int uint32;
#endif
#ifdef SHORT32
typedef short int int32;
typedef unsigned short int uint32;
#endif
#ifdef FLOAT32
typedef float float32;
#endif
#ifdef DOUBLE64
typedef double float64;
#endif
#ifdef LONG64
typedef long int int64;
typedef unsigned long int uint64;
#else
typedef long long int int64;
typedef unsigned long long int uint64;
#endif

#endif

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
#ifdef FLOAT32
#define REAL double
#else
#define REAL float
#endif
#else
#define REAL float
#endif

#if _INT_SIZE == 8
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

#if defined __alpha || defined __ia64 || defined __x86_64__ || defined __i386
#define LITTLE__ENDIAN
#else
#define BIG__ENDIAN
#endif

#ifdef _CRAY
#ifndef _CRAYIEEE
#define _CRAYNONIEEE
#endif
#endif

#ifdef _CRAY
#ifndef _CRAYMPP
#define _CRAYPVP
#endif
#endif

#if defined _CRAY
#define swapbytes SWAPBYTES
#define expand21 EXPAND21
#ifndef _CRAYPVP
#define expand21_r4 EXPAND21_R4_
#define expand21_r8 EXPAND21_R8_
#endif
#elif defined __sun || defined __sgi || defined __osf__ || defined __uxpv__ || defined __linux || defined _SX
#define swapbytes swapbytes_
#define expand21 expand21_
#define expand21_r4 expand21_r4_
#define expand21_r8 expand21_r8_
#endif

/* definition of a cosfile */

typedef struct {
	char            *fname;   /* file name                         */
	FILE            *fp;      /* current file                      */
	unsigned long   fwi;      /* forward index of current bcw/rcw  */
	unsigned long   pri;      /* backward index of current bcw/rcw */
} COSFILE;

/* Routines callable from C */

COSFILE *cos_open (char *, char *);
int cos_close (COSFILE *);
int cos_rewind (COSFILE *);
int cos_backspace (COSFILE *);
int cos_read (COSFILE *, void *, int, int *);
int c8_to_r4(void *, void *, int);
int c8_to_i4(void *, void *, int);
int c8_to_l4(void *, void *, int);
int c8_to_r8(void *, void *, int);
int c8_to_i8(void *, void *, int);
int c8_to_l8(void *, void *, int);
int r8_to_r4(void *, void *, int);
int i8_to_i4(void *, void *, int);
int r4_to_r8(void *, void *, int);
int i4_to_i8(void *, void *, int);
#ifdef _CRAY
int r4_to_c8(void *, void *, int);
int i4_to_c8(void *, void *, int);
int l4_to_c8(void *, void *, int);
int r8_to_c8(void *, void *, int);
int i8_to_c8(void *, void *, int);
int l8_to_c8(void *, void *, int);
#endif
int ibmi4_to_i4(void *, void *, int);
int ibmi2_to_i4(void *, void *, int, int);
int ibmr4_to_r4(void *, void *, int);
int ibmi4_to_i8(void *, void *, int, int);
int ibmi2_to_i8(void *, void *, int, int);
int ibmr4_to_r8(void *, void *, int, int);
int ibmr8_to_r8(void *, void *, int);
#ifdef _CRAYPVP
int ibmi4_to_c8(void *, void *, int, int);
int ibmi2_to_c8(void *, void *, int, int);
int ibmr4_to_c8(void *, void *, int, int);
int ibmr8_to_c8(void *, void *, int);
#endif
void swap_bytes(void *, INTEGER, INTEGER);

/* Routines callable from Fortran and C */

void expand21(INTEGER *, void *, void *, INTEGER *);
#ifndef _CRAYPVP
void expand21_r4(INTEGER *, void *, void *, INTEGER *);
void expand21_r8(INTEGER *, void *, void *, INTEGER *);
#endif
#ifdef _CRAY
int CRY2CRI(int *, int *, void *, int *, void *, int *, int *, int *);
int CRI2CRY(int *, int *, void *, int *, void *, int *, int *, int *);
int IEG2CRI(int *, int *, void *, int *, void *, int *, int *, int *);
int CRI2IEG(int *, int *, void *, int *, void *, int *, int *, int *);
int IBM2CRI(int *, int *, void *, int *, void *, int *, int *, int *);
#endif
#ifdef _CRAYPVP
int IBM2CRAY(int *, int *, void *, int *, void *);
int CRAY2IBM(int *, int *, void *, int *, void *);
#endif
#ifdef _CRAYNONIEEE
int CRAY2IEG(int *, int *, void *, int *, void *);
int IEG2CRAY(int *, int *, void *, int *, void *);
#endif

#endif
