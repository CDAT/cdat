#if !defined(COS_HDR)
#define COS_HDR

#ifdef _CRAY
#include <fortran.h>
typedef _fcd fpchar;
#else
typedef char *fpchar;
#endif

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

#define CBCW    0   /* COS block control word */
#define CEOR    010 /* COS end of record      */
#define CEOF    016 /* COS end of file        */
#define CEOD    017 /* COS end of data        */

#define CRAYWORD 8     /* Size of Cray word in bytes    */
#define BLOCKSIZE 4096 /* Size of Cray block in bytes   */

#define MINSEXP32  0x3f6b   /* min valid (translatable subnormal) 
                               Cray masked exponent for IEEE 32 bit f.p. */
#define MINEXP32   0x3f83   /* min valid (translatable normal) 
                               Cray masked exponent for IEEE 32 bit f.p. */
#define MAXEXP32   0x4080   /* max valid (translatable) 
                               Cray masked exponent for IEEE 32 bit f.p. */
#define MINSEXP64  0x3bce   /* min valid (translatable subnormal) 
                               Cray masked exponent for IEEE 64 bit f.p. */
#define MINEXP64   0x3c03   /* min valid (translatable normal) 
                               Cray masked exponent for IEEE 64 bit f.p. */
#define MAXEXP64   0x4400   /* max valid (translatable) 
                               Cray masked exponent for IEEE 64 bit f.p. */

#define MINCEXP    0x2003   /* min valid Cray masked exponent */
#define MAXCEXP    0x5ffe   /* max valid Cray masked exponent */

#define MINI32EXP  1        /* min valid 32 bit IEEE masked exponent */
#define MAXI32EXP  254      /* max valid 32 bit IEEE masked exponent */
#define MINI64EXP  1        /* min valid 64 bit IEEE masked exponent */
#define MAXI64EXP  2046     /* max valid 64 bit IEEE masked exponent */

#define CBIAS      040000      /* Cray f.p. exponent bias */
#define I32BIAS    0177        /* IEEE 32 bit f.p. exponent bias */
#define I64BIAS    01777       /* IEEE 64 bit f.p. exponent bias */

#define CSIGNMASK  0x80        /* Mask to get 1st of 8 bits */
#define CSIGNMASK1 0x80000000  /* Mask to get 1st of 32 bits */
#define I32_NAN    0x7fffffff
#define I32_INFP   0x7f800000
#define I32_INFN   0xff800000
#define I32_ZEROP  0x00000000
#define I32_ZERON  0x80000000
#ifdef LONG64
#define CSIGNMASK2 0x8000000000000000l  /* Mask to get 1st of 64 bits */
#define I64_NAN    0x7fffffffffffffffl
#define I64_INFP   0x7ff0000000000000l
#define I64_INFN   0xfff0000000000000l
#else
#define CSIGNMASK2 0x8000000000000000ll /* Mask to get 1st of 64 bits */
#define I64_NAN    0x7fffffffffffffffll
#define I64_INFP   0x7ff0000000000000ll
#define I64_INFN   0xfff0000000000000ll
#endif

#if defined _CRAY
#define cosopen COSOPEN
#define cosclose COSCLOSE
#define cosrewind COSREWIND
#define cosbackspace COSBACKSPACE
#define cosread COSREAD
#define swapbytes SWAPBYTES
#define c8tor4 C8TOR4
#define c8toi4 C8TOI4
#define c8tol4 C8TOL4
#define c8tor8 C8TOR8
#define c8toi8 C8TOI8
#define c8tol8 C8TOL8
#define r8tor4 R8TOR4
#define ii8toi4 II8TOI4
#define r4tor8 R4TOR8
#define ii4toi8 II4TOI8
#define r4toc8 R4TOC8
#define i4toc8 I4TOC8
#define l4toc8 L4TOC8
#define r8toc8 R8TOC8
#define i8toc8 I8TOC8
#define l8toc8 L8TOC8
#define ibmi4toi4 IBMI4TOI4
#define ibmi2toi4 IBMI2TOI4
#define ibmr4tor4 IBMR4TOR4
#define ibmi4toi8 IBMI4TOI8
#define ibmi2toi8 IBMI2TOI8
#define ibmr4tor8 IBMR4TOR8
#define ibmr8tor8 IBMR8TOR8
#ifdef _CRAYPVP
#define ibmi4toc8 IBMI4TOC8
#define ibmi2toc8 IBMI2TOC8
#define ibmr4toc8 IBMR4TOC8
#define ibmr8toc8 IBMR8TOC8
#endif
#define expand21 EXPAND21
#ifndef _CRAYPVP
#define expand21_r4 EXPAND21_R4_
#define expand21_r8 EXPAND21_R8_
#endif
#elif defined __sun || defined __sgi || defined __osf__ || defined __uxpv__ || defined __linux || defined _SX
#define cosopen cosopen_
#define cosclose cosclose_
#define cosrewind cosrewind_
#define cosbackspace cosbackspace_
#define cosread cosread_
#define swapbytes swapbytes_
#define c8tor4 c8tor4_
#define c8toi4 c8toi4_
#define c8tol4 c8tol4_
#define c8tor8 c8tor8_
#define c8toi8 c8toi8_
#define c8tol8 c8tol8_
#define r8tor4 r8tor4_
#define ii8toi4 ii8toi4_
#define r4tor8 r4tor8_
#define ii4toi8 ii4toi8_
#define ibmi4toi4 ibmi4toi4_
#define ibmi2toi4 ibmi2toi4_
#define ibmr4tor4 ibmr4tor4_
#define ibmi4toi8 ibmi4toi8_
#define ibmi2toi8 ibmi2toi8_
#define ibmr4tor8 ibmr4tor8_
#define ibmr8tor8 ibmr8tor8_
#define expand21  expand21_
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
void swap_bytes(void *, int, int);
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

/* Routines callable from Fortran */

void cosopen (COSFILE **, fpchar, fpchar, INTEGER *, long, long);
void cosclose (COSFILE **, INTEGER *);
void cosrewind (COSFILE **, INTEGER *);
void cosbackspace (COSFILE **, INTEGER *);
void cosread (COSFILE **, void *, INTEGER *, INTEGER *, INTEGER *);
void swapbytes(void *, INTEGER *, INTEGER *);
void c8tor4(void *, void *, INTEGER *, INTEGER *);
void c8toi4(void *, void *, INTEGER *, INTEGER *);
void c8tol4(void *, void *, INTEGER *, INTEGER *);
void c8tor8(void *, void *, INTEGER *, INTEGER *);
void c8toi8(void *, void *, INTEGER *, INTEGER *);
void c8tol8(void *, void *, INTEGER *, INTEGER *);
void expand21(INTEGER *, void *, void *, INTEGER *);
#ifndef _CRAYPVP
void expand21_r4(INTEGER *, void *, void *, INTEGER *);
void expand21_r8(INTEGER *, void *, void *, INTEGER *);
#endif
void r8tor4(void *, void *, INTEGER *, INTEGER *);
void ii8toi4(void *, void *, INTEGER *, INTEGER *);
void r4tor8(void *, void *, INTEGER *, INTEGER *);
void ii4toi8(void *, void *, INTEGER *, INTEGER *);
#ifdef _CRAY
void r4toc8(void *, void *, INTEGER *, INTEGER *);
void i4toc8(void *, void *, INTEGER *, INTEGER *);
void l4toc8(void *, void *, INTEGER *, INTEGER *);
void r8toc8(void *, void *, INTEGER *, INTEGER *);
void i8toc8(void *, void *, INTEGER *, INTEGER *);
void l8toc8(void *, void *, INTEGER *, INTEGER *);
#endif
void ibmi4toi4 (void *, void *, INTEGER *, INTEGER *);
void ibmi2toi4 (void *, void *, INTEGER *, INTEGER *, INTEGER *);
void ibmr4tor4 (void *, void *, INTEGER *, INTEGER *);
void ibmi4toi8 (void *, void *, INTEGER *, INTEGER *, INTEGER *);
void ibmi2toi8 (void *, void *, INTEGER *, INTEGER *, INTEGER *);
void ibmr4tor8 (void *, void *, INTEGER *, INTEGER *, INTEGER *);
void ibmr8tor8 (void *, void *, INTEGER *, INTEGER *);
#ifdef _CRAYPVP
void ibmi4toc8 (void *, void *, INTEGER *, INTEGER *, INTEGER *);
void ibmi2toc8 (void *, void *, INTEGER *, INTEGER *, INTEGER *);
void ibmr4toc8 (void *, void *, INTEGER *, INTEGER *, INTEGER *);
void ibmr8toc8 (void *, void *, INTEGER *, INTEGER *);
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
#endif

#endif
