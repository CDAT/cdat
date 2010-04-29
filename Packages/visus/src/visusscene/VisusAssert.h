/***********************************************************************
*
* Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
* Produced at the Lawrence Livermore National Laboratory  
* Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
* LLNL-CODE-406031.  
* All rights reserved.  
*   
* This file is part of "Simple and Flexible Scene Graph Version 2.0."
* Please also read BSD_ADDITIONAL.txt.
*   
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are
* met:
*   
* @ Redistributions of source code must retain the above copyright
*   notice, this list of conditions and the disclaimer below.
* @ Redistributions in binary form must reproduce the above copyright
*   notice, this list of conditions and the disclaimer (as noted below) in
*   the documentation and/or other materials provided with the
*   distribution.
* @ Neither the name of the LLNS/LLNL nor the names of its contributors
*   may be used to endorse or promote products derived from this software
*   without specific prior written permission.
*   
*  
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
* A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
* LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
* PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
* LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
* NEGLIGENCE OR OTHERWISE) ARISING
*
***********************************************************************/


#ifndef VISUSASSERT_H
#define VISUSASSERT_H

#include <cassert>

#undef vassert
#undef vassert_core
#undef vwarning

//#undef NDEBUG
//#define VISUS_VERBOSE
//#define VISUS_VERBOSE_LEVEL 100

#ifndef NDEBUG 
#include <cstdio>
#include <cstring>
#endif 

#define vassert_core(expression, file, lineno)  \
  (fprintf (stderr,"%s:%u: failed assertion\n", file, lineno),	\
   exit(0), 0)


#ifndef NDEBUG 
//#if 1

#define vassert(expression)                                       \
  ((void) ((expression) ? 0 : fprintf (stderr,"%s:%u: failed assertion\n", __FILE__, __LINE__)))

#define vwarning(msg,...) {char error[200] = "WARNING: %s::%u:\n\t";strcat(error,msg);strcat(error,"\n");fprintf(stderr,error,__FILE__,__LINE__ , ## __VA_ARGS__);}

#define verror(expression,msg,return_value)                             \
  if ((expression)) {                                                   \
    fprintf(stderr,"WARNING: %s::%u: %s\n",__FILE__,__LINE__,msg);      \
    return return_value;                                                \
  }

#define type_index_check(index,return_value)                            \
  if ((index < 0) || (index >= VISUS_NUM_TYPEIDS)) {                    \
    fprintf(stderr,"WARNING: %s::%u: Type ID %d does not exists no such shared value type\n",__FILE__,__LINE__,index); \
    return return_value;                                                \
  }

#define vwarn(expression,msg)                                            \
  if ((expression)) {                                                   \
    fprintf(stderr,"WARNING: %s::%u: %s\n",__FILE__,__LINE__,msg);      \
  }


#define vmessage(msg,...)                \
  fprintf(stderr,msg, ## __VA_ARGS__);   \
  fflush(stderr);

void check_gl_error(const char* file, const int linenum);

#define vglerror() check_gl_error(__FILE__,__LINE__);
  
#else

#define vassert(expression)                                        \
((void) 0)

#define vwarning(msg,...) {;}

#define type_index_check(index,return_value)  ;

#define verror(expression,msg,return_value)  if (expression) return return_value;

#define vwarn(expression,msg)  ;

#define vmessage(msg,...)  ;

#define vglerror() ;

#endif

#if 1
#define vthread_msg(msg,...)                                            \
  fprintf(stderr,"VisusThreadMsg: %s::%u\n",__FILE__,__LINE__);         \
  fprintf(stderr, msg, ## __VA_ARGS__);
#else
#define vthread_msg(msg,...) ;
#endif
 
// Special Debug Prints For Very Verbose - Don't Necessarily Always Want In Debug Build
#define VISUS_XML_VERBOSE 100
#define VISUS_VSP_VERBOSE 1000
#define VISUS_RENDER_VERBOSE 400
#define VISUS_TIME_VERBOSE 500

#ifdef VISUS_VERBOSE
#define vverbose(msg,level,...)                 \
  if (level == VISUS_VERBOSE_LEVEL || !level) { \
    printf(msg, ## __VA_ARGS__);                \
    fflush(stdout);                             \
  }

#else
#define vverbose(msg,level,...) ;
#endif

#endif
