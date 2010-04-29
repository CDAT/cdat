/*****************************************************************************\
 *
 * utilities.h -- some utility functions & macros
 *
 *
 * Author:      Fausto Bernardini (fxb@cs.purdue.edu)
 *
 * Created - June 15, 1993
 * Ported to C++ by Raymund Merkert - June 1995
 * Changes by Fausto Bernardini - Sept 1995 
 *
\*****************************************************************************/

// $Id: utilities.h,v 1.1.2.1 2007/09/11 23:52:24 pascucci Exp $

#ifndef __UTILITIES_H
#define __UTILITIES_H

#include <time.h>
#include <limits.h>
#include <fstream>
using namespace std;

#include <stdio.h>

#include "basic.h"


/*****************************************************************************\
 * doc
\*****************************************************************************/

//@Man: Utilities
//@Memo: Miscellaneous utility functions
//@Doc: Miscellaneous utility functions.
//@{


/*****************************************************************************\
 * Misc
\*****************************************************************************/

//@ManDoc: Issue a warning message.
void warning( const char *msg );

//@ManDoc: Print an error message and exit.
void panic( const char *msg );

//@ManDoc: Initialize random numebr generator (based on current clock).
void initrand( void );

//@ManDoc: Print date, time, host.
void run_stamp( char *msg, int sz );


/*****************************************************************************\
 * read gzip'ed files
\*****************************************************************************/

class gzifstream: public ifstream
{
public:
  //@ManDoc: Default c'tor
  gzifstream();
  /*@ManDoc: Open file #name# for reading. If the file name ends with a ".gz"
    extension, it is assumed to be a gzip'ed file and will be piped through zcat */
  gzifstream(const char* name);
  //@ManDoc: D'tor
  ~gzifstream();
  //@ManDoc: Open file #name# for reading.
  void open(const char* name);

private:
  int gzopen(const char* name);
  int gzclose();

  bool pipe;
  FILE* fp;
};


/*****************************************************************************\
 * timing
\*****************************************************************************/

//@ManDoc: Time elapsed between two instants of time.
double diffclock( clock_t t2, clock_t t1 );


/*****************************************************************************\
 * brakes !!   (used to stop execution)
\*****************************************************************************/

//@ManDoc: When a <ctrl>-c is hit, the variable brakes is set to true.
extern bool brakes;

//@ManDoc: Initialize #brake#.
void brake_init( void );


/*****************************************************************************\
 * Check Bool condition
\*****************************************************************************/

//@ManDoc: Check Boolean condition.
void check( int cond, const char *msg );

//@ManDoc: Reset check counter.
void check_reset( void );

//@ManDoc: Return number of failed checks.
int check_count( void );


//@}
#endif
