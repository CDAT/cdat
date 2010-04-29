/*****************************************************************************\
 *
 * utilities.cc -- some utility functions & macros
 *
 *
 * Author:      Fausto Bernardini (fxb@cs.purdue.edu)
 *
 * Created - June 15, 1993
 * Ported to C++ by Raymund Merkert - June 1995
 * Changes by Fausto Bernardini - Sept 1995 
 *
\*****************************************************************************/

// $Id: utilities.C,v 1.2 2003/09/02 19:08:26 scorzell Exp $

#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <sys/stat.h>
#include <iostream.h>

#ifndef WIN32
#include <strstream.h>
#include <unistd.h>
#endif


#include "utilities.h"


/*****************************************************************************\
 * Misc
\*****************************************************************************/

void warning( const char *msg )
{
  cerr << msg << endl;
}


void panic( const char *msg )
{
  cerr << "Panic: " << msg << endl;
  exit( 1 );
}


void initrand( void )
{
  srand( (unsigned)time( 0 ) );
}


void run_stamp( char *msg, int sz )
{
  time_t        h;
  struct tm     *t;

  h = time( 0 );
  t = localtime( &h );
  ostrstream os(msg, sz);
  os << "Date: " << asctime(t) 
     << "Host: " << getenv( "HOST" ) << endl << ends;
}


/*****************************************************************************\
 * read gzip'ed files
\*****************************************************************************/

gzifstream::gzifstream()
{
}


gzifstream::gzifstream(const char* name)
{
  if( !gzopen(name) )
    attach(fileno(fp));
}


gzifstream::~gzifstream()
{
  gzclose();
}


void gzifstream::open(const char* name)
{
  if( !gzopen(name) )
    attach(fileno(fp));
}


int gzifstream::gzopen(const char* name)
{
  char zname[FILENAME_MAX+4];

  fp = NULL;
  // check if file exists
  if (access(name, R_OK) != 0) return EOF;
  // check extension
  int len = strlen(name);
  if ((name[len-1] == 'Z' && name[len-2] == '.') ||
      (name[len-1]=='z' && name[len-2] == 'g' && name[len-3] == '.')) {
    // name is a compressed file, let's see if we can read it
    sprintf(zname, "zcat %s", name);
    if ((fp = popen(zname, "r")) == NULL) return EOF;
    pipe = true;
  } else {
    // non compressed
    if ((fp = fopen(name, "r")) == NULL) return EOF;
    pipe = false;
  }
  return 0;
}


int gzifstream::gzclose()
{
  if( pipe ) {
    return pclose(fp);
  } else {
    return fclose(fp);
  }
}


/*****************************************************************************\
 * timing
\*****************************************************************************/


#ifdef sparc
#define CLOCKS_PER_SEC 1000000
#endif


double diffclock( clock_t t2, clock_t t1 )
{
  return( (double)t2/CLOCKS_PER_SEC-(double)t1/CLOCKS_PER_SEC );
}


/*****************************************************************************\
 * Brakes !!   (used to stop execution)
\*****************************************************************************/

bool brakes;

#ifdef __mips__

static void brake( ... )
{
  brakes = true;
}


void brake_init( void )
{
  brakes = false;
  signal( SIGINT, &brake );
}

#else if defined __sparc__

static void brake( int )
{
  brakes = true;
}


typedef void (*SIGNAL_TYPE(int, void (*)(int)))(int);

void brake_init( void )
{
  brakes = false;
  *((SIGNAL_TYPE*)(&signal))( SIGINT, &brake );
}
#endif


/*****************************************************************************\
 * Check Boolean condition
\*****************************************************************************/

static int check_counter;

void check_reset( void )
{
  check_counter = 0;
}


int check_count( void )
{
  return( check_counter );
}


void check( int cond, const char *msg )
{
  if( !cond ) {
    cerr << "Warning: " << msg << endl;
    check_counter++;
  }
}
