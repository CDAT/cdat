c -*-Mode: Fortran;-*-
c  Module:      cdms.inc - CDMS Fortran API include file
c 
c  Copyright:	1996, Regents of the University of California
c 		This software may not be distributed to others without
c 		permission of the author.
c 
c  Author:      Bob Drach, Lawrence Livermore National Laboratory
c               drach@llnl.gov
c 
c  Version:     $Id$
c 
c  Revision History:
c 
c  $Log: cdms.inc,v $
c Revision 1.1  1996/04/04  22:36:02  drach
c - Initial version
c
c 

c
c     Declarations
c     
      external fcdchar2comp, fcdchar2rel, fcdcomp2char
      external fcdcomp2rel, fcdrel2char, fcdrel2comp
      external fcdrel2rel

      integer CD_STANDARD, CD_JULIAN, CD_NOLEAP, CD_360, CD_CLIM
      integer CD_CLIMLEAP, CD_CLIM360, CD_NULL_YEAR, CD_NULL_MONTH
      integer CD_LAST_DAY, CD_MAX_CHARTIME, CD_MAX_TIME_DELTA
      integer CD_NULL_DAY
      double precision CD_NULL_HOUR
c     
c     Time types and parameters
c     
      parameter(CD_STANDARD = '00001111'X)
      parameter(CD_JULIAN = '00011111'X)
      parameter(CD_MIXED = '00021111'X)
      parameter(CD_NOLEAP = '00001011'X)
      parameter(CD_360 = '00000011'X)
      parameter(CD_CLIM = '00001000'X)
      parameter(CD_CLIMLEAP = '00001100'X)
      parameter(CD_CLIM360 = '00000000'X)
      
      parameter(CD_NULL_YEAR = 0)
      parameter(CD_NULL_MONTH = 1)
      parameter(CD_LAST_DAY = 1)
      parameter(CD_MAX_CHARTIME = 48)
      parameter(CD_MAX_TIME_DELTA = 64)
      parameter(CD_NULL_DAY = 1)
      parameter(CD_NULL_HOUR = 0.0)
      





