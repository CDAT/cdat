c drsdef.H,v 2.7 1995/10/26 23:23:21 drach Exp
c drsdef.H,v
c Revision 2.7  1995/10/26  23:23:21  drach
c - Added IDRS_NOCOPY aslun flag, NSL version
c
c Revision 2.6  1995/10/16  18:47:44  drach
c - Modified for DEC Alpha
c
c Revision 2.5  1995/03/30  00:50:37  drach
c Added disclaimer
c
c Revision 2.4  1993/10/21  01:23:26  drach
c changed name of sync options for consistency, added sync error.
c
c Revision 2.3  1993/10/20  17:17:14  drach
c Add autosync options.
c
c Revision 2.2  1993/07/14  22:35:35  drach
c Corrected CVS comment header.
c
c  * Revision 2.1  1993/07/14  22:33:20  drach
c  * Replaced drsdef.h with drsdef.H
c  *
c Revision 2.2  1993/07/13  17:53:31  drach
c Fixed up CVS install
c
c  * Revision 2.1  1993/07/13  01:12:49  drach
c  * Merged Sun, Unicos, SGI, and HP versions.
c  *
c Revision 2.4  1992/10/15  00:17:34  drach
c Corrected misspelling
c
c Revision 2.3  1992/10/14  23:15:39  drach
c Added putdic options.
c
c Revision 2.2  1992/10/06  00:54:25  drach
c Added putdic errors.
c
c Revision 2.1  1992/05/22  01:08:29  drach
c Removed drsmsg (error messages), moved to drserr.f
c
c Revision 2.0  1992/03/07  00:10:39  drach
c Entered into RCS
c
c
c                     Data Retrieval and Storage System
c
c**********************************************************************
c
c			DISCLAIMER
c
c   This software was prepared as an account of work sponsored by an
c   agency of the United States Government. Neither the United
c   States Government nor the University of California nor any of
c   their employees, makes any warranty, express or implied, or
c   assumes any liability or responsibility for the accuracy,
c   completeness, or usefulness of any information, apparatus,
c   product, or process disclosed, or represents that its use would
c   not infringe privately owned rights. Reference herein to any
c   specific commercial products, process, or service by trade name,
c   trademark, manufacturer, or otherwise, does not necessarily
c   constitute or imply its endorsement, recommendation, or favoring
c   by the United States Government or the University of California.
c   The views and opinions of authors expressed herein do not
c   necessarily state or reflect those of the United States
c   Government or the University of California, and shall not be
c   used for advertising or product endorsement purposes.
c   
c**********************************************************************
c
c#######################################################################
c                 drsdef.h

c#######################################################################
c     General definitions
      parameter(IDRS_NOVALUE=0
     $     ,IDRS_DEFAULT=0
     $     )
     
c     DimensionType
      parameter (IDRS_EQUALLY_SPACED = 1,
     $     IDRS_UNEQUALLY_SPACED = 2)

c     FileStatus
      parameter (IDRS_READ = 1,
     $     IDRS_CREATE = 2,
     $     IDRS_EXTEND = 3)

c     MachineName
      parameter (IDRS_SUN = 1,
     $     IDRS_CRAY = 2)

      parameter (IDRS_MACHINE=IDRS_SUN,
     $    IDRS_BYTES_PER_WORD=4
     $    )
c     see GETIND
      parameter (IDRS_MAXPATH=1024)

	character*8 DRS_FILETAG

c     DRSVersion
c     DRS_VERSION is current version
c     DRS_MAXVERSION is max allowed version before version
c     is considered *novalue*
      parameter(DRS_VERSION=2.1,
     $    DRS_MAXVERSION=10.0,
     $    DRS_FILETAG='DRS DATA'
     $    )

c     Inquiry operators (INQDICT)
      parameter(IDRS_GETFIRSTVAR=1
     $     ,IDRS_GETNEXTVAR=2
     $     )
      
c     Cray-to-IEEE translation parameters
      parameter(IDRS_LEFTHALFWORD=0
     $     ,IDRS_RIGHTHALFWORD=4
     $     )

c     ElementType
      parameter(IDRS_I4=1
     $     ,IDRS_I8=2
     $     ,IDRS_IEEE_R4=3
     $     ,IDRS_CRAY_R8=4
     $     ,IDRS_ASCII=5
     $     ,IDRS_USER=6
     $     )

c     Synchronization options
      parameter(IDRS_SYNC_OFF=1
     $     ,IDRS_SYNC_ON=2)

c     Error reporting
      parameter (IDRS_NOREPORT=1
     $     ,IDRS_FATAL=2
     $     ,IDRS_WARNING=3
     $     ,IDRS_INTERNAL=4
     $     )

c     Putdic options
      parameter (IDRS_BLANKS_ARE_NULL=1
     $     ,IDRS_BLANKS_ARE_LITERAL=2
     $     )

c     Error definitions
      parameter (IDRS_SUCCESS=0
     $     ,IDRS_NOMEMORY=1
     $     ,IDRS_BINFAILED=2002
     $     ,IDRS_BADLEN=3
     $     ,IDRS_NOMONO=4
     $     ,IDRS_NOCOMPARISON=2005
     $     ,IDRS_VDBNOTFOUND=6
     $     ,IDRS_BADDIM=7
     $     ,IDRS_NOTMONOTONE=8
     $     ,IDRS_DICTREADERROR=9)
      parameter (IDRS_NODICTFILE=10
     $     ,IDRS_BADLU=11
     $     ,IDRS_BADTYPE=12
     $     ,IDRS_AMBIGUITYEXISTS=13
     $     ,IDRS_CANNOTADDDATA=14
     $     ,IDRS_DICTFULL=15
     $     ,IDRS_VERSION1FILE=1016
     $     ,IDRS_NEWFILEFORMAT=1017
     $     ,IDRS_CANNOTREADHEADER=18
     $     ,IDRS_CANNOTREADDATA=19)
      parameter(IDRS_BADDIMNAME=20
     $     ,IDRS_TOOMANYFILES=21
     $     ,IDRS_CANNOTOPENDICT=22
     $     ,IDRS_CANNOTOPENDATA=23
     $     ,IDRS_BADSTATUS=24
     $     ,IDRS_BADDIMTYPE=25
     $     ,IDRS_INDEXHIGH=2026
     $     ,IDRS_INDEXLOW=2027
     $     ,IDRS_INDEXBETWEEN=2028
     $     ,IDRS_NORANGE=29)
      parameter(IDRS_SAVEBUFOVERFLOW=30
     $     ,IDRS_BADERRLEVEL=31
     $     ,IDRS_ERROROUTOFRANGE=32
     $     ,IDRS_CANNOTWRITEHEADER=33
     $     ,IDRS_CANNOTWRITEDATA=34
     $     ,IDRS_BADCHARLEN=35
     $     ,IDRS_BADOPER=36
     $     ,IDRS_NOMOREVARS=1037
     $     ,IDRS_DICTALREADYOPEN=38
     $     ,IDRS_LOOKUPFAILED=2039
     $     )
      parameter(IDRS_DICTWRITEERROR=40
     $     ,IDRS_DICTEXTENDERROR=41
     $     ,IDRS_DATEXTENDERROR=42
     $     ,IDRS_DICTRUNCATEERR=43
     $     ,IDRS_DATTRUNCATEERR=44
     $     ,IDRS_BADIEEEFP=1045
     $     ,IDRS_BADCRAYFP=1046
     $     ,IDRS_BADCRAYINT=1047
     $     ,IDRS_CANNOTCONVERT=48
     $     ,IDRS_INEXACTMATCH=1049
     $     )
      parameter(IDRS_DUPLICATEVAR=50
     $     ,IDRS_CANNOTWRITEDIC=51
     $     ,IDRS_BADSYNCOPT=52
     $     ,IDRS_LASTERROR=53
     $     )

