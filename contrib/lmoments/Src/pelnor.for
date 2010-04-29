      SUBROUTINE PELNOR(XMOM,PARA)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  PARAMETER ESTIMATION VIA L-MOMENTS FOR THE NORMAL DISTRIBUTION
C
C  PARAMETERS OF ROUTINE:
C  XMOM   * INPUT* ARRAY OF LENGTH 2. CONTAINS THE L-MOMENTS LAMBDA-1,
C                  LAMBDA-2.
C  PARA   *OUTPUT* ARRAY OF LENGTH 2. ON EXIT, CONTAINS THE PARAMETERS
C                  IN THE ORDER MU, SIGMA (LOCATION, SCALE).
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION XMOM(2),PARA(2)
      DATA ZERO/0D0/
      DATA ROOTPI/1.7724 53850 90551 603D0/
C
      IF(XMOM(2).LE.ZERO)GOTO 1000
      PARA(2)=XMOM(2)*ROOTPI
      PARA(1)=XMOM(1)
      RETURN
C
 1000 WRITE(6,7000)
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE PELNOR : L-MOMENTS INVALID')
      END
