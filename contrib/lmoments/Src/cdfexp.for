      DOUBLE PRECISION FUNCTION CDFEXP(X,PARA)
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
C  DISTRIBUTION FUNCTION OF THE EXPONENTIAL DISTRIBUTION
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARA(2)
      DATA ZERO/0D0/,ONE/1D0/
      U=PARA(1)
      A=PARA(2)
      IF(A.LE.ZERO)GOTO 1000
      Y=(X-U)/A
      CDFEXP=ZERO
      IF(Y.LE.ZERO)RETURN
      CDFEXP=ONE-DEXP(-Y)
      RETURN
C
 1000 WRITE(6,7000)
      CDFEXP=ZERO
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE CDFEXP : PARAMETERS INVALID')
      END
