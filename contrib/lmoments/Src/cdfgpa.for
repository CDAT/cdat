      DOUBLE PRECISION FUNCTION CDFGPA(X,PARA)
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
C  DISTRIBUTION FUNCTION OF THE GENERALIZED PARETO DISTRIBUTION
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARA(3)
      DATA ZERO/0D0/,ONE/1D0/
C
C         SMALL IS USED TO TEST WHETHER X IS EFFECTIVELY AT
C         THE ENDPOINT OF THE DISTRIBUTION
C
      DATA SMALL/1D-15/
C
      CDFGPA=ZERO
      U=PARA(1)
      A=PARA(2)
      G=PARA(3)
      IF(A.LE.ZERO)GOTO 1000
      Y=(X-U)/A
      IF(Y.LE.ZERO)RETURN
      IF(G.EQ.ZERO)GOTO 20
      ARG=ONE-G*Y
      IF(ARG.GT.SMALL)GOTO 10
      CDFGPA=ONE
      RETURN
   10 Y=-DLOG(ARG)/G
   20 CDFGPA=ONE-DEXP(-Y)
      RETURN
C
 1000 WRITE(6,7000)
      CDFGPA=ZERO
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE CDFGPA : PARAMETERS INVALID')
      END
