      DOUBLE PRECISION FUNCTION CDFGLO(X,PARA)
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
C  DISTRIBUTION FUNCTION OF THE GENERALIZED LOGISTIC DISTRIBUTION
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
      U=PARA(1)
      A=PARA(2)
      G=PARA(3)
      IF(A.LE.ZERO)GOTO 1000
      Y=(X-U)/A
      IF(G.EQ.ZERO)GOTO 20
      ARG=ONE-G*Y
      IF(ARG.GT.SMALL)GOTO 10
      IF(G.LT.ZERO)CDFGLO=ZERO
      IF(G.GT.ZERO)CDFGLO=ONE
      RETURN
   10 Y=-DLOG(ARG)/G
   20 CDFGLO=ONE/(ONE+DEXP(-Y))
      RETURN
C
 1000 WRITE(6,7000)
      CDFGLO=ZERO
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE CDFGLO : PARAMETERS INVALID')
      END
