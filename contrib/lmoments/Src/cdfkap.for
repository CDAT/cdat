      DOUBLE PRECISION FUNCTION CDFKAP(X,PARA)
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
C  DISTRIBUTION FUNCTION OF THE KAPPA DISTRIBUTION
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARA(4)
      DATA ZERO/0D0/,ONE/1D0/
C
C         SMALL IS A SMALL NUMBER, USED TO TEST WHETHER X IS
C         EFFECTIVELY AT AN ENDPOINT OF THE DISTRIBUTION
C
      DATA SMALL/1D-15/
C
      U=PARA(1)
      A=PARA(2)
      G=PARA(3)
      H=PARA(4)
      IF(A.LE.ZERO)GOTO 1000
      Y=(X-U)/A
      IF(G.EQ.ZERO)GOTO 20
      ARG=ONE-G*Y
      IF(ARG.GT.SMALL)GOTO 10
      IF(G.LT.ZERO)CDFKAP=ZERO
      IF(G.GT.ZERO)CDFKAP=ONE
      RETURN
   10 Y=-DLOG(ARG)/G
   20 Y=DEXP(-Y)
      IF(H.EQ.ZERO)GOTO 40
      ARG=ONE-H*Y
      IF(ARG.GT.SMALL)GOTO 30
      CDFKAP=ZERO
      RETURN
   30 Y=-DLOG(ARG)/H
   40 CDFKAP=DEXP(-Y)
      RETURN
C
 1000 WRITE(6,7000)
      CDFKAP=ZERO
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE CDFKAP : PARAMETERS INVALID')
      END
