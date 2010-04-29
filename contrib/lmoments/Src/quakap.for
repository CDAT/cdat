      DOUBLE PRECISION FUNCTION QUAKAP(F,PARA)
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
C  QUANTILE FUNCTION OF THE KAPPA DISTRIBUTION
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARA(4)
      DATA ZERO/0D0/,ONE/1D0/
      U=PARA(1)
      A=PARA(2)
      G=PARA(3)
      H=PARA(4)
      IF(A.LE.ZERO)GOTO 1000
      IF(F.LE.ZERO.OR.F.GE.ONE)GOTO 10
      Y=-DLOG(F)
      IF(H.NE.ZERO)Y=(ONE-DEXP(-H*Y))/H
      Y=-DLOG(Y)
      IF(G.NE.ZERO)Y=(ONE-DEXP(-G*Y))/G
      QUAKAP=U+A*Y
      RETURN
C
   10 IF(F.EQ.ZERO)GOTO 20
      IF(F.EQ.ONE)GOTO 30
      GOTO 1010
   20 IF(H.LE.ZERO.AND.G.LT.ZERO)QUAKAP=U+A/G
      IF(H.LE.ZERO.AND.G.GE.ZERO)GOTO 1010
      IF(H.GT.ZERO.AND.G.NE.ZERO)QUAKAP=U+A/G*(ONE-H**(-G))
      IF(H.GT.ZERO.AND.G.EQ.ZERO)QUAKAP=U+A*DLOG(H)
      RETURN
   30 IF(G.LE.ZERO)GOTO 1010
      QUAKAP=U+A/G
      RETURN
C
 1000 WRITE(6,7000)
      QUAKAP=ZERO
      RETURN
 1010 WRITE(6,7010)
      QUAKAP=ZERO
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE QUAKAP : PARAMETERS INVALID')
 7010 FORMAT(' *** ERROR *** ROUTINE QUAKAP :',
     *  ' ARGUMENT OF FUNCTION INVALID')
      END
