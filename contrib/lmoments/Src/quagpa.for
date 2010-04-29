      DOUBLE PRECISION FUNCTION QUAGPA(F,PARA)
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
C  QUANTILE FUNCTION OF THE GENERALIZED PARETO DISTRIBUTION
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARA(3)
      DATA ZERO/0D0/,ONE/1D0/
      U=PARA(1)
      A=PARA(2)
      G=PARA(3)
      IF(A.LE.ZERO)GOTO 1000
      IF(F.LE.ZERO.OR.F.GE.ONE)GOTO 10
      Y=-DLOG(ONE-F)
      IF(G.NE.ZERO)Y=(ONE-DEXP(-G*Y))/G
      QUAGPA=U+A*Y
      RETURN
C
   10 IF(F.EQ.ZERO)QUAGPA=U
      IF(F.EQ.ZERO)RETURN
      IF(F.EQ.ONE.AND.G.GT.ZERO)QUAGPA=U+A/G
      IF(F.EQ.ONE.AND.G.GT.ZERO)RETURN
      WRITE(6,7000)
      QUAGPA=ZERO
      RETURN
C
 1000 WRITE(6,7010)
      QUAGPA=ZERO
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE QUAGPA :',
     *  ' ARGUMENT OF FUNCTION INVALID')
 7010 FORMAT(' *** ERROR *** ROUTINE QUAGPA : PARAMETERS INVALID')
      END
