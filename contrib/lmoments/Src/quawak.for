      DOUBLE PRECISION FUNCTION QUAWAK(F,PARA)
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
C  QUANTILE FUNCTION OF THE WAKEBY DISTRIBUTION
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARA(5)
      DATA ZERO/0D0/,ONE/1D0/
C
C         UFL SHOULD BE CHOSEN SO THAT EXP(UFL) JUST DOES NOT CAUSE
C         UNDERFLOW
C
      DATA UFL/-170D0/
C
      XI=PARA(1)
      A=PARA(2)
      B=PARA(3)
      C=PARA(4)
      D=PARA(5)
C
C         TEST FOR VALID PARAMETERS
C
      IF(B+D.LE.ZERO.AND.(B.NE.ZERO.OR.C.NE.ZERO.OR.D.NE.ZERO))GOTO 1000
      IF(A.EQ.ZERO.AND.B.NE.ZERO)GOTO 1000
      IF(C.EQ.ZERO.AND.D.NE.ZERO)GOTO 1000
      IF(C.LT.ZERO.OR.A+C.LT.ZERO)GOTO 1000
      IF(A.EQ.ZERO.AND.C.EQ.ZERO)GOTO 1000
C
      IF(F.LE.ZERO.OR.F.GE.ONE)GOTO 10
      Z=-DLOG(ONE-F)
      Y1=Z
      IF(B.EQ.ZERO)GOTO 5
      TEMP=-B*Z
      IF(TEMP.LT.UFL)Y1=ONE/B
      IF(TEMP.GE.UFL)Y1=(ONE-DEXP(TEMP))/B
    5 CONTINUE
      Y2=Z
      IF(D.NE.ZERO)Y2=(ONE-DEXP(D*Y2))/(-D)
      QUAWAK=XI+A*Y1+C*Y2
      RETURN
C
   10 IF(F.EQ.ZERO)GOTO 20
      IF(F.EQ.ONE)GOTO 30
      GOTO 1010
   20 QUAWAK=XI
      RETURN
   30 IF(D.GT.ZERO)GOTO 1010
      IF(D.LT.ZERO)QUAWAK=XI+A/B-C/D
      IF(D.EQ.ZERO.AND.C.GT.ZERO)GOTO 1010
      IF(D.EQ.ZERO.AND.C.EQ.ZERO.AND.B.EQ.ZERO)GOTO 1010
      IF(D.EQ.ZERO.AND.C.EQ.ZERO.AND.B.GT.ZERO)QUAWAK=XI+A/B
      RETURN
C
 1000 WRITE(6,7000)
      QUAWAK=ZERO
      RETURN
 1010 WRITE(6,7010)
      QUAWAK=ZERO
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE QUAWAK : PARAMETERS INVALID')
 7010 FORMAT(' *** ERROR *** ROUTINE QUAWAK :',
     *  ' ARGUMENT OF FUNCTION INVALID')
      END
