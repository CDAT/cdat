      DOUBLE PRECISION FUNCTION QUAPE3(F,PARA)
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
C  QUANTILE FUNCTION OF THE PEARSON TYPE 3 DISTRIBUTION
C
C  OTHER ROUTINES USED: DERF,DLGAMA,GAMIND,QUAGAM,QUASTN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARA(3),PAR(2)
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/,TWO/2D0/,FOUR/4D0/
C
C         SMALL IS USED TO TEST WHETHER SKEWNESS IS EFFECTIVELY ZERO
C
      DATA SMALL/1D-6/
C
      IF(PARA(2).LE.ZERO)GOTO 1000
      GAMMA=PARA(3)
      IF(F.LE.ZERO.OR.F.GE.ONE)GOTO 20
      IF(DABS(GAMMA).LT.SMALL)GOTO 10
      ALPHA=FOUR/(GAMMA*GAMMA)
      BETA=DABS(HALF*PARA(2)*GAMMA)
      PAR(1)=ALPHA
      PAR(2)=BETA
      IF(GAMMA.GT.ZERO)QUAPE3=PARA(1)-ALPHA*BETA+QUAGAM(F,PAR)
      IF(GAMMA.LT.ZERO)QUAPE3=PARA(1)+ALPHA*BETA-QUAGAM(ONE-F,PAR)
      RETURN
C
C         ZERO SKEWNESS
C
   10 QUAPE3=PARA(1)+PARA(2)*QUASTN(F)
      RETURN
C
   20 IF(F.EQ.ZERO.AND.GAMMA.GT.ZERO)GOTO 30
      IF(F.EQ.ONE .AND.GAMMA.LT.ZERO)GOTO 30
      WRITE(6,7000)
      QUAPE3=ZERO
      RETURN
   30 QUAPE3=PARA(1)-TWO*PARA(2)/GAMMA
      RETURN
C
 1000 WRITE(6,7010)
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE QUAPE3 :',
     *  ' ARGUMENT OF FUNCTION INVALID')
 7010 FORMAT(' *** ERROR *** ROUTINE QUAPE3 : PARAMETERS INVALID')
      END
