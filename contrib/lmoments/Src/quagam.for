      DOUBLE PRECISION FUNCTION QUAGAM(F,PARA)
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
C  QUANTILE FUNCTION OF THE GAMMA DISTRIBUTION
C
C  OTHER ROUTINES USED: DERF,DLGAMA,GAMIND,QUASTN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARA(2)
      DATA ZERO/0D0/,P01/0.01D0/,ONE/1D0/,NINE/9D0/
C
C         EPS,MAXIT CONTROL THE TEST FOR CONVERGENCE OF N-R ITERATION
C
      DATA EPS/1D-10/,MAXIT/30/
C
      QUAGAM=ZERO
      ALPHA=PARA(1)
      BETA=PARA(2)
      IF(ALPHA.LE.ZERO.OR.BETA.LE.ZERO)GOTO 1000
      IF(F.LT.ZERO.OR.F.GE.ONE)GOTO 1010
      IF(F.EQ.ZERO)RETURN
      AM1=ALPHA-ONE
      IF(AM1.NE.ZERO)GOTO 10
C
C         CASE ALPHA.EQ.1 - GAMMA IS EXPONENTIAL
C
      QUAGAM=(-DLOG(ONE-F))*BETA
      RETURN
C
C         INITIAL ESTIMATE OF ROOT OF EQUATION GAMIND(X)=F:
C         - IF ALPHA.GT.1, USE WILSON-HILFERTY APPROXIMATION IF IT'S
C           POSITIVE AND NOT TOO CLOSE TO ZERO;
C         - IF ALPHA.LT.1, OR IF W-H APPROX. ISN'T POSITIVE ENOUGH,
C           USE THE SMALL-X APPROXIMATION OF IGNORING THE EXP(-T) TERM
C           IN THE INTEGRAL DEFINING GAMIND(X)
C
   10 DLOGG=DLGAMA(ALPHA)
      IF(AM1.LE.ZERO)GOTO 20
      ROOT=ALPHA*(ONE-ONE/(NINE*ALPHA)+QUASTN(F)/DSQRT(NINE*ALPHA))**3
      IF(ROOT.GT.P01*ALPHA)GOTO 30
   20 ROOT=DEXP((DLOG(ALPHA*F)+DLOGG)/ALPHA)
   30 CONTINUE
C
C         REFINE INITIAL ESTIMATE BY NEWTON-RAPHSON ITERATION
C
      DO 40 IT=1,MAXIT
      FUNC=GAMIND(ROOT,ALPHA,DLOGG)-F
      RINC=FUNC*DEXP(DLOGG+ROOT-AM1*DLOG(ROOT))
      ROOT=ROOT-RINC
      IF(DABS(FUNC).LE.EPS)GOTO 50
   40 CONTINUE
      WRITE(6,7020)
C
C         SCALE SOLUTION
C
   50 QUAGAM=ROOT*BETA
      RETURN
C
 1000 WRITE(6,7000)
      RETURN
 1010 WRITE(6,7010)
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE QUAGAM : PARAMETERS INVALID')
 7010 FORMAT(' *** ERROR *** ROUTINE QUAGAM :',
     *  ' ARGUMENT OF FUNCTION INVALID')
 7020 FORMAT(' ** WARNING ** ROUTINE QUAGAM :',
     *  ' ITERATION HAS NOT CONVERGED. RESULT MAY BE UNRELIABLE')
      END
