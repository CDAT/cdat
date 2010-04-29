      SUBROUTINE PELGAM(XMOM,PARA)
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
C  PARAMETER ESTIMATION VIA L-MOMENTS FOR THE GAMMA DISTRIBUTION
C
C  PARAMETERS OF ROUTINE:
C  XMOM   * INPUT* ARRAY OF LENGTH 2. CONTAINS THE L-MOMENTS LAMBDA-1,
C                  LAMBDA-2.
C  PARA   *OUTPUT* ARRAY OF LENGTH 2. ON EXIT, CONTAINS THE PARAMETERS
C                  IN THE ORDER ALPHA, BETA (SHAPE, SCALE).
C
C  OTHER ROUTINES USED: DLGAMA
C
C  METHOD: RATIONAL APPROXIMATION IS USED TO EXPRESS ALPHA AS A FUNCTION
C  OF L-CV. RELATIVE ACCURACY OF THE  APPROXIMATION IS BETTER THAN 5E-5.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION XMOM(2),PARA(2)
      DATA ZERO/0D0/,HALF/0.5D0/ONE/1D0/
C
C         CONSTANTS USED IN MINIMAX APPROXIMATIONS
C
      DATA A1,A2,A3/-0.3080D0,-0.05812D0,0.01765D0/
      DATA B1,B2,B3,B4/0.7213D0,-0.5947D0,-2.1817D0,1.2113D0/
      DATA PI/3.1415927D0/
C
      IF(XMOM(1).LE.XMOM(2).OR.XMOM(2).LE.ZERO)GOTO 1000
      CV=XMOM(2)/XMOM(1)
      IF(CV.GE.HALF)GOTO 10
      T=PI*CV*CV
      ALPHA=(ONE+A1*T)/(T*(ONE+T*(A2+T*A3)))
      GOTO 20
   10 CONTINUE
      T=ONE-CV
      ALPHA=T*(B1+T*B2)/(ONE+T*(B3+T*B4))
   20 CONTINUE
      PARA(1)=ALPHA
      PARA(2)=XMOM(1)/ALPHA
      RETURN
C
 1000 WRITE(6,7000)
      PARA(1)=ZERO
      PARA(2)=ZERO
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE PELGAM : L-MOMENTS INVALID')
      END
