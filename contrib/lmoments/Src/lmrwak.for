      SUBROUTINE LMRWAK(PARA,XMOM,NMOM)
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
C  L-MOMENT RATIOS FOR THE WAKEBY DISTRIBUTION
C
C  PARAMETERS OF ROUTINE:
C  PARA   * INPUT* ARRAY OF LENGTH 5. CONTAINS THE PARAMETERS OF THE
C                  DISTRIBUTION, IN THE ORDER XI, ALPHA, BETA, GAMMA,
C                  DELTA.
C  XMOM   *OUTPUT* ARRAY OF LENGTH NMOM. ON EXIT, CONTAINS THE L-MOMENTS
C                  LAMBDA-1, LAMBDA-2, TAU-3, TAU-4, ... .
C  NMOM   * INPUT* NUMBER OF L-MOMENTS TO BE FOUND. AT MOST 20.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARA(5),XMOM(NMOM)
      DATA ZERO/0D0/,ONE/1D0/,TWO/2D0/
C
      XI=PARA(1)
      A=PARA(2)
      B=PARA(3)
      C=PARA(4)
      D=PARA(5)
C
C         TEST FOR VALID PARAMETERS
C
      IF(D.GE.ONE)GOTO 1000
      IF(B+D.LE.ZERO.AND.(B.NE.ZERO.OR.C.NE.ZERO.OR.D.NE.ZERO))GOTO 1000
      IF(A.EQ.ZERO.AND.B.NE.ZERO)GOTO 1000
      IF(C.EQ.ZERO.AND.D.NE.ZERO)GOTO 1000
      IF(C.LT.ZERO)GOTO 1000
      IF(A+C.LT.ZERO)GOTO 1000
      IF(A.EQ.ZERO.AND.C.EQ.ZERO)GOTO 1000
      IF(NMOM.GT.20)GOTO 1010
C
C         LAMBDA-1
C
      Y=A/(ONE+B)
      Z=C/(ONE-D)
      XMOM(1)=XI+Y+Z
      IF(NMOM.EQ.1)RETURN
C
C         LAMBDA-2
C
      Y=Y/(TWO+B)
      Z=Z/(TWO-D)
      ALAM2=Y+Z
      XMOM(2)=ALAM2
      IF(NMOM.EQ.2)RETURN
C
C         HIGHER MOMENTS
C
      DO 10 M=3,NMOM
      AM=M
      Y=Y*(AM-TWO-B)/(AM+B)
      Z=Z*(AM-TWO+D)/(AM-D)
      XMOM(M)=(Y+Z)/ALAM2
   10 CONTINUE
      RETURN
C
 1000 WRITE(6,7000)
      RETURN
 1010 WRITE(6,7010)
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE LMRWAK : PARAMETERS INVALID')
 7010 FORMAT(' *** ERROR *** ROUTINE LMRWAK : PARAMETER NMOM TOO LARGE')
      END
