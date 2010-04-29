      SUBROUTINE SAMPWM(X,N,XMOM,NMOM,A,B,KIND)
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
C  PROBABILITY WEIGHTED MOMENTS OF A DATA ARRAY
C
C  PARAMETERS OF ROUTINE:
C  X      * INPUT* ARRAY OF LENGTH N. CONTAINS THE DATA, IN ASCENDING
C                  ORDER.
C  N      * INPUT* NUMBER OF DATA VALUES
C  XMOM   *OUTPUT* ARRAY OF LENGTH NMOM. ON EXIT, CONTAINS THE SAMPLE
C                  PROBABILITY WEIGHTED MOMENTS. XMOM(I) CONTAINS
C                  ALPHA-SUB-(I-1) OR BETA-SUB-(I-1).
C  NMOM   * INPUT* NUMBER OF PROBABILITY WEIGHTED MOMENTS TO BE FOUND.
C                  AT MOST MAX(N,20).
C  A      * INPUT* ) PARAMETERS OF PLOTTING
C  B      * INPUT* ) POSITION (SEE BELOW)
C  KIND   * INPUT* SPECIFIES WHICH KIND OF PWM'S ARE TO BE FOUND.
C                  1  ALPHA-SUB-R = E ( X (1-F(X))**R )
C                  2  BETA -SUB-R = E ( X F(X)**R )
C
C  FOR UNBIASED ESTIMATES SET A AND B EQUAL TO ZERO. OTHERWISE,
C  PLOTTING-POSITION ESTIMATORS ARE USED, BASED ON THE PLOTTING POSITION
C  (J+A)/(N+B)  FOR THE J'TH SMALLEST OF N OBSERVATIONS. FOR EXAMPLE,
C  A=-0.35D0 AND B=0.0D0 YIELDS THE ESTIMATORS RECOMMENDED BY
C  HOSKING ET AL. (1985, TECHNOMETRICS) FOR THE GEV DISTRIBUTION.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION X(N),XMOM(NMOM)
      DATA ZERO/0D0/,ONE/1D0/
      IF(NMOM.GT.20.OR.NMOM.GT.N)GOTO 1000
      IF(KIND.NE.1.AND.KIND.NE.2)GOTO 1010
      DO 10 J=1,NMOM
   10 XMOM(J)=ZERO
      DN=N
      IF(A.EQ.ZERO.AND.B.EQ.ZERO)GOTO 50
      IF(A.LE.-ONE.OR.A.GE.B)GOTO 1020
C
C         PLOTTING-POSITION ESTIMATES OF PWM'S
C
      DO 30 I=1,N
      PPOS=(I+A)/(N+B)
      IF(KIND.EQ.1)PPOS=ONE-PPOS
      TERM=X(I)
      XMOM(1)=XMOM(1)+TERM
      DO 20 J=2,NMOM
      TERM=TERM*PPOS
   20 XMOM(J)=XMOM(J)+TERM
   30 CONTINUE
      DO 40 J=1,NMOM
   40 XMOM(J)=XMOM(J)/DN
      RETURN
C
C         UNBIASED ESTIMATES OF PWM'S
C
   50 DO 70 I=1,N
      DI=I
      WEIGHT=ONE/DN
      XMOM(1)=XMOM(1)+WEIGHT*X(I)
      DO 60 J=2,NMOM
      DJ=J-ONE
      IF(KIND.EQ.1)WEIGHT=WEIGHT*(DN-DI-DJ+ONE)/(DN-DJ)
      IF(KIND.EQ.2)WEIGHT=WEIGHT*(DI-DJ)/(DN-DJ)
   60 XMOM(J)=XMOM(J)+WEIGHT*X(I)
   70 CONTINUE
      RETURN
C
 1000 WRITE(6,7000)
      RETURN
 1010 WRITE(6,7010)
      RETURN
 1020 WRITE(6,7020)
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE SAMPWM : PARAMETER NMOM INVALID')
 7010 FORMAT(' *** ERROR *** ROUTINE SAMPWM : PARAMETER KIND INVALID')
 7020 FORMAT(' *** ERROR *** ROUTINE SAMPWM :',
     *  ' PLOTTING-POSITION PARAMETERS INVALID')
      END
