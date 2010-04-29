      SUBROUTINE LMRKAP(PARA,XMOM,NMOM)
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
C  L-MOMENT RATIOS FOR THE KAPPA DISTRIBUTION
C
C  PARAMETERS OF ROUTINE:
C  PARA   * INPUT* ARRAY OF LENGTH 4. CONTAINS THE PARAMETERS OF THE
C                  DISTRIBUTION, IN THE ORDER XI, ALPHA, K, H.
C  XMOM   *OUTPUT* ARRAY OF LENGTH NMOM. ON EXIT, CONTAINS THE L-MOMENTS
C                  LAMBDA-1, LAMBDA-2, TAU-3, TAU-4, ... .
C  NMOM   * INPUT* NUMBER OF L-MOMENTS TO BE FOUND. AT MOST 20.
C
C  OTHER ROUTINES USED: DLGAMA,DIGAMD
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARA(4),XMOM(NMOM),BETA(20)
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/,THREE/3D0/,FOUR/4D0/,SIX/6D0/
C
C         EU  IS EULER'S CONSTANT
C
      DATA EU/0.577215664901532861D0/
C
C         SMALL IS USED TO TEST WHETHER H IS EFFECTIVELY ZERO
C         OFL SHOULD BE CHOSEN SO THAT EXP(OFL) JUST DOES NOT CAUSE
C         OVERFLOW
C
      DATA SMALL/1D-8/,OFL/170D0/
C
      U=PARA(1)
      A=PARA(2)
      G=PARA(3)
      H=PARA(4)
C
C         TEST FOR FEASIBLE PARAMETERS
C
      IF(A.LE.ZERO)GOTO 1000
      IF(G.LE.-ONE)GOTO 1000
      IF(H.LT.ZERO.AND.G*H.LE.-ONE)GOTO 1000
      IF(NMOM.GT.20)GOTO 1010
C
C         CALCULATE FUNCTIONS OCCURRING IN THE PWM'S BETA-SUB-R
C
      DLGAM=DLGAMA(ONE+G)
      ICASE=1
      IF(H.GT.ZERO)ICASE=3
      IF(DABS(H).LT.SMALL)ICASE=2
      IF(G.EQ.ZERO)ICASE=ICASE+3
      GOTO(10,30,50,70,90,110),ICASE
C
C         - CASE H<0, G NONZERO
C
   10 DO 20 IR=1,NMOM
      R=IR
      ARG=DLGAM+DLGAMA(-R/H-G)-DLGAMA(-R/H)-G*DLOG(-H)
      IF(DABS(ARG).GT.OFL)GOTO 1020
   20 BETA(IR)=DEXP(ARG)
      GOTO 130
C
C         - CASE H SMALL, G NONZERO
C
   30 DO 40 IR=1,NMOM
      R=IR
   40 BETA(IR)=DEXP(DLGAM-G*DLOG(R))*(ONE-HALF*H*G*(ONE+G)/R)
      GOTO 130
C
C         - CASE H>0, G NONZERO
C
   50 DO 60 IR=1,NMOM
      R=IR
      ARG=DLGAM+DLGAMA(ONE+R/H)-DLGAMA(ONE+G+R/H)-G*DLOG(H)
      IF(DABS(ARG).GT.OFL)GOTO 1020
   60 BETA(IR)=DEXP(ARG)
      GOTO 130
C
C         - CASE H<0, G=0
C
   70 DO 80 IR=1,NMOM
      R=IR
   80 BETA(IR)=EU+DLOG(-H)+DIGAMD(-R/H)
      GOTO 130
C
C         - CASE H SMALL, G=0
C
   90 DO 100 IR=1,NMOM
      R=IR
  100 BETA(IR)=EU+DLOG(R)
      GOTO 130
C
C         - CASE H>0, G=0
C
  110 DO 120 IR=1,NMOM
      R=IR
  120 BETA(IR)=EU+DLOG(H)+DIGAMD(ONE+R/H)
      GOTO 130
C
C         LAMBDA-1
C
  130 CONTINUE
      IF(G.EQ.ZERO)XMOM(1)=U+A*BETA(1)
      IF(G.NE.ZERO)XMOM(1)=U+A*(ONE-BETA(1))/G
      IF(NMOM.EQ.1)RETURN
C
C         LAMBDA-2
C
      ALAM2=BETA(2)-BETA(1)
      IF(G.EQ.ZERO)XMOM(2)=A*ALAM2
      IF(G.NE.ZERO)XMOM(2)=A*ALAM2/(-G)
      IF(NMOM.EQ.2)RETURN
C
C         HIGHER MOMENTS
C
      Z0=ONE
      DO 170 J=3,NMOM
      DJ=J
      Z0=Z0*(FOUR*DJ-SIX)/DJ
      Z=Z0*THREE*(DJ-ONE)/(DJ+ONE)
      SUM=Z0*(BETA(J)-BETA(1))/ALAM2-Z
      IF(J.EQ.3)GOTO 160
      DO 150 I=2,J-2
      DI=I
      Z=Z*(DI+DI+ONE)*(DJ-DI)/((DI+DI-ONE)*(DJ+DI))
      SUM=SUM-Z*XMOM(I+1)
  150 CONTINUE
  160 XMOM(J)=SUM
  170 CONTINUE
      RETURN
C
 1000 WRITE(6,7000)
      RETURN
 1010 WRITE(6,7010)
      RETURN
 1020 WRITE(6,7020)
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE LMRKAP : PARAMETERS INVALID')
 7010 FORMAT(' *** ERROR *** ROUTINE LMRKAP : PARAMETER NMOM TOO LARGE')
 7020 FORMAT(' *** ERROR *** ROUTINE LMRKAP :',
     *  ' CALCULATIONS OF L-MOMENTS HAVE BROKEN DOWN')
      END
