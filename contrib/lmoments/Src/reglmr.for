      SUBROUTINE REGLMR(NSITE,NMOM,NXMOM,XMOM,WEIGHT,RMOM)
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
C  REGIONAL WEIGHTED AVERAGE OF L-MOMENTS
C
C  PARAMETERS OF ROUTINE:
C  NSITE  * INPUT* NUMBER OF SITES IN REGION
C  NMOM   * INPUT* NUMBER OF L-MOMENTS TO BE FOUND.
C  NXMOM  * INPUT* THE FIRST DIMENSION OF ARRAY XMOM, AS DECLARED IN THE
C                  CALLING PROGRAM.
C  XMOM   * INPUT* ARRAY OF DIMENSION (NXMOM,NSITE). X(I,J) CONTAINS
C                  THE I'TH L-MOMENT RATIO FOR SITE J.
C  WEIGHT * INPUT* ARRAY OF LENGTH NSITE. CONTAINS THE WEIGHTS TO BE
C                  APPLIED TO EACH SITE.
C  RMOM   *OUTPUT* ARRAY OF LENGTH NMOM. ON EXIT, CONTAINS THE REGIONAL
C                  WEIGHTED AVERAGE L-MOMENT RATIOS.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION XMOM(NXMOM,NSITE),WEIGHT(NSITE),RMOM(NMOM)
      DATA ZERO/0D0/,ONE/1D0/
      IF(NMOM.LT.2.OR.NMOM.GT.NXMOM)GOTO 1000
      DO 10 J=1,NMOM
   10 RMOM(J)=ZERO
      WSUM=ZERO
      DO 30 ISITE=1,NSITE
      SMEAN=XMOM(1,ISITE)
      IF(SMEAN.EQ.ZERO)GOTO 1010
      W=WEIGHT(ISITE)
      WSUM=WSUM+W
      RMOM(2)=RMOM(2)+W*XMOM(2,ISITE)/SMEAN
      IF(NMOM.EQ.2)GOTO 30
      DO 20 J=3,NMOM
   20 RMOM(J)=RMOM(J)+W*XMOM(J,ISITE)
   30 CONTINUE
      IF(WSUM.LE.ZERO)GOTO 1020
      RMOM(1)=ONE
      RMOM(2)=RMOM(2)/WSUM
      IF(NMOM.EQ.2)RETURN
      DO 40 J=3,NMOM
   40 RMOM(J)=RMOM(J)/WSUM
      RETURN
C
 1000 WRITE(6,7000)
      RETURN
 1010 WRITE(6,7010)ISITE
      RETURN
 1020 WRITE(6,7020)
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE REGLMR : PARAMETER NMOM INVALID')
 7010 FORMAT(' *** ERROR *** ROUTINE REGLMR : ZERO MEAN AT SITE',I4)
 7020 FORMAT(' *** ERROR *** ROUTINE REGLMR :',
     *  ' SUM OF WEIGHTS IS NEGATIVE OR ZERO')
      END
