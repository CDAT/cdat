      SUBROUTINE DURAND(SEED,N,X)
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
C  PSEUDO RANDOM NUMBER GENERATOR
C
C  PARAMETERS OF ROUTINE:
C  SEED   *IN/OUT* SEED FOR RANDOM NUMBER GENERATOR. SHOULD BE A WHOLE
C                  NUMBER IN THE RANGE 2D0 TO 2147483647D0.
C  N      * INPUT* NUMBER OF NUMBERS TO BE GENERATED
C  X      *OUTPUT* ARRAY OF LENGTH N. ON EXIT, CONTAINS RANDOM NUMBERS.
C
C  METHOD USED: MULTIPLICATIVE CONGRUENTIAL GENERATOR WITH BASE 2**31-1
C  AND MULTIPLIER 7**5 (P.A.W. LEWIS ET AL., 1969, IBM SYSTEMS JOURNAL)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION X(N)
      DATA AMULT/16807D0/
      DATA BASE,RBASE/2147483647D0,4.65661287524579692D-10/
      DO 10 I=1,N
      SEED=DMOD(SEED*AMULT,BASE)
      X(I)=SEED*RBASE
   10 CONTINUE
      RETURN
      END
