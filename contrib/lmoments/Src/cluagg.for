      SUBROUTINE CLUAGG(METHOD,X,NX,N,NATT,MERGE,DISP,IWORK,WORK,NW)
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
C*  VERSION 3.02  MARCH 1997                                           *
C*  * Implement single-link and complete-link clustering               *
C*                                                                     *
C***********************************************************************
C
C  CLUSTER ANALYSIS BY ANY OF SEVERAL AGGLOMERATIVE HIERARCHICAL METHODS
C
C  PARAMETERS OF ROUTINE:
C  METHOD * INPUT* CLUSTERING METHOD. SHOULD BE SET TO:
C                   1 FOR SINGLE-LINK CLUSTERING
C                   2 FOR COMPLETE-LINK CLUSTERING
C                   3 FOR WARD'S PROCEDURE
C  X      * INPUT* ARRAY OF DIMENSION (NX,NATT).  X(I,J) SHOULD CONTAIN
C                  THE J'TH ATTRIBUTE FOR THE I'TH DATA POINT.
C  NX     * INPUT* THE FIRST DIMENSION OF ARRAY X, AS DECLARED IN THE
C                  CALLING PROGRAM.
C  N      * INPUT* NUMBER OF DATA POINTS
C  NATT   * INPUT* NUMBER OF ATTRIBUTES FOR EACH DATA POINT
C  MERGE  *OUTPUT* ARRAY OF DIMENSION (2,N). MERGE(1,I) AND MERGE(2,I)
C                  ARE THE LABELS OF THE CLUSTERS MERGED AT THE I'TH
C                  STAGE.  MERGE(1,N) AND MERGE(2,N) ARE NOT USED.
C  DISP   *OUTPUT* ARRAY OF LENGTH N.  DISP(I) IS A MEASURE OF THE
C                  WITHIN-CLUSTER DISPERSION AFTER THE I'TH MERGE.
C                  DISPERSION IS DEFINED DIFFERENTLY FOR EACH METHOD:
C                  SEE BELOW.  DISP(N) IS NOT USED.
C  IWORK  * LOCAL* WORK ARRAY OF LENGTH N
C  WORK   * LOCAL* WORK ARRAY OF LENGTH NW
C  NW     * INPUT* LENGTH OF ARRAY WORK. MUST BE AT LEAST N*(N-1)/2.
C
C  Agglomerative hierarchical clustering: general description.
C  Initially there are N clusters, each containing one data point,
C  labeled 1 through N in the same order as the data points.  At each
C  stage of clustering, two clusters are merged.  Their labels are saved
C  in the MERGE array.  The smaller of the two labels is used as the
C  label of the merged cluster.  After the Mth stage of clustering
C  there are N-M clusters.  To find which data points belong to which
C  clusters, use routine CLUINF.
C
C  Single-link clustering: the distance between two clusters A and B is
C  defined to be the minimum of the Euclidean distances between pairs of
C  points with one point in A and one in B.  At each stage, the two
C  clusters separated by the smallest distance are merged.  The square
C  of this distance is saved in the corresponding element of array DISP.
C
C  Complete-link clustering: the distance between two clusters A and B
C  is defined to be the maximum of the Euclidean distances between pairs
C  of points with one point in A and one in B.  At each stage, the two
C  clusters separated by the smallest distance are merged.  The square
C  of this distance is saved in the corresponding element of array DISP.
C  DISP(I) is therefore the largest squared Euclidean distance between
C  two points that are in the same cluster after the Ith merge.
C
C  Ward's procedure: at each stage, the clusters that are merged are
C  chosen to minimize the within-cluster sum of squared deviations of
C  each attribute about the cluster mean.  This sum of squares is saved
C  in the corresponding element of array DISP.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION X(NX,NATT),DISP(N),WORK(NW)
      INTEGER MERGE(2,N),IWORK(N)
      DATA ZERO/0D0/,HALF/0.5D0/
C
C         BIG IS A LARGE NUMBER, USED TO INITIALIZE THE SEARCH CRITERION
C
      DATA BIG/1D72/
C
      NWREQ=N*(N-1)/2
      IF(NW.LT.NWREQ)GOTO 1000
C
C         INITIALLY THERE ARE N CLUSTERS, EACH CONTAINING ONE DATA
C         POINT.  COMPUTE THE COST (INCREASE IN DISPERSION) OF MERGING
C         EACH PAIR OF CLUSTERS.
C
      IW=0
      DO 20 J=2,N
      DO 20 I=1,J-1
      SUM=ZERO
      DO 10 IATT=1,NATT
   10 SUM=SUM+(X(I,IATT)-X(J,IATT))**2
      IW=IW+1
      WORK(IW)=SUM
   20 CONTINUE
      DO 30 I=1,N
   30 IWORK(I)=1
      CCOST=ZERO
C
C         START OF MAIN LOOP
C
      DO 100 IMERGE=1,N-1
C
C         FIND THE PAIR OF CLUSTERS WITH THE LOWEST COST OF MERGING
C
      COST=BIG
      DO 50 J=2,N
      IF(IWORK(J).EQ.0)GOTO 50
      IORIG=(J-1)*(J-2)/2
      DO 40 I=1,J-1
      IF(IWORK(I).EQ.0)GOTO 40
      LIJ=IORIG+I
      IF(WORK(LIJ).GE.COST)GOTO 40
      COST=WORK(LIJ)
      II=I
      JJ=J
   40 CONTINUE
   50 CONTINUE
C
C         MERGE THEM
C
      MERGE(1,IMERGE)=II
      MERGE(2,IMERGE)=JJ
      IF(METHOD.EQ.1.OR.METHOD.EQ.2)DISP(IMERGE)=COST
      IF(METHOD.EQ.3)CCOST=CCOST+COST
      IF(METHOD.EQ.3)DISP(IMERGE)=HALF*CCOST
C
C         COMPUTE THE COST OF MERGING THE NEW CLUSTER WITH EACH OF THE
C         OTHERS
C
      NI=IWORK(II)
      NJ=IWORK(JJ)
      NIJ=NI+NJ
      DO 60 KK=1,N
      NK=IWORK(KK)
      IF(NK.EQ.0)GOTO 60
      IF(KK.EQ.II.OR.KK.EQ.JJ)GOTO 60
      MM=MAX0(II,KK)
      M =MIN0(II,KK)
      IK=(MM-1)*(MM-2)/2+M
      MM=MAX0(JJ,KK)
      M =MIN0(JJ,KK)
      JK=(MM-1)*(MM-2)/2+M
      IF(METHOD.EQ.1)WORK(IK)=DMIN1(WORK(IK),WORK(JK))
      IF(METHOD.EQ.2)WORK(IK)=DMAX1(WORK(IK),WORK(JK))
      IF(METHOD.EQ.3)
     *  WORK(IK)=((NI+NK)*WORK(IK)+(NJ+NK)*WORK(JK)-NK*COST)/(NIJ+NK)
   60 CONTINUE
      IWORK(II)=NIJ
      IWORK(JJ)=0
C
C         END OF MAIN LOOP
C
  100 CONTINUE
C
      RETURN
C
 1000 WRITE(6,7000)NWREQ
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE CLUAGG : INSUFFICIENT WORKSPACE.',
     *  ' LENGTH OF WORK ARRAY SHOULD BE AT LEAST ',I8)
C
      END
