      PROGRAM XSIM
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
C*  VERSION 3.01  DECEMBER 1996                                        *
C*  * Replaced call to nonexistent function CDFSTN by call to DERF     *
C*                                                                     *
C*  VERSION 3.02  MARCH 1997                                           *
C*  * Changed random number seed                                       *
C*                                                                     *
C*  VERSION 3.03  JUNE 2000                                            *
C*  * Replaced RETURN statements by STOP                               *
C*                                                                     *
C***********************************************************************
C
C  Regional frequency analysis - world specified by its L-moments.
C  Includes calculation of heterogeneity measures.
C
C  Parameters of program:
C  SEED   - Seed for random-number generator.
C  NREP   - Number of simulated regions.
C  NSIM   - Number of simulations used by routine REGTST.  Set it to
C           zero if simulation of heterogeneity and goodness-of-fit
C           measures is not required.  N.B. Total number of simulated
C           regions is NREP*(NSIM+1): large values of NREP and NSIM will
C           need a lot of computing time!
C  NSITE  - Number of sites in region.
C  NMAX   - Maximum record length at any site.
C  RMED   - Correlation between each pair of sites.  At least 0, less
C           than 1.
C  NQ     - Number of quantiles to be estimated (their values are held
C           in array FVAL).
C  NQQ    - Number of empirical quantiles of the distribution of
C           quantile estimates to be found (their values are held in
C           array QUANT).
C  NGROUP - Number of groups in the histogram used to accumulate
C           empirical distribution of quantile estimates.  Include two
C           extra groups for points falling outside the range of the
C           histogram.
C  START  - Lower endpoint for the above histogram.
C  GRINT  - Group interval for the above histogram.
C  KPRINT - Flag for printing simulation results:
C            0 - no printing;
C            1 - print only regional averages;
C            2 - print results for all sites.
C  KOUT   - I/O stream number for printed output.  N.B. If NQ is larger
C           than 16, the output width will be greater than 133 columns;
C           the output stream must be able to accept these long records.
C
C  Arrays whose elements are set in DATA statements:
C  NREC   - Integer array of length NSITE.  Record lengths at each site.
C           No record length should exceed NMAX.
C  CV     - Array of length NSITE.  L-CVs at each site.
C  SKEW   - Array of length NSITE.  L-skewnesses at each site.
C  FVAL   - Array of length NQ.  Quantiles to be estimated.
C  QUANT  - Array of length NQQ.  Empirical quantiles to be estimated.
C           They should be in ascending order.
C
C  Arrays used to store the simulation results:
C  BIAS   - Array of dimension (NQ,NS,2).  BIAS(I,J,1) contains the
C           relative bias of the Ith quantile, and BIAS(I,J,2) the
C           relative bias of the Ith 'growth curve component'
C           (quantile divided by the mean), for site J.
C  RMSE   - Array of dimension (NQ,NS,2).  RMSE(I,J,1) contains the
C           relative RMSE of the Ith quantile, and RMSE(I,J,2) the
C           relative RMSE of the Ith growth curve component, for site J.
C  QEMP   - Array of dimension (NQQ,NQ,NS,2).  QEMP(L,I,J,1) contains
C           the Lth empirical quantile of the distribution of
C           (estimated quantile)/(true quantile) for the Ith quantile
C           at site J; QEMP(L,I,J,2) contains the corresponding quantity
C           for (estimated growth curve component)/(true growth curve
C           component).
C             The empirical quantile may lie outside the range of the
C           histogram used to accumulate the values -- this range being
C           from START to START+GRINT*(NGROUP-2).  In this case the
C           corresponding element of QEMP is unchanged from its initial
C           value, BIG as defined in a DATA statement below.
C              In arrays BIAS, RMSE, and QEMP, the elements for site NS
C           (where NS=NSITE+1) contain averages over all sites of the
C           corresponding array elements.  E.g. BIAS(I,NS,1) is the
C           average of BIAS(I,1,1), BIAS(I,2,1), ..., BIAS(1,NSITE,1).
C  HAVE   - Array of dimension 3.  Contains the average, over all
C           simulated regions, of the three heterogeneity statistics
C           calculated by routine REGTST.
C  ACCEPT - Array of dimension 4.  Contains the proportion of
C           simulations in which each of four distributions
C           (GLO = generalized logistic, GEV = generalized
C           extreme-value, LN3 = lognormal, PE3 = Pearson type III)
C           gave an acceptable fit to the simulated data, according to
C           the goodness-of-fit statistic calculated by routine REGTST.
C  CHOOSE - Array of dimension 4.  Contains the proportion of
C           simulations in which each of the above four distributions
C           gave the best fit to the simulated data.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (SEED=417935084D0,NREP=10000,NSIM=0)
      PARAMETER (NSITE=19,NMAX=100,RMED=0.64D0)
      PARAMETER (NQ=6,NQQ=2,NGROUP=302,START=0.5D0,GRINT=0.005D0)
      PARAMETER (KPRINT=2,KOUT=6)
C
      PARAMETER (NS=NSITE+1)
      CHARACTER*10 HAAB,HBIAS,HRMSE
      CHARACTER*12 NAMES(NSITE)
      INTEGER IHIST(NGROUP,NQ,NS,2),NREC(NSITE)
      DOUBLE PRECISION
     *  AAB(NQ,2),ACCEPT(4),BIAS(NQ,NS,2),CHOOSE(4),COR(NSITE,NSITE),
     *  CV(NSITE),DD(NSITE),FVAL(NQ),H(3),HAVE(3),PARA(5,NSITE),
     *  PPARA(5,6),PROB(1),QEMP(NQQ,NQ,NS,2),QUANT(NQQ),RMOM(5),
     *  RMSE(NQ,NS,2),RPARA(5),RRMOM(5),SKEW(NSITE),SMEAN(NSITE),
     *  SMOM(5,NSITE),TRUEAV(NQ),TRUEGC(NQ),TRUEP(3,NSITE),
     *  TRUEQ(NQ,NSITE),VBAR(3),VOBS(3),VSD(3),X(NMAX,NSITE),XX(NSITE),
     *  YY(NSITE),ZZZ(5)
      DATA HAAB/'  ABS.BIAS'/,HBIAS/'      BIAS'/,HRMSE/'      RMSE'/
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/
      DATA RTHALF/0.70710 67811 86547 524D0/
C
C         ZCRIT - Critical value for Z statistic
C         SMALL - A small number, used to avoid having to evaluate a
C                 quantile function at a value too close to 0 or 1.
C         BIG   - A large number, used to initialize array QEMP and
C                 variable ZMIN.
C
      DATA ZCRIT/1.645D0/,SMALL/1D-10/,BIG/1D10/
C
      DATA FVAL/0.01D0,0.1D0,0.5D0,0.9D0,0.99D0,0.999D0/
      DATA QUANT/0.05D0,0.95D0/
      DATA (NREC(ISITE),CV(ISITE),SKEW(ISITE),ISITE=1,NSITE)/
     *  98,  0.0978D0, 0.0279D0,
     *  59,  0.0992D0, 0.0279D0,
     *  90,  0.1006D0, 0.0279D0,
     *  61,  0.1020D0, 0.0279D0,
     *  65,  0.1034D0, 0.0279D0,
     *  86,  0.1047D0, 0.0279D0,
     *  78,  0.1061D0, 0.0279D0,
     *  72,  0.1075D0, 0.0279D0,
     *  67,  0.1089D0, 0.0279D0,
     *  99,  0.1103D0, 0.0279D0,
     *  49,  0.1117D0, 0.0279D0,
     *  61,  0.1131D0, 0.0279D0,
     *  69,  0.1145D0, 0.0279D0,
     *  73,  0.1159D0, 0.0279D0,
     *  70,  0.1172D0, 0.0279D0,
     *  66,  0.1186D0, 0.0279D0,
     *  59,  0.1200D0, 0.0279D0,
     *  74,  0.1214D0, 0.0279D0,
     *  82,  0.1228D0, 0.0279D0/
C
C         CALCULATE POPULATION PARAMETERS FOR EACH SITE
C
      NNMAX=0
      DO 10 ISITE=1,NSITE
      RMOM(1)=ONE
      RMOM(2)=CV(ISITE)
      RMOM(3)=SKEW(ISITE)
      CALL PELGNO(RMOM,TRUEP(1,ISITE))
      IF(NREC(ISITE).GT.NNMAX)NNMAX=NREC(ISITE)
   10 CONTINUE
      IF(NNMAX.GT.NMAX)GOTO 1000
C
C         CALCULATE POPULATION QUANTILES FOR EACH SITE
C
      DO 30 IQ=1,NQ
      SUMAV=ZERO
      SUMGC=ZERO
      DO 20 ISITE=1,NSITE
      Q=QUAGNO(FVAL(IQ),TRUEP(1,ISITE))
      TRUEQ(IQ,ISITE)=Q
      SUMAV=SUMAV+Q
      SUMGC=SUMGC+ONE/Q
   20 CONTINUE
      TRUEAV(IQ)=SUMAV/NSITE
      TRUEGC(IQ)=NSITE/SUMGC
   30 CONTINUE
C
C         PRINT DESCRIPTION OF WORLD
C
      IF(KPRINT.EQ.0)GOTO 80
      IF(NQ.GE.13)GOTO 50
      WRITE(KOUT,6000)NREP,SEED,(FVAL(IQ),IQ=1,NQ)
      DO 40 ISITE=1,NSITE
      WRITE(KOUT,6010)ISITE,(TRUEP(I,ISITE),I=1,3),
     *  CV(ISITE),SKEW(ISITE),NREC(ISITE),(TRUEQ(IQ,ISITE),IQ=1,NQ)
   40 CONTINUE
      WRITE(KOUT,6020)(TRUEAV(IQ),IQ=1,NQ)
      WRITE(KOUT,6030)(TRUEGC(IQ),IQ=1,NQ)
      GOTO 80
   50 CONTINUE
      WRITE(KOUT,6000)NREP,SEED
      DO 60 ISITE=1,NSITE
      WRITE(KOUT,6010)ISITE,(TRUEP(I,ISITE),I=1,3),
     *  CV(ISITE),SKEW(ISITE),NREC(ISITE)
   60 CONTINUE
      WRITE(KOUT,6040)(FVAL(IQ),IQ=1,NQ)
      DO 70 ISITE=1,NSITE
      WRITE(KOUT,6050)ISITE,(TRUEQ(IQ,ISITE),IQ=1,NQ)
   70 CONTINUE
      WRITE(KOUT,6060)(TRUEAV(IQ),IQ=1,NQ)
      WRITE(KOUT,6070)(TRUEGC(IQ),IQ=1,NQ)
   80 CONTINUE
C
C         INITIALIZE CORRELATION MATRIX
C
      IF(KPRINT.GT.0)WRITE(KOUT,6080)RMED
      IF(RMED.EQ.ZERO.OR.NSITE.EQ.1)GOTO 140
      COR(1,1)=ONE
      DO 100 I=2,NSITE
      DO 90 J=1,I-1
   90 COR(I,J)=RMED
  100 COR(I,I)=ONE
C
C         CHOLESKY DECOMPOSITION OF CORRELATION MATRIX
C         FIRST COLUMN IS ALREADY CORRECT, BECAUSE COR(1,1)=1,
C         SO TRANSFORM ONLY THE SECOND AND SUBSEQUENT COLUMNS
C
      DO 130 I=2,NSITE
C
C         - DIAGONAL ELEMENTS
C
      SUM=ZERO
      DO 110 K=1,I-1
  110 SUM=SUM+COR(I,K)**2
      SUM=COR(I,I)-SUM
      COR(I,I)=DSQRT(SUM)
      IF(I.EQ.NSITE)GOTO 140
C
C         - OFF-DIAGONAL ELEMENTS
C
      DO 130 J=I+1,NSITE
      SUM=ZERO
      DO 120 K=1,I-1
  120 SUM=SUM+COR(I,K)*COR(J,K)
      COR(J,I)=(COR(J,I)-SUM)/COR(I,I)
  130 CONTINUE
C
  140 CONTINUE
C
C         INITIALIZE ARRAYS
C
      DO 150 IH=1,3
  150 HAVE(IH)=ZERO
      DO 160 IZ=1,4
      ACCEPT(IZ)=ZERO
  160 CHOOSE(IZ)=ZERO
      DO 190 IQ=1,NQ
      DO 190 IS=1,NS
      BIAS(IQ,IS,1)=ZERO
      BIAS(IQ,IS,2)=ZERO
      RMSE(IQ,IS,1)=ZERO
      RMSE(IQ,IS,2)=ZERO
      DO 170 IG=1,NGROUP
      IHIST(IG,IQ,IS,1)=0
  170 IHIST(IG,IQ,IS,2)=0
      DO 180 IQQ=1,NQQ
      QEMP(IQQ,IQ,IS,1)=BIG
  180 QEMP(IQQ,IQ,IS,2)=BIG
  190 CONTINUE
      NFAIL=0
      NWARN=0
      SSEED=SEED
C
C         START OF SIMULATION LOOP
C
      DO 400 IREP=1,NREP
C
      XSEED=SSEED
C
C         GENERATE INDEPENDENT UNIFORM RANDOM VARIATES
C
      DO 200 ISITE=1,NSITE
      CALL DURAND(SSEED,NNMAX,X(1,ISITE))
  200 CONTINUE
C
C         FOR CORRELATED DATA: TRANSFORM TO NORMAL, FORM CORRELATED
C         NORMAL VARIATES, TRANSFORM BACK TO UNIFORM
C
      IF(RMED.EQ.ZERO.OR.NSITE.EQ.1)GOTO 260
      DO 250 IX=1,NNMAX
      DO 210 ISITE=1,NSITE
      X(IX,ISITE)=QUASTN(X(IX,ISITE))
  210 CONTINUE
      DO 240 I=NSITE,1,-1
      SUM=COR(I,I)*X(IX,I)
      IF(I.EQ.1)GOTO 230
      DO 220 J=1,I-1
  220 SUM=SUM+COR(I,J)*X(IX,J)
  230 CONTINUE
      TEMP=HALF+HALF*DERF(SUM*RTHALF)
      IF(TEMP.EQ.ONE)TEMP=ONE-SMALL
      IF(TEMP.EQ.ZERO)TEMP=SMALL
      X(IX,I)=TEMP
  240 CONTINUE
  250 CONTINUE
  260 CONTINUE
C
C         TRANSFORM TO CORRECT MARGINAL DISTRIBUTION,
C         DIVIDE BY SAMPLE MEAN AND ORDER THE DATA
C
      DO 290 ISITE=1,NSITE
      N=NREC(ISITE)
      SUM=ZERO
      DO 270 I=1,N
      X(I,ISITE)=QUAGNO(X(I,ISITE),TRUEP(1,ISITE))
  270 SUM=SUM+X(I,ISITE)
      SMEAN(ISITE)=SUM/N
      DO 280 I=1,N
  280 X(I,ISITE)=X(I,ISITE)/SMEAN(ISITE)
      CALL SORT(X(1,ISITE),N)
  290 CONTINUE
C
C         CALCULATE L-MOMENT RATIOS
C
      DO 300 I=1,5
  300 RMOM(I)=ZERO
      NSY=0
      DO 310 ISITE=1,NSITE
      N=NREC(ISITE)
      NSY=NSY+N
      CALL SAMLMR(X(1,ISITE),N,SMOM(1,ISITE),5,ZERO,ZERO)
      DO 310 I=1,5
  310 RMOM(I)=RMOM(I)+N*SMOM(I,ISITE)
      DO 320 I=1,5
  320 RMOM(I)=RMOM(I)/NSY
C
C         CALCULATE HETEROGENEITY STATISTICS
C
      IF(NSIM.EQ.0)GOTO 350
      RSEED=XSEED
      NPROB=0
      JPRINT=0
      CALL REGTST(NSITE,NAMES,NREC,SMOM,ZERO,ZERO,RSEED,NSIM,NPROB,
     *   PROB,JPRINT,KOUT,RRMOM,DD,VOBS,VBAR,VSD,H,ZZZ,PPARA)
      DO 330 IH=1,3
  330 HAVE(IH)=HAVE(IH)+H(IH)
      ZMIN=BIG
      DO 340 IZ=1,4
      ZABS=DABS(ZZZ(IZ))
      IF(ZABS.LE.ZCRIT)ACCEPT(IZ)=ACCEPT(IZ)+ONE
      IF(ZABS.GE.ZMIN)GOTO 340
      ICHOOS=IZ
      ZMIN=ZABS
  340 CONTINUE
      CHOOSE(ICHOOS)=CHOOSE(ICHOOS)+1
  350 CONTINUE
C
C         ESTIMATE PARAMETERS
C
      IFAIL=0
      CALL PELGNO(RMOM,RPARA)
      IF(RPARA(2).LT.ZERO)IFAIL=2
      IF(IFAIL.EQ.0)GOTO 360
      NFAIL=NFAIL+1
      GOTO 390
  360 CONTINUE
C
C         COMPUTE QUANTILES ...
C
      DO 380 IQ=1,NQ
      GC=QUAGNO(FVAL(IQ),RPARA)
C
C         ... AND ACCUMULATE HISTOGRAMS, OF EMPIRICAL DISTRIBUTIONS
C             OF QUANTILE ESTIMATES ...
C
      QSUM=ZERO
      DO 370 ISITE=1,NSITE
      Q=GC*SMEAN(ISITE)/TRUEQ(IQ,ISITE)
      QSUM=QSUM+Q
      BIAS(IQ,ISITE,1)=BIAS(IQ,ISITE,1)+(Q-ONE)
      RMSE(IQ,ISITE,1)=RMSE(IQ,ISITE,1)+(Q-ONE)**2
      IG=(Q-START)/GRINT+2
      IF(IG.LT.1)IG=1
      IF(IG.GT.NGROUP)IG=NGROUP
      IHIST(IG,IQ,ISITE,1)=IHIST(IG,IQ,ISITE,1)+1
      Q=GC/TRUEQ(IQ,ISITE)
      BIAS(IQ,ISITE,2)=BIAS(IQ,ISITE,2)+(Q-ONE)
      RMSE(IQ,ISITE,2)=RMSE(IQ,ISITE,2)+(Q-ONE)**2
      IG=(Q-START)/GRINT+2
      IF(IG.LT.1)IG=1
      IF(IG.GT.NGROUP)IG=NGROUP
      IHIST(IG,IQ,ISITE,2)=IHIST(IG,IQ,ISITE,2)+1
  370 CONTINUE
C
C         ... OF THE 'AVERAGE FOR ALL SITES'
C
      Q=QSUM/NSITE
      IG=(Q-START)/GRINT+2
      IF(IG.LT.1)IG=1
      IF(IG.GT.NGROUP)IG=NGROUP
      IHIST(IG,IQ,NS,1)=IHIST(IG,IQ,NS,1)+1
C
C         ... AND OF THE 'GROWTH CURVE COMPONENT'
C
      Q=GC/TRUEGC(IQ)
      IG=(Q-START)/GRINT+2
      IF(IG.LT.1)IG=1
      IF(IG.GT.NGROUP)IG=NGROUP
      IHIST(IG,IQ,NS,2)=IHIST(IG,IQ,NS,2)+1
  380 CONTINUE
  390 CONTINUE
C
C         END OF SIMULATION LOOP
C
  400 CONTINUE
C
C         CALCULATE FINAL RESULTS FOR HET AND GOF STATISTICS
C
      IF(NSIM.EQ.0)GOTO 430
      DO 410 IH=1,3
  410 HAVE(IH)=HAVE(IH)/NREP
      IF(KPRINT.GT.0)WRITE(KOUT,6090)HAVE,NSIM
      DO 420 IZ=1,4
      ACCEPT(IZ)=ACCEPT(IZ)/NREP
  420 CHOOSE(IZ)=CHOOSE(IZ)/NREP
      IF(KPRINT.GT.0)WRITE(KOUT,6100)(ACCEPT(IZ),IZ=1,4)
      IF(KPRINT.GT.0)WRITE(KOUT,6110)(CHOOSE(IZ),IZ=1,4)
  430 CONTINUE
C
C         CALCULATE FINAL RESULTS FOR ESTIMATION
C
      IF(NREP.LE.1)STOP
      NOK=NREP-NFAIL
      DO 510 IQ=1,NQ
      DO 510 ITYPE=1,2
C
C         -  BIAS AND RMSE, SITE BY SITE ...
C
      DO 440 ISITE=1,NSITE
      BIAS(IQ,ISITE,ITYPE)=BIAS(IQ,ISITE,ITYPE)/NOK
      RMSE(IQ,ISITE,ITYPE)=DSQRT(RMSE(IQ,ISITE,ITYPE)/NOK)
  440 CONTINUE
C
C         - ... AND AVERAGED OVER ALL SITES
C
      BIAS(IQ,NS,ITYPE)=ZERO
      RMSE(IQ,NS,ITYPE)=ZERO
      AAB(IQ,ITYPE)=ZERO
      DO 450 ISITE=1,NSITE
      BIAS(IQ,NS,ITYPE)=BIAS(IQ,NS,ITYPE)+BIAS(IQ,ISITE,ITYPE)
      RMSE(IQ,NS,ITYPE)=RMSE(IQ,NS,ITYPE)+RMSE(IQ,ISITE,ITYPE)
      AAB(IQ,ITYPE)=AAB(IQ,ITYPE)+DABS(BIAS(IQ,ISITE,ITYPE))
  450 CONTINUE
      BIAS(IQ,NS,ITYPE)=BIAS(IQ,NS,ITYPE)/NSITE
      RMSE(IQ,NS,ITYPE)=RMSE(IQ,NS,ITYPE)/NSITE
      AAB(IQ,ITYPE)=AAB(IQ,ITYPE)/NSITE
C
C        - QUANTILES OF EMPIRICAL DISTRIBUTIONS OF ESTIMATORS
C
      DO 500 ISITE=1,NS
      IG=0
      SUM=ZERO
      DO 480 IQQ=1,NQQ
      CRIT=QUANT(IQQ)*NOK
      DO 460 IGG=1,NGROUP
      IF(SUM.GE.CRIT)GOTO 470
      IG=IG+1
      HH=IHIST(IG,IQ,ISITE,ITYPE)
      SUM=SUM+HH
  460 CONTINUE
  470 IF(IG.LE.1)GOTO 480
      IF(IG.GE.NGROUP)GOTO 490
      QEMP(IQQ,IQ,ISITE,ITYPE)=START+GRINT*(IG-ONE-(SUM-CRIT)/HH)
  480 CONTINUE
  490 CONTINUE
  500 CONTINUE
C
  510 CONTINUE
C
C         PRINT RESULTS
C
      IF(KPRINT.LE.0)GOTO 570
C
C         - HEADER LINES
C
      WRITE(KOUT,6120)NFAIL,NWARN
      WRITE(KOUT,6130)(FVAL(IQ),IQ=1,NQ)
C
C         - SITE BY SITE
C
      IF(KPRINT.EQ.1)GOTO 540
      DO 530 ISITE=1,NSITE
      WRITE(KOUT,6140)ISITE
      WRITE(KOUT,6170)HBIAS,(BIAS(IQ,ISITE,1),IQ=1,NQ)
      WRITE(KOUT,6170)HRMSE,(RMSE(IQ,ISITE,1),IQ=1,NQ)
      DO 520 IQQ=1,NQQ
      WRITE(KOUT,6180)QUANT(IQQ),(QEMP(IQQ,IQ,ISITE,1),IQ=1,NQ)
  520 CONTINUE
  530 CONTINUE
  540 CONTINUE
C
C         - 'AVERAGE FOR ALL SITES' AND 'GROWTH CURVE COMPONENT'
C
      WRITE(KOUT,6150)
      WRITE(KOUT,6170)HAAB,(AAB(IQ,1),IQ=1,NQ)
      WRITE(KOUT,6170)HBIAS,(BIAS(IQ,NS,1),IQ=1,NQ)
      WRITE(KOUT,6170)HRMSE,(RMSE(IQ,NS,1),IQ=1,NQ)
      DO 550 IQQ=1,NQQ
  550 WRITE(KOUT,6180)QUANT(IQQ),(QEMP(IQQ,IQ,NS,1),IQ=1,NQ)
      WRITE(KOUT,6160)
      WRITE(KOUT,6170)HAAB,(AAB(IQ,2),IQ=1,NQ)
      WRITE(KOUT,6170)HBIAS,(BIAS(IQ,NS,2),IQ=1,NQ)
      WRITE(KOUT,6170)HRMSE,(RMSE(IQ,NS,2),IQ=1,NQ)
      DO 560 IQQ=1,NQQ
  560 WRITE(KOUT,6180)QUANT(IQQ),(QEMP(IQQ,IQ,NS,2),IQ=1,NQ)
  570 CONTINUE
C-----------------------------------------------------------------------
C  NOTE -- The section of the program between the dashed lines is
C  included for interest only, and is not an integral part of the
C  simulation.  It shows how Table 6.2 of Hosking and Wallis (1997) is
C  derived from the simulation results for the 'growth curve component'.
C
C         SET REGIONAL L-MOMENTS FOR 'NORTH CASCADES' REGION
C         AND ESTIMATE PARAMETERS OF REGIONAL LOGNORMAL DISTRIBUTION
C
      RMOM(1)=ONE
      RMOM(2)=0.1103D0
      RMOM(3)=0.0279D0
      CALL PELGNO(RMOM,RPARA)
C
C         COMPUTE AND PRINT THE ENTRIES FOR THE TABLE
C
      WRITE(KOUT,9010)
      DO 900 IQ=1,NQ
      Q=QUAGNO(FVAL(IQ),RPARA)
      RMS=RMSE(IQ,NS,2)
      ERRLOW=Q/QEMP(2,IQ,NS,2)
      ERRHI =Q/QEMP(1,IQ,NS,2)
      WRITE(KOUT,9020)FVAL(IQ),Q,RMS,ERRLOW,ERRHI
  900 CONTINUE
 9010 FORMAT(//' Hosking and Wallis (1997), Table 6.2'//
     *  '        F      qhat(F)     RMSE      Error bounds')
 9020 FORMAT(1X,5F10.3)
C-----------------------------------------------------------------------
C
C         THE END
C
      STOP
C
 1000 WRITE(6,7000)
      STOP
C
 6000 FORMAT(' REGIONAL LOGNORMAL SIMULATIONS    NREP=',I6,
     *  '  SEED=',F12.0////' SITE     XI   ALPHA       K',
     *  '    L-CV  L-SKEW   N',:,'    QUANTILES .....'/49X,12F8.4)
 6010 FORMAT(1X,I3,5F8.4,I4,12F8.3)
 6020 FORMAT(9X,'AVERAGE FOR ALL SITES (ARITHMETIC MEAN)',12F8.3)
 6030 FORMAT(9X,'REGIONAL GROWTH CURVE (HARMONIC MEAN)  ',12F8.3)
 6040 FORMAT(/' SITE   QUANTILES .....'/4X,16F8.4)
 6050 FORMAT(1X,I3,16F8.3)
 6060 FORMAT(' AVERAGE FOR ALL SITES (ARITHMETIC MEAN)'/4X,16F8.3)
 6070 FORMAT(' REGIONAL GROWTH CURVE (HARMONIC MEAN)  '/4X,16F8.3)
 6080 FORMAT(//' INTER-SITE CORRELATION=',F6.2)
 6090 FORMAT(//' AVERAGE HETEROGENEITY MEASURES',3F8.2,
     *  '   (BASED ON',I6,' SIMULATIONS)')
 6100 FORMAT(' DISTRIBUTIONS ACCEPTED:',
     *  ' GLO',F7.4,', GEV',F7.4,', LN3',F7.4,', PE3',F7.4)
 6110 FORMAT(' DISTRIBUTIONS CHOSEN  :',
     *  ' GLO',F7.4,', GEV',F7.4,', LN3',F7.4,', PE3',F7.4)
 6120 FORMAT(//' L-MOMENT ESTIMATION OF LOGNORMAL DISTRIBUTION ',10X,
     *  'FAILURES:',I6,8X,'WARNINGS:',I6)
 6130 FORMAT(/9X,'F',5X,16F7.4)
 6140 FORMAT(/22X,'SITE',I3,' QUANTILES')
 6150 FORMAT(/20X,'AVERAGE FOR ALL SITES')
 6160 FORMAT(/20X,'GROWTH CURVE COMPONENT')
 6170 FORMAT(A10,5X,16F7.3)
 6180 FORMAT(1X,F5.3,' PT.',5X,16F7.3)
 7000 FORMAT('*** ERROR *** PROGRAM XSIM   : NMAX PARAMETER TOO SMALL')
      END
