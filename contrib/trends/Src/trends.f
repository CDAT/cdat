

c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
c     SUBROUTINE REG_AR1.
c
c     Date:    27-MAR-2000.
c     Author:  B.D. Santer, PCMDI, Livermore.
c     Version: Single Precision.
c
c     Performs a simple linear regression with dependent variable y and
c     independent variable x. Various other statistical quantities are
c     also computed. Also computes lag-1 autocorrelation of residuals from
c     linear regression, and uses this to adjust effective temporal degrees
c     of freedom for confidence interval about estimated regression slope.
c
c     REFERENCE:
c
c     NAGFLIB:801/715:Mk5:Nov74. 
c     Routine G02CAF. 
c     
c     EXTERNALS:
c
c     function BETAI   - Computes incomplete Beta function.
c     function BETACF  - Computes continued fraction for BETAI. 
c     function GAMMLN  - Computes Gamma function
c     function PROBF1  - Computes probability for given F-value.
c     function PROBND1 - Computes area under a normal curve.
c     subroutine AUTO1 - Computes autocovariance and autocorrelation.
c
c
      subroutine reg_ar1(nmax, n1, lag, rneff, x, y, 
     &result, res, cxx, rxx)
c     
c
c
      implicit real (o-z, a-h)
c
c     ** INPUT **
c     integer nmax          Max. length of x and y 
c     integer n1            Actual length of x and y 
c     integer lag           Max. lag for autocorrelations
c     dimension x(nmax)     Independent variable
c     dimension y(nmax)     Dependent variable
c
c     ** OUTPUT **
c     real rneff               Effective sample size
c     dimension result(31)     Array of linear regression results
c     dimension res(nmax)      Residuals from linear regression
c     dimension cxx(0 : lag)   Autocovariance function        
c     dimension rxx(0 : lag)   Autocorrelation function        
c
      integer nmax, n1, lag
      real rneff
      dimension x(nmax), y(nmax), result(31), res(nmax)
      dimension cxx(0 : lag), rxx(0 : lag)
c
c
c
c-------------------------------------------------------------------------------
c
c
c
      eps1  = 1.0e-8
      bogus = -99.999
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.0 **
c
c     Initialize sums.
c-------------------------------------------------------------------------------
c
c
c
      sumx  = 0.0
      sumy  = 0.0
      varx  = 0.0
      vary  = 0.0
      xy    = 0.0
      ssd   = 0.0
c
      do 10 i = 1, n1
        sumx  = sumx + x(i)
        sumy  = sumy + y(i)
   10 continue
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.1 **
c
c     Compute means.
c-------------------------------------------------------------------------------
c
c
c
      dn   = float(n1)
      xbar = sumx / dn 
      ybar = sumy / dn 
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.2 **
c
c     Compute standard deviations (sdx, sdy).
c-------------------------------------------------------------------------------
c
c
c
      do 12 i = 1, n1
       x1   = x(i) - xbar 
       y1   = y(i) - ybar 
       xy   = xy + (x1 * y1)
       varx = varx + (x1 * x1)
       vary = vary + (y1 * y1)
   12 continue
c
      if (varx .le. eps1) then
        print *, 'In REG_AR1. Small or zero variance detected in x!'
        print *, 'varx = ', varx
        do 121 kk = 1, 31
          result(kk) = bogus
  121   continue
        goto 1000
      else
        sdx  = sqrt(varx / (dn - 1.0))
      end if
c
      if (vary .eq. 0.0) then
        print *, 'In REG_AR1. Small or zero variance detected in y!'
        print *, 'vary = ', vary
        do 122 kk = 1, 31
          result(kk) = bogus
  122   continue
        goto 1000
      else
        sdy  = sqrt(vary / (dn - 1.0))
      end if
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.3 **
c
c     Compute Pearson product-moment correlation coefficient (r).
c-------------------------------------------------------------------------------
c
c
c
      r = xy / sqrt(varx * vary)
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.4 **
c
c     Compute regression coefficient (b) and regression constant (a).
c-------------------------------------------------------------------------------
c
c
c
      b    = xy / varx  
      a    = ybar - b * xbar
c
c
c 
c-------------------------------------------------------------------------------
c     ** 1.5 **
c
c     Compute:
c     ssr       Sum of squares attributable to the regression.
c     ssd       Sum of squares of deviations about the regression.
c     sst       Total sum of squares.
c     res       Residuals from regression.
c     rxx       Autocorrelation function. 
c     rneff     Effective sample size.
c-------------------------------------------------------------------------------
c
c
c
      sst = vary
      do 15 i = 1, n1
        x2     = y(i) - a - b * x(i)
        ssd    = ssd + (x2 * x2)
        res(i) = x2
   15 continue
c
      ssr = sst - ssd
c
      call auto1(nmax, n1, lag, res, cxx, rxx)
c
      term1 = 1.0 - rxx(1)
      term2 = 1.0 + rxx(1)
      term3 = n1 * 1.0     
      rneff = term3 * (term1 / term2)
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.6 **
c
c     Compute: 
c     idfr      D.O.F. attributable to the regression.
c     idfd1     D.O.F. of deviations about the regression (unadjusted).
c     rdfd2     D.O.F. of deviations about the regression (adjusted).
c     idft      Total degrees of freedom.
c-------------------------------------------------------------------------------
c
c
c
      idft  = n1 - 1
      idfd1 = n1 - 2
      rdfd2 = rneff - 2.0
      idfr  = 1
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.7 **
c
c     Compute: 
c     amsr      Mean square attributable to the regression.
c     amsd1     Mean square of deviations about the regression (unadjusted).
c     amsd2     Mean square of deviations about the regression (adjusted).
c-------------------------------------------------------------------------------
c
c
c
      amsr  = ssr / float(idfr)
      amsd1 = ssd / float(idfd1)
      amsd2 = ssd / rdfd2
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.8 **
c
c     Compute the F-value for the analysis of variance (f). See also D.S.  
c     Wilks (Statistical Methods in the Atmospheric Sciences, Academic
c     Press, 1995, pp. 166-168).
c-------------------------------------------------------------------------------
c
c
c
      f = amsr / amsd1
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.9 **
c
c     Compute: 
c     seb1      Standard error of regression coefficient b (unadjusted).
c     seb2      Standard error of regression coefficient b (adjusted).
c     sea1      Standard error of regression constant a (unadjusted).
c     sea2      Standard error of regression constant a (adjusted).
c-------------------------------------------------------------------------------
c
c
c
      seb1 = sqrt(amsd1 / varx)
      seb2 = sqrt(amsd2 / varx)
      s1   = (1.0 / dn) + ((xbar * xbar) / varx)
      sea1 = sqrt(amsd1 * s1)
      sea2 = sqrt(amsd2 * s1)
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.10 **
c
c     Compute: 
c     tb1       t-value for regression coefficient b (unadjusted).
c     tb2       t-value for regression coefficient b (adjusted).
c     ta1       t-value for regression constant a (unadjusted).
c     ta2       t-value for regression constant a (adjusted).
c-------------------------------------------------------------------------------
c
c
c
      tb1 = b / seb1
      tb2 = b / seb2
      ta1 = a / sea1
      ta2 = a / sea2
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.11 **
c
c     Compute: 
c     pt1       p-value for regression coefficient t-value. Effective sample
c               size adjustment for standard error (seb).
c     pt2       p-value for regression coefficient t-value. Effective sample
c               size adjustment for standard error (seb) and critical t-value.
c     pt3       p-value for regression coefficient t-value. No adjustment for
c               standard error or critical t-value.
c     pf1       p-value for regression coefficient F-value (one-tailed).
c     pf2       p-value for regression coefficient F-value (two-tailed).
c
c     The values pt1 and pt2 are use to test the null hypothesis that b = 0
c     (i.e., y is independent of x). 
c
c     The values pf1 and pf2 are use to test the null hypothesis that the
c     regression is linear (goodness of linear fit). For non-replicated
c     values of y, the degrees of freedom are 1 and n - 2.
c-------------------------------------------------------------------------------
c
c
c
      ii1 = 1
      xx1 = n1 / (n1 + (tb2 * tb2))
      xx2 = rneff / (rneff + (tb2 * tb2))
      xx3 = n1 / (n1 + (tb1 * tb1))
      aa1 = n1 / 2.0
      aa2 = rneff / 2.0
      bb  = 1.0 / 2.0
c
      pt1 = betai(aa1, bb, xx1)
      pt2 = betai(aa2, bb, xx2)
      pt3 = betai(aa1, bb, xx3)
      pf1 = probf1(f, dum3, ii1, n1 - 2, 1)
      pf2 = probf1(f, dum4, ii1, n1 - 2, 2)
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.12 **
c
c     Store results (result). 
c-------------------------------------------------------------------------------
c
c
c
      result(1)  = xbar
      result(2)  = ybar
      result(3)  = sdx 
      result(4)  = sdy 
      result(5)  = r 
      result(6)  = b 
      result(7)  = a 
      result(8)  = seb1
      result(9)  = seb2
      result(10) = sea1
      result(11) = sea2
      result(12) = tb1
      result(13) = tb2
      result(14) = ta1
      result(15) = ta2
      result(16) = ssr 
      result(17) = idfr * 1.0
      result(18) = amsr 
      result(19) = f 
      result(20) = ssd 
      result(21) = idfd1 * 1.0
      result(22) = rdfd2
      result(23) = amsd1
      result(24) = amsd2
      result(25) = sst 
      result(26) = idft * 1.0
      result(27) = pt1 
      result(28) = pt2 
      result(29) = pt3 
      result(30) = pf1 
      result(31) = pf2 
c
 1000 continue
      return
      end
c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
      FUNCTION betai(a,b,x)
      REAL betai,a,b,x
CU    USES betacf,gammln
      REAL bt,betacf,gammln
      if(x.lt.0..or.x.gt.1.)pause 'bad argument x in betai'
      if(x.eq.0..or.x.eq.1.)then
        bt=0.
      else
        bt=exp(gammln(a+b)-gammln(a)-gammln(b)+a*log(x)+b*log(1.-x))
      endif
      if(x.lt.(a+1.)/(a+b+2.))then
        betai=bt*betacf(a,b,x)/a
        return
      else
        betai=1.-bt*betacf(b,a,1.-x)/b
        return
      endif
      END
c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
      FUNCTION betacf(a,b,x)
      INTEGER MAXIT
      REAL betacf,a,b,x,EPS,FPMIN
      PARAMETER (MAXIT=100,EPS=3.e-7,FPMIN=1.e-30)
      INTEGER m,m2
      REAL aa,c,d,del,h,qab,qam,qap
      qab=a+b
      qap=a+1.
      qam=a-1.
      c=1.
      d=1.-qab*x/qap
      if(abs(d).lt.FPMIN)d=FPMIN
      d=1./d
      h=d
      do 11 m=1,MAXIT
        m2=2*m
        aa=m*(b-m)*x/((qam+m2)*(a+m2))
        d=1.+aa*d
        if(abs(d).lt.FPMIN)d=FPMIN
        c=1.+aa/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        h=h*d*c
        aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
        d=1.+aa*d
        if(abs(d).lt.FPMIN)d=FPMIN
        c=1.+aa/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        del=d*c
        h=h*del
        if(abs(del-1.).lt.EPS)goto 1
11    continue
      pause 'a or b too big, or MAXIT too small in betacf'
1     betacf=h
      return
      END
c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
      FUNCTION gammln(xx)
      REAL gammln,xx
      INTEGER j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+log(stp*ser/x)
      return
      END
c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
c     FUNCTION PROBF1.
c
c     Date:    6-MAR-1992.
c     Author:  B.D. Santer, MPI, Hamburg.
c     Version: Single Precision.
c
c     The output is either the one- or two-tailed test area: i.e., the
c     area under an F-curve (with N1 and N2 degrees of freedom) to the
c     right of X if X exceeds 1.0 (one-tailed test) or twice this area
c     (two-tailed test).
c
c     Note: if X is less than 1.0, this function gives the area to the
c     right of 1/X with reversed order for the degrees of freedom. This
c     ensures the accuracy of the numerical algorithm.
c
c     REFERENCE:
c
c     M. Abramowitz and I.A. Stegun.
c     Handbook of Mathematical Functions. 
c     Dover, 1970, page 947 (26.6.15). 
c
c     EXTERNALS:
c
c     function PROBND1 - Calculates the area under a normal curve.
c
c
c
      function probf1(y, x, n1, n2, id)
c
c
c
      implicit real (o-z, a-h)
c
c     ** INPUT **
c     real y            Calculated F-value (unchanged on output)
c     real x            Inverse of Y if Y is less than 1.0
c     integer n1, n2    Degrees of freedom
c     integer id        Identifier for one- or two-tailed test
c
c     ** OUTPUT **
c     real probf1       Significance level (p-value) for F-value
c
c
c
      integer n1, n2, id
      real y, x, probf1
c
c
c
c-------------------------------------------------------------------------------
c
c
c
      x = y
      if (x .gt. 1.0) goto 1
      x  = 1.0 / x
      n  = n1
      n1 = n2
      n2 = n
c
    1 continue
      dn1 = float(n1)
      dn2 = float(n2)
      term1 = 2.0 / (9.0 * dn1)
      term2 = 2.0 / (9.0 * dn2)
      term3 = ((x ** (1.0 / 3.0)) * (1.0 - term2)) - (1.0 - term1)
      term4 = sqrt(term1 + ((x ** (2.0 / 3.0)) * term2))
      term5 = term3 / term4
      if (id .eq. 2) probf1 = 2.0 * probnd1(term5)
c
c
c
c-------------------------------------------------------------------------------
c     The numerical algorithm can have problems when the F-value is
c     close to 1.0 and the degrees of freedom are small. Therefore,
c     insure that the probabilities returned cannot exceed 1.0.
c-------------------------------------------------------------------------------
c
c
c
      if (id .eq. 1) probf1 = probnd1(term5)
      if (probf1 .gt. 1.0) probf1 = 1.0
      return
      end
c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
c     FUNCTION PROBND1.
c
c     Date:    6-MAR-1992.
c     Author:  B.D. Santer, MPI, Hamburg.
c     Version: Single Precision.
c
c     Calculates the area under a normal curve (mean=0.0, variance=1.0)
c     to the right of x. The accuracy is better than 7.5 * 10.**-8.
c
c     REFERENCE:
c
c     M. Abramowitz and I.A. Stegun.
c     Handbook of Mathematical Functions.
c     Dover, 1970, pages 931-932 (26.2.1 and 26.2.17).
c
c
c
      function probnd1(x)
c
c
c
      implicit real (o-z, a-h)
c
c     ** INPUT **
c     real x         Input z score
c
c     ** OUTPUT **
c     real probnd1   Area under normal curve to right of x
c
c
c
      real x, probnd1
c
c
c
c-------------------------------------------------------------------------------
c
c
c
      b1 =  0.319381530
      b2 = -0.356563782
      b3 =  1.781477937
      b4 = -1.821255978
      b5 =  1.330274429
      p  =  0.2316419
      pi =  4.0 * atan(1.0)
c
      t = 1.0 / (1.0 + (p * x))
      term1 = ((((b1 * t) + (b2 * (t ** 2))) + (b3 * (t ** 3))) + (b4 *
     &(t ** 4))) + (b5 * (t ** 5))
c
c
c
c-------------------------------------------------------------------------------
c     If x is large, set probnd1 to zero to avoid error in computing
c     z (underflow).
c-------------------------------------------------------------------------------
c
c
c
      probnd1 = 0.0
      if (x .gt. 7.0) goto 1
      z = (1.0 / sqrt(2.0 * pi)) * exp(- ((x * x) / 2.0))
      probnd1 = z * term1
    1 continue
      return
      end
c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
c     SUBROUTINE AUTO1.
c
c     Date:    27-OCT-1994.
c     Author:  B.D. Santer, LLNL, Livermore.
c     Version: Single precision.
c
c     Computes autocovariance (cxx) and autocorrelation (rxx) functions 
c     of an input data series x(t). User specifies the number of lags
c     computed.
c
c     REFERENCE:
c
c     G.M. Jenkins and D.G. Watts.
c     Spectral Analysis and its Applications.
c     Holden-Day, 1968, pages 180-182, (5.3.25) and (5.3.33).
c
c
c
      subroutine auto1(n, n1, lag, x, cxx, rxx)
c
c
c
      implicit real (o-z, a-h)
c
c     ** INPUT **
c     integer n           Maximum dimension of x 
c     integer n1          Actual dimension of x 
c     integer lag         Number of lags computed 
c     dimension x(n)      Input data series
c
c     ** OUTPUT **
c     dimension cxx(0 : lag)    Autocovariance function 
c     dimension rxx(0 : lag)    Autocovariance function 
c
      integer n, n1
      dimension x(n), cxx(0 : lag), rxx(0 : lag)
c 
c 
c 
c-------------------------------------------------------------------------------
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.0 **
c
c     Compute sample mean of x.
c-------------------------------------------------------------------------------
c
c
c
      sum1 = 0.0
      sum2 = 0.0
c
      do 100 it = 1, n1
        sum1 = sum1 + x(it)
  100 continue
      xbar = sum1 / float(n1)
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.1 **
c
c     Compute total variance of x.
c-------------------------------------------------------------------------------
c
c
c
      do 110 it = 1, n1
        sum2 = sum2 + ((x(it) - xbar) ** 2)
  110 continue
      c0 = sum2 / float(n1)
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.2 **
c
c     Compute autocovariance and autocorrelation for each lag (0 to lag).
c-------------------------------------------------------------------------------
c
c
c
      do 121 k = 0, lag
        sum3 = 0.0
        n2   = n1 - k
        do 120 it = 1, n2
          sum3 = sum3 + ((x(it) - xbar) * (x(it + k) - xbar))
  120   continue
c
        cxx(k) = sum3 / float(n1)
        if (c0 .eq. 0.0) then
          print *, 'AUTO1. zero variance for c0! '
          rxx(k) = 1.0
        else
          rxx(k) = cxx(k) / c0 
        end if
c
c       if (k .eq. 0) write(*, 990)
c       write(*,991) k, cxx(k), rxx(k)
  121 continue
c
  990 format(t2, 'Lag', t10, 'Autocovariance', t30,
     &'Autocorrelation' )
  991 format(i3, t10, f12.5, t30, f12.5)
c
      return
      end










