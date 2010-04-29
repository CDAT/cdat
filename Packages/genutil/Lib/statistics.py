# Adapted for numpy/ma/cdms2 by convertcdms.py
import MV2
import numpy.ma,cdms2
#import genutil
from grower import grower
import numpy

import arrayindexing,array_indexing_emulate as array_indexing

class StatisticsError (Exception):
    def __init__ (self, args=None):
        """Create an exception"""
        self.args = args
    def __str__(self):
        """Calculate the string representation"""
        return str(self.args)
    __repr__ = __str__



def __gammln1(x):
    cof=[76.18009172947146,-86.50532032941677,
         24.01409824083091,-1.231739572450155,
         .1208650973866179E-2,-.5395239384953E-5]
    stp=2.5066282746310005
    y=x*1.
    tmp=x+5.5
    tmp=(x+0.5)*numpy.ma.log(tmp)-tmp
    ser=1.000000000190015
    for j in range(6):
        y=y+1.
        ser=ser+cof[j]/y
    return tmp+numpy.ma.log(stp*ser/x)

def __betacf1(a,b,x):
    MAXIT=100
    EPS=3.E-7
    FPMIN=1.E-30
    qab=a+b
    qap=a+1.
    qam=a-1.
    c=1.
    d=1.-qab*x/qap
    d=numpy.ma.where(numpy.ma.less(numpy.ma.absolute(d),FPMIN),FPMIN,d)
    d=1./d
    h=d
    for m in range(1,MAXIT+1):
        m2=2*m
        aa=m*(b-m)*x/((qam+m2)*(a+m2))
        d=1.+aa*d
        d=numpy.ma.where(numpy.ma.less(numpy.ma.absolute(d),FPMIN),FPMIN,d)
        c=1.+aa/c
        c=numpy.ma.where(numpy.ma.less(numpy.ma.absolute(c),FPMIN),FPMIN,c)
        d=1./d
        h=h*d*c
        aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
        d=1.+aa*d
        d=numpy.ma.where(numpy.ma.less(numpy.ma.absolute(d),FPMIN),FPMIN,d)
        c=1.+aa/c
        c=numpy.ma.where(numpy.ma.less(numpy.ma.absolute(c),FPMIN),FPMIN,c)
        d=1./d
        delet=d*c
        h=h*delet
        if numpy.ma.allclose(delet,numpy.ones(delet.shape),atol=EPS,rtol=0.):
            break
    h=numpy.ma.masked_where(numpy.ma.greater(numpy.ma.absolute(delet-1.),EPS),h)
    return h
    
def __betai1(a, b, x):
    bt=numpy.ma.logical_or(numpy.ma.equal(x,0.),numpy.ma.equal(x,1.))
    bt=numpy.ma.where(bt,0.,numpy.ma.exp(
        __gammln1(a+b)-__gammln1(a)-__gammln1(b)+\
        a*numpy.ma.log(x)+b*numpy.ma.log(1.-x)
        )
                )
    return numpy.ma.where(numpy.ma.less(x,(a+1.)/(a+b+2.)),
                    bt*__betacf1(a,b,x)/a,
                    1.-bt*__betacf1(b,a,1.-x)/b)

def __probnd1(x):
    """
    c     FUNCTION PROBND1.
    c
    c     Calculates the area under a normal curve (mean=0.0, variance=1.0)
    c     to the right of x. The accuracy is better than 7.5 * 10.**-8.
    c
    c     REFERENCE:
    c
    c     M. Abramowitz and I.A. Stegun.
    c     Handbook of Mathematical Functions.
    c     Dover, 1970, pages 931-932 (26.2.1 and 26.2.17).
"""
    b1 =  0.319381530
    b2 = -0.356563782
    b3 =  1.781477937
    b4 = -1.821255978
    b5 =  1.330274429
    p  =  0.2316419
    t = 1.0 / (1.0 + (p * x))
    term1 = ((((b1 * t) + (b2 * (t ** 2))) + \
              (b3 * (t ** 3))) + (b4 * (t ** 4))) + \
              (b5 * (t ** 5))
    z = (1.0 / numpy.ma.sqrt(2.0 * numpy.pi)) * numpy.ma.exp(- ((x * x) / 2.0))
    return numpy.ma.where(numpy.ma.greater(x,7.),0.,z * term1)
    

def __probf1(y, n1, n2, id):
    """
c     FUNCTION PROBF1.
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

c     ** INPUT **
c     real y            Calculated F-value
c     real x            Inverse of Y if Y is less than 1.0
c     integer n1, n2    Degrees of freedom
c     integer id        Identifier for one- or two-tailed test
c
c     ** OUTPUT **
c     real probf1       Significance level (p-value) for F-value
c
c     EXTERNALS:
c
c     function PROBND1 - Calculates the area under a normal curve.
"""
    ly=numpy.ma.less(y,1.)
    x=numpy.ma.where(ly,1./numpy.ma.array(y),y)
    n=numpy.ma.where(ly,n1,n2)
    n1=numpy.ma.where(ly,n2,n1)
    n2=numpy.ma.where(ly,n,n2)
    term1 = 2.0 / (9.0 * n1)
    term2 = 2.0 / (9.0 * n2)
    term3 = ((x ** (1.0 / 3.0)) * (1.0 - term2)) - (1.0 - term1)
    term4 = numpy.ma.sqrt(term1 + ((x ** (2.0 / 3.0)) * term2))
    term5 = term3 / term4
    probf1 = id * __probnd1(term5)
        
    #     The numerical algorithm can have problems when the F-value is
    #     close to 1.0 and the degrees of freedom are small. Therefore,
    #     insure that the probabilities returned cannot exceed 1.0.
    
    return numpy.ma.where(numpy.ma.greater(probf1,1.),1.,probf1)


def __geometricmean(x):
    """
    Function: __geom
    Description:
       Returns the geometric mean (on first dimension)
    Usage:
    geo=__geometricmean(x)
    """
    g=numpy.ma.exp(numpy.ma.sum(numpy.ma.log(x)/numpy.ma.count(x,axis=0),0))
    # Now check for negative values
    t=numpy.ma.sum(numpy.ma.less(x,0.),axis=0)
    g=numpy.ma.masked_where(numpy.ma.greater(t,0.),g)
    t=numpy.ma.sum(numpy.ma.equal(x,0),axis=0)
    g=numpy.ma.where(numpy.ma.greater(t,0),0.,g)
    return g
    
   
def _treat_missing(out,x,max_pct_missing=100.):
    """
    # Max_pct_missing to missing data specified, calculate new mask
    # If fraction of data that is missing is more than the max_pct_missing allows mask point out
    """
##     xmask=numpy.ma.less_equal((1. - max_pct_missing/100.),(numpy.ma.size(x,axis=0)-numpy.ma.count(x,axis=0))/float(numpy.ma.size(x,axis=0)))
##     print 'X:',x.shape,numpy.ma.size(x,axis=0),numpy.ma.count(x,axis=0)
    xmask = numpy.ma.less_equal((max_pct_missing/100.),(numpy.ma.size(x,axis=0)-numpy.ma.count(x,axis=0))/float(numpy.ma.size(x,axis=0)))
    # numpy.ma.sum ignores missing_data, mask out results where amount of missing_data
    # is over the specified max_pct_missing
##     inverse_mask=numpy.choose(numpy.ma.getmaskarray(out),(1.0,0,0)) * xmask
##     return numpy.ma.masked_where(numpy.choose(inverse_mask,(1.0,0.0)),out)
##     print 'TM:',xmask.shape,out.shape
    return numpy.ma.masked_where(xmask,out)
    
def __covariance(x,y,weights=None,centered=1,biased=1):
    """
    Function: __covariance
     
    Description of function:
        Does the main computation for returning covariance. See documentation
        of covariance() for details.
    """
    if not weights is None and biased!=1 :
        raise StatisticsError,'Error in covariance, you cannot have weights and unbiased together'
    
    if centered == 1:
        xmean=numpy.ma.average(x,weights=weights, axis=0)
        ymean=numpy.ma.average(y,weights=weights, axis=0)
        x=x-xmean
        y=y-ymean
        del(xmean)
        del(ymean)
    #
    if weights is None:
        weights=numpy.ma.ones(x.shape,dtype=x.dtype.char)
    if not ((x.mask is None) or (x.mask is MV2.nomask)) :
        weights=numpy.ma.masked_where(x.mask,weights)
##     if not ((y.mask is None) or (y.mask is MV2.nomask)) :
##         weights=numpy.ma.masked_where(y.mask,weights)
    if biased == 1:
        cov = numpy.ma.sum(x*y*weights,axis = 0)/numpy.ma.sum(weights,axis = 0)
    else:
        cov = numpy.ma.sum(x*y, axis=0)/(numpy.ma.count(x*y,axis=0)-1)

    return cov

def __variance(x,weights=None,centered=1,biased=1):
    """
    Function: __variance
    
    Description of function:
        Does the main computation for returning variance. See documentation
        of variance() for details.
    """
    return __covariance(x,x,weights=weights,centered=centered,biased=biased)
                        
def __std(x,weights=None,centered=1,biased=1):
    """
    Function: __std
     
    Description of function:
        Does the main computation for returning standard deviation. See 
        documentation of std() for details.
    """
    return numpy.ma.sqrt(__variance(x,weights=weights,centered=centered,biased=biased))


def __correlation(x,y,weights=None,centered=1,biased=1):
    """
    Function: __correlation
     
    Description of function:
        Does the main computation for returning correlation. See documentation
        of correlation() for details.
    """
    cov=__covariance(x,y,weights=weights,centered=centered,biased=biased)
    sx=__std(x,weights=weights,centered=centered,biased=biased)
    sy=__std(y,weights=weights,centered=centered,biased=biased)
    return cov/(sx*sy)

def __rms(x,y,weights=None,centered=0,biased=1):
    """
    Function: __rms
     
    Description of function:
        Does the main computation for returning rms. See documentation
        of rms() for details.
    """
    
    return __std(x-y,centered=centered,biased=biased,weights=weights)

def __laggedcovariance(x,y,lag=1,centered=1,partial=1):
    """
    Function: __laggedcovariance
     
    Description of function:
        Does the main computation for returning lagged covariance. See 
        documentation of laggedcovariance() for details.
    """
    if lag==0 : return __covariance(x,y,centered=centered)
    
    if partial==1:
        if lag>0:
            x=x[lag:]
            y=y[:-lag]
        else:
            x=x[:lag]
            y=y[-lag:]
        
    
    if centered == 1 :
        xmean=numpy.ma.average(x, axis=0)
        ymean=numpy.ma.average(y, axis=0)
    else:
        xmean=0.
        ymean=0.
    x=x-xmean
    y=y-ymean
    del(xmean)
    del(ymean)
    
    if partial==1:
        tmp=x*y
    else:
        if lag>0:
            tmp=x[lag:]*y[:-lag]
        else:
            tmp=x[:-lag]*y[lag:]
    return numpy.ma.sum(tmp, axis=0)/numpy.ma.count(x*y,axis=0)

def __laggedcorrelation(x,y,lag,centered=1,partial=1,biased=1):
    """
    Function: __laggedcorrelation
     
    Description of function:
        Does the main computation for returning lagged correlation. See 
        documentation of laggedcorrelation() for details.
    """
    cov=__laggedcovariance(x,y,lag,centered=centered,partial=partial)
    if partial==1 and lag!=0:
        if lag>0:
            sx=__std(x[lag:], centered=centered,biased=biased)
            sy=__std(y[:-lag],centered=centered,biased=biased)
        else:
            sx=__std(x[:lag], centered=centered,biased=biased)
            sy=__std(y[-lag:],centered=centered,biased=biased)
            
    else:
        sx=__std(x,centered=centered,biased=biased)
        sy=__std(y,centered=centered,biased=biased)
        
    return cov/(sx*sy)



def __autocovariance(x,lag,centered=1,partial=1):
    """
    Function: __autocovariance
     
    Description of function:
        Does the main computation for returning autocovariance. See 
        documentation of autocovariance() for details.
    """
    return __laggedcovariance(x,x,lag,centered=centered,partial=partial)

def __autocorrelation(x,lag,centered=1,partial=1):
    """
    Function: __autocorrelation
     
    Description of function:
        Does the main computation for returning autocorrelation. See 
        documentation of autocorrelation() for details.
    """
    if partial==1 and centered==1 and lag!=0:
        mean1=numpy.ma.average(x[:-lag], axis=0)
        mean2=numpy.ma.average(x[lag:], axis=0)
        x1=x[:-lag]-mean1
        x2=x[lag:]-mean2
        num=numpy.ma.sum(x1*x2, axis=0)
        den=numpy.ma.sum(numpy.ma.power(x1,2),axis=0)*numpy.ma.sum(numpy.ma.power(x2,2), axis=0)
        return num/numpy.ma.sqrt(den)
    else:
        return __autocovariance(x,lag,centered=centered,partial=partial)/__autocovariance(x,0,centered=centered,partial=partial)

def __meanabsdiff(x,y,weights=None,centered=1):
    """
    Function: __meanabsdiff
     
    Description of function:
        Does the main computation for returning mean absolute difference. See 
        documentation of meanabsdiff() for details.
    """    
    if centered == 1 :
        xmean=numpy.ma.average(x,weights=weights, axis=0)
        ymean=numpy.ma.average(y,weights=weights, axis=0)
    else:
        xmean=0.
        ymean=0.
    x=x-xmean
    y=y-ymean
    del(xmean)
    del(ymean)
    if weights is None:
        weights=numpy.ma.ones(x.shape,dtype=x.dtype.char)
    if not ((x.mask is None) or (x.mask is MV2.nomask)) :
        weights=numpy.ma.masked_where(x.mask,weights)
    if not ((y.mask is None) or (y.mask is MV2.nomask)) :
        weights=numpy.ma.masked_where(y.mask,weights)
        
    return numpy.ma.sum(numpy.ma.absolute(x-y)*weights,axis=0)/numpy.ma.sum(weights, axis=0)
                             

def __linearregression(y,x,error=None,probability=None,noslope=None,nointercept=None):
    """
    returns slope/intercept for linear regression of dependant var y and indep var x
    also possibly returns error and P values.
    """
    if (not (noslope is None or noslope==0)) and (not (nointercept is None or nointercept==0)):
        raise StatisticsError,'Error in __linearregression, at least one of the following argument as to be None: noslope or nointercept, you are requesting nothing back !'
    if (not probability is None) and (error is None):
        raise StatisticsError,'Error in __linearregression, error must not be None if probability is defined, probability is:'+str(probability)
    if not error is None:
        if error>3:
            raise StatisticsError,"Error in __linearregression, error must be None (0), 1, ,2 or 3"
    xmean=numpy.ma.average(x, axis=0)
    ymean=numpy.ma.average(y, axis=0)
    x=x-xmean
    y=y-ymean
    xy=numpy.ma.sum(y*x, axis=0)
    xx=numpy.ma.sum(x*x, axis=0)
    slope=xy/xx
    intercept=ymean-slope*xmean
    V=[]
    if noslope is None or noslope==0:
        V.append(slope)
    if nointercept is None or nointercept==0:
        V.append(intercept)
    if error is None or error==0:
        return V
    elif error==1:
        E=[]
        n1=numpy.ma.count(y,axis=0)
        # Unadjusted errors
        res=(y+ymean)-(intercept+(x+xmean)*numpy.ma.resize(slope,numpy.ma.shape(y))) # x2
        ssd=numpy.ma.sum(res*res, axis=0)
        amsd1=ssd/(n1-2.) # amsd1=ssd/idfd1
        if noslope is None or noslope==0:
            E.append(numpy.ma.sqrt(amsd1/xx))
        if nointercept is None or nointercept==0:
            s1=xmean*xmean/xx+1./n1
            E.append(numpy.ma.sqrt(amsd1*s1))
        if probability is None or probability==0:
            return V, E
        else:
            Pt1=[]
            Pt2=[]
            Pf1=[]
            Pf2=[]
            f=numpy.ma.sum(y*y, axis=0)-ssd # ssr
            f=f/amsd1
            aa1 = n1 / 2.0
            if noslope is None or noslope==0:
                tb1=slope/E[0]
                xx3 = n1 / (n1 + (tb1 * tb1))
                Pt1.append(__betai1(aa1, .5, xx3))
                Pt2.append(None)
                Pf1.append(__probf1(f, 1, n1 - 2, 1))
                Pf2.append( __probf1(f, 1, n1 - 2, 2))
            if nointercept is None or nointercept==0:
                tb1=V[-1]/E[-1]
                xx3 = n1 / (n1 + (tb1 * tb1))
                Pt1.append(__betai1(aa1, .5, xx3))
                Pt2.append(None)
                Pf1.append(__probf1(f, 1, n1 - 2, 1))
                Pf2.append( __probf1(f, 1, n1 - 2, 2))
            return V,E,Pt1,Pt2,Pf1,Pf2
    else:
        E=[]
        # Adjusted error from residual
        n1=numpy.ma.count(y,axis=0)
        res=(y+ymean)-(intercept+numpy.ma.resize(slope,numpy.ma.shape(y))*(x+xmean)) # x2
        ssd=numpy.ma.sum(res*res, axis=0)
        if error==2:
            ac=__autocorrelation(res,1,centered=1,partial=0)
            rdfd2 = 1.0 + ac
            rdfd2 = (1.0 - ac)/rdfd2
        elif error==3:
            ac=__autocorrelation(y+ymean,1,centered=1,partial=0)
            rdfd2 = 1.0 + ac
            rdfd2 = (1.0 - ac)/rdfd2            
        rneff = n1 * rdfd2 # rneff 
        amsd2=ssd/(rneff -2.)   # ssd/rdfd2
        if noslope is None or noslope==0:
            E.append(numpy.ma.sqrt(amsd2/xx))
        if nointercept is None or nointercept==0:
            s1=xmean*xmean/xx+1./n1
            E.append(numpy.ma.sqrt(amsd2*s1))
        if probability is None or probability==0:
            return V, E
        else:
            Pt1=[]
            Pt2=[]
            Pf1=[]
            Pf2=[]
            f=numpy.ma.sum(y*y, axis=0)-ssd # ssr = amsr
            amsd1=ssd/(n1-2.) # amsd1=ssd/idfd1
            f=f/amsd1 # amsr/ssd
            aa1 = n1 / 2.0
            aa2 = rneff / 2.0
            if noslope is None or noslope==0:
                tb2=slope/E[0]
                xx1 = n1 / (n1 + (tb2 * tb2))
                xx2 = rneff / (rneff + (tb2 * tb2))
                Pt1.append(__betai1(aa1, .5, xx1))
                Pt2.append(__betai1(aa2, .5, xx2))
                Pf1.append(__probf1(f, 1, n1 - 2, 1))
                Pf2.append( __probf1(f, 1, n1 - 2, 2))
            if nointercept is None or nointercept==0:
                tb2=V[-1]/E[-1]
                xx1 = n1 / (n1 + (tb2 * tb2))
                xx2 = rneff / (rneff + (tb2 * tb2))
                Pt1.append(__betai1(aa1, .5, xx1))
                Pt2.append(__betai1(aa2, .5, xx2))
                Pf1.append(__probf1(f, 1, n1 - 2, 1))
                Pf2.append( __probf1(f, 1, n1 - 2, 2))
            return V,E,Pt1,Pt2,Pf1,Pf2


def __makeweights(x,w,axes):
    """
    This function takes an array and weights options from Krishna\'s averager
    and return an numpy.ma of the coresponding weights
    """

    # Now if weights is a list, uses Krishna's stuff to get the weights....
    import cdutil
    tmpaxes=axes
    if type(axes)==type(1): tmpaxes=str(axes)
    # First make sure x and w have same dims if w is MV2
    if cdms2.isVariable(w) and cdms2.isVariable(x) and x.shape!=w.shape:
        x,w=grower(x,w)
    w=cdutil.__check_weightoptions(x, tmpaxes, w)
    if not numpy.ma.isarray(w):
        # Ok Krishna returned a list of 1D arrays.... Let's put it together
        axs=x.getAxisList()
        axes=cdms2.order2index(axs,axes)[:len(cdms2.orderparse(axes))]
        endax=[]
        for i in range(len(axes)):
            if w[i]=='unweighted':
                w[i]=numpy.ma.ones(len(axs[axes[i]]),dtype=x.dtype.char)
            if i==0:
                wo=w[i]
                endax.append(axs[axes[i]])
            else:
                wo=wo[...,None]*w[i]
                endax.append(axs[axes[i]])
        w=cdms2.MV2.array(wo)
        w.setAxisList(endax)
##     else:
##         w.setAxisList(x.getAxisList())
    return w
            
def __checker(x,y,w,axes,smally=0):
    # Are the input Variables ?
    xismv=cdms2.isVariable(x)
    yismv=cdms2.isVariable(y)
    if y is None : yismv=1
    wismv=cdms2.isVariable(w)
    if w is None : wismv=1
    ax=None
    if not numpy.ma.isarray(x):
        x=numpy.ma.array(x,copy=0)
    if not numpy.ma.isarray(y) and not y is None:
        y=numpy.ma.array(y,copy=0)
    if not numpy.ma.isarray(w) and not w is None and not type(w)==type(''):
        if not type(w[0])==type(''):
            w=numpy.ma.array(w,copy=0)
        else:
            if not xismv:
                raise StatisticsError,'Error if weights are a list then x must be an MV2 !!!'
            w=__makeweights(x,w,axes)
            wismv=1
    elif not w is None:
            if not xismv:
                raise StatisticsError,'Error if weights are a list then x must be an MV2 !!!'
            w=__makeweights(x,w,axes)
            wismv=1
        
    if xismv * yismv * wismv !=1:
        # We didn't pass all MV2s shapes have to match (unless None)
        if smally==0:
            if x.shape!=numpy.ma.shape(y) and not y is None:
                raise StatisticsError,'Error x and y shape do not match !'+str(x.shape)+','+str(numpy.ma.shape(y))
        else:
            shy=list(y.shape)
            shy2=y.shape
            shx=list(x.shape)
            if isinstance(axes,str):
                myaxes=[]
                for i in axes:
                    myaxes.append(eval(i))
            elif isinstance(axes,int):
                myaxes=[axes,]
            else:
                myaxes=list(axes)
            for anaxis in myaxes[::-1]:
                shy.insert(0,shx[anaxis])
            y=numpy.ma.resize(y,shy)
            sh=range(len(x.shape))
            if axes!=0:
                for i in range(len(myaxes)):
                    sh[myaxes[i]]=i
                    sh[i]=myaxes[i]
                y=numpy.ma.transpose(y,sh)
            if x.shape!=numpy.ma.shape(y) and not y is None:
                raise StatisticsError,'Error x and y shape do not match (y shouldbe 1D less than x) !'+str(x.shape)+','+str(shy2)+' Remember y must be 1D less than x'
        if x.shape!=numpy.ma.shape(w) and not w is None:
            raise StatisticsError,'Error x and weights shape do not match !'+str(x.shape)+','+str(numpy.ma.shape(w))+' ATTENTION if you are trynig to pass a list of 1D arrays for each dim, then x must be an MV2 !!!'
        if type(axes)!=type([]) :
            axes=cdms2.orderparse(str(axes))
        for i in axes:
            if len(x.shape)<i:
                raise StatisticsError,'Error you have '+str(len(x.shape))+' dimensions and try to work on dim:'+str(i)
    else:
        if not y is None:
            x,y=grower(x,y)
            if x.shape!=y.shape :
                raise StatisticsError,'Error x and y have different shapes'+str(x.shape)+', '+str(y.shape)
        ax=x.getAxisList()
        xorder=x.getOrder(ids=1)
        # Now grows w
        if not w is None:
            worder=w.getOrder(ids=1)
            waxes=w.getAxisList()
            for o in worder:
                if not o in xorder:
                    raise StatisticsError,'Error weights have a dimension that is neither in x or y:'+o
            x,w=grower(x,w)
            if x.shape!=w.shape:
                raise StatisticsError,'Error x and weights have different shapes'+str(x.shape)+', '+str(w.shape)
        # Last thing convert the axes input to numbers
        if type(axes)==type(1) : axes=str(axes)
        if type(axes)!=type([]):
            axesparse=cdms2.orderparse(axes)
            naxes=len(axesparse)
            for i in range(naxes):
                o=axesparse[i]
                if type(o)==type(''):
                    for j in range(len(xorder)):
                        if xorder[j]==o : axesparse[i]=j
                    if type(axesparse[i])==type(''): # Well it must be a name for x y t....
                        for j in range(len(x.shape)):
                            if o[1:-1]==x.getAxis(j).id:
                                axesparse[i]=j
                    if type(axesparse[i])==type(''): # Everything failed the axis id must be not existing in the slab...
                        raise StatisticsError,'Error axis id :'+o+' not found in first slab: '+x.getOrder(ids=1)
            axes=axesparse
    # Now we have array those shape match, and a nice list of axes let's keep going
    naxes=len(axes)
    n0=1
    xsh=x.shape
    xorder=range(len(x.shape))
    forder=[]
    for i in range(naxes):
        forder.append(axes[i])
        n0=n0*xsh[axes[i]]
    fsh=[n0]
    ax2=[]
    for i in range(len(x.shape)):
        if not i in forder:
            forder.append(i)
            fsh.append(xsh[i])
            if not ax is None: ax2.append(ax[i])
    if not ax is None: ax=ax2
    x=numpy.ma.transpose(x,forder)
    x=numpy.ma.resize(x,fsh)
    if not y is None:
        y=numpy.ma.transpose(y,forder)
        y=numpy.ma.resize(y,fsh)
    if not w is None:
        w=numpy.ma.transpose(w,forder)
        w=numpy.ma.resize(w,fsh)
    # Now mask everything correctly (union of masks)
    if not y is None:
        m=y.mask
        if not m is numpy.ma.nomask:
            x=numpy.ma.masked_where(m,x)
        m=x.mask
        if not m is numpy.ma.nomask:
            y=numpy.ma.masked_where(m,y)
    if not w is None:
        m=x.mask
        if not m is numpy.ma.nomask:
            w=numpy.ma.masked_where(m,w)

    ## IF y has to be 1D less than x, then it is shrunk back
    if smally==1:
        y=y[0]
    return x,y,w,axes,ax
    
def covariance(x,y,weights=None,axis=0,centered=1,biased=1,max_pct_missing=100.):
    """
    Function: covariance
     
    Description of function:
        Returns the covariance between 2 slabs. By default on the first dimension,
        centered and biased by default.
    Usage:
        cov = covariance(x, y, weights=weightoptions, axis=axisoptions,
                         centered=centeredoptions, biased=biasedoptions,
                         max_pct_missing=max_pct_missingoptions)
    Options:
        weightoptions
            default = None. If you want to compute the weighted covariance,
            provide the weights here.
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.
        centeredoptions None | 0 | 1
            default value = 1 removes the mean first. Set to 0 or None for
            uncentered.
        biasedoptions None | 0 | 1
            default value = 1 If want to compute an unbiased variance pass
            anything but 1.
        max_pct_missingoptions
            default value = 100. Maximum fraction of cell which is allowed to be masked (missing).
            Set to a percentage between 0 and 100%.
            Set to 0. to mask results if any data is masked.
            Set to 100. to calculate result if any data is not masked
    """
    if cdms2.isVariable(x) : xatt=x.attributes
    if cdms2.isVariable(y) : yatt=y.attributes
    x,y,weights,axis,ax=__checker(x,y,weights,axis)
    
    cov=__covariance(x,y,weights=weights,centered=centered,biased=biased)
    cov=_treat_missing(cov,x,max_pct_missing=max_pct_missing)
    if not ax is None:
        cov=cdms2.createVariable(cov,axes=ax,id='covariance',copy=0)
        if 'units' in xatt.keys() and 'units' in yatt.keys(): cov.units=xatt['units']+'*'+yatt['units']
    return cov


def variance(x,weights=None,axis=0,centered=1,biased=1,max_pct_missing=100.):
    """
    Function: variance
    
    Description of function:
        Returns the variance from a slab. By default  on first dimension,
        centered, and biased.
    Usage:
        result = variance(x, weights=weightoptions, axis = axisoptions,
                          centered=centeredoptions, biased = biasedoptions,
                          max_pct_missing=max_pct_missingoptions)
    Options:
        weightoptions 
            If you want to compute the weighted variance, provide weights here.
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.
        centeredoptions None | 0 | 1
            default value = 1 removes the mean first. Set to 0 or None for
            uncentered.
        biasedoptions None | 0 | 1
            default value = 1 If want to compute an unbiased variance pass
            anything but 1.
        max_pct_missingoptions
            default value = 100. Maximum fraction of cell which is allowed to be masked (missing).
            Set to a percentage between 0 and 100%.
            Set to 0. to mask results if any data is masked.
            Set to 100. to calculate result if any data is not masked
    """
    if cdms2.isVariable(x) : xatt=x.attributes
    x,dum,weights,axis,ax=__checker(x,None,weights,axis)

    var=__variance(x,weights=weights,centered=centered,biased=biased)
    var=_treat_missing(var,x,max_pct_missing=max_pct_missing)
    if not ax is None:
        var=cdms2.createVariable(var,axes=ax,id='variance',copy=0)
        if 'units' in xatt.keys() : var.units=xatt['units']+'*'+xatt['units']
    return var

def checker(x,weights=None,axis=0,centered=1):
    if cdms2.isVariable(x) : xatt=x.attributes
    x,dum,weights,axis,ax=__checker(x,None,weights,axis)

    return x,weights,axis,ax

def std(x,weights=None,axis=0,centered=1,biased=1,max_pct_missing=100.):
    """
    Function: std
     
    Description of function:
        Returns the standard deviation from a slab. By default  on first
        dimension, centered, and biased.
    Usage:
        result = std(x, weights=weightoptions, axis = axisoptions,
                     centered=centeredoptions, biased = biasedoptions,
                     max_pct_missing=max_pct_missingoptions)
    Options:
        weightoptions 
            If you want to compute the weighted statistic, provide weights here.
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.
        centeredoptions None | 0 | 1
            default value = 1 removes the mean first. Set to 0 or None for
            uncentered.
        biasedoptions None | 0 | 1
            default value = 1 If want to compute an unbiased variance pass
            anything but 1.
        max_pct_missingoptions
            default value = 100. Maximum fraction of cell which is allowed to be masked (missing).
            Set to a percentage between 0 and 100%.
            Set to 0. to mask results if any data is masked.
            Set to 100. to calculate result if any data is not masked
    """
    if cdms2.isVariable(x) : xatt=x.attributes
    x,dum,weights,axis,ax=__checker(x,None,weights,axis)
    std=__std(x,weights=weights,centered=centered,biased=biased)
    std=_treat_missing(std,x,max_pct_missing=max_pct_missing)
    if not ax is None:
        std=cdms2.createVariable(std,axes=ax,id='standard_deviation',copy=0)
        if 'units' in xatt.keys() : std.units=xatt['units']
    return std


def correlation(x,y,weights=None,axis=0,centered=1,biased=1,max_pct_missing=100.):
    """
    Function: correlation
     
    Description of function:
        Returns the correlation between 2 slabs. By default on the first
        dimension, centered and biased by default.
    Usage:
        result = correlation(x, y, weights=weightoptions, axis=axisoptions,
                             centered=centeredoptions, biased=biasedoptions,
                             max_pct_missing=max_pct_missingoptions)
    Options:
        weightoptions
            default = None. If you want to compute the weighted correlation,
            provide the weights here.
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.
        centeredoptions None | 0 | 1
            default value = 1 removes the mean first. Set to 0 or None for
            uncentered.
        biasedoptions None | 0 | 1
            default value = 1 returns biased statistic. If want to compute an
            unbiased statistic pass anything but 1.
        max_pct_missingoptions
            default value = 100. Maximum fraction of cell which is allowed to be masked (missing).
            Set to a percentage between 0 and 100%.
            Set to 0. to mask results if any data is masked.
            Set to 100. to calculate result if any data is not masked
    """
    x,y,weights,axis,ax=__checker(x,y,weights,axis)
    
    cor=__correlation(x,y,weights=weights,centered=centered,biased=biased)
    cor=_treat_missing(cor,x,max_pct_missing=max_pct_missing)
    if not ax is None:
        cor=cdms2.createVariable(cor,axes=ax,id='correlation',copy=0)
        cor.units='-'
    return cor

def rms(x,y,weights=None,axis=0,centered=0,biased=1,max_pct_missing=100.):
    """
    Function: rms
     
    Description of function:
        Returns the root mean square difference between 2 slabs. By default from
        a slab (on first dimension) "uncentered" and "biased" by default.
    Usage:
        result = rms(x, y, weights=weightoptions, axis = axisoptions,
                     centered=centeredoptions, biased = biasedoptions,
                     max_pct_missing=max_pct_missingoptions)
    Options:
        weightoptions 
            default = None returns equally weighted statistic. If you want to
            compute the weighted statistic, provide weights here.
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.
        centeredoptions None | 0 | 1
            default value = 0 returns uncentered statistic (same as None). To
            remove the mean first (i.e centered statistic) set to 1. NOTE: Most
            other statistic functions return a centered statistic by default.
        biasedoptions None | 0 | 1
            default value = 1 If want to compute an unbiased variance pass
            anything but 1.
        max_pct_missingoptions
            default value = 100. Maximum fraction of cell which is allowed to be masked (missing).
            Set to a percentage between 0 and 100%.
            Set to 0. to mask results if any data is masked.
            Set to 100. to calculate result if any data is not masked
    """
    if cdms2.isVariable(x) : xatt=x.attributes
    x,y,weights,axis,ax=__checker(x,y,weights,axis)
    rmsans=__rms(x,y,weights=weights,centered=centered,biased=biased)
    rmsans=_treat_missing(rmsans,x,max_pct_missing=max_pct_missing)
    if not ax is None:
        rmsans=cdms2.createVariable(rmsans,axes=ax,id='RMS_difference',copy=0)
        if 'units' in xatt.keys(): rms.units=xatt['units']
        
    return rmsans

def laggedcovariance(x,y,lag=None,axis=0,centered=1,partial=1,noloop=0,max_pct_missing=100.):
    """
    Function: laggedcovariance
     
    Description of function:
        Returns the covariance between 2 slabs at lag k centered and partial by
        default.
    Usage: 
        result = laggedcovariance(x, y, lag=lagoptions, axis=axisoptions,
                                  centered=centeredoptions,
                                  partial=partialoptions, noloop=noloopoptions)

        Returns value for x lags y by lag (integer)
        
    Options:
        lagoptions None | n | (n1, n2, n3...) | [n1, n2, n3 ....]
            default value = None  the maximum possible lags for specified axis
            is used.You can pass an integer, list of integers, or tuple of
            integers. 
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.
        centeredoptions
            default value = 1 removes the mean first. Set to 0 or None for
            uncentered
        partialoptions None | 0 | 1
            default value = 1 uses only common time for means.
        noloopoptions None | 0 | 1
            default value = 0 computes statistic at all lags upto 'lag'. If you
            set noloop=1 statistic is computed at lag only (not up to lag). 
        max_pct_missingoptions
            default value = 100. Maximum fraction of cell which is allowed to be masked (missing).
            Set to a percentage between 0 and 100%.
            Set to 0. to mask results if any data is masked.
            Set to 100. to calculate result if any data is not masked
    """
    if cdms2.isVariable(x) : xatt=x.attributes
    if cdms2.isVariable(y) : yatt=y.attributes
    x,y,w,axis,ax=__checker(x,y,None,axis)
    if lag is None:
        lags=range(x.shape[0])
    elif isinstance(lag,(int,long)):
        if not noloop:
            lags=range(lag+1)
        else:
            lags=[lag]
    elif not isinstance(lag,(list,tuple)):
        raise StatisticsError, 'lags type must be None, integer, list or tuple'
    else:
        lags=lag

    for k in lags:
        lcov=numpy.ma.array(__laggedcovariance(x,y,lag=k,centered=centered,partial=partial))
        lcov =_treat_missing(lcov,x,max_pct_missing=max_pct_missing)
        sh=list(lcov.shape)
        sh.insert(0,1)
        lcov=numpy.ma.resize(lcov,sh)
        if k==lags[0]:
            lcovs=lcov
        else:
            lcovs=numpy.ma.concatenate((lcovs,lcov),0)
            
    #lcovs=_treat_missing(lcovs,x,max_pct_missing=max_pct_missing)
    if not ax is None:
        newax=cdms2.createAxis(lags)
        newax.id='lag'
        ax.insert(0,newax)
        lcovs=cdms2.createVariable(lcovs,axes=ax,id='lagged_covariance'+str(lag),copy=0)
        if 'units' in xatt.keys() and 'units' in yatt.keys(): lcovs.units=xatt['units']+'*'+yatt['units']
    return lcovs

def laggedcorrelation(x,y,lag=None,axis=0,centered=1,partial=1,biased=1,noloop=0,max_pct_missing=100.):
    """
    Function: laggedcorrelation
     
    Description of function:
        Returns the correlation between 2 slabs at lag k centered, partial and
        "biased" by default.
    Usage:
        result = laggedcorrelation(x, y, lag=lagoptions, axis=axisoptions,
                                   centered=centeredoptions,
                                   partial=partialoptions,
                                   biased=biasedoptions, noloop=noloopoptions)

        Returns value for x lags y by lag
        
    Options:
        lagoptions None | n | (n1, n2, n3...) | [n1, n2, n3 ....]
            default value = None  the maximum possible lags for specified axis
            is used.You can pass an integer, list of integers, or tuple of integers. 
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.
        centeredoptions
            default value = 1 removes the mean first. Set to 0 or None for
            uncentered
        partialoptions None | 0 | 1
            default value = 1 uses only common time for means.
        biasedoptions None | 0 | 1
            default value = 1 If want to compute an unbiased variance pass
            anything but 1.
        noloopoptions None | 0 | 1
            default value = 0 computes statistic at all lags upto 'lag'. If you
            set noloop=1 statistic is computed at lag only (not up to lag). 
        max_pct_missingoptions
            default value = 100. Maximum fraction of cell which is allowed to be masked (missing).
            Set to a percentage between 0 and 100%.
            Set to 0. to mask results if any data is masked.
            Set to 100. to calculate result if any data is not masked
    """
    x,y,w,axis,ax=__checker(x,y,None,axis)
    if lag is None:
        lags=range(x.shape[0])
    elif isinstance(lag,(int,long)):
        if not noloop:
            lags=range(lag+1)
        else:
            lags=[lag]
    elif not isinstance(lag,(list,tuple)):
        raise StatisticsError, 'lags type must be None, integer, list or tuple'
    else:
        lags=lag
        
    for k in lags:
        lcor=numpy.ma.array(__laggedcorrelation(x,y,lag=k,centered=centered,partial=partial,biased=biased))
        lcor=_treat_missing(lcor,x,max_pct_missing=max_pct_missing)
        sh=list(lcor.shape)
        sh.insert(0,1)
        lcor=numpy.ma.resize(lcor,sh)
        if k==lags[0]:
            lcors=lcor
        else:
            lcors=numpy.ma.concatenate((lcors,lcor),0)
    if not ax is None:
        newax=cdms2.createAxis(lags)
        newax.id='lag'
        ax.insert(0,newax)
        lcors=cdms2.createVariable(lcors,axes=ax,id='lagged_correlation'+str(lag),copy=0)
        lcors.units='-'
        
    return lcors



def autocovariance(x,lag=None,axis=0,centered=1,partial=1,noloop=0,max_pct_missing=100.):
    """
    Function: autocovariance
     
    Description of function:
        Returns the autocovariance of a slab. By default over the first dimension,  centered, and partial.
    Usage:
        result = autocovariance(x, lag=lagoptions, axis=axisoptions,
                                centered=centeredoptions,
                                partial=partialoptions, noloop=noloopoptions)
    Options:
        lagoptions None | n | (n1, n2, n3...) | [n1, n2, n3 ....]
            default value = None  the maximum possible lags for specified axis
            is used.You can pass an integer, list of integers, or tuple of integers. 
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.
        centeredoptions None | 0 | 1
            default value = 1 removes the mean first. Set to 0 or None for
            uncentered statistic.
        partialoptions None | 0 | 1
            default value = 1 uses only common time for means.
        noloopoptions None | 0 | 1
            default value = 0 computes statistic at all lags upto 'lag'. If you
            set noloop=1 statistic is computed at lag only (not up to lag). 
        max_pct_missingoptions
            default value = 100. Maximum fraction of cell which is allowed to be masked (missing).
            Set to a percentage between 0 and 100%.
            Set to 0. to mask results if any data is masked.
            Set to 100. to calculate result if any data is not masked
    """
    if cdms2.isVariable(x) : xatt=x.attributes
    x,dum,dum,axis,ax=__checker(x,None,None,axis)
    if lag is None:
        lags=range(x.shape[0])
    elif isinstance(lag,(int,long)):
        if not noloop:
            lags=range(lag+1)
        else:
            lags=[lag]
    elif not isinstance(lag,(list,tuple)):
        raise StatisticsError, 'lags type must be None, integer, list or tuple'
    else:
        lags=lag

    for k in lags:
        acov=numpy.ma.array(__autocovariance(x,lag=k,centered=centered,partial=partial))
        acov=_treat_missing(acov,x,max_pct_missing=max_pct_missing)
        sh=list(acov.shape)
        sh.insert(0,1)
        acov=numpy.ma.resize(acov,sh)
        if k==lags[0]:
            acovs=acov
        else:
            acovs=numpy.ma.concatenate((acovs,acov),0)
    if not ax is None:
        newax=cdms2.createAxis(lags)
        newax.id='lag'
        ax.insert(0,newax)
        acovs=cdms2.createVariable(acovs,axes=ax,id='autocovariance'+str(lag),copy=0)        
        if 'units' in xatt.keys(): acovs.units=xatt['units']+'*'+xatt['units']
    return acovs

def autocorrelation(x,lag=None,axis=0,centered=1,partial=1,biased=1,noloop=0,max_pct_missing=100.):
    """
    Function: autocorrelation
     
    Description of function:
      Returns the autocorrelation of a slab at lag k centered,partial and
      "biased" by default
      
    Usage:
      result = autocorrelation(x, lag=lagoptions, axis=axisoptions,
                               centered=centeredoptions, partial=partialoptions,
                               biased=biasedoptions, noloop=noloopoptions)
    Options:
        lagoptions None | n | (n1, n2, n3...) | [n1, n2, n3 ....]
            default value = None  the maximum possible lags for specified axis is
            used.You can pass an integer, list of integers, or tuple of integers. 
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n  
            default value = 0. You can pass the name of the dimension or index 
            (integer value 0...n) over which you want to compute the statistic. 
        centeredoptions None | 0 | 1
            default value = 1 removes the mean first. Set to 0 or None for
            uncentered statistic.
        partialoptions None | 0 | 1
            default value = 1 uses only common time for means.
        biasedoptions None | 0 | 1
            default value = 1 computes the biased statistic. If want to compute 
            an unbiased statistic pass anything but 1.
        noloopoptions None | 0 | 1
            default value = 0 computes statistic at all lags upto 'lag'. If you 
            set noloop=1 statistic is computed at lag only (not up to lag). 
        max_pct_missingoptions
            default value = 100. Maximum fraction of cell which is allowed to be masked (missing).
            Set to a percentage between 0 and 100%.
            Set to 0. to mask results if any data is masked.
            Set to 100. to calculate result if any data is not masked
    """
    x,dum,dum,axis,ax=__checker(x,None,None,axis)
    if lag is None:
        lags=range(x.shape[0])
    elif isinstance(lag,(int,long)):
        if not noloop:
            lags=range(lag+1)
        else:
            lags=[lag]
    elif not isinstance(lag,(list,tuple)):
        raise StatisticsError, 'lags type must be None, integer, list or tuple'
    else:
        lags=lag
    
    for k in lags:
        acov=numpy.ma.array(__autocorrelation(x,lag=k,centered=centered,partial=partial))
        acov=_treat_missing(acov,x,max_pct_missing=max_pct_missing)
        sh=list(acov.shape)
        sh.insert(0,1)
        acov=numpy.ma.resize(acov,sh)
        if k==lags[0]:
            acovs=acov
        else:
            acovs=numpy.ma.concatenate((acovs,acov),0)
    if not ax is None:
        newax=cdms2.createAxis(lags)
        newax.id='lag'
        ax.insert(0,newax)
        acovs=cdms2.createVariable(acovs,axes=ax,id='autocorrelation'+str(lag),copy=0)
        acovs.units='-'
        
    return acovs
        
def meanabsdiff(x,y,weights=None,axis=0,centered=1,max_pct_missing=100.):
    """
    Function: meanabsdiff
     
    Description of function:
        Returns the mean absolute difference between 2 slabs x and y. By default
        on the first dimension and centered
    Usage:
        result = meanabsdiff(x, y, weights=weightoptions, axis = axisoptions,
                             centered=centeredoptions)
    Options:
        weightoptions 
            default = None returns equally weighted statistic. If you want to
            compute the weighted statistic, provide weights here.
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.
        centeredoptions None | 0 | 1
            default value = 1 removes the mean first. Set to 0 or None for uncentered.
        max_pct_missingoptions
            default value = 100. Maximum fraction of cell which is allowed to be masked (missing).
            Set to a percentage between 0 and 100%.
            Set to 0. to mask results if any data is masked.
            Set to 100. to calculate result if any data is not masked
    """
    if cdms2.isVariable(x) : xatt=x.attributes
    x,y,weights,axis,ax=__checker(x,y,weights,axis)
    mad=__meanabsdiff(x,y,weights=weights,centered=centered)
    mad=_treat_missing(mad,x,max_pct_missing=max_pct_missing)
    if not ax is None:
        mad=cdms2.createVariable(mad,axes=ax,id='mean_absolute_difference',copy=0)
        if 'units' in xatt.keys(): mad.units=xatt['units']
    return mad



def linearregression(y,axis=None,x=None,error=None,probability=None,nointercept=None,noslope=None):
    """
    Computes the linear regression of y over x or an axis. This function returns
    Values of the slope and intercept, and optionally, Error estimates and
    associated probability distributions for T-value (T-Test) and F-value (for
    analysis of variance f) can be returned. You can choose to return all these
    for either slope or intercept  or both (default behaviour). For theoretical
    details, refer to 'Statistical Methods in Atmospheric Sciences' by
    Daniel S. Wilks, Academic Press, 1995.
    
    Usage:
    result = linearregression(y, axis=axisoptions, x=xoptions, \
                    error=erroroptions, probability=probabilityoptions, \
                    nointercept=nointerceptoptions, noslope=noslopeoptions)

    Options:
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to treat the array as the
            dependent variable.
        xvalues
            default = None. You can pass an array of values that are to be used
            as the independent axis x
        nointerceptoptions None | 0 | 1
            default = None. Setting to 0 or None means intercept calculations
            are returned. To turn OFF the intercept computations set
            nointercept = 1.
        noslopeoptions None | 0 | 1
            default = None. Setting to None or 0 means slope calculations are
            returned. To turn OFF the slope computations set noslope to 1.
        erroroptions None | 0 | 1 | 2 | 3
            default = None. If set to 0 or None, no associated errors are
            returned. 
            If set to 1, the unadjusted standard error is returned. 
            If set to 2, standard error returned. This standard error is
                adjusted using the centered autocorrelation of the residual.
            If set to 3, standard error returned. The standard error here is
                adjusted using the centered autocorrelation of the raw data (y).
        probabilityoptions None | 0 | 1
            default = None. If set to 0 or None, no associated probabilities are
                returned. Set this to 1to compute probabilities.
            Note: Probabilities are returned only if erroroptions are set to one
                  of 1, 2, or 3. If it is set to None or 0, then setting
                  probabilityoptions has no meaning.

    What is returned?
        The returned values depend on the combination of options you select. If
        both slope and intercept are required, a tuple is returned for both Value
        and optionally Error (or optionally associated Probabilities), but single
        values (not tuples) are returned if only one set (slope OR intercept) is
        required. See examples below for more details.
        
        When erroroption = 1 (from description above for erroroptions you know
            that means unadjusted standard error) and probabilityoption=1, then
            the following are returned:
            pt1 : The p-value for regression coefficient t-value. (With no
                  adjustment for standard error or critical t-value.)
            None: There is only one p-value to be returned (pf1) but None is
                  returned to keep the length of the returned values consistent.
            pf1 : The p-value for regression coefficient F-value (one-tailed).
            pf2 : The p-value for regression coefficient F-value (two-tailed).
            
        When erroroption = 2 or 3 (implying error adjustment using the residual
            or the raw data and probabilityoption = 1, then the following are
            returned: 
            pt1 : The p-value for regression coefficient t-value.(With effective
                  sample size adjustment for standard error of slope.
            pt2 : The p-value for regression coefficient t-value.(With effective
                  sample size adjustment for standard error of slope and critical
                  t-value.)
            pf1 : The p-value for regression coefficient F-value (one-tailed).
            pf2 : The p-value for regression coefficient F-value (two-tailed).
            
        The values pt1 and pt2 are used to test the null hypothesis that b = 0
            (i.e., y is independent of x).
        The values pf1 and pf2 are used to test the null hypothesis that the
            regression is linear (goodness of linear fit). For non-replicated
            values of y, the degrees of freedom are 1 and n-2.

            
        Examples:

        Let us first examine the default behaviour of the linearregression
        function.
        >>> Values = statistics.linearregression(y)
        
        #The returned "Values" is actually a tuple consisting of the slope and
        #intercept. They can also be accessed as follows:
        >>> slope, intercept = statistics.linearregression(y)

        If error estimates are also required, then:
        >>> Values, Errors = linearregression(y, error=1)
        
        #where "Values" and "Errors" are tuples containing answer for
        #slope AND intercept. You can break them as follows.
        #slope, intercept = Value and slope_error, intercept_error = Errors. i.e.
        >>> (slope, intercept), (slo_error, int_error) = \
                            linearregression(y, error=1)
        
        #WARNING: The following will not work.
        >>> slope, intercept, slo_error, int_error = linearregression(y, error=1)
        
        #To get the standard error non adjusted result for slope only
        >>> slope, slope_error = linearregression(y, error=1, nointercept=1)
        
        #In the line below all the returned values are tuples. 
        >>> Values,Errors,Pt1,Pt2,Pf1,Pf2 = \
                     linearregression(y, error=2,probability=1)
        #That means in the above statement is returning tuples ordered so:
        #(slope, intercept), (slo_error, int_error), (pt1_slo, pt1_int),\
                     (pt2_slo, pt2_int), (pf1_slo, pf1_int), (pf2_slo, pf2_int)

        #If we want results returned for the intercept only.
        >>> intercept,intercept_error,pt1,pt2,pf1,pf2 = \
                  linearregression(y, error=2,probability=1,noslope=1)
                  
"""
    yisV=cdms2.isVariable(y)
    if yisV : yatt=y.attributes
    if not axis is None and not x is None:
        raise StatisticsError,'Error you cannot pass an indepedent variable and an axis'
    if x is None and axis is None :
        axis=0
    if not axis is None:
        if not type(axis)==type([]) : ax=cdms2.orderparse(str(axis))
        if len(ax)>1: raise StatisticsError, 'Error only one dim allowed'
        if yisV :
            ax=y.getAxisList()
            yid=y.id
        y,x,w,axis,axs=__checker(y,x,None,axis)
        if yisV :
            ax2=ax.pop(axis[0])
            ax.insert(0,ax2)
            y=cdms2.createVariable(y,axes=ax,id=yid,copy=0)
            x=cdms2.MV2.array(ax2[:])
            x.setAxis(0,ax2)
            y,x,w,axis,axs=__checker(y,x,None,0)
        else:
            x=numpy.ma.arange(y.shape[0])
            x=numpy.ma.resize(x,y.shape)
    else:
        y,x,w,axis,axs=__checker(y,x,None,0)
        ax2=cdms2.asVariable(x)
        try:
            ax2.units=x.units
        except:
            ax2.units=''

    if error is None or error==0:
        val=__linearregression(y,x,nointercept=nointercept,noslope=noslope)
    elif probability is None or probability==0:
        val,err=__linearregression(y,x,error=error,nointercept=nointercept,noslope=noslope)
    else:
        val,err,pt1,pt2,pf1,pf2=__linearregression(y,x,error=error,probability=probability,nointercept=nointercept,noslope=noslope)
    if yisV:
        if noslope is None or noslope==0:
            val[0]=cdms2.createVariable(val[0],axes=axs,id='slope',copy=0)
        if nointercept is None or nointercept==0:
            val[-1]=cdms2.createVariable(val[-1],axes=axs,id='intercept',copy=0)
        if 'units' in yatt.keys():
            for v in val:
                v.units=yatt['units']+' per '+ax2.units
    if error is None or error==0:
        if len(val)>1:
            return val
        else:
            return val[0]
    elif probability is None or probability==0:
        if yisV:
            if noslope is None or noslope==0:
                err[0]=cdms2.createVariable(err[0],axes=axs,id='standard_error',copy=0)
                if error==1:
                    setattr(err[0],'long_name','standard error for regression coefficient')
                elif error==2:
                    setattr(err[0],'long_name','standard error for regression coefficient adjusted with residual (using centered autocorrelation)')
                elif error==3:
                    setattr(err[0],'long_name','standard error for regression coefficient adjusted with y (using centered autocorrelation)')
  
            if nointercept is None or nointercept==0:
                err[-1]=cdms2.createVariable(err[-1],axes=axs,id='standard_error',copy=0)
                if error==1:
                    setattr(err[-1],'long_name','standard error for regression constant')
                elif error==2:
                    setattr(err[-1],'long_name','standard error for regression constant adjusted with residual (using centered autocorrelation)')
                elif error==3:
                    setattr(err[-1],'long_name','standard error for regression constant adjusted with y (using centered autocorrelation)')
            if 'units' in yatt.keys():
                for e in err:
                    e.units=yatt['units']+' per '+ax2.units
        if len(val)>1:
            return val,err
        else:
            return val[0],err[0]
    else:
        if yisV:
            if noslope is None or noslope==0:
                err[0]=cdms2.createVariable(err[0],axes=axs,id='standard_error',copy=0)
                if error==1:
                    setattr(err[0],'long_name','standard error for regression coefficient')
                elif error==2:
                    setattr(err[0],'long_name','standard error for regression coefficient adjusted with residual (using centered autocorrelation)')
                elif error==3:
                    setattr(err[0],'long_name','standard error for regression coefficient adjusted with y (using centered autocorrelation)')
  
            if nointercept is None or nointercept==0:
                err[-1]=cdms2.createVariable(err[-1],axes=axs,id='standard_error',copy=0)
                if error==1:
                    setattr(err[-1],'long_name','standard error for regression constant')
                elif error==2:
                    setattr(err[-1],'long_name','standard error for regression constant adjusted with residual (using centered autocorrelation)')
                elif error==3:
                    setattr(err[-1],'long_name','standard error for regression constant adjusted with y (using centered autocorrelation)')
            if 'units' in yatt.keys():
                for e in err:
                    e.units=yatt['units']+' per '+ax2.units
            if noslope is None or noslope==0:
                if error>1:
                    pt1[0]=cdms2.createVariable(pt1[0],axes=axs,id='p-value',copy=0)
                    pt1[0].units='-'
                    pt1[0].long_name='p-value for regression coefficient t-value. Effective sample size adjustment for standard error (seb).'
                    pt2[0]=cdms2.createVariable(pt2[0],axes=axs,id='p-value',copy=0)
                    pt2[0].units='-'
                    pt2[0].long_name='p-value for regression coefficient t-value. Effective sample size adjustment for standard error (seb) and critical t-value.'
                else:
                    pt1[0]=cdms2.createVariable(pt1[0],axes=axs,id='p-value',copy=0)
                    pt1[0].units='-'
                    pt1[0].long_name='p-value for regression coefficient t-value. No adjustment for standard error or critical t-value.'
                pf1[0]=cdms2.createVariable(pf1[0],axes=axs,id='p-value',copy=0)
                pf1[0].unit='-'
                pf1[0].long_name='p-value for regression coefficient F-value (one-tailed)'
                pf2[0]=cdms2.createVariable(pf2[0],axes=axs,id='p-value',copy=0)
                pf2[0].unit='-'
                pf2[0].long_name='p-value for regression coefficient F-value (two-tailed)'
            if nointercept is None or nointercept==0:
                if error>1:
                    pt1[-1]=cdms2.createVariable(pt1[-1],axes=axs,id='p-value',copy=0)
                    pt1[-1].units='-'
                    pt1[-1].long_name='p-value for regression coefficient t-value. Effective sample size adjustment for standard error (seb).'
                    pt2[-1]=cdms2.createVariable(pt2[-1],axes=axs,id='p-value',copy=0)
                    pt2[-1].units='-'
                    pt2[-1].long_name='p-value for regression coefficient t-value. Effective sample size adjustment for standard error (seb) and critical t-value.'
                else:
                    pt1[-1]=cdms2.createVariable(pt1[-1],axes=axs,id='p-value',copy=0)
                    pt1[-1].units='-'
                    pt1[-1].long_name='p-value for regression coefficient t-value. No adjustment for standard error or critical t-value.'
                pf1[-1]=cdms2.createVariable(pf1[-1],axes=axs,id='p-value',copy=0)
                pf1[-1].unit='-'
                pf1[-1].long_name='p-value for regression coefficient F-value (one-tailed)'
                pf2[-1]=cdms2.createVariable(pf2[-1],axes=axs,id='p-value',copy=0)
                pf2[-1].unit='-'
                pf2[-1].long_name='p-value for regression coefficient F-value (two-tailed)'
        if len(val)>1:
            return val,err,pt1,pt2,pf1,pf2
        else:
            return val[0],err[0],pt1[0],pt2[0],pf1[0],pf2[0]
        
def geometricmean(x,axis=0,max_pct_missing=100.):
    """
    Function: geometricmean
     
    Description of function:
        Returns the geometric mean over a specified axis.

    Usage:
        result = geometricmean(x, axis=axisoptions)
    Options:
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.    
        max_pct_missingoptions
            default value = 100. Maximum fraction of cell which is allowed to be masked (missing).
            Set to a percentage between 0 and 100%.
            Set to 0. to mask results if any data is masked.
            Set to 100. to calculate result if any data is not masked
    """
    if cdms2.isVariable(x) : xatt=x.attributes
    x,dum,weights,axis,ax=__checker(x,None,None,axis)
    gmean=__geometricmean(x)
    gmean=_treat_missing(gmean,x,max_pct_missing=max_pct_missing)
    if not ax is None:
        gmean=cdms2.createVariable(gmean,axes=ax,id='geometric_mean',copy=0)
        if 'units' in xatt.keys() : gmean.units=xatt['units']
    return gmean

def _percentiles(out,percent):
    if cdms2.isVariable(out):
        out=MV2.sort(out,axis=0).asma()
        ns=MV2.count(out,axis=0).asma()
    else:
        out=numpy.ma.sort(out,axis=0)
        ns=numpy.ma.count(out,axis=0)

    output=None
    for p in percent:
        i=numpy.floor((p/100.)*(ns-1))
        try:
            i=i.astype(numpy.int)
        except:
            i=int(i)
        ii = i + 1
        tmp=(100.*i)/(ns-1)
        try:
            tmp=tmp.filled(1.E20)
        except:
            pass
        Ai=numpy.where(numpy.equal(ns,1), 0., tmp)
        tmp=(100.*ii)/(ns-1)
        try:
            tmp=tmp.filled(1.E20)
        except:
            pass
        Aii=numpy.where(numpy.equal(ns,1), 100., tmp)
        ii=numpy.where(numpy.equal(ii,ns),ns-1,ii)
        if numpy.rank(ii)>0:
            ii=ii.astype(numpy.int)
##         tmp = (p-Ai)/(Aii-Ai)*array_indexing.extract(out,ii) + \
##              (Aii-p)/(Aii-Ai)*array_indexing.extract(out,i)

        tmp = (p-Ai)/(Aii-Ai)*arrayindexing.get(out,ii) + \
             (Aii-p)/(Aii-Ai)*arrayindexing.get(out,i)
        try:
            sh=list(tmp.shape)
            sh.insert(0,1)
            tmp=numpy.ma.reshape(tmp,sh)
        except:
            pass
        if output is None:
            output=numpy.ma.array(tmp)
            output=output.astype(out.dtype.char)
        else:
            output=numpy.ma.concatenate((output,tmp.astype(numpy.float32)),0)
    return output

def percentiles(x,percentiles=[50.],axis=0):
    """
    Function: percentiles

    Description of function:
        Returns values at the defined percentiles for an array.
        
    Usage:
        result = percentiles(x, percentiles=percentilesoptions, axis=axisoptions)

    Options:
        percentilesoptions A python list of values
              Default = [50.] (the 50th percentile i.e the median value)             
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.    
    """
    if cdms2.isVariable(x) : xatt=x.attributes
    x,dum,weights,axis,ax=__checker(x,None,None,axis)
    p=_percentiles(x,percentiles)
    if not ax is None and ax != []:
        pax=cdms2.createAxis(percentiles)
        pax.id='percentiles'
        pax.units='%'
        ax.insert(0,pax)
        p=MV2.array(p)
        p=cdms2.createVariable(p,axes=ax,id='percentiles',copy=0)
        if 'units' in xatt.keys() : p.units=xatt['units']
    return p

def median(x,axis=0):
    """
    Function: median

    Description of function:
        Returns the median value of an array.
        
    Usage:
        result = median(x, axis=axisoptions)
        
    Options:
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.            
    """
    tmp=percentiles(x,percentiles=[50.],axis=axis)
    tmp.id='median'
    return tmp
    

def rank(x,axis=0):
    """
    Function: rank

    Description of function:
        Returns the rank of each element along the specified axis
        where 0 is lowest value, 100 is maximum value. Handles missing values correctly
        
    Usage:
        result = median(x, axis=axisoptions)
        
    Options:
        axisoptions 'x' | 'y' | 'z' | 't' | '(dimension_name)' | 0 | 1 ... | n 
            default value = 0. You can pass the name of the dimension or index
            (integer value 0...n) over which you want to compute the statistic.            
    """

    # preprocessing
    if cdms2.isVariable(x) :
        xatt=x.attributes
        axs=x.getAxisList()
        o=x.getOrder(ids=1)
    x,dum,weights,axis,ax=__checker(x,None,None,axis)

    # First figures out indices to sort
    a0=numpy.ma.array(numpy.ma.argsort(x.filled(1.E20),axis=0),dtype='i')
    n=a0.shape[0]

    # initialize output array
    b=numpy.ma.zeros(a0.shape,dtype='f')

    # Get the indices
##     print 'Indices are:',a0,x
    ## Make sure b and a0 are of the right type
    b=array_indexing.rank(b,a0)
    m=x.mask
    if not m is numpy.ma.nomask:
        b=MV2.masked_where(m,b)
    else:
        b=MV2.array(b)
    n=MV2.count(b,axis=0)
    n.setAxisList(b.getAxisList()[1:])
    b,n=grower(b,n)
    b=100.*b/(n-1)

##     print 'Axis:',axis
    # Now reorders everything
    if not ax is None:
        # First set the unchanged axes
        sh=[]
        for i in range(len(ax)):
            sh.append(len(ax[i]))
        # Now figures the other axes to add
        for i in range(len(axis)):
            sh.insert(i,len(axs[axis[i]]))
        b=MV2.reshape(b,sh)
        for i in range(len(ax)):
            b.setAxis(i+len(axis),ax[i])
        for i in range(len(axis)):
            b.setAxis(i,axs[axis[i]])
        b=b(order=o)
        for a in xatt.keys():
            if a[0]!='_':
                setattr(b,a,xatt[a])
        b.units='%'
    elif len(axis)==1:
        sh=range(b.rank())
        sh[0]=axis[0]
        sh[axis[0]]=0
        b=numpy.ma.transpose(b,sh)
    return b
