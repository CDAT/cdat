# Adapted for numpy/ma/cdms2 by convertcdms.py
import MV2
import genutil
import cdms2
import numpy
def reconstructPressureFromHybrid(ps,A,B,Po):
    """
    Reconstruct the Pressure field on sigma levels, from the surface pressure
    
    Input
    Ps   : Surface pressure
    A,B,Po: Hybrid Convertion Coefficients, such as: p=B.ps+A.Po
    Ps: surface pressure
    B,A are 1D : sigma levels
    Po and Ps must have same units
    
    Output
    Pressure field
    Such as P=B*Ps+A*Po

    Example
    P=reconstructPressureFromHybrid(ps,A,B,Po)
    """
    # Compute the pressure for the sigma levels
    ps,B=genutil.grower(ps,B)
    ps,A=genutil.grower(ps,A)
    p=ps*B
    p=p+A*Po
    p.setAxisList(ps.getAxisList())
    p.id='P'
    try:
      p.units=ps.units
    except:
      pass
    t=p.getTime()
    if not t is None:
      p=p(order='tz...')
    else:
     p=p(order='z...')
    return p
    
def linearInterpolation(A,I,levels=[100000, 92500, 85000, 70000, 60000, 50000, 40000, 30000, 25000, 20000, 15000, 10000, 7000, 5000, 3000, 2000, 1000], status=None):
    """
    Linear interpolation
    to interpolate a field from some levels to another set of levels
    Value below "surface" are masked
    
    Input
    A :      array to interpolate
    I :      interpolation field (usually Pressure or depth) from TOP (level 0) to BOTTOM (last level), i.e P value going up with each level
    levels : levels to interplate to (same units as I), default levels are:[100000, 92500, 85000, 70000, 60000, 50000, 40000, 30000, 25000, 20000, 15000, 10000, 7000, 5000, 3000, 2000, 1000]

    I and levels must have same units

    Output
    array on new levels (levels)
    
    Examples:
    A=interpolate(A,I,levels=[100000, 92500, 85000, 70000, 60000, 50000, 40000, 30000, 25000, 20000, 15000, 10000, 7000, 5000, 3000, 2000, 1000])
    """
    
    try:
        nlev=len(levels)  # Number of pressure levels
    except:
        nlev=1  # if only one level len(levels) would breaks
        levels=[levels,]
    order=A.getOrder()
    A=A(order='z...')
    I=I(order='z...')
    sh=list(I.shape)
    nsigma=sh[0] #number of sigma levels
    sh[0]=nlev
    t=MV2.zeros(sh,typecode=MV2.float32)
    sh2=I[0].shape
    prev=-1
    for ilev in range(nlev): # loop through pressure levels
        if status is not None:
            prev=genutil.statusbar(ilev,nlev-1.,prev)
        lev=levels[ilev] # get value for the level
        Iabv=MV2.ones(sh2,MV2.float)
        Aabv=-1*Iabv # Array on sigma level Above
        Abel=-1*Iabv # Array on sigma level Below
        Ibel=-1*Iabv # Pressure on sigma level Below
        Iabv=-1*Iabv # Pressure on sigma level Above
        Ieq=MV2.masked_equal(Iabv,-1) # Area where Pressure == levels
        for i in range(1,nsigma): # loop from second sigma level to last one
            a = MV2.greater_equal(I[i],  lev) # Where is the pressure greater than lev
            b =    MV2.less_equal(I[i-1],lev) # Where is the pressure less than lev
            # Now looks if the pressure level is in between the 2 sigma levels
            # If yes, sets Iabv, Ibel and Aabv, Abel
            a=MV2.logical_and(a,b)
            Iabv=MV2.where(a,I[i],Iabv) # Pressure on sigma level Above
            Aabv=MV2.where(a,A[i],Aabv) # Array on sigma level Above
            Ibel=MV2.where(a,I[i-1],Ibel) # Pressure on sigma level Below
            Abel=MV2.where(a,A[i-1],Abel) # Array on sigma level Below
            Ieq= MV2.where(MV2.equal(I[i],lev),A[i],Ieq)

        val=MV2.masked_where(MV2.equal(Ibel,-1.),numpy.ones(Ibel.shape)*lev) # set to missing value if no data below lev if there is
        
        tl=(val-Ibel)/(Iabv-Ibel)*(Aabv-Abel)+Abel # Interpolation
        if ((Ieq.mask is None) or (Ieq.mask is MV22.nomask)):
            tl=Ieq
        else:
            tl=MV2.where(1-Ieq.mask,Ieq,tl)
        t[ilev]=tl.astype(MV2.float32)

    ax=A.getAxisList()
    autobnds=cdms2.getAutoBounds()
    cdms2.setAutoBounds('off')
    lvl=cdms2.createAxis(MV2.array(levels).filled())
    cdms2.setAutoBounds(autobnds)
    try:
        lvl.units=I.units
    except:
        pass
    lvl.id='plev'
    
    try:
      t.units=I.units
    except:
      pass
  
    ax[0]=lvl
    t.setAxisList(ax)
    t.id=A.id
    for att in A.listattributes():
        setattr(t,att,getattr(A,att))
    return t(order=order)

def logLinearInterpolation(A,P,levels=[100000, 92500, 85000, 70000, 60000, 50000, 40000, 30000, 25000, 20000, 15000, 10000, 7000, 5000, 3000, 2000, 1000],status=None):
    """
    Log-linear interpolation
    to convert a field from sigma levels to pressure levels
    Value below surface are masked
    
    Input
    A :    array on sigma levels
    P :    pressure field from TOP (level 0) to BOTTOM (last level)
    levels : pressure levels to interplate to (same units as P), default levels are:[100000, 92500, 85000, 70000, 60000, 50000, 40000, 30000, 25000, 20000, 15000, 10000, 7000, 5000, 3000, 2000, 1000]

    P and levels must have same units

    Output
    array on pressure levels (levels)
    
    Examples:
    A=logLinearInterpolation(A,P),levels=[100000, 92500, 85000, 70000, 60000, 50000, 40000, 30000, 25000, 20000, 15000, 10000, 7000, 5000, 3000, 2000, 1000])
    """
    
    try:
        nlev=len(levels)  # Number of pressure levels
    except:
        nlev=1  # if only one level len(levels) would breaks
        levels=[levels,]
    order=A.getOrder()
    A=A(order='z...')
    P=P(order='z...')
    sh=list(P.shape)
    nsigma=sh[0] #number of sigma levels
    sh[0]=nlev
    t=MV2.zeros(sh,typecode=MV2.float32)
    sh2=P[0].shape
    prev=-1
    for ilev in range(nlev): # loop through pressure levels
        if status is not None:
            prev=genutil.statusbar(ilev,nlev-1.,prev)
        lev=levels[ilev] # get value for the level
        Pabv=MV2.ones(sh2,MV2.float)
        Aabv=-1*Pabv # Array on sigma level Above
        Abel=-1*Pabv # Array on sigma level Below
        Pbel=-1*Pabv # Pressure on sigma level Below
        Pabv=-1*Pabv # Pressure on sigma level Above
        Peq=MV2.masked_equal(Pabv,-1) # Area where Pressure == levels
        for i in range(1,nsigma): # loop from second sigma level to last one
            a=MV2.greater_equal(P[i],  lev) # Where is the pressure greater than lev
            b=   MV2.less_equal(P[i-1],lev) # Where is the pressure less than lev
            # Now looks if the pressure level is in between the 2 sigma levels
            # If yes, sets Pabv, Pbel and Aabv, Abel
            a=MV2.logical_and(a,b)
            Pabv=MV2.where(a,P[i],Pabv) # Pressure on sigma level Above
            Aabv=MV2.where(a,A[i],Aabv) # Array on sigma level Above
            Pbel=MV2.where(a,P[i-1],Pbel) # Pressure on sigma level Below
            Abel=MV2.where(a,A[i-1],Abel) # Array on sigma level Below
            Peq= MV2.where(MV2.equal(P[i],lev),A[i],Peq)

        val=MV2.masked_where(MV2.equal(Pbel,-1),numpy.ones(Pbel.shape)*lev) # set to missing value if no data below lev if there is
        
        tl=MV2.log(val/Pbel)/MV2.log(Pabv/Pbel)*(Aabv-Abel)+Abel # Interpolation
        if ((Peq.mask is None) or (Peq.mask is MV2.nomask)):
            tl=Peq
        else:
            tl=MV2.where(1-Peq.mask,Peq,tl)
        t[ilev]=tl.astype(MV2.float32)
        
    ax=A.getAxisList()
    autobnds=cdms2.getAutoBounds()
    cdms2.setAutoBounds('off')
    lvl=cdms2.createAxis(MV2.array(levels).filled())
    cdms2.setAutoBounds(autobnds)
    try:
        lvl.units=P.units
    except:
        pass
    lvl.id='plev'
    
    try:
      t.units=P.units
    except:
      pass
  
    ax[0]=lvl
    t.setAxisList(ax)
    t.id=A.id
    for att in A.listattributes():
        setattr(t,att,getattr(A,att))
    return t(order=order)
    
sigma2Pressure=logLinearInterpolation
