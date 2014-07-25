import cdms2,MV2,sys,os
import cdat_info

def create_surrounds(data):
    sh=list(data.shape)
    L=data.getAxis(1)
    bL=L.getBounds()
    if L.isCircular() and bL[-1][1]-bL[0][0] % L.modulo == 0:
        sh[0]=sh[0]-2
    else:
        sh[0]=sh[0]-2
        sh[1]=sh[1]-2
        
    UL=MV2.ones(sh)
    UC=MV2.ones(sh)
    UR=MV2.ones(sh)
    ML=MV2.ones(sh)
    MC=MV2.ones(sh)
    MR=MV2.ones(sh)
    LL=MV2.ones(sh)
    LC=MV2.ones(sh)
    LR=MV2.ones(sh)

    if L.isCircular() and bL[-1][1]-bL[0][0] % L.modulo == 0:
        UC[:,:]=data[2:]
    
        LC[:,:]=data[:-2]
        
        ML[:,1:]=data[1:-1,:-1]
        ML[:,0]=data[1:-1,-1]
        
        MR[:,:-1]=data[1:-1,1:]
        MR[:,-1]=data[1:-1,0]
        
        
        UL[:,1:]=data[2:,:-1]
        UL[:,0]=data[2:,-1]
        
        UR[:,:-1]=data[2:,1:]
        UR[:,-1]=data[2:,0]
        
        LL[:,1:]=data[:-2,:-1]
        LL[:,0]=data[:-2,-1]
        
        LR[:,:-1]=data[:-2,1:]
        LR[:,-1]=data[:-2,0]
    else:
        UC[:,:]=data[2:,1:-1]
    
        LC[:,:]=data[:-2,1:-1]
        ML[:,:]=data[1:-1,:-2]
        
        MR[:,:]=data[1:-1,2:]
        
        
        UL[:,:]=data[2:,:-2]
        
        UR[:,:]=data[2:,2:]
        
        LL[:,:]=data[:-2,:-2]
        
        LR[:,:]=data[:-2,2:]
        
    return UL,UC,UR,ML,MR,LL,LC,LR


def map2four(data,target,regridTool='regrid2'):
    lons=target.getLongitude()
    lats=target.getLatitude()
    lonso=cdms2.createAxis(lons[::2])
    lonse=cdms2.createAxis(lons[1::2])
    latso=cdms2.createAxis(lats[::2])
    latse=cdms2.createAxis(lats[1::2])
    
    oo=cdms2.createRectGrid(latso,lonso)
    oe=cdms2.createRectGrid(latso,lonse)
    eo=cdms2.createRectGrid(latse,lonso)
    ee=cdms2.createRectGrid(latse,lonse)
    
    doo = data.regrid(oo,regridTool=regridTool)
    doe = data.regrid(oe,regridTool=regridTool)
    deo = data.regrid(eo,regridTool=regridTool)
    dee = data.regrid(ee,regridTool=regridTool)

    out=MV2.zeros(data.shape,dtype='f')

    out[::2,::2]=doo
    out[::2,1::2]=doe
    out[1::2,::2]=deo
    out[1::2,1::2]=dee

    out.id=data.id
    out.setAxisList((lats,lons))

    return out

def improve(mask,navy_frac_t,threshold_1,threshold_2,UL,UC,UR,ML,MR,LL,LC,LR,regridTool='regrid2'):
    mask_approx = map2four(mask,mask.getGrid(),regridTool=regridTool)
    diff =  navy_frac_t - mask_approx
    ## Land point conversion
    c1 = MV2.greater(diff,threshold_1)
    c2 = MV2.greater(navy_frac_t,threshold_2)
    c= MV2.logical_and(c1,c2)
##     x.plot(c.astype("i"))
##     raw_input()
##     x.clear()
    ## Now figures out local maxima
    cUL,cUC,cUR,cML,cMR,cLL,cLC,cLR = create_surrounds(c)
    L=c.getAxis(1)
    bL=L.getBounds()
    if L.isCircular() and bL[-1][1]-bL[0][0] % L.modulo == 0:
        c=c[1:-1] # elimnitates north and south poles
        tmp = navy_frac_t[1:-1]
    else:
        c=c[1:-1,1:-1] # elimnitates north and south poles
        tmp = navy_frac_t[1:-1,1:-1]
    m = MV2.logical_and(c,MV2.greater(tmp,MV2.where(cUL,UL,0.)))
    m = MV2.logical_and(m,MV2.greater(tmp,MV2.where(cUC,UC,0.)))
    m = MV2.logical_and(m,MV2.greater(tmp,MV2.where(cUR,UR,0.)))
    m = MV2.logical_and(m,MV2.greater(tmp,MV2.where(cML,ML,0.)))
    m = MV2.logical_and(m,MV2.greater(tmp,MV2.where(cMR,MR,0.)))
    m = MV2.logical_and(m,MV2.greater(tmp,MV2.where(cLL,LL,0.)))
    m = MV2.logical_and(m,MV2.greater(tmp,MV2.where(cLC,LC,0.)))
    m = MV2.logical_and(m,MV2.greater(tmp,MV2.where(cLR,LR,0.)))
    # Ok now update the mask by setting these points to land
    mask2 = mask*1.
    if L.isCircular() and bL[-1][1]-bL[0][0] % L.modulo == 0:
        mask2[1:-1] = MV2.where(m,1,mask[1:-1])
    else:
        mask2[1:-1,1:-1] = MV2.where(m,1,mask[1:-1,1:-1])

    ## ocean point conversion
    c1 = MV2.less(diff,-threshold_1)
    c2 = MV2.less(navy_frac_t,1.-threshold_2)
    c= MV2.logical_and(c1,c2)
    cUL,cUC,cUR,cML,cMR,cLL,cLC,cLR = create_surrounds(c)
    L=c.getAxis(1)
    bL=L.getBounds()
    if L.isCircular() and bL[-1][1]-bL[0][0] % L.modulo == 0:
        c=c[1:-1] # elimnitates north and south poles
        tmp = navy_frac_t[1:-1]
    else:
        c=c[1:-1,1:-1] # elimnitates north and south poles
        tmp = navy_frac_t[1:-1,1:-1]
    ## Now figures out local maxima
    m = MV2.logical_and(c,MV2.less(tmp,MV2.where(cUL,UL,1.)))
    m = MV2.logical_and(m,MV2.less(tmp,MV2.where(cUC,UC,1.)))
    m = MV2.logical_and(m,MV2.less(tmp,MV2.where(cUR,UR,1.)))
    m = MV2.logical_and(m,MV2.less(tmp,MV2.where(cML,ML,1.)))
    m = MV2.logical_and(m,MV2.less(tmp,MV2.where(cMR,MR,1.)))
    m = MV2.logical_and(m,MV2.less(tmp,MV2.where(cLL,LL,1.)))
    m = MV2.logical_and(m,MV2.less(tmp,MV2.where(cLC,LC,1.)))
    m = MV2.logical_and(m,MV2.less(tmp,MV2.where(cLR,LR,1.)))
    # Ok now update the mask by setting these points to ocean
    if L.isCircular() and bL[-1][1]-bL[0][0] % L.modulo == 0:
        mask2[1:-1] = MV2.where(m,0,mask2[1:-1])
    else:
        mask2[1:-1,1:-1] = MV2.where(m,0,mask2[1:-1,1:-1])
    mask2.setAxisList(mask.getAxisList())
    return mask2

def generateLandSeaMask(target,source=None,threshold_1 = .2, threshold_2 = .3,regridTool='regrid2'):
    """ Generates a best guess mask on any rectilinear grid, using the method described in PCMDI's report #58
    see: http://www-pcmdi.llnl.gov/publications/ab58.html
    Input:
       target: either a MV2 object with a grid, or a cdms2 grid (rectilinear grid only)
       source: A fractional (0 to 1.) land sea mask, where 1 means all land
       threshold_1 (optional): criteria 1 for detecting cells with possible increment see report for detail
                               difference threshold
       threshold_2 (optional): criteria 2 for detecting cells with possible increment see report for detail
                               water/land content threshold
       regridTool: which cdms2 regridder tool to use, default is regrid2
    Output:
       landsea maks on target grid
    """
    cdat_info.pingPCMDIdb("cdat","cdutil.generateLandSeaMask")
    if cdms2.isVariable(target):
        target = target.getGrid()
        if target is None:
            raise Exception,"Error target data passed do not have  a grid"
    if not isinstance(target,cdms2.grid.TransientRectGrid):
        raise Exception, "Error: target grid must be rectilinear"

    if source is None:
        source = cdms2.open(os.path.join(sys.prefix,'share','cdutil','navy_land.nc'))('sftlf')
        
    try:
        navy_frac_t = source.regrid(target,regridTool='regrid2')
    except Exception,err:
        raise "error, cannot regrid source data to target, got error message: %s" % err
    
    mask = MV2.greater(navy_frac_t,.5).astype('i') # First guess, anything greater than 50% is land
    UL,UC,UR,ML,MR,LL,LC,LR = create_surrounds(navy_frac_t)
    cont = True
    i=0
    while cont:
        mask2 = improve(mask,navy_frac_t,threshold_1,threshold_2,UL,UC,UR,ML,MR,LL,LC,LR,regridTool=regridTool)
        if MV2.allequal(mask2,mask) or i>25: # shouldn't be more than 10 at max, 25 is way safe
            cont=False
        mask=mask2
        i+=1
    mask.id='sftlf'
    return mask
