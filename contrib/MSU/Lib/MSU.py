# Adapted for numpy/ma/cdms2 by convertcdms.py
import eqmsu,MV2,numpy

def msu(array,weights,max_pct_missing=50.):
    """
     This subroutine processes temperature data at discrete pressure levels,
     and computes from this data an "equivalent MSU" temperature. The MSU
     weighting function is assumed to be in pressure coordinates (with 5 mb
     vertical resolution). 

    Usage
    msu=MSU.msu(array,weights,max_pct_missing)
    Where:
       array                 Input temperature data
       weights               Input MSU weighting function    
       max_pct_missing       Maximum amount of missing weights in % (default 50)
    
    """
    inorder=array.getOrder()
    axes=array.getAxisList()
    
    if not 'z' in inorder:
        raise 'error you must have a level axis on your input array'
    order=weights.getOrder()

    levs=array.getLevel()[:].astype('f')

    if not 0.<=max_pct_missing<=100.:
        raise 'Error max_pct_missing must be between 0 and 100'

    if not 'z' in order:
        raise 'error you must have a level axis on your weights array'

    if not array.getAxis(-1).isLevel():
        array=array(order='...z')

    if levs[0]>levs[1]:
        array=array[...,::-1]
        levs=array.getLevel()[:].astype('f')

    if not weights.rank()==2:
        raise 'Error weights must be 2D (msu_lev and channels)'

    if not weights.getAxis(-1).isLevel():
        weights=weights(order='...z')

    ch_levs=weights.getAxis(0)
    msu_levs=weights.getLevel()[:].astype('f')

    sh=array.shape
    n=1
    for i in sh[:-1]:
        n*=i
    array=MV2.reshape(array,(n,array.shape[-1]))

    array=array.filled(1.e20)
    weights=weights.filled()
    if array.dtype.char!='f':
        array=array.astype('f')
    if weights.dtype.char!='f':
        weights=weights.astype('f')

##     print msu_levs.shape,levs.shape,max_pct_missing,array.shape,weights.shape
    tmp1 = numpy.zeros(levs.shape,'f')
    tmp2 = numpy.zeros(levs.shape,'f')
    tmp3 = numpy.zeros(msu_levs.shape,'f')
    out = eqmsu.loop_thru_dims( (1.-max_pct_missing/100.), numpy.transpose(array), numpy.transpose(weights), levs, msu_levs, tmp1, tmp2, tmp3)
    out = numpy.transpose(out)
    
    ax=[]
    sh=[]
    for x in axes:
        if not x.isLevel():
            sh.append(len(x))
            ax.append(x)
    ax.append(ch_levs)
    sh.append(len(ch_levs))
    out=MV2.masked_equal(out,1.e20)
    out=MV2.reshape(out,sh)
    out.id='equiv_msu'
    out.setAxisList(ax)
    return out
