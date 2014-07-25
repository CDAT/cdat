# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
This module contains functions to map an array of "regions" onto a mask
"""

import regrid2,cdutil,MV2,genutil,cdms2,sys,os
import cdat_info


def sumregions(potential_reg,potential):
    out=potential_reg[0]*1.
    wts=potential[0]*1.
    for i in range(1,potential.shape[0]):
        c=MV2.greater(potential[i]-wts,0)
        out=MV2.where(c,potential_reg[i],out)
        wts=MV2.where(c,potential[i],wts)
    return out

def loop(potential,potential_reg,c2,w3,region):
    nmax=potential.shape[0]
    c3=MV2.not_equal(w3,0.)
    c=MV2.logical_and(c2,c3)
    thisturn=MV2.ones(c.shape)
    for i in range(nmax):
        c1=MV2.logical_or(MV2.equal(potential_reg[i],region),MV2.equal(potential[i],-999))
        c2=MV2.logical_and(c,c1)
        c2=MV2.logical_and(c2,thisturn)
        potential_reg[i]=MV2.where(c2,region,potential_reg[i])
        thisturn=MV2.where(c2,0,thisturn)
        c1=MV2.logical_and(c2,MV2.equal(potential[i],-999))
        c2=MV2.logical_and(c2,MV2.not_equal(potential[i],-999))
        potential[i]=MV2.where(c1,w3,potential[i])
        potential[i]=MV2.where(c2,potential[i]+w3,potential[i])
    ## Ultimate test to see if more would be needed !
    if not MV2.allequal(MV2.logical_and(c,thisturn),0):
        raise 'OOOPS WE COULD USE MORE REGIONS BUDDY !'
    return


def generateSurfaceTypeByRegionMask(mask,sftbyrgn=None,sftbyrgnmask=215,regions = range(201,223), maximum_regions_per_cell=4,extend_up_to=3,verbose=True):
    """ Maps a "types" dataset onto a landsea mask
    Usage:
    mapped,found = generateSurfaceTypeByRegionMask(mask,sftbyrgn,sftbyrgnmask=None,regions=None,maximum_regions_per_cell=4,extend_up_to=3,verbode=True)
    Input:
    mask : land/sea mask (100/0) onto you wish to map our grid (will generate a ld/sea mask for you)
    sftbyrgn: mask you wish to map
              if None then uses our own "sftbyrgn" dataset (old ezget type)
    sftbyrgnmask: land/sea mask for sftbyrgn
                  or a number specifying limit in values of sftbygrn
                  which indicate the threshold land/sea (greater values are land)
    regions: Numbers from sftbyrgn array that you want to map onto mask
    maximum_regions_per_cell: maximum number f regions concidered in a cell
    extend_up_to : how many grid cells away around a cell can we extent to identify a guess
    verbose: prints to the screen what's going on (default is True)

    Output:
     mapped : mapped input mask
     found  : ???
    """
    cdat_info.pingPCMDIdb("cdat","cdutil.generateSurfaceTypeByRegionMask")
    ## OK first determine which regions are available
    ## Must be integer values
    if isinstance(mask, cdms2.grid.TransientRectGrid):
        mask = cdutil.generateLandSeaMask(mask)*100.

    if sftbyrgn is None:
        sftbyrgn = cdms2.open(os.path.join(sys.prefix,'share','cdutil','sftbyrgn.nc'))('sftbyrgn')
        
    if regions is None:
        if verbose: print 'Preparing regions'
##         regions = range(201,223)

        regions=[]
        for i in range(0,10000):
            genutil.statusbar(i,9999)
            c=float(MV2.sum(MV2.ravel(MV2.equal(sftbyrgn,i)),0))
            if c!=0: regions.append(i)

    if verbose: print 'Regions:',regions
    ## If no mask passed fr sftbyrgn, assumes everything greater 5000 is land)
    if isinstance(sftbyrgnmask,int):
        split = sftbyrgnmask
        n=MV2.maximum(mask)
        sftbyrgnmask=MV2.greater_equal(sftbyrgn,sftbyrgnmask)*n
    else:
        split = MV2.maximum(sftbyrgnmask)/2.
    ## Now guess the type for each regions
    keys={}
## ## Nice way to do it
##     for r in regions:
##         c=MV2.not_equal(sftbyrgn,r)
##         c=MV2.masked_where(c,sftbyrgnmask)
##         n=MV2.count(c)
##         c=float(MV2.sum(MV2.ravel(c),0)/n)
##         print r,c,n
##         keys[r]=c
## Fast but not so "general" way to do it
    for r in regions:
        if r< split:
            keys[r]=0.
        else:
            keys[r]=100.
    sh=list(mask.shape)
    sh.insert(0,maximum_regions_per_cell)
    potential=MV2.ones(sh,dtype='d')*-999
    potential_reg=MV2.ones(sh,dtype='d')*-999

    g1=sftbyrgn.getGrid()
    g2=mask.getGrid()
    r1=regrid2.Horizontal(g1,g2)
    w=cdutil.area_weights(sftbyrgn)

    if verbose: print 'First pass'
    itmp=0.
    for ireg in keys.keys():
        genutil.statusbar(itmp,len(keys.keys())-1)
        itmp+=1.
        c=MV2.equal(sftbyrgn,ireg)
        w2=1.-c*w
        s2,w3=r1(sftbyrgn,mask=w2.filled(),returnTuple=1)
        c2=MV2.equal(mask,keys[ireg])
        loop(potential,potential_reg,c2,w3,ireg)

    found=MV2.zeros(sh[1:],typecode='f')
    for i in range(maximum_regions_per_cell):
        found=found+MV2.not_equal(potential[i],-999)
    sh2=list(sh)
    for k in range(extend_up_to):
        sh2[1]=sh[1]+2*(k+1)
        sh2[2]=sh[2]+2*(k+1)
        ## Form the possible i/j couples !
        s=MV2.sum(MV2.ravel(MV2.equal(potential[0],-999)),0)
        if verbose: print 'Expanding up to',k+1,'cells while trying to fix',s,'cells'
##         if dump:
##             f=cdms2.open('tmp_'+str(k)+'.nc','w')
##             f.write(sumregions(potential_reg,potential).astype('f'),id='sftbyrgn',axes=mask.getAxisList())
##             f.close()
##         g=sumregions(potential_reg,potential).astype('d')
##         g=MV2.masked_equal(g,-999)
##         g=MV2.greater(g,4999)*100.
##         g=MV2.absolute(mask-g)
##         g=MV2.masked_equal(g,0.)
##         print 'Number of differences:',MV2.count(g)

        if float(s)!=0:
            c0=MV2.equal(potential[0],-999)
            couples=[]
            sft2=MV2.zeros(sh2[1:],dtype='d')-888.
            sft2[k+1:-k-1,k+1:-k-1]=mask
            for i in range(-k-1,k+2):
                for j in range(-k-1,k+2):
                    if abs(i)>k or abs(j)>k: couples.append([i,j])
            ntot=len(keys.keys())*len(couples)-1
            itmp=0
            for ireg in keys.keys():
                c=MV2.equal(sftbyrgn,ireg)
                w2=1.-c*w
                s2,w3=r1(sftbyrgn,mask=w2.filled(),returnTuple=1)
                w4=MV2.zeros(sh2[1:],typecode='d')
                w4[k+1:-k-1,k+1:-k-1]=w3
                for i,j in couples:
                    if verbose: genutil.statusbar(itmp,ntot)
                    itmp+=1.
                    c2=MV2.equal(sft2[j+k+1:j+k+1+sh[1],i+k+1:i+k+1+sh[2]],keys[ireg])
                    c3=MV2.equal(sft2[j+k+1:j+k+1+sh[1],i+k+1:i+k+1+sh[2]],mask)
                    c2=MV2.logical_and(c2,c3)
                    c2=MV2.logical_and(c2,c0)
                    loop(potential,potential_reg,c2,w4[j+k+1:j+k+1+sh[1],i+k+1:i+k+1+sh[2]],ireg)
           
        found=MV2.where(MV2.equal(potential[0],-999),found-1,found)

    out=sumregions(potential_reg,potential)
    out.setAxisList(mask.getAxisList())
    found.setAxisList(mask.getAxisList())
    found=found.astype('i')
    found.missing_value=-999
    found.id='found'
    out.id='sftbyrgn'
    out=out.astype('i')
    out.missing_value=-999
    del(out.name)
    del(found.name)
    return out,found
