#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

"""This is a very complicated example that shows MOST of the options and power of VariablesMatcher.
Once again we retrieve NCEP and ECMWF (for 1981), but this time, they are both masked for land first.
ECMWF is then regridded to a T63 grid and NCEP to a T42 grid. There they are masked where the temperatures are less than 280K or more than 300K (in two different ways).
The JONES external variable is then applied for additional external masking (with a personal land mask).
Finally, everything is put on the 10x10 grid and masked for land.
Also a 'selector' for Northern Hemisphere is applied (see cdutil.region documentation)
"""

import cdutil, MV2 as MV, os,sys
# First let's create the mask (it is the same for NCEP and ECMWF since they are on the same grid)
refmsk = os.path.join(sys.prefix,'sample_data','sftlf_dnm.nc')
M=cdutil.WeightsMaker(refmsk, var='sftlf_dnm', values=[1.])

# Reference
ref = os.path.join(sys.prefix,'sample_data','tas_dnm-95a.xml')
Ref=cdutil.VariableConditioner(ref, weightsMaker=M)
Ref.var='tas'
Ref.id='ECMWF'
Ref.cdmsKeywords={'time':('1979','1980','co')}

# Ok now the final grid for this variable is a T63, masked where temperatures are not between 280K and 300K
ECMWFGrid=cdutil.WeightedGridMaker(source=refmsk,var='sftlf_dnm')
ECMWFinalMask=cdutil.WeightsMaker()
ECMWFinalMask.values=[('input',280.),('input',300.)]
ECMWFinalMask.actions=[MV.less, MV.greater]

# Associate the mask with the grid
ECMWFGrid.weightsMaker=ECMWFinalMask
# Now associates the grid with the variable.
Ref.weightedGridMaker=ECMWFGrid

# Test
tstmsk = os.path.join(sys.prefix,'sample_data','sftlf_ccsr.nc')
M=cdutil.WeightsMaker(tstmsk, var='sftlf_ccsr', values=[1.])
tst = os.path.join(cdutil.__path__[0],'..','..','..','..','sample_data','tas_ccsr-95a.xml')
Tst=cdutil.VariableConditioner(tst, weightsMaker=M)
Tst.var='tas'
Tst.id='NCEP'

# Ok now the final grid for this variable is a T42, masked where temperatures are not between 280K and 300K
NCEPGrid=cdutil.WeightedGridMaker()
NCEPGrid.latitude.n=64
NCEPGrid.latitude.type='gaussian'

# Ok now let's create a function to return the mask
def myMakeMask(array, range):
    """Returns the input array masked where the values are not between range[0] and range[1]"""
    m1=MV.less (array, range[0]) # mask where it is less than the 1st value
    m2=MV.greater(array, range[1]) # mask where it is more than the 2nd value
    return MV.logical_or(m1,m2)

# And associate the mask with the grid
NCEPGrid.weightsMaker.values=[('input',(280.,300.))]
NCEPGrid.weightsMaker.actions=[myMakeMask]

# Now associates the grid with the variable.
Tst.weightedGridMaker=NCEPGrid

# External Variable
ext = ref
extmask = refmsk
EMask=cdutil.WeightsMaker(source=extmask, var='sftlf_dnm')
ED=cdutil.VariableConditioner(ext, weightsMaker=EMask)
ED.var='tas'
ED.id='JONES'

# Final Grid
# We need a mask for the final grid
fgmask=os.path.join(sys.prefix,'sample_data','sftlf_10x10.nc')
M2=cdutil.WeightsMaker(source=fgmask, var='sftlf', values=[100.])
FG=cdutil.WeightedGridMaker(weightsMaker=M2)
FG.longitude.n=36
FG.longitude.first=0.
FG.longitude.delta=10.
FG.latitude.n=18
FG.latitude.first=-85.
FG.latitude.delta=10.

# Now creates the compare object
c=cdutil.VariablesMatcher(Ref, Tst, weightedGridMaker=FG, externalVariableConditioner=ED)
c.cdmsArguments=[cdutil.region.NH]
#print c
# And gets it
(ref, reffrc), (test, tfrc) = c()
print 'Shapes:', test.shape, ref.shape

