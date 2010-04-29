# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
This script converts a land sea mask to a stbyrgn mask
Input:
Land/sea mask
Original sftbyrgn
dctionary of values in sftbyrgn/type (ld or water)

Ouptput:
Newsftbyrgn
"""

import cdms2,cdutil,MV2,os,sys
import vcs.test.support
bg= vcs.test.support

din=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..',"sample_data","clt.nc"))("clt",slice(0,1))
print 'generating mask'
sftlf = cdutil.generateLandSeaMask(din)*100.

print 'done:',sftlf.shape
newsftbyrgn,n=cdutil.generateSurfaceTypeByRegionMask(sftlf)


import vcs
x=vcs.init()
x.plot(newsftbyrgn,bg=bg)
vcs.test.support.check_plot(x)
x.clear()
x.plot(n,bg=bg)
vcs.test.support.check_plot(x)
