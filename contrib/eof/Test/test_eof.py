# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms, vcs, sys, os
from eof import Eof
import vcs.test.support
bg= vcs.test.support.bg

amr = [-20, 40, 60, 120]
f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
v = f.getVariable('clt')
u = v.subRegion(latitude=(amr[0], amr[1], 'cc'), longitude=(amr[2],amr[3],'cc'))
print u.shape
result = Eof(u, number_of_components=4, mean_choice=12)
principal_components = result.principal_components
print "Percent explained", result.percent_explained
x=vcs.init()
vcs.pauser.pause(3)
print len(principal_components)
for y in principal_components:
    x.isofill(y,bg=bg)
    vcs.test.support.check_plot(x)
    x.clear()
u1 = v.subRegion(latitude=(amr[0], amr[1], 'cc'), 
                 longitude=(amr[2],amr[3],'cc'), order='xyt')
result2 = Eof(u1, number_of_components=4, mean_choice=12)
print "Percent explained", result2.percent_explained







