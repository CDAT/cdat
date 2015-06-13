# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,sys,WK,cdutil
import os

f= cdms.open(os.path.join(vcs.sample_data,'clt.nc'))

s=f('clt')

W=WK.WK()

failed = False
try:
    p = W.process(s)
except:
    print 'ok did not work on boundless time dim as expected'
    failed = True
if not failed:
    raise Exception,"Error shoud have fail on boundless time dim"

cdutil.times.setTimeBoundsMonthly(s)

p = W.process(s)

s,a = W.split(p)

#W.tkbar=1
b = W.background(s,a)

print p.shape

