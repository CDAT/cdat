# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2
import sys
import WK
import cdutil
import os
import cdat_info

f = cdms2.open(os.path.join(cdat_info.get_sampledata_path(), 'clt.nc'))

s = f('clt')

W = WK.WK()

failed = False
try:
    p = W.process(s)
except:
    print 'ok did not work on boundless time dim as expected'
    failed = True
if not failed:
    raise Exception("Error shoud have fail on boundless time dim")

cdutil.times.setTimeBoundsMonthly(s)

p = W.process(s)

s, a = W.split(p)

# W.tkbar=1
b = W.background(s, a)

print p.shape
