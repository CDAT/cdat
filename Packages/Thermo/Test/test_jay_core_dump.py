# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,thermo,vcs.test.support

bg=vcs.test.support.bg

f=cdms.open('dar.meteo.mod.cam3.era.v31.h0.l3.nrstpt.cp.2000070100.2000080100.tau.12.36.nc')

s=f('ta',time=slice(0,1),squeeze=1)

th = thermo.Gth(name='my')

try:
    th.plot_TP(s,bg=bg)
    failed = False
except:
    failed = True
    pass

if not failed:
    print failed
    raise "Error should have raised an exception on all missing data!"

vcs.test.support.check_plot(th.x) # Make sure we have an empty plot!
th.clear()

s=f('ta',time=slice(1,2),squeeze=1)
th.type='stuve'
th.plot_TP(s,bg=bg)
vcs.test.support.check_plot(th.x)
th.clear()

th.type='emagram'
th.plot_TP(s,bg=bg)
vcs.test.support.check_plot(th.x)
th.clear()

th.type='tephigram'
th.plot_TP(s,bg=bg)
vcs.test.support.check_plot(th.x)
th.clear()

th.type='skewT'
th.plot_TP(s,bg=bg)
vcs.test.support.check_plot(th.x)
th.clear()

