# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs_legacy,cdms2 as cdms,MV2 as MV,sys,support,os
bg=support.bg
f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f('clt',slice(0,5),latitude=(0.,0.,'cob'),squeeze=1)

nt=s.shape[0]

ax=MV.arange(nt,typecode='d')
bounds=MV.zeros((nt,2),typecode='d')

for i in range(nt):
	bounds[i,0]=ax[i]
	bounds[i,1]=ax[i]+1.
ax=cdms.createAxis(ax)
s.setAxis(0,ax)
x=vcs_legacy.init()
x.plot(s,bg=bg)
support.check_plot(x)
y=vcs_legacy.init()
y.plot(s,ybounds=bounds,bg=bg) # instead of being halfway, bounds are from node to node
support.check_plot(y)
