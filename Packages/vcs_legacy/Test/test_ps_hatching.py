# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,vcs_legacy,sys,time,support,os
bg=support.bg

x=vcs_legacy.init()
x.portrait()
#x.setdefaultfont(2)
f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f('clt')
iso = x.createisofill('my')

levs = range(0,95,5)
#print len(levs)
colors = vcs_legacy.getcolors(levs)
hatch = []

iso.levels=levs
iso.fillareacolors=colors
iso.fillareastyle='pattern'
iso.fillareastyle='hatch'
iso.fillareaindices=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]
#print iso.fillareaindices
#iso.fillareaindices=[17,]*21
#print colors
#iso.list()
l = x.createline('my')
l.x=[.001,.999,.999,.001,.001]
l.y=[.001,.001,.999,.999,.001]
x.plot(l,bg=bg)
support.check_plot(x)
x.plot(s,iso,bg=bg)
support.check_plot(x)
