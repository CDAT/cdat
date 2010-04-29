import vcs,vcsaddons
import cdms2,sys,os
x=vcs.init()
import vcs.test.support
bg=vcs.test.support.bg
c=vcsaddons.createusercontinents(x=x)

lon1=-125
lon2=-75.
lat1=20.
lat2=55.

lon1=-10
lon2=25
lat1=35
lat2=60
c.types = ['ngdc','ngdc','ngdc','ngdc']
c.sources = ['WCL (World Coast Line) (designed for 1:5,000,000)','International Boundaries Only from WDBII','Rivers from WDBII','Internal Boundaries Only from WDBII']
c.colors = [241,241,244,241]
c.widths=[2,2,1]
c.lines=['solid','solid','solid','dot']
f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f("clt",latitude=(lat1,lat2),longitude=(lon1,lon2),time=slice(0,1))
t=x.createtemplate()
iso=x.createisofill()
lons = range(-180,181,5)
L={}
for l in lons:
    if l<0:
        L[int(l)]="%iW" % int(l)
    elif l>0:
        L[int(l)]="%iE" % int(l)
    else:
        L[0]="0"
L2={}
for l in lons:
    if l<0:
        L2[int(l)]="%iS" % int(l)
    elif l>0:
        L2[int(l)]="%iN" % int(l)
    else:
        L2[0]="Eq"

iso.xticlabels1=L
iso.xticlabels2=L
iso.yticlabels1=L2
iso.yticlabels2=L2
c.xticlabels1=L
c.xticlabels2=L
c.yticlabels1=L2
c.yticlabels2=L2
x.plot(s,t,iso,continents=0,ratio='autot',bg=bg)
vcs.test.support.check_plot(x)
x.plot(s,c,t,ratio='autot',bg=bg)
vcs.test.support.check_plot(x)

