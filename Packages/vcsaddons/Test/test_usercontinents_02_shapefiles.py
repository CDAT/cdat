import vcs,vcsaddons
import cdms2,sys,os
x=vcs.init()
import vcs.test.support
bg=vcs.test.support.bg

c=vcsaddons.createusercontinents(x=x)

lon1=-122.55
lon2=-122.35
lat1=37.7
lat2=37.82

c.types = ['shapefile',]
c.sources = ['../Data/fe_2007_06075_edges',]
c.colors = [241,246,244,241]
c.widths=[1,2,1]
c.lines=['solid','solid','solid','dot']
f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f("clt",latitude=(lat1-5,lat2+5),longitude=(lon1-5,lon2+5),time=slice(0,1))
t=x.createtemplate()
iso=x.createisofill()
iso.datawc_x1=lon1
iso.datawc_x2=lon2
iso.datawc_y1=lat1
iso.datawc_y2=lat2
c.datawc_x1=lon1
c.datawc_x2=lon2
c.datawc_y1=lat1
c.datawc_y2=lat2

import numpy
lons = numpy.arange(-180,181,.05)
L={}
for l in lons:
    if l<0:
        L[l]="%.2fW" % l
    elif l>0:
        L[l]="%.2fE" % l
    else:
        L[0]="0"
L2={}
for l in lons:
    if l<0:
        L2[l]="%.2fS" % l
    elif l>0:
        L2[l]="%.2fN" % l
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

