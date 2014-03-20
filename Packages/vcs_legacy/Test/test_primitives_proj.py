#!/usr/bin/env python

import vcs_legacy,cdms2,os,sys,support
bg=support.bg
lon1=-120.
lon2=-20.
lat1=20.
lat2 = 60.
## lon1=-180.
## lon2=0.
## lat1=10.
## lat2 = 90.

# Area to draw data and primitives (primtives are smaller by 10%)
Lon1=-117.5
Lon2=-22.5
Lat1=22.5
Lat2=57.5

proj='default'
proj='polar'
proj='mollweide'
proj='robinson'
proj='mercator'
proj='lambert'
x=vcs_legacy.init()

t=x.createtemplate('new')
t.scale(.9)
#t.data.list()


fi=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','tas_cru_1979.nc'))
s=fi('tas',slice(0,1),squeeze=1,longitude=(Lon1,Lon2),latitude=(Lat1,Lat2))
isof=x.createisofill('new')
isof.projection=proj
isof.datawc_x1=lon1
isof.datawc_x2=lon2
isof.datawc_y1=lat1
isof.datawc_y2=lat2
x.plot(s,isof,t,bg=bg)
support.check_plot(x)
#print '-------------------------------------------------------------'
f=x.createfillarea('new')
f.projection=proj
f.worldcoordinate=(lon1,lon2,lat1,lat2)
f.priority=2
f.viewport=(t.data.x1,t.data.x2,t.data.y1,t.data.y2)
f.x=[Lon1*.9,Lon2*1.1,Lon2*1.1,Lon1*.9]
f.y=[Lat1*1.1,Lat1*1.1,Lat2*.9,Lat2*.9]
f.color=[242]
x.plot(f,bg=bg)
support.check_plot(x)

if not '--extended' in sys.argv:
   print '\n************* PARTIAL TEST *****************'
   print 'FOR COMPLETE TEST OF THIS MODULE USE '
   print '   -F (--full) or -E (--extended) option'
   print '************* PARTIAL TEST *****************\n'
   sys.exit()

#print '-------------------------------------------------------------'
l=x.createline('new')
l.projection=proj
l.worldcoordinate=f.worldcoordinate
l.priority=3
l.viewport=f.viewport
l.x=[Lon1*.9,Lon2*1.1,Lon2*1.1,Lon1*.9]
l.y=[Lat1*1.1,Lat1*1.1,Lat2*.9,Lat2*.9]
l.color=[241]
x.plot(l,bg=bg)
support.check_plot(x)
#print '-------------------------------------------------------------'
m=x.createmarker('new')
m.worldcoordinate=f.worldcoordinate
m.viewport=f.viewport
m.x=[[Lon1*.9]]
m.y=[[Lat1*1.1]]
m.type=['cross']
m.size=20
m.color=243
m.projection=proj
x.plot(m,bg=bg)
support.check_plot(x)
#print '-------------------------------------------------------------'
t=x.createtext('new','default','new')
t.worldcoordinate=f.worldcoordinate
t.viewport=f.viewport
t.x=[Lon1*.9]
t.y=[Lat1*1.1]
t.string=['Test Text Here']
t.height=10
t.halign='center'
t.projection=proj
## t.list()
x.plot(t,bg=bg)
support.check_plot(x)
#print '-------------------------------------------------------------'



