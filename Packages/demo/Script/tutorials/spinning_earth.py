#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
This example shows how to draw a spinning animated Earth (gif)
TECHNIQUES DEMONSTRATED: ANIMATED GIF, PROJECTIONS
MODULE USED: cdms,vcs,cdutil
"""

import os, sys
import cdms2 as cdms,vcs

f=cdms.open(os.path.join(sys.prefix, 'sample_data/tas_mo.nc'))
f1=cdms.open(os.path.join(sys.prefix, 'sample_data/clt.nc'))

## Retrieve 2 years
s=f('tas',time=('1980','1982','co'),longitude=(-180,180))

## Retrieve wind data
u=f1('u',plev=200,longitude=(-180,180))
v=f1('v',plev1=200,longitude=(-180,180))

## Reset the time bounds
import cdutil
cdutil.times.setTimeBoundsMonthly(s)

## Computes departures from annual cycle
s=cdutil.times.ANNUALCYCLE.departures(s)

## Initializes VCS
x=vcs.init()
y=vcs.init()

## Create a new template and scale it
t=x.createtemplate('new')
t.scale(.56,axis='x',font=1)

## Create isofill graphic method
iso=x.createisofill('new')

## Create vector graphic method
vc=y.createvector('new')
vc=x.createvector('vector')


## Adjust levels and colors
levs=[-1.E20,-15,-12,-10,-8,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,8,10,12,15,1.e20]
cols=vcs.getcolors(levs,colors=range(50,240),split=1)
iso.levels=levs
iso.fillareacolors=cols
vc.linecolor=3
 
## Create a new projection of type orthographic
p=x.createprojection('new')
p.type='orthographic'
p.centerlatitude=60

## Assign it to isfill and vector graphic methods
iso.projection=p
vc.projection=p

## Documentation for projection
print p.__doc__
print '\n\nTHIS DEMO MAY TAKE UP TO 12 MINUTES TO RUN DEPENDING ON YOUR SYSTEM\n\n'

## Set the Ticks
lons={-180:'',-150:'',-120:'',-90:'',-60:'',-30:'',0:'',180:'',150:'',120:'',90:'',60:'',30:''}  
lon1=-180.
iso.xticlabels1=lons
iso.xticlabels2=lons

for i in range(12): ## does one time step
    # Sets the longitude to watch
    lon1=(lon1+180)%360-180
    print 'Slice : ',i,'of 12', lon1
    p.centerlongitude=lon1
    ## retrieve the correct slice
    s2=s(time=slice(i,i+1))
    u2=u()
    v2=v()
    x.plot(s2,t,iso,bg=1)
    u2.long_name = 'deg C          Air Temperature and Wind Speed        ua,va'
    x.plot(u2,v2,t,vc,bg=1)
    lon1-=30
    a='a'
    if i==0:
        a='r' # First time create a new gif
    x.gif('spinning_earth',merge=a)
    x.clear()
    
    


