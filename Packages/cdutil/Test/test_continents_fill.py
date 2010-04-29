# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2,vcs,cdutil.continent_fill,sys,os
import vcs.test.support
bg= vcs.test.support
f = cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f('clt')
x=vcs.init()
#x.ratio=0
t=x.createtemplate('new')
t.reset('x',.2,.8,t.data.x1,t.data.x2)
t.reset('y',.2,.8,t.data.y1,t.data.y2)

b=x.createboxfill('new')
b.datawc_x1=-160
b.datawc_x2=-70
b.datawc_y1=20
b.datawc_y2=70
b.projection='lambert'
x.plot(s,b,t,continents=0,bg=bg)
vcs.test.support.check_plot(x)
 

if '--extended' not in sys.argv:
     print '\n************* PARTIAL TEST *****************'
     print 'FOR COMPLETE TEST OF THIS MODULE USE '
     print '   --extended'
     print '************* PARTIAL TEST *****************\n'
     sys.exit()


gc=cdutil.continent_fill.Gcf()
gc.datawc_x1=b.datawc_x1
gc.datawc_x2=b.datawc_x2
gc.datawc_y1=b.datawc_y1
gc.datawc_y2=b.datawc_y2
gc.projection=b.projection
gc.fill='y'
gc.line='y'
gc.plot(x=x,template=t,bg=bg)
vcs.test.support.check_plot(x)

