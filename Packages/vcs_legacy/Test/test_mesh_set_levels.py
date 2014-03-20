# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs_legacy,cdms2 as cdms,support
bg=support.bg


f=cdms.open("test_mesh.nc")
data = f("variable_227")
mesh = f("variable_226")
x=vcs_legacy.init()

elts = x.listelements("meshfill")

m=x.createmeshfill("PTb_968")
elts2 = x.listelements("meshfill")
if elts2==elts:
    raise Exception,"Error, meshfill object seem to have not been created"

levels = [-1.,-.8,-.6,-.4,-.2,0,.2,.4,.6,.8,1.]

m.levels=levels

if list(m.levels)!=levels:
    raise Exception,"levels not set porperly!"

x.plot(data,mesh,m,bg=bg)
support.check_plot(x)

