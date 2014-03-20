# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs_legacy,MV2 as MV,support
bg=support.bg

x=vcs_legacy.init()
if bg==0:
    x.open()
## Create a template from the default taylor diagram
t=x.createtemplate('mytaylor','deftaylor')

## Create a taylordiagrma graphic method
td=x.createtaylordiagram('my')
x.portrait()

## Create a line which we will make dotted and grey
gdl=x.createline('gdl')
gdl.color=252
gdl.type='dot'
## Create a line which we will make grey
gl=x.createline('gl')
gl.color=252
gl.type='solid'


# Now make the template grid appear and extend thru
# the whole grid
t.ytic2.x1=t.data.x1
t.ytic2.x2=t.data.x2
t.ytic2.line='gl'

## Same for the min tics
t.ymintic2.x1=t.data.x1
t.ymintic2.x2=t.data.x2
t.ymintic2.line='gdl'

# Same thing the circles
t.xtic2.x1=t.data.x1
t.xtic2.x2=t.data.x2
t.xtic2.line='gl'
# Same thing the mintic circles
t.xmintic2.x1=t.data.x1
t.xmintic2.x2=t.data.x2
t.xmintic2.line='gdl'

# turn the circles on
t.xtic2.priority=1
t.xmintic2.priority=1

# Create some dummy data for display purposes
data=MV.array([[1.52,.52,],[.83,.84]])

x.plot(data,t,td,bg=bg)
support.check_plot(x)

