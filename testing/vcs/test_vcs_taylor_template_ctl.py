
import sys, os, vcs, MV2
import vcs.testing.regression as regression

x = regression.init()

## Create a template from the default taylor diagram
t=x.createtemplate('mytaylor','deftaylor')

## Create a taylordiagrma graphic method
td=x.createtaylordiagram('my')

x.portrait()

## Create a line which we will make dotted and grey
gdl=x.createline('gdl')
gdl.color=["grey",]
gdl.type='dot'
## Create a line which we will make grey
gl=x.createline('gl')
gl.color=["grey",]
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
t.xtic2.y1=t.data.y1
t.xtic2.y2=t.data.y2
t.xtic2.line='gl'
# Same thing the mintic circles
t.xmintic2.y1=t.data.y1
t.xmintic2.y2=t.data.y2
t.xmintic2.line='gdl'

# turn the circles on
t.xtic2.priority=1
t.xmintic2.priority=1

# Create some dummy data for display purposes
data=MV2.array([[1.52,.52,],[.83,.84]])

x.plot(data, t, td, bg=1)
regression.run(x, "test_vcs_taylor_template_ctl.png")
