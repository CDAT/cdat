
import sys,os
src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
import vcs,MV2

bg=True

x=vcs.init()
x.drawlogooff()
if bg:
  x.setbgoutputdimensions(1200,1091,units="pixels")
if not bg:
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

x.plot(data,t,td,bg=bg)
fnm="test_vcs_taylor_template_ctl.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
