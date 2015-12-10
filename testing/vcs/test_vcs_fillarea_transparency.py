import vcs
import sys,os
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1090,units="pixels")

fa1 = x.createfillarea()

fa1.x=[.2,.8,.8,.2]
fa1.y = [.2,.2,.8,.8]
fa1.color = 242

fa2=x.createfillarea(source = fa1)

fa2.x = [.1,.9,.9,.1]
fa2.y = [.1,.1,.9,.9]

cmap = x.createcolormap()
cmap.setcolorcell(242,0,0,100,50)

fa2.colormap = cmap

x.plot(fa1,bg=True)
x.plot(fa2,bg=True)

fnm = os.path.split(__file__[:-2]+"png")[-1]
x.png(fnm)
src = sys.argv[1]

ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
