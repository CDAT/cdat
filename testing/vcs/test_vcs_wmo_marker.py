
import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

m = x.createmarker()
M=1
m.worldcoordinate=[0,M,0,M]
m.type = "w07"
m.color=[242,]
m.size=[1.,2.,5.]
m.x = [[.25,],[.5,],[.75]]
m.y = [.5,]
x.plot(m,bg=1)
fnm = 'wmo_marker.png'
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)

