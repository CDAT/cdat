
import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

m = x.createmarker()

m.type = ["star", "triangle_right", "triangle_left", "triangle_up", "triangle_down"]
m.x = [[.1], [.3], [.5], [.7], [.9]]
m.y = [[.1], [.3], [.5], [.7], [.9]]
m.color = [200, 150, 160, 175, 125]
m.size = [50, 50, 50, 50, 50]
x.plot(m,bg=1)
fnm = "test_star_triangle_markers.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src

ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
