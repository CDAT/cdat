import cdms2
f=cdms2.open("/Users/doutriaux1/Dean/clt.nc")
s=f("clt",slice(0,13))
import vcs
x=vcs.init()
x.plot(s)
x.animate.create()
raw_input("Creating anim")
x.animate.close()
x.clear()
x.plot(s)
x.animate.create()
raw_input("Creating anim")
x.animate.run()

raw_input("Creating anim")
