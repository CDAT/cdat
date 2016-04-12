
import vcs

b=vcs.createboxfill()
x=vcs.init()
x.drawlogooff()
b.colormap = "rainbow"
x.setcolormap("rainbow")
assert(x.colormap=="rainbow")
assert(x.getcolormapname()=="rainbow")
assert(x.getcolormap().name=="default")
assert(x.getcolorcell(16)==[55.,6.,98.,100.])
assert(vcs.getcolorcell(16,x)==[55.,6.,98.,100.])
assert(vcs.getcolorcell(16,b)==[55,6.,98.,100.])
vcs.setcolorcell("rainbow",16,100,100,100)
assert(x.getcolorcell(16)==[100.,100.,100.,100.])
x.setcolorcell(16,0,100,0)
assert(x.getcolorcell(16)==[0.,100.,0.,100.])
vcs.setcolorcell(b,16,100,100,100)
assert(x.getcolorcell(16)==[100.,100.,100.,100.])
