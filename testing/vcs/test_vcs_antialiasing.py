
import vcs
x=vcs.init()
x.drawlogooff()
x.open()

## test it is off by default
assert(x.getantialiasing()==0)

## test we can set it
x.setantialiasing(3)
assert(x.getantialiasing()==3)
## test we can set it off
x.setantialiasing(0)
assert(x.getantialiasing()==0)
