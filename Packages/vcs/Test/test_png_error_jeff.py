import vcs
import cdms2

f=cdms2.open("debug_out0.nc")
s=f("variable_134")
x=vcs.init()
x.plot(s)
x.png("debug")
raw_input("ok?")
