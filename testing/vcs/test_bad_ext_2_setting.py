import vcs
x=vcs.init()
iso=x.createisofill()
iso.levels=[[1.e20,1.e20]]
iso.ext_2="n"

x.close()
assert(iso.levels == [[1.e20,1.e20]])
