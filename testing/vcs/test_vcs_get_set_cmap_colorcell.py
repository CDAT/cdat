import vcs

x=vcs.init()

gm = x.createboxfill()
cmap = x.createcolormap()

rgb = cmap.getcolorcell(25)
assert(rgb == [0.,60.,100.,100.])

rgb = x.getcolorcell(25)
assert(rgb == [0.,60.,100.,100.])

rgb = x.getcolorcell(25,x)
assert(rgb == [0.,60.,100.,100.])

rgb = x.getcolorcell(25,gm)
assert(rgb == [0.,60.,100.,100.])

cmap.setcolorcell(25,56,23,29)
assert(cmap.index[25] == [56.,23.,29.,100.])

cmap.setcolorcell(25,56,23,29,55.7)
assert(cmap.index[25] == [56.,23.,29.,55.7])
