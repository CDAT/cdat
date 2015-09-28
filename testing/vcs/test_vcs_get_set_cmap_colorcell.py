import vcs

x=vcs.init()

cmap = x.createcolormap()

rgb = cmap.getcolorcell(25)
assert(rgb == [0,60,100])

cmap.setcolorcell(25,56,23,29)
assert(cmap.index[25] == [56,23,29])
