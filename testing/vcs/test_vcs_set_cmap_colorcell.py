import vcs

x=vcs.init()

cmap = x.createcolormap()

cmap.setcolorcell(25,56,23,29)

assert(cmap.index[25] == [56,23,29])

