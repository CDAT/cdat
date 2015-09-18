import vcs

x=vcs.init()

cmap = x.createcolormap()

rgb = cmap.getcolorcell(25)
print rgb
assert(rgb == [0,60,100])

