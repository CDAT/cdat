import vcs, sys, os, vcs.testing.regression as regression

x = regression.init()

fa1 = x.createfillarea()

fa1.x=[.2,.8,.8,.2]
fa1.y = [.2,.2,.8,.8]
fa1.color = 242

fa2=x.createfillarea(source = fa1)

fa2.x = [.1,.9,.9,.1]
fa2.y = [.1,.1,.9,.9]

cmap = x.createcolormap()
cmap.setcolorcell(242,0,0,100,50)

fa2.colormap = cmap

x.plot(fa1,bg=True)
x.plot(fa2,bg=True)

fnm = os.path.split(__file__[:-2]+"png")[-1]
regression.run(x, fnm)