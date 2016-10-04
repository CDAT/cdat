import vcsaddons
import os, sys, vcs, vcs.testing.regression as regression
x = vcs.init()
x.drawlogooff()
x.setantialiasing(0)
bg = True
t = vcs.createtemplate()
t.drawLinesAndMarkersLegend(x,
      ["red","blue","green"], ["solid","dash","dot"],[1,4,8],
      ["blue","green","red"], ["cross","square","dot"],[3,4,5],
      ["sample A","type B","thing C"],bg=bg,render=True)

fnm = "test_drawLinesAndMarkersLegend.png"
x.png(fnm)

src = sys.argv[1]
ret = regression.check_result_image(fnm, src)
sys.exit(ret)

